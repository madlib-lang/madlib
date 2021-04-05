{
{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Parse.Madlib.Lexer
  ( Alex(..)
  , AlexState(..)
  , alexEOF
  , Token(..)
  , Loc(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToArea
  , strV
  )
where

import           Control.Monad.State
import           System.Exit
import qualified Data.Text.Lazy     as T
import           Explain.Location
import           Text.Regex.TDFA
import           Debug.Trace (trace)
import           Text.Show.Pretty (ppShow)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
}

%wrapper "monadUserState"

$alpha    = [a-zA-Z]                        -- alphabetic characters
$empty    = [\ \t\f\v\r]                    -- equivalent to $white but without line return
$superEmpty = [\ \t\f\v\r\n]                    -- equivalent to $white but without line return
$head     = [\n \ ]                         -- leading whitespace and / or newline
$tail     = [\n]                            -- trailing newline
$multilineStringContent = [$printable \n]

$jsxText  = [^\<\>\{\}]
$jsxTextPopOut = [\<\>\{\}]

$digit    = 0-9                             -- digits
@decimal  = $digit($digit)*                 -- decimal
@negative = \-
@signed = @negative ?
@floating = @decimal \. @decimal | @decimal -- floating point

tokens :-
  <0, jsxOpeningTag, jsxAutoClosed> [\ \n]*"//"[^\n]*[\n]?                                ; -- Comments
  <0, jsxOpeningTag, jsxAutoClosed, jsxText, comment> $head*\/\*                          { beginComment }
  <comment>   [.\n]                                                                       ;
  <comment>   \*\/                                                                        { endComment }

  <0> import                                                                                 { mapToken (\_ -> TokenImport) }
  <0> export                                                                                 { mapToken (\_ -> TokenExport) }
  <0> from                                                                                   { mapToken (\_ -> TokenFrom) }
  <0> type                                                                                   { mapToken (\_ -> TokenType) }
  <0> alias                                                                                  { mapToken (\_ -> TokenAlias) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \$                                 { mapToken (\_ -> TokenDollar) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> if                                 { mapToken (\_ -> TokenIf) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> else                               { mapToken (\_ -> TokenElse) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> where                              { mapToken (\_ -> TokenWhere) }
  <0> interface                                                                              { mapToken (\_ -> TokenInterface ) }
  <0> instance                                                                               { mapToken (\_ -> TokenInstance ) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> pipe                               { mapToken (\_ -> TokenPipeKeyword) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> return                             { mapToken (\_ -> TokenReturnKeyword) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> is                                 { mapToken (\_ -> TokenIs) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxOpeningTag> \=                  { mapToken (\_ -> TokenEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-Infinity                  { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> Infinity                           { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> @signed @floating                  { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "true"                             { mapToken (\_ -> (TokenBool "true")) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "false"                            { mapToken (\_ -> (TokenBool "false")) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "=="                               { mapToken (\_ -> TokenDoubleEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "!="                               { mapToken (\_ -> TokenExclamationMarkEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \.                 { mapToken (\_ -> TokenDot) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\,($tail|\ )*     { mapToken (\_ -> TokenComma) }
  <0> \{\{$tail*                                                                             { mapToken (\_ -> TokenLeftDoubleCurly) }
  <0, jsxOpeningTag, jsxAutoClosed, instanceHeader, jsxText> \{$tail*                        { mapToken (\_ -> TokenLeftCurly) }
  <0, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\}                                 { mapToken (\_ -> TokenRightCurly) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \[$tail*                           { mapToken (\_ -> TokenLeftSquaredBracket) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\]                           { mapToken (\_ -> TokenRightSquaredBracket) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \(                 { mapToken (\_ -> TokenLeftParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \($tail*           { mapToken (\_ -> TokenLeftParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\)           { mapToken (\_ -> TokenRightParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \)                 { mapToken (\_ -> TokenRightParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\:\:$tail*   { mapToken (\_ -> TokenDoubleColon) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \:                 { mapToken (\_ -> TokenColon) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\-\>$tail*   { mapToken (\_ -> TokenArrow) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\=\>$tail*   { mapToken (\_ -> TokenFatArrow) }
  <0> \|                                                                                     { mapToken (\_ -> TokenPipe) }
  <0> \;                                                                                     { mapToken (\_ -> TokenSemiColon) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> [\n]               { mapToken (\_ -> TokenReturn) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag, instanceHeader> [$alpha \_] [$alpha $digit \_ \']* { mapToken (\s -> TokenName s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\+                           { mapToken (\_ -> TokenPlus) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\+\+                         { mapToken (\_ -> TokenDoublePlus) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \-                                 { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-                          { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\?                           { mapToken (\_ -> TokenQuestionMark) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\*                           { mapToken (\_ -> TokenStar) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag> $head*\/            { mapToken (\_ -> TokenSlash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\%                           { mapToken (\_ -> TokenPercent) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\|\>                         { mapToken (\_ -> TokenPipeOperator) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \.\.\.                             { mapToken (\_ -> TokenSpreadOperator) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \&\&                               { mapToken (\_ -> TokenDoubleAmpersand) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \|\|                               { mapToken (\_ -> TokenDoublePipe) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag, instanceHeader> \>                 { mapToken (\_ -> TokenRightChevron) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxText, instanceHeader> \<        { mapToken (\_ -> TokenLeftChevron) }
  <0, stringTemplateMadlib> \>\=                               { mapToken (\_ -> TokenRightChevronEq) }
  <0, stringTemplateMadlib> \<\=                               { mapToken (\_ -> TokenLeftChevronEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \!                                 { mapToken (\_ -> TokenExclamationMark) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \"(($printable # \")|\\\")*\"      { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> '(($printable # ')|\\')*'          { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0, jsxOpeningTag> \#\- ([$alpha $digit \" \_ \' \` \$ \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/]|\\\#)* \-\#
    { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $empty+               ;
  <0, jsxOpeningTag, jsxAutoClosed> `                                           { beginStringTemplate }
  <stringTemplate, jsxOpeningTag, jsxAutoClosed> \$\{                           { beginStringTemplateMadlib }
  <stringTemplateMadlib> \{                       { stringTemplateMadlibLeftCurly }
  <stringTemplateMadlib> \}                       { stringTemplateMadlibRightCurly }
  <stringTemplate> \\[nt`]                        { escapedStringTemplateContent }
  <stringTemplate> `                              { endStringTemplate }
  <stringTemplate> [.\n]                          { pushStringToTemplate }
  <jsxText> [\ \n]*"//"[^\n]*                     ; -- Comments jsx
  <jsxText> \n($superEmpty|\/\/)*                 ;
  <jsxText> $jsxText+                             { mapToken (\s -> TokenName s) }
  <jsxText> $jsxTextPopOut                        { jsxTextPopOut }
{
blackList :: Regex
blackList = toRegex "\\`[\ \t \n]*(where|if|else|is|data|alias|export|}|[a-zA-Z0-9]+[\ \t \n]*[=]+|[a-zA-Z0-9]+[\ \t \n]*(::)+).*"


whiteList :: Regex
whiteList = toRegex "\\`[\ \t \n]*[a-zA-Z0-9\"]+[\\(]?.*"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt

jsxTagOpen :: Regex
jsxTagOpen = toRegex "\\`<[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+=(\"[^\"]*\"|{.*}))*[ \n\t]*>"

jsxTagSingle :: Regex
jsxTagSingle = toRegex "\\`<[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+=(\"[^\"]*\"|{.*}))*[ \n\t]*\\/>"

jsxTagClose :: Regex
jsxTagClose = toRegex "\\`<\\/[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+=(\"[^\"]*\"|{.*}))*[ \n\t]*>"

constraintRegex :: Regex
constraintRegex = toRegex "\\`[^={]*(=>)[^}]*"
-- constraintRegex = toRegex "\\`[^={]*(=>)[^}]*([^=]=[^=]|{[\\t\\ ]*\n).*"

-- Int: commentDepth
-- (String, Int): (stringBuffer, curlyCount)
-- Int: jsx depth
-- Int: start code stack
data AlexUserState = AlexUserState Int (String, Int) Int [Int] deriving(Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 ("", 0) 0 []

setStartCode :: Int -> Alex ()
setStartCode code = do
  sc <- getStartCode
  Alex $ \s -> Right (s{ alex_scd = code }, ())

pushStartCode :: Int -> Alex ()
pushStartCode code = do
  (AlexUserState n strState jsxDepth codeStack) <- getState
  setStartCode code
  setState $ AlexUserState n strState jsxDepth (codeStack ++ [code])

popStartCode :: Alex ()
popStartCode = do
  (AlexUserState n strState jsxDepth codeStack) <- getState
  if not (null codeStack) then do
    let popped  = init codeStack
        newCode = if not (null popped) then last popped else 0
    setState $ AlexUserState n strState jsxDepth popped
    setStartCode newCode
  else
    setStartCode 0

getStartCodeStack :: Alex [Int]
getStartCodeStack = do
  (AlexUserState _ _ _ scs) <- getState
  return scs

getPreviousStartCode :: Alex Int
getPreviousStartCode = do
  codeStack <- getStartCodeStack
  if length codeStack > 1 then
    return $ (last . init) codeStack
  else
    return 0

getStartCode :: Alex Int
getStartCode = Alex $ \s -> do
  let sc = alex_scd s
  Right (s, sc)

setDefaultStartCode :: Alex ()
setDefaultStartCode =
  Alex $ \s ->
    Right
      ( s{ alex_scd = 0 }
      , ()
      )

getState :: Alex AlexUserState
getState = Alex $ \s@AlexState{alex_ust = state} -> Right (s, state)

setState :: AlexUserState -> Alex ()
setState state = Alex $ \s -> Right (s{ alex_ust = state }, ())


jsxTagOpened :: Alex ()
jsxTagOpened = do
  (AlexUserState n strState jsxDepth codeStack) <- getState
  setState (AlexUserState n strState (jsxDepth + 1) codeStack)
  pushStartCode jsxOpeningTag

jsxTagClosed :: Alex ()
jsxTagClosed = do
  (AlexUserState n strState jsxDepth codeStack) <- getState
  setState (AlexUserState n strState (jsxDepth - 1) codeStack)
  popStartCode
  pushStartCode jsxClosingTag


beginComment :: AlexInput -> Int -> Alex Token
beginComment input n = do
  pushStartCode comment
  skip input n

endComment :: AlexInput -> Int -> Alex Token
endComment input n = do
  popStartCode
  skip input n



resetStringTemplate :: Alex ()
resetStringTemplate = do
  (AlexUserState n (_, curlyCount) jsxDepth codeStack) <- getState
  setState $ AlexUserState n ("", curlyCount) jsxDepth codeStack

resetCurlyCount :: Alex ()
resetCurlyCount = do
  (AlexUserState n (strBuffer, _) jsxDepth codeStack) <- getState
  setState $ AlexUserState n (strBuffer, 0) jsxDepth codeStack

updateCurlyCount :: Int -> Alex ()
updateCurlyCount n = do
  (AlexUserState cd (strBuffer, _) jsxDepth codeStack) <- getState
  setState $ AlexUserState n (strBuffer, n) jsxDepth codeStack

appendStringToTemplate :: String -> Alex ()
appendStringToTemplate extra = do
  (AlexUserState n (strBuffer, curlyCount) jsxDepth codeStack) <- getState
  setState $ AlexUserState n (strBuffer <> extra, curlyCount) jsxDepth codeStack

beginStringTemplateMadlib :: AlexInput -> Int -> Alex Token
beginStringTemplateMadlib i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (strBuffer, _) _ _) <- getState
  pushStartCode stringTemplateMadlib
  resetStringTemplate
  resetCurlyCount
  return $ Token (makeArea posn (take len input)) (TokenStr strBuffer)

stringTemplateMadlibLeftCurly :: AlexInput -> Int -> Alex Token
stringTemplateMadlibLeftCurly i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (_, count) _ _) <- getState
  updateCurlyCount $ count + 1
  return $ Token (makeArea posn (take len input)) TokenLeftCurly

stringTemplateMadlibRightCurly :: AlexInput -> Int -> Alex Token
stringTemplateMadlibRightCurly i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (_, count) _ _) <- getState
  if count == 0 then do
    popStartCode
    begin stringTemplate i len
  else do
    updateCurlyCount $ count - 1
    return $ Token (makeArea posn (take len input)) TokenRightCurly

beginStringTemplate :: AlexInput -> Int -> Alex Token
beginStringTemplate i@(posn, prevChar, pending, input) len = do
  pushStartCode stringTemplate
  return $ Token (makeArea posn (take len input)) TokenTemplateStringStart

endStringTemplate :: AlexInput -> Int -> Alex Token
endStringTemplate i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (strBuffer, _) _ _) <- getState
  resetStringTemplate
  popStartCode
  return $ (Token (makeArea posn (take len input)) (TokenTemplateStringEnd strBuffer))

escapedStringTemplateContent :: AlexInput -> Int -> Alex Token
escapedStringTemplateContent i@(posn, prevChar, pending, input) len = do
  case take len input of
    "\\`" -> appendStringToTemplate "\\`"
    "\\n" -> appendStringToTemplate ('\n':"")
    "\\t" -> appendStringToTemplate ('\t':"")
  
  skip i len

pushStringToTemplate :: AlexInput -> Int -> Alex Token
pushStringToTemplate i@(posn, prevChar, pending, input) len = do
  appendStringToTemplate $ take len input
  skip i len


jsxTextPopOut :: AlexInput -> Int -> Alex Token
jsxTextPopOut i@(posn, prevChar, pending, input) len = do
  popStartCode
  return $ (Token (makeArea posn (take len input)) (TokenEOF))

mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = do
  sc <- getStartCode
  scs <- getStartCodeStack

  token <- case (tokenizer (take len input)) of
        TokenLeftCurly ->
          if sc == instanceHeader then do
            popStartCode
            return TokenLeftDoubleCurly
          else if sc == jsxText || sc == jsxOpeningTag || sc == jsxAutoClosed then do
            pushStartCode 0
            return TokenLeftCurly
          else
            return TokenLeftCurly

        TokenName s ->
          if sc /= instanceHeader then
            return $ TokenName s
          else
            let next = BLU.fromString $ take 250 input 
                matched = match constraintRegex next :: Bool
                -- matched = match constraintRegex next :: BS.ByteString -- 10.4
            in
              if not matched then
                return $ TokenName s
              else
                return $ TokenConstraintName s

        TokenRightChevron ->
          let next  = BLU.fromString $ ((tail . (take 200)) input)
              matchWL = match whiteList next :: BS.ByteString
              matchBL = match blackList matchWL :: Bool
          in
            if not (BS.null matchWL) && not matchBL
            then return TokenRightChevron
            else return TokenTupleEnd
        
        TokenLeftChevron ->
          let next    = BLU.fromString $ take 400 input
              matchedOpen = match jsxTagOpen next :: Bool
              matchedClose = match jsxTagClose next :: Bool
              matchedSingle = match jsxTagSingle next :: Bool
          in
            if matchedSingle then do
              return TokenJsxTagOpenSingle
            else if matchedOpen then
              return TokenJsxTagOpenStart
            else if matchedClose then do
              return TokenJsxTagOpenEnd
            else
              return TokenLeftChevron
        tok -> return tok


  case token of
    TokenInstance -> pushStartCode instanceHeader
    TokenJsxTagOpenStart -> jsxTagOpened
    TokenJsxTagOpenEnd -> jsxTagClosed
    TokenJsxTagOpenSingle -> pushStartCode jsxAutoClosed
    TokenRightCurly -> do
      previousCode <- getPreviousStartCode
      if previousCode == jsxText || previousCode == jsxAutoClosed || previousCode == jsxOpeningTag then
        popStartCode
      else
        return ()
    TokenRightChevron ->
      if sc == jsxOpeningTag then do
        popStartCode
        pushStartCode jsxText
      else if sc == jsxClosingTag then
        popStartCode
      else if sc == jsxAutoClosed then
        popStartCode
      else
        return ()
    TokenTupleEnd ->
      if sc == jsxOpeningTag then do
        popStartCode
        pushStartCode jsxText
      else if sc == jsxClosingTag then
        popStartCode
      else if sc == jsxAutoClosed then
        popStartCode
      else
        return ()
    _ -> return ()

  return $ Token (makeArea posn (take len input)) token



sanitizeStr :: String -> String
sanitizeStr s = s-- >>= escapeBacktick
-- sanitizeStr s = (tail . init $ s) >>= escapeBacktick

escapeBacktick :: Char -> String
escapeBacktick c = case c of
  '`' -> "\\`"
  _   -> c:""

-- sanitizeSingleQuotedStr :: String -> String
-- sanitizeSingleQuotedStr s =
--   let 

sanitizeJSBlock :: String -> String
sanitizeJSBlock = strip . tail . tail . init . init

strip  = T.unpack . T.strip . T.pack


makeArea :: AlexPosn -> String -> Area
makeArea (AlexPn a l c) tokenContent =
  let start         = Loc a l c
      contentLines  = lines tokenContent
      lastLine      = last contentLines
      numberOfLines = length contentLines
      end           = if numberOfLines > 1
                      then Loc (a + length tokenContent) (l + numberOfLines - 1) (length lastLine)
                      else Loc (a + length tokenContent) l (c + length tokenContent)
  in  Area start end

tokenToArea :: Token -> Area
tokenToArea (Token area _) = area

data Token = Token Area TokenClass deriving (Eq, Show)

data TokenClass
 = TokenConst
 | TokenNumber String
 | TokenStr  String
 | TokenName String
 | TokenConstraintName String
 | TokenDottedName String
 | TokenJSBlock String
 | TokenBool String
 | TokenDollar
 | TokenIf
 | TokenElse
 | TokenInterface
 | TokenInstance
 | TokenWhere
 | TokenIs
 | TokenReturnKeyword
 | TokenEq
 | TokenPlus
 | TokenDoublePlus
 | TokenDash
 | TokenStar
 | TokenSlash
 | TokenPercent
 | TokenDoubleEq
 | TokenExclamationMarkEq
 | TokenComma
 | TokenLeftDoubleCurly
 | TokenLeftCurly
 | TokenRightCurly
 | TokenLeftSquaredBracket
 | TokenRightSquaredBracket
 | TokenLeftParen
 | TokenRightParen
 | TokenDoubleColon
 | TokenColon
 | TokenQuestionMark
 | TokenDot
 | TokenArrow
 | TokenFatArrow
 | TokenEOF
 | TokenImport
 | TokenExport
 | TokenFrom
 | TokenPipe
 | TokenPipeOperator
 | TokenSpreadOperator
 | TokenType
 | TokenAlias
 | TokenSemiColon
 | TokenReturn
 | TokenDoubleAmpersand
 | TokenDoublePipe
 | TokenTupleEnd
 | TokenRightChevron
 | TokenLeftChevron
 | TokenRightChevronEq
 | TokenLeftChevronEq
 | TokenExclamationMark
 | TokenPipeKeyword
 | TokenTemplateStringStart
 | TokenTemplateStringEnd String
 | TokenJsxTagOpenStart
 | TokenJsxTagOpenEnd
 | TokenJsxTagOpenSingle
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))               = x
strV (Token _ (TokenTemplateStringEnd x)) = x
strV (Token _ (TokenNumber x))            = x
strV (Token _ (TokenBool x))              = x
strV (Token _ (TokenName x))              = x
strV (Token _ (TokenConstraintName x))    = x
strV (Token _ (TokenJSBlock x))           = x


alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TokenEOF)
}
