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
import           Data.Char
import           Explain.Location
import           Text.Regex.TDFA
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Text.Show.Pretty
import           Debug.Trace
}

%wrapper "monadUserState"


$alpha    = [a-zA-Z]                        -- alphabetic characters
$empty    = [\ \t\f\v\r]                    -- equivalent to $white but without line return
$superEmpty = [\ \t\f\v\r\n]
$head     = [\ \n]                         -- leading whitespace and / or newline
$tail     = [\n]                            -- trailing newline
$multilineStringContent = [$printable \n]

$jsxText  = [^\<\>\{\}]
$jsxTextPopOut = [\<\>\{\}]

$digit    = 0-9                             -- digits

@head = [\ ]*[\n]?[\ ]*

@decimal  = $digit($digit)*                 -- decimal
@negative = \-
@signed = @negative ?
@floating = @decimal \. @decimal | @decimal -- floating point

tokens :-
  <0, jsxOpeningTag, jsxAutoClosed, jsxText> $head*\/\*                                       { beginComment }
  <comment>   [.\n]                                                                           ;
  <comment>   \*\/                                                                            { endComment }
  <0, jsxOpeningTag, jsxAutoClosed> @head"//"[^\n]*[\n]?                                      ; -- Comments

  <0> import                                                                                  { mapToken (\_ -> TokenImport) }
  <0> export                                                                                  { decideTokenExport }
  <0> from                                                                                    { mapToken (\_ -> TokenFrom) }
  <0> type                                                                                    { mapToken (\_ -> TokenType) }
  <0> alias                                                                                   { mapToken (\_ -> TokenAlias) }
  <0> extern                                                                                  { mapToken (\_ -> TokenExtern) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \$                                  { mapToken (\_ -> TokenDollar) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> if                                  { mapToken (\_ -> TokenIf) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> else                                { mapToken (\_ -> TokenElse) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> where                               { mapToken (\_ -> TokenWhere) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> do                                  { mapToken (\_ -> TokenDo) }
  <0> interface                                                                               { mapToken (\_ -> TokenInterface ) }
  <0> instance                                                                                { mapToken (\_ -> TokenInstance ) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> pipe                                { mapToken (\_ -> TokenPipeKeyword) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> return                              { mapToken (\_ -> TokenReturnKeyword) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> is                                  { mapToken (\_ -> TokenIs) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxOpeningTag> \=                   { mapToken (\_ -> TokenEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-Infinity                   { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> Infinity                            { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> @decimal                            { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> @floating                           { mapToken (\s -> TokenFloat s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> 0x[0-9a-fA-F]*                      { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "true"                              { mapToken (\_ -> (TokenBool "true")) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "false"                             { mapToken (\_ -> (TokenBool "false")) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*"=="                          { mapToken (\_ -> TokenDoubleEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*"!="                          { mapToken (\_ -> TokenExclamationMarkEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\.\.\.                        { mapToken (\_ -> TokenSpreadOperator) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\.            { mapToken (\_ -> TokenDot) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\,($tail|\ )* { mapToken (\_ -> TokenComma) }
  <0> \{\{$tail*                                                                              { mapToken (\_ -> TokenLeftDoubleCurly) }
  <0, jsxOpeningTag, jsxAutoClosed, instanceHeader, jsxText> \{$tail*                         { mapToken (\_ -> TokenLeftCurly) }
  <0, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\}                                  { mapToken (\_ -> TokenRightCurly) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \#\[$tail*          { mapToken (\_ -> TokenTupleStart) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \[$tail*                            { mapToken (\_ -> TokenLeftSquaredBracket) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\]            { mapToken (\_ -> TokenRightSquaredBracket) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \(                  { mapToken (\_ -> TokenLeftParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \($tail*            { mapToken (\_ -> TokenLeftParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\)            { mapToken (\_ -> TokenRightParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \)                  { mapToken (\_ -> TokenRightParen) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\:\:$tail*    { mapToken (\_ -> TokenDoubleColon) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \:                  { mapToken (\_ -> TokenColon) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\<\-$tail*    { mapToken (\_ -> TokenLeftArrow) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\-\>$tail*    { mapToken (\_ -> TokenRightArrow) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\=\>$tail*    { mapToken (\_ -> TokenFatArrow) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \|                  { mapToken (\_ -> TokenPipe) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \&                  { mapToken (\_ -> TokenAmpersand) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \^                  { mapToken (\_ -> TokenXor) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> \~                  { mapToken (\_ -> TokenTilde) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\<\<          { mapToken (\_ -> TokenDoubleLeftChevron) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\>\>          { mapToken (\_ -> TokenDoubleRightChevron) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\>\>\>        { mapToken (\_ -> TokenTripleRightChevron) }
  <0> \;                                                                                      { mapToken (\_ -> TokenSemiColon) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> [\n]                { mapToken (\_ -> TokenReturn) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag, instanceHeader> [$alpha \_] [$alpha $digit \_ \']* { decideTokenName }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\+                            { mapToken (\_ -> TokenPlus) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\+\+                          { mapToken (\_ -> TokenDoublePlus) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \-[\ \n]+                           { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-[\ \n]+                    { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \-                                  { mapToken (\_ -> TokenDashUnary) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-                           { mapToken (\_ -> TokenDashUnary) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\?                            { mapToken (\_ -> TokenQuestionMark) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\*                            { mapToken (\_ -> TokenStar) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag> @head\/              { mapToken (\_ -> TokenSlash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\%                            { mapToken (\_ -> TokenPercent) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\|\>                          { mapToken (\_ -> TokenPipeOperator) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\&\&$tail*                    { mapToken (\_ -> TokenDoubleAmpersand) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\|\|$tail*                    { mapToken (\_ -> TokenDoublePipe) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag, instanceHeader> \>   { decideTokenRightChevron }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxText, instanceHeader> \<         { decideTokenLeftChevron }
  <0, stringTemplateMadlib> \>\=                                                              { mapToken (\_ -> TokenRightChevronEq) }
  <0, stringTemplateMadlib> \<\=                                                              { mapToken (\_ -> TokenLeftChevronEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \!                                  { mapToken (\_ -> TokenExclamationMark) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \"(($printable # \")|\\\")*\"       { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> '(($printable # ')|\\')*'           { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0, jsxOpeningTag> \#\- ([$alpha $digit \" \_ \' \` \$ \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/\^]|\\\#)* \-\#
    { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $empty+             ;
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> `                                   { beginStringTemplate }
  <stringTemplate, jsxOpeningTag, jsxAutoClosed> \$\{                                         { beginStringTemplateMadlib }
  <stringTemplateMadlib> \{                                                                   { stringTemplateMadlibLeftCurly }
  <stringTemplateMadlib> \}                                                                   { stringTemplateMadlibRightCurly }
  <stringTemplate> \\[nt`]                                                                    { escapedStringTemplateContent }
  <stringTemplate> `                                                                          { endStringTemplate }
  <stringTemplate> [.\n]                                                                      { pushStringToTemplate }
  <jsxText> [\ \n]*"//"[^\n]*                                                                 ; -- Comments jsx
  <jsxText> \n($superEmpty|\/\/)*                                                             ;
  <jsxText> $jsxText+                                                                         { decideTokenName }
  <jsxText> $jsxTextPopOut                                                                    { jsxTextPopOut }
{
blackList :: Regex
blackList = toRegex "\\`[\ \t \n]*(where|if|else|is|type|alias|export|}|[a-zA-Z0-9]+[\ \t \n]*[=]+|[a-zA-Z0-9]+[\ \t \n]*(::)+).*"


whiteList :: Regex
whiteList = toRegex "\\`[\ \t \n]*[a-zA-Z0-9\"]+[\\(]?.*"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = True, newSyntax = True } defaultExecOpt

jsxTagOpen :: Regex
jsxTagOpen = toRegex "\\`<[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+=(\"[^\"]*\"|{(.|\n|\t)*}))*[ \n\t]*>"

jsxTagSingle :: Regex
jsxTagSingle = toRegex "\\`<[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+=(\"[^\"]*\"|{(.|\n|\t)*}))*[ \n\t]*\\/>"

jsxTagClose :: Regex
jsxTagClose = toRegex "\\`<\\/[a-zA-Z1-9]+[ \n\t]*>"

constraintRegex :: Regex
constraintRegex = toRegex "\\`[^={]*(=>)[^}]*"

-- Use for instance headers, the initial opening curly is already consumed
recordTypeRegex :: Regex
recordTypeRegex = toRegex "\\`[ \n\t]*[a-zA-Z0-9_]*[ \n\t]*::"

isTokenExport :: Regex
isTokenExport = toRegex "\\`export[ ]+(type[ ]+)?[A-Za-z0-9_ ]+[ \n]*="

isTypeExport :: Regex
isTypeExport = toRegex "\\`export[ ]+type"


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
  token <-
        if take len input == ">" then do
          pushStartCode jsxText
          return TokenRightChevron
        else
          return TokenEOF

  return $ (Token (makeArea posn (take len input)) token)


decideTokenExport :: AlexInput -> Int -> Alex Token
decideTokenExport (posn, prevChar, pending, input) len =
  let next           = BLU.fromString $ take 125 input
      matchedTypeExp = match isTypeExport next :: Bool
      matchedTE      = match isTokenExport next :: Bool
      token          = 
        if matchedTE then
          TokenExport
        else if matchedTypeExp then
          TokenTypeExport
        else
          TokenExport
  
  in  return $ Token (makeArea posn (take len input)) token


decideTokenName :: AlexInput -> Int -> Alex Token
decideTokenName (posn, prevChar, pending, input) len = do
  sc <- getStartCode
  let s     = take len input
      token =
        if sc /= instanceHeader then
          TokenName s
        else
          let next    = BLU.fromString $ take 500 input
              matched = match constraintRegex next :: Bool
          in
            if not matched then
              TokenName s
            else
              TokenConstraintName s

  return $ Token (makeArea posn (take len input)) token


decideTokenLeftChevron :: AlexInput -> Int -> Alex Token
decideTokenLeftChevron (posn, prevChar, pending, input) len = do
  let next          = BLU.fromString $ take 800 input
      matchedOpen   = match jsxTagOpen next :: Bool
      matchedClose  = match jsxTagClose next :: Bool
      matchedSingle = match jsxTagSingle next :: Bool
  token <-
    if matchedSingle then do
      pushStartCode jsxAutoClosed
      return TokenJsxTagOpenSingle
    else if matchedOpen then do
      jsxTagOpened
      return TokenJsxTagOpenStart
    else if matchedClose then do
      jsxTagClosed
      return TokenJsxTagOpenEnd
    else
      return TokenLeftChevron
  return $ Token (makeArea posn (take len input)) token

decideTokenRightChevron :: AlexInput -> Int -> Alex Token
decideTokenRightChevron (posn, prevChar, pending, input) len = do
  let next    = BLU.fromString $ ((tail . (take 200)) input)
      matchWL = match whiteList next :: BS.ByteString
      matchBL = match blackList matchWL :: Bool
      token = TokenRightChevron

  sc <- getStartCode
  if sc == jsxOpeningTag then do
    popStartCode
    pushStartCode jsxText
  else if sc == jsxClosingTag then
    popStartCode
  else if sc == jsxAutoClosed then
    popStartCode
  else
    return ()

  return $ Token (makeArea posn (take len input)) token


mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = do
  sc <- getStartCode
  scs <- getStartCodeStack

  token <- case (tokenizer (take len input)) of
    TokenLeftCurly ->
      if sc == instanceHeader then do
        let next        = BLU.fromString $ ((tail . (take 70)) input)
            matchRecord = match recordTypeRegex next :: Bool
        if matchRecord then
          return TokenLeftCurly
        else do
          popStartCode
          return TokenLeftDoubleCurly
      else if sc == jsxText || sc == jsxOpeningTag || sc == jsxAutoClosed then do
        pushStartCode 0
        return TokenLeftCurly
      else
        return TokenLeftCurly

    tok -> return tok


  case token of
    TokenInstance -> pushStartCode instanceHeader
    TokenRightCurly -> do
      previousCode <- getPreviousStartCode
      if previousCode == jsxText || previousCode == jsxAutoClosed || previousCode == jsxOpeningTag then
        popStartCode
      else
        return ()
    _ -> return ()

  return $ Token (makeArea posn (take len input)) token



sanitizeStr :: String -> String
sanitizeStr s = s

escapeBacktick :: Char -> String
escapeBacktick c = case c of
  '`' -> "\\`"
  _   -> c:""


sanitizeJSBlock :: String -> String
sanitizeJSBlock = tail . tail . init . init

strip  = T.unpack . T.strip . T.pack


computeTokenOffsetFromLeadingWhiteChars :: (Int, Int, Int) -> String -> (Int, Int, Int)
computeTokenOffsetFromLeadingWhiteChars initialLoc input =
  let leadingWhites = takeWhile isSpace input
  in  computeTokenOffsetFromLeadingWhiteChars' initialLoc leadingWhites

computeTokenOffsetFromLeadingWhiteChars' :: (Int, Int, Int) -> [Char] -> (Int, Int, Int)
computeTokenOffsetFromLeadingWhiteChars' prev@(prevAbs, prevL, prevC) whites =
  case whites of
    ('\n':cs) ->
      let (computedAbs, computedL, computedC) = (prevAbs + 1, prevL + 1, 1)
      in  computeTokenOffsetFromLeadingWhiteChars' (computedAbs, computedL, computedC) cs
    (_:cs)    ->
      let (computedAbs, computedL, computedC) = (prevAbs + 1, prevL, prevC + 1)
      in  computeTokenOffsetFromLeadingWhiteChars' (computedAbs, computedL, computedC) cs
    _         -> prev


makeArea :: AlexPosn -> String -> Area
makeArea (AlexPn a l c) tokenContent =
  let (a', l', c')  = computeTokenOffsetFromLeadingWhiteChars (a, l, c) tokenContent
      start         = Loc a' l' c'
      contentLines  = lines tokenContent
      lastLine      = last contentLines
      numberOfLines = length contentLines
      end           = if numberOfLines > 1
                      then Loc (a + length tokenContent) (l + numberOfLines - 1) (length lastLine + 1)
                      else Loc (a + length tokenContent) l (c + length tokenContent)
  in  Area start end

tokenToArea :: Token -> Area
tokenToArea (Token area _) = area

data Token = Token Area TokenClass deriving (Eq, Show)

data TokenClass
 = TokenConst
 | TokenNumber String
 | TokenFloat String
 | TokenStr  String
 | TokenName String
 | TokenConstraintName String
 | TokenJSBlock String
 | TokenBool String
 | TokenDollar
 | TokenIf
 | TokenElse
 | TokenInterface
 | TokenInstance
 | TokenDo
 | TokenWhere
 | TokenIs
 | TokenReturnKeyword
 | TokenEq
 | TokenPlus
 | TokenDoublePlus
 | TokenDash
 | TokenDashUnary
 | TokenStar
 | TokenSlash
 | TokenPercent
 | TokenDoubleEq
 | TokenAmpersand
 | TokenXor
 | TokenTilde
 | TokenDoubleLeftChevron
 | TokenDoubleRightChevron
 | TokenTripleRightChevron
 | TokenExclamationMarkEq
 | TokenComma
 | TokenLeftDoubleCurly
 | TokenLeftCurly
 | TokenRightCurly
 | TokenTupleStart
 | TokenLeftSquaredBracket
 | TokenRightSquaredBracket
 | TokenLeftParen
 | TokenRightParen
 | TokenDoubleColon
 | TokenColon
 | TokenQuestionMark
 | TokenDot
 | TokenLeftArrow
 | TokenRightArrow
 | TokenFatArrow
 | TokenEOF
 | TokenImport
 | TokenExport
 | TokenTypeExport
 | TokenFrom
 | TokenPipe
 | TokenPipeOperator
 | TokenSpreadOperator
 | TokenType
 | TokenAlias
 | TokenExtern
 | TokenSemiColon
 | TokenReturn
 | TokenDoubleAmpersand
 | TokenDoublePipe
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
strV (Token _ (TokenFloat x))             = x
strV (Token _ (TokenBool x))              = x
strV (Token _ (TokenName x))              = x
strV (Token _ (TokenConstraintName x))    = x
strV (Token _ (TokenJSBlock x))           = x


alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TokenEOF)
}
