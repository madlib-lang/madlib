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

module Parse.Lexer
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
import qualified Data.Text     as T
import           Explain.Location
import           Text.Regex.TDFA
import           Debug.Trace (trace)
import           Text.Show.Pretty (ppShow)
}

%wrapper "monadUserState"

$alpha    = [a-zA-Z]                        -- alphabetic characters
$empty    = [\ \t\f\v\r]                    -- equivalent to $white but without line return
$head     = [\n \ ]                         -- leading whitespace and / or newline
$tail     = [\n]                            -- trailing newline
$multilineStringContent = [$printable \n]

$digit    = 0-9                             -- digits
@decimal  = $digit($digit)*                 -- decimal
@negative = \-
@signed = @negative ?
@floating = @decimal \. @decimal | @decimal -- floating point

tokens :-
  <0> import                                                   { mapToken (\_ -> TokenImport) }
  <0> export                                                   { mapToken (\_ -> TokenExport) }
  <0> from                                                     { mapToken (\_ -> TokenFrom) }
  <0> data                                                     { mapToken (\_ -> TokenData) }
  <0> alias                                                    { mapToken (\_ -> TokenAlias) }
  <0> const                                                    { mapToken (\_ -> TokenConst) }
  <0, stringTemplateMadlib> if                                 { mapToken (\_ -> TokenIf) }
  <0, stringTemplateMadlib> else                               { mapToken (\_ -> TokenElse) }
  <0, stringTemplateMadlib> where                              { mapToken (\_ -> TokenWhere) }
  <0> interface                                                { mapToken (\_ -> TokenInterface ) }
  <0> instance                                                 { mapToken (\_ -> TokenInstance ) }
  <0, stringTemplateMadlib> pipe                               { mapToken (\_ -> TokenPipeKeyword) }
  <0, stringTemplateMadlib> return                             { mapToken (\_ -> TokenReturnKeyword) }
  <0, stringTemplateMadlib> is                                 { mapToken (\_ -> TokenIs) }
  <0, stringTemplateMadlib> \=                                 { mapToken (\_ -> TokenEq) }
  <0, stringTemplateMadlib> @signed @floating                  { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib> "true"                             { mapToken (\_ -> (TokenBool "true")) }
  <0, stringTemplateMadlib> "false"                            { mapToken (\_ -> (TokenBool "false")) }
  <0, stringTemplateMadlib> "=="                               { mapToken (\_ -> TokenDoubleEq) }
  <0, stringTemplateMadlib> "!="                               { mapToken (\_ -> TokenExclamationMarkEq) }
  <0, stringTemplateMadlib> \.                                 { mapToken (\_ -> TokenDot) }
  <0, stringTemplateMadlib> $head*\,$tail*                     { mapToken (\_ -> TokenComma) }
  <0> \{$tail*                                                 { mapToken (\_ -> TokenLeftCurly) }
  <0> $head*\}                                                 { mapToken (\_ -> TokenRightCurly) }
  <0, stringTemplateMadlib> \[$tail*                           { mapToken (\_ -> TokenLeftSquaredBracket) }
  <0, stringTemplateMadlib> $head*\]                           { mapToken (\_ -> TokenRightSquaredBracket) }
  <0, stringTemplateMadlib> \(                                 { mapToken (\_ -> TokenLeftParen) }
  <0, stringTemplateMadlib> \($tail*                           { mapToken (\_ -> TokenLeftParen) }
  <0, stringTemplateMadlib> $head*\)                           { mapToken (\_ -> TokenRightParen) }
  <0, stringTemplateMadlib> \)                                 { mapToken (\_ -> TokenRightParen) }
  <0, stringTemplateMadlib> $head*\:\:$tail*                   { mapToken (\_ -> TokenDoubleColon) }
  <0, stringTemplateMadlib> \:                                 { mapToken (\_ -> TokenColon) }
  <0, stringTemplateMadlib> $head*\-\>$tail*                   { mapToken (\_ -> TokenArrow) }
  <0, stringTemplateMadlib> $head*\=\>$tail*                   { mapToken (\_ -> TokenFatArrow) }
  <0> \|                                                       { mapToken (\_ -> TokenPipe) }
  <0> \;                                                       { mapToken (\_ -> TokenSemiColon) }
  <0, stringTemplateMadlib> [\n]                               { mapToken (\_ -> TokenReturn) }
  <0, stringTemplateMadlib> [$alpha \_] [$alpha $digit \_ \']* { mapToken (\s -> TokenName s) }
  <0, stringTemplateMadlib> $head*\+                           { mapToken (\_ -> TokenPlus) }
  <0, stringTemplateMadlib> $head*\+\+                         { mapToken (\_ -> TokenDoublePlus) }
  <0, stringTemplateMadlib> \-                                 { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib> \n[\ ]*\-                          { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib> $head*\?                           { mapToken (\_ -> TokenQuestionMark) }
  <0, stringTemplateMadlib> $head*\*                           { mapToken (\_ -> TokenStar) }
  <0, stringTemplateMadlib> $head*\/                           { mapToken (\_ -> TokenSlash) }
  <0, stringTemplateMadlib> $head*\%                           { mapToken (\_ -> TokenPercent) }
  <0, stringTemplateMadlib> $head*\|\>                         { mapToken (\_ -> TokenPipeOperator) }
  <0, stringTemplateMadlib> \.\.\.                             { mapToken (\_ -> TokenSpreadOperator) }
  <0, stringTemplateMadlib> \&\&                               { mapToken (\_ -> TokenDoubleAmpersand) }
  <0, stringTemplateMadlib> \|\|                               { mapToken (\_ -> TokenDoublePipe) }
  <0, stringTemplateMadlib> \>                                 { mapToken (\_ -> TokenRightChevron) }
  <0, stringTemplateMadlib> \<                                 { mapToken (\_ -> TokenLeftChevron) }
  <0, stringTemplateMadlib> \>\=                               { mapToken (\_ -> TokenRightChevronEq) }
  <0, stringTemplateMadlib> \<\=                               { mapToken (\_ -> TokenLeftChevronEq) }
  <0, stringTemplateMadlib> \!                                 { mapToken (\_ -> TokenExclamationMark) }
  <0, stringTemplateMadlib> \"($printable # \")*\"             { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0, stringTemplateMadlib> '($printable # ')*'                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0> \#\- [$alpha $digit \" \_ \' \` \$ \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/]* \-\#
    { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  <0> [\ \n]*"//".*                               ; -- Comments
  <0, stringTemplateMadlib> $empty+               ;
  <0,comment> \/\*                                { beginComment }
  <comment>   [.\n]                               ;
  <comment>   \*\/                                { endComment }

  <0> `                                           { beginStringTemplate }

  <stringTemplate> \$\{                           { beginStringTemplateMadlib }
  <stringTemplateMadlib> \{                       { stringTemplateMadlibLeftCurly }
  <stringTemplateMadlib> \}                       { stringTemplateMadlibRightCurly }
  <stringTemplate> \\[nt`]                        { escapedStringTemplateContent }
  <stringTemplate> `                              { endStringTemplate }
  <stringTemplate> [.\n]                          { pushStringToTemplate }
{
blackList :: String
blackList = "\\`[\ \t \n]*(where|if|else|is|data|alias|export|}|[a-zA-Z0-9]+[\ \t \n]*[=]+|[a-zA-Z0-9]+[\ \t \n]*(::)+).*"


whiteList :: String
whiteList = "\\`[\ \t \n]*[a-zA-Z0-9\"]+[\\(]?.*"

-- Int: commentDepth
-- (String, Int): (stringBuffer, curlyCount)
data AlexUserState = AlexUserState Int (String, Int) deriving(Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 ("", 0)

setStartCode :: Int -> Alex ()
setStartCode code =
  Alex $ \s ->
    Right (s{ alex_scd = code }, ())

setDefaultStartCode :: Alex ()
setDefaultStartCode =
  Alex $ \s ->
    Right
      ( s{ alex_scd = 0 }
      , ()
      )

getState :: Alex AlexUserState
getState = Alex $ \s@AlexState{alex_ust = state} -> Right (s, state)

updateCommentDepth :: Int -> Alex ()
updateCommentDepth n = do
  (AlexUserState _ strState) <- getState
  Alex $ \s ->
    Right
      ( s{ alex_ust = AlexUserState n strState, alex_scd = if n > 0 then comment else 0 }
      , ()
      )

beginComment :: AlexInput -> Int -> Alex Token
beginComment input n = do
  (AlexUserState cd _) <- getState
  updateCommentDepth $ cd + 1
  skip input n

endComment :: AlexInput -> Int -> Alex Token
endComment input n = do
  (AlexUserState cd _) <- getState
  updateCommentDepth $ cd - 1
  skip input n



resetStringTemplate :: Alex ()
resetStringTemplate = do
  (AlexUserState n (_, curlyCount)) <- getState
  Alex $ \s ->
    Right
      ( s{ alex_ust = AlexUserState n ("", curlyCount) }
      , ()
      )

resetCurlyCount :: Alex ()
resetCurlyCount = do
  (AlexUserState n (strBuffer, _)) <- getState
  Alex $ \s ->
    Right
      ( s{ alex_ust = AlexUserState n (strBuffer, 0) }
      , ()
      )

updateCurlyCount :: Int -> Alex ()
updateCurlyCount n = do
  (AlexUserState cd (strBuffer, _)) <- getState
  Alex $ \s ->
    Right
      ( s{ alex_ust = AlexUserState n (strBuffer, n) }
      , ()
      )

appendStringToTemplate :: String -> Alex ()
appendStringToTemplate extra = do
  (AlexUserState n (strBuffer, curlyCount)) <- getState
  Alex $ \s ->
    Right
      ( s{ alex_ust = AlexUserState n (strBuffer <> extra, curlyCount) }
      , ()
      )

beginStringTemplateMadlib :: AlexInput -> Int -> Alex Token
beginStringTemplateMadlib i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (strBuffer, _)) <- getState
  setStartCode stringTemplateMadlib
  resetStringTemplate
  resetCurlyCount
  return $ Token (makeArea posn (take len input)) (TokenStr strBuffer)

stringTemplateMadlibLeftCurly :: AlexInput -> Int -> Alex Token
stringTemplateMadlibLeftCurly i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (_, count)) <- getState
  updateCurlyCount $ count + 1
  return $ Token (makeArea posn (take len input)) TokenLeftCurly

stringTemplateMadlibRightCurly :: AlexInput -> Int -> Alex Token
stringTemplateMadlibRightCurly i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (_, count)) <- getState
  if count == 0 then
    begin stringTemplate i len
  else do
    updateCurlyCount $ count - 1
    return $ Token (makeArea posn (take len input)) TokenRightCurly

beginStringTemplate :: AlexInput -> Int -> Alex Token
beginStringTemplate i@(posn, prevChar, pending, input) len = do
  setStartCode stringTemplate
  return $ Token (makeArea posn (take len input)) TokenTemplateStringStart

endStringTemplate :: AlexInput -> Int -> Alex Token
endStringTemplate i@(posn, prevChar, pending, input) len = do
  (AlexUserState _ (strBuffer, _)) <- getState
  resetStringTemplate
  setDefaultStartCode
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


mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = do
  s <- getState
  return $ Token (makeArea posn (take len input)) token
  
  where
    token = case (tokenizer (take len input)) of
      TokenRightChevron ->
        let next  = ((tail . (take 100)) input)
            matchWL = next =~ whiteList :: String
            matchBL = matchWL =~ blackList :: String
        in
          if ((not . null) matchWL) && null matchBL
          then TokenRightChevron
          else TokenTupleEnd
      tok -> tok


sanitizeStr :: String -> String
sanitizeStr = tail . init

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
 | TokenDottedName String
 | TokenJSBlock String
 | TokenBool String
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
 | TokenData
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
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))               = x
strV (Token _ (TokenTemplateStringEnd x)) = x
strV (Token _ (TokenNumber x))            = x
strV (Token _ (TokenBool x))              = x
strV (Token _ (TokenName x))              = x
strV (Token _ (TokenJSBlock x))           = x


alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TokenEOF)
}
