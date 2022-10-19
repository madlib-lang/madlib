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
  , tokenArea
  , tokenTarget
  , strV
  , charData
  )
where

import           Control.Monad.State
import           System.Exit
import qualified Data.Text.Lazy     as T
import           Data.Char          as Char
import           Explain.Location
import           AST.Source
import           Text.Regex.TDFA
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Text.Printf

import           Text.Show.Pretty
import           Debug.Trace

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as TextEncoding
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
}

%wrapper "monadUserState"


$alpha    = [a-zA-Z]                        -- alphabetic characters
$empty    = [\ \t\f\v\r]                    -- equivalent to $white but without line return
$superEmpty = [\ \t\f\v\r\n]
$head     = [\ \n]                          -- leading whitespace and / or newline
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
  -- <0, jsxOpeningTag, jsxAutoClosed> @head\/\/[^\n]*                                           ; -- Comments
  <0, jsxOpeningTag, jsxAutoClosed> @head\/\/[^\n]*[\n]?                                      ; -- Comments

  <0> import                                                                                  { mapToken (\_ -> TokenImport) }
  <0> export                                                                                  { decideTokenExport }
  <0> from                                                                                    { mapToken (\_ -> TokenFrom) }
  <0> type                                                                                    { mapToken (\_ -> TokenType) }
  <0> alias                                                                                   { mapToken (\_ -> TokenAlias) }
  <0> extern                                                                                  { mapToken (\_ -> TokenExtern) }
  <0> \#iftarget[\ ]*llvm                                                                     { processIfTarget TargetLLVM }
  <0> \#iftarget[\ ]*js                                                                       { processIfTarget TargetJS }
  <0> \#elseif[\ ]*llvm                                                                       { processElseIfTarget TargetLLVM }
  <0> \#elseif[\ ]*js                                                                         { processElseIfTarget TargetJS }
  <0> \#endif                                                                                 { processEndIfTarget }
  <0, stringTemplateMadlib> \#                                                                { mapToken (\_ -> TokenSharpSign) }
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
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxOpeningTag> \=$tail?             { mapToken (\_ -> TokenEq) }
  -- <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-Infinity                   { mapToken (\s -> TokenNumber s) }
  -- <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> Infinity                            { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> @decimal                            { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> @floating                           { mapToken (\s -> TokenFloat s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> 0x[0-9a-fA-F]*                      { mapToken (\s -> TokenNumber s) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "true"                              { mapToken (\_ -> (TokenBool "true")) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> "false"                             { mapToken (\_ -> (TokenBool "false")) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*"=="                          { mapToken (\_ -> TokenDoubleEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*"!="                          { mapToken (\_ -> TokenExclamationMarkEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $head*\.\.\.        { mapToken (\_ -> TokenSpreadOperator) }
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
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\+                                      { mapToken (\_ -> TokenPlus) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\+\+                                    { mapToken (\_ -> TokenDoublePlus) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \-[\ \n]+                                     { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-[\ \n]+                              { mapToken (\_ -> TokenDash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \-                                            { mapToken (\_ -> TokenDashUnary) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \n[\ ]*\-                                     { mapToken (\_ -> TokenDashUnary) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\?                                      { mapToken (\_ -> TokenQuestionMark) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\*                                      { mapToken (\_ -> TokenStar) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag> @head\/                        { mapToken (\_ -> TokenSlash) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\%                                      { mapToken (\_ -> TokenPercent) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\|\>                                    { mapToken (\_ -> TokenPipeOperator) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\&\&$tail*                              { mapToken (\_ -> TokenDoubleAmpersand) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> $head*\|\|$tail*                              { mapToken (\_ -> TokenDoublePipe) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxClosingTag, instanceHeader> \>             { decideTokenRightChevron }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, jsxText, instanceHeader> \<                   { decideTokenLeftChevron }
  <0, stringTemplateMadlib> \>\=                                                                        { mapToken (\_ -> TokenRightChevronEq) }
  <0, stringTemplateMadlib> \<\=                                                                        { mapToken (\_ -> TokenLeftChevronEq) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \!                                            { mapToken (\_ -> TokenExclamationMark) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \"(($printable # \")|\\\")*\"                 { mapToken (\s -> TokenStr (sanitizeStr s)) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> \' ($printable # [\'\\] | " " | \\. | \') \'  { mapCharToken }
  <0, jsxOpeningTag> \#\- ([$alpha $digit \" \_ \' \` \$ \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/\^]|\\\#)* \-\#
    { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed, instanceHeader> $empty+                       ;
  <0, stringTemplateMadlib, jsxOpeningTag, jsxAutoClosed> `                                             { beginStringTemplate }
  <stringTemplate, jsxOpeningTag, jsxAutoClosed> \$\{                                                   { beginStringTemplateMadlib }
  <stringTemplateMadlib> \{                                                                             { stringTemplateMadlibLeftCurly }
  <stringTemplateMadlib> \}                                                                             { stringTemplateMadlibRightCurly }
  <stringTemplate> \\[nt`]                                                                              { escapedStringTemplateContent }
  <stringTemplate> `                                                                                    { endStringTemplate }
  <stringTemplate> [.\n]                                                                                { pushStringToTemplate }
  <jsxText> [\ \n]*"//"[^\n]*                                                                           ; -- Comments jsx
  <jsxText> \n($superEmpty|\/\/)*                                                                       ;
  <jsxText> $jsxText+                                                                                   { decideTokenName }
  <jsxText> $jsxTextPopOut                                                                              { jsxTextPopOut }
{
blackList :: Regex
blackList = toRegex "\\`[\ \t \n]*(where|if|else|is|type|alias|export|}|[a-zA-Z0-9]+[\ \t \n]*[=]+|[a-zA-Z0-9]+[\ \t \n]*(::)+).*"


whiteList :: Regex
whiteList = toRegex "\\`[\ \t \n]*[a-zA-Z0-9\"]+[\\(]?.*"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = True, newSyntax = True } defaultExecOpt

jsxTagOpen :: Regex
jsxTagOpen = toRegex "\\`<[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+(=(\"(\\\"|[^\"])*\"|{(.|\n|\t)*}))?)*[ \n\t]*>"

jsxTagSingle :: Regex
jsxTagSingle = toRegex "\\`<[a-zA-Z1-9]+([ \n\t]+[a-zA-Z]+(=(\"(\\\"|[^\"])*\"|{([^>]|\n|\t)*}))?)*[ \n\t]*\\/>"

jsxTagClose :: Regex
jsxTagClose = toRegex "\\`<\\/[a-zA-Z1-9]+[ \n\t]*>"

constraintRegex :: Regex
constraintRegex = toRegex "\\`([^={]|\n)*(=>)[^}]*"

-- Use for instance headers, the initial opening curly is already consumed
recordTypeRegex :: Regex
recordTypeRegex = toRegex "\\`[ \n\t]*(\\.\\.\\.[a-zA-Z0-9_]*(,)?|[a-zA-Z0-9_]*[ \n\t]*::)"

isTokenExport :: Regex
isTokenExport = toRegex "\\`export[ ]+(type[ ]+)?[A-Za-z0-9_ ]+([ \n]*\\/\\/[^\n]*)*[ \n]*="

isTypeExport :: Regex
isTypeExport = toRegex "\\`export[ ]+type"


-- Int: commentDepth
-- (String, Int): (stringBuffer, curlyCount)
-- Int: jsx depth
-- Int: start code stack
-- SourceTarget: current source target we're in
data AlexUserState = AlexUserState Int (String, Int) Int [Int] SourceTarget deriving(Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 ("", 0) 0 [] TargetAll

setCurrentSourceTarget :: SourceTarget -> Alex ()
setCurrentSourceTarget sourceTarget = do
  (AlexUserState n strState jsxDepth codeStack _) <- getState
  setState $ AlexUserState n strState jsxDepth codeStack sourceTarget

getCurrentSourceTarget :: Alex SourceTarget
getCurrentSourceTarget = do
  (AlexUserState _ _ _ _ sourceTarget) <- getState
  return sourceTarget

setStartCode :: Int -> Alex ()
setStartCode code = do
  sc <- getStartCode
  Alex $ \s -> Right (s{ alex_scd = code }, ())

pushStartCode :: Int -> Alex ()
pushStartCode code = do
  (AlexUserState n strState jsxDepth codeStack sourceTarget) <- getState
  setStartCode code
  setState $ AlexUserState n strState jsxDepth (codeStack ++ [code]) sourceTarget

popStartCode :: Alex ()
popStartCode = do
  (AlexUserState n strState jsxDepth codeStack sourceTarget) <- getState
  if not (null codeStack) then do
    let popped  = init codeStack
        newCode = if not (null popped) then last popped else 0
    setState $ AlexUserState n strState jsxDepth popped sourceTarget
    setStartCode newCode
  else
    setStartCode 0

getStartCodeStack :: Alex [Int]
getStartCodeStack = do
  (AlexUserState _ _ _ scs _) <- getState
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
  (AlexUserState n strState jsxDepth codeStack sourceTarget) <- getState
  setState (AlexUserState n strState (jsxDepth + 1) codeStack sourceTarget)
  pushStartCode jsxOpeningTag

jsxTagClosed :: Alex ()
jsxTagClosed = do
  (AlexUserState n strState jsxDepth codeStack sourceTarget) <- getState
  setState (AlexUserState n strState (jsxDepth - 1) codeStack sourceTarget)
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
  (AlexUserState n (_, curlyCount) jsxDepth codeStack sourceTarget) <- getState
  setState $ AlexUserState n ("", curlyCount) jsxDepth codeStack sourceTarget

resetCurlyCount :: Alex ()
resetCurlyCount = do
  (AlexUserState n (strBuffer, _) jsxDepth codeStack sourceTarget) <- getState
  setState $ AlexUserState n (strBuffer, 0) jsxDepth codeStack sourceTarget

updateCurlyCount :: Int -> Alex ()
updateCurlyCount n = do
  (AlexUserState cd (strBuffer, _) jsxDepth codeStack sourceTarget) <- getState
  setState $ AlexUserState n (strBuffer, n) jsxDepth codeStack sourceTarget

appendStringToTemplate :: String -> Alex ()
appendStringToTemplate extra = do
  (AlexUserState n (strBuffer, curlyCount) jsxDepth codeStack sourceTarget) <- getState
  setState $ AlexUserState n (strBuffer <> extra, curlyCount) jsxDepth codeStack sourceTarget

beginStringTemplateMadlib :: AlexInput -> Int -> Alex Token
beginStringTemplateMadlib i@(posn, prevChar, pending, input) len = do
  sourceTarget                           <- getCurrentSourceTarget
  (AlexUserState _ (strBuffer, _) _ _ _) <- getState
  pushStartCode stringTemplateMadlib
  resetStringTemplate
  resetCurlyCount
  return $ Token (makeArea posn (take len input)) sourceTarget (TokenStr strBuffer)

stringTemplateMadlibLeftCurly :: AlexInput -> Int -> Alex Token
stringTemplateMadlibLeftCurly i@(posn, prevChar, pending, input) len = do
  sourceTarget                       <- getCurrentSourceTarget
  (AlexUserState _ (_, count) _ _ _) <- getState
  updateCurlyCount $ count + 1
  return $ Token (makeArea posn (take len input)) sourceTarget TokenLeftCurly

stringTemplateMadlibRightCurly :: AlexInput -> Int -> Alex Token
stringTemplateMadlibRightCurly i@(posn, prevChar, pending, input) len = do
  sourceTarget                       <- getCurrentSourceTarget
  (AlexUserState _ (_, count) _ _ _) <- getState
  if count == 0 then do
    popStartCode
    begin stringTemplate i len
  else do
    updateCurlyCount $ count - 1
    return $ Token (makeArea posn (take len input)) sourceTarget TokenRightCurly

beginStringTemplate :: AlexInput -> Int -> Alex Token
beginStringTemplate i@(posn, prevChar, pending, input) len = do
  sourceTarget <- getCurrentSourceTarget
  pushStartCode stringTemplate
  return $ Token (makeArea posn (take len input)) sourceTarget TokenTemplateStringStart

endStringTemplate :: AlexInput -> Int -> Alex Token
endStringTemplate i@(posn, prevChar, pending, input) len = do
  sourceTarget                           <- getCurrentSourceTarget
  (AlexUserState _ (strBuffer, _) _ _ _) <- getState
  resetStringTemplate
  popStartCode
  return $ (Token (makeArea posn (take len input)) sourceTarget (TokenTemplateStringEnd strBuffer))

escapedStringTemplateContent :: AlexInput -> Int -> Alex Token
escapedStringTemplateContent i@(posn, prevChar, pending, input) len = do
  case take len input of
    "\\`" ->
      appendStringToTemplate "`"

    or ->
      appendStringToTemplate or
    -- "\\"  -> appendStringToTemplate ('\\':'\\':"")
    -- "\\n" -> appendStringToTemplate ('\n':"")
    -- "\\t" -> appendStringToTemplate ('\t':"")

  skip i len

pushStringToTemplate :: AlexInput -> Int -> Alex Token
pushStringToTemplate i@(posn, prevChar, pending, input) len = do
  appendStringToTemplate $ take len input
  skip i len


processIfTarget :: SourceTarget -> AlexInput -> Int -> Alex Token
processIfTarget sourceTarget i@(posn, prevChar, pending, input) len = do
  setCurrentSourceTarget sourceTarget
  return $ Token (makeArea posn (take len input)) sourceTarget TokenMacroIfTarget

processElseIfTarget :: SourceTarget -> AlexInput -> Int -> Alex Token
processElseIfTarget sourceTarget i@(posn, prevChar, pending, input) len = do
  setCurrentSourceTarget sourceTarget
  return $ Token (makeArea posn (take len input)) sourceTarget TokenMacroElseIf

processEndIfTarget :: AlexInput -> Int -> Alex Token
processEndIfTarget i@(posn, prevChar, pending, input) len = do
  setCurrentSourceTarget TargetAll
  return $ Token (makeArea posn (take len input)) TargetAll TokenMacroEndIf


jsxTextPopOut :: AlexInput -> Int -> Alex Token
jsxTextPopOut i@(posn, prevChar, pending, input) len = do
  sourceTarget <- getCurrentSourceTarget
  token        <-
        if take len input == ">" then do
          pushStartCode jsxText
          return TokenRightChevron
        else
          return TokenEOF

  return $ (Token (makeArea posn (take len input)) sourceTarget token)


decideTokenExport :: AlexInput -> Int -> Alex Token
decideTokenExport (posn, prevChar, pending, input) len = do
  sourceTarget <- getCurrentSourceTarget
  let next           = BLU.fromString $ take 300 input
      matchedTypeExp = match isTypeExport next :: Bool
      matchedTE      = match isTokenExport next :: Bool
      token          = 
        if matchedTE then
          TokenExport
        else if matchedTypeExp then
          TokenTypeExport
        else
          TokenExport
  
  return $ Token (makeArea posn (take len input)) sourceTarget token


decideTokenName :: AlexInput -> Int -> Alex Token
decideTokenName (posn, prevChar, pending, input) len = do
  sc           <- getStartCode
  sourceTarget <- getCurrentSourceTarget
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

  return $ Token (makeArea posn (take len input)) sourceTarget token


decideTokenLeftChevron :: AlexInput -> Int -> Alex Token
decideTokenLeftChevron (posn, prevChar, pending, input) len = do
  sourceTarget <- getCurrentSourceTarget
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
  return $ Token (makeArea posn (take len input)) sourceTarget token

decideTokenRightChevron :: AlexInput -> Int -> Alex Token
decideTokenRightChevron (posn, prevChar, pending, input) len = do
  sourceTarget <- getCurrentSourceTarget
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

  return $ Token (makeArea posn (take len input)) sourceTarget token


mapCharToken :: AlexInput -> Int -> Alex Token
mapCharToken inputData@(posn, prevChar, pending, input) len = do
  let charData   = take len input
      parser     = ReadP.readP_to_S $ ReadP.many $ ReadP.readS_to_P Char.readLitChar
      parsed     = fst $ last $ parser charData
      charData'  = parsed !! 1
      token      = TokenChar charData'

  if length parsed == 3 then do
    sourceTarget <- getCurrentSourceTarget
    return $ Token (makeArea posn (take len input)) sourceTarget token
  else do
    let Area (Loc a l c) _ = makeArea posn (take len input)
    alexError (printf "%d\n%d\nSyntax error - line: %d, column: %d\nThe following token is not valid: %s" l c l c (show token))


mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = do
  sc           <- getStartCode
  scs          <- getStartCodeStack
  sourceTarget <- getCurrentSourceTarget

  token <- case tokenizer (take len input) of
    TokenLeftCurly ->
      if sc == instanceHeader then do
        let next        = BLU.fromString $ ((tail . (take 70)) input)
            matchRecord = match recordTypeRegex next :: Bool
        if matchRecord then
          return TokenLeftCurly
        else do
          popStartCode
          return TokenLeftDoubleCurly
      -- else if sc == jsxText || sc == jsxOpeningTag || sc == jsxAutoClosed then do
      --   pushStartCode 0
      --   return TokenLeftCurly
      else do
        pushStartCode 0
        return TokenLeftCurly

    TokenLeftDoubleCurly ->
      if sc /= instanceHeader then do
        pushStartCode 0
        pushStartCode 0
        return TokenLeftDoubleCurly
      else
        return TokenLeftDoubleCurly

    tok ->
      return tok


  case token of
    TokenInstance -> pushStartCode instanceHeader
    TokenRightCurly -> do
      sc           <- getStartCode
      previousCode <- getPreviousStartCode
      if previousCode == jsxText || previousCode == jsxAutoClosed || previousCode == jsxOpeningTag then
        popStartCode
      else if sc /= instanceHeader then do
        popStartCode
      else
        return ()
    _ -> return ()

  return $ Token (makeArea posn (take len input)) sourceTarget token



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

tokenArea :: Token -> Area
tokenArea (Token area _ _) =
  area

tokenTarget :: Token -> SourceTarget
tokenTarget (Token _ sourceTarget _) =
  sourceTarget

data Token = Token Area SourceTarget TokenClass deriving (Eq, Show)

data TokenClass
 = TokenConst
 | TokenNumber String
 | TokenFloat String
 | TokenStr String
 | TokenChar Char
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
 | TokenSharpSign
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
 | TokenMacroIfTarget
 | TokenMacroElseIf
 | TokenMacroEndIf
 deriving (Eq, Show)


charData :: Token -> Char
charData (Token _ _ (TokenChar x)) = x


strV :: Token -> String
strV (Token _ _ (TokenStr x)) =
  x

strV (Token _ _ (TokenTemplateStringEnd x)) =
  x

strV (Token _ _ (TokenNumber x)) =
  x

strV (Token _ _ (TokenFloat x)) =
  x

strV (Token _ _ (TokenBool x)) =
  x

strV (Token _ _ (TokenName x)) =
  x

strV (Token _ _ (TokenConstraintName x)) =
  x

strV (Token _ _ (TokenJSBlock x)) =
  x



alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TargetAll TokenEOF)
}
