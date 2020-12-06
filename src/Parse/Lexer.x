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

import           System.Exit
import qualified Data.Text     as T
import           Explain.Location
import           Text.Regex.TDFA
}

%wrapper "monad"

$alpha    = [a-zA-Z]                        -- alphabetic characters
$empty    = [\ \t\f\v\r]                    -- equivalent to $white but without line return
$head     = [\n \ ]                         -- leading whitespace and / or newline
$tail     = [\n]                            -- trailing newline

$digit    = 0-9                             -- digits
@decimal  = $digit($digit)*                 -- decimal
@negative = \-
@signed = @negative ?
@floating = @decimal \. @decimal | @decimal -- floating point

tokens :-
  import                                { mapToken (\_ -> TokenImport) }
  export                                { mapToken (\_ -> TokenExport) }
  from                                  { mapToken (\_ -> TokenFrom) }
  data                                  { mapToken (\_ -> TokenData) }
  alias                                 { mapToken (\_ -> TokenAlias) }
  const                                 { mapToken (\_ -> TokenConst) }
  if                                    { mapToken (\_ -> TokenIf) }
  else                                  { mapToken (\_ -> TokenElse) }
  where                                 { mapToken (\_ -> TokenWhere) }
  pipe                                  { mapToken (\_ -> TokenPipeKeyword) }
  return                                { mapToken (\_ -> TokenReturnKeyword) }
  is                                    { mapToken (\_ -> TokenIs) }
  \=                                    { mapToken (\_ -> TokenEq) }
  @signed @floating                     { mapToken (\s -> TokenNumber s) }
  "true"                                { mapToken (\_ -> (TokenBool "true")) }
  "false"                               { mapToken (\_ -> (TokenBool "false")) }
  "=="                                  { mapToken (\_ -> TokenDoubleEq) }
  "!="                                  { mapToken (\_ -> TokenExclamationMarkEq) }
  \.                                    { mapToken (\_ -> TokenDot) }
  $head*\,$tail*                        { mapToken (\_ -> TokenComma) }
  \{$tail*                              { mapToken (\_ -> TokenLeftCurly) }
  $head*\}                              { mapToken (\_ -> TokenRightCurly) }
  \[$tail*                              { mapToken (\_ -> TokenLeftSquaredBracket) }
  $head*\]                              { mapToken (\_ -> TokenRightSquaredBracket) }
  \(                                    { mapToken (\_ -> TokenLeftParen) }
  \($tail*                              { mapToken (\_ -> TokenLeftParen) }
  $head*\)                              { mapToken (\_ -> TokenRightParen) }
  \)                                    { mapToken (\_ -> TokenRightParen) }
  $head*\:\:$tail*                      { mapToken (\_ -> TokenDoubleColon) }
  \:                                    { mapToken (\_ -> TokenColon) }
  $head*\-\>$tail*                      { mapToken (\_ -> TokenArrow) }
  $head*\=\>$tail*                      { mapToken (\_ -> TokenFatArrow) }
  \|                                    { mapToken (\_ -> TokenPipe) }
  \;                                    { mapToken (\_ -> TokenSemiColon) }
  [\n]                                  { mapToken (\_ -> TokenReturn) }
  [$alpha \_] [$alpha $digit \_ \']*    { mapToken (\s -> TokenName s) }
  $head*\+                              { mapToken (\_ -> TokenPlus) }
  $head*\+\+                            { mapToken (\_ -> TokenDoublePlus) }
  \-                                    { mapToken (\_ -> TokenDash) }
  $head*\?                              { mapToken (\_ -> TokenQuestionMark) }
  \n[\ ]*\-                             { mapToken (\_ -> TokenDash) }
  $head*\*                              { mapToken (\_ -> TokenStar) }
  $head*\/                              { mapToken (\_ -> TokenSlash) }
  $head*\%                              { mapToken (\_ -> TokenPercent) }
  $head*\|\>                            { mapToken (\_ -> TokenPipeOperator) }
  \.\.\.                                { mapToken (\_ -> TokenSpreadOperator) }
  \&\&                                  { mapToken (\_ -> TokenDoubleAmpersand) }
  \|\|                                  { mapToken (\_ -> TokenDoublePipe) }
  \>                                    { mapToken (\_ -> TokenRightChevron) }
  \<                                    { mapToken (\_ -> TokenLeftChevron) }
  \>\=                                  { mapToken (\_ -> TokenRightChevronEq) }
  \<\=                                  { mapToken (\_ -> TokenLeftChevronEq) }
  \!                                    { mapToken (\_ -> TokenExclamationMark) }
  \"($printable # \")*\"                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  '($printable # ')*'                   { mapToken (\s -> TokenStr (sanitizeStr s)) }
  `($printable # `)*`                   { mapToken (\s -> TokenStr (sanitizeStr s)) }
  \#\- [$alpha $digit \" \_ \' \` \$ \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/]* \-\#
    { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  [\ \n]*"//".*                         ; -- Comments
  $empty+                               ;

{
blackList :: String
blackList = "\\`[\ \t \n]*(where|if|else|is|data|alias|export|}|[a-zA-Z0-9]+[\ \t \n]*[=]+|[a-zA-Z0-9]+[\ \t \n]*(::)+).*"


whiteList :: String
whiteList = "\\`[\ \t \n]*[a-zA-Z0-9\"]+[\\(]?.*"


--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = do
  return $ Token (makeArea posn (take len input)) token
  
  -- where token = tokenizer (take len input)
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
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))  = x
strV (Token _ (TokenNumber x)) = x
strV (Token _ (TokenBool x)) = x
strV (Token _ (TokenName x)) = x
strV (Token _ (TokenJSBlock x)) = x


alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TokenEOF)
}
