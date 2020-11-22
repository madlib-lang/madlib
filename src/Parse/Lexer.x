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
import           Debug.Trace
import qualified Data.Text     as T
import           Explain.Location
}

%wrapper "monad"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$empty =  [\ \t\f\v\r]          -- equivalent to $white but without line return
$head = [\n \ ]                 -- leading whitespace and / or newline
$tail = [\n]                    -- trailing newline

tokens :-
  import                                { mapToken (\_ -> TokenImport) }
  export                                { mapToken (\_ -> TokenExport) }
  from                                  { mapToken (\_ -> TokenFrom) }
  data                                  { mapToken (\_ -> TokenData) }
  const                                 { mapToken (\_ -> TokenConst) }
  if                                    { mapToken (\_ -> TokenIf) }
  else                                  { mapToken (\_ -> TokenElse) }
  where                                 { mapToken (\_ -> TokenWhere) }
  is                                    { mapToken (\_ -> TokenIs) }
  \=                                    { mapToken (\_ -> TokenEq) }
  $digit+                               { mapToken (\s -> TokenInt s) }
  "true"                                { mapToken (\_ -> (TokenBool "true")) }
  "false"                               { mapToken (\_ -> (TokenBool "false")) }
  "=="                                  { mapToken (\_ -> TokenDoubleEq) }
  \.                                    { mapToken (\_ -> TokenDot) }
  $head*\,$tail*                        { mapToken (\_ -> TokenComma) }
  \{$tail*                              { mapToken (\_ -> TokenLeftCurly) }
  $head*\}                              { mapToken (\_ -> TokenRightCurly) }
  \[$tail*                              { mapToken (\_ -> TokenLeftSquaredBracket) }
  $head*\]                              { mapToken (\_ -> TokenRightSquaredBracket) }
  \(                                    { mapToken (\_ -> TokenLeftParen) }
  \($tail*                              { mapToken (\_ -> TokenLeftParen) }
  $head*\)                              { mapToken (\_ -> TokenRightParen) }
  \:\:                                  { mapToken (\_ -> TokenDoubleColon) }
  \:                                    { mapToken (\_ -> TokenColon) }
  \-\>                                  { mapToken (\_ -> TokenArrow) }
  \=\>                                  { mapToken (\_ -> TokenFatArrow) }
  \|                                    { mapToken (\_ -> TokenPipe) }
  \;                                    { mapToken (\_ -> TokenSemiColon) }
  [\n]                                  { mapToken (\_ -> TokenReturn) }
  [$alpha \_] [$alpha $digit \_ \']*    { mapToken (\s -> TokenName s) }
  $head*\+                              { mapToken (\_ -> TokenPlus) }
  \-                                    { mapToken (\_ -> TokenDash) }
  $head*\?                              { mapToken (\_ -> TokenQuestionMark) }
  \n[\ ]*\-                             { mapToken (\_ -> TokenDash) }
  $head*\*                              { mapToken (\_ -> TokenStar) }
  $head*\/                              { mapToken (\_ -> TokenSlash) }
  $head*\|\>                            { mapToken (\_ -> TokenPipeOperator) }
  \.\.\.                                { mapToken (\_ -> TokenSpreadOperator) }
  \&\&                                  { mapToken (\_ -> TokenDoubleAmpersand) }
  \|\|                                  { mapToken (\_ -> TokenDoublePipe) }
  \>                                    { mapToken (\_ -> TokenRightChevron) }
  \<                                    { mapToken (\_ -> TokenLeftChevron) }
  \>\=                                  { mapToken (\_ -> TokenRightChevronEq) }
  \<\=                                  { mapToken (\_ -> TokenLeftChevronEq) }
  \!                                    { mapToken (\_ -> TokenExclamationMark) }
  \"($printable # \")+\"                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  '($printable # ')+'                   { mapToken (\s -> TokenStr (sanitizeStr s)) }
  `($printable # `)+`                   { mapToken (\s -> TokenStr (sanitizeStr s)) }
  \#\- [$alpha $digit \" \_ \' \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/]* \-\#
    { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  [\ \n]*"//".*                         ; -- Comments
  $empty+                               ;

{
sanitizeStr :: String -> String
sanitizeStr = tail . init

sanitizeJSBlock :: String -> String
sanitizeJSBlock = strip . tail . tail . init . init

strip  = T.unpack . T.strip . T.pack

--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = return $ Token (makeArea posn (take len input)) token
  -- where token = tokenizer (take len input)
  where token = trace (show $ tokenizer (take len input)) (tokenizer (take len input))

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
 | TokenInt  String
 | TokenStr  String
 | TokenName String
 | TokenDottedName String
 | TokenJSBlock String
 | TokenBool String
 | TokenIf
 | TokenElse
 | TokenWhere
 | TokenIs
 | TokenEq
 | TokenPlus
 | TokenDash
 | TokenStar
 | TokenSlash
 | TokenDoubleEq
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
 | TokenSemiColon
 | TokenReturn
 | TokenDoubleAmpersand
 | TokenDoublePipe
 | TokenRightChevron
 | TokenLeftChevron
 | TokenRightChevronEq
 | TokenLeftChevronEq
 | TokenExclamationMark
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))  = x
strV (Token _ (TokenInt x)) = x
strV (Token _ (TokenBool x)) = x
strV (Token _ (TokenName x)) = x
strV (Token _ (TokenJSBlock x)) = x


alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TokenEOF)
}
