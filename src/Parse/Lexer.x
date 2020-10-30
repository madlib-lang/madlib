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
  , tokenToLoc
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

tokens :-
  [\n]                                  { mapToken (\_ -> TokenReturn) }
  \"($printable # \")+\"                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  [\ \n]*"--".*                         ;
  import                                { mapToken (\_ -> TokenImport) }
  export                                { mapToken (\_ -> TokenExport) }
  from                                  { mapToken (\_ -> TokenFrom) }
  data                                  { mapToken (\_ -> TokenData) }
  const                                 { mapToken (\_ -> TokenConst) }
  if                                    { mapToken (\_ -> TokenIf) }
  else                                  { mapToken (\_ -> TokenElse) }
  switch                                { mapToken (\_ -> TokenSwitch) }
  case                                  { mapToken (\_ -> TokenCase) }
  \=                                    { mapToken (\_ -> TokenEq) }
  $digit+                               { mapToken (\s -> TokenInt s) }
  "True"                                { mapToken (\_ -> (TokenBool "True")) }
  "False"                               { mapToken (\_ -> (TokenBool "False")) }
  "==="                                 { mapToken (\_ -> TokenTripleEq) }
  \.                                    { mapToken (\_ -> TokenDot) }
  \,                                    { mapToken (\_ -> TokenComma) }
  \{                                    { mapToken (\_ -> TokenLeftCurly) }
  \}                                    { mapToken (\_ -> TokenRightCurly) }
  \[                                    { mapToken (\_ -> TokenLeftSquaredBracket) }
  \]                                    { mapToken (\_ -> TokenRightSquaredBracket) }
  \(                                    { mapToken (\_ -> TokenLeftParen) }
  \)                                    { mapToken (\_ -> TokenRightParen) }
  \:\:                                  { mapToken (\_ -> TokenDoubleColon) }
  \:                                    { mapToken (\_ -> TokenColon) }
  \-\>                                  { mapToken (\_ -> TokenArrow) }
  \=\>                                  { mapToken (\_ -> TokenFatArrow) }
  \|                                    { mapToken (\_ -> TokenPipe) }
  \;                                    { mapToken (\_ -> TokenSemiColon) }
  [$alpha \_] [$alpha $digit \_ \']*    { mapToken (\s -> TokenName s) }
  \#\- [$alpha $digit \_ \' \ \+ \. \, \( \) \; \: \{ \} \n \= \> \\ \/]* \-\#   { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  [\n \ ]*\+                            { mapToken (\_ -> TokenPlus) }
  \-                                    { mapToken (\_ -> TokenDash) }
  \n[\ ]*\-                             { mapToken (\_ -> TokenDash) }
  [\n \ ]*\*                            { mapToken (\_ -> TokenStar) }
  [\n \ ]*\/                            { mapToken (\_ -> TokenSlash) }
  [\n \ ]*\|\>                          { mapToken (\_ -> TokenPipeOperator) }
  $empty+                               ;

{
sanitizeStr :: String -> String
sanitizeStr = tail . init

sanitizeJSBlock :: String -> String
sanitizeJSBlock = strip . tail . tail . init . init

strip  = T.unpack . T.strip . T.pack

--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = return $ Token (makeLoc posn) token
  where token = trace (show $ tokenizer (take len input)) (tokenizer (take len input))

makeLoc :: AlexPosn -> Loc
makeLoc (AlexPn a l c) = Loc a l c

tokenToLoc :: Token -> Loc
tokenToLoc (Token x _) = x

data Token = Token Loc TokenClass deriving (Eq, Show)

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
 | TokenSwitch
 | TokenCase
 | TokenEq
 | TokenPlus
 | TokenDash
 | TokenStar
 | TokenSlash
 | TokenTripleEq
 | TokenComma
 | TokenLeftCurly
 | TokenRightCurly
 | TokenLeftSquaredBracket
 | TokenRightSquaredBracket
 | TokenLeftParen
 | TokenRightParen
 | TokenDoubleColon
 | TokenColon
 | TokenDot
 | TokenArrow
 | TokenFatArrow
 | TokenEOF
 | TokenImport
 | TokenExport
 | TokenFrom
 | TokenPipe
 | TokenPipeOperator
 | TokenData
 | TokenSemiColon
 | TokenReturn
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))  = x
strV (Token _ (TokenInt x)) = x
strV (Token _ (TokenBool x)) = x
strV (Token _ (TokenName x)) = x
strV (Token _ (TokenJSBlock x)) = x


-- intV :: Token -> Int
-- intV (Token _ (TokenInt x)) = x

-- boolV :: Token -> Bool
-- boolV (Token _ (TokenBool x)) = x

alexEOF :: Alex Token
alexEOF = return (Token (Loc 1 1 1) TokenEOF)
}
