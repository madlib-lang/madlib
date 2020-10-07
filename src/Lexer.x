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

module Lexer
  ( Alex(..)
  , AlexState(..)
  , alexEOF
  , Token(..)
  , Pos(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPos
  , strV
  , intV
  , boolV
  )
where

import System.Exit
import Debug.Trace
}

%wrapper "monad"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$empty =  [\ \t\f\v\r]          -- equivalent to $white but without line return

tokens :-
  [\n]                                  { mapToken (\_ -> TokenReturn) }
  \"($printable # \")+\"                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  "--".*                                ;
  import                                { mapToken (\_ -> TokenImport) }
  data                                  { mapToken (\_ -> TokenData) }
  const                                 { mapToken (\_ -> TokenConst) }
  [=]                                   { mapToken (\_ -> TokenEq) }
  \+                                    { mapToken (\_ -> TokenPlus) }
  \-                                    { mapToken (\_ -> TokenDash) }
  \*                                    { mapToken (\_ -> TokenStar) }
  \/                                    { mapToken (\_ -> TokenSlash) }
  if                                    { mapToken (\_ -> TokenIf) }
  $digit+                               { mapToken (\s -> TokenInt (read s)) }
  "true"                                { mapToken (\_ -> (TokenBool True)) }
  "false"                               { mapToken (\_ -> (TokenBool False)) }
  "==="                                 { mapToken (\_ -> TokenTripleEq) }
  \,                                    { mapToken (\_ -> TokenComa) }
  \{                                    { mapToken (\_ -> TokenLeftCurly) }
  \}                                    { mapToken (\_ -> TokenRightCurly) }
  \(                                    { mapToken (\_ -> TokenLeftParen) }
  \)                                    { mapToken (\_ -> TokenRightParen) }
  \:\:                                  { mapToken (\_ -> TokenDoubleColon) }
  \-\>                                  { mapToken (\_ -> TokenArrow) }
  \=\>                                  { mapToken (\_ -> TokenFatArrow) }
  \|                                    { mapToken (\_ -> TokenPipe) }
  \;                                    { mapToken (\_ -> TokenSemiColon) }
  $alpha [$alpha $digit \_ \']*         { mapToken (\s -> TokenName s) }
  $empty+                               ;

{
sanitizeStr :: String -> String
sanitizeStr = tail . init

--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = return $ Token (makePos posn) token
  where token = trace (show $ tokenizer (take len input)) (tokenizer (take len input))

makePos :: AlexPosn -> Pos
makePos (AlexPn a l c) = Pos a l c

tokenToPos :: Token -> Pos
tokenToPos (Token x _) = x

data Token = Token Pos TokenClass deriving (Eq, Show)

data Pos = Pos Int Int Int deriving (Eq, Show)

data TokenClass
 = TokenConst
 | TokenInt  Int
 | TokenStr  String
 | TokenName String
 | TokenBool Bool
 | TokenIf
 | TokenEq
 | TokenPlus
 | TokenDash
 | TokenStar
 | TokenSlash
 | TokenTripleEq
 | TokenComa
 | TokenLeftCurly
 | TokenRightCurly
 | TokenLeftParen
 | TokenRightParen
 | TokenDoubleColon
 | TokenArrow
 | TokenFatArrow
 | TokenEOF
 | TokenImport
 | TokenPipe
 | TokenData
 | TokenSemiColon
 | TokenReturn
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))  = x
strV (Token _ (TokenName x)) = x

intV :: Token -> Int
intV (Token _ (TokenInt x)) = x

boolV :: Token -> Bool
boolV (Token _ (TokenBool x)) = x

alexEOF :: Alex Token
alexEOF = return (Token (Pos 1 1 1) TokenEOF)
}
