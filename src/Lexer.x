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
  , TokenPos(..)
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

tokens :-
  \"($printable # \")+\"                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  "--".*                                ;
  $white+                               ;
  const                                 { mapToken (\_ -> TokenConst) }
  [=]                                   { mapToken (\_ -> TokenEq) }
  if                                    { mapToken (\_ -> TokenIf) }
  $digit+                               { mapToken (\s -> TokenInt (read s)) }
  [\n]                                  { mapToken (\_ -> TokenReturn) }
  "true"                                { mapToken (\_ -> (TokenBool True)) }
  "false"                               { mapToken (\_ -> (TokenBool False)) }
  "==="                                 { mapToken (\_ -> TokenTripleEq) }
  -- Mix the two curlies and parens in one token with CLeft or CRight value ?
  "{"                                   { mapToken (\_ -> TokenLeftCurly) }
  "}"                                   { mapToken (\_ -> TokenRightCurly) }
  "("                                   { mapToken (\_ -> TokenLeftParen) }
  ")"                                   { mapToken (\_ -> TokenRightParen) }
  $alpha [$alpha $digit \_ \']*         { mapToken (\s -> TokenName s) }
  "::"                                  { mapToken (\_ -> TokenDoubleColon) }
  "->"                                  { mapToken (\_ -> TokenArrow) }

{
sanitizeStr :: String -> String
sanitizeStr = tail . init

--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = return (Token (makePos posn) (tokenizer (take len input)))

makePos :: AlexPosn -> TokenPos
makePos (AlexPn a l c) = TokenPos a l c

tokenToPos :: Token -> TokenPos
tokenToPos (Token x _) = x

data Token = Token TokenPos TokenClass deriving (Eq, Show)

data TokenPos = TokenPos Int Int Int deriving (Eq, Show)

data TokenClass
 = TokenConst
 | TokenInt    Int
 | TokenStr    String
 | TokenName   String
 | TokenBool   Bool
 | TokenIf
 | TokenEq
 | TokenTripleEq
 | TokenLeftCurly
 | TokenRightCurly
 | TokenLeftParen
 | TokenRightParen
 | TokenReturn
 | TokenDoubleColon
 | TokenArrow
 | TokenEOF
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))  = x
strV (Token _ (TokenName x)) = x

intV :: Token -> Int
intV (Token _ (TokenInt x)) = x

boolV :: Token -> Bool
boolV (Token _ (TokenBool x)) = x

alexEOF :: Alex Token
alexEOF = return (Token (TokenPos 1 1 1) TokenEOF)
}
