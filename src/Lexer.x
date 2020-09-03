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
  $alpha [$alpha $digit \_ \']*         { mapToken (\s -> TokenVar s) }
  $digit+                               { mapToken (\s -> TokenInt (read s)) }
  [\n]                                  { mapToken (\_ -> TokenReturn) }

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
 | TokenVar    String
 | TokenEq
 | TokenReturn
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return (Token (TokenPos 1 1 1) TokenEOF)
}
