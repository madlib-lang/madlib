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
  \"($printable # \")+\"                { mapToken TTStr }
  "--".*                                ;
  $white+                               ;
  const                                 { mapToken TTConst }
  [=]                                   { mapToken TTEq }
  $alpha [$alpha $digit \_ \']*         { mapToken TTVar }
  $digit+                               { mapToken TTInt }--(\s -> TokenInt (read s)) }
  [\n]                                  { mapToken TTReturn }

{
data TokenType = TTStr | TTConst | TTEq | TTVar | TTInt | TTReturn

-- replace cl with a function again and get rid of TokenType
--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: TokenType -> AlexInput -> Int -> Alex Token
mapToken cl (posn, prevChar, pending, input) len = case cl of
  TTStr    -> return (Token (makePos posn) (TokenStr (take len input)))
  TTConst  -> return (Token (makePos posn) TokenConst)
  TTEq     -> return (Token (makePos posn) TokenEq)
  TTReturn -> return (Token (makePos posn) TokenReturn)
  TTInt    -> return (Token (makePos posn) (TokenInt $ read (take len input)))
  TTVar    -> return (Token (makePos posn) (TokenVar (take len input)))
  _        -> return (Token (makePos posn) TokenConst)

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
