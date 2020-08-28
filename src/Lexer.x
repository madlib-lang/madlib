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
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , AlexUserState
  , alexError
  , alexMonadScan
  , runAlex
  , alexInitUserState
  , getUserState
  )
where

import System.Exit
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-
  "--".*                                ;
  (" "|\t)                     ;
  const                                 { pushToken (\s -> TokenConst) }
  [=]                                   { pushToken (\s -> TokenEq) }
  $alpha [$alpha $digit \_ \']*         { pushToken (\s -> TokenVar s) }
  $digit+                               { pushToken (\s -> TokenInt (read s)) }
  [\n]                                  { pushToken (\s -> TokenReturn) }
{
-- s :: AlexUserState -> AlexUserState
-- alex_ust :: AlexState -> AlexUserState
-- -> Returns the current state from AlexState.
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let current = alex_ust s
                                     new     = f current
                                 in
                                   Right (s { alex_ust = new },())

-- Each action per token should be a value of result `AlexAction`.
-- type AlexAction a = AlexInput -> Int -> Alex a
-- type AlexInput = (AlexPosn, Char, [Byte], String)
pushToken :: (String -> Token) -> AlexAction ()
pushToken tokenizer =
  \(posn,prevChar,pending,s) len -> modifyUserState (push $ take len s) >> alexMonadScan
    where
       push :: String -> AlexUserState -> AlexUserState
       push s ts = ts ++ [(tokenizer s)]

-- The token type:
-- data Token = Token AlexPosn TokenClass
--  deriving (Show)

data Token
 = TokenConst
 | TokenInt    Int
 | TokenVar    String
 | TokenReturn
 | TokenEq
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex ()
alexEOF = return ()

-- 

type AlexUserState = [Token]
alexInitUserState = []

-- type AlexUserState = [Token]

-- alexInitUserState :: AlexUserState
-- alexInitUserState = []

-- Returns the current state.
-- I.e., a list of tokens.
getUserState :: Alex AlexUserState 
getUserState = Alex $ \s -> Right (s,alex_ust s)

}