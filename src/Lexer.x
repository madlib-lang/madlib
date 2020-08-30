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
  , scanTokens
  )
where

import System.Exit
import Debug.Trace
}

%wrapper "monadUserState"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-
  "--".*                                ;
  $white+                               ;
  const                                 { pushToken (\s -> TokenConst) }
  [=]                                   { pushToken (\s -> TokenEq) }
  $alpha [$alpha $digit \_ \']*         { pushToken (\s -> TokenVar s) }
  $digit+                               { pushToken (\s -> TokenInt (read s)) }
  \"($printable # \")+\"                  { pushToken (\s -> TokenStr s) }
  [\n]                                  { pushToken (\s -> TokenReturn) }

{
type AlexUserState = [Token]

alexInitUserState :: AlexUserState
alexInitUserState = []

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
  \(posn, prevChar, pending, s) len -> do
    state <- modifyUserState (push $ take len s)
    traceM $ "state: " ++ show state ++ show posn ++ " " ++ show s ++ " " ++ show pending
    alexMonadScan
    where
       push :: String -> AlexUserState -> AlexUserState
       push s ts = ts ++ [(tokenizer s)]

-- The token type:
-- data Token = Token AlexPosn TokenClass
--  deriving (Show)

data Token
 = TokenConst
 | TokenInt    Int
 | TokenStr    String
 | TokenVar    String
 | TokenEq
 | TokenReturn
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex ()
alexEOF = return ()

-- 


-- Returns the current state.
-- I.e., a list of tokens.
getUserState :: Alex AlexUserState 
getUserState = Alex $ \s -> Right (s,alex_ust s)

scanTokens :: String -> Either String AlexUserState
scanTokens s = runAlex s $ alexMonadScan >> getUserState

}