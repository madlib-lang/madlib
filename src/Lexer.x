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
  , TokenPos(..)
  , TokenClass(..)
  , AlexUserState
  , alexError
  , alexMonadScan
  , runAlex
  , alexInitUserState
  , getUserState
  , scanTokens
  , tokenToPos
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
  \"($printable # \")+\"                { pushToken (\s -> TokenStr s) }
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
pushToken :: (String -> TokenClass) -> AlexAction ()
pushToken tokenizer =
  \(posn, prevChar, pending, input) len -> do
    state <- modifyUserState (push posn (take len input))
    traceM $ "state: " ++ show state ++ show posn ++ " " ++ show input ++ " " ++ show len
    alexMonadScan
    where
       push :: AlexPosn -> String -> AlexUserState -> AlexUserState
       push pos input ts = ts ++ [(Token (makePos pos) (tokenizer input))]

-- The token type:
-- data Token = Token AlexPosn TokenClass
--  deriving (Show)

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