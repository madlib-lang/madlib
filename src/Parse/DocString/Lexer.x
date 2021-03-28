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

module Parse.DocString.Lexer
  ( Alex(..)
  , AlexState(..)
  , alexEOF
  , Token(..)
  , alexError
  , alexMonadScan
  , runAlex
  , getModuleDescCharacter
  , getFunctionName
  )
where

import           Control.Monad.State
import           System.Exit
import qualified Data.Text     as T
import           Explain.Location
import           Text.Regex.TDFA
-- import           Text.Regex.PCRE
-- import Data.Bits ((.|.))
import           Debug.Trace (trace)
import           Text.Show.Pretty (ppShow)
}

%wrapper "monadUserState"

$alpha    = [a-zA-Z]                        -- alphabetic characters
$empty    = [\ \t\f\v\r]                    -- equivalent to $white but without line return
$head     = [\n \ ]                         -- leading whitespace and / or newline
$tail     = [\n]                            -- trailing newline
$multilineStringContent = [$printable \n]

$digit    = 0-9                             -- digits
@decimal  = $digit($digit)*                 -- decimal
@negative = \-
@signed = @negative ?
@floating = @decimal \. @decimal | @decimal -- floating point

tokens :-
  <0> \/\*\*          { beginDocString }
  <docString>   \*\/  { endDocString }
  <docString> (.|\n)  { mapToken (\input -> TokenModuleDescriptionCharacter input) }
  <0> (.|\n)          ;
{
regexFunctionDocString :: String
regexFunctionDocString = "\\`\n[ ]*export[\n\\ ]*([a-zA-Z0-9_]*)[\n\\ ]*=.*"

regexTypeDefDocString :: String
regexTypeDefDocString = "\\`\n[ ]*([a-zA-Z0-9_]*)[ ]*\\:\\:.*"

regexEndOfDocString :: String
regexEndOfDocString = "\\*\\/"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False, lastStarGreedy = False, rightAssoc = True } defaultExecOpt



data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

setStartCode :: Int -> Alex ()
setStartCode code =
  Alex $ \s ->
    Right (s{ alex_scd = code }, ())

getStartCode :: Alex Int
getStartCode = Alex $ \s -> do
  let sc = alex_scd s
  Right (s, sc)

setDefaultStartCode :: Alex ()
setDefaultStartCode =
  Alex $ \s ->
    Right
      ( s{ alex_scd = 0 }
      , ()
      )

getState :: Alex AlexUserState
getState = Alex $ \s@AlexState{alex_ust = state} -> Right (s, state)


beginDocString :: AlexInput -> Int -> Alex Token
beginDocString i@(posn, prevChar, pending, input) len = do
  setStartCode docString

  let (_, _, afterDocString) = match (toRegex regexEndOfDocString) (take 1000 input) :: (String, String, String)
  let matchedTyping = match (toRegex regexTypeDefDocString) afterDocString :: (String, String, String, [String])
  let matchedFn = match (toRegex regexFunctionDocString) afterDocString :: (String, String, String, [String])
  case (matchedFn, matchedTyping) of
    ((_, _, _, [fnName]), _) -> return $ TokenFunctionDocStringStart fnName
    (_, (_, _, _, [fnName])) -> return $ TokenFunctionDocStringStart fnName
    _                        -> return $ TokenModuleDocStringStart

endDocString :: AlexInput -> Int -> Alex Token
endDocString i@(posn, prevChar, pending, input) len = do
  setDefaultStartCode
  return $ TokenDocStringEnd


mapToken :: (String -> Token) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (_, _, _, input) len = do
  return $ tokenizer (take len input)

description :: (String -> Token) -> AlexInput -> Int -> Alex Token
description tokenizer (_, _, _, input) len = do
  setDefaultStartCode
  return $ tokenizer $ init (init (take len input))


data Token
  = TokenModuleDocStringStart
  | TokenFunctionDocStringStart String
  | TokenDocStringEnd
  | TokenModuleDescriptionCharacter String
  | TokenEOF
 deriving (Eq, Show)

getModuleDescCharacter :: Token -> String
getModuleDescCharacter (TokenModuleDescriptionCharacter s) = s

getFunctionName :: Token -> String
getFunctionName (TokenFunctionDocStringStart n) = n


alexEOF :: Alex Token
alexEOF = return TokenEOF
}
