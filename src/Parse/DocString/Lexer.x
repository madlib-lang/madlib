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
  , getDocStringCharacter
  , getFunctionName
  , getTypeName
  , getInterfaceName
  , getInstanceName
  )
where

import           Control.Monad.State
import           System.Exit
import qualified Data.Text     as T
import           Explain.Location
import           Text.Regex.TDFA
import           Debug.Trace (trace)
import           Text.Show.Pretty (ppShow)
}

%wrapper "monadUserState"

tokens :-
  <0> \/\*\*            { beginDocString }
  <docString> \*\/      { endDocString }
  <docString> \@example { mapToken (\_ -> TokenExampleStart) }
  <docString> \@since   { mapToken (\_ -> TokenSinceStart) }
  <docString> (.|\n)    { mapToken (\input -> TokenDocStringCharacter input) }
  <0> (.|\n)            ;

{
regexFunctionDocString :: String
regexFunctionDocString = "\\`\n[ ]*export[\n\\ ]*([a-zA-Z0-9_]*)[\n\\ ]*=.*"

regexTypingDocString :: String
regexTypingDocString = "\\`\n[ ]*([a-zA-Z0-9_]*)[ ]*\\:\\:.*"

regexTypeDefDocString :: String
regexTypeDefDocString = "\\`\n[ ]*export (type|alias)[ ]*([a-zA-Z0-9_]*)[a-zA-Z0-9_ \n]*\\=.*"

regexInterfaceDocString :: String
regexInterfaceDocString = "\\`\n[ ]*interface([^=]*=>)?[ ]*([a-zA-Z0-9_]*)[a-zA-Z0-9_ \n]*\\{.*"

regexInstanceDocString :: String
regexInstanceDocString = "\\`\n[ ]*instance([^=]*=>)?[ ]*([a-zA-Z0-9_ ()<>]*)\\{.*"

regexEndOfDocString :: String
regexEndOfDocString = "\\*\\/"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False, lastStarGreedy = False, rightAssoc = False } defaultExecOpt



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
  let matchedTyping = match (toRegex regexTypingDocString) afterDocString :: (String, String, String, [String])
  let matchedFn = match (toRegex regexFunctionDocString) afterDocString :: (String, String, String, [String])
  let matchedTypeDec = match (toRegex regexTypeDefDocString) afterDocString :: (String, String, String, [String])
  let matchedInterface = match (toRegex regexInterfaceDocString) afterDocString :: (String, String, String, [String])
  let matchedInstance = match (toRegex regexInstanceDocString) afterDocString :: (String, String, String, [String])

  case (matchedFn, matchedTyping, matchedTypeDec, matchedInterface, matchedInstance) of
    ((_, _, _, [fnName]), _, _, _, _)      -> return $ TokenFunctionDocStringStart fnName
    (_, (_, _, _, [fnName]), _, _, _)      -> return $ TokenFunctionDocStringStart fnName
    (_, _, (_, _, _, [_, typeName]), _, _) -> return $ TokenTypeDefDocStringStart typeName
    (_, _, _, (_, _, _, [_, typeName]), _) -> return $ TokenInterfaceDocStringStart typeName
    (_, _, _, _, (_, _, _, [_, typeName])) -> return $ TokenInstanceDocStringStart typeName
    _                                      -> return $ TokenModuleDocStringStart

endDocString :: AlexInput -> Int -> Alex Token
endDocString i@(posn, prevChar, pending, input) len = do
  setDefaultStartCode
  return $ TokenDocStringEnd


mapToken :: (String -> Token) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (_, _, _, input) len = return $ tokenizer (take len input)


data Token
  = TokenModuleDocStringStart
  | TokenFunctionDocStringStart String
  | TokenTypeDefDocStringStart String
  | TokenInterfaceDocStringStart String
  | TokenInstanceDocStringStart String
  | TokenDocStringEnd
  | TokenExampleStart
  | TokenSinceStart
  | TokenDocStringCharacter String
  | TokenEOF
 deriving (Eq, Show)

getDocStringCharacter :: Token -> String
getDocStringCharacter (TokenDocStringCharacter s) = s

getFunctionName :: Token -> String
getFunctionName (TokenFunctionDocStringStart n) = n

getTypeName :: Token -> String
getTypeName (TokenTypeDefDocStringStart n) = n

getInterfaceName :: Token -> String
getInterfaceName (TokenInterfaceDocStringStart n) = n

getInstanceName :: Token -> String
getInstanceName (TokenInstanceDocStringStart n) = n


alexEOF :: Alex Token
alexEOF = return TokenEOF
}
