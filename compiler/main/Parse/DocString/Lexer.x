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
  , tokenTarget
  )
where

import           Control.Monad.State
import           System.Exit
import qualified Data.Text                as T
import           Explain.Location
import           Text.Regex.TDFA
import           AST.Source
}

%wrapper "monadUserState"

tokens :-
  <0> \/\*\*               { beginDocString }
  <docString> \*\/         { endDocString }
  <docString> \@example    { mapToken (\_ sourceTarget -> TokenExampleStart sourceTarget) }
  <docString> \@since      { mapToken (\_ sourceTarget -> TokenSinceStart sourceTarget) }
  <docString> (.|\n)       { mapToken (\input sourceTarget -> TokenDocStringCharacter sourceTarget input) }
  <0> \#iftarget[\ ]*llvm  { processIfTarget TargetLLVM }
  <0> \#iftarget[\ ]*js    { processIfTarget TargetJS }
  <0> \#elseif[\ ]*llvm    { processElseIfTarget TargetLLVM }
  <0> \#elseif[\ ]*js      { processElseIfTarget TargetJS }
  <0> \#endif              { processEndIfTarget }
  <0> (.|\n)               ;

{
regexFunctionDocString :: String
regexFunctionDocString = "\\`\n[ ]*export[\n\\ ]*([a-zA-Z0-9_]*)[\n\\ ]*=.*"

regexTypingDocString :: String
regexTypingDocString = "\\`\n[ ]*([a-zA-Z0-9_]*)[ ]*\\:\\:.*"

regexTypeDefDocString :: String
regexTypeDefDocString = "\\`\n[ ]*(export )?(type|alias)[ ]*([a-zA-Z0-9_]*)[a-zA-Z0-9_ \n]*.*"

regexInterfaceDocString :: String
regexInterfaceDocString = "\\`\n[ ]*interface([^=]*=>)?[ ]*([a-zA-Z0-9_]*)[a-zA-Z0-9_ \n]*\\{.*"

regexInstanceDocString :: String
regexInstanceDocString = "\\`\n[ ]*instance([^=]*=>)?[ ]*([a-zA-Z0-9_ ()<>]*)\\{.*"

regexEndOfDocString :: String
regexEndOfDocString = "\\*\\/"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False, lastStarGreedy = False, rightAssoc = False } defaultExecOpt



data AlexUserState = AlexUserState SourceTarget

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState TargetAll


getState :: Alex AlexUserState
getState = Alex $ \s@AlexState{alex_ust = state} -> Right (s, state)

setState :: AlexUserState -> Alex ()
setState state = Alex $ \s -> Right (s{ alex_ust = state }, ())


setCurrentSourceTarget :: SourceTarget -> Alex ()
setCurrentSourceTarget sourceTarget = do
  setState $ AlexUserState sourceTarget

getCurrentSourceTarget :: Alex SourceTarget
getCurrentSourceTarget = do
  (AlexUserState sourceTarget) <- getState
  return sourceTarget


processIfTarget :: SourceTarget -> AlexInput -> Int -> Alex Token
processIfTarget sourceTarget i@(posn, prevChar, pending, input) len = do
  setCurrentSourceTarget sourceTarget
  skip i len
  -- return $ Token (makeArea posn (take len input)) sourceTarget TokenMacroIfTarget

processElseIfTarget :: SourceTarget -> AlexInput -> Int -> Alex Token
processElseIfTarget sourceTarget i@(posn, prevChar, pending, input) len = do
  setCurrentSourceTarget sourceTarget
  skip i len
  -- return $ Token (makeArea posn (take len input)) sourceTarget TokenMacroElseIf

processEndIfTarget :: AlexInput -> Int -> Alex Token
processEndIfTarget i@(posn, prevChar, pending, input) len = do
  setCurrentSourceTarget TargetAll
  skip i len
  -- return $ Token (makeArea posn (take len input)) TargetAll TokenMacroEndIf



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



beginDocString :: AlexInput -> Int -> Alex Token
beginDocString i@(posn, prevChar, pending, input) len = do
  setStartCode docString

  sourceTarget <- getCurrentSourceTarget

  let (_, _, afterDocString) = match (toRegex regexEndOfDocString) (take 1000 input) :: (String, String, String)
  let matchedTyping          = match (toRegex regexTypingDocString) afterDocString :: (String, String, String, [String])
  let matchedFn              = match (toRegex regexFunctionDocString) afterDocString :: (String, String, String, [String])
  let matchedTypeDec         = match (toRegex regexTypeDefDocString) afterDocString :: (String, String, String, [String])
  let matchedInterface       = match (toRegex regexInterfaceDocString) afterDocString :: (String, String, String, [String])
  let matchedInstance        = match (toRegex regexInstanceDocString) afterDocString :: (String, String, String, [String])

  case (matchedFn, matchedTyping, matchedTypeDec, matchedInterface, matchedInstance) of
    ((_, _, _, [fnName]), _, _, _, _) ->
      return $ TokenFunctionDocStringStart sourceTarget fnName

    (_, (_, _, _, [fnName]), _, _, _) ->
      return $ TokenFunctionDocStringStart sourceTarget fnName

    (_, _, (_, _, _, [_, _, typeName]), _, _) ->
      return $ TokenTypeDefDocStringStart sourceTarget typeName

    (_, _, _, (_, _, _, [_, typeName]), _) ->
      return $ TokenInterfaceDocStringStart sourceTarget typeName

    (_, _, _, _, (_, _, _, [_, typeName])) ->
      return $ TokenInstanceDocStringStart sourceTarget typeName

    _ ->
      return $ TokenModuleDocStringStart sourceTarget


endDocString :: AlexInput -> Int -> Alex Token
endDocString i@(posn, prevChar, pending, input) len = do
  sourceTarget <- getCurrentSourceTarget
  setDefaultStartCode
  return $ TokenDocStringEnd sourceTarget


mapToken :: (String -> SourceTarget -> Token) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (_, _, _, input) len = do
  sourceTarget <- getCurrentSourceTarget
  return $ tokenizer (take len input) sourceTarget


data Token
  = TokenModuleDocStringStart SourceTarget
  | TokenFunctionDocStringStart SourceTarget String
  | TokenTypeDefDocStringStart SourceTarget String
  | TokenInterfaceDocStringStart SourceTarget String
  | TokenInstanceDocStringStart SourceTarget String
  | TokenDocStringEnd SourceTarget
  | TokenExampleStart SourceTarget
  | TokenSinceStart SourceTarget
  | TokenDocStringCharacter SourceTarget String
  | TokenEOF
 deriving (Eq, Show)

getDocStringCharacter :: Token -> String
getDocStringCharacter (TokenDocStringCharacter _ s) = s

getFunctionName :: Token -> String
getFunctionName (TokenFunctionDocStringStart _ n) = n

getTypeName :: Token -> String
getTypeName (TokenTypeDefDocStringStart _ n) = n

getInterfaceName :: Token -> String
getInterfaceName (TokenInterfaceDocStringStart _ n) = n

getInstanceName :: Token -> String
getInstanceName (TokenInstanceDocStringStart _ n) = n

tokenTarget :: Token -> SourceTarget
tokenTarget token = case token of
  TokenModuleDocStringStart target ->
    target

  TokenFunctionDocStringStart target _ ->
    target

  TokenTypeDefDocStringStart target _ ->
    target

  TokenInterfaceDocStringStart target _ ->
    target

  TokenInstanceDocStringStart target _ ->
    target

  TokenDocStringEnd target ->
    target

  TokenExampleStart target ->
    target

  TokenSinceStart target ->
    target

  TokenDocStringCharacter target _ ->
    target

  TokenEOF ->
    TargetAll


alexEOF :: Alex Token
alexEOF = return TokenEOF
}
