{-# LANGUAGE OverloadedStrings #-}
module Run.LanguageServer.Completion
  ( getAutocompletionSuggestions
  , completionSuggestionsTask
  , AutocompletionKind(..)
  , computeAutocompletionKind
  , getLocalNames
  , findTopLevelExp
  ) where

import Language.LSP.Server
import Language.LSP.Types
import qualified Data.Text as T
import qualified Rock
import qualified Driver
import qualified Driver.Query as Query
import qualified Data.Map as Map
import Explain.Location (Loc(..))
import qualified AST.Solved as Slv
import           Infer.Type (Qual((:=>)), Type (..), Kind (Star), TVar (..), Scheme (Forall), getParamTypes)
import           Run.Target
import Control.Monad.IO.Class
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Run.Options (Options(optEntrypoint))
import           Data.Char (isAlphaNum, isDigit, isUpper)
import qualified Infer.Env as SlvEnv

import Run.LanguageServer.State
import Run.LanguageServer.Hover (isInRange, isLocAfter, isLocAfterStart, prettyScheme, findNodeInExps, Node(..), prettyQt)


data AutocompletionKind
  = AutocompletingName String
  | AutocompletingRecordAccess String String
  | AutocompletingNamespaceAccess String
  deriving(Eq, Show)


computeAutocompletionKind :: String -> AutocompletionKind
computeAutocompletionKind reversedCurrentLine =
  let consumeChars chars = case chars of
        c : cs | isAlphaNum c ->
          c : consumeChars cs

        _ ->
          []
      readName chars =
        let _name = reverse $ consumeChars chars
            _rest = drop (length _name) chars
        in  (dropWhile isDigit _name, _rest)
      (name, rest) = readName reversedCurrentLine
  in  case rest of
        '.' : afterDot ->
          let (recordName, rest') = readName afterDot
          in  case rest' of
                _ | not (null recordName) && isUpper (head recordName) ->
                  AutocompletingNamespaceAccess recordName


                _ ->
                  AutocompletingRecordAccess recordName name

        _ ->
          AutocompletingName name


findTopLevelExp :: Loc -> [Slv.Exp] -> Maybe Slv.Exp
findTopLevelExp loc exps = case exps of
  e@(Slv.Typed _ area _) : next ->
    if isInRange loc area then
      Just e
    else
      findTopLevelExp loc next

  _ : next ->
    findTopLevelExp loc next

  [] ->
    Nothing


getLocalNames :: Loc -> Slv.Exp -> [(String, Type)]
getLocalNames loc exp = case exp of
  Slv.Typed _ area (Slv.Assignment _ e) | not (isLocAfter loc area) ->
    getLocalNames loc e

  Slv.Typed (_ :=> t) _ (Slv.Assignment n e) ->
    (n, t) : getLocalNames loc e

  Slv.Typed _ area (Slv.Abs _ body) | not (isLocAfterStart loc area) ->
    body >>= getLocalNames loc

  Slv.Typed (_ :=> t) _ (Slv.Abs param body) ->
    let paramType = case getParamTypes t of
          (pt : _) -> pt
          []       -> TVar (TV (-1) Star)
    in  (Slv.getValue param, paramType) : (body >>= getLocalNames loc)

  Slv.Typed _ _ (Slv.Export e) ->
    getLocalNames loc e

  Slv.Typed _ _ (Slv.TypedExp e _ _) ->
    getLocalNames loc e

  _ ->
    []


getAutocompletionSuggestions :: State -> Loc -> FilePath -> String -> LspM () [(String, String, CompletionItemKind)]
getAutocompletionSuggestions autocompletionState loc path moduleContent = do
  -- Use single target for completion — types are target-independent for the vast majority of code
  jsOptions <- buildOptions TNode
  jsResult <- liftIO $ safeRunTask autocompletionState jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (completionSuggestionsTask loc path moduleContent)
  return $ case jsResult of
    Just (suggestions, _, _) -> suggestions
    Nothing                  -> []


completionSuggestionsTask :: Loc -> FilePath -> String -> Rock.Task Query.Query [(String, String, CompletionItemKind)]
completionSuggestionsTask loc@(Loc _ line col) modulePath moduleContent = do
  (typedAst, env)  <- Rock.fetch $ Query.SolvedASTWithEnv modulePath

  let contentLines = lines moduleContent
  let foundLine = if line - 1 >= 0 && line - 1 < length contentLines
                  then take (col - 1) (contentLines !! (line - 1))
                  else ""
  let autocompletionKind = computeAutocompletionKind (reverse foundLine)

  let topLevelExp = findTopLevelExp loc (Slv.aexps typedAst)
  let localNames = map (\(n, t) -> (n, Forall [] ([] :=> t))) $ Maybe.fromMaybe [] $ (getLocalNames loc) <$> topLevelExp
  let namesInEnv = Map.toList $ SlvEnv.envVars env
  let methodsInEnv = Map.toList $ SlvEnv.envMethods env
  case autocompletionKind of
    AutocompletingNamespaceAccess namespace ->
      return
        $ map (\(n, qt) -> (if '.' `List.elem` n then List.tail $ dropWhile (/= '.') n else n, prettyScheme qt, CiFunction))
        $ filter (List.isPrefixOf namespace . fst) namesInEnv

    AutocompletingName _ ->
      return
        $ map (\(n, qt) -> (n, prettyScheme qt, CiFunction))
        $ localNames ++ namesInEnv ++ methodsInEnv

    AutocompletingRecordAccess recordName fieldName -> do
      let updatedLoc = Loc 0 line (col - length fieldName - 1)
      case findNodeInExps updatedLoc (Slv.aexps typedAst ++ Slv.getAllMethods typedAst) of
        Just (ExpNode _ (Slv.Typed (_ :=> TRecord fields _ extraFields) _ _)) ->
          return
            $ map (\(n, t) -> (n, prettyScheme (Forall [] ([] :=> t)), CiField))
            $ Map.toList $ fields <> extraFields

        Just (NameNode _ (Slv.Typed (_ :=> TApp _ (TRecord fields _ extraFields)) _ _)) ->
          return
            $ map (\(n, t) -> (n, prettyScheme (Forall [] ([] :=> t)), CiField))
            $ Map.toList $ fields <> extraFields

        _ ->
          case List.find (\(n, _) -> n == recordName) localNames of
            Just (_, Forall _ (_ :=> TRecord fields _ extraFields)) ->
              return
                $ map (\(n, t) -> (n, prettyScheme (Forall [] ([] :=> t)), CiField))
                $ Map.toList
                $ fields <> extraFields

            _ ->
              return $ map (\(n, qt) -> (n, prettyScheme qt, CiField)) $ localNames ++ namesInEnv ++ methodsInEnv
