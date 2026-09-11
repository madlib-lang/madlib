{-# LANGUAGE OverloadedStrings #-}
module Run.LanguageServer.Workspace
  ( getWorkspaceSymbols
  , refreshWorkspaceSymbols
  , refreshReverseModulePaths
  ) where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified AST.Solved as Slv
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.List as List
import           Data.Char (toLower)
import           Control.Monad (forM)
import           Data.IORef
import qualified Rock
import qualified Driver
import qualified Driver.Query as Query
import           Explain.Location (Area)
import           Run.Options (Options(optEntrypoint))
import           Run.Target (Target(TNode))

import Run.LanguageServer.State


-- | Read the incrementally maintained workspace-symbol index. Query matching
-- happens in memory, so a request does not trigger a compiler task per module.
getWorkspaceSymbols :: State -> String -> LspM () [SymbolInformation]
getWorkspaceSymbols state query = do
  bgDone <- liftIO $ readIORef (_backgroundDone state)
  if not bgDone || null query then return []
  else do
    symbolIndex <- liftIO $ readIORef (_workspaceSymbols state)
    return $ filter (workspaceSymbolMatches query) (concat $ Map.elems symbolIndex)


workspaceSymbolsTask :: FilePath -> Rock.Task Query.Query [SymbolInformation]
workspaceSymbolsTask modPath = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
  let fileUri = Uri (T.pack $ "file://" ++ modPath)
  let expSyms = Maybe.mapMaybe (expToSymbolInfo fileUri) (Slv.aexps typedAst)
  let typeSyms = Maybe.mapMaybe (typeDeclToSymbolInfo fileUri) (Slv.atypedecls typedAst)
  return $ expSyms ++ typeSyms


workspaceSymbolMatches :: String -> SymbolInformation -> Bool
workspaceSymbolMatches query (SymbolInformation name _ _ _ _ _) =
  map toLower query `List.isInfixOf` map toLower (T.unpack name)


-- | Refresh only modules that have just been checked or discovered.
refreshWorkspaceSymbols :: State -> [FilePath] -> LspM () ()
refreshWorkspaceSymbols state paths = do
  options <- buildOptions TNode
  updates <- liftIO $ forM paths $ \path -> do
    result <- safeRunTask state options { optEntrypoint = path }
      Driver.Don'tPrune mempty mempty (workspaceSymbolsTask path)
    return $ case result of
      Just (symbols, _, _) -> Just (path, symbols)
      Nothing              -> Nothing
  liftIO $ modifyIORef' (_workspaceSymbols state) $ \index ->
    List.foldl' (\acc (path, symbols) -> Map.insert path symbols acc) index (Maybe.catMaybes updates)


-- | Rebuild one module's reverse edges after its imports change. Failed edits
-- keep their last successful closure so diagnostics can still reach its known
-- dependents.
refreshReverseModulePaths :: State -> FilePath -> LspM () ()
refreshReverseModulePaths state modPath = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = modPath }
    Driver.Don'tPrune mempty mempty (Rock.fetch $ Query.ModulePathsToBuild modPath)
  case result of
    Just (paths, _, _) -> liftIO $ do
      modifyIORef' (_allModulePaths state) (Set.union (Set.fromList paths))
      modifyIORef' (_reverseModulePaths state) $ \index ->
        let withoutOldEdges = Map.mapMaybe removeDependent index
            removeDependent dependents =
              let remaining = Set.delete modPath dependents
              in if Set.null remaining then Nothing else Just remaining
            addDependent dependents dependency
              | dependency == modPath = dependents
              | otherwise = Map.insertWith Set.union dependency (Set.singleton modPath) dependents
        in List.foldl' addDependent withoutOldEdges paths
    Nothing -> return ()


expToSymbolInfo :: Uri -> Slv.Exp -> Maybe SymbolInformation
expToSymbolInfo fileUri exp = case exp of
  Slv.Typed _ area (Slv.Assignment name _) -> Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _) -> Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _))) -> Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _) -> Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.Extern _ name _) -> Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.Export (Slv.Typed _ _ (Slv.Extern _ name _))) -> Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  _ -> Nothing


typeDeclToSymbolInfo :: Uri -> Slv.TypeDecl -> Maybe SymbolInformation
typeDeclToSymbolInfo fileUri td = case td of
  Slv.Untyped area (Slv.ADT { Slv.adtname = name }) -> Just $ mkSymInfo (T.pack name) SkEnum area fileUri
  Slv.Typed _ area (Slv.ADT { Slv.adtname = name }) -> Just $ mkSymInfo (T.pack name) SkEnum area fileUri
  Slv.Untyped area (Slv.Alias { Slv.aliasname = name }) -> Just $ mkSymInfo (T.pack name) SkClass area fileUri
  Slv.Typed _ area (Slv.Alias { Slv.aliasname = name }) -> Just $ mkSymInfo (T.pack name) SkClass area fileUri
  _ -> Nothing


mkSymInfo :: T.Text -> SymbolKind -> Area -> Uri -> SymbolInformation
mkSymInfo name symbolKind area fileUri =
  SymbolInformation name symbolKind Nothing Nothing (Location fileUri (areaToRange area)) Nothing
