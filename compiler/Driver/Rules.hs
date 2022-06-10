{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Driver.Rules where

import qualified Rock
import qualified AST.Source                 as Src
import qualified AST.Canonical              as Can
import qualified Canonicalize.Env           as CanEnv
import qualified Canonicalize.AST           as Can
import qualified Canonicalize.CanonicalM    as Can
import qualified AST.Solved                 as Slv
import           Error.Error (CompilationError(CompilationError))
import           Data.IORef
import           Parse.Madlib.AST
import           Control.Monad.IO.Class
import           Driver.Query
import           Run.Target
import           Parse.Madlib.TargetMacro
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Infer.AST
import           Infer.Infer
import           Control.Monad.State
import           Control.Monad.Except
import Text.Show.Pretty (ppShow)


rules :: FilePath -> Rock.GenRules (Rock.Writer Rock.TaskKind Query) Query
rules rootPath (Rock.Writer query) = case query of
  File path -> do
    liftIO $ putStrLn path
    input $ liftIO $ readFile path

  ParsedAST path -> nonInput $ do
    source <- Rock.fetch $ File path
    case buildAST path source of
      Right ast -> do
        liftIO $ computeAbsoluteImportPaths rootPath (resolveMacros TLLVM ast)

      Left err ->
        return Src.AST {}

  CanonicalizedASTWithEnv path -> nonInput $ do
    sourceAst <- Rock.fetch $ ParsedAST path

    (can, _) <- runStateT (runExceptT (Can.canonicalizeAST "" TLLVM CanEnv.initialEnv sourceAst))
                                    (Can.CanonicalState { Can.warnings = [], Can.namesAccessed = Set.empty, Can.accumulatedJS = "", Can.typesToDerive = [], Can.derivedTypes = Set.empty, Can.placeholderIndex = 0 })
    case can of
      Right c ->
        return c

      Left err -> do
        liftIO $ putStrLn (ppShow err)
        return (Can.AST {}, CanEnv.initialEnv)

  CanonicalizedInterface modulePath name -> nonInput $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST modulePath
    let importedModulePaths = Src.getImportAbsolutePath <$> aimports

    interfac <- tryModules importedModulePaths
    case interfac of
      Just found ->
        return found
    where
      tryModules :: Rock.MonadFetch Query m => [FilePath] -> m (Maybe CanEnv.Interface)
      tryModules paths = case paths of
        [] ->
          return Nothing

        path : next -> do
          (canAst, canEnv) <- Rock.fetch $ CanonicalizedASTWithEnv path
          case Map.lookup name (CanEnv.envInterfaces canEnv) of
            Just found ->
              return $ Just found

            Nothing -> do
              next' <- tryModules next
              case next' of
                Just found ->
                  return $ Just found

                Nothing -> do
                  Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST path
                  let importedModulePaths = Src.getImportAbsolutePath <$> aimports
                  tryModules importedModulePaths






  ForeignType modulePath typeName -> nonInput $ do
    (canAst, canEnv) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    case Map.lookup typeName (CanEnv.envTypeDecls canEnv) of
      Just found ->
        return found

  SolvedTable paths -> nonInput $ do
    res <- runExceptT (runStateT (solveManyASTs mempty paths) InferState { count = 0, errors = [] })
    case res of
      Right (table, _) ->
        return table

  _ ->
    undefined



input :: (Functor f) => f a -> f (a, Rock.TaskKind)
input = fmap (, Rock.Input)


nonInput :: (Functor f) => f a -> f (a, Rock.TaskKind)
nonInput = fmap (, Rock.NonInput)


ignoreTaskKind :: Rock.GenRules (Rock.Writer Rock.TaskKind f) f -> Rock.Rules f
ignoreTaskKind rs key = fst <$> rs (Rock.Writer key)


-- runQuery :: Query a -> IO a
-- runQuery query = do
--   memoVar <- newIORef mempty
--   let task = Rock.fetch query
--   Rock.runTask (Rock.memoise memoVar (ignoreTaskKind rules)) task



-- TODO: move target resolution
-- parse :: Target -> FilePath -> FilePath -> IO Src.AST
-- parse target rootPath path = do
--   ast <- runQuery $ ParsedAST path
--   return $ resolveMacros target ast

buildSolvedTable :: FilePath -> [FilePath] -> IO Slv.Table
buildSolvedTable rootPath paths = do
  memoVar <- newIORef mempty
  let task = Rock.fetch $ SolvedTable paths
  Rock.runTask (Rock.memoise memoVar (ignoreTaskKind (rules rootPath))) task
--   x <- runExceptT (runStateT (solveManyASTs mempty paths) InferState { count = 0, errors = [] })
  
