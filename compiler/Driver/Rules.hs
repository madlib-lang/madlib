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
import qualified Utils.PathUtils             as PathUtils
import Text.Show.Pretty (ppShow)
import Run.Options

-- TODO: wrap all inputs into one parameter
rules :: Options -> Rock.GenRules (Rock.Writer Rock.TaskKind Query) Query
rules options (Rock.Writer query) = case query of
  File path -> do
    liftIO $ putStrLn path
    input $ liftIO $ readFile path

  ParsedAST path -> nonInput $ do
    source <- Rock.fetch $ File path
    ast <- liftIO $ buildAST options path source
    case ast of
      Right ast -> do
        return ast

      Left err ->
        return Src.AST {}

  CanonicalizedASTWithEnv path -> nonInput $ do
    sourceAst <- Rock.fetch $ ParsedAST path

    (can, _) <- runCanonicalM $ Can.canonicalizeAST "" (optTarget options) CanEnv.initialEnv sourceAst
    case can of
      Right c ->
        return c

      Left err -> do
        liftIO $ putStrLn (ppShow err)
        return (Can.AST {}, CanEnv.initialEnv)

  CanonicalizedInterface modulePath name -> nonInput $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST modulePath
    let importedModulePaths = Src.getImportAbsolutePath <$> aimports

    interfac <- findInterface name importedModulePaths
    case interfac of
      Just found ->
        return found

  ForeignType modulePath typeName -> nonInput $ do
    (canAst, canEnv) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    case Map.lookup typeName (CanEnv.envTypeDecls canEnv) of
      Just found ->
        return found

  SolvedTable paths -> nonInput $ do
    res <- runInfer $ solveManyASTs mempty paths
    case res of
      Right (table, _) ->
        return table

  _ ->
    undefined


runInfer :: StateT InferState (ExceptT e m) a -> m (Either e (a, InferState))
runInfer a =
  runExceptT (runStateT a InferState { count = 0, errors = [] })


runCanonicalM :: ExceptT e (StateT Can.CanonicalState m) a -> m (Either e a, Can.CanonicalState)
runCanonicalM a =
  runStateT
    (runExceptT a)
    (
      Can.CanonicalState
        { Can.warnings = []
        , Can.namesAccessed = Set.empty
        , Can.accumulatedJS = ""
        , Can.typesToDerive = []
        , Can.derivedTypes = Set.empty
        , Can.placeholderIndex = 0
        }
    )


findInterface :: Rock.MonadFetch Query m => String -> [FilePath] -> m (Maybe CanEnv.Interface)
findInterface name paths = case paths of
  [] ->
    return Nothing

  path : next -> do
    (canAst, canEnv) <- Rock.fetch $ CanonicalizedASTWithEnv path
    case Map.lookup name (CanEnv.envInterfaces canEnv) of
      Just found ->
        return $ Just found

      Nothing -> do
        next' <- findInterface name next
        case next' of
          Just found ->
            return $ Just found

          Nothing -> do
            Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST path
            let importedModulePaths = Src.getImportAbsolutePath <$> aimports
            findInterface name importedModulePaths


input :: (Functor f) => f a -> f (a, Rock.TaskKind)
input = fmap (, Rock.Input)


nonInput :: (Functor f) => f a -> f (a, Rock.TaskKind)
nonInput = fmap (, Rock.NonInput)


ignoreTaskKind :: Rock.GenRules (Rock.Writer Rock.TaskKind f) f -> Rock.Rules f
ignoreTaskKind rs key = fst <$> rs (Rock.Writer key)


buildSolvedTable :: Options -> [FilePath] -> IO Slv.Table
buildSolvedTable options paths = do
  memoVar <- newIORef mempty
  let task = Rock.fetch $ SolvedTable paths
  Rock.runTask (Rock.memoise memoVar (ignoreTaskKind (rules options))) task
  
