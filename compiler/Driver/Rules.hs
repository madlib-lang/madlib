{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Driver.Rules where

import qualified Rock
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import qualified Canonicalize.Env              as CanEnv
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.CanonicalM       as Can
import qualified AST.Solved                    as Slv
import           Infer.AST
import           Infer.Infer
import           Infer.EnvUtils
import qualified Infer.Env                     as SlvEnv
import           Error.Error (CompilationError(CompilationError))
import           Data.IORef
import           Parse.Madlib.AST
import           Control.Monad.IO.Class
import           Driver.Query
import           Run.Target
import           Parse.Madlib.TargetMacro
import           Optimize.StripNonJSInterfaces
import           Optimize.ToCore
import qualified Optimize.EtaReduction         as EtaReduction
import qualified Optimize.TCE                  as TCE
import qualified Generate.LLVM.Rename          as Rename
import qualified Generate.LLVM.ClosureConvert  as ClosureConvert
import qualified Generate.LLVM.LLVM            as LLVM
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Control.Monad.Except
import qualified Utils.PathUtils               as PathUtils
import Text.Show.Pretty (ppShow)
import Run.Options
import Data.Bifunctor (first)
import qualified Explain.Format as Explain
import qualified Data.List as List
import Control.Monad.Writer



rules :: Options -> Rock.GenRules (Rock.Writer [CompilationError] (Rock.Writer Rock.TaskKind Query)) Query
rules options (Rock.Writer (Rock.Writer query)) = case query of
  File path -> do
    -- liftIO $ putStrLn ("load file: " <> path)
    input $ do
      liftIO $ readFile path

  ParsedAST path -> nonInput $ do
    source <- Rock.fetch $ File path
    ast <- liftIO $ buildAST options path source
    case ast of
      Right ast -> do
        return (ast, mempty)

      Left err ->
        return (emptySrcAST, [err])

  CanonicalizedASTWithEnv path -> nonInput $ do
    sourceAst <- Rock.fetch $ ParsedAST path

    (can, _) <- runCanonicalM $ Can.canonicalizeAST "" (optTarget options) CanEnv.initialEnv sourceAst
    case can of
      Right c ->
        return (c, mempty)

      Left err ->
        return ((emptyCanAST, CanEnv.initialEnv), [err])

  CanonicalizedInterface modulePath name -> nonInput $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST modulePath
    let importedModulePaths = Src.getImportAbsolutePath <$> aimports

    interfac <- findCanInterface name importedModulePaths
    case interfac of
      Just found ->
        return (found, mempty)

  ForeignADTType modulePath typeName -> nonInput $ do
    (canAst, canEnv) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    case Map.lookup typeName (CanEnv.envTypeDecls canEnv) of
      Just found ->
        return (Just found, mempty)

      Nothing ->
        return (Nothing, mempty)

  SolvedASTWithEnv path -> nonInput $ do
    (canAst, _) <- Rock.fetch $ CanonicalizedASTWithEnv path
    res <- runInfer $ inferAST' initialEnv canAst
    case res of
      Right (astAndEnv, InferState _ []) ->
        return (astAndEnv, mempty)

      Right (astAndEnv, InferState _ errors) ->
        return (astAndEnv, errors)

      Left error ->
        return ((emptySlvAST, initialEnv), [error])

  -- SolvedTable paths -> nonInput $ do
  --   res <- runInfer $ solveManyASTs mempty paths
  --   case res of
  --     Right (table, InferState _ []) ->
  --       return (table, mempty)

  --     Right (table, InferState _ errors) ->
  --       return (table, errors)

  --     Left error ->
  --       return (mempty, [error])

  SolvedInterface modulePath name -> nonInput $ do
    (Can.AST { Can.aimports }, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    let importedModulePaths = Can.getImportAbsolutePath <$> aimports

    interfac <- findSlvInterface name importedModulePaths
    case interfac of
      Just found ->
        return (found, mempty)

  ForeignScheme modulePath name -> nonInput $ do
    (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv modulePath
    case Map.lookup name (SlvEnv.envVars slvEnv <> SlvEnv.envMethods slvEnv) of
      Just found ->
        return (Just found, mempty)

      Nothing ->
        return (Nothing, mempty)

  CoreAST path -> nonInput $ do
    (slvAst, _) <- Rock.fetch $ SolvedASTWithEnv path
    case optTarget options of
      TLLVM -> do
        let coreAst          = astToCore False slvAst
            renamedAst       = Rename.renameAST coreAst
            reducedAst       = EtaReduction.reduceAST renamedAst
            closureConverted = ClosureConvert.convertAST reducedAst
        return (TCE.resolveAST closureConverted, mempty)

      _ -> do
        let coreAst     = astToCore (optOptimized options) slvAst
            strippedAst = stripAST coreAst
        return (TCE.resolveAST strippedAst, mempty)

  SymbolTableWithEnv path -> nonInput $ do
    coreAst <- Rock.fetch $ CoreAST path
    table   <- LLVM.compileModule' options coreAst
    return (table, mempty)

  BuiltInSymbolTableWithEnv -> nonInput $ do
    table <- LLVM.compileDefaultModule options
    return (table, mempty)

  _ ->
    undefined


emptySrcAST :: Src.AST
emptySrcAST = Src.AST { Src.aimports = [], Src.aexps = [], Src.atypedecls = [], Src.ainstances = [], Src.ainterfaces = [], Src.apath = Nothing }

emptyCanAST :: Can.AST
emptyCanAST = Can.AST { Can.aimports = [], Can.aexps = [], Can.atypedecls = [], Can.ainstances = [], Can.ainterfaces = [], Can.apath = Nothing }

emptySlvAST :: Slv.AST
emptySlvAST = Slv.AST { Slv.aimports = [], Slv.aexps = [], Slv.atypedecls = [], Slv.ainstances = [], Slv.ainterfaces = [], Slv.apath = Nothing }


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


findCanInterface :: Rock.MonadFetch Query m => String -> [FilePath] -> m (Maybe CanEnv.Interface)
findCanInterface name paths = case paths of
  [] ->
    return Nothing

  path : next -> do
    (_, canEnv) <- Rock.fetch $ CanonicalizedASTWithEnv path
    case Map.lookup name (CanEnv.envInterfaces canEnv) of
      Just found ->
        return $ Just found

      Nothing -> do
        next' <- findCanInterface name next
        case next' of
          Just found ->
            return $ Just found

          Nothing -> do
            (Can.AST { Can.aimports }, _)<- Rock.fetch $ CanonicalizedASTWithEnv path
            let importedModulePaths = Can.getImportAbsolutePath <$> aimports
            findCanInterface name importedModulePaths

findSlvInterface :: Rock.MonadFetch Query m => String -> [FilePath] -> m (Maybe SlvEnv.Interface)
findSlvInterface name paths = case paths of
  [] ->
    return Nothing

  path : next -> do
    (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv path
    case Map.lookup name (SlvEnv.envInterfaces slvEnv) of
      Just found ->
        return $ Just found

      Nothing -> do
        next' <- findSlvInterface name next
        case next' of
          Just found ->
            return $ Just found

          Nothing -> do
            (Slv.AST { Slv.aimports }, _) <- Rock.fetch $ SolvedASTWithEnv path
            let importedModulePaths = Slv.getImportAbsolutePath <$> aimports
            findSlvInterface name importedModulePaths


noError :: (Monoid w, Functor f) => f a -> f ((a, Rock.TaskKind), w)
noError = fmap ((, mempty) . (, Rock.NonInput))

nonInput :: Functor f => f (a, w) -> f ((a, Rock.TaskKind), w)
nonInput = fmap $ first (, Rock.NonInput)

input :: (Monoid w, Functor f) => f a -> f ((a, Rock.TaskKind), w)
input = fmap ((, mempty) . (, Rock.Input))


ignoreTaskKind :: Rock.GenRules (Rock.Writer Rock.TaskKind f) f -> Rock.Rules f
ignoreTaskKind rs key = fst <$> rs (Rock.Writer key)


printErrors :: Options -> [CompilationError] -> Rock.Task Query ()
printErrors options [] = return ()
printErrors options errors = do
  -- TODO: make Explain.format fetch the file from the store directly
  formattedErrors <- liftIO $ mapM (Explain.format readFile False) errors
  let fullError = List.intercalate "\n\n\n" formattedErrors
  liftIO $ putStrLn fullError-- >> exitFailure


-- buildSolvedTable :: Options -> [FilePath] -> IO Slv.Table
-- buildSolvedTable options paths = do
--   memoVar <- newIORef mempty
--   let task = Rock.fetch $ SolvedTable paths
--   Rock.runTask (Rock.memoise memoVar (ignoreTaskKind (Rock.writer (\_ errs -> printErrors options errs) $ rules options))) task


compile :: Options -> FilePath -> IO ()
compile options path = do
  memoVar <- newIORef mempty
  let task = Rock.fetch $ SymbolTableWithEnv path
  Rock.runTask (Rock.memoise memoVar (ignoreTaskKind (Rock.writer (\_ errs -> printErrors options errs) $ rules options))) task
  return ()
