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
import qualified Generate.LLVM.Env             as LLVMEnv
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Control.Monad.Except
import Text.Show.Pretty (ppShow)
import Run.Options
import Data.Bifunctor (first)
import qualified Explain.Format as Explain
import qualified Data.List as List
import Control.Monad.Writer
import Utils.List
-- import Parse.Madlib.ImportCycle (detectCycle)
import Error.Warning
import Canonicalize.CanonicalM (CanonicalState(CanonicalState, warnings))
import qualified Utils.PathUtils as PathUtils
import qualified Generate.Javascript as Javascript
import System.FilePath (takeDirectory, joinPath, takeExtension)
import Utils.Path (computeTargetPath, resolveAbsoluteSrcPath)
import qualified Parse.DocString.Grammar as DocString
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as ByteString
import System.Directory
import qualified Utils.Path                    as Path
import           Error.Error
import           Error.Context
import Parse.Madlib.ImportCycle (detectCycle)
import AST.Canonical (AST(atypedecls))

rules :: Options -> Rock.GenRules (Rock.Writer ([CompilationWarning], [CompilationError]) (Rock.Writer Rock.TaskKind Query)) Query
rules options (Rock.Writer (Rock.Writer query)) = case query of
  DictionaryModuleAbsolutePath -> nonInput $ do
    dictModulePath <- liftIO $ Utils.Path.resolveAbsoluteSrcPath (optPathUtils options) (optRootPath options) "Dictionary"
    return (Maybe.fromMaybe "" dictModulePath, (mempty, mempty))

  ModulePathsToBuild entrypoint -> input $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST entrypoint
    let importPaths = Src.getImportAbsolutePath <$> aimports
    fromImports <- mapM (Rock.fetch . ModulePathsToBuild) importPaths
    return $ removeDuplicates $ List.concat fromImports ++ importPaths ++ [entrypoint]

  DetectImportCycle _ path -> nonInput $ do
    r <- detectCycle [] path
    case r of
      Just err ->
        return (True, (mempty, [err]))

      Nothing ->
        return (False, (mempty, mempty))

  -- DetectImportCycle importChain path -> nonInput $ do
  --   Src.AST { Src.aimports, Src.apath } <- Rock.fetch $ ParsedAST path

  --   case apath of
  --     Just path ->
  --       foldM
  --         (\(alreadyErrored, errorsAndWarnings) imp ->
  --           if alreadyErrored then
  --             return (alreadyErrored, errorsAndWarnings)
  --           else do
  --             let importPath = Src.getImportAbsolutePath imp
  --             let importArea = Src.getArea imp
  --             if importPath `elem` importChain then
  --               return (True, ([], [CompilationError (ImportCycle $ importChain ++ [path, importPath]) (Context path importArea [])]))
  --             else do
  --               res <- Rock.fetch $ DetectImportCycle (importChain ++ [path]) importPath
  --               return (res, (mempty, mempty))
  --         )
  --         (False, ([], []))
  --         aimports

  File path -> input $ do
    liftIO $ (PathUtils.readFile $ optPathUtils options) path

  ParsedAST path -> nonInput $ do
    -- liftIO $ putStrLn $ "fetch " <> path
    source <- Rock.fetch $ File path
    ast    <- liftIO $ buildAST options path source
    case ast of
      Right ast -> do
        return (ast, (mempty, mempty))

      Left err ->
        return (emptySrcAST, (mempty, [err]))

  DocStrings path -> nonInput $ do
    file <- Rock.fetch $ File path
    case DocString.parse file of
      Right ds ->
        return (ds, (mempty, mempty))

      Left _ ->
        return ([], (mempty, mempty))

  CanonicalizedASTWithEnv path -> nonInput $ do
    sourceAst      <- Rock.fetch $ ParsedAST path
    dictModulePath <- Rock.fetch DictionaryModuleAbsolutePath

    (can, Can.CanonicalState { warnings }) <- runCanonicalM $ Can.canonicalizeAST dictModulePath options CanEnv.initialEnv sourceAst
    case can of
      Right c ->
        return (c, (warnings, mempty))

      Left err ->
        return ((Can.emptyAST, CanEnv.initialEnv, mempty), (warnings, [err]))

  CanonicalizedInterface modulePath name -> nonInput $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST modulePath
    let importedModulePaths = Src.getImportAbsolutePath <$> aimports

    found <- findCanInterface name importedModulePaths
    return (found, (mempty, mempty))

  ForeignADTType modulePath typeName -> nonInput $ do
    (canAst, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    case Map.lookup typeName (CanEnv.envTypeDecls canEnv) of
      Just found ->
        return (Just found, (mempty, mempty))

      Nothing ->
        return (Nothing, (mempty, mempty))

  SolvedASTWithEnv path -> nonInput $ do
    (canAst, _, instancesToDerive) <- Rock.fetch $ CanonicalizedASTWithEnv path
    res <- runInfer $ inferAST options initialEnv instancesToDerive canAst
    case res of
      Right (astAndEnv, InferState _ []) -> do
        return (astAndEnv, (mempty, mempty))

      Right (_, InferState _ errors) ->
        -- return ((emptySlvAST, initialEnv), (mempty, errors))
        return ((emptySlvAST { Slv.apath = Just path }, initialEnv), (mempty, errors))

      Left error ->
        -- return ((emptySlvAST, initialEnv), (mempty, [error]))
        return ((emptySlvAST { Slv.apath = Just path }, initialEnv), (mempty, [error]))

  SolvedInterface modulePath name -> nonInput $ do
    (Can.AST { Can.aimports }, _, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    let importedModulePaths = Can.getImportAbsolutePath <$> aimports

    interfac <- findSlvInterface name importedModulePaths
    case interfac of
      Just found ->
        return (Just found, (mempty, mempty))

      Nothing ->
        return (Nothing, (mempty, mempty))

  ForeignScheme modulePath name -> nonInput $ do
    (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv modulePath
    return (Map.lookup name (SlvEnv.envVars slvEnv <> SlvEnv.envMethods slvEnv), (mempty, mempty))

  ForeignExp modulePath name -> nonInput $ do
    (slvAst, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    return $ findExpByName name (Slv.aexps slvAst)
    where
      findExpByName name exps = case exps of
        [] ->
          (Nothing, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.Assignment n _)) : next) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment n _)))) : next) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment n _)) _ _)) : next) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment n _)))) _ _)) : next) | n == name ->
          (Just e, (mempty, mempty))

        (_ : next) ->
          findExpByName name next

  ForeignConstructor modulePath name -> nonInput $ do
    (Slv.AST { Slv.atypedecls }, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    let allConstructors = concat $ Maybe.mapMaybe Slv.getADTConstructors atypedecls
    return (List.find ((== name) . Slv.getConstructorName) allConstructors, (mempty, mempty))

  ForeignTypeDeclaration modulePath name -> nonInput $ do
    (Slv.AST { Slv.atypedecls }, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    return (List.find (\fullTd@(Slv.Untyped _ td) -> Slv.isADT fullTd && Slv.adtname td == name || Slv.isAlias fullTd && Slv.aliasname td == name) atypedecls, (mempty, mempty))

  CoreAST path -> nonInput $ do
    (slvAst, _) <- Rock.fetch $ SolvedASTWithEnv path
    case optTarget options of
      TLLVM -> do
        let coreAst          = astToCore False slvAst
            renamedAst       = Rename.renameAST coreAst
            reducedAst       = EtaReduction.reduceAST renamedAst
            closureConverted = ClosureConvert.convertAST reducedAst
        return (TCE.resolveAST closureConverted, (mempty, mempty))

      _ -> do
        let coreAst     = astToCore (optOptimized options) slvAst
            strippedAst = stripAST coreAst
        return (TCE.resolveAST strippedAst, (mempty, mempty))

  BuiltObjectFile path -> nonInput $ do
    coreAst                           <- Rock.fetch $ CoreAST path
    builtModule@(_, _, objectContent) <- LLVM.compileModule options coreAst

    liftIO $ do
      let outputFolder   = takeDirectory (optOutputPath options)
      let outputPath     = Path.computeLLVMTargetPath outputFolder (optRootPath options) path
      createDirectoryIfMissing True $ takeDirectory outputPath
      ByteString.writeFile outputPath objectContent

    return (builtModule, (mempty, mempty))

  BuiltInBuiltObjectFile -> nonInput $ do
    builtModule@(_, _, objectContent) <- LLVM.compileDefaultModule options

    liftIO $ do
      let defaultInstancesModulePath =
              if takeExtension (optEntrypoint options) == "" then
                joinPath [optEntrypoint options, "__default__instances__.mad"]
              else
                joinPath [takeDirectory (optEntrypoint options), "__default__instances__.mad"]
      let outputFolder = takeDirectory (optOutputPath options)
      let outputPath = Path.computeLLVMTargetPath outputFolder (optRootPath options) defaultInstancesModulePath
      createDirectoryIfMissing True $ takeDirectory outputPath
      ByteString.writeFile outputPath objectContent

    return (builtModule, (mempty, mempty))

  GeneratedJSModule path -> nonInput $ do
    paths    <- Rock.fetch $ ModulePathsToBuild (optEntrypoint options)
    coreAst  <- Rock.fetch $ CoreAST path
    jsModule <- liftIO $ Javascript.generateJSModule options paths coreAst
    return (jsModule, (mempty, mempty))

  BuiltJSModule path -> nonInput $ do
    jsModule <- Rock.fetch $ GeneratedJSModule path
    let computedOutputPath = computeTargetPath (takeDirectory (optOutputPath options)) (optRootPath options) path

    liftIO $ do
      createDirectoryIfMissing True $ takeDirectory computedOutputPath
      writeFile computedOutputPath jsModule
    return (jsModule, (mempty, mempty))

  BuiltTarget path -> nonInput $ do
    paths <- Rock.fetch $ ModulePathsToBuild path

    if optTarget options == TLLVM then do
      moduleResults <- mapM (Rock.fetch . BuiltObjectFile) paths
      let moduleEnvs = (\(_, env, _) -> env) <$> moduleResults

      if any (null . LLVMEnv.envASTPath) moduleEnvs then
        return ((), (mempty, mempty))
      else do
        LLVM.buildTarget options path
        return ((), (mempty, mempty))

    else do
      forM_ paths $ Rock.fetch . BuiltJSModule
      liftIO $ Javascript.generateInternalsModule options

      when (optBundle options) $ do
        let mainOutputPath = computeTargetPath (takeDirectory (optOutputPath options)) (optRootPath options) (optEntrypoint options)
        result <- liftIO $ Javascript.runBundle mainOutputPath
        case result of
          Left err ->
            liftIO $ putStr err

          Right ("", stderr) ->
            liftIO $ putStrLn stderr

          Right (bundle, _) -> do
            liftIO $ writeFile (optOutputPath options) bundle

        return ()

      return ((), (mempty, mempty))


-- TODO: Move to AST.Source module
emptySrcAST :: Src.AST
emptySrcAST = Src.AST { Src.aimports = [], Src.aexps = [], Src.atypedecls = [], Src.ainstances = [], Src.ainterfaces = [], Src.apath = Nothing }

-- TODO: Move to AST.Solved module
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
    (_, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv path
    case Map.lookup name (CanEnv.envInterfaces canEnv) of
      Just found ->
        return $ Just found

      Nothing -> do
        next' <- findCanInterface name next
        case next' of
          Just found ->
            return $ Just found

          Nothing -> do
            (Can.AST { Can.aimports }, _, _)<- Rock.fetch $ CanonicalizedASTWithEnv path
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
