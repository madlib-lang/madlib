{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Driver.Rules where

import qualified Rock
import Data.Graph
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import qualified Canonicalize.Env              as CanEnv
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.CanonicalM       as Can
import qualified AST.Solved                    as Slv
import           Infer.AST
import           Infer.Infer
import qualified Infer.Infer                   as Slv
import           Infer.EnvUtils
import qualified Infer.Env                     as SlvEnv
import           Parse.Madlib.AST
import           Control.Monad.IO.Class
import           Driver.Query
import           Run.Target
import           Parse.Madlib.TargetMacro
import           Optimize.ToCore
import qualified Optimize.SortExpressions as SortExpressions
import qualified Optimize.EtaReduction         as EtaReduction
import qualified Optimize.EtaExpansion         as EtaExpansion
import qualified Optimize.TCE                  as TCE
import qualified Generate.LLVM.Rename          as Rename
import qualified Generate.LLVM.ClosureConvert  as ClosureConvert
import qualified Generate.LLVM.LLVM            as LLVM
import qualified Generate.LLVM.Env             as LLVMEnv
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Control.Monad.Except
import           Data.IORef
import Text.Show.Pretty (ppShow)
import Run.Options
import Data.Bifunctor (first)
import qualified Explain.Format as Explain
import qualified Data.List as List
import Utils.List
import Error.Warning
import Canonicalize.CanonicalM (CanonicalState(CanonicalState, warnings, coverableInfo))
import qualified Utils.PathUtils as PathUtils
import qualified Generate.Javascript as Javascript
import System.FilePath (takeDirectory, dropFileName, joinPath, takeExtension)
import Utils.Path
import qualified Parse.DocString.Grammar as DocString
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as ByteString
import System.Directory
import qualified Utils.Path                    as Path
import           Error.Error
import Parse.Madlib.ImportCycle (detectCycle)
import AST.Canonical (AST(atypedecls))
import Explain.Location (emptyArea)
import Infer.Type
import qualified Infer.Unify as Unify
import qualified MadlibDotJson.MadlibDotJson as MadlibDotJson
import MadlibDotJson.MadlibVersion
import Paths_madlib (version)
import System.Environment
import qualified Canonicalize.Coverage as Coverage
import AST.Source (SourceTarget(TargetAll))
import qualified Infer.Monomorphize as MM
import           Infer.MonomorphizationState
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import qualified AST.Core as Core
import qualified Optimize.SimplifyCalls as SimplifyCalls
import qualified Optimize.FoldCalls as FoldCalls
import qualified Canonicalize.Rewrite as Rewrite
import qualified Optimize.HigherOrderCopyPropagation as HigherOrderCopyPropagation
import Run.OptimizationLevel
import Infer.Test



rules :: Options -> Rock.GenRules (Rock.Writer ([CompilationWarning], [CompilationError]) (Rock.Writer Rock.TaskKind Query)) Query
rules options (Rock.Writer (Rock.Writer query)) = case query of
  AbsolutePreludePath moduleName -> nonInput $ do
    dictModulePath <- liftIO $ Utils.Path.resolveAbsoluteSrcPath (optPathUtils options) (optRootPath options) moduleName
    return (Maybe.fromMaybe "" dictModulePath, (mempty, mempty))

  DictionaryModuleAbsolutePath -> nonInput $ do
    dictModulePath <- liftIO $ Utils.Path.resolveAbsoluteSrcPath (optPathUtils options) (optRootPath options) "Dictionary"
    return (Maybe.fromMaybe "" dictModulePath, (mempty, mempty))

  ModulePathsToBuild entrypoint -> input $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST entrypoint
    let importPaths = Src.getImportAbsolutePath <$> aimports
    importPaths' <-
          if optCoverage options && not ("prelude/__internal__" `List.isInfixOf` entrypoint) && not ("prelude\\__internal__" `List.isInfixOf` entrypoint) then do
            coverageModulePath <- Rock.fetch $ AbsolutePreludePath "__Coverage__"
            return $ coverageModulePath : importPaths
          else
            return importPaths
    fromImports <- mapM (Rock.fetch . ModulePathsToBuild) importPaths'
    return $ removeDuplicates $ List.concat fromImports ++ importPaths' ++ [entrypoint]

  DetectImportCycle _ path -> nonInput $ do
    r <- detectCycle [] path
    case r of
      Just err ->
        return (True, (mempty, [err]))

      Nothing ->
        return (False, (mempty, mempty))

  File path -> input $ do
    liftIO $ (PathUtils.readFile $ optPathUtils options) path

  ParsedAST path -> nonInput $ do
    source <- Rock.fetch $ File path
    ast    <- liftIO $ buildAST options path source
    wishModulePath <- Rock.fetch $ AbsolutePreludePath "Wish"

    case ast of
      Right ast ->
        return (addTestEmptyExports wishModulePath ast, (mempty, mempty))

      Left err ->
        return (emptySrcAST, (mempty, [err]))

  DocStrings path -> nonInput $ do
    file <- Rock.fetch $ File path
    case DocString.parse file of
      Right ds ->
        return (ds, (mempty, mempty))

      Left _ -> do
        return ([], (mempty, mempty))

  CanonicalizedASTWithEnv path -> nonInput $ do
    sourceAst      <- Rock.fetch $ ParsedAST path
    dictModulePath <- Rock.fetch DictionaryModuleAbsolutePath

    (can, Can.CanonicalState { Can.warnings }) <- runCanonicalM $ do
      (ast, env, instancesToDerive) <- Can.canonicalizeAST dictModulePath options CanEnv.initialEnv sourceAst
      ast' <-
        if optCoverage options then
          Coverage.addTrackers options ast
        else
          return ast

      let ast'' =
            if optMustHaveMain options && optOptimizationLevel options > O1 then
              Rewrite.rewriteAST ast'
            else
              -- we don't do it if we're not building an executable
              ast'
      return (ast'', env, instancesToDerive)

    case can of
      Right (ast, env, instancesToDerive) ->
        return ((ast, env, instancesToDerive), (warnings, mempty))

      Left err ->
        return ((Can.emptyAST, CanEnv.initialEnv, mempty), (warnings, [err]))

  ForeignCanTypeDeclaration modulePath typeName -> nonInput $ do
    (Can.AST { Can.atypedecls }, _, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath

    let found = List.find ((== typeName) . Can.getTypeDeclName) atypedecls
    return (found, (mempty, mempty))

  CanonicalizedInterface modulePath name -> nonInput $ do
    Src.AST { Src.aimports } <- Rock.fetch $ ParsedAST modulePath
    let importedModulePaths = Src.getImportAbsolutePath <$> aimports

    found <- findCanInterface name importedModulePaths
    return (found, (mempty, mempty))

  ForeignADTType modulePath typeName -> nonInput $ do
    (_, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    case Map.lookup typeName (CanEnv.envTypeDecls canEnv) of
      Just found ->
        return (Just found, (mempty, mempty))

      Nothing ->
        return (Nothing, (mempty, mempty))

  ForeignConstructorInfos modulePath typeName -> nonInput $ do
    (_, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
    case Map.lookup typeName (CanEnv.envConstructorInfos canEnv) of
      Just found ->
        return (Just found, (mempty, mempty))

      Nothing ->
        return (Nothing, (mempty, mempty))

  SolvedASTWithEnv path -> nonInput $ do
    (canAst, _, instancesToDerive) <- Rock.fetch $ CanonicalizedASTWithEnv path
    initialEnvRes <- runInfer initialEnv
    let Right (initialEnv', _) = initialEnvRes
    res <- runInfer $ do
      (ast', env) <- inferAST options initialEnv' instancesToDerive canAst

      wishModulePath <- Rock.fetch $ AbsolutePreludePath "Wish"
      listModulePath <- Rock.fetch $ AbsolutePreludePath "List"
      testModulePath <- Rock.fetch $ AbsolutePreludePath "Test"
      let ast'' = updateTestExports wishModulePath testModulePath listModulePath ast'
      verifyTests ast''
      forM_ (Slv.aexps ast'') $ \e ->
        catchError (verifyTopLevelExp path e) (\err -> pushError err >> return ())

      return (ast'', env)

    case res of
      Right (astAndEnv, InferState _ _ [] warnings) -> do
        return (astAndEnv, (warnings, mempty))

      Right ((ast, env), InferState _ _ errors warnings) ->
        return ((ast { Slv.apath = Just path }, env), (warnings, errors))

      Left error ->
        return ((emptySlvAST { Slv.apath = Just path }, initialEnv'), (mempty, [error]))

  AllSolvedASTsWithEnvs -> nonInput $ do
    pathsToBuild <- Rock.fetch $ ModulePathsToBuild (optEntrypoint options)
    astTable <- mapM
      (\path -> do
        solved <- Rock.fetch $ SolvedASTWithEnv path
        return (path, solved)
      )
      pathsToBuild
    return (Map.fromList astTable, (mempty, mempty))


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

        (e@(Slv.Typed _ _ (Slv.Assignment n _)) : _) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment n _)))) : _) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment n _)) _ _)) : _) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment n _)))) _ _)) : _) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Extern _ n _)))) : _) | n == name ->
          (Just e, (mempty, mempty))

        (e@(Slv.Typed _ _ (Slv.Extern _ n _)) : _) | n == name ->
          (Just e, (mempty, mempty))

        (_ : next) ->
          findExpByName name next

  ForeignMethod modulePath methodName methodCallType -> nonInput $ do
    (slvAst, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    matchingMethods <-
      mapM
        (\(Slv.Untyped _ (Slv.Instance _ _ _ methods)) ->
          case Map.lookup methodName methods of
            Nothing ->
              return []

            Just (method, _) -> do
              let t = Slv.getType method
              unified <- runInfer (Unify.unify t methodCallType)
              case unified of
                Left _ ->
                  return []

                Right _ ->
                  return [method]
        )
        (Slv.ainstances slvAst)

    case concat matchingMethods of
      [] ->
        return (Nothing, (mempty, mempty))

      (found : _) ->
        return (Just found, (mempty, mempty))

  SolvedMethodNode methodName methodCallType -> nonInput $ do
    astTable <- Rock.fetch AllSolvedASTsWithEnvs
    found <- findMethodByNameAndType (Map.elems astTable) methodName methodCallType
    return (found, (mempty, mempty))

  DefinesInterfaceForMethod modulePath methodName -> nonInput $ do
    (slvAst, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    matched <- mapM
      (\(Slv.Untyped _ (Slv.Interface _ _ _ methods _)) ->
        case Map.lookup methodName methods of
          Nothing ->
            return False

          Just _ -> do
            return True
      )
      (Slv.ainterfaces slvAst)

    return (or matched, (mempty, mempty))

  ForeignConstructor modulePath name -> nonInput $ do
    (Slv.AST { Slv.atypedecls }, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    let allConstructors = concat $ Maybe.mapMaybe Slv.getADTConstructors atypedecls
    return (List.find ((== name) . Slv.getConstructorName) allConstructors, (mempty, mempty))

  ForeignTypeDeclaration modulePath name -> nonInput $ do
    (Slv.AST { Slv.atypedecls }, _) <- Rock.fetch $ SolvedASTWithEnv modulePath
    return ( List.find
              (\fullTd@(Slv.Untyped _ td) ->
                Slv.isADT fullTd && Slv.adtname td == name
                || Slv.isAlias fullTd && Slv.aliasname td == name
              )
              atypedecls
           , (mempty, mempty)
           )

  MonomorphizedProgram -> nonInput $ do
    liftIO $ atomicModifyIORef monomorphizationState (const (mempty, ()))
    liftIO $ atomicModifyIORef monomorphizationImports (const (mempty, ()))
    liftIO $ atomicModifyIORef monomorphicMethods (const (mempty, ()))

    mainFn <- MM.findExpByName (optEntrypoint options) "main"
    case mainFn of
      Just (fn, modulePath) -> do
        liftIO $ putStrLn "Running monomorphization.."
        let localState = makeLocalMonomorphizationState ()
        MM.monomorphizeDefinition
          (optTarget options)
          True
          MM.Env
            { MM.envCurrentModulePath = modulePath
            , MM.envSubstitution = mempty
            , MM.envLocalState = localState
            , MM.envEntrypointPath = optEntrypoint options
            , MM.envLocalBindingsToExclude = mempty
            }
          "main"
          (Slv.getType fn)

        noColor <- liftIO $ lookupEnv "NO_COLOR"
        when (noColor == Just "" || noColor == Nothing) $ do
          liftIO $ putStrLn ""
        liftIO $ putStrLn "Monomorphization complete."

      _ ->
        return ()

    state <- liftIO $ readIORef monomorphizationState
    imports <- liftIO $ readIORef monomorphizationImports
    methods <- liftIO $ readIORef monomorphicMethods

    return ((state, imports, methods), (mempty, mempty))

  MonomorphizedAST path -> nonInput $ do
    Rock.fetch MonomorphizedProgram
    (ast, _) <- Rock.fetch $ SolvedASTWithEnv path
    merged <- liftIO $ MM.mergeResult ast

    return (merged, (mempty, mempty))

  CoreAST path -> nonInput $ do
    monomorphicAST <- Rock.fetch $ MonomorphizedAST path
    let optLevel = optOptimizationLevel options

    case optTarget options of
      TLLVM -> do
        coreAst <- astToCore False monomorphicAST

        -- Only runs for the REPL
        let coreAst' = SortExpressions.keepLastMainExpAndDeps coreAst

        let sortedAST = SortExpressions.sortASTExpressions coreAst'
        let renamedAst       = Rename.renameAST sortedAST
            reducedAst       =
              if optLevel > O1 then
                SimplifyCalls.reduceAST renamedAst
              else
                renamedAst
            tceResolved      =
              if optLevel > O0 then
                TCE.resolveAST reducedAst
              else
                reducedAst
            closureConverted = ClosureConvert.convertAST tceResolved
            folded           =
              if optLevel > O1 then
                FoldCalls.foldAST closureConverted
              else
                closureConverted

        return (folded, (mempty, mempty))

      _ -> do
        coreAst <- astToCore (optOptimized options) monomorphicAST

        -- Only runs for the REPL
        let coreAst' = SortExpressions.keepLastMainExpAndDeps coreAst

        let renamedAst       = Rename.renameAST coreAst'
            reducedAst       =
              if optLevel > O1 then
                SimplifyCalls.reduceAST renamedAst
              else
                renamedAst
            tceResolved      =
              if optLevel > O0 then
                TCE.resolveAST reducedAst
              else
                reducedAst

        return (tceResolved, (mempty, mempty))

  PropagatedAST path -> nonInput $ do
    coreAST <- Rock.fetch $ CoreAST path
    if optOptimizationLevel options > O2 then do
      (propagatedAST, _) <- runStateT (HigherOrderCopyPropagation.propagateAST coreAST) (HigherOrderCopyPropagation.PropagationState 0 [] Map.empty)
      return (propagatedAST, (mempty, mempty))
    else
      return (coreAST, (mempty, mempty))

  ForeignCoreExp modulePath name -> nonInput $ do
    coreAST <- Rock.fetch $ CoreAST modulePath
    let found = findExpByName name (Core.aexps coreAST)
    return (found, (mempty, mempty))
    where
      findExpByName name exps = List.find (\e -> Core.getExpName e == Just name) exps

  BuiltObjectFile path -> nonInput $ do
    coreAst                           <- Rock.fetch $ PropagatedAST path
    builtModule@(_, _, objectContent) <- LLVM.compileModule options coreAst

    liftIO $ do
      let outputFolder   = takeDirectory (optOutputPath options)
      let outputPath     = Path.computeLLVMTargetPath outputFolder (optRootPath options) path
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
    let computedOutputPath = computeTargetPath (optOutputPath options) (optRootPath options) path

    liftIO $ do
      createDirectoryIfMissing True $ Path.takeDirectoryIfFile computedOutputPath
      writeFile computedOutputPath jsModule
    return (jsModule, (mempty, mempty))

  BuiltTarget path -> nonInput $ do
    globalWarnings <- liftIO globalChecks
    paths <- Rock.fetch $ ModulePathsToBuild path

    if optTarget options == TLLVM then do
      moduleResults <- mapM (Rock.fetch . BuiltObjectFile) paths
      let moduleEnvs = (\(_, env, _) -> env) <$> moduleResults

      if any (null . LLVMEnv.envASTPath) moduleEnvs then
        return ((), (globalWarnings, mempty))
      else do
        staticLibPaths <- Rock.fetch $ StaticLibPathsToLink path
        LLVM.buildTarget options staticLibPaths path
        return ((), (globalWarnings, mempty))

    else do
      mainAST <- mergedMainAST (optEntrypoint options)
      let mainASTWithSortedExps = SortExpressions.sortASTExpressions mainAST
      paths    <- Rock.fetch $ ModulePathsToBuild (optEntrypoint options)
      jsModule <- liftIO $ Javascript.generateJSModule options paths mainASTWithSortedExps
      let computedOutputPath = computeTargetPath (optOutputPath options) (optRootPath options) path

      liftIO $ do
        createDirectoryIfMissing True $ Path.takeDirectoryIfFile computedOutputPath
        writeFile computedOutputPath jsModule

      liftIO $ Javascript.generateInternalsModule options

      when (optBundle options) $ do
        let mainOutputPath = computeTargetPath (optOutputPath options) (optRootPath options) path
        result <- liftIO $ Javascript.runBundle mainOutputPath (optTarget options)
        case result of
          Left err ->
            liftIO $ putStr err

          Right ("", stderr) ->
            liftIO $ putStrLn stderr

          Right (bundle, _) -> do
            liftIO $ writeFile (optOutputPath options) bundle

        return ()

      return ((), (globalWarnings, mempty))

  StaticLibPathsToLink _ -> nonInput $ do
    madlibModulesExist <- liftIO $ doesPathExist "madlib_modules"
    modulePaths <-
      if madlibModulesExist then
        liftIO $ listDirectory "madlib_modules"
      else
        return []
    let allMadlibDotJsonPaths = "madlib.json" : ((\p -> joinPath ["madlib_modules", p, "madlib.json"]) <$> modulePaths)
    staticLibPaths <- liftIO $ concat <$>
      mapM
        (\p -> do
          let basePath = dropFileName p
          relPaths <- MadlibDotJson.getStaticLibPaths (optPathUtils options) p
          return $ joinPath . (basePath:) . (:[]) <$> relPaths
        )
        allMadlibDotJsonPaths

    return (staticLibPaths, (mempty, mempty))

  EnvVar name -> nonInput $ do
    value <- liftIO $ lookupEnv name
    return (value, (mempty, mempty))


globalChecks :: IO [CompilationWarning]
globalChecks = do
  parsedMadlibDotJson <- MadlibDotJson.loadCurrentMadlibDotJson

  case parsedMadlibDotJson of
    Right MadlibDotJson.MadlibDotJson { MadlibDotJson.madlibVersion = Just madlibVersion, MadlibDotJson.name = pkgName } ->
      case checkVersion pkgName madlibVersion version of
        Just warning ->
          return [warning]

        Nothing      ->
          return []

    _ -> return []


-- TODO: Move to AST.Source module
emptySrcAST :: Src.AST
emptySrcAST = Src.AST { Src.aimports = [], Src.aexps = [], Src.atypedecls = [], Src.ainstances = [], Src.ainterfaces = [], Src.aderived = [], Src.apath = Nothing }

-- TODO: Move to AST.Solved module
emptySlvAST :: Slv.AST
emptySlvAST = Slv.AST { Slv.aimports = [], Slv.aexps = [], Slv.atypedecls = [], Slv.ainstances = [], Slv.ainterfaces = [], Slv.apath = Nothing }


runInfer :: StateT InferState (ExceptT e m) a -> m (Either e (a, InferState))
runInfer a =
  runExceptT (runStateT a InferState { extensibleRecordsToDerive = mempty, count = 0, errors = [], Slv.warnings = [] })


mergedMainAST :: Rock.MonadFetch Query m => FilePath -> m Core.AST
mergedMainAST entrypoint = do
  paths <- Rock.fetch $ ModulePathsToBuild entrypoint
  let pathsWithoutMain = filter (/= entrypoint) paths
  astsToMerge <- mapM (Rock.fetch . CoreAST) pathsWithoutMain
  mainAST <- Rock.fetch $ CoreAST entrypoint
  let mainAst =
        foldr
          (\input dist ->
              dist
                { Core.atypedecls = Core.atypedecls input ++ Core.atypedecls dist
                , Core.aexps = Core.aexps input ++ Core.aexps dist
                , Core.aimports = []
                }
          )
          mainAST
          astsToMerge

  return mainAst


runCanonicalM :: ExceptT e (StateT Can.CanonicalState m) a -> m (Either e a, Can.CanonicalState)
runCanonicalM a =
  runStateT
    (runExceptT a)
    (
      Can.CanonicalState
        { Can.warnings = []
        , Can.namesAccessed = Set.empty
        , Can.namesDeclared = Set.empty
        , Can.accumulatedJS = ""
        , Can.typesToDerive = []
        , Can.derivedTypes = Set.empty
        , Can.placeholderIndex = 0
        , Can.parameterIndex = 0
        , Can.coverableInfo = []
        , Can.linesTracked = []
        , Can.anonymousFunctionIndex = 0
        , Can.nextBlockIndex = 0
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


findMethod :: String -> Type -> [Slv.Instance] -> Maybe Slv.Exp
findMethod methodName typeItsCalledWith instances = case instances of
  (Slv.Untyped _ (Slv.Instance _ _ _ methods)) : next ->
    case Map.lookup methodName methods of
      Nothing ->
        findMethod methodName typeItsCalledWith next

      Just (method, _) ->
        let t = Slv.getType method
        in  if Unify.quickMatch t typeItsCalledWith then
              Just method
            else
              findMethod methodName typeItsCalledWith next

  _ ->
    Nothing


findMethodByNameAndType :: (Rock.MonadFetch Query m, MonadIO m) => [(Slv.AST, SlvEnv.Env)] -> String -> Type -> m (Maybe (Slv.Exp, FilePath))
findMethodByNameAndType asts methodName typeItsCalledWith = case asts of
  (slvAst, _) : more ->
    case findMethod methodName typeItsCalledWith (Slv.ainstances slvAst) of
      Just found ->
        return $ Just (found, Maybe.fromMaybe undefined (Slv.apath slvAst))

      Nothing -> do
        findMethodByNameAndType more methodName typeItsCalledWith

  [] ->
    return Nothing


noError :: (Monoid w, Functor f) => f a -> f ((a, Rock.TaskKind), w)
noError = fmap ((, mempty) . (, Rock.NonInput))


nonInput :: Functor f => f (a, w) -> f ((a, Rock.TaskKind), w)
nonInput = fmap $ first (, Rock.NonInput)


input :: (Monoid w, Functor f) => f a -> f ((a, Rock.TaskKind), w)
input = fmap ((, mempty) . (, Rock.Input))


-- Test runner

makeEmptyHook :: String -> Src.Exp
makeEmptyHook hookName = Src.Source emptyArea TargetAll (Src.Export (Src.Source emptyArea TargetAll (Src.Assignment hookName (Src.Source emptyArea TargetAll (Src.Abs [Src.Source emptyArea TargetAll "_"] [Src.Source emptyArea TargetAll (Src.App (Src.Source emptyArea TargetAll (Src.Var "of")) [Src.Source emptyArea TargetAll Src.LUnit])])))))


updateHooks :: Bool -> Bool -> [Src.Exp] -> [Src.Exp]
updateHooks foundBeforeAll foundAfterAll exps = case exps of
  (exp@(Src.Source _ _ (Src.Export (Src.Source _ _ (Src.Assignment name _)))) : more) ->
    if name == "beforeAll" then
      exp : updateHooks True foundAfterAll more
    else if name == "afterAll" then
      exp : updateHooks foundBeforeAll True more
    else
      exp : updateHooks foundBeforeAll foundAfterAll more

  (exp@(Src.Source _ _ (Src.Assignment name _)) : more) ->
    if name == "beforeAll" then
      Src.Source emptyArea TargetAll (Src.Export exp) :  updateHooks True foundAfterAll more
    else if name == "afterAll" then
      Src.Source emptyArea TargetAll (Src.Export exp) :  updateHooks foundBeforeAll True more
    else
      exp : updateHooks foundBeforeAll foundAfterAll more

  (exp@(Src.Source _ _ (Src.NamedTypedExp name (Src.Source _ _ (Src.Export _)) _)) : more) ->
    if name == "beforeAll" then
      exp : updateHooks True foundAfterAll more
    else if name == "afterAll" then
      exp : updateHooks foundBeforeAll True more
    else
      exp : updateHooks foundBeforeAll foundAfterAll more

  (exp@(Src.Source _ _ (Src.NamedTypedExp name e typing)) : more) ->
    if name == "beforeAll" then
      Src.Source emptyArea TargetAll (Src.NamedTypedExp "beforeAll" (Src.Source emptyArea TargetAll (Src.Export e)) typing) :  updateHooks True foundAfterAll more
    else if name == "afterAll" then
      Src.Source emptyArea TargetAll (Src.NamedTypedExp "afterAll" (Src.Source emptyArea TargetAll (Src.Export e)) typing) :  updateHooks foundBeforeAll True more
    else
      exp : updateHooks foundBeforeAll foundAfterAll more

  (exp : more) ->
    exp : updateHooks foundBeforeAll foundAfterAll more

  [] ->
    if not foundBeforeAll && not foundAfterAll then
      [makeEmptyHook "beforeAll", makeEmptyHook "afterAll"]
    else if not foundBeforeAll then
      [makeEmptyHook "beforeAll"]
    else if not foundAfterAll then
      [makeEmptyHook "afterAll"]
    else
      []


addWishImport :: FilePath -> [Src.Import] -> [Src.Import]
addWishImport wishModulePath imports = case imports of
  (i : is) ->
    let (_, p) = Src.getImportPath i
    in  if p == "Wish" then
          i : is
        else
          i : addWishImport wishModulePath is

  [] ->
    [Src.Source emptyArea TargetAll (Src.NamedImport [] "Wish" wishModulePath)]


addTestEmptyExports :: FilePath -> Src.AST -> Src.AST
addTestEmptyExports wishModulePath ast@Src.AST{ Src.apath = Just apath } =
  if ".spec.mad" `List.isSuffixOf` apath then
    let exps             = Src.aexps ast
        imports          = Src.aimports ast
        exps'            = updateHooks False False exps
        imports'         = addWishImport wishModulePath imports
        -- that export is needed for type checking or else we get an error that the name is not exported
        testsExport      = Src.Source emptyArea Src.TargetAll (Src.Export (Src.Source emptyArea Src.TargetAll (Src.Assignment "__tests__" (Src.Source emptyArea Src.TargetAll (Src.ListConstructor [])))))
    in  ast { Src.aexps = exps' ++ [testsExport], Src.aimports = imports' }
  else
    ast
addTestEmptyExports _ ast =
  ast


data TestAssignment
  = SingleTest String
  | BatchTest String


generateTestAssignment :: Int -> Slv.Exp -> (Slv.Exp, Maybe TestAssignment)
generateTestAssignment index exp = case exp of
  Slv.Typed qt@(_ :=>
    (TApp
      (TCon (TC "List" (Kfun Star Star)) "prelude")
      (TApp
        (TApp (TCon (TC "Wish" _) _) (TCon (TC "TestResult" _) _))
        (TCon (TC "TestResult" _) _))))
    area
    _ ->
      let assignmentName = "__t" <> show index <> "__"
      in (Slv.Typed qt area (Slv.Assignment assignmentName exp), Just (BatchTest assignmentName))

  Slv.Typed qt@(_ :=> TApp (TApp (TCon (TC "Wish" _) _) (TCon (TC "TestResult" _) _)) (TCon (TC "TestResult" _) _)) area _ ->
    let assignmentName = "__t" <> show index <> "__"
    in  (Slv.Typed qt area (Slv.Assignment assignmentName exp), Just (SingleTest assignmentName))

  _ ->
    (exp, Nothing)


testType :: FilePath -> FilePath -> Type
testType wishPath testModulePath =
  TApp (TApp (TCon (TC "Wish" (Kfun Star (Kfun Star Star))) wishPath)
    (TCon (TC "TestResult" (Kfun Star Star)) testModulePath))
    (TCon (TC "TestResult" (Kfun Star Star)) testModulePath)


testListType :: FilePath -> FilePath -> Type
testListType wishPath testModulePath =
  TApp
    (TCon (TC "List" (Kfun Star Star)) "prelude")
    (testType wishPath testModulePath)


addTestsToSuite :: FilePath -> FilePath -> Slv.Exp -> [TestAssignment] -> Slv.Exp
addTestsToSuite wishPath testModulePath currentTests assignments = case assignments of
  [] ->
    currentTests

  (SingleTest name : more) ->
    let testExp =
          Slv.Typed
            ([] :=> tListOf (testListType wishPath testModulePath))
            emptyArea
            (Slv.ListConstructor
              [Slv.Typed ([] :=> testListType wishPath testModulePath) emptyArea (Slv.ListItem (Slv.Typed ([] :=> testListType wishPath testModulePath) emptyArea (Slv.Var name False)))])
        added =
          Slv.Typed
            ([] :=> testListType wishPath testModulePath)
            emptyArea
            (Slv.App
              (Slv.Typed
                ([] :=> (testListType wishPath testModulePath `fn` testListType wishPath testModulePath))
                emptyArea
                (Slv.App
                  (Slv.Typed ([] :=> (testListType wishPath testModulePath `fn` testListType wishPath testModulePath `fn` testListType wishPath testModulePath)) emptyArea (Slv.Var "__List__.concat" False))
                  currentTests
                  False))
              testExp
              True)
    in  addTestsToSuite wishPath testModulePath added more

  (BatchTest name : more) ->
    let batchTestExp =
          Slv.Typed
            ([] :=> tListOf (testListType wishPath testModulePath))
            emptyArea
            (Slv.Var name False)
        added =
          Slv.Typed
            ([] :=> testListType wishPath testModulePath)
            emptyArea
            (Slv.App
              (Slv.Typed
                ([] :=> (testListType wishPath testModulePath `fn` testListType wishPath testModulePath))
                emptyArea
                (Slv.App
                  (Slv.Typed ([] :=> (testListType wishPath testModulePath `fn` testListType wishPath testModulePath `fn` testListType wishPath testModulePath)) emptyArea (Slv.Var "__List__.concat" False))
                  currentTests
                  False))
              batchTestExp
              True)
    in  addTestsToSuite wishPath testModulePath added more


listImport :: FilePath -> Slv.Import
listImport listModulePath =
  Slv.Untyped emptyArea (Slv.DefaultImport (Slv.Untyped emptyArea "__List__") "List" listModulePath)


updateTestExports :: FilePath -> FilePath -> FilePath -> Slv.AST -> Slv.AST
updateTestExports wishPath testModulePath listPath ast@Slv.AST{ Slv.apath = Just apath, Slv.aexps } =
  if ".spec.mad" `List.isSuffixOf` apath && not (null aexps) then
    let exps              = init aexps
        assigned          = uncurry generateTestAssignment <$> zip [0..] exps
        exps'             = fst <$> assigned
        testAssignments   = Maybe.mapMaybe snd assigned
        initialTests      = Slv.Typed ([] :=> testListType wishPath testModulePath) emptyArea (Slv.ListConstructor [])
        testsExport       =
          Slv.Typed
            ([] :=> tListOf (testListType wishPath testModulePath))
            emptyArea
            (Slv.Export
              (Slv.Typed
                ([] :=> tListOf (testListType wishPath testModulePath))
                emptyArea
                (Slv.Assignment
                  "__tests__"
                  (addTestsToSuite wishPath testModulePath initialTests testAssignments))))
    in  ast { Slv.aexps = exps' ++ [testsExport], Slv.aimports = listImport listPath : Slv.aimports ast }
  else
    ast
updateTestExports _ _ _ ast =
  ast
