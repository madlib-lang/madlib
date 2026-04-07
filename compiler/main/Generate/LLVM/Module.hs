{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Generate.LLVM.Module
  ( generateLLVMModule
  , generateModule
  , compileModule
  , buildObjectFile
  , makeExecutablePath
  , buildTarget
  , emitLLVMIR
  , generateImport
  , buildSymbolTableFromImports
  , hashModulePath
  , generateModuleFunctionExternals
  , callModuleFunctions
  , getLLVMParameterTypes
  , getLLVMReturnType
  , generateExternalForName
  , generateExternForImportName
  , buildSymbolTableFromImportInfo
  , buildSymbolTableFromImport
  ) where

import qualified Data.Map                     as Map
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Control.Monad                as Monad
import qualified Control.Monad.Fix            as MonadFix
import qualified Control.Monad.Writer         as Writer
import qualified Control.Monad.State          as State
import           Data.ByteString              as ByteString
import           Data.ByteString.Short        as ShortByteString
import           Data.ByteString.Char8        as Char8
import           System.Process               (callCommand)
import           System.Environment.Executable (getExecutablePath)
import           System.FilePath              (takeDirectory, joinPath, replaceExtension, splitPath)
import           System.Directory            (createDirectoryIfMissing, canonicalizePath, doesDirectoryExist, doesFileExist)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified System.IO                    as SystemIO
import           System.Environment           (getEnv)
import           Control.Exception            (try)

import           LLVM.Target                  (withHostTargetMachineDefault)
import           LLVM.Module                  (withModuleFromAST, moduleObject, moduleLLVMAssembly)
import           LLVM.AST                     as AST hiding (function)
import qualified LLVM.AST                     as LLVMAST
import           LLVM.AST.Type                as Type
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand hiding (Module)
import qualified LLVM.AST.Global              as Global
import qualified LLVM.AST.Linkage             as Linkage
import qualified Distribution.System          as DistributionSystem
import           LLVM.AST.ParameterAttribute  (ParameterAttribute)
import           LLVM.AST.Attribute           (FunctionAttribute(..))
import qualified LLVM.AST.FunctionAttribute   as FunctionAttribute

import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Constant      as C
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction   as Instruction
import           LLVM.Context                 (withContext)
import           LLVM.PassManager

import           AST.Core                     as Core
import qualified Infer.Type                   as IT
import           Infer.Type                   (isFunctionType)
import qualified Driver.Query                 as Query
import qualified Rock
import           Run.Options
import           Run.OptimizationLevel
import           Run.PGOMode (PGOMode(..))
import qualified Utils.Path                   as Path
import qualified Utils.IO                     as IOUtils
import           Utils.Hash                   (generateHashFromPath, hash)
import qualified Data.ByteString.Lazy.Char8   as BLChar8
import qualified Data.String.Utils            as ListUtils
import qualified Canonicalize.Env             as CanEnv
import qualified AST.Solved                   as Slv

import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import           Generate.LLVM.Types          (boxType, listType, stringType, papType, recordType, buildLLVMType, buildLLVMType', adtSymbol)
import           Generate.LLVM.Builtins
import           Generate.LLVM.WithMetadata   (functionWithMetadata, functionWithMetadataC, callWithMetadata, callFastWithMetadata, externFast, declareWithAttributes)
import           Generate.LLVM.Debug
import           Generate.LLVM.Helper
import           Generate.LLVM.Function       (FunctionCtx, generateExps, generateConstructors, generateTopLevelFunctions,
                                                addTopLevelFnToSymbolTable, expsForMain, topLevelFunctions,
                                                fnSymbol, topLevelSymbol, constructorSymbol)

maxSupportedPAPArity :: Int
maxSupportedPAPArity = 50

mkPAPArityError :: String -> Int -> String
mkPAPArityError name arity =
  "LLVM import arity limit reached for '" <> name <> "' (arity " <> show arity
  <> ", supported max " <> show maxSupportedPAPArity <> "). "
  <> "Raise runtime/src/apply-pap.* max arity before compiling this module."


-- Symbol constructors (re-used for building symbol tables from imports)

getLLVMParameterTypes :: Type.Type -> [Type.Type]
getLLVMParameterTypes t = case t of
  Type.PointerType (Type.FunctionType _ paramTypes _) _ ->
    paramTypes

  _ ->
    error $ "getLLVMParameterTypes: expected function pointer type, got: " <> show t

getLLVMReturnType :: Type.Type -> Type.Type
getLLVMReturnType t = case t of
  Type.PointerType (Type.FunctionType returnType _ _) _ ->
    returnType

  _ ->
    error $ "getLLVMReturnType: expected function pointer type, got: " <> show t


generateExternalForName :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> String -> IT.Type -> Core.ImportType -> m ()
generateExternalForName symbolTable name t importType = case importType of
  Core.DefinitionImport arity -> do
    let paramTypes = List.replicate arity boxType
        returnType = boxType
    externFast (AST.mkName name) paramTypes returnType
    return ()

  Core.ConstructorImport -> do
    let arity  = List.length $ IT.getParamTypes t
        paramTypes = List.replicate arity boxType
        returnType = boxType
    externFast (AST.mkName name) paramTypes returnType
    return ()

  Core.ExpressionImport -> do
    let expType =
          if IT.isFunctionType t then
            papType
          else
            buildLLVMType initialEnv symbolTable ([] IT.:=> t)
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = expType, Global.linkage = Linkage.External }
    let def = AST.GlobalDefinition g
    emitDefn def
    return ()


generateExternForImportName :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Core.Core ImportInfo -> m ()
generateExternForImportName symbolTable optimizedName = case optimizedName of
  Core.Typed (_ IT.:=> t) _ _ (Core.ImportInfo name importType) ->
    generateExternalForName symbolTable name t importType

  _ ->
    error "Unreachable: generateExternForImportName called with non-ImportInfo expression"


generateImport :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Import -> m ()
generateImport symbolTable imp = case imp of
  Core.Untyped _ _ (NamedImport names _ _) -> do
    mapM_ (generateExternForImportName symbolTable) names

  _ ->
    error "Unreachable: generateImport called with non-NamedImport"


buildSymbolTableFromImportInfo :: (Rock.MonadFetch Query.Query m, MonadIO m) => Core ImportInfo -> m SymbolTable
buildSymbolTableFromImportInfo importInfo = case importInfo of
  Typed qt@(_ IT.:=> t) _ _ (ImportInfo name ExpressionImport) ->
    if IT.isFunctionType t then do
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType, Type.i8]
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      return $ Map.singleton name (topLevelSymbol globalRef)
    else do
      (expType, maybeAdtSymbol) <- buildLLVMType' qt
      let globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      let adtSym = case maybeAdtSymbol of
            Just (n, s) ->
              Map.singleton n s

            _ ->
              Map.empty
      return $ Map.singleton name (topLevelSymbol globalRef) <> adtSym

  Typed _ _ _ (ImportInfo name (DefinitionImport arity)) -> do
    Monad.when (arity > maxSupportedPAPArity) $
      error (mkPAPArityError name arity)

    let fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName name))
    return $ Map.singleton name (fnSymbol arity fnRef)

  Typed (_ IT.:=> t) _ _ (ImportInfo name ConstructorImport) -> do
    let paramTypes = IT.getParamTypes t
        adtType = IT.getReturnType t
        adtTypeName = IT.getTConName adtType
        adtTypePath = IT.getTConPath adtType
        arity  = List.length paramTypes
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName name))

    Monad.when (arity > maxSupportedPAPArity) $
      error (mkPAPArityError name arity)

    constructorInfos <- Rock.fetch (Query.ForeignConstructorInfos adtTypePath adtTypeName)
    let constructorIndex = case constructorInfos of
          Just infos ->
            let sortedInfos = List.sortBy (\(CanEnv.ConstructorInfo a _) (CanEnv.ConstructorInfo b _) -> compare a b) infos
            in  case List.findIndex (\(CanEnv.ConstructorInfo n _) -> ("_" <> generateHashFromPath adtTypePath <> "_" <> n) == name) sortedInfos of
                  Just index ->
                    index

                  _ ->
                    0

          _ ->
            0

    maybeADT <- Rock.fetch $ Query.ForeignTypeDeclaration adtTypePath adtTypeName
    let adtSym = case maybeADT of
          Nothing ->
            Map.empty

          Just (Slv.Untyped _ adt) ->
            let maxArity = Slv.findMaximumConstructorArity (Slv.adtconstructors adt)
                ctorCount = List.length (Slv.adtconstructors adt)
            in  Map.singleton (adtTypePath <> "_" <> adtTypeName) (adtSymbol maxArity ctorCount)

          _ ->
            Map.empty

    return $ Map.singleton name (constructorSymbol fnRef constructorIndex arity) <> adtSym

  _ ->
    error "Unreachable: buildSymbolTableFromImportInfo called with untyped import info"


buildSymbolTableFromImport :: (Rock.MonadFetch Query.Query m, MonadIO m) => Import -> m SymbolTable
buildSymbolTableFromImport imp = case imp of
  Untyped _ _ (NamedImport infos _ _) -> do
    results <- mapM buildSymbolTableFromImportInfo infos
    return $ mconcat results

  _ ->
    return Map.empty


buildSymbolTableFromImports :: (Rock.MonadFetch Query.Query m, MonadIO m) => [Import] -> m SymbolTable
buildSymbolTableFromImports imports = do
  results <- mapM buildSymbolTableFromImport imports
  return $ mconcat results


hashModulePath :: AST -> String
hashModulePath ast =
  generateHashFromPath $ Maybe.fromMaybe "" (apath ast)


generateModuleFunctionExternals :: (MonadModuleBuilder m) => [FilePath] -> m ()
generateModuleFunctionExternals allModulePaths = case allModulePaths of
  (path : next) -> do
    let functionName = "_" <> generateHashFromPath path <> "_moduleFunction"
    extern (AST.mkName functionName) [] Type.void

    generateModuleFunctionExternals next

  [] ->
    return ()


callModuleFunctions :: (MonadIRBuilder m, MonadModuleBuilder m) => [FilePath] -> m ()
callModuleFunctions allModulePaths = case allModulePaths of
  (path : next) -> do
    let functionName = "_" <> generateHashFromPath path <> "_moduleFunction"
    call (Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName functionName)) []

    callModuleFunctions next

  [] ->
    return ()


generateLLVMModule :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, Writer.MonadFix m, MonadModuleBuilder m)
                   => FunctionCtx (IRBuilderT m)
                   -> (Operand -> Type -> IRBuilderT m Operand)
                   -> Env -> Bool -> [String] -> SymbolTable -> AST -> m ()
generateLLVMModule _ _ _ _ _ _ Core.AST{ Core.apath = Nothing } = error "generateLLVMModule: AST has no path"
generateLLVMModule ctx safeBitcastFn env isMain currentModulePaths initialSymbolTable ast@Core.AST{ Core.apath = Just astPath } = do
  env' <-
    if envIsDebugBuild env then do
      fileSymbolId <- newMetadataId
      compilationUnitSymbolId <- newMetadataId
      emitDefn $ NamedMetadataDefinition "llvm.dbg.cu" [MetadataNodeID compilationUnitSymbolId]
      emitDefn $ makeCompileUnitMetadata fileSymbolId compilationUnitSymbolId
      emitDefn $ makeFileMetadata fileSymbolId astPath

      debugInfoVersionId <- newMetadataId
      emitDefn $ MetadataNodeDefinition (MetadataNodeID debugInfoVersionId)
                    (MDTuple [ Just (MDValue (int32 2))
                             , Just (MDString "Debug Info Version")
                             , Just (MDValue (int32 3))
                             ])

      dwarfVersionId <- newMetadataId
      emitDefn $ MetadataNodeDefinition (MetadataNodeID dwarfVersionId)
                    (MDTuple [ Just (MDValue (int32 2))
                             , Just (MDString "Dwarf Version")
                             , Just (MDValue (int32 3))
                             ])

      emitDefn $ NamedMetadataDefinition "llvm.module.flags"
        [ MetadataNodeID debugInfoVersionId
        , MetadataNodeID dwarfVersionId
        ]

      return env
              { envCurrentCompilationUnitSymbolIndex = compilationUnitSymbolId
              , envCurrentFileSymbolIndex = fileSymbolId
              }
    else
      return env

  symbolTableWithConstructors <- generateConstructors env' initialSymbolTable (atypedecls ast)
  let symbolTableWithTopLevel  = List.foldr (flip (addTopLevelFnToSymbolTable env')) symbolTableWithConstructors (aexps ast)

  let moduleHash = hashModulePath ast
  let moduleFunctionName =
        if isMain then
          "main"
        else
          "_" <> moduleHash <> "_moduleFunction"

  mapM_ (generateImport initialSymbolTable) $ aimports ast

  symbolTable <- generateTopLevelFunctions ctx env' symbolTableWithTopLevel (topLevelFunctions $ aexps ast)

  externVarArgs (AST.mkName "__applyPAP__")                          [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)
  externVarArgs (AST.mkName "madlib__record__internal__buildRecord") [Type.i32, boxType] recordType
  extern (AST.mkName "__applyPAP4__")                                [boxType, boxType, boxType, boxType, boxType] boxType
  extern (AST.mkName "__applyPAP3__")                                [boxType, boxType, boxType, boxType] boxType
  extern (AST.mkName "__applyPAP2__")                                [boxType, boxType, boxType] boxType
  extern (AST.mkName "__applyPAP1__")                                [boxType, boxType] boxType
  extern (AST.mkName "madlib__process__internal__typedHoleReached")  [] Type.void
  extern (AST.mkName "madlib__process__internal__arrayOutOfBounds")  [Type.i64, Type.i64] Type.void

  declareWithAttributes [FunctionAttribute.NoUnwind, FunctionAttribute.ReadNone, FunctionAttribute.OptimizeNone, FunctionAttribute.NoInline] (AST.mkName "llvm.dbg.declare")                             [Type.MetadataType, Type.MetadataType, Type.MetadataType] Type.void

  extern (AST.mkName "madlib__record__internal__selectField")        [stringType, recordType] boxType
  extern (AST.mkName "madlib__string__internal__areStringsEqual")    [stringType, stringType] Type.i1
  extern (AST.mkName "madlib__string__internal__areStringsNotEqual") [stringType, stringType] Type.i1
  extern (AST.mkName "madlib__string__internal__concat")             [stringType, stringType] stringType

  extern (AST.mkName "madlib__list__internal__hasMinLength")         [Type.i64, listType] Type.i1
  extern (AST.mkName "madlib__list__internal__hasLength")            [Type.i64, listType] Type.i1
  extern (AST.mkName "madlib__list__singleton")                      [Type.ptr Type.i8] listType
  extern (AST.mkName "madlib__list__internal__push")                 [Type.ptr Type.i8, listType] listType
  extern (AST.mkName "madlib__list__concat")                         [listType, listType] listType

  extern (AST.mkName "GC_malloc")              [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc_atomic")       [Type.i64] (Type.ptr Type.i8)

  -- Perceus RC runtime functions
  extern (AST.mkName "rc_alloc")           [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "rc_inc")             [Type.ptr Type.i8] Type.void
  extern (AST.mkName "rc_dec")             [Type.ptr Type.i8] Type.void
  extern (AST.mkName "rc_dec_with_drop")   [Type.ptr Type.i8, Type.ptr (Type.FunctionType Type.void [Type.ptr Type.i8] False)] Type.void
  extern (AST.mkName "rc_is_unique")       [Type.ptr Type.i8] Type.i1
  extern (AST.mkName "rc_reuse")           [Type.ptr Type.i8, Type.i64] (Type.ptr Type.i8)

  extern (AST.mkName "!=")                     [boxType, boxType, boxType] boxType

  Monad.when isMain $ do
    extern (AST.mkName "madlib__process__internal__initExtra")    [] Type.void
    extern (AST.mkName "__main__init__")                          [Type.i32, Type.ptr (Type.ptr Type.i8)] Type.void
    extern (AST.mkName "__initEventLoop__")                       [] Type.void
    extern (AST.mkName "__startEventLoop__")                      [] Type.void
    extern (AST.mkName "madlib__process__internal__getArgs")      [] listType
    generateModuleFunctionExternals currentModulePaths

  moduleFunction <-
    if isMain then do
      let getArgs     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [] False) "madlib__process__internal__getArgs")
      let (Symbol _ mainFunction) = Maybe.fromMaybe (error $ "Main function not found: _" <> moduleHash <> "_main") $ Map.lookup ("_" <> moduleHash <> "_main") symbolTable
      -- this function starts the runtime with a fresh stack etc
      functionWithMetadataC [] (AST.mkName "__main__start__") [] Type.void $ \_ -> do
        block `named` "entry"
        call initExtra []
        call initEventLoop []

        callModuleFunctions currentModulePaths

        generateExps ctx env' symbolTable (expsForMain $ aexps ast)
        -- call user main that should be named like main_moduleHash
        mainArgs <- call getArgs []
        mainArgs' <- safeBitcastFn mainArgs boxType
        callFastWithMetadata [] mainFunction [(mainArgs', [])]
        call startEventLoop []
        retVoid

      let argc = (Type.i32, ParameterName $ stringToShortByteString "argc")
          argv = (Type.ptr (Type.ptr Type.i8), ParameterName $ stringToShortByteString "argv")
      functionWithMetadataC [] (AST.mkName moduleFunctionName) [argc, argv] Type.i32 $ \[argc, argv] -> do
        block `named` "entry"
        call mainInit [(argc, []), (argv, [])]
        ret $ i32ConstOp 0
    else do
      functionWithMetadataC [] (AST.mkName moduleFunctionName) [] Type.void $ \_ -> do
        block `named` "entry"
        generateExps ctx env' symbolTable (expsForMain $ aexps ast)
        retVoid

  Writer.tell $ Map.singleton moduleFunctionName (fnSymbol 0 moduleFunction)
  Writer.tell symbolTableWithTopLevel


generateModule :: (MonadIO m, Rock.MonadFetch Query.Query m, Writer.MonadFix m)
               => (forall n. (MonadIO n, Writer.MonadWriter SymbolTable n, State.MonadState Int n, MonadFix.MonadFix n, MonadIRBuilder n, MonadModuleBuilder n) => FunctionCtx n)
               -> (forall n. MonadIRBuilder n => Operand -> Type -> n Operand)
               -> Options -> AST -> m (LLVMAST.Module, SymbolTable, Env)
generateModule mkCtx safeBitcastFn options ast@Core.AST{ apath = Just modulePath } = do
  let imports = aimports ast
  symbolTable <- buildSymbolTableFromImports imports

  let isMain = optEntrypoint options == modulePath
  let envForAST =
        Env
          { isTopLevel = False
          , recursionData = Nothing
          , envASTPath = modulePath
          , envIsDebugBuild = optDebug options
          , envCurrentCompilationUnitSymbolIndex = 0
          , envCurrentFileSymbolIndex = 0
          , envCurrentSubProgramSymbolIndex = Nothing
          }
  let moduleName =
        if isMain then
          "main"
        else
          hashModulePath ast

  importModulePaths <-
    if isMain then do
      allPaths <- Rock.fetch $ Query.ModulePathsToBuild modulePath
      let importedPaths = List.filter (/= modulePath) allPaths
      Monad.filterM
        (\path -> do
          coreAst <- Rock.fetch $ Query.CoreAST path
          return $ not $ List.null $ expsForMain $ Core.aexps coreAst
        )
        importedPaths
    else
      return []

  ((mod, table), _) <- State.runStateT (Writer.runWriterT $ buildModuleT (stringToShortByteString moduleName) (generateLLVMModule mkCtx safeBitcastFn envForAST isMain importModulePaths symbolTable ast)) 0
  return (mod, table, envForAST)

generateModule _ _ _ _ =
  error "generateModule: invalid arguments"


compileModule :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m)
              => (forall n. (MonadIO n, Writer.MonadWriter SymbolTable n, State.MonadState Int n, MonadFix.MonadFix n, MonadIRBuilder n, MonadModuleBuilder n) => FunctionCtx n)
              -> (forall n. MonadIRBuilder n => Operand -> Type -> n Operand)
              -> Options -> Core.AST -> m (SymbolTable, Env, ByteString.ByteString)
compileModule _ _ _ Core.AST { Core.apath = Nothing } = return (mempty, initialEnv, Char8.pack "")
compileModule mkCtx safeBitcastFn options ast@Core.AST { Core.apath = Just modulePath } = do
  (astModule, table, env) <- generateModule mkCtx safeBitcastFn options ast

  objectContent <- liftIO $ buildObjectFile options astModule

  Monad.when (optEmitLLVM options) $ liftIO $ do
    irContent <- emitLLVMIR options astModule
    let outputFolder = takeDirectory (optOutputPath options)
    let objPath = Path.computeLLVMTargetPath outputFolder (optRootPath options) modulePath
    let irPath = replaceExtension objPath ".ll"
    createDirectoryIfMissing True $ takeDirectory irPath
    ByteString.writeFile irPath irContent

  pathsToBuild <- Rock.fetch $ Query.ModulePathsToBuild (optEntrypoint options)
  let rest = List.dropWhile (/= modulePath) pathsToBuild
  let total = List.length pathsToBuild
  let curr = total - List.length rest + 1
  let currStr = if curr < 10 then " " <> show curr else show curr
  liftIO $ SystemIO.hPutStrLn SystemIO.stdout $ "[" <> currStr <> " of "<> show total<>"] Compiled '" <> modulePath <> "'"

  return (table, env, objectContent)


buildObjectFile :: Options -> LLVMAST.Module -> IO ByteString.ByteString
buildObjectFile options astModule = case optPGOMode options of
  NoPGO -> buildObjectFileNative options astModule
  pgo   -> buildObjectFileViaCLang options pgo astModule


buildObjectFileNative :: Options -> LLVMAST.Module -> IO ByteString.ByteString
buildObjectFileNative options astModule = do
  let optLevel' =
        case optOptimizationLevel options of
          O0 ->
            Nothing

          O1 ->
            Just 1

          O2 ->
            Just 2

          O3 ->
            Just 3

  let inlineThreshold =
        if optDebug options || optOptimizationLevel options == O0 then
          Nothing
        else
          Just 200

  let isO0 = optOptimizationLevel options == O0

  withHostTargetMachineDefault $ \target -> do
      withContext $ \ctx -> do
        withModuleFromAST ctx astModule $ \mod' -> do
          mod'' <-
            withPassManager
              defaultCuratedPassSetSpec
                { optLevel = optLevel'
                , useInlinerWithThreshold = inlineThreshold
                , simplifyLibCalls = if isO0 then Nothing else Just True
                , loopVectorize = if isO0 then Just False else Just True
                , superwordLevelParallelismVectorize = if isO0 then Just False else Just True
                }
            $ \pm -> do
              runPassManager pm mod'
              return mod'
          moduleObject target mod''


-- | Compile via clang with PGO flags.  Runs the standard pass manager first
-- (same as 'buildObjectFileNative') to lower old-style @ptrtoint@ constant
-- expressions that clang 17 no longer accepts, then emits the processed IR to
-- a temp file and calls @clang++ -c@ with the appropriate PGO flag.
buildObjectFileViaCLang :: Options -> PGOMode -> LLVMAST.Module -> IO ByteString.ByteString
buildObjectFileViaCLang options pgo astModule = do
  let pgoFlag = case pgo of
        PGOInstrument   -> "-fprofile-generate"
        PGOOptimize f   -> "-fprofile-use=" <> f <> " -fprofile-correction"
        NoPGO           -> ""
  let optFlag = case optOptimizationLevel options of
        O0 -> "-O0"; O1 -> "-O1"; O2 -> "-O2"; O3 -> "-O3"
  -- Use per-module source filename for unique temp files (avoids race on parallel builds).
  -- We take 16 hex chars of the MD5 for low collision probability.
  let modPath    = Char8.unpack $ ShortByteString.fromShort $ LLVMAST.moduleSourceFileName astModule
      pathHash   = Prelude.take 16 $ hash $ BLChar8.pack modPath
      tmpIR      = "/tmp/madlib_pgo_" <> pathHash <> ".ll"
      tmpObj     = "/tmp/madlib_pgo_" <> pathHash <> ".o"
  let optLevel' = case optOptimizationLevel options of
        O0 -> Nothing
        O1 -> Just 1
        O2 -> Just 2
        O3 -> Just 3
  let isO0 = optOptimizationLevel options == O0
  let inlineThreshold = if optDebug options || isO0 then Nothing else Just 200
  withContext $ \ctx ->
    withModuleFromAST ctx astModule $ \mod' -> do
      -- Run pass manager first so that constant-expression ptrtoint nodes (the
      -- "sizeof idiom") are lowered to ordinary instructions that clang 17 accepts.
      mod'' <-
        withPassManager
          defaultCuratedPassSetSpec
            { optLevel                          = optLevel'
            , useInlinerWithThreshold           = inlineThreshold
            , simplifyLibCalls                  = if isO0 then Nothing else Just True
            , loopVectorize                     = if isO0 then Just False else Just True
            , superwordLevelParallelismVectorize = if isO0 then Just False else Just True
            }
          $ \pm -> do
            runPassManager pm mod'
            return mod'
      ir <- moduleLLVMAssembly mod''
      ByteString.writeFile tmpIR ir
      -- llvm-hs emits `ptrtoint (i64 N to i64)` no-op constant expressions that
      -- clang 17 rejects as invalid casts.  Strip the outer wrapper to leave just N.
      callCommand $ "sed -i '' 's/ptrtoint (i64 \\([0-9][0-9]*\\) to i64)/\\1/g' " <> tmpIR
      callCommand $ "clang++ -c " <> optFlag <> " " <> pgoFlag
                    <> " -Wno-override-module"
                    <> " -x ir " <> tmpIR <> " -o " <> tmpObj
      result <- ByteString.readFile tmpObj
      callCommand $ "rm -f " <> tmpIR <> " " <> tmpObj
      return result


emitLLVMIR :: Options -> LLVMAST.Module -> IO ByteString.ByteString
emitLLVMIR _ astModule = do
  withContext $ \ctx -> do
    withModuleFromAST ctx astModule $ \mod' -> do
      moduleLLVMAssembly mod'


makeExecutablePath :: FilePath -> FilePath
makeExecutablePath output = case output of
  "./build/" ->
    case DistributionSystem.buildOS of
      DistributionSystem.Windows ->
        "a.exe"

      _ ->
        "a.out"

  or ->
    or


-- | Walk up the directory tree from 'dir' looking for a 'runtime/lib' subdirectory.
-- Returns the directory that contains 'runtime/' when found, Nothing when the root is reached.
-- | Walk up the directory tree from 'dir' looking for the madlib runtime library.
-- Returns the directory that contains 'runtime/build/libruntime.a' when found.
findRuntimeDir :: FilePath -> IO (Maybe FilePath)
findRuntimeDir ""  = return Nothing
findRuntimeDir dir = do
  libExists <- doesFileExist (joinPath [dir, "runtime", "build", "libruntime.a"])
  if libExists
    then return (Just dir)
    else let parent = joinPath . Prelude.init . splitPath $ dir
         in  if Prelude.null parent || parent == dir
               then return Nothing
               else findRuntimeDir parent


buildTarget :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m) => Options -> [String] -> FilePath -> m ()
buildTarget options staticLibs entrypoint = do
  let outputFolder = takeDirectory (optOutputPath options)
  modulePaths <- Rock.fetch $ Query.ModulePathsToBuild entrypoint

  Rock.fetch $ Query.BuiltObjectFile entrypoint

  let objectFilePaths = Path.computeLLVMTargetPath outputFolder (optRootPath options) <$> modulePaths

  runtimeDir <- liftIO $ do
    exePath  <- getExecutablePath
    -- Search up from the invoking path first (direct invocation case).
    result   <- findRuntimeDir (takeDirectory exePath)
    case result of
      Just dir -> return dir
      Nothing  -> do
        -- Fall back to the canonicalized (symlink-resolved) path.
        realPath <- canonicalizePath exePath
        found    <- findRuntimeDir (takeDirectory realPath)
        return $ Maybe.fromMaybe (takeDirectory exePath) found
  let executablePath = makeExecutablePath (optOutputPath options)

  let objectFilePathsForCli = ListUtils.join " " objectFilePaths
      runtimeLibPathOpt     = "-L\"" <> joinPath [runtimeDir, "runtime", "lib"] <> "\""
      runtimeBuildPathOpt   = "-L\"" <> joinPath [runtimeDir, "runtime", "build"] <> "\""
      linkerOptFlag         =
        case optOptimizationLevel options of
          O0 -> "-O0"
          O1 -> "-O1"
          O2 -> "-O2"
          O3 -> "-O3"
      pgoLinkerFlag         =
        case optPGOMode options of
          NoPGO         -> ""
          PGOInstrument -> " -fprofile-generate"
          PGOOptimize f -> " -fprofile-use=" <> f <> " -fprofile-correction"

  liftIO $ IOUtils.putStrLnAndFlush "Linking.."

  ldflags <- liftIO $ try $ getEnv "LDFLAGS"
  let ldflags' = case (ldflags :: Either IOError String) of
        Left _ ->
          ""

        Right flags ->
          flags

  case DistributionSystem.buildOS of
    DistributionSystem.OSX ->
      liftIO $ callCommand $
        "clang++ -flto -dead_strip -foptimize-sibling-calls " <> linkerOptFlag <> pgoLinkerFlag <> " "
        <> objectFilePathsForCli
        <> " " <> runtimeLibPathOpt
        <> " " <> runtimeBuildPathOpt
        <> " " <> ListUtils.join " " staticLibs
        <> " -lruntime -lgc -lgccpp -luv -lpcre2-8"
        <> " -lcurl -framework CoreFoundation -framework SystemConfiguration -framework CoreFoundation -lssl -lcrypto -lz"
        <> " " <> ldflags' <> " "
        <>" -o " <> executablePath

    DistributionSystem.Windows ->
      liftIO $ callCommand $
        "g++ -static " <> linkerOptFlag <> pgoLinkerFlag <> " "
        <> objectFilePathsForCli
        <> " " <> runtimeLibPathOpt
        <> " " <> runtimeBuildPathOpt
        <> " " <> ListUtils.join " " staticLibs
        <> " -lruntime -lmman -lgc -lgccpp -luv -lpcre2-8 -pthread -ldl -lws2_32 -liphlpapi -lUserEnv -lcurl -lz -lssl -lcrypto -lgdi32 -lcrypt32 -lwldap32 -lws2_32"
        <> " " <> ldflags' <> " "
        <> " -o " <> executablePath

    _ ->
      liftIO $ callCommand $
        "g++ -static " <> linkerOptFlag <> pgoLinkerFlag <> " "
        <> objectFilePathsForCli
        <> " " <> runtimeLibPathOpt
        <> " " <> runtimeBuildPathOpt
        <> " " <> ListUtils.join " " staticLibs
        <> " -lruntime -lgc -lgccpp -luv -lpcre2-8 -lcurl -lssl -lcrypto -lz -pthread -ldl -Wl,--allow-multiple-definition"
        <> " " <> ldflags' <> " "
        <> " -o " <> executablePath
