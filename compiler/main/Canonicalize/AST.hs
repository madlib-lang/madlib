{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Canonicalize.AST where


import           Run.Target
import qualified AST.Source                               as Src
import qualified AST.Canonical                            as Can
import           Canonicalize.Env                         as CanEnv
import           Canonicalize.Canonicalize
import qualified Parse.Madlib.AST                         as P
import           Canonicalize.CanonicalM
import           Canonicalize.InstanceToDerive
import           Canonicalize.ADT
import           Canonicalize.Interface
import           Canonicalize.EnvUtils
import           Infer.Type
import           Error.Error
import           Error.Warning
import           Error.Context
import           Data.Maybe
import qualified Data.Map                                 as M
import qualified Data.Set                                 as S
import qualified Utils.Tuple                              as T
import           Control.Monad.Except
import           Explain.Location
import           Text.Regex.TDFA
import           AST.Solved (Import_(NamedImport))
import           Canonicalize.Derive
import           Data.List
import           Utils.List
import           AST.Canonical (Import_(TypeImport))
import qualified Driver.Query as Query
import qualified Rock
import Run.Options



findAllExportedNames :: Can.AST -> [Can.Name]
findAllExportedNames ast =
  let exportedADTs = filter Can.isTypeDeclExported (Can.atypedecls ast)
      ctors        = concat $ Can.getCtors <$> exportedADTs
      ctorNames    = Can.getCtorName <$> ctors
      varNames     = mapMaybe Can.getExportName (Can.aexps ast)
  in  ctorNames ++ varNames


findAllExportedTypeNames :: Can.AST -> [Can.Name]
findAllExportedTypeNames ast =
  let exportedADTs    = filter Can.isTypeDeclExported (Can.atypedecls ast)
      typeExportNames = Can.getTypeExportName <$> filter Can.isTypeExport (Can.aexps ast)
      typeNames       = Can.getTypeDeclName <$> exportedADTs
  in  typeExportNames ++ typeNames


validateImport :: FilePath -> Src.Import -> CanonicalM ()
validateImport originAstPath imp = do
  let path = Src.getImportAbsolutePath imp
  (ast, _, _, _) <- Rock.fetch $ Query.CanonicalizedASTWithEnv path

  let allExportNames   = findAllExportedNames ast
  let allImportNames   = Src.getImportNames imp
  let namesNotExported = filter (not . (`elem` allExportNames) . Src.getSourceContent) allImportNames

  let allExportTypes   = findAllExportedTypeNames ast
  let allImportTypes   = Src.getImportTypeNames imp
  let typesNotExported = filter (not . (`elem` allExportTypes) . Src.getSourceContent) allImportTypes

  let allNotExported   = namesNotExported ++ typesNotExported

  unless
    (null allNotExported)
    (throwError $ CompilationError (NotExported (Src.getSourceContent $ head allNotExported) path)
                                   (Context originAstPath (Src.getArea $ head allNotExported))
    )


extractExportsFromAST :: Env -> Can.AST -> CanonicalM (M.Map String Type)
extractExportsFromAST env ast = do
  let tds         = filter Can.isTypeDeclExported $ Can.atypedecls ast
      typeExports = filter Can.isTypeExport $ Can.aexps ast
  exportedTds   <- mapM (extractExport env) tds
  typeExportTds <- mapM (extractTypeExport env) typeExports

  return $ M.fromList (exportedTds ++ typeExportTds)


extractTypeExport :: Env -> Can.Exp -> CanonicalM (String, Type)
extractTypeExport env typeExport = do
  let name = Can.getTypeExportName typeExport
  t <- lookupADT env name
  return (name, t)


extractExport :: Env -> Can.TypeDecl -> CanonicalM (String, Type)
extractExport env typeDecl = do
  let name = Can.getTypeDeclName typeDecl
  t <- lookupADT env name
  return (name, t)


getAllImportedNames :: [Can.Import] -> [(String, Area)]
getAllImportedNames imports = concat $ getNamesFromImport <$> imports


getNamesFromImport :: Can.Import -> [(String, Area)]
getNamesFromImport imp = (\n -> (Can.getCanonicalContent n, Can.getArea n)) <$> Can.getImportNames imp


getAllImportedNamespaces :: [Can.Import] -> [(String, Area)]
getAllImportedNamespaces imports =
  (\n -> (Can.getCanonicalContent n, Can.getArea n)) <$> mapMaybe Can.getImportAlias imports


getAllImportedTypes :: [Can.Import] -> [(String, Area)]
getAllImportedTypes imports =
  (\n -> (Can.getCanonicalContent n, Can.getArea n)) <$> concat (Can.getImportTypeNames <$> imports)


checkUnusedImports :: Env -> [Can.Import] -> CanonicalM ()
checkUnusedImports env imports = do
  let importedNames      = getAllImportedNames imports
  let importedNamespaces = getAllImportedNamespaces imports
  let importedTypes      = getAllImportedTypes imports

  namesAccessed <- S.toList <$> getAllNameAccesses
  typesAccessed <- S.toList <$> getAllTypeAccesses

  let unusedNames   = filter (not . (`elem` namesAccessed) . fst) importedNames
  let unusedAliases = filter (not . (`elem` namesAccessed) . fst) importedNamespaces
  let unusedTypes   = filter (not . (`elem` typesAccessed) . fst) importedTypes

  let allUnused     = unusedNames ++ unusedAliases

  withJSCheck <- if null allUnused
    then return allUnused
    else do
      allJS <- getJS
      return $ filter (not . (allJS =~) . fst) allUnused
  mapM_
    (\(name, area) ->
      pushWarning (CompilationWarning (UnusedImport name (envCurrentPath env)) (Context (envCurrentPath env) area))
    )
    (withJSCheck ++ unusedTypes)


findDictionaryFromListName :: FilePath -> [Src.Import] -> String
findDictionaryFromListName dictionaryModulePath imports = case imports of
  ((Src.Source _ _ (Src.NamedImport names _ path)) : next) | path == dictionaryModulePath ->
    if "fromList" `elem` (Src.getSourceContent <$> names) then
      "fromList"
    else
      findDictionaryFromListName dictionaryModulePath next

  ((Src.Source _ _ (Src.DefaultImport (Src.Source _ _ namespace) _ path)) : _) | path == dictionaryModulePath ->
    namespace <> ".fromList"

  (_ : next) ->
    findDictionaryFromListName dictionaryModulePath next

  _ ->
    ""


importInfo :: Src.Import -> [ImportInfo]
importInfo (Src.Source _ _ imp) = case imp of
  Src.NamedImport names _ path ->
    ImportInfo path CanEnv.NameImport . Src.getSourceContent <$> names

  Src.TypeImport typeNames _ path ->
    ImportInfo path CanEnv.TypeImport . Src.getSourceContent <$> typeNames

  Src.DefaultImport name _ path ->
    [ImportInfo path CanEnv.NamespaceImport (Src.getSourceContent name)]


buildImportInfos :: Env -> Src.AST -> Env
buildImportInfos env Src.AST { Src.aimports } =
  let info = concatMap importInfo aimports
  in  env { envImportInfo = info }


canonicalizeAST :: FilePath -> Options -> Env -> Src.AST -> CanonicalM (Can.AST, Env, [InstanceToDerive])
canonicalizeAST dictionaryModulePath options env sourceAst@Src.AST{ Src.apath = Just astPath, Src.aimports } = do
  mapM_ (validateImport astPath) aimports

  let env'  = buildImportInfos env sourceAst
  let env'' = env'
            { envCurrentPath = astPath
            , envFromDictionaryListName = findDictionaryFromListName dictionaryModulePath (Src.aimports sourceAst)
            , envIsMainModule = astPath == optEntrypoint options
            }

  foldM_ (verifyExport env'') [] (Src.aexps sourceAst)

  resetNameAccesses
  resetJS

  (env''', typeDecls)   <- canonicalizeTypeDecls env'' astPath $ Src.atypedecls sourceAst
  -- liftIO $ putStrLn (ppShow env''')
  imports               <- mapM (canonicalize env''' (optTarget options)) $ Src.aimports sourceAst
  exps                  <- mapM (canonicalize env''' (optTarget options)) $ Src.aexps sourceAst
  (env'''', interfaces) <- canonicalizeInterfaces env''' $ Src.ainterfaces sourceAst
  instances             <- canonicalizeInstances env'''' (optTarget options) $ Src.ainstances sourceAst

  when (optMustHaveMain options && astPath == optEntrypoint options) $ do
    if any ((== Just "main") . Can.getExpName) exps then
      return ()
    else
      throwError $ CompilationError NoMain (Context astPath (Area (Loc 1 1 1) (Loc 2 2 2)))

  checkUnusedImports env'' imports


  derivedTypes             <- getDerivedTypes
  typeDeclarationsToDerive <- getTypeDeclarationsToDerive
  let typeDeclarationsToDerive' = removeDuplicates $ typeDeclarationsToDerive \\ S.toList derivedTypes
      derivedEqInstances        =
        if optGenerateDerivedInstances options then
          mapMaybe (deriveEqInstance env'') typeDeclarationsToDerive'
        else
          []
      derivedInspectInstances   =
        if optGenerateDerivedInstances options then
          mapMaybe (deriveInspectInstance env'') typeDeclarationsToDerive'
        else
          []
  
  addDerivedTypes (S.fromList typeDeclarationsToDerive')
  resetToDerive

  let canonicalizedAST = Can.AST { Can.aimports    = imports
                                 , Can.aexps       = exps
                                 , Can.atypedecls  = typeDecls
                                 , Can.ainterfaces = interfaces
                                --  , Can.ainstances  = instances
                                 , Can.ainstances  = derivedEqInstances ++ derivedInspectInstances ++ instances
                                 , Can.apath       = Src.apath sourceAst
                                 }

  -- add `export type TypeName` types to the current env.
  envTds <- extractExportsFromAST env'''' canonicalizedAST

  return (canonicalizedAST, env'''' { envTypeDecls = envTypeDecls env'''' <> envTds }, typeDeclarationsToDerive')

canonicalizeAST _ _ _ _ =
  return (Can.emptyAST, initialEnv, mempty)


performExportCheck :: Env -> Area -> [String] -> String -> CanonicalM [String]
performExportCheck env area exportedNames name = do
  if name `elem` exportedNames
    then throwError $ CompilationError (NameAlreadyExported name) (Context (envCurrentPath env) area)
    else return $ name : exportedNames

verifyExport :: Env -> [String] -> Src.Exp -> CanonicalM [String]
verifyExport env exportedNames (Src.Source area _ exp) = case exp of
  Src.NameExport name ->
    performExportCheck env area exportedNames name

  Src.Export (Src.Source _ _ (Src.Assignment name _)) ->
    performExportCheck env area exportedNames name

  _ ->
    return exportedNames


findASTM :: Can.Table -> FilePath -> CanonicalM Can.AST
findASTM table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> throwError $ CompilationError (ImportNotFound path) NoContext

findAST :: Can.Table -> FilePath -> Either CompilationError Can.AST
findAST table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> Left $ CompilationError (ImportNotFound path) NoContext
