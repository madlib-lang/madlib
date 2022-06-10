{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Canonicalize.AST where


import           Run.Target
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Canonicalize.Env              as CanEnv
import           Canonicalize.Canonicalize
import qualified Parse.Madlib.AST              as P
import           Canonicalize.CanonicalM
import           Canonicalize.ADT
import           Canonicalize.Interface
import           Canonicalize.EnvUtils
import           Infer.Type
import           Error.Error
import           Error.Warning
import           Error.Context
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Utils.Tuple                   as T
import           Control.Monad.Except
import           Control.Monad.State
import           Explain.Location
import           Text.Regex.TDFA
import AST.Solved (Import_(NamedImport))
import Canonicalize.Derive
import Data.List
import Utils.List
import AST.Canonical (Import_(TypeImport))
-- import qualified Driver.Rules as Rules


type TableCache = M.Map FilePath (Env, Can.AST)

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

-- canonicalizeImportedAST
--   :: TableCache -> FilePath -> Target -> FilePath -> Src.Import -> CanonicalM (Can.Table, Env, TableCache)
-- canonicalizeImportedAST dictionaryModulePath target originAstPath imp = do
--   let path = Src.getImportAbsolutePath imp

--   (table', env) <- canonicalizeAST dictionaryModulePath target initialEnv path
--   ast                  <- findASTM table' path

--   let allExportNames   = findAllExportedNames ast
--   let allImportNames   = Src.getImportNames imp
--   let namesNotExported = filter (not . (`elem` allExportNames) . Src.getSourceContent) allImportNames

--   let allExportTypes   = findAllExportedTypeNames ast
--   let allImportTypes   = Src.getImportTypeNames imp
--   let typesNotExported = filter (not . (`elem` allExportTypes) . Src.getSourceContent) allImportTypes

--   let allNotExported   = namesNotExported ++ typesNotExported

--   unless
--     (null allNotExported)
--     (throwError $ CompilationError (NotExported (Src.getSourceContent $ head allNotExported) path)
--                                    (Context originAstPath (Src.getArea $ head allNotExported) [])
--     )

--   envTds <- mapImportToEnvTypeDecls env imp ast

--   let interfaces = case imp of
--         Src.Source _ _ Src.TypeImport{} -> mempty
--         _                             -> envInterfaces env

--   let env' = (initialWithPath path) { envTypeDecls = envTds, envInterfaces = interfaces }
--   return (table', env', tableCache <> cache)

mapImportToEnvTypeDecls :: Env -> Src.Import -> Can.AST -> CanonicalM (M.Map String Type)
mapImportToEnvTypeDecls env imp ast = do
  envTds <- extractExportsFromAST env ast
  return $ fromExportToImport imp envTds

fromExportToImport :: Src.Import -> M.Map String Type -> M.Map String Type
fromExportToImport imp exports = case imp of
  Src.Source _ _ (Src.TypeImport    names _ _) ->
    M.restrictKeys exports $ S.fromList (Src.getSourceContent <$> names)

  Src.Source _ _ (Src.NamedImport   names _ _) ->
    mempty

  Src.Source _ _ (Src.DefaultImport name  _ _) ->
    M.mapKeys ((Src.getSourceContent name ++ ".") ++) exports

  Src.Source _ _ (Src.ImportAll _ _) ->
    mempty

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

-- processImports
--   :: FilePath -> Target -> FilePath -> [Src.Import] -> CanonicalM (Can.Table, Env, TableCache)
-- processImports dictionaryModulePath target astPath imports = do
--   foldM
--     (\(table', env', tableCache') imp -> do
--       (table'', env'', tableCache'') <- canonicalizeImportedAST tableCache' dictionaryModulePath target astPath imp
--       return
--         ( table' <> table''
--         , env' { envTypeDecls  = envTypeDecls env' <> envTypeDecls env''
--                , envInterfaces = envInterfaces env' <> envInterfaces env''
--                }
--         , tableCache' <> tableCache''
--         )
--     )
--     (mempty, initialEnv, tableCache)
--     imports


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
      pushWarning (CompilationWarning (UnusedImport name (envCurrentPath env)) (Context (envCurrentPath env) area []))
    )
    (withJSCheck ++ unusedTypes)


findDictionaryFromListName :: FilePath -> [Src.Import] -> String
findDictionaryFromListName dictionaryModulePath imports = case imports of
  ((Src.Source area target (Src.NamedImport names _ path)) : next) | path == dictionaryModulePath ->
    if "fromList" `elem` (Src.getSourceContent <$> names) then
      "fromList"
    else
      findDictionaryFromListName dictionaryModulePath next

  (imp@(Src.Source _ _ (Src.ImportAll _ path)) : next) | path == dictionaryModulePath ->
    "fromList"

  ((Src.Source _ _ (Src.DefaultImport (Src.Source _ _ namespace) _ path)) : next) | path == dictionaryModulePath ->
    namespace <> ".fromList"

  (imp : next) ->
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

  -- TODO: handle correctly or remove import all
  Src.ImportAll _ _ ->
    []

buildImportInfos :: Env -> Src.AST -> Env
buildImportInfos env Src.AST { Src.aimports } =
  let info = concatMap importInfo aimports
  in  env { envImportInfo = info }


canonicalizeAST :: FilePath -> Target -> Env -> Src.AST -> CanonicalM (Can.AST, Env)
canonicalizeAST dictionaryModulePath target env sourceAst@Src.AST{ Src.apath = Just astPath } = do
  -- ast <- liftIO $ Rules.parse target "/Users/a.boeglin/Code/madlib" astPath
  -- (table, env', nextCache) <- processImports dictionaryModulePath target astPath $ Src.aimports sourceAst

  let env'  = buildImportInfos env sourceAst
  let env'' = env' { envCurrentPath = astPath, envFromDictionaryListName = findDictionaryFromListName dictionaryModulePath (Src.aimports sourceAst) }

  foldM_ (verifyExport env'') [] (Src.aexps sourceAst)

  resetNameAccesses
  resetJS

  (env''', typeDecls)   <- canonicalizeTypeDecls env'' astPath $ Src.atypedecls sourceAst
  imports               <- mapM (canonicalize env''' target) $ Src.aimports sourceAst
  exps                  <- mapM (canonicalize env''' target) $ Src.aexps sourceAst
  (env'''', interfaces) <- canonicalizeInterfaces env''' $ Src.ainterfaces sourceAst
  instances             <- canonicalizeInstances env'''' target $ Src.ainstances sourceAst


  checkUnusedImports env'' imports


  derivedTypes             <- getDerivedTypes
  typeDeclarationsToDerive <- getTypeDeclarationsToDerive
  let typeDeclarationsToDerive' = removeDuplicates $ typeDeclarationsToDerive \\ S.toList derivedTypes
      derivedEqInstances        = mapMaybe deriveEqInstance typeDeclarationsToDerive'
      derivedInspectInstances   = mapMaybe deriveInspectInstance typeDeclarationsToDerive'
  
  addDerivedTypes (S.fromList typeDeclarationsToDerive')
  resetToDerive

  let canonicalizedAST = Can.AST { Can.aimports    = imports
                                  , Can.aexps       = exps
                                  , Can.atypedecls  = typeDecls
                                  , Can.ainterfaces = interfaces
                                  , Can.ainstances  = derivedInspectInstances ++ derivedEqInstances ++ instances
                                  , Can.apath       = Src.apath sourceAst
                                  }

  return (canonicalizedAST, env'''')

canonicalizeAST _ _ _ _ = undefined
  -- return (M.insert astPath canonicalizedAST table, env'''', M.insert astPath (env'''', canonicalizedAST) nextCache)


-- canonicalizeAST :: TableCache -> FilePath -> Target -> Env -> FilePath -> CanonicalM (Can.Table, Env, TableCache)
-- canonicalizeAST tableCache dictionaryModulePath target env astPath = case M.lookup astPath tableCache of
--   Just (env', ast') ->
--     return (snd <$> tableCache, env', tableCache)

--   Nothing           -> do
--     ast <- liftIO $ Rules.parse target "/Users/a.boeglin/Code/madlib" astPath
--     (table, env', nextCache) <- processImports tableCache dictionaryModulePath target astPath $ Src.aimports ast

--     let env'' = env' { envCurrentPath = astPath, envFromDictionaryListName = findDictionaryFromListName dictionaryModulePath (Src.aimports ast) }

--     foldM_ (verifyExport env'') [] (Src.aexps ast)

--     resetNameAccesses
--     resetJS

--     (env''', typeDecls)   <- canonicalizeTypeDecls env'' astPath $ Src.atypedecls ast
--     imports               <- mapM (canonicalize env''' target) $ Src.aimports ast
--     exps                  <- mapM (canonicalize env''' target) $ Src.aexps ast
--     (env'''', interfaces) <- canonicalizeInterfaces env''' $ Src.ainterfaces ast
--     instances             <- canonicalizeInstances env'''' target $ Src.ainstances ast


--     checkUnusedImports env'' imports


--     derivedTypes             <- getDerivedTypes
--     typeDeclarationsToDerive <- getTypeDeclarationsToDerive
--     let typeDeclarationsToDerive' = removeDuplicates $ typeDeclarationsToDerive \\ S.toList derivedTypes
--         derivedEqInstances        = mapMaybe deriveEqInstance typeDeclarationsToDerive'
--         derivedInspectInstances   = mapMaybe deriveInspectInstance typeDeclarationsToDerive'
    
--     addDerivedTypes (S.fromList typeDeclarationsToDerive')
--     resetToDerive

--     let canonicalizedAST = Can.AST { Can.aimports    = imports
--                                    , Can.aexps       = exps
--                                    , Can.atypedecls  = typeDecls
--                                    , Can.ainterfaces = interfaces
--                                    , Can.ainstances  = derivedInspectInstances ++ derivedEqInstances ++ instances
--                                    , Can.apath       = Src.apath ast
--                                    }

--     return (M.insert astPath canonicalizedAST table, env'''', M.insert astPath (env'''', canonicalizedAST) nextCache)


performExportCheck :: Env -> Area -> [String] -> String -> CanonicalM [String]
performExportCheck env area exportedNames name = do
  if name `elem` exportedNames
    then throwError $ CompilationError (NameAlreadyExported name) (Context (envCurrentPath env) area [])
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

-- runCanonicalization
--   :: FilePath
--   -> Target
--   -> Env
--   -> Src.AST
--   -> IO (Either CompilationError (Can.AST, Env), [CompilationWarning])
-- runCanonicalization dictionaryModulePath target env ast = do
--   (canonicalized, s) <- runStateT (runExceptT (canonicalizeAST dictionaryModulePath target env ast))
--                                     (CanonicalState { warnings = [], namesAccessed = S.empty, accumulatedJS = "", typesToDerive = [], derivedTypes = S.empty, placeholderIndex = 0 })
--   return (canonicalized, warnings s)


-- canonicalizeMany
--   :: FilePath -> Target -> Env -> [FilePath] -> IO (Either CompilationError [Can.AST], [CompilationWarning])
-- canonicalizeMany = canonicalizeMany'

-- canonicalizeMany'
--   :: FilePath -> Target -> Env -> [FilePath] -> IO (Either CompilationError [Can.AST], [CompilationWarning])
-- canonicalizeMany' dictionaryModulePath target env fps = case fps of
--   [] ->
--     return (Right mempty, [])

--   fp : fps' -> do
--     result <- runCanonicalization dictionaryModulePath target env fp
--     case result of
--       (curr@(Right _), warnings) -> do
--         (next, nextWarnings) <- canonicalizeMany' dictionaryModulePath target env fps'
--         return (liftM2 (\(table') table'' -> table' <> table'') curr next, warnings ++ nextWarnings)

--       (Left e, ws) ->
--         return (Left e, ws)