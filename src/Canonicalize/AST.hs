{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.AST where


import           Target
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Canonicalize.Env
import           Canonicalize.Canonicalize
import qualified Parse.Madlib.AST              as P
import           Canonicalize.CanonicalM
import           Canonicalize.ADT
import           Canonicalize.Interface
import           Infer.Type
import           Error.Error
import           Error.Warning
import           Error.Context
import           Data.List
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.Except
import           Control.Monad.State
import           Explain.Location
import           Explain.Meta
import           Text.Regex.TDFA


findAllExportedNames :: Can.AST -> [Can.Name]
findAllExportedNames ast =
  let exportedADTs    = filter Can.isTypeDeclExported (Can.atypedecls ast)
      ctors           = concat $ Can.getCtors <$> exportedADTs
      typeExportNames = Can.getTypeExportName <$> filter Can.isTypeExport (Can.aexps ast)
      typeNames       = Can.getTypeDeclName <$> exportedADTs
      ctorNames       = Can.getCtorName <$> ctors
      varNames        = mapMaybe Can.getExportName (Can.aexps ast)
  in  typeExportNames ++ typeNames ++ ctorNames ++ varNames

findAllExportedTypeNames :: Can.AST -> [Can.Name]
findAllExportedTypeNames ast =
  let exportedADTs    = filter Can.isTypeDeclExported (Can.atypedecls ast)
      typeExportNames = Can.getTypeExportName <$> filter Can.isTypeExport (Can.aexps ast)
      typeNames       = Can.getTypeDeclName <$> exportedADTs
  in  typeExportNames ++ typeNames

canonicalizeImportedAST :: Target -> FilePath -> Src.Table -> Src.Import -> CanonicalM (Can.Table, Env)
canonicalizeImportedAST target originAstPath table imp = do
  let path = Src.getImportAbsolutePath imp
  (table', env) <- canonicalizeAST target initialEnv table path
  ast           <- findASTM table' path

  let allExportNames = findAllExportedNames ast
  let allImportNames = Src.getImportNames imp
  let namesNotExported = filter (not . (`elem` allExportNames) . Src.getSourceContent) allImportNames

  let allExportTypes = findAllExportedTypeNames ast
  let allImportTypes = Src.getImportTypeNames imp
  let typesNotExported = filter (not . (`elem` allExportNames) . Src.getSourceContent) allImportTypes

  let allNotExported = namesNotExported ++ typesNotExported

  unless
    (null allNotExported)
    (throwError $ CompilationError (NotExported (Src.getSourceContent $ head allNotExported) path)
                                   (Context originAstPath (Src.getArea $ head allNotExported) [])
    )

  envTds <- mapImportToEnvTypeDecls env imp ast

  let interfaces = case imp of
        Src.Source _ _ Src.TypeImport{} -> mempty
        _                               -> envInterfaces env

  let env' = (initialWithPath path) { envTypeDecls = envTds, envInterfaces = interfaces }
  return (table', env')

mapImportToEnvTypeDecls :: Env -> Src.Import -> Can.AST -> CanonicalM (M.Map String Type)
mapImportToEnvTypeDecls env imp ast = do
  envTds <- extractExportsFromAST env ast
  return $ fromExportToImport imp envTds

fromExportToImport :: Src.Import -> M.Map String Type -> M.Map String Type
fromExportToImport imp exports = case imp of
  Src.Source _ _ (Src.TypeImport names _ _)    -> M.restrictKeys exports $ S.fromList (Src.getSourceContent <$> names)

  Src.Source _ _ (Src.NamedImport   names _ _) -> mempty --M.restrictKeys exports $ S.fromList (Src.getSourceContent <$> names)

  Src.Source _ _ (Src.DefaultImport name  _ _) -> M.mapKeys ((Src.getSourceContent name ++ ".") ++) exports

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
getAllImportedNamespaces imports = (\n -> (Can.getCanonicalContent n, Can.getArea n)) <$> mapMaybe Can.getImportAlias imports

getAllImportedTypes :: [Can.Import] -> [(String, Area)]
getAllImportedTypes imports = (\n -> (Can.getCanonicalContent n, Can.getArea n)) <$> concat (Can.getImportTypeNames <$> imports)

processImports :: Target -> FilePath -> Src.Table -> [Src.Import] -> CanonicalM (Can.Table, Env)
processImports target astPath table imports = do
  imports' <- mapM (canonicalizeImportedAST target astPath table) imports
  let table' = foldl' (<>) mempty (fst <$> imports')
  let env' = foldl'
        (\env env' -> env { envTypeDecls  = envTypeDecls env <> envTypeDecls env'
                          , envInterfaces = envInterfaces env <> envInterfaces env'
                          }
        )
        initialEnv
        (snd <$> imports')
  return (table', env')


checkUnusedImports :: Env -> [Can.Import] -> CanonicalM ()
checkUnusedImports env imports = do
  let importedNames = getAllImportedNames imports
  let importedNamespaces = getAllImportedNamespaces imports
  let importedTypes = getAllImportedTypes imports

  namesAccessed <- S.toList <$> getAllNameAccesses
  typesAccessed <- S.toList <$> getAllTypeAccesses

  let unusedNames = filter (not . (`elem` namesAccessed) . fst) importedNames
  let unusedAliases = filter (not . (`elem` namesAccessed) . fst) importedNamespaces
  let unusedTypes = filter (not . (`elem` typesAccessed) . fst) importedTypes

  let allUnused = unusedNames ++ unusedAliases

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


canonicalizeAST :: Target -> Env -> Src.Table -> FilePath -> CanonicalM (Can.Table, Env)
canonicalizeAST target env table astPath = do
  ast <- case P.findAST table astPath of
    Right ast -> return ast
    Left  e   -> throwError $ CompilationError (ImportNotFound astPath) NoContext

  (table, env') <- processImports target astPath table $ Src.aimports ast
  let env'' = env' { envCurrentPath = astPath }

  foldM_ (verifyExport env'') [] (Src.aexps ast)

  resetNameAccesses
  resetJS

  (env''', typeDecls)   <- canonicalizeTypeDecls env'' astPath $ Src.atypedecls ast
  imports               <- mapM (canonicalize env''' target) $ Src.aimports ast
  exps                  <- mapM (canonicalize env''' target) $ Src.aexps ast
  (env'''', interfaces) <- canonicalizeInterfaces env''' $ Src.ainterfaces ast
  instances             <- canonicalizeInstances env'''' target $ Src.ainstances ast


  checkUnusedImports env'' imports

  let canonicalizedAST = Can.AST { Can.aimports    = imports
                                 , Can.aexps       = exps
                                 , Can.atypedecls  = typeDecls
                                 , Can.ainterfaces = interfaces
                                 , Can.ainstances  = instances
                                 , Can.apath       = Src.apath ast
                                 }

  return (M.insert astPath canonicalizedAST table, env'''')


performExportCheck :: Env -> Area -> [String] -> String -> CanonicalM [String]
performExportCheck env area exportedNames name = do
  if name `elem` exportedNames
    then throwError $ CompilationError (NameAlreadyExported name) (Context (envCurrentPath env) area [])
    else return $ name : exportedNames

verifyExport :: Env -> [String] -> Src.Exp -> CanonicalM [String]
verifyExport env exportedNames (Src.Source _ area exp) = case exp of
  Src.NameExport name -> performExportCheck env area exportedNames name

  Src.Export (Src.Source _ _ (Src.Assignment name _)) -> performExportCheck env area exportedNames name

  _                   -> return exportedNames


findASTM :: Can.Table -> FilePath -> CanonicalM Can.AST
findASTM table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> throwError $ CompilationError (ImportNotFound path) NoContext

findAST :: Can.Table -> FilePath -> Either CompilationError Can.AST
findAST table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> Left $ CompilationError (ImportNotFound path) NoContext

runCanonicalization
  :: Target -> Env -> Src.Table -> FilePath -> (Either CompilationError Can.Table, [CompilationWarning])
runCanonicalization target env table entrypoint = do
  let (canonicalized, s) = runState (runExceptT (canonicalizeAST target env table entrypoint))
                                    (CanonicalState { warnings = [], namesAccessed = S.empty, accumulatedJS = "" })
  (fst <$> canonicalized, warnings s)

canonicalizeMany
  :: Target -> Env -> Src.Table -> [FilePath] -> (Either CompilationError Can.Table, [CompilationWarning])
canonicalizeMany target env table fps = case fps of
  [] -> (Right mempty, [])
  fp : fps' ->
    let (canTable , warnings    ) = runCanonicalization target env table fp
        (nextTable, nextWarnings) = canonicalizeMany target env table fps'
    in  (liftM2 (<>) canTable nextTable, warnings ++ nextWarnings)
