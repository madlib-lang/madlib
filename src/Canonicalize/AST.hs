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
import           Data.List
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.Except
import           Explain.Location
import           Explain.Meta


findAllExportedNames :: Can.AST -> [Can.Name]
findAllExportedNames ast =
  let exportedADTs    = filter Can.isTypeDeclExported (Can.atypedecls ast)
      ctors           = concat $ Can.getCtors <$> exportedADTs
      typeExportNames = Can.getTypeExportName <$> filter Can.isTypeExport (Can.aexps ast)
      typeNames       = Can.getTypeDeclName <$> exportedADTs
      ctorNames       = Can.getCtorName <$> ctors
      varNames        = mapMaybe Can.getExportName (Can.aexps ast)
  in  typeExportNames ++ typeNames ++ ctorNames ++ varNames

canonicalizeImportedAST :: Target -> FilePath -> Src.Table -> Src.Import -> CanonicalM (Can.Table, Env)
canonicalizeImportedAST target originAstPath table imp = do
  let path = Src.getImportAbsolutePath imp
  (table', env) <- canonicalizeAST target initialEnv table path
  ast           <- findASTM table' path

  let allExportNames = findAllExportedNames ast
  let allImportNames = Src.getImportNames imp
  let notExported    = filter (not . (`elem` allExportNames) . Src.getSourceContent) allImportNames--allImportNames \\ allExportNames

  unless (null notExported)
         (throwError $ InferError (NotExported (Src.getSourceContent $ head notExported) path) (Context originAstPath (Src.getArea $ head notExported) []))

  envTds <- mapImportToEnvTypeDecls env imp ast

  let env' = (initialWithPath path) { envTypeDecls = envTds, envInterfaces = envInterfaces env }
  return (table', env')

mapImportToEnvTypeDecls :: Env -> Src.Import -> Can.AST -> CanonicalM (M.Map String Type)
mapImportToEnvTypeDecls env imp ast = do
  envTds <- extractExportsFromAST env ast
  return $ fromExportToImport imp envTds

fromExportToImport :: Src.Import -> M.Map String Type -> M.Map String Type
fromExportToImport imp exports = case imp of
  Src.Source _ _ (Src.NamedImport   names _ _) -> M.restrictKeys exports $ S.fromList (Src.getSourceContent <$> names)

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

canonicalizeAST :: Target -> Env -> Src.Table -> FilePath -> CanonicalM (Can.Table, Env)
canonicalizeAST target env table astPath = do
  ast <- case P.findAST table astPath of
    Right ast -> return ast
    Left  e   -> throwError $ InferError (ImportNotFound astPath) NoContext

  (table, env') <- processImports target astPath table $ Src.aimports ast
  let env'' = env' { envCurrentPath = astPath }

  foldM (verifyExport env'') [] (Src.aexps ast)

  (env''', typeDecls)   <- canonicalizeTypeDecls env'' astPath $ Src.atypedecls ast
  imports               <- mapM (canonicalize env''' target) $ Src.aimports ast
  exps                  <- mapM (canonicalize env''' target) $ Src.aexps ast
  (env'''', interfaces) <- canonicalizeInterfaces env''' $ Src.ainterfaces ast
  instances             <- canonicalizeInstances env'''' target $ Src.ainstances ast

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
    then throwError $ InferError (NameAlreadyExported name) (Context (envCurrentPath env) area [])
    else return $ name : exportedNames

verifyExport :: Env -> [String] -> Src.Exp -> CanonicalM [String]
verifyExport env exportedNames (Src.Source _ area exp) = case exp of
  Src.NameExport name -> performExportCheck env area exportedNames name

  Src.Export (Src.Source _ _ (Src.Assignment name _)) -> performExportCheck env area exportedNames name

  _                   -> return exportedNames


findASTM :: Can.Table -> FilePath -> CanonicalM Can.AST
findASTM table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> throwError $ InferError (ImportNotFound path) NoContext

findAST :: Can.Table -> FilePath -> Either InferError Can.AST
findAST table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> Left $ InferError (ImportNotFound path) NoContext

runCanonicalization :: Target -> Env -> Src.Table -> FilePath -> Either InferError Can.Table
runCanonicalization target env table entrypoint = runExcept $ fst <$> canonicalizeAST target env table entrypoint

canonicalizeMany :: Target -> Env -> Src.Table -> [FilePath] -> Either InferError Can.Table
canonicalizeMany target env table fps = case fps of
  []        -> return mempty
  fp : fps' -> do
    current <- runCanonicalization target env table fp
    next    <- canonicalizeMany target env table fps'
    return $ current <> next
