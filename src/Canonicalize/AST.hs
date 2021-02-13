{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.AST where


import           Target
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Canonicalize.Env
import           Canonicalize.Canonicalize
import qualified Parse.AST                     as P
import           Canonicalize.CanonicalM
import           Canonicalize.ADT
import           Canonicalize.Interface
import           Infer.Type
import           Error.Error
import           Explain.Reason
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.Except


canonicalizeImportedAST
  :: Target -> Src.Table -> Src.Import -> CanonicalM (Can.Table, Env)
canonicalizeImportedAST target table imp = do
  let path = Src.getImportAbsolutePath imp
  (table', env) <- canonicalizeAST target initialEnv table path
  ast           <- findASTM table' path
  envTds        <- mapImportToEnvTypeDecls env imp ast

  let env' = (initialWithPath path) { envTypeDecls  = envTds
                                    , envInterfaces = envInterfaces env
                                    }
  return (table', env')

mapImportToEnvTypeDecls
  :: Env -> Src.Import -> Can.AST -> CanonicalM (M.Map String Type)
mapImportToEnvTypeDecls env imp ast = do
  envTds <- extractExportsFromAST env ast
  return $ fromExportToImport imp envTds

fromExportToImport :: Src.Import -> M.Map String Type -> M.Map String Type
fromExportToImport imp exports = case imp of
  Src.Source _ _ (Src.NamedImport names _ _) ->
    M.restrictKeys exports $ S.fromList names

  Src.Source _ _ (Src.DefaultImport name _ _) ->
    M.mapKeys ((name ++ ".") ++) exports

extractExportsFromAST :: Env -> Can.AST -> CanonicalM (M.Map String Type)
extractExportsFromAST env ast =
  let tds = filter Can.isTypeDeclExported $ Can.atypedecls ast
  in  M.fromList <$> mapM (extractExport env) tds

extractExport :: Env -> Can.TypeDecl -> CanonicalM (String, Type)
extractExport env typeDecl = do
  let name = Can.getTypeDeclName typeDecl
  t <- lookupADT env name
  return (name, t)

processImports
  :: Target -> Src.Table -> [Src.Import] -> CanonicalM (Can.Table, Env)
processImports target table imports = do
  imports' <- mapM (canonicalizeImportedAST target table) imports
  let table' = foldl (<>) mempty (fst <$> imports')
  let env' = foldl
        (\env env' -> env
          { envTypeDecls  = envTypeDecls env <> envTypeDecls env'
          , envInterfaces = envInterfaces env <> envInterfaces env'
          }
        )
        initialEnv
        (snd <$> imports')
  return (table', env')

canonicalizeAST
  :: Target -> Env -> Src.Table -> FilePath -> CanonicalM (Can.Table, Env)
canonicalizeAST target env table astPath = do
  ast <- case P.findAST table astPath of
    Right ast -> return ast
    Left  e   -> throwError $ InferError (ImportNotFound astPath) NoReason

  (table, env') <- processImports target table $ Src.aimports ast

  (env'', typeDecls) <- canonicalizeTypeDecls env' astPath $ Src.atypedecls ast
  imports <- mapM (canonicalize env'' target) $ Src.aimports ast
  exps <- mapM (canonicalize env'' target) $ Src.aexps ast
  (env''', interfaces) <- canonicalizeInterfaces env'' $ Src.ainterfaces ast
  instances <- canonicalizeInstances env''' target $ Src.ainstances ast

  let canonicalizedAST = Can.AST { Can.aimports    = imports
                                 , Can.aexps       = exps
                                 , Can.atypedecls  = typeDecls
                                 , Can.ainterfaces = interfaces
                                 , Can.ainstances  = instances
                                 , Can.apath       = Src.apath ast
                                 }

  return (M.insert astPath canonicalizedAST table, env''')


findASTM :: Can.Table -> FilePath -> CanonicalM Can.AST
findASTM table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> throwError $ InferError (ImportNotFound path) NoReason

findAST :: Can.Table -> FilePath -> Either InferError Can.AST
findAST table path = case M.lookup path table of
  Just found -> return found
  Nothing    -> Left $ InferError (ImportNotFound path) NoReason

runCanonicalization
  :: Target -> Env -> Src.Table -> FilePath -> Either InferError Can.Table
runCanonicalization target env table entrypoint =
  runExcept $ fst <$> canonicalizeAST target env table entrypoint
