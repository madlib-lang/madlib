{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Infer.AST where

import qualified AST.Solved                    as Slv
import qualified AST.Canonical                 as Can
import qualified Canonicalize.AST              as Can
import           Infer.Infer
import           Infer.Env
import           Infer.Typing
import           Infer.Interface
import           Infer.Type
import           Infer.Exp
import           Error.Error
import           Explain.Reason
import           Data.Maybe
import           Data.List
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S

{-|
Module      : AST
Description : Type checking for AST and Table data types
License     : BSD-3

High level type checking. The main function is solveTable, which takes a
Canonical AST table and returns a Solved AST table. To do this it also
requires an AST which is the AST of the entrypoint, where the whole compilation
actually starts.
-}


populateTopLevelTypings :: Env -> [Can.Exp] -> Infer Env
populateTopLevelTypings env []                         = return env
populateTopLevelTypings env ((Can.Canonical _ e) : es) = do
  nextEnv <- case e of
    Can.TypedExp (Can.Canonical _ (Can.Assignment name _)) sc ->
      return $ extendVars env (name, sc)

    Can.TypedExp (Can.Canonical _ (Can.Export (Can.Canonical _ (Can.Assignment name _)))) sc
      -> return $ extendVars env (name, sc)

    _ -> return env

  populateTopLevelTypings nextEnv es


buildInitialEnv :: Env -> Can.AST -> Infer Env
buildInitialEnv priorEnv Can.AST { Can.aexps, Can.atypedecls, Can.ainterfaces, Can.ainstances, Can.apath = Just apath }
  = do
    let methods = foldl (\mtds (Can.Interface _ _ _ mtds') -> mtds <> mtds')
                        mempty
                        ainterfaces
    env' <- foldM
      (\env (Can.Interface id preds vars _) -> addInterface env id vars preds)
      priorEnv
      ainterfaces
    env'' <- foldM
      (\env (Can.Instance id preds p _) -> addInstance env preds p)
      env'
      ainstances
    let constructors = M.fromList $ concat $ mapMaybe
          (\case
            adt@Can.ADT{} ->
              Just
                $   (\ctor -> (Can.getCtorName ctor, Can.getCtorScheme ctor))
                <$> Can.adtconstructors adt
            _ -> Nothing
          )
          atypedecls

    let env = env'' { envvars        = constructors <> envvars env''
                    , envmethods     = methods <> envmethods priorEnv
                    , envcurrentpath = apath
                    }

    populateTopLevelTypings env aexps



extractExportedExps :: Slv.AST -> M.Map Slv.Name Slv.Exp
extractExportedExps Slv.AST { Slv.aexps, Slv.apath } = case apath of
  Just p -> M.fromList $ bundleExports <$> filter Slv.isExport aexps

 where
  bundleExports :: Slv.Exp -> (Slv.Name, Slv.Exp)
  bundleExports e'@(Slv.Solved _ _ exp) = case exp of
    Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)) -> (n, e')

    Slv.TypedExp (Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)))) _
      -> (n, e')


findASTM :: Slv.Table -> FilePath -> Infer Slv.AST
findASTM table path = case M.lookup path table of
  Just a  -> return a
  Nothing -> throwError $ InferError (ImportNotFound path) NoReason


extractImportedConstructors :: Env -> Slv.AST -> Can.Import -> Vars
extractImportedConstructors env ast imp =
  let exportedADTs      = filter Slv.isADTExported (Slv.atypedecls ast)
      exportedCtors     = concat $ Slv.adtconstructors <$> exportedADTs
      exportedCtorNames = Slv.getConstructorName <$> exportedCtors
      exportedCtorSchemes =
          M.restrictKeys (envvars env) $ S.fromList exportedCtorNames
  in  filterExportsByImport imp exportedCtorSchemes


extractImportedVars :: Env -> Slv.AST -> Can.Import -> Infer Vars
extractImportedVars env ast imp = do
  let exportedNames = M.keys $ extractExportedExps ast
  exportTuples <- mapM (\name -> (name, ) <$> lookupVar env name) exportedNames
  let exports = M.fromList exportTuples
  return $ filterExportsByImport imp exports


filterExportsByImport :: Can.Import -> Vars -> Vars
filterExportsByImport imp vars = case imp of
  Can.Canonical _ (Can.DefaultImport namespace _ _) ->
    M.mapKeys ((namespace <> ".") <>) vars

  Can.Canonical _ (Can.NamedImport names _ _) ->
    M.restrictKeys vars $ S.fromList names


solveImports
  :: Can.Table -> [Can.Import] -> Infer (Slv.Table, Vars, Interfaces, Methods)
solveImports table (imp : is) = do
  let modulePath = Can.getImportAbsolutePath imp

  (solvedAST, solvedTable, solvedEnv) <- case Can.findAST table modulePath of
    Right ast -> do
      (solvedTable, env) <- solveTable' table ast
      solvedAST          <- findASTM solvedTable modulePath
      return (solvedAST, solvedTable, env)

  importedVars <- extractImportedVars solvedEnv solvedAST imp
  let constructorImports = extractImportedConstructors solvedEnv solvedAST imp

  (nextTable, nextVars, nextInterfaces, nextMethods) <- solveImports table is
  let allVars = nextVars <> constructorImports <> importedVars

  return
    ( M.insert modulePath solvedAST (M.union solvedTable nextTable)
    , allVars
    , mergeInterfaces (envinterfaces solvedEnv) nextInterfaces
    , M.union (envmethods solvedEnv) nextMethods
    )

solveImports _ [] = return
  (M.empty, envvars initialEnv, envinterfaces initialEnv, envmethods initialEnv)

mergeInterfaces :: Interfaces -> Interfaces -> Interfaces
mergeInterfaces = M.foldrWithKey mergeInterface

mergeInterface :: Id -> Interface -> Interfaces -> Interfaces
mergeInterface = M.insertWith
  (\(Interface tvs ps is) (Interface _ _ is') ->
    Interface tvs ps $ is `union` is'
  )

updateInterface :: Can.Interface -> Slv.Interface
updateInterface (Can.Interface name preds vars methods) =
  Slv.Interface name preds vars methods

inferAST :: Env -> Can.AST -> Infer (Slv.AST, Env)
inferAST env Can.AST { Can.aexps, Can.apath, Can.aimports, Can.atypedecls, Can.ainstances, Can.ainterfaces }
  = do
    (inferredExps, env') <- inferExps env aexps
    inferredInstances    <- mapM (resolveInstance env') ainstances
    let updatedInterfaces = updateInterface <$> ainterfaces

    return
      ( Slv.AST
        { Slv.aexps       = inferredExps
        , Slv.apath       = apath
        , Slv.atypedecls  = updateADT <$> atypedecls
        , Slv.aimports    =
          (\case
            g@Slv.DefaultImport{}            -> g
            i@(Slv.NamedImport names fp afp) -> Slv.NamedImport
              (mapMaybe (\n -> M.lookup n (envvars env) >> Just n) names)
              fp
              afp
          )
          .   updateImport
          <$> aimports
        , Slv.ainterfaces = updatedInterfaces
        , Slv.ainstances  = inferredInstances
        }
      , env'
      )

updateADT :: Can.TypeDecl -> Slv.TypeDecl
updateADT adt@Can.ADT{} = Slv.ADT
  { Slv.adtname         = Can.adtname adt
  , Slv.adtparams       = Can.adtparams adt
  , Slv.adtconstructors = updateADTConstructor <$> Can.adtconstructors adt
  , Slv.adtexported     = Can.adtexported adt
  }
updateADT alias@Can.Alias{} = Slv.Alias
  { Slv.aliasname     = Can.aliasname alias
  , Slv.aliasparams   = Can.aliasparams alias
  , Slv.aliastype     = updateTyping $ Can.aliastype alias
  , Slv.aliasexported = Can.aliasexported alias
  }

updateADTConstructor :: Can.Constructor -> Slv.Constructor
updateADTConstructor (Can.Constructor cname cparams _) =
  Slv.Constructor cname $ updateTyping <$> cparams

updateImport :: Can.Import -> Slv.Import
updateImport i = case i of
  Can.Canonical _ (Can.NamedImport   ns p fp) -> Slv.NamedImport ns p fp

  Can.Canonical _ (Can.DefaultImport n  p fp) -> Slv.DefaultImport n p fp


-- |The 'solveTable' function is the main function of this module.
-- It runs type checking for a given canonical ast table and an entrypoint ast
-- by following imports and returning a solved table. To do this, it starts
-- by resolving the asts of the imports of the entrypoint AST, which will
-- then themselves recursively call solveTable until all imports are type
-- checked. It then updates the environment with definitions for imported
-- symbols and type checks the given AST, putting it back in the table
-- and returning it.
solveTable :: Can.Table -> Can.AST -> Infer Slv.Table
solveTable table ast = do
  (table, _) <- solveTable' table ast
  return table

solveTable' :: Can.Table -> Can.AST -> Infer (Slv.Table, Env)
solveTable' table ast@Can.AST { Can.aimports } = do
  -- First we resolve imports to update the env
  (inferredASTs, vars, interfaces, methods) <- solveImports table aimports

  let importEnv = Env { envvars        = vars
                      , envcurrentpath = fromMaybe "" (Can.apath ast)
                      , envinterfaces  = interfaces
                      , envmethods     = methods
                      }

  -- Then we infer the ast
  env <- buildInitialEnv importEnv ast
  let envWithImports = env { envvars = M.union (envvars env) vars }

  (inferredAST, env) <- inferAST envWithImports ast

  case Slv.apath inferredAST of
    Just fp -> return (M.insert fp inferredAST inferredASTs, env)


-- -- TODO: Make it call inferAST so that inferAST can return an (Infer TBD)
-- -- Well, or just adapt it somehow
runInfer :: Env -> Can.AST -> Either InferError Slv.AST
runInfer env ast =
  fst . fst <$> runExcept (runStateT (inferAST env ast) Unique { count = 0 })
