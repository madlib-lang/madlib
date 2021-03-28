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
import           Data.Maybe
import           Data.List
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Infer.Unify
import           Infer.Instantiate

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
    Can.TypedExp (Can.Canonical _ (Can.Assignment name _)) sc -> return $ extendVars env (name, sc)

    Can.TypedExp (Can.Canonical _ (Can.Export (Can.Canonical _ (Can.Assignment name _)))) sc ->
      return $ extendVars env (name, sc)

    (Can.Assignment name _) -> return $ extendVars env (name, Forall [Star] $ [] :=> TGen 0)

    (Can.Export (Can.Canonical _ (Can.Assignment name _))) ->
      return $ extendVars env (name, Forall [Star] $ [] :=> TGen 0)

    _ -> return env

  populateTopLevelTypings nextEnv es


buildInitialEnv :: Env -> Can.AST -> Infer Env
buildInitialEnv priorEnv Can.AST { Can.aexps, Can.atypedecls, Can.ainterfaces, Can.ainstances, Can.apath = Just apath }
  = do
    let methods = foldl (\mtds (Can.Canonical _ (Can.Interface _ _ _ mtds' _)) -> mtds <> mtds') mempty ainterfaces
    env' <- foldM (\env (Can.Canonical _ (Can.Interface id preds vars _ _)) -> addInterface env id vars preds)
                  priorEnv
                  ainterfaces
    env'' <- foldM
      (\env inst@(Can.Canonical _ (Can.Instance id preds p _)) ->
        catchError (addInstance env preds p) (addContext env' inst)
      )
      env'
      ainstances

    let constructors = concat $ mapMaybe
          (\case
            Can.Canonical _ adt@Can.ADT{} ->
              Just $ Can.adtconstructors adt
            _ -> Nothing
          )
          atypedecls

    env''' <- addConstructors env'' constructors

    let env = env''' { envMethods     = methods <> envMethods priorEnv
                     , envCurrentPath = apath
                     }

    populateTopLevelTypings env aexps



hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = S.fromList list



addConstructors :: Env -> [Can.Constructor] -> Infer Env
addConstructors env ctors = do
  foldM (\env'' ctor@(Can.Canonical area (Can.Constructor name _ sc)) -> do
          catchError (safeExtendVars env'' (name, sc)) (\(InferError e _) -> throwError $ InferError e (Context (envCurrentPath env'') area [BTConstructor ctor]))
        ) env ctors

extractExportedExps :: Slv.AST -> M.Map Slv.Name Slv.Exp
extractExportedExps Slv.AST { Slv.aexps, Slv.apath } = case apath of
  Just p -> M.fromList $ bundleExports <$> filter Slv.isExport aexps

 where
  bundleExports :: Slv.Exp -> (Slv.Name, Slv.Exp)
  bundleExports e'@(Slv.Solved _ _ exp) = case exp of
    Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)) -> (n, e')

    Slv.TypedExp (Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)))) _ -> (n, e')


findASTM :: Slv.Table -> FilePath -> Infer Slv.AST
findASTM table path = case M.lookup path table of
  Just a  -> return a
  Nothing -> throwError $ InferError (ImportNotFound path) NoContext


extractImportedConstructors :: Env -> Slv.AST -> Can.Import -> Vars
extractImportedConstructors env ast imp =
  let exportedADTs        = Slv.getValue <$> filter Slv.isADTExported (Slv.atypedecls ast)
      exportedCtors       = concat $ Slv.adtconstructors <$> exportedADTs
      exportedCtorNames   = Slv.getConstructorName <$> exportedCtors
      exportedCtorSchemes = M.restrictKeys (envVars env) $ S.fromList exportedCtorNames
  in  filterExportsByImport imp exportedCtorSchemes


extractImportedVars :: Env -> Slv.AST -> Can.Import -> Infer Vars
extractImportedVars env ast imp = do
  let exportedNames = M.keys $ extractExportedExps ast
  exportTuples <- mapM (\name -> (name, ) <$> lookupVar env name) exportedNames
  let exports = M.fromList exportTuples
  return $ filterExportsByImport imp exports


filterExportsByImport :: Can.Import -> Vars -> Vars
filterExportsByImport imp vars = case imp of
  Can.Canonical _ (Can.DefaultImport namespace _ _) -> M.mapKeys ((namespace <> ".") <>) vars

  Can.Canonical _ (Can.NamedImport   names     _ _) -> M.restrictKeys vars $ S.fromList names


solveImports :: Can.Table -> [Can.Import] -> Infer (Slv.Table, Vars, Interfaces, Methods)
solveImports = solveImports' mempty

solveImports' :: M.Map FilePath (Slv.Table, Slv.AST, Env) -> Can.Table -> [Can.Import] -> Infer (Slv.Table, Vars, Interfaces, Methods)
solveImports' solved table (imp : is) = do
  let modulePath = Can.getImportAbsolutePath imp

  solvedImport@(solvedTable, solvedAST, solvedEnv) <- case M.lookup modulePath solved of
    Just x -> return x
    Nothing -> case Can.findAST table modulePath of
      Right ast -> do
        (solvedTable, solvedEnv) <- solveTable' solved table ast
        solvedAST          <- findASTM solvedTable modulePath
        return (solvedTable, solvedAST, solvedEnv)

  importedVars       <- extractImportedVars solvedEnv solvedAST imp
  let constructorImports = extractImportedConstructors solvedEnv solvedAST imp
  let solvedVars = constructorImports <> importedVars
  let solvedInterfaces = envInterfaces solvedEnv
  let solvedMethods = envMethods solvedEnv


  let solved' = M.insert modulePath solvedImport solved
  (nextTable, nextVars, nextInterfaces, nextMethods) <- solveImports' solved' table is

  return ( M.insert modulePath solvedAST (solvedTable <> nextTable)
         , solvedVars <> nextVars
         , mergeInterfaces solvedInterfaces nextInterfaces
         , M.union solvedMethods nextMethods
         )

solveImports' _ _ [] = return (M.empty, envVars initialEnv, envInterfaces initialEnv, envMethods initialEnv)

mergeInterfaces :: Interfaces -> Interfaces -> Interfaces
mergeInterfaces = M.foldrWithKey mergeInterface

mergeInterface :: Id -> Interface -> Interfaces -> Interfaces
mergeInterface = M.insertWith (\(Interface tvs ps is) (Interface _ _ is') -> Interface tvs ps $ is `union` is')

updateInterface :: Can.Interface -> Slv.Interface
updateInterface (Can.Canonical area (Can.Interface name preds vars methods methodTypings)) =
  Slv.Untyped area $ Slv.Interface name preds vars methods (M.map updateTyping methodTypings)

inferAST :: Env -> Can.AST -> Infer (Slv.AST, Env)
inferAST env Can.AST { Can.aexps, Can.apath, Can.aimports, Can.atypedecls, Can.ainstances, Can.ainterfaces } = do
  (inferredExps, env') <- inferExps env aexps
  inferredInstances    <- resolveInstances env' { envBacktrace = [] } ainstances --mapM (resolveInstance env') ainstances
  let updatedInterfaces = updateInterface <$> ainterfaces

  updatedADTs <- mapM updateADT atypedecls

  return
    ( Slv.AST
      { Slv.aexps       = inferredExps
      , Slv.apath       = apath
      , Slv.atypedecls  = updatedADTs
      , Slv.aimports    = (\case
                            g@(Slv.Untyped _    Slv.DefaultImport{}           ) -> g
                            i@(Slv.Untyped area (Slv.NamedImport names fp afp)) -> Slv.Untyped area
                              $ Slv.NamedImport (mapMaybe (\n -> M.lookup n (envVars env) >> Just n) names) fp afp
                          )
                          .   updateImport
                          <$> aimports
      , Slv.ainterfaces = updatedInterfaces
      , Slv.ainstances  = inferredInstances
      }
    , env'
    )


updateADT :: Can.TypeDecl -> Infer Slv.TypeDecl
updateADT (Can.Canonical area adt@Can.ADT{}) = do
  updatedConstructors <- mapM updateADTConstructor (Can.adtconstructors adt)
  return $ Slv.Untyped
    area
    Slv.ADT { Slv.adtname         = Can.adtname adt
            , Slv.adtparams       = Can.adtparams adt
            , Slv.adtconstructors = updatedConstructors
            , Slv.adtexported     = Can.adtexported adt
            , Slv.adtType         = Can.adtType adt
            }
updateADT (Can.Canonical area alias@Can.Alias{}) = return $ Slv.Untyped
  area
  Slv.Alias { Slv.aliasname     = Can.aliasname alias
            , Slv.aliasparams   = Can.aliasparams alias
            , Slv.aliastype     = updateTyping $ Can.aliastype alias
            , Slv.aliasexported = Can.aliasexported alias
            }

updateADTConstructor :: Can.Constructor -> Infer Slv.Constructor
updateADTConstructor (Can.Canonical area (Can.Constructor cname cparams scheme)) = do
  (ps :=> t) <- instantiate scheme
  return $ Slv.Untyped area $ Slv.Constructor cname (updateTyping <$> cparams) t

updateImport :: Can.Import -> Slv.Import
updateImport i = case i of
  Can.Canonical area (Can.NamedImport   ns p fp) -> Slv.Untyped area $ Slv.NamedImport ns p fp

  Can.Canonical area (Can.DefaultImport n  p fp) -> Slv.Untyped area $ Slv.DefaultImport n p fp


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
  (table, _) <- solveTable' mempty table ast
  return table

solveTable' :: M.Map FilePath (Slv.Table, Slv.AST, Env) -> Can.Table -> Can.AST -> Infer (Slv.Table, Env)
solveTable' solved table ast@Can.AST { Can.aimports } = do
  -- First we resolve imports to update the env
  (inferredASTs, vars, interfaces, methods) <- solveImports' solved table aimports

  let importEnv = Env { envVars        = vars
                      , envCurrentPath = fromMaybe "" (Can.apath ast)
                      , envInterfaces  = interfaces
                      , envMethods     = methods
                      , envBacktrace   = mempty
                      }

  -- Then we infer the ast
  env <- buildInitialEnv importEnv ast
  let envWithImports = env { envVars = M.union (envVars env) vars }

  (inferredAST, env) <- inferAST envWithImports ast

  case Slv.apath inferredAST of
    Just fp -> return (M.insert fp inferredAST inferredASTs, env)


-- -- TODO: Make it call inferAST so that inferAST can return an (Infer TBD)
-- -- Well, or just adapt it somehow
runInfer :: Env -> Can.AST -> Either InferError Slv.AST
runInfer env ast =
  let result = runExcept (runStateT (inferAST env ast) InferState { count = 0, errors = [] })
  in  case result of
        Left e -> Left e

        Right ((ast, _), state) ->
          let errs      = errors state
              hasErrors = not (null errs)
          in  if hasErrors then Left (head errs) else Right ast
