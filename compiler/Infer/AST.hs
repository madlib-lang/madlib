{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Infer.AST where

import qualified AST.Solved                    as Slv
import qualified AST.Canonical                 as Can
import qualified Canonicalize.AST              as Can
import           Infer.Infer
import           Infer.Env
import           Infer.EnvUtils
import           Infer.Typing
import           Infer.Interface
import           Infer.Type
import           Infer.Exp
import           Error.Error
import           Error.Warning
import           Error.Context
import           Error.Backtrace
import           Data.Maybe
import           Data.List
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Infer.Unify
import           Infer.Instantiate
import           Infer.Scope
import Debug.Trace
import Text.Show.Pretty
import qualified Data.Set as Set
import Infer.Pred
import Infer.Scheme
import Infer.Substitute
import AST.Canonical (getImportAbsolutePath)
import qualified Driver.Query as Query
import qualified Rock


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
populateTopLevelTypings env []                             = return env
populateTopLevelTypings env (exp@(Can.Canonical _ e) : es) = do
  let nextEnv = case e of
        Can.TypedExp (Can.Canonical _ (Can.Assignment name _)) _ sc -> do
          (ps :=> t) <- instantiate sc
          ps' <- dedupePreds <$> getAllParentPreds env (dedupePreds ps)
          let sc' = quantify (ftv (ps :=> t)) (ps' :=> t)
          safeExtendVars env (name, sc')

        Can.TypedExp (Can.Canonical _ (Can.Export (Can.Canonical _ (Can.Assignment name _)))) _ sc -> do
          (ps :=> t) <- instantiate sc
          ps' <- dedupePreds <$> getAllParentPreds env (dedupePreds ps)
          let sc' = quantify (ftv (ps :=> t)) (ps' :=> t)
          safeExtendVars env (name, sc')

        Can.Assignment name _ -> do
          tv <- newTVar Star
          safeExtendVars env (name, Forall [] $ [] :=> tv)

        Can.Export (Can.Canonical _ (Can.Assignment name _)) -> do
          tv <- newTVar Star
          safeExtendVars env (name, Forall [] $ [] :=> tv)

        Can.Extern sc name _ -> do
          safeExtendVars env (name, sc)

        Can.Export (Can.Canonical _ (Can.Extern sc name _)) -> do
          safeExtendVars env (name, sc)

        _ -> return env

  nextEnv' <- catchError
    nextEnv
    (\(CompilationError err _) ->
      throwError $ CompilationError err (Context (envCurrentPath env) (Can.getArea exp) (envBacktrace env))
    )

  populateTopLevelTypings nextEnv' es


-- TODO: split this ugly mess
buildInitialEnv :: Env -> Can.AST -> Infer Env
buildInitialEnv priorEnv Can.AST { Can.apath = Nothing } = return priorEnv
buildInitialEnv priorEnv Can.AST { Can.atypedecls, Can.ainterfaces, Can.ainstances, Can.apath = Just apath }
  = do
    let methods = foldr (\(Can.Canonical _ (Can.Interface _ _ _ mtds' _)) mtds -> mtds <> mtds') mempty ainterfaces
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
            Can.Canonical _ adt@Can.ADT{} -> Just $ Can.adtconstructors adt
            _                             -> Nothing
          )
          atypedecls

    env''' <- addConstructors env'' constructors

    return $ env''' { envMethods = methods <> envMethods priorEnv, envCurrentPath = apath }


addConstructors :: Env -> [Can.Constructor] -> Infer Env
addConstructors env ctors = do
  foldM
    (\env'' ctor@(Can.Canonical area (Can.Constructor name _ sc _)) -> do
      env''' <- catchError
        (safeExtendVars env'' (name, sc))
        (\(CompilationError e _) ->
          throwError $ CompilationError e (Context (envCurrentPath env'') area [BTConstructor ctor])
        )

      return env''' { envConstructors = Set.insert name (envConstructors env''') }
    )
    env
    ctors


findASTM :: Slv.Table -> FilePath -> Infer Slv.AST
findASTM table path = case M.lookup path table of
  Just a  -> return a
  Nothing -> throwError $ CompilationError (ImportNotFound path) NoContext


extractImportedConstructors :: Env -> Slv.AST -> Can.Import -> Vars
extractImportedConstructors env ast imp =
  let exportedADTs        = Slv.getValue <$> filter Slv.isADTExported (Slv.atypedecls ast)
      exportedCtors       = concat $ Slv.adtconstructors <$> exportedADTs
      exportedCtorNames   = Slv.getConstructorName <$> exportedCtors
      exportedCtorSchemes = M.restrictKeys (envVars env) $ S.fromList exportedCtorNames
  in  filterExportsByImport imp exportedCtorSchemes


extractImportedVars :: Env -> Slv.AST -> Can.Import -> Infer Vars
extractImportedVars env ast imp = do
  let exportedNames = M.keys $ Slv.extractExportedExps ast
  exportTuples <- mapM (\name -> (name, ) <$> lookupVar env name) exportedNames
  let exports = M.fromList exportTuples
  return $ filterExportsByImport imp exports


filterExportsByImport :: Can.Import -> Vars -> Vars
filterExportsByImport imp vars = case imp of
  Can.Canonical _ (Can.DefaultImport (Can.Canonical _ namespace) _ _) ->
    M.mapKeys ((namespace <> ".") <>) vars

  Can.Canonical _ (Can.NamedImport names _ _) ->
    M.restrictKeys vars $ S.fromList (Can.getCanonicalContent <$> names)

  Can.Canonical _ Can.TypeImport{} ->
    mempty

  Can.Canonical _ Can.ImportAll{}  ->
    vars


mergeEnv' :: Env -> Env -> Env
mergeEnv' previous new =
  previous
    { envInterfaces = mergeInterfaces (envInterfaces previous) (envInterfaces new)
    , envMethods = M.union (envMethods new) (envMethods previous)
    }


solveImport :: Env -> Can.Import -> Infer Env
solveImport env imp = do
  let path = Can.getImportAbsolutePath imp
  (ast, env') <- Rock.fetch $ Query.SolvedASTWithEnv path
  importedVars <- extractImportedVars env' ast imp
  let constructorImports = extractImportedConstructors env' ast imp
  let env'' = mergeEnv' env env'
  return env'' { envVars = envVars env'' <> importedVars <> constructorImports }


solveImports :: Env -> [Can.Import] -> Infer Env
solveImports env imports = case imports of
  [] ->
    return env

  _ ->
    foldM solveImport env imports


mergeInterfaces :: Interfaces -> Interfaces -> Interfaces
mergeInterfaces = M.foldrWithKey mergeInterface


mergeInterface :: Id -> Interface -> Interfaces -> Interfaces
mergeInterface = M.insertWith (\(Interface tvs ps is) (Interface _ _ is') -> Interface tvs ps $ is `union` is')


updateInterface :: Can.Interface -> Slv.Interface
updateInterface (Can.Canonical area (Can.Interface name preds vars methods methodTypings)) =
  Slv.Untyped area $ Slv.Interface name preds vars methods (M.map updateTyping methodTypings)


namespacesInScopeFromImports :: [Can.Import] -> Set.Set String
namespacesInScopeFromImports imports = Set.fromList $ mapMaybe Can.getImportNamespace imports


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
updateADTConstructor (Can.Canonical area (Can.Constructor cname cparams scheme _)) = do
  (ps :=> t) <- instantiate scheme
  return $ Slv.Untyped area $ Slv.Constructor cname (updateTyping <$> cparams) t


hasModuleNormalImport :: [Can.Import] -> FilePath -> Bool
hasModuleNormalImport allImports fp =
  any ((== fp) . Can.getImportAbsolutePath) (filter (not . Can.isTypeImport) allImports)


updateImport ::[Can.Import] -> Can.Import -> Maybe Slv.Import
updateImport allImports i = case i of
  Can.Canonical area (Can.NamedImport ns p fp) ->
    Just $ Slv.Untyped area $ Slv.NamedImport (updateImportName <$> ns) p fp

  -- If a TypeImport does not have a corresponding normal import we need to carry it in order
  -- to generate the constructor types for the LLVM backend.
  Can.Canonical area (Can.TypeImport ns p fp) ->
    if hasModuleNormalImport allImports fp then
      Nothing
    else
      Just $ Slv.Untyped area $ Slv.NamedImport [] p fp

  Can.Canonical area (Can.DefaultImport n p fp) ->
    Just $ Slv.Untyped area $ Slv.DefaultImport (updateImportName n) p fp

  -- Can.Canonical area (Can.ImportAll p fp) ->
  --   Just $ updateImportAll solvedTable i


updateImportAll :: M.Map FilePath (Slv.AST, Env) -> Can.Import -> Slv.Import
updateImportAll solvedTable (Can.Canonical area (Can.ImportAll path absPath)) = case M.lookup absPath solvedTable of
  Nothing ->
    Slv.Untyped area (Slv.NamedImport [] path absPath)

  Just (ast, _) ->
    let exportedNames       = M.keys $ Slv.extractExportedExps ast
        exportedADTs        = Slv.getValue <$> filter Slv.isADTExported (Slv.atypedecls ast)
        exportedCtors       = concat $ Slv.adtconstructors <$> exportedADTs
        exportedCtorNames   = Slv.getConstructorName <$> exportedCtors
    in  Slv.Untyped area (Slv.NamedImport (Slv.Untyped area <$> exportedNames <> exportedCtorNames) path absPath)


updateImportName :: Can.Canonical Can.Name -> Slv.Solved Slv.Name
updateImportName (Can.Canonical area name) = Slv.Untyped area name


importInfo :: Can.Import -> [ImportInfo]
importInfo (Can.Canonical _ imp) = case imp of
  Can.NamedImport names _ path ->
    ImportInfo path NameImport . Can.getCanonicalContent <$> names

  Can.TypeImport typeNames _ path ->
    ImportInfo path TypeImport . Can.getCanonicalContent <$> typeNames

  Can.DefaultImport name _ path ->
    [ImportInfo path NamespaceImport (Can.getCanonicalContent name)]

  -- TODO: handle correctly or remove import all
  Can.ImportAll _ _ ->
    []


buildImportInfos :: Env -> Can.AST -> Env
buildImportInfos env Can.AST { Can.aimports } =
  let info = concatMap importInfo aimports
  in  env { envImportInfo = info }


inferAST' :: Env -> Can.AST -> Infer (Slv.AST, Env)
inferAST' env ast@Can.AST { Can.aexps, Can.apath, Can.aimports, Can.atypedecls, Can.ainstances, Can.ainterfaces } = do
  let namespacesInScope = namespacesInScopeFromImports aimports
      envWithNamespaces = setNamespacesInScope env namespacesInScope
      envWithImportInfo = buildImportInfos envWithNamespaces ast
  -- TODO: remove this and make the instance retrieval be recursive to add up all instances
  envWithImports <- solveImports envWithImportInfo aimports
  initialEnv     <- buildInitialEnv envWithImports ast
  fullEnv        <- populateTopLevelTypings initialEnv (Can.aexps ast)
  (env'        , inferredInstances)  <- resolveInstances fullEnv { envBacktrace = [] } ainstances
  (inferredExps, env''             ) <- inferExps env' aexps
  let updatedInterfaces = updateInterface <$> ainterfaces
  updatedADTs <- mapM updateADT atypedecls

  let ast' = Slv.AST
          { Slv.aexps       = inferredExps
          , Slv.apath       = apath
          , Slv.atypedecls  = updatedADTs
          , Slv.aimports    =
            mapMaybe
              (
                (
                  (\case
                    i@(Slv.Untyped area (Slv.NamedImport names fp afp)) ->
                      Slv.Untyped area $ Slv.NamedImport
                        (mapMaybe (\(Slv.Untyped area n) -> M.lookup n (envVars fullEnv) >> Just (Slv.Untyped area n)) names)
                        fp
                        afp

                    others ->
                      others
                  )
                  <$>
                )
                . updateImport aimports
              )
              aimports
          , Slv.ainterfaces = updatedInterfaces
          , Slv.ainstances  = inferredInstances
          }

  checkAST initialEnv ast'

  return (ast' , env'')
