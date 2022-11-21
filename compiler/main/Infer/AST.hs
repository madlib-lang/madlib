{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Infer.AST where

import qualified AST.Solved                                 as Slv
import qualified AST.Canonical                              as Can
import qualified Canonicalize.AST                           as Can
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
import           Data.Maybe
import           Data.List
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import           Infer.Unify
import           Infer.Instantiate
import           Infer.Scope
import           Debug.Trace
import           Text.Show.Pretty
import qualified Data.Set                                   as Set
import           Infer.Pred
import           Infer.Scheme
import           Infer.Substitute
import           AST.Canonical (getImportAbsolutePath)
import           Canonicalize.InstanceToDerive
import qualified Driver.Query                               as Query
import qualified Rock
import Explain.Location (emptyArea)
import Run.Options
import qualified Infer.ExhaustivePatterns as ExhaustivePatterns
import Utils.List (removeDuplicates)
import Canonicalize.Derive (deriveEqInstance, deriveInspectInstance)


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
      throwError $ CompilationError err (Context (envCurrentPath env) (Can.getArea exp))
    )

  populateTopLevelTypings nextEnv' es


buildDefaultImportNames :: [String] -> Env -> [Can.Import] -> Infer [String]
buildDefaultImportNames alreadyFound env imports = case imports of
  [] ->
    return alreadyFound

  imp : next ->
    case Can.getImportNamespace imp of
      Just n ->
        if n `elem` alreadyFound then
          throwError $ CompilationError (NameAlreadyDefined n) (Context (envCurrentPath env) (Can.getArea imp))
        else
          buildDefaultImportNames (n : alreadyFound) env next

      Nothing ->
        buildDefaultImportNames alreadyFound env next


-- TODO: split this ugly mess
buildInitialEnv :: Env -> Can.AST -> Infer Env
buildInitialEnv priorEnv Can.AST { Can.apath = Nothing } = return priorEnv
buildInitialEnv priorEnv Can.AST { Can.atypedecls, Can.ainterfaces, Can.ainstances, Can.apath = Just apath, Can.aimports }
  = do
    let methods = foldr (\(Can.Canonical _ (Can.Interface _ _ _ mtds' _)) mtds -> mtds <> mtds') mempty ainterfaces
    env' <- foldM (\env (Can.Canonical area (Can.Interface id preds vars _ _)) -> addInterface env { envCurrentPath = apath } area id vars preds)
                  priorEnv
                  ainterfaces
    env'' <- foldM
      (\env inst@(Can.Canonical _ (Can.Instance _ preds p _)) ->
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

    -- We add these to get errors when redefining a default import
    defaultImportNames <- buildDefaultImportNames [] env''' { envCurrentPath = apath } aimports
    let defaultImportVars  = M.fromList $ (, Forall [] $ [] :=> tVar "__defaultImport__") <$> defaultImportNames

    return $ env''' { envMethods = methods <> envMethods priorEnv, envCurrentPath = apath, envVars = defaultImportVars <> envVars env''' }


addConstructors :: Env -> [Can.Constructor] -> Infer Env
addConstructors env ctors = do
  foldM
    (\env'' (Can.Canonical area (Can.Constructor name _ sc _)) -> do
      env''' <- catchError
        (safeExtendVars env'' (name, sc))
        (\(CompilationError e _) ->
          throwError $ CompilationError e (Context (envCurrentPath env'') area)
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
  (_ :=> t) <- instantiate scheme
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
  Can.Canonical area (Can.TypeImport _ p fp) ->
    if hasModuleNormalImport allImports fp then
      Nothing
    else
      Just $ Slv.Untyped area $ Slv.NamedImport [] p fp

  Can.Canonical area (Can.DefaultImport n p fp) ->
    Just $ Slv.Untyped area $ Slv.DefaultImport (updateImportName n) p fp


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


buildImportInfos :: Env -> Can.AST -> Env
buildImportInfos env Can.AST { Can.aimports } =
  let info = concatMap importInfo aimports
  in  env { envImportInfo = info }




searchTypeInConstructor :: Id -> Type -> Maybe Type
searchTypeInConstructor id t = case t of
  TVar (TV n _) ->
    if n == id then Just t else Nothing

  TCon _ _ ->
    Nothing

  TApp l r ->
    case (l, r) of
      (TVar (TV n _), _) | n == id ->
        Just t

      _ ->
        case (searchTypeInConstructor id l, searchTypeInConstructor id r) of
          (Just x, _     ) ->
            Just x

          (_     , Just x) ->
            Just x

          _                ->
            Nothing

  _ ->
    Nothing


chars :: [String]
chars = (:"") <$> ['a'..]


-- TODO: Move to Infer.Derive
buildEnvForDerivedInstance :: Env -> InstanceToDerive -> Env
buildEnvForDerivedInstance env@Env{ envInterfaces } instanceToDerive = case instanceToDerive of
  TypeDeclToDerive (Can.Canonical _ Can.ADT { Can.adtparams, Can.adtconstructors, Can.adtType }) ->
        -- Env
    let constructorTypes = Can.getCtorType <$> adtconstructors
        varsInType  = Set.toList $ Set.fromList $ concat $ (\t -> mapMaybe (`searchTypeInConstructor` t) adtparams) <$> constructorTypes
        instPreds interfaceName =
          (\varInType ->
              IsIn interfaceName [varInType] Nothing
          ) <$> varsInType
        eqInstPreds = instPreds "Eq"
        inspectInstPreds = instPreds "Inspect"
        eqInstanceForEnv = Instance (eqInstPreds :=> IsIn "Eq" [adtType] Nothing) mempty
        inspectInstanceForEnv = Instance (inspectInstPreds :=> IsIn "Inspect" [adtType] Nothing) mempty
        newEqInterface = case M.lookup "Eq" envInterfaces of
                          Just (Interface vars preds instances) ->
                            Interface vars preds (eqInstanceForEnv : instances)

                          _ ->
                            undefined
        newInspectInterface = case M.lookup "Inspect" envInterfaces of
                          Just (Interface vars preds instances) ->
                            Interface vars preds (inspectInstanceForEnv : instances)

                          _ ->
                            undefined
        updatedInterfaces = M.insert "Inspect" newInspectInterface $ M.insert "Eq" newEqInterface envInterfaces
    in  env { envInterfaces = updatedInterfaces }

  RecordToDerive fieldNames ->
    let fieldNamesWithVars = zip (Set.toList fieldNames) chars
        fields             = TVar . (`TV` Star) <$> M.fromList fieldNamesWithVars
        recordType         = TRecord fields Nothing mempty
        instPreds interfaceName = (\var -> IsIn interfaceName [var] Nothing) <$> M.elems fields
        inspectInstPreds = instPreds "Inspect"
        eqInstanceForEnv = Instance (instPreds "Eq" :=> IsIn "Eq" [recordType] Nothing) mempty
        inspectInstanceForEnv = Instance (inspectInstPreds :=> IsIn "Inspect" [recordType] Nothing) mempty
        newEqInterface = case M.lookup "Eq" envInterfaces of
                          Just (Interface vars preds instances) ->
                            Interface vars preds (eqInstanceForEnv : instances)

                          _ ->
                            undefined
        newInspectInterface = case M.lookup "Inspect" envInterfaces of
                          Just (Interface vars preds instances) ->
                            Interface vars preds (inspectInstanceForEnv : instances)

                          _ ->
                            undefined
        updatedInterfaces = M.insert "Inspect" newInspectInterface $ M.insert "Eq" newEqInterface envInterfaces
    in  env { envInterfaces = updatedInterfaces }

buildEnvForDerivedInstances :: Env -> [InstanceToDerive] -> Env
buildEnvForDerivedInstances env instancesToDerive =
  foldr (flip buildEnvForDerivedInstance) env instancesToDerive


verifyTopLevelExp :: FilePath -> Slv.Exp -> Infer ()
verifyTopLevelExp astPath exp = case exp of
  Slv.Typed _ _ (Slv.Assignment _ _) ->
    return ()

  Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment _ _))) ->
    return ()

  Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment _ _)))) _ _) ->
    return ()

  Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment _ _)) _ _) ->
    return ()

  Slv.Typed _ _ (Slv.JSExp _) ->
    return ()

  Slv.Typed _ _ Slv.Extern{} ->
    return ()

  Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ Slv.Extern{})) ->
    return ()

  Slv.Typed _ _ (Slv.NameExport _) ->
    return ()

  Slv.Typed _ _ (Slv.TypeExport _) ->
    return ()

  _ ->
    throwError $ CompilationError NotADefinition (Context astPath (Slv.getArea exp))


deriveExtra :: Options -> Env -> [InstanceToDerive] -> Set.Set InstanceToDerive -> Infer (Env, [Slv.Instance])
deriveExtra options env derivedTypes extra = do
  let typeDeclarationsToDerive' = removeDuplicates $ S.toList extra \\ derivedTypes
      derivedEqInstances        =
        if optGenerateDerivedInstances options then
          mapMaybe (deriveEqInstance (envCurrentPath env)) typeDeclarationsToDerive'
        else
          []
      derivedInspectInstances   =
        if optGenerateDerivedInstances options then
          mapMaybe (deriveInspectInstance (envCurrentPath env)) typeDeclarationsToDerive'
        else
          []
      allInstances = derivedEqInstances ++ derivedInspectInstances

  resolveInstances options env allInstances


inferAST :: Options -> Env -> [InstanceToDerive] -> Can.AST -> Infer (Slv.AST, Env)
inferAST options env instancesToDerive ast@Can.AST { Can.aexps, Can.apath, Can.aimports, Can.atypedecls, Can.ainstances, Can.ainterfaces } = do
  let envWithDerivedInstances = buildEnvForDerivedInstances env instancesToDerive
      namespacesInScope = namespacesInScopeFromImports aimports
      envWithNamespaces = setNamespacesInScope envWithDerivedInstances namespacesInScope
      envWithImportInfo = buildImportInfos envWithNamespaces ast
  -- TODO: remove this and make the instance retrieval be recursive to add up all instances
  envWithImports                      <- solveImports envWithImportInfo aimports
  initialEnv                          <- buildInitialEnv envWithImports ast
  fullEnv                             <- populateTopLevelTypings initialEnv (Can.aexps ast)
  (inferredExps, env'             )   <- inferExps options fullEnv aexps
  (env''        , inferredInstances)  <- resolveInstances options env' ainstances

  extraToDerive <- gets extensibleRecordsToDerive
  (_, extraDerivedInstances) <- deriveExtra options env'' instancesToDerive extraToDerive

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
                    (Slv.Untyped area (Slv.NamedImport names fp afp)) ->
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
          , Slv.ainstances  = extraDerivedInstances ++ inferredInstances
          }

  checkAST initialEnv ast'
  ExhaustivePatterns.check env'' ast'

  return (ast' , env'')
