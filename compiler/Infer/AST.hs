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
import           Error.Backtrace
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

te :: Qual Type -> a -> Slv.Solved a
te qt a = Slv.Typed qt emptyArea a


generateCtorParamPatternNames :: Char -> [Type] -> [(String, Type)]
generateCtorParamPatternNames prefix types =
  (\(n, t) -> (prefix : show n, t)) <$> zip [0..] types


buildConstructorIsForEq :: Type -> Can.Constructor -> Slv.Is
buildConstructorIsForEq adtType ctor = case ctor of
  Can.Canonical _ (Can.Constructor name typings _ ctorType) ->
    let tupleType = tTuple2Of adtType adtType
        ctorParamTypes = getParamTypes ctorType
        aVars = generateCtorParamPatternNames 'a' ctorParamTypes
        bVars = generateCtorParamPatternNames 'b' ctorParamTypes
        conditions =
          if null typings then
            te ([] :=> tBool) $ Slv.LBool "true"
          else
            foldr
              (\((aVar, aType), (bVar, bType)) previousCondition ->
                  let aType' = if isFunctionType aType then tVar "a" `fn` tVar "b" else aType
                  in  te ([] :=> tBool) $ Slv.App
                        (te ([] :=> (tBool `fn` tBool)) $ Slv.App
                          (te ([] :=> (tBool `fn` tBool `fn` tBool)) $ Slv.Var "&&" False)
                          (te ([] :=> tBool) $ Slv.App
                            (te ([] :=> (bType `fn` tBool)) $ Slv.App
                              (te ([] :=> (aType `fn` bType `fn` tBool)) $ Slv.Placeholder (Slv.MethodRef "Eq" "==" (isTVar aType'), [aType'])
                                (te ([] :=> (aType `fn` bType `fn` tBool)) $ Slv.Var "==" False))
                              (te ([] :=> aType) $ Slv.Var aVar False)
                              False
                            )
                            (te ([] :=> bType) $ Slv.Var bVar False)
                            True
                          )
                          False
                        )
                        previousCondition
                        True
              )
              (te ([] :=> tBool) $ Slv.LBool "true")
              (zip aVars bVars)
    in
      te ([] :=> (tupleType `fn` tupleType `fn` tBool)) $ Slv.Is (te ([] :=> tupleType) $ Slv.PTuple [
        te ([] :=> adtType) $ Slv.PCon name ((\(n, t) -> te ([] :=> t) $ Slv.PVar n) <$> aVars),
        te ([] :=> adtType) $ Slv.PCon name ((\(n, t) -> te ([] :=> t) $ Slv.PVar n) <$> bVars)
      ]) conditions


buildConstructorIsForInspect :: Type -> Can.Constructor -> Slv.Is
buildConstructorIsForInspect adtType ctor = case ctor of
  Can.Canonical _ (Can.Constructor name typings _ ctorType) ->
    let ctorParamTypes = getParamTypes ctorType
        vars = generateCtorParamPatternNames 'a' ctorParamTypes
        inspected =
          if null typings then
            te ([] :=> tStr) $ Slv.LStr ("\"" <> name <> "\"")
          else
            let constructorNameLStr = te ([] :=> tStr) $ Slv.LStr ("\"" <> name <> "(\"")
                closingParenthesis  = te ([] :=> tStr) $ Slv.LStr "\")\""
                inspectedValues     =
                  (\(var, varType) ->
                    let varType' = if isFunctionType varType then tVar "a" `fn` tVar "b" else varType
                    in  te ([] :=> tStr) $ Slv.App (
                          te ([] :=> (varType' `fn` tStr)) $ Slv.Placeholder (Slv.MethodRef "Inspect" "inspect" (isTVar varType'), [varType']) (
                            te ([] :=> (varType' `fn` tStr)) $ Slv.Var "inspect" False)
                          )
                          (te ([] :=> varType') $ Slv.Var var False)
                          True
                  ) <$> vars
                commaSeparated      = intersperse (te ([] :=> tStr) $ Slv.LStr "\", \"") inspectedValues
            in  te ([] :=> tStr) $ Slv.TemplateString ([constructorNameLStr] ++ commaSeparated ++ [closingParenthesis])
    in
      te ([] :=> (adtType `fn` tStr)) $ Slv.Is (te ([] :=> adtType) $ Slv.PCon name ((\(var, varType) -> te ([] :=> varType) $ Slv.PVar var) <$> vars)) inspected



instancePlaceholders :: String -> [Type] -> Slv.Exp -> Slv.Exp
instancePlaceholders interfaceName types cont = case types of
  t : nextTypes ->
    let cont' = instancePlaceholders interfaceName nextTypes cont
    in  te ([] :=> tUnit) (Slv.Placeholder (Slv.ClassRef interfaceName [] False True, [t]) cont')

  [] ->
    cont


buildFieldConditions :: Type -> [(String, Type)] -> Slv.Exp
buildFieldConditions recordType =
  foldr
    (\(fieldName, fieldType) previousCondition ->
      te ([] :=> tBool) $ Slv.App
        (te ([] :=> (tBool `fn` tBool)) $ Slv.App
          (te ([] :=> (tBool `fn` tBool `fn` tBool)) $ Slv.Var "&&" False)
          (te ([] :=> tBool) $ Slv.App
            (te ([] :=> (fieldType `fn` tBool)) $ Slv.App
              (te ([] :=> (fieldType `fn` fieldType `fn` tBool)) $ Slv.Placeholder (Slv.MethodRef "Eq" "==" True, [fieldType])
                (te ([] :=> (fieldType `fn` fieldType `fn` tBool)) $ Slv.Var "==" False) --Add method ref
              )
              (te ([] :=> fieldType) $ Slv.Access (te ([] :=> recordType) $ Slv.Var "__$a__" False) (te ([] :=> (recordType `fn` fieldType)) $ Slv.Var ('.':fieldName) False))
              False
            )
            (te ([] :=> fieldType) $ Slv.Access (te ([] :=> recordType) $ Slv.Var "__$b__" False) (te ([] :=> (recordType `fn` fieldType)) $ Slv.Var ('.':fieldName) False))
            True
          )
          False
        )
        previousCondition
        True
    )
    (te ([] :=> tBool) $ Slv.LBool "true")


inspectFields :: Type -> [(String, Type)] -> Slv.Exp
inspectFields recordType fields =
  let inspectedFields =
        (\(fieldName, fieldType) ->
            let fieldNameStr = te ([] :=> tStr) $ Slv.LStr ("\""<> fieldName <> ": \"")
                fieldValue   = te ([] :=> fieldType) $ Slv.Access (te ([] :=> recordType) $ Slv.Var "__$a__" False) (te ([] :=> (recordType `fn` fieldType)) $ Slv.Var ('.':fieldName) False)
                inspectedFieldValue = te ([] :=> tStr) $ Slv.App (te ([] :=> (fieldType `fn` tStr)) $ Slv.Placeholder (Slv.MethodRef "Inspect" "inspect" True, [fieldType]) (te ([] :=> (fieldType `fn` tStr)) $ Slv.Var "inspect" False)) fieldValue True
            in  te ([] :=> tStr) $ Slv.App (te ([] :=> (tStr `fn` tStr)) $ Slv.App (te ([] :=> (tStr `fn` tStr `fn` tStr)) $ Slv.Var "++" False) fieldNameStr False) inspectedFieldValue True
        ) <$> fields
      commaSeparated = intersperse (te ([] :=> tStr) $ Slv.LStr "\", \"") inspectedFields
  in  te ([] :=> tStr) $ Slv.TemplateString ([te ([] :=> tStr) $ Slv.LStr "\"{ \""] ++ commaSeparated ++ [te ([] :=> tStr) $ Slv.LStr "\" }\""])


-- TODO: Move to Infer.Derive
buildEnvForDerivedInstance :: (Env, [Slv.Instance]) -> InstanceToDerive -> (Env, [Slv.Instance])
buildEnvForDerivedInstance (env@Env{ envInterfaces }, instances) instanceToDerive = case instanceToDerive of
  TypeDeclToDerive (Can.Canonical _ Can.ADT { Can.adtname, Can.adtparams, Can.adtconstructors, Can.adtType }) ->
        -- Env
    let constructorTypes = Can.getCtorType <$> adtconstructors
        varsInType  = Set.toList $ Set.fromList $ concat $ (\t -> mapMaybe (`searchTypeInConstructor` t) adtparams) <$> constructorTypes
        tvsInType = ftv varsInType
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

        eqTupleType = tTuple2Of adtType adtType

        -- Instances
        eqInstance =
          Slv.Untyped emptyArea (
            Slv.Instance
            "Eq"
            eqInstPreds
            (IsIn "Eq" [adtType] Nothing)
            (
              M.singleton
              "=="
              (
                te ([] :=> (adtType `fn` adtType `fn` tBool)) (Slv.Assignment "==" (
                  instancePlaceholders "Eq" varsInType $
                    te ([] :=> (adtType `fn` adtType `fn` tBool)) $ Slv.Abs (te (eqInstPreds :=> adtType) "__$a__") [
                      te ([] :=> (adtType `fn` tBool)) $ Slv.Abs (te (eqInstPreds :=> adtType) "__$b__") [
                        te ([] :=> tBool) $ Slv.Where (te ([] :=> eqTupleType) (Slv.TupleConstructor [te ([] :=> adtType) $ Slv.Var "__$a__" False, te ([] :=> adtType) $ Slv.Var "__$b__" False]))
                          (
                            (buildConstructorIsForEq adtType <$> adtconstructors)
                              ++  [
                                    -- if no previous pattern matches then the two values are not equal
                                    te ([] :=> (eqTupleType `fn` tBool)) $ Slv.Is (te ([] :=> eqTupleType) Slv.PAny) (te ([] :=> tBool) $ Slv.LBool "false")
                                  ]
                          )
                ]])),
                Forall [] $ eqInstPreds :=> adtType
              )
            )
          )

        inspectInstance =
          Slv.Untyped emptyArea (
            Slv.Instance
              "Inspect"
              inspectInstPreds
              (IsIn "Inspect" [adtType] Nothing)
              (
                M.singleton
                "inspect"
                (
                  te ([] :=> (adtType `fn` tStr)) (Slv.Assignment "inspect" (
                    instancePlaceholders "Inspect" varsInType $
                      te ([] :=> (adtType `fn` tStr)) $ Slv.Abs (te ([] :=> adtType) "__$a__") [
                        te ([] :=> tStr) $ Slv.Where (te ([] :=> adtType) $ Slv.Var "__$a__" False)
                          (
                            (buildConstructorIsForInspect adtType <$> adtconstructors)
                              ++ 
                              [
                                -- if no previous pattern matches then the two values are not equal
                                te ([] :=> (adtType `fn` tStr)) $ Slv.Is (te ([] :=> adtType) Slv.PAny) (te ([] :=> tStr) $ Slv.LStr "\"Unknown\"")
                              ]
                          )
                  ]))
                , Forall [] $ inspectInstPreds :=> adtType
                )
              )
          )
    in  (env { envInterfaces = updatedInterfaces }, instances ++ [eqInstance, inspectInstance])

  RecordToDerive fieldNames ->
    let fieldNamesWithVars = zip (Set.toList fieldNames) chars
        fields             = TVar . (`TV` Star) <$> M.fromList fieldNamesWithVars
        varsInType         = TVar . (`TV` Star) . snd <$> fieldNamesWithVars
        recordType         = TRecord fields Nothing
        instPreds interfaceName = (\var -> IsIn interfaceName [var] Nothing) <$> M.elems fields
        eqInstPreds = instPreds "Eq"
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

        eqInstance =
          Slv.Untyped emptyArea (
            Slv.Instance
              "Eq"
              eqInstPreds
              (IsIn "Eq" [recordType] Nothing) (
                M.singleton
                "=="
                ( te ([] :=> (recordType `fn` recordType `fn` tBool)) (
                    Slv.Assignment "==" (
                      instancePlaceholders "Eq" varsInType $
                        te ([] :=> (recordType `fn` recordType `fn` tBool)) $
                          Slv.Abs (te ([] :=> recordType) "__$a__") [te ([] :=> (recordType `fn` tBool)) $
                            Slv.Abs (te ([] :=> recordType) "__$b__") [
                              buildFieldConditions recordType (M.toList fields)
                  ]]))
                , Forall [] $ eqInstPreds :=> recordType
                )
          ))

        inspectInstance =
          Slv.Untyped emptyArea (Slv.Instance "Inspect" inspectInstPreds (IsIn "Inspect" [recordType] Nothing) (
            M.singleton
            "inspect"
            (
              te ([] :=> (recordType `fn` tStr)) (Slv.Assignment "inspect" (
                instancePlaceholders "Inspect" varsInType $
                  te ([] :=> (recordType `fn` tStr)) $ Slv.Abs (te ([] :=> recordType) "__$a__") [
                    inspectFields recordType (M.toList fields)
              ])),
              Forall [] $ inspectInstPreds :=> recordType
            )
          ))
    in  (env { envInterfaces = updatedInterfaces }, instances ++ [eqInstance, inspectInstance])

buildEnvForDerivedInstances :: Env -> [InstanceToDerive] -> (Env, [Slv.Instance])
buildEnvForDerivedInstances env instancesToDerive =
  foldr (flip buildEnvForDerivedInstance) (env, []) instancesToDerive


inferAST :: Env -> [InstanceToDerive] -> Can.AST -> Infer (Slv.AST, Env)
inferAST env instancesToDerive ast@Can.AST { Can.aexps, Can.apath, Can.aimports, Can.atypedecls, Can.ainstances, Can.ainterfaces } = do
  let (envWithDerivedInstances, _) = buildEnvForDerivedInstances env instancesToDerive
      namespacesInScope = namespacesInScopeFromImports aimports
      envWithNamespaces = setNamespacesInScope envWithDerivedInstances namespacesInScope
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
