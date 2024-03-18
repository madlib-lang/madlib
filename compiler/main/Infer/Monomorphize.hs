{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module Infer.Monomorphize where

import qualified Data.Map                       as Map
import           Data.IORef
import           Infer.MonomorphizationState
import           AST.Solved
import qualified Rock
import           Driver.Query
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Infer.Unify (gentleUnify, Unify (unify))
import           Infer.Type
import           Infer.Substitute
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import           Explain.Location (emptyArea, Area)
import qualified Infer.Env as Slv
import qualified Control.Monad as Monad
import           Run.Target (Target (TBrowser, TNode))
import           Control.Monad (when)
import           Control.Applicative


genType :: Substitution -> Type -> Type
genType s t =
  let t' = apply s t
      tvs = ftv t'
      sWithGens = Map.fromList $ zipWith (curry (\(_, TV initial k) -> (TV initial k, tUnit))) [0..] tvs
      -- sWithGens = Map.fromList $ zipWith (curry (\(index, TV initial k) -> (TV initial k, TVar $ TV (index - 1000) k))) [0..] tvs
  in  cleanRecords $ apply s $ apply sWithGens t'


cleanRecords :: Type -> Type
cleanRecords t = case t of
  TRecord fields _ _ ->
    TRecord (cleanRecords <$> fields) Nothing mempty

  TApp l r ->
    TApp (cleanRecords l) (cleanRecords r)

  or ->
    or


findCtorForeignModulePath :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> m String
findCtorForeignModulePath moduleWhereItsUsed ctorName = do
  (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
  case findForeignModuleForImportedName ctorName ast of
    Just foreignModulePath -> do
      findCtorForeignModulePath foreignModulePath ctorName

    _ ->
      return moduleWhereItsUsed


findNamespaceModulePath :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> m String
findNamespaceModulePath moduleWhereItsUsed namespace = do
  (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
  case findForeignModuleForImportedName namespace ast of
    Just foreignModulePath -> do
      return foreignModulePath

    _ ->
      return moduleWhereItsUsed


-- TODO: move this to a Query!
findExpByName :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> m (Maybe (Exp, FilePath))
findExpByName moduleWhereItsUsed expName = do
  maybeExp <- Rock.fetch $ ForeignExp moduleWhereItsUsed expName
  case maybeExp of
    Just found ->
      return $ Just (found, moduleWhereItsUsed)

    Nothing -> do
      (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
      case findForeignModuleForImportedName expName ast of
        Just foreignModulePath -> do
          found <- Rock.fetch $ ForeignExp foreignModulePath expName

          case found of
            Nothing ->
              findExpByName foreignModulePath expName

            Just exp ->
              return $ Just (exp, foreignModulePath)

        _ ->
          return Nothing


findForeignExpByNameInNamespace :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> String -> m (Maybe (Exp, FilePath))
findForeignExpByNameInNamespace moduleWhereItsUsed expName namespace = do
  (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
  case findForeignModuleForImportedName namespace ast of
    Just foreignModulePath -> do
      found <- Rock.fetch $ ForeignExp foreignModulePath expName

      case found of
        Just exp ->
          return $ Just (exp, foreignModulePath)

        Nothing ->
          findExpByName foreignModulePath expName

    _ ->
      return Nothing


type Monomorphize a = forall m . (Rock.MonadFetch Query m, MonadIO m) => m a

data Env
  = Env
  { envCurrentModulePath :: FilePath
  , envSubstitution :: Substitution
  , envLocalState :: IORef [ScopeState]
  , envEntrypointPath :: FilePath
  , envLocalBindingsToExclude :: Set.Set String
  }


-- TODO: it's possibly not fully complete
applyAndCleanQt :: Substitution -> Qual Type -> Qual Type
applyAndCleanQt subst (ps :=> t) =
  let ps' = apply subst ps
      filteredPs = filter (\(IsIn _ ts _) -> not $ null (ftv ts)) ps'
  in  filteredPs :=> apply subst t


updateName :: String -> Exp -> Exp
updateName newName fnDefinition = case fnDefinition of
  Typed qt area (Export e) ->
    Typed qt area (Export (updateName newName e))

  Typed qt area (Extern qualType _ foreignName) ->
    Typed qt area (Extern qualType newName foreignName)

  Typed qt area (TypedExp e typing sc) ->
    Typed qt area (TypedExp (updateName newName e) typing sc)

  Typed qt area (Assignment _ e) ->
    Typed qt area (Assignment newName (updateName newName e))

  or ->
    or


addImport :: FilePath -> FilePath -> String -> Type -> ImportType -> Monomorphize ()
addImport localModule foreignModule monomorphicName t importType = do
  Monad.when (localModule /= foreignModule) $ do
    liftIO $ atomicModifyIORef
      monomorphizationImports
      (\imports ->
        let importsForCurrentModule = Maybe.fromMaybe mempty $ Map.lookup localModule imports
            withNewImport = Map.insertWith (<>) foreignModule (Set.singleton (monomorphicName, t, importType)) importsForCurrentModule
        in  (Map.insert localModule withNewImport imports, ())
      )


makeDefinitionType :: Type -> Exp -> Type
makeDefinitionType typeItIsCalledWith def =
  let definedParamCount = getFullAbsParamCount def
      paramTypes = getParamTypes typeItIsCalledWith
      returnType = getReturnType typeItIsCalledWith
      paramsBefore = take definedParamCount paramTypes
      paramsAfter = drop definedParamCount paramTypes
  in  if null paramsAfter || null paramsBefore then
        typeItIsCalledWith
      else if length paramsBefore > 1 then
        foldr1 fn paramsBefore `fn` foldr fn returnType paramsAfter
      else
        head paramsBefore `fn` foldr fn returnType paramsAfter
        -- foldr fn (foldr fn returnType paramsAfter) paramsBefore


-- TODO: split this monster in 3 sub functions
monomorphizeDefinition :: Target -> Bool -> Env -> String -> Type -> Monomorphize String
monomorphizeDefinition target isMain env@Env{ envCurrentModulePath, envLocalState } fnName typeItIsCalledWith' = do
  let typeItIsCalledWith = genType (envSubstitution env) typeItIsCalledWith'

  -- first we look in the local namespace
  localState <- liftIO $ readIORef envLocalState
  let flippedScopes = reverse localState
  let foundScopeIndex = List.findIndex (\ScopeState { ssDefinitions } -> Map.member fnName ssDefinitions) flippedScopes
  case foundScopeIndex of
    Just index -> do
      let ScopeState { ssRequests, ssDefinitions } = flippedScopes!!index
      let (Just fnDefinition) = Map.lookup fnName ssDefinitions
      let fnId = FunctionId fnName envCurrentModulePath typeItIsCalledWith
      case Map.lookup fnId ssRequests of
        Just MonomorphizationRequest { mrIndex } -> do
          let monomorphicName = buildMonomorphizedName fnName mrIndex
          return monomorphicName

        Nothing -> do
          let s = gentleUnify (getType fnDefinition) typeItIsCalledWith
          (monomorphicName, nextIndex) <- liftIO $ atomicModifyIORef
            envLocalState
            (\localState ->
              let flippedScopes = reverse localState
                  ScopeState { ssRequests, ssDefinitions } = flippedScopes!!index
                  nextIndex = Map.size $ Map.filterWithKey (\FunctionId { fiFunctionName } _ -> fiFunctionName == fnName) ssRequests
                  req = MonomorphizationRequest nextIndex Nothing False
                  withNewRequest = Map.insert fnId req ssRequests
                  updatedScope = ScopeState { ssRequests = withNewRequest, ssDefinitions }
                  monomorphicName = buildMonomorphizedName fnName nextIndex
                  result =
                    List.reverse $ zipWith
                      (\scope i -> (if i == index then updatedScope else scope))
                      flippedScopes
                      [0..]
              in  (result, (monomorphicName, nextIndex))
            )

          monomorphized <-
            monomorphize
              target
              env { envSubstitution = s }
              (updateName monomorphicName fnDefinition)

          liftIO $ atomicModifyIORef
            envLocalState
            (\localState ->
              let flippedScopes = reverse localState
                  ScopeState { ssRequests, ssDefinitions } = flippedScopes!!index
                  updatedReq = MonomorphizationRequest nextIndex (Just monomorphized) False
                  withUpdatedRequest = Map.insert fnId updatedReq ssRequests
                  updatedScope2 = ScopeState { ssRequests = withUpdatedRequest, ssDefinitions }
                  updatedScopes2 = List.reverse $ zipWith
                    (\scope i -> (if i == index then updatedScope2 else scope))
                    flippedScopes
                    [0..]
              in  (updatedScopes2, ())
            )

          return monomorphicName

    Nothing -> do
      -- then we look in the global namespace if not found in local namespace
      state <- liftIO $ readIORef monomorphizationState
      (fnName', foundExp) <-
        if "." `List.isInfixOf` fnName then do
          let namespace = takeWhile (/= '.') fnName
          let realFnName = tail $ dropWhile (/= '.') fnName
          found <- findForeignExpByNameInNamespace envCurrentModulePath realFnName namespace
          return (realFnName, found)
        else do
          found <- findExpByName envCurrentModulePath fnName
          return (fnName, found)

      case foundExp of
        Just (fnDefinition, fnModulePath) ->
          if isExtern fnDefinition then do
            -- For now we skip externs completely and simply rename them to avoid collisions
            let typeForExtern = getType fnDefinition
            let fnId = FunctionId fnName' fnModulePath typeForExtern

            case Map.lookup fnId state of
              Just MonomorphizationRequest { mrIndex } -> do
                let monomorphicName = buildMonomorphizedName fnName' mrIndex
                addImport envCurrentModulePath fnModulePath monomorphicName typeForExtern (DefinitionImport $ length $ getParamTypes typeForExtern)
                return monomorphicName

              Nothing -> do
                monomorphicName <- liftIO $ newRequest fnName' fnModulePath False typeForExtern
                liftIO $ setRequestResult fnName' fnModulePath False typeForExtern (updateName monomorphicName fnDefinition)
                addImport envCurrentModulePath fnModulePath monomorphicName typeForExtern (DefinitionImport $ length $ getParamTypes typeForExtern)
                return monomorphicName
          else do
            let fnId = FunctionId fnName' fnModulePath typeItIsCalledWith

            case Map.lookup fnId state of
              Just MonomorphizationRequest { mrIndex, mrResult } -> do
                let monomorphicName = buildMonomorphizedName fnName' mrIndex
                let importType =
                      case mrResult of
                        Nothing ->
                          DefinitionImport (getFullAbsParamCount fnDefinition)

                        Just monomorphicExp ->
                          if isAbs monomorphicExp then
                            DefinitionImport (getFullAbsParamCount fnDefinition)
                          else
                            ExpressionImport
                addImport envCurrentModulePath fnModulePath monomorphicName typeItIsCalledWith importType
                return monomorphicName

              Nothing -> do
                let s = gentleUnify (getType fnDefinition) typeItIsCalledWith
                monomorphicName <- liftIO $ newRequest fnName' fnModulePath False typeItIsCalledWith
                let nameToUse =
                      if isMain then
                        fnName'
                      else
                        monomorphicName

                monomorphized <-
                  monomorphize
                    target
                    env
                      { envSubstitution = s
                      , envCurrentModulePath = fnModulePath
                      , envLocalState = makeLocalMonomorphizationState ()
                      , envLocalBindingsToExclude = mempty
                      }
                    (updateName nameToUse fnDefinition)
                liftIO $ setRequestResult fnName' fnModulePath False typeItIsCalledWith monomorphized

                let importType =
                      if isAbs monomorphized then
                        DefinitionImport (getFullAbsParamCount fnDefinition)
                      else
                        ExpressionImport
                addImport envCurrentModulePath fnModulePath monomorphicName typeItIsCalledWith importType
                return nameToUse

        Nothing -> do
          -- Try for methods:
          (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv envCurrentModulePath

          foundMethod <-
            if Map.member fnName (Slv.envMethods slvEnv) then do
              Rock.fetch $ SolvedMethodNode fnName typeItIsCalledWith
            else
              return Nothing

          case foundMethod of
            Just (methodExp@(Typed (_ :=> t) area (Assignment n method)), methodModulePath) -> do
              let fnId = FunctionId fnName methodModulePath typeItIsCalledWith
              let (typeForImport, importType) =
                    if isAbs methodExp then
                      (typeItIsCalledWith, DefinitionImport (getFullAbsParamCount methodExp))
                    else
                      (tUnit `fn` typeItIsCalledWith, DefinitionImport 1)

              case Map.lookup fnId state of
                Just MonomorphizationRequest { mrIndex } -> do
                  let monomorphicName = buildMonomorphizedName fnName mrIndex
                  addImport envCurrentModulePath methodModulePath monomorphicName typeForImport importType

                  return monomorphicName

                Nothing -> do
                  let s = gentleUnify (getType methodExp) typeItIsCalledWith
                  monomorphicName <- liftIO $ newRequest fnName methodModulePath (not $ isAbs methodExp) typeItIsCalledWith

                  let methodExp' =
                        if isAbs methodExp then
                          Typed ([] :=> t) area (Assignment n method)
                        else
                          -- Typed ([] :=> t) area (Assignment n method)
                          Typed ([] :=> (tUnit `fn` t)) area (Assignment n (Typed ([] :=> (tUnit `fn` t)) area (Abs (Typed ([] :=> tUnit) area "_") [method])))
                  let methodExp'' = Typed (getQualType methodExp') (getArea methodExp') (Export methodExp')

                  monomorphized <-
                    monomorphize
                      target
                      env
                        { envSubstitution = s
                        , envCurrentModulePath = methodModulePath
                        , envLocalState = makeLocalMonomorphizationState ()
                        , envLocalBindingsToExclude = mempty
                        }
                      (updateName monomorphicName methodExp'')
                  liftIO $ setRequestResult fnName methodModulePath (not $ isAbs methodExp) typeItIsCalledWith monomorphized
                  addImport envCurrentModulePath methodModulePath monomorphicName typeForImport importType

                  liftIO $ atomicModifyIORef
                    monomorphicMethods
                    (\methods ->
                      (methods <> Set.singleton monomorphicName, ())
                    )
                  return monomorphicName

            _ -> do
              return fnName


blackList :: [String]
blackList =
  ["&&", "||", "+", "-", "*", "/", "!=", "!", "%", ">>", ">>>", "<<", "~", "^", "unary-minus"]

eqExcludeTypes :: [Type]
eqExcludeTypes =
  [tInteger, tShort, tByte, tFloat, tStr, tBool, tUnit, tChar]

comparableExcludeTypes :: [Type]
comparableExcludeTypes =
  [tInteger, tShort, tByte, tFloat, tBool, tUnit, tChar]

monomorphizeApp :: Target -> Env -> Exp -> Monomorphize Exp
monomorphizeApp target env@Env{ envSubstitution } exp = case exp of
  Typed qt area (App fn arg final) -> do
    arg' <- monomorphize target env arg
    fn'  <- monomorphizeApp target env fn
    return $ Typed (applyAndCleanQt envSubstitution qt) area (App fn' arg' final)

  -- case of record field access
  Typed qt area (Var ('.' : fieldName) False) -> do
    return $ Typed (apply envSubstitution qt) area (Var ('.' : fieldName) False)

  Typed qt area (Var "==" False) | target == TNode || target == TBrowser ->
    return $ Typed qt area (Var "==" False)

  -- Constructors
  Typed qt area (Var ctorName True) -> do
    if "." `List.isInfixOf` ctorName then do
      let namespace = takeWhile (/= '.') ctorName
      let realCtorName = tail $ dropWhile (/= '.') ctorName
      firstLevelForeignPath <- findNamespaceModulePath (envCurrentModulePath env) namespace

      foreignModulePath <- findCtorForeignModulePath firstLevelForeignPath realCtorName
      ctor <- Rock.fetch $ ForeignConstructor foreignModulePath realCtorName

      case ctor of
        Just (Untyped _ (Constructor _ _ t)) ->
          addImport (envCurrentModulePath env) foreignModulePath realCtorName t ConstructorImport

        _ ->
          addImport (envCurrentModulePath env) foreignModulePath realCtorName (getQualified qt) ConstructorImport

      return $ Typed (apply envSubstitution qt) area (Var realCtorName True)
    else do
      foreignModulePath <- findCtorForeignModulePath (envCurrentModulePath env) ctorName
      ctor <- Rock.fetch $ ForeignConstructor foreignModulePath ctorName

      case ctor of
        Just (Untyped _ (Constructor _ _ t)) ->
          addImport (envCurrentModulePath env) foreignModulePath ctorName t ConstructorImport

        _ ->
          addImport (envCurrentModulePath env) foreignModulePath ctorName (getQualified qt) ConstructorImport

      return $ Typed (apply envSubstitution qt) area (Var ctorName True)

  Typed qt area (Var fnName False) -> do
    if
      fnName `List.elem` blackList
      || fnName `Set.member` envLocalBindingsToExclude env
      || (fnName == "==" && head (getParamTypes $ apply envSubstitution $ getQualified qt) `List.elem` eqExcludeTypes)
      || (fnName `List.elem` [">", "<", ">=", "<="] && head (getParamTypes $ apply envSubstitution $ getQualified qt) `List.elem` comparableExcludeTypes)
    then
      return $ Typed (applyAndCleanQt envSubstitution qt) area (Var fnName False)
    else if fnName == ">" then
      monomorphizeApp target env (Typed qt area (Var "__BUILTINS__.gt" False))
    else if fnName == ">=" then
      monomorphizeApp target env (Typed qt area (Var "__BUILTINS__.ge" False))
    else if fnName == "<" then
      monomorphizeApp target env (Typed qt area (Var "__BUILTINS__.lt" False))
    else if fnName == "<=" then
      monomorphizeApp target env (Typed qt area (Var "__BUILTINS__.le" False))
    else do
      let callType = genType envSubstitution (getQualified qt)
      monomorphicName <- monomorphizeDefinition target False env fnName callType


      (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv (envCurrentModulePath env)
      let isMethod = Map.member fnName (Slv.envMethods slvEnv)
      if isMethod && monomorphicName /= fnName then do
        -- find the req for the method
        state <- liftIO $ readIORef monomorphizationState
        let found =
              Map.filterWithKey
                (\(FunctionId fnName' _ t) _ -> fnName' == fnName && t == callType)
                state
        case Map.elems found of
          [MonomorphizationRequest _ _ True] -> do
            let (_ :=> t') = applyAndCleanQt envSubstitution qt
            return $ Typed (applyAndCleanQt envSubstitution qt) area (App (Typed ([] :=> (tUnit `fn` t')) area (Var monomorphicName False)) (Typed ([] :=> tUnit) area LUnit) True)

          _ ->
            return $ Typed (applyAndCleanQt envSubstitution qt) area (Var monomorphicName False)

            
      else
        return $ Typed (applyAndCleanQt envSubstitution qt) area (Var monomorphicName False)

  e -> do
    monomorphize target env e


monomorphizeLocalAssignment :: Target -> Env -> Qual Type -> Area -> String -> Exp -> Monomorphize Exp
monomorphizeLocalAssignment target env qt area name exp = case exp of
  Typed _ _ Abs{} -> do
    liftIO $ atomicModifyIORef
      (envLocalState env)
      (\localState ->
        let currentScopeState = last localState
            withNewDefinition =
              currentScopeState
                { ssDefinitions = Map.insert name (Typed qt area (Assignment name exp)) (ssDefinitions currentScopeState) }
        in  (init localState <> [withNewDefinition], ())
      )

    return exp

  _ ->
    monomorphize target env exp


monomorphizeBodyExp :: Target -> Env -> Exp -> Monomorphize Exp
monomorphizeBodyExp target env exp = case exp of
  Typed _ _ (TypedExp e _ _) -> do
    monomorphizeBodyExp target env e

  Typed qt area (Assignment n e) -> do
    e' <- monomorphizeLocalAssignment target env qt area n e
    return $ Typed (apply (envSubstitution env) qt) area (Assignment n e')

  Typed qt area node ->
    monomorphize target env $ Typed (apply (envSubstitution env) qt) area node

  or ->
    monomorphize target env or


pushNewScopeState :: Env -> Monomorphize ()
pushNewScopeState env = do
  liftIO $ atomicModifyIORef
    (envLocalState env)
    (\localState ->
      (localState <> [ScopeState mempty mempty], ())
    )


popScopeState :: Env -> Monomorphize ScopeState
popScopeState env = do
  liftIO $ atomicModifyIORef
    (envLocalState env)
    (\localState ->
      let withPoppedScope = init localState
          poppedState = last localState
      in  (withPoppedScope, poppedState)
    )


replaceLocalFunctions :: Map.Map FunctionId MonomorphizationRequest -> Exp -> [Exp]
replaceLocalFunctions requests exp = case getExpName exp of
  Just name ->
    let matchingRequests = Map.elems $ Map.filterWithKey (\fnId _ -> fiFunctionName fnId == name) requests
    in  case matchingRequests of
      [] ->
        if isAbs exp then
          []
        else
          [exp]

      reqs ->
        map (Maybe.fromMaybe undefined . mrResult) reqs

  Nothing ->
    [exp]


getScopeBindingsToExclude :: [Exp] -> Set.Set String
getScopeBindingsToExclude exps =
  let notFunctions = filter (not . isAbs) exps
      bindings = Maybe.mapMaybe getExpName notFunctions
  in  Set.fromList bindings


monomorphize :: Target -> Env -> Exp -> Monomorphize Exp
monomorphize target env@Env{ envSubstitution } exp = case exp of
  Typed _ _ App{} -> do
    monomorphizeApp target env exp

  Typed qt area (Export e) -> do
    e' <- monomorphize target env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Export e')

  Typed _ _ (TypedExp e _ _) -> do
    monomorphize target env e

  Typed qt area (Assignment n e) -> do
    e' <- monomorphize target env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Assignment n e')

  Typed qt area (Mutate lhs e) -> do
    lhs' <- monomorphize target env lhs
    e' <- monomorphize target env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Mutate lhs' e')

  -- Look for simple function names as args, record field, list item etc
  Typed _ _ (Var _ _) -> do
    monomorphizeApp target env exp

  Typed qt area (Abs (Typed pQt pArea pName) es) -> do
    pushNewScopeState env
    let localBindingsToExclude = getScopeBindingsToExclude es
    let env' = env { envLocalBindingsToExclude = envLocalBindingsToExclude env <> Set.singleton pName <> localBindingsToExclude }
    es' <- mapM (monomorphizeBodyExp target env') es
    poppedScopeState <- popScopeState env
    let requestsFromScope = ssRequests poppedScopeState

    let es'' = es' >>= replaceLocalFunctions requestsFromScope

    return $
      Typed (applyAndCleanQt envSubstitution qt) area
        (Abs (Typed (applyAndCleanQt envSubstitution pQt) pArea pName) es'')

  Typed qt area (Do es) -> do
    pushNewScopeState env
    es' <- mapM (monomorphizeBodyExp target env) es
    poppedScopeState <- popScopeState env
    let requestsFromScope = ssRequests poppedScopeState

    let es'' = es' >>= replaceLocalFunctions requestsFromScope

    return $ Typed (applyAndCleanQt envSubstitution qt) area (Do es'')

  Typed qt area (Record fields) -> do
    fields' <- mapM (monomorphizeField target env) fields
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Record fields')

  Typed qt area (ListConstructor items) -> do
    items' <- mapM (monomorphizeListItem target env) items
    return $ Typed (applyAndCleanQt envSubstitution qt) area (ListConstructor items')

  Typed qt area (TupleConstructor items) -> do
    items' <- mapM (monomorphize target env) items
    return $ Typed (applyAndCleanQt envSubstitution qt) area (TupleConstructor items')

  Typed qt area (TemplateString es) -> do
    es' <- mapM (monomorphize target env) es
    return $ Typed (applyAndCleanQt envSubstitution qt) area (TemplateString es')

  Typed qt area (Where e iss) -> do
    e' <- monomorphize target env e
    iss' <- mapM (monomorphizeIs target env) iss
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Where e' iss')

  Typed qt area (If cond truthy falsy) -> do
    cond' <- monomorphize target env cond
    truthy' <- monomorphize target env truthy
    falsy' <- monomorphize target env falsy
    return $ Typed (applyAndCleanQt envSubstitution qt) area (If cond' truthy' falsy')

  Typed qt area (While cond body) -> do
    cond' <- monomorphize target env cond
    body' <- monomorphize target env body
    return $ Typed (applyAndCleanQt envSubstitution qt) area (While cond' body')

  Typed qt area (Access rec (Typed qt' area' field)) -> do
    rec' <- monomorphize target env rec
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Access rec' (Typed (applyAndCleanQt envSubstitution qt') area' field))

  Typed qt area e ->
    return $ Typed (applyAndCleanQt envSubstitution qt) area e

  or ->
    return or


monomorphizeField :: Target -> Env -> Field -> Monomorphize Field
monomorphizeField target env field = case field of
  Typed qt area (Field (name, exp)) -> do
    exp' <- monomorphize target env exp
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (Field (name, exp'))

  Typed qt area (FieldSpread exp) -> do
    exp' <- monomorphize target env exp
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (FieldSpread exp')

  or ->
    return or

monomorphizeListItem :: Target -> Env -> ListItem -> Monomorphize ListItem
monomorphizeListItem target env field = case field of
  Typed qt area (ListItem exp) -> do
    exp' <- monomorphize target env exp
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (ListItem exp')

  Typed qt area (ListSpread exp) -> do
    exp' <- monomorphize target env exp
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (ListSpread exp')

  or ->
    return or


monomorphizePattern :: Target -> Env -> Pattern -> Monomorphize Pattern
monomorphizePattern target env pat = case pat of
  Typed qt area (PCon n args) -> do
    args' <- mapM (monomorphizePattern target env) args

    if "." `List.isInfixOf` n then do
      let namespace = takeWhile (/= '.') n
      let realCtorName = tail $ dropWhile (/= '.') n
      foreignModulePath <- findNamespaceModulePath (envCurrentModulePath env) namespace
      ctor <- Rock.fetch $ ForeignConstructor foreignModulePath realCtorName
      case ctor of
        Just (Untyped _ (Constructor _ _ t)) ->
          addImport (envCurrentModulePath env) foreignModulePath realCtorName t ConstructorImport

        _ ->
          addImport (envCurrentModulePath env) foreignModulePath realCtorName (getQualified qt) ConstructorImport

      return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (PCon realCtorName args')
    else do
      foreignModulePath <- findCtorForeignModulePath (envCurrentModulePath env) n
      ctor <- Rock.fetch $ ForeignConstructor foreignModulePath n

      case ctor of
        Just (Untyped _ (Constructor _ _ t)) ->
          addImport (envCurrentModulePath env) foreignModulePath n t ConstructorImport

        _ -> do
          let argTypes = map getType args
          let fullType = foldr fn (getQualified qt) argTypes
          addImport (envCurrentModulePath env) foreignModulePath n fullType ConstructorImport

      return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (PCon n args')

  Typed qt area (PRecord fields) -> do
    fields' <- mapM (monomorphizePattern target env) fields
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (PRecord fields')

  Typed qt area (PList items) -> do
    items' <- mapM (monomorphizePattern target env) items
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (PList items')

  Typed qt area (PTuple items) -> do
    items' <- mapM (monomorphizePattern target env) items
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (PTuple items')

  Typed qt area p ->
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area p

  _ ->
    return pat


varsInPattern :: Pattern -> Set.Set String
varsInPattern pat = case pat of
  Typed _ _ (PVar n) ->
    Set.singleton n

  Typed _ _ (PSpread p) ->
    varsInPattern p

  Typed _ _ (PCon _ args) -> do
    foldr (<>) Set.empty (map varsInPattern args)

  Typed _ _ (PRecord fields) -> do
    foldr (<>) Set.empty (map varsInPattern (Map.elems fields))

  Typed _ _ (PList items) -> do
    foldr (<>) Set.empty (map varsInPattern items)

  Typed _ _ (PTuple items) -> do
    foldr (<>) Set.empty (map varsInPattern items)

  _ ->
    Set.empty


monomorphizeIs :: Target -> Env -> Is -> Monomorphize Is
monomorphizeIs target env is = case is of
  Typed qt area (Is pat exp) -> do
    pat' <- monomorphizePattern target env pat
    let vars = varsInPattern pat
    exp' <- monomorphize target env { envLocalBindingsToExclude = envLocalBindingsToExclude env <> vars } exp
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (Is pat' exp')

  or ->
    return or


getMonomorphicFunctions :: String -> Map.Map FunctionId MonomorphizationRequest -> [Exp]
getMonomorphicFunctions fnName state =
  let monomorphicInstances =
        Map.filterWithKey
          (\id _ -> fiFunctionName id == fnName)
          state
  in  map (\(MonomorphizationRequest _ (Just e) _) -> e) $ Map.elems monomorphicInstances


getMonomorphicFunctionNamesAndTypes :: String -> Map.Map FunctionId MonomorphizationRequest -> [(String, Type)]
getMonomorphicFunctionNamesAndTypes fnName state =
  let monomorphicInstances =
        Map.filterWithKey
          (\id _ -> fiFunctionName id == fnName)
          state
  in  map (\(FunctionId name _ t, MonomorphizationRequest index _ _) -> (buildMonomorphizedName name index, t)) $ Map.toList monomorphicInstances


isTracker :: String -> Bool
isTracker n = "__lineTracker_" `List.isInfixOf` n || "__functionTracker_" `List.isInfixOf` n || "__branchTracker_" `List.isInfixOf` n

replaceDefinitionWithMonomorphicOnes :: Target -> Env -> Map.Map FunctionId MonomorphizationRequest -> Exp -> Monomorphize [Exp]
replaceDefinitionWithMonomorphicOnes target env state exp =
  case getExpName exp of
    Just n ->
      case getMonomorphicFunctions n state of
        [] | isTracker n -> do
          exp' <- monomorphize target env exp
          return [exp']

        or ->
          return or

    Nothing ->
      return [exp]


findMonomorphicMethods :: Map.Map FunctionId MonomorphizationRequest -> Instance -> [Exp]
findMonomorphicMethods state interface = case interface of
  Untyped _ (Instance _ _ _ methods) ->
    let methodNames = Map.keys methods
    in  concatMap (`getMonomorphicFunctions` state) methodNames

  _ ->
    []


replaceTypedNameWithMonomorphicOnes ::  Map.Map FunctionId MonomorphizationRequest -> Solved String -> [Solved String]
replaceTypedNameWithMonomorphicOnes _ Typed{} = undefined
replaceTypedNameWithMonomorphicOnes state (Untyped area n) =
  let monomorphizedNamesAndTypes = getMonomorphicFunctionNamesAndTypes n state
      mapped = map (\(monoName, _) -> Untyped area monoName) monomorphizedNamesAndTypes
  in  mapped


filterMonomorphicFunctionsForModule :: Map.Map FunctionId MonomorphizationRequest -> FilePath -> Map.Map FunctionId MonomorphizationRequest
filterMonomorphicFunctionsForModule state modulePath =
  Map.filterWithKey
    (\id _ -> fiModulePath id == modulePath)
    state


-- Target and Env are only needed so that we can monomorphize coverage trackers
-- that would otherwise be eliminated so that they resolve to the right,
-- meaning the one with the monomorphic name.
mergeResult :: Target -> Env -> AST -> Monomorphize AST
mergeResult target env ast@AST{ apath = Just currentModulePath } = do
  state <- liftIO $ readIORef monomorphizationState
  let monomorphizedFunctionsForModule = filterMonomorphicFunctionsForModule state currentModulePath
  let methodExps = Set.toList $ Set.fromList $ ainstances ast >>= findMonomorphicMethods monomorphizedFunctionsForModule
  newExps <- concat <$> mapM (replaceDefinitionWithMonomorphicOnes target env monomorphizedFunctionsForModule) (aexps ast)
  let allExps = methodExps ++ newExps

  allImports <- liftIO $ readIORef monomorphizationImports
  let importedNames = Map.toList $ Maybe.fromMaybe mempty $ Map.lookup currentModulePath allImports
  let generatedImports =
        map
          (\(foreignModulePath, names) ->
              let solvedNames = map (\(n, t, _) -> Typed ([] :=> t) emptyArea n) (Set.toList names)
              in  Untyped emptyArea (NamedImport solvedNames foreignModulePath foreignModulePath)
          )
          (filter (\(foreignPath, _) -> foreignPath /= currentModulePath) importedNames)

  return ast { aexps = allExps, aimports = generatedImports, ainstances = [], ainterfaces = [] }
mergeResult _ _ ast =
  return ast
