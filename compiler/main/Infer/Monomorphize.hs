{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use guards" #-}
module Infer.Monomorphize where

import qualified Data.Map                       as Map
import           Data.IORef
import           Infer.MonomorphizationState
import           AST.Solved
import qualified Rock
import           Driver.Query
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Infer.Unify (gentleUnify)
import           Infer.Type
import           Infer.Substitute
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Explain.Location (emptyArea, Area)
import qualified Infer.Env as Slv
import qualified Control.Monad as Monad
import Run.Target (Target (TBrowser, TNode))
import Control.Monad (when)
import Text.Show.Pretty (ppShow)

-- TODO: consider if monomorphizing local functions would have an impact
-- with regards to mutations
-- it does and we need to add value restriction

findCtorForeignModulePath :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> m String
findCtorForeignModulePath moduleWhereItsUsed ctorName = do
  (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
  case findForeignModuleForImportedName ctorName ast of
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
          return $ (,foreignModulePath) <$> found

        _ ->
          return Nothing


findForeignExpByNameInNamespace :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> String -> m (Maybe (Exp, FilePath))
findForeignExpByNameInNamespace moduleWhereItsUsed expName namespace = do
  (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
  case findForeignModuleForImportedName namespace ast of
    Just foreignModulePath -> do
      found <- Rock.fetch $ ForeignExp foreignModulePath expName
      return $ (,foreignModulePath) <$> found

    _ ->
      return Nothing


findModulePathForInterfaceByMethodNameInImports :: (Rock.MonadFetch Query m, MonadIO m) => [FilePath] -> String -> m (Maybe FilePath)
findModulePathForInterfaceByMethodNameInImports importPaths methodName = case importPaths of
  [] ->
    return Nothing

  (modulePath : nextModulePaths) -> do
    found <- findModulePathForInterfaceByMethodName modulePath methodName
    case found of
      Just realPath ->
        return $ Just realPath

      Nothing ->
        findModulePathForInterfaceByMethodNameInImports nextModulePaths methodName

findModulePathForInterfaceByMethodName :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> m (Maybe FilePath)
findModulePathForInterfaceByMethodName moduleWhereItsUsed methodName = do
  isHere <- Rock.fetch $ DefinesInterfaceForMethod moduleWhereItsUsed methodName
  if isHere then
    return $ Just moduleWhereItsUsed
  else do
    (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
    let imports = map getImportAbsolutePath (aimports ast)
    findModulePathForInterfaceByMethodNameInImports imports methodName


type Monomorphize a = forall m . (Rock.MonadFetch Query m, MonadIO m) => m a

data Env
  = Env
  { envCurrentModulePath :: FilePath
  , envSubstitution :: Substitution
  , envLocalState :: IORef [ScopeState]
  , envEntrypointPath :: FilePath
  }


-- TODO: it's possibly not fully complete
applyAndCleanQt :: Substitution -> Qual Type -> Qual Type
applyAndCleanQt subst (ps :=> t) =
  let ps' = apply subst ps
      filteredPs = filter (\(IsIn _ ts _) -> not $ null (ftv ts)) ps'
  in  filteredPs :=> apply subst t


removeParameterPlaceholdersAndUpdateName :: String -> Exp -> Exp
removeParameterPlaceholdersAndUpdateName newName fnDefinition = case fnDefinition of
  Typed qt area (Export e) ->
    Typed qt area (Export (removeParameterPlaceholdersAndUpdateName newName e))

  Typed qt area (Extern qualType _ foreignName) ->
    Typed qt area (Extern qualType newName foreignName)

  Typed qt area (TypedExp e typing sc) ->
    Typed qt area (TypedExp (removeParameterPlaceholdersAndUpdateName newName e) typing sc)

  Typed qt area (Assignment _ e) ->
    Typed qt area (Assignment newName (removeParameterPlaceholdersAndUpdateName newName e))

  Typed _ _ (Placeholder (ClassRef _ _ False _, _) e) ->
    removeParameterPlaceholdersAndUpdateName newName e

  or ->
    or


addImport :: FilePath -> FilePath -> String -> Type -> ImportType -> Monomorphize ()
addImport localModule foreignModule monomorphicName t importType = do
  Monad.when (localModule /= foreignModule) $ do
    imports <- liftIO $ readIORef monomorphizationImports
    let importsForCurrentModule = Maybe.fromMaybe mempty $ Map.lookup localModule imports
    let withNewImport = Map.insertWith (<>) foreignModule (Set.singleton (monomorphicName, t, importType)) importsForCurrentModule
    let updatedImports = Map.insert localModule withNewImport imports
    liftIO $ writeIORef monomorphizationImports updatedImports


monomorphizeDefinition :: Target -> Bool -> Env -> String -> Type -> Monomorphize String
monomorphizeDefinition target isMain env@Env{ envCurrentModulePath, envLocalState } fnName typeItIsCalledWith = do
  liftIO $ putStrLn $ "MONO: '" <> fnName <> "'"

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
          let s = gentleUnify typeItIsCalledWith (getType fnDefinition)
          let nextIndex = Map.size $ Map.filterWithKey (\FunctionId { fiFunctionName } _ -> fiFunctionName == fnName) ssRequests
          let req = MonomorphizationRequest nextIndex Nothing
          let withNewRequest = Map.insert fnId req ssRequests
          let updatedScope = ScopeState { ssRequests = withNewRequest, ssDefinitions }
          let monomorphicName = buildMonomorphizedName fnName nextIndex
          let updatedScopes = List.reverse $ zipWith
                (\scope i -> (if i == index then updatedScope else scope))
                flippedScopes
                [0..]
          liftIO $ writeIORef envLocalState updatedScopes
          monomorphized <-
            monomorphize
              target
              env
                { envSubstitution = s
                }
              (removeParameterPlaceholdersAndUpdateName monomorphicName fnDefinition)
          -- TODO: set `monomorphized` for the request result
          let updatedReq = MonomorphizationRequest nextIndex (Just monomorphized)
          let withUpdatedRequest = Map.insert fnId updatedReq ssRequests
          let updatedScope2 = ScopeState { ssRequests = withUpdatedRequest, ssDefinitions }
          let updatedScopes2 = List.reverse $ zipWith
                (\scope i -> (if i == index then updatedScope2 else scope))
                flippedScopes
                [0..]
          liftIO $ writeIORef envLocalState updatedScopes2

          return monomorphicName

    Nothing -> do
      -- then we look in the global namesapce if not found in local namespace
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
            -- For now we skip externs completely
            -- let fnId = FunctionId fnName fnModulePath (getType fnDefinition)
            liftIO $ setRequestResult fnName' fnModulePath (getType fnDefinition) fnDefinition
            addImport envCurrentModulePath fnModulePath fnName' typeItIsCalledWith DefinitionImport
            return fnName'
          else do
            let fnId = FunctionId fnName' fnModulePath typeItIsCalledWith

            case Map.lookup fnId state of
              Just MonomorphizationRequest { mrIndex, mrResult } -> do
                let monomorphicName = buildMonomorphizedName fnName' mrIndex
                let importType =
                      case mrResult of
                        Nothing ->
                          DefinitionImport

                        Just monomorphicExp ->
                          if isAbs monomorphicExp then
                            DefinitionImport
                          else
                            ExpressionImport
                addImport envCurrentModulePath fnModulePath monomorphicName typeItIsCalledWith importType
                return monomorphicName

              Nothing -> do
                let s = gentleUnify typeItIsCalledWith (getType fnDefinition)
                monomorphicName <- liftIO $ newRequest fnName' fnModulePath typeItIsCalledWith
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
                      }
                    (removeParameterPlaceholdersAndUpdateName nameToUse fnDefinition)
                liftIO $ setRequestResult fnName' fnModulePath typeItIsCalledWith monomorphized

                let importType =
                      if isAbs monomorphized then
                        DefinitionImport
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
                      (typeItIsCalledWith, DefinitionImport)
                    else if isFunctionType t then
                      (typeItIsCalledWith, ExpressionImport)
                    else
                      (tUnit `fn` typeItIsCalledWith, DefinitionImport)

              case Map.lookup fnId state of
                Just MonomorphizationRequest { mrIndex } -> do
                  let monomorphicName = buildMonomorphizedName fnName mrIndex
                  addImport envCurrentModulePath methodModulePath monomorphicName typeForImport importType

                  return monomorphicName

                Nothing -> do
                  let s = gentleUnify typeItIsCalledWith (getType methodExp)
                  monomorphicName <- liftIO $ newRequest fnName methodModulePath typeItIsCalledWith

                  let nameToUse =
                        if isMain then
                          fnName
                        else
                          monomorphicName

                  let methodExp' =
                        if isFunctionType t then
                          methodExp
                        else
                          Typed ([] :=> (tUnit `fn` t)) area (Assignment n (Typed ([] :=> (tUnit `fn` t)) area (Abs (Typed ([] :=> tUnit) area "_") [method])))
                  let methodExp'' = Typed (getQualType methodExp') (getArea methodExp') (Export methodExp')

                  monomorphized <-
                    monomorphize
                      target
                      env
                        { envSubstitution = s
                        , envCurrentModulePath = methodModulePath
                        , envLocalState = makeLocalMonomorphizationState ()
                        }
                      (removeParameterPlaceholdersAndUpdateName nameToUse methodExp'')
                  liftIO $ setRequestResult fnName methodModulePath typeItIsCalledWith monomorphized
                  addImport envCurrentModulePath methodModulePath monomorphicName typeForImport importType

                  return nameToUse

            _ ->
              return fnName


monomorphizeApp :: Target -> Env -> Exp -> Monomorphize (Exp, Bool)
monomorphizeApp target env@Env{ envSubstitution } exp = case exp of
  Typed qt area (App fn arg final) -> do
    arg' <- monomorphize target env arg
    (fn', wasPerformed)  <- monomorphizeApp target env fn
    return
      ( Typed (applyAndCleanQt envSubstitution qt) area (App fn' arg' final)
      , wasPerformed
      )

  Typed _ _ (Placeholder _ e) -> do
    monomorphizeApp target env e

  -- case of record field access
  Typed _ _ (Var ('.' : _) False) -> do
    -- TODO: update type
    return (exp, False)

  -- TODO: this should probably only happen for the JS backend?
  Typed qt area (Var "==" False) | target == TNode || target == TBrowser ->
    return (Typed qt area (Var "==" False), False)

  -- Constructors
  Typed qt area (Var ctorName True) -> do
    -- TODO: Handle case of constructors accessed via namespace
    foreignModulePath <- findCtorForeignModulePath (envCurrentModulePath env) ctorName
    ctor <- Rock.fetch $ ForeignConstructor foreignModulePath ctorName

    case ctor of
      Just (Untyped _ (Constructor _ _ t)) ->
        addImport (envCurrentModulePath env) foreignModulePath ctorName t ConstructorImport

      _ ->
        addImport (envCurrentModulePath env) foreignModulePath ctorName (getQualified qt) ConstructorImport


    return (Typed (apply envSubstitution qt) area (Var ctorName True), False)

  Typed qt area (Var fnName False) -> do
    monomorphicName <- monomorphizeDefinition target False env fnName (apply envSubstitution $ getQualified qt)

    return
      ( Typed (applyAndCleanQt envSubstitution qt) area (Var monomorphicName False)
      , monomorphicName /= fnName
      )

  e -> do
    e' <- monomorphize target env e
    return (e', False)


monomorphizeLocalAssignment :: Target -> Env -> Qual Type -> Area -> String -> Exp -> Monomorphize Exp
monomorphizeLocalAssignment target env qt area name exp = case exp of 
  Typed _ _ (Placeholder _ e) ->
    monomorphizeLocalAssignment target env qt area name e

  Typed _ _ Abs{} -> do
    localState <- liftIO $ readIORef $ envLocalState env
    let currentScopeState = last localState
    let withNewDefinition =
          currentScopeState
            { ssDefinitions = Map.insert name (Typed qt area (Assignment name exp)) (ssDefinitions currentScopeState) }
    let updatedLocalState = init localState <> [withNewDefinition]
    liftIO $ writeIORef (envLocalState env) updatedLocalState

    return exp

  _ ->
    monomorphize target env exp


monomorphizeBodyExp :: Target -> Env -> Exp -> Monomorphize Exp
monomorphizeBodyExp target env exp = case exp of
  Typed qt area (Assignment n e) -> do
    e' <- monomorphizeLocalAssignment target env qt area n e
    return $ Typed qt area (Assignment n e')

  or ->
    monomorphize target env or


pushNewScopeState :: Env -> Monomorphize ()
pushNewScopeState env = do
  localState <- liftIO $ readIORef $ envLocalState env
  let withNewScope = localState <> [ScopeState mempty mempty]
  liftIO $ writeIORef (envLocalState env) withNewScope

popScopeState :: Env -> Monomorphize ScopeState
popScopeState env = do
  localState <- liftIO $ readIORef $ envLocalState env
  let withPoppedScope = init localState
  let poppedState = last localState
  liftIO $ writeIORef (envLocalState env) withPoppedScope
  return poppedState


replaceLocalFunctions :: Map.Map FunctionId MonomorphizationRequest -> Exp -> [Exp]
replaceLocalFunctions requests exp = case getExpName exp of
  Just name ->
    let matchingRequests = Map.elems $ Map.filterWithKey (\fnId _ -> fiFunctionName fnId == name) requests
    in  case matchingRequests of
      [] ->
        [exp]

      reqs ->
        map (Maybe.fromMaybe undefined . mrResult) reqs

  Nothing ->
    [exp]


-- TODO: we need special handling for monomorphizing local functions
monomorphize :: Target -> Env -> Exp -> Monomorphize Exp
monomorphize target env@Env{ envSubstitution } exp = case exp of
  Typed _ _ (App (Typed _ _ (App (Typed _ _ (Var "==" _)) (Typed (_ :=> tArg) _ _) _)) _ _) -> do
    -- TODO: don't monomorphize == for these types "Integer", "Byte", "Float", "String", "Boolean", "Unit", "Char"
    -- Or not at all for JS target
    if target == TBrowser || target == TNode then
      return exp
    else do
      (monomorphized, _) <- monomorphizeApp target env exp
      return monomorphized

  Typed _ _ App{} -> do
    (monomorphized, _) <- monomorphizeApp target env exp
    return monomorphized

  Typed qt area (Export e) -> do
    e' <- monomorphize target env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Export e')

  Typed _ _ (TypedExp e _ _) -> do
    monomorphize target env e

  -- Typed qt area (Assignment n e) -> do
  --   e' <- if isFunctionType (apply envSubstitution $ getQualified qt) then do
  --           (monomorphized, _) <- monomorphizeApp env e
  --           return monomorphized
  --         else
  --           monomorphize env e
  --   return $ Typed (applyAndCleanQt envSubstitution qt) area (Assignment n e')
  Typed qt area (Assignment n e) -> do
    e' <- monomorphize target env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Assignment n e')

  -- Look for nullary method access
  Typed (_ :=> t) area (Var varName False) | not (isFunctionType t) -> do
    (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv (envCurrentModulePath env)
    let isMethod = Map.member varName (Slv.envMethods slvEnv)
    if isMethod then do
      monomorphic <- monomorphizeApp target env exp
      let (Typed _ _ monomorphicVar, _) = monomorphic
      return $ Typed ([] :=> t) area (App (Typed ([] :=> (tUnit `fn` t)) area monomorphicVar) (Typed ([] :=> tUnit) area LUnit) True)
    else do
      (m, _) <- monomorphizeApp target env exp
      return m

  -- Look for simple function names as args, record field, list item etc
  Typed _ _ (Var _ _) -> do
    (m, _) <- monomorphizeApp target env exp
    return m

  Typed qt area (Abs (Typed pQt pArea pName) es) -> do
    pushNewScopeState env
    es' <- mapM (monomorphizeBodyExp target env) es
    poppedScopeState <- popScopeState env
    let requestsFromScope = ssRequests poppedScopeState

    let es'' =
          if not (Map.null requestsFromScope) then
            es' >>= replaceLocalFunctions requestsFromScope
          else
            es'

    return $
      Typed (applyAndCleanQt envSubstitution qt) area
        (Abs (Typed (applyAndCleanQt envSubstitution pQt) pArea pName) es'')

  Typed qt area (Do es) -> do
    pushNewScopeState env
    es' <- mapM (monomorphizeBodyExp target env) es
    poppedScopeState <- popScopeState env
    let requestsFromScope = ssRequests poppedScopeState

    let es'' =
          if not (Map.null requestsFromScope) then
            es' >>= replaceLocalFunctions requestsFromScope
          else
            es'
    
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Do es'')

  -- TODO: maybe remove as this should probably not happen?
  -- BUT we possibly still need this for now in order to keep method calls for
  -- not yet monomorphizable methods and in this case we'd need to keep the
  -- Placeholder and not skip it like now.
  Typed qt area (Placeholder ref e) -> do
    monomorphize target env e
    -- case ref of
    --   (MethodRef "Eq" _ _, _) -> do
    --     return $ Typed qt area (Placeholder ref e)

    --   -- TODO: got to keep some here

    --   _ ->
    --     monomorphize target env e

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

  Typed qt area (Access rec field) -> do
    rec' <- monomorphize target env rec
    return $ Typed qt area (Access rec' field)

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
    foreignModulePath <- findCtorForeignModulePath (envCurrentModulePath env) n
    ctor <- Rock.fetch $ ForeignConstructor foreignModulePath n

    case ctor of
      Just (Untyped _ (Constructor _ _ t)) ->
        addImport (envCurrentModulePath env) foreignModulePath n t ConstructorImport

      _ -> do
        let argTypes = map getType args
        let fullType = foldr fn (getQualified qt) argTypes
        addImport (envCurrentModulePath env) foreignModulePath n fullType ConstructorImport

    args' <- mapM (monomorphizePattern target env) args

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


monomorphizeIs :: Target -> Env -> Is -> Monomorphize Is
monomorphizeIs target env is = case is of
  Typed qt area (Is pat exp) -> do
    exp' <- monomorphize target env exp
    pat' <- monomorphizePattern target env pat
    return $ Typed (applyAndCleanQt (envSubstitution env) qt) area (Is pat' exp')

  or ->
    return or


getMonomorphicFunctions :: String -> Map.Map FunctionId MonomorphizationRequest -> [Exp]
getMonomorphicFunctions fnName state =
  let monomorphicInstances =
        Map.filterWithKey
          (\id _ -> fiFunctionName id == fnName)
          state
  in  map (\(MonomorphizationRequest _ (Just e)) -> e) $ Map.elems monomorphicInstances


getMonomorphicFunctionNamesAndTypes :: String -> Map.Map FunctionId MonomorphizationRequest -> [(String, Type)]
getMonomorphicFunctionNamesAndTypes fnName state =
  let monomorphicInstances =
        Map.filterWithKey
          (\id _ -> fiFunctionName id == fnName)
          state
  in  map (\(FunctionId name _ t, MonomorphizationRequest index _) -> (buildMonomorphizedName name index, t)) $ Map.toList monomorphicInstances


replaceDefinitionWithMonomorphicOnes :: Map.Map FunctionId MonomorphizationRequest -> Exp -> [Exp]
replaceDefinitionWithMonomorphicOnes state exp =
  case getExpName exp of
    Just n ->
      getMonomorphicFunctions n state

    Nothing ->
      [exp]


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

mergeResult :: AST -> IO AST
mergeResult ast@AST{ apath = Just currentModulePath } = do
  state <- readIORef monomorphizationState
  let monomorphizedFunctionsForModule = filterMonomorphicFunctionsForModule state currentModulePath
  let methodExps = Set.toList $ Set.fromList $ ainstances ast >>= findMonomorphicMethods monomorphizedFunctionsForModule
  let newExps = aexps ast >>= replaceDefinitionWithMonomorphicOnes monomorphizedFunctionsForModule
  -- let allExps = newExps ++ methodExps
  let newExpsAtTheTop = filter (Maybe.isNothing . getExpName) newExps
  -- let newExpsToBeSorted = filter (\(i, _) -> i /= (-1)) allExps
  -- -- let sortedNewExps = map snd $ List.sortBy (\a b -> compare (fst b) (fst a)) newExpsToBeSorted
  -- let sortedNewExps = map snd $ methodExps ++ newExps
  let allExps = methodExps ++ newExps
  let dependencies = buildDependenciesForAllExps allExps
  let sortedNames = sortByDependencies dependencies
  let sortedExps =
        Maybe.mapMaybe
          (\n ->
            List.find (\e -> getExpName e == Just n) allExps
          )
          sortedNames

  allImports <- liftIO $ readIORef monomorphizationImports
  let importedNames = Map.toList $ Maybe.fromMaybe mempty $ Map.lookup currentModulePath allImports
  let generatedImports =
        map
          (\(foreignModulePath, names) ->
              let solvedNames = map (\(n, t, _) -> Typed ([] :=> t) emptyArea n) (Set.toList names)
              in  Untyped emptyArea (NamedImport solvedNames foreignModulePath foreignModulePath)
          )
          (filter (\(foreignPath, _) -> foreignPath /= currentModulePath) importedNames)


  return ast { aexps = newExpsAtTheTop ++ sortedExps, aimports = generatedImports, ainstances = [], ainterfaces = [] }
mergeResult ast =
  return ast


compareDependencies :: (String, [String]) -> (String, [String]) -> Ordering
compareDependencies entry1 entry2
  | fst entry1 `elem` snd entry2 = LT
--   | null (dependencies entry1) = LT
  | fst entry2 `elem` snd entry1 = GT
  | null (snd entry1) && not (null (snd entry2)) = LT
  | null (snd entry2) && not (null (snd entry1)) = GT
  | otherwise = EQ
  -- | otherwise = GT

sortByDependencies :: [(String, [String])] -> [String]
sortByDependencies entries = map fst $ List.sortBy compareDependencies entries


buildDependenciesForAllExps :: [Exp] -> [(String, [String])]
buildDependenciesForAllExps exps =
  let allLocalNames = Maybe.mapMaybe getExpName exps
  in  Maybe.mapMaybe (buildDependencies allLocalNames) exps

buildDependencies :: [String] -> Exp -> Maybe (String, [String])
buildDependencies localNames exp =
  (\name -> (name, buildDependencies' localNames name exp)) <$> getExpName exp

buildDependencies' :: [String] -> String -> Exp -> [String]
buildDependencies' localNames expName exp = case exp of
  Typed _ _ (Abs _ body) ->
    body >>= buildDependencies' localNames expName

  Typed _ _ (Do exps) ->
    exps >>= buildDependencies' localNames expName

  Typed _ _ (Assignment _ e) ->
    buildDependencies' localNames expName e

  Typed _ _ (Export e) ->
    buildDependencies' localNames expName e

  Typed _ _ (Placeholder _ e) ->
    buildDependencies' localNames expName e

  Typed _ _ (TypedExp e _ _) ->
    buildDependencies' localNames expName e

  Typed _ _ (Var n _) ->
    if n `elem` localNames then
      [n]
    else
      []

  Typed _ _ (Where e iss) ->
    buildDependencies' localNames expName e
    ++ (iss >>= (buildDependencies' localNames expName . getIsExpression))

  Typed _ _ (If cond truthy falsy) ->
    buildDependencies' localNames expName cond
    ++ buildDependencies' localNames expName truthy
    ++ buildDependencies' localNames expName falsy

  Typed _ _ (App fn arg _) ->
    buildDependencies' localNames expName fn
    ++ buildDependencies' localNames expName arg

  Typed _ _ (Record fields) ->
    concatMap
      (\case
        Typed _ _ (Field (_, exp)) ->
          buildDependencies' localNames expName exp

        Typed _ _ (FieldSpread exp) ->
          buildDependencies' localNames expName exp

        _ ->
          []
      )
      fields

  Typed _ _ (Access rec _) ->
    buildDependencies' localNames expName rec

  _ ->
    []
