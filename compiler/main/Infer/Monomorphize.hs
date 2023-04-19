{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Infer.Monomorphize where

import qualified Data.Map                       as Map
import           Data.IORef
import           Infer.MonomorphizationState
import           AST.Solved
import qualified Rock
import           Driver.Query
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Text.Show.Pretty
import           Infer.Unify (gentleUnify)
import           Infer.Type
import           Infer.Substitute
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Explain.Location (emptyArea)
import qualified Infer.Env as Slv

-- TODO: consider if monomorphizing local functions would have an impact
-- with regards to mutations
-- it does and we need to add value restriction

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

-- findMethodByNameAndTypeInImports :: (Rock.MonadFetch Query m, MonadIO m) => [FilePath] -> String -> Type -> m (Maybe (Exp, FilePath))
-- findMethodByNameAndTypeInImports importPaths methodName typeItsCalledWith = case importPaths of
--   [] ->
--     return Nothing

--   (modulePath : nextModulePaths) -> do
--     found <- findMethodByNameAndType modulePath methodName typeItsCalledWith
--     case found of
--       Just method ->
--         return $ Just method

--       Nothing ->
--         findMethodByNameAndTypeInImports nextModulePaths methodName typeItsCalledWith

-- findMethodByNameAndType :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> Type -> m (Maybe (Exp, FilePath))
-- findMethodByNameAndType moduleWhereItsUsed methodName typeItsCalledWith = do
--   maybeExp <- Rock.fetch $ ForeignMethod moduleWhereItsUsed methodName typeItsCalledWith
--   case maybeExp of
--     Just found ->
--       return $ Just (found, moduleWhereItsUsed)

--     Nothing -> do
--       (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
--       let imports = map getImportAbsolutePath (aimports ast)
--       findMethodByNameAndTypeInImports imports methodName typeItsCalledWith

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


monomorphizeDefinition :: Bool -> Env -> String -> Type -> Monomorphize String
monomorphizeDefinition isMain env@Env{ envCurrentModulePath, envLocalState } fnName typeItIsCalledWith = do
  liftIO $ putStrLn $ "current def: " <> fnName
  -- first we look in the local namespace
  localState <- liftIO $ readIORef envLocalState
  let flippedScopes = reverse localState
  let foundScopeIndex = List.findIndex (\ScopeState { ssDefinitions } -> Map.member fnName ssDefinitions) flippedScopes
  case foundScopeIndex of
    Just index -> do
      let ScopeState { ssRequests, ssDefinitions } = flippedScopes!!index
      let (Just fnDefinition) = Map.lookup fnName ssDefinitions
      let s = gentleUnify typeItIsCalledWith (getType fnDefinition)
      let fnId = FunctionId fnName envCurrentModulePath typeItIsCalledWith
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
      foundExp <- findExpByName envCurrentModulePath fnName

      case foundExp of
        Just (fnDefinition, fnModulePath) ->
          if isExtern fnDefinition then do
            -- For now we skip externs completely
            -- let fnId = FunctionId fnName fnModulePath (getType fnDefinition)
            liftIO $ setRequestResult fnName fnModulePath (getType fnDefinition) fnDefinition
            return fnName
          else do
            let fnId = FunctionId fnName fnModulePath typeItIsCalledWith

            case Map.lookup fnId state of
              Just MonomorphizationRequest { mrIndex } ->
                return $ buildMonomorphizedName fnName mrIndex

              Nothing -> do
                let s = gentleUnify typeItIsCalledWith (getType fnDefinition)
                monomorphicName <- liftIO $ newRequest fnName fnModulePath typeItIsCalledWith
                let nameToUse =
                      if isMain then
                        fnName
                      else
                        monomorphicName

                monomorphized <-
                  monomorphize
                    env
                      { envSubstitution = s
                      , envCurrentModulePath = fnModulePath
                      , envLocalState = makeLocalMonomorphizationState ()
                      }
                    (removeParameterPlaceholdersAndUpdateName nameToUse fnDefinition)
                liftIO $ setRequestResult fnName fnModulePath typeItIsCalledWith monomorphized

                return nameToUse

        Nothing -> do
          -- Try for methods:
          (_, slvEnv) <- Rock.fetch $ SolvedASTWithEnv envCurrentModulePath


          foundMethod <-
            if Map.member fnName (Slv.envMethods slvEnv) then
              Rock.fetch $ SolvedMethodNode fnName typeItIsCalledWith
            else
              return Nothing
          -- let foundMethod = Nothing

          -- foundMethod <- findMethodByNameAndType (envEntrypointPath env) fnName typeItIsCalledWith
          -- foundMethod <- findMethodByNameAndType envCurrentModulePath fnName typeItIsCalledWith
          -- foundInterfaceModule <- findModulePathForInterfaceByMethodName envCurrentModulePath fnName

          case foundMethod of
            Just (methodExp@(Typed (ps :=> t) area (Assignment n method)), methodModulePath) -> do
              liftIO $ putStrLn $ "found and will monomorphize method: " <> fnName
              let fnId = FunctionId fnName methodModulePath typeItIsCalledWith

              case Map.lookup fnId state of
                Just MonomorphizationRequest { mrIndex } -> do
                  let monomorphicName = buildMonomorphizedName fnName mrIndex

                  -- Add import
                  imports <- liftIO $ readIORef monomorphizationImports
                  let importsForCurrentModule = Maybe.fromMaybe mempty $ Map.lookup envCurrentModulePath imports
                  let withNewImport = Map.insertWith (<>) methodModulePath (Set.singleton monomorphicName) importsForCurrentModule
                  let updatedImports = Map.insert envCurrentModulePath withNewImport imports
                  liftIO $ writeIORef monomorphizationImports updatedImports

                  return monomorphicName

                Nothing -> do
                  let s = gentleUnify typeItIsCalledWith (getType methodExp)
                  monomorphicName <- liftIO $ newRequest fnName methodModulePath typeItIsCalledWith

                  -- -- Add import
                  imports <- liftIO $ readIORef monomorphizationImports
                  let importsForCurrentModule = Maybe.fromMaybe mempty $ Map.lookup envCurrentModulePath imports
                  let withNewImport = Map.insertWith (<>) methodModulePath (Set.singleton monomorphicName) importsForCurrentModule
                  let updatedImports = Map.insert envCurrentModulePath withNewImport imports
                  liftIO $ writeIORef monomorphizationImports updatedImports

                  let nameToUse =
                        if isMain then
                          fnName
                        else
                          monomorphicName

                  let methodExp' =
                        if isFunctionType t then
                          methodExp
                        else
                          Typed (ps :=> t) area (Assignment n (Typed ([] :=> (tUnit `fn` t)) area (Abs (Typed ([] :=> tUnit) area "_") [method])))
                  let methodExp'' = Typed (getQualType methodExp') (getArea methodExp') (Export methodExp')

                  monomorphized <-
                    monomorphize
                      env
                        { envSubstitution = s
                        , envCurrentModulePath = methodModulePath
                        , envLocalState = makeLocalMonomorphizationState ()
                        }
                      (removeParameterPlaceholdersAndUpdateName nameToUse methodExp'')
                  liftIO $ setRequestResult fnName methodModulePath typeItIsCalledWith monomorphized

                  return nameToUse

            _ ->
              return fnName


monomorphizeApp :: Env -> Exp -> Monomorphize (Exp, Bool)
monomorphizeApp env@Env{ envSubstitution } exp = case exp of
  Typed qt area (App fn arg final) -> do
    arg' <- monomorphize env arg
    (fn', wasPerformed)  <- monomorphizeApp env fn
    return
      ( Typed (applyAndCleanQt envSubstitution qt) area (App fn' arg' final)
      , wasPerformed
      )

  Typed qt area (Placeholder (ref, ts) e) -> do
    (e', wasPerformed) <- monomorphizeApp env e
    if wasPerformed then
      return (e', True)
    else do
      let newRef = case ref of
            ClassRef n preds call _ ->
              ClassRef n preds call False

            MethodRef cls mtd _ ->
              MethodRef cls mtd False
      return (Typed (applyAndCleanQt envSubstitution qt) area (Placeholder (newRef, apply envSubstitution ts) e'), False)

  Typed qt area (Var fnName False) -> do
    -- TODO: handle case of Namespace.fnName?
    monomorphicName <- monomorphizeDefinition False env fnName (apply envSubstitution $ getQualified qt)
    -- liftIO $ putStrLn $ "MONO NAME: " ++ monomorphicName
    -- liftIO $ putStrLn $ "envSubstitution: " ++ ppShow envSubstitution

    return
      ( Typed (applyAndCleanQt envSubstitution qt) area (Var monomorphicName False)
      , monomorphicName /= fnName
      )

  e -> do
    e' <- monomorphize env e
    return (e', False)


monomorphizeBodyExp :: Env -> Exp -> Monomorphize Exp
monomorphizeBodyExp env exp = case exp of
  Typed _ _ (Assignment n (Typed _ _ Abs{})) -> do
    localState <- liftIO $ readIORef $ envLocalState env
    let currentScopeState = last localState
    let withNewDefinition =
          currentScopeState
            { ssDefinitions = Map.insert n exp (ssDefinitions currentScopeState) }
    let updatedLocalState = init localState <> [withNewDefinition]
    liftIO $ writeIORef (envLocalState env) updatedLocalState

    -- Do we need to monomorphize this at this point? It should probably be done at the time
    -- we find a request.
    -- monomorphize env exp
    return exp

  or ->
    monomorphize env or


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
        []
        -- [exp]

      reqs ->
        map (Maybe.fromMaybe undefined . mrResult) reqs

  Nothing ->
    [exp]


-- TODO: we need special handling for monomorphizing local functions
monomorphize :: Env -> Exp -> Monomorphize Exp
monomorphize env@Env{ envSubstitution } exp = case exp of
  Typed _ _ App{} -> do
    (monomorphized, _) <- monomorphizeApp env exp
    return monomorphized

  Typed qt area (Export e) -> do
    e' <- monomorphize env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Export e')

  Typed _ _ (TypedExp e _ _) -> do
    monomorphize env e

  Typed qt area (Assignment n e) -> do
    e' <- if isFunctionType (apply envSubstitution $ getQualified qt) then do
            (monomorphized, _) <- monomorphizeApp env e
            return monomorphized
          else
            monomorphize env e
    -- e' <- monomorphize env e
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Assignment n e')

  -- Look for nullary method access
  Typed (_ :=> t) area (Var varName False) | not (isFunctionType t) -> do
    -- found <- findMethodByNameAndType (envCurrentModulePath env) varName t
    -- found <- findMethodByNameAndType (envEntrypointPath env) varName t

    -- TODO: this is terribly slow right now and we need to figure out when we should actually call it.
    -- Possibly we need to mark vars that are methods during inference.
    -- found <- Rock.fetch $ SolvedMethodNode varName t
    let found = Nothing
    case found of
      Just _ -> do
        monomorphic <- monomorphizeApp env exp
        let (Typed _ _ monomorphicVar, _) = monomorphic
        return $ Typed ([] :=> t) area (App (Typed ([] :=> (tUnit `fn` t)) area monomorphicVar) (Typed ([] :=> tUnit) area LUnit) True)

      _ -> do
        (m, _) <- monomorphizeApp env exp
        return m
        -- return exp
    -- return exp

  Typed qt area (Abs (Typed pQt pArea pName) es) -> do
    pushNewScopeState env
    es' <- mapM (monomorphizeBodyExp env) es
    poppedScopeState <- popScopeState env
    let requestsFromScope = ssRequests poppedScopeState

    let es'' =
          if not (Map.null requestsFromScope) then
            es' >>= replaceLocalFunctions requestsFromScope
          else
            es'
    -- TODO: look for monomorphization requests in the poppedScopeState and if there
    -- is any, we need to go through es' again and introduce the monomorphic versions
    -- liftIO $ putStrLn $ ppShow poppedScopeState

    return $
      Typed (applyAndCleanQt envSubstitution qt) area
        (Abs (Typed (applyAndCleanQt envSubstitution pQt) pArea pName) es'')

  -- TODO: maybe remove as this should probably not happen?
  Typed qt area (Placeholder (ref, ts) e) -> do
    -- e' <- monomorphize env e
    -- return $ Typed (applyAndCleanQt envSubstitution qt) area (Placeholder (ref, apply envSubstitution ts) e')
    monomorphize env e

  Typed qt area e ->
    return $ Typed (applyAndCleanQt envSubstitution qt) area e

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
      case getMonomorphicFunctions n state of
        [] ->
          []
          -- [exp]

        monomorphicOnes ->
          -- TODO: at some point remove the original function, but for now keep
          -- it
          if n == "main" then
            monomorphicOnes
          else
            monomorphicOnes
            -- exp : monomorphicOnes

    Nothing ->
      [exp]


-- findMonomorphicMethods :: Map.Map FunctionId MonomorphizationRequest -> Interface -> [Exp]
-- findMonomorphicMethods state interface = case interface of
--   Untyped _ (Interface _ _ _ methods _) ->
--     let methodNames = Map.keys methods
--         monomorphicMethods = concatMap (`getMonomorphicFunctions` state) methodNames
--     in  monomorphicMethods

--   _ ->
--     []
findMonomorphicMethods :: Map.Map FunctionId MonomorphizationRequest -> Instance -> [Exp]
findMonomorphicMethods state interface = case interface of
  Untyped _ (Instance _ _ _ methods) ->
    let methodNames = Map.keys methods
        monomorphicMethods = concatMap (`getMonomorphicFunctions` state) methodNames
    in  monomorphicMethods

  _ ->
    []


replaceTypedNameWithMonomorphicOnes ::  Map.Map FunctionId MonomorphizationRequest -> Solved String -> [Solved String]
replaceTypedNameWithMonomorphicOnes _ Typed{} = undefined
replaceTypedNameWithMonomorphicOnes state name@(Untyped area n) =
  let monomorphizedNamesAndTypes = getMonomorphicFunctionNamesAndTypes n state
      mapped = map (\(monoName, _) -> Untyped area monoName) monomorphizedNamesAndTypes
  in  mapped
  -- in  if null mapped then
  --       [name]
  --     else
  --       name : mapped


filterMonomorphicFunctionsForModule :: Map.Map FunctionId MonomorphizationRequest -> FilePath -> Map.Map FunctionId MonomorphizationRequest
filterMonomorphicFunctionsForModule state modulePath =
  Map.filterWithKey
    (\id _ -> fiModulePath id == modulePath)
    state

mergeResult :: AST -> IO AST
mergeResult ast@AST{ apath = Just currentModulePath } = do
  state <- readIORef monomorphizationState
  let monomorphizedFunctionsForModule = filterMonomorphicFunctionsForModule state currentModulePath
  -- let methodExps = ainterfaces ast >>= findMonomorphicMethods monomorphizedFunctionsForModule
  let methodExps = Set.toList $ Set.fromList $ ainstances ast >>= findMonomorphicMethods monomorphizedFunctionsForModule
  let newExps = aexps ast >>= replaceDefinitionWithMonomorphicOnes monomorphizedFunctionsForModule
  let newImports =
        map
          (\case
            Untyped area (NamedImport names rel abs) ->
              let monomorphizedFunctionsForImport = filterMonomorphicFunctionsForModule state abs
              in  Untyped area (NamedImport (names >>= replaceTypedNameWithMonomorphicOnes monomorphizedFunctionsForImport) rel abs)

            or ->
              or
          )
          (aimports ast)
  
  allMethodImports <- liftIO $ readIORef monomorphizationImports
  let importedMethods = Map.toList $ Maybe.fromMaybe mempty $ Map.lookup currentModulePath allMethodImports
  let importsForMethods =
        map
          (\(foreignModulePath, names) ->
              let solvedNames = map (Untyped emptyArea) (Set.toList names)
              in  Untyped emptyArea (NamedImport solvedNames foreignModulePath foreignModulePath)
          )
          (filter (\(foreignPath, _) -> foreignPath /= currentModulePath) importedMethods)


  return ast { aexps = methodExps ++ newExps, aimports = newImports ++ importsForMethods, ainstances = [], ainterfaces = [] }
mergeResult ast =
  return ast
