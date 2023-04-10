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


findExpByName :: (Rock.MonadFetch Query m, MonadIO m) => FilePath -> String -> m (Maybe (Exp, FilePath))
findExpByName moduleWhereItsUsed expName = do
  maybeExp <- Rock.fetch $ ForeignExp moduleWhereItsUsed expName
  case maybeExp of
    Just found ->
      return $ Just (found, moduleWhereItsUsed)

    Nothing -> do
      (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
      -- liftIO $ putStrLn $ ppShow $ aimports ast
      case findForeignModuleForImportedName expName ast of
        Just foreignModulePath -> do
          found <- Rock.fetch $ ForeignExp foreignModulePath expName
          return $ (,foreignModulePath) <$> found

        _ ->
          return Nothing

type Monomorphize a = forall m . (Rock.MonadFetch Query m, MonadIO m) => m a

data Env
  = Env
  { envCurrentModulePath :: FilePath
  , envSubstitution :: Substitution
  , envLocalState :: IORef [ScopeState]
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
monomorphizeDefinition isMain env@Env{ envCurrentModulePath } fnName typeItIsCalledWith = do
  state <- liftIO $ readIORef monomorphizationState
  foundExp <- findExpByName envCurrentModulePath fnName

  liftIO $ putStrLn $ ppShow fnName
  liftIO $ putStrLn $ ppShow $ foundExp /= Nothing
  liftIO $ putStrLn ""

  case foundExp of
    Just (fnDefinition, fnModulePath) ->
      if isExtern fnDefinition then
        -- For now we skip externs completely
        return fnName
      else do
        let s = gentleUnify typeItIsCalledWith (getType fnDefinition)
        let fnId = FunctionId fnName fnModulePath typeItIsCalledWith

        case Map.lookup fnId state of
          Just MonomorphizationRequest {} ->
            liftIO $ makeMonomorphizedName fnName fnModulePath typeItIsCalledWith

          Nothing -> do
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

    Nothing ->
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
    liftIO $ putStrLn $ "MONO NAME: " ++ monomorphicName

    return
      ( Typed (applyAndCleanQt envSubstitution qt) area (Var monomorphicName False)
      , monomorphicName /= fnName
      )

  e -> do
    e' <- monomorphize env e
    return (e', False)


monomorphizeBodyExp :: Env -> Exp -> Monomorphize Exp
monomorphizeBodyExp env exp = case exp of
  Typed (_ :=> t) _ (Assignment n _) | isFunctionType t -> do
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
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Assignment n e')

  Typed qt area (Abs (Typed pQt pArea pName) es) -> do
    pushNewScopeState env
    es' <- mapM (monomorphizeBodyExp env) es
    poppedScopeState <- popScopeState env
    -- TODO: look for monomorphization requests in the poppedScopeState and if there
    -- is any, we need to go through es' again and introduce the monomorphic versions
    liftIO $ putStrLn $ ppShow poppedScopeState

    return $
      Typed (applyAndCleanQt envSubstitution qt) area
        (Abs (Typed (applyAndCleanQt envSubstitution pQt) pArea pName) es')

  -- TODO: maybe remove as this should probably not happen?
  Typed qt area (Placeholder (ref, ts) e) -> do
    e' <- monomorphize env e
    -- let newRef = case ref of
    --         ClassRef n preds call _ ->
    --           ClassRef n preds call False

    --         or ->
    --           or
    return $ Typed (applyAndCleanQt envSubstitution qt) area (Placeholder (ref, apply envSubstitution ts) e')

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


replaceDefinitionWithMonomorphicOnes ::  Map.Map FunctionId MonomorphizationRequest -> Exp -> [Exp]
replaceDefinitionWithMonomorphicOnes state exp =
  case getExpName exp of
    Just n ->
      case getMonomorphicFunctions n state of
        [] ->
          [exp]

        monomorphicOnes ->
          -- TODO: at some point remove the original function, but for now keep
          -- it
          if n == "main" then
            monomorphicOnes
          else
            exp : monomorphicOnes

    Nothing ->
      [exp]

replaceTypedNameWithMonomorphicOnes ::  Map.Map FunctionId MonomorphizationRequest -> Solved String -> [Solved String]
replaceTypedNameWithMonomorphicOnes _ (Typed _ _ _) = undefined
replaceTypedNameWithMonomorphicOnes state name@(Untyped area n) =
  let monomorphizedNamesAndTypes = getMonomorphicFunctionNamesAndTypes n state
      mapped = map (\(monoName, t) -> Untyped area monoName) monomorphizedNamesAndTypes
  in  if null mapped then
        [name]
      else
        name : mapped
  -- case getValue name of
  --   Just n ->
  --     case getMonomorphicFunctions n state of
  --       [] ->
  --         [exp]

  --       monomorphicOnes ->
  --         -- TODO: at some point remove the original function, but for now keep
  --         -- it
  --         exp : monomorphicOnes

  --   Nothing ->
  --     [exp]

filterMonomorphicFunctionsForModule :: Map.Map FunctionId MonomorphizationRequest -> FilePath -> Map.Map FunctionId MonomorphizationRequest
filterMonomorphicFunctionsForModule state modulePath =
  Map.filterWithKey
    (\id _ -> fiModulePath id == modulePath)
    state

mergeResult :: AST -> IO AST
mergeResult ast@AST{ apath = Just modulePath } = do
  state <- readIORef monomorphizationState
  let monomorphizedFunctionsForModule = filterMonomorphicFunctionsForModule state modulePath

  let newExps = aexps ast >>= replaceDefinitionWithMonomorphicOnes monomorphizedFunctionsForModule
  let newImports =
        map
          (\case
            Untyped area (NamedImport names rel abs) ->
              let monomorphizedFunctionsForImport = filterMonomorphicFunctionsForModule state abs
              in  Untyped area (NamedImport (names >>= replaceTypedNameWithMonomorphicOnes monomorphizedFunctionsForImport) rel abs)

            -- Untyped area (DefaultImport name rel abs) ->
            --   let monomorphizedFunctionsForImport = filterMonomorphicFunctionsForModule state abs
            --   in  undefined

            or ->
              or
          )
          (aimports ast)
  
  return ast { aexps = newExps, aimports = newImports }
mergeResult ast =
  return ast
