{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE LambdaCase #-}
module           Optimize.HigherOrderCopyPropagation where
import qualified Rock
import           Driver.Query
import           AST.Core
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Infer.Type
import qualified Data.Maybe                                   as Maybe
import           Text.Show.Pretty
import           Control.Monad (zipWithM, when)
import qualified Data.Map                                     as Map
import           Control.Monad.State
import           Utils.Hash (generateHashFromPath)
import qualified Data.Set as Set
import qualified Data.List as List


{-
Optimizes this pattern:
  map = (f, list) => where(list) {
    [a, ...xs] =>
      [f(a), ...map(f, xs)]

    [] =>
      []
  }

  map(inc, [1, 2, 3])

to this:
  map_opt = (list) => where(list) {
    [a, ...xs] =>
      [inc(a), ...map_opt(xs)]

    [] =>
      []
  }

  map_opt([1, 2, 3])

This can only be done if inc is a known function, that is, that it is a globally
available function.

But also this case:
  weirdMap :: (a -> a) -> List a -> List a
  weirdMap f (x:xs) = f x : weirdMap (f . f) xs
  weirdMap _ [] = []

  weirdMap (+ 1) [0, 0, 0, 0]
  -- -> [1, 2, 4, 8]

can't be optimized either, so if it is recursive, the optimized parameter needs to be

-}

data PropagationState = PropagationState Int [Exp] (Map.Map FilePath (Set.Set String))

type Propagate a = forall m . (Rock.MonadFetch Query m, MonadIO m, MonadState PropagationState m) => m a


findExpByName :: (Rock.MonadFetch Query m) => FilePath -> String -> m (Maybe (Exp, FilePath))
findExpByName moduleWhereItsUsed expName = do
  maybeExp <- Rock.fetch $ ForeignCoreExp moduleWhereItsUsed expName
  case maybeExp of
    Just found ->
      return $ Just (found, moduleWhereItsUsed)

    Nothing -> do
      ast <- Rock.fetch $ CoreAST moduleWhereItsUsed
      case findForeignModuleForImportedName expName ast of
        Just foreignModulePath -> do
          found <- Rock.fetch $ ForeignCoreExp foreignModulePath expName

          case found of
            Nothing ->
              findExpByName foreignModulePath expName

            Just exp ->
              return $ Just (exp, foreignModulePath)

        _ ->
          return Nothing


findGlobalAccessesInBody :: Set.Set String -> [Exp] -> Set.Set String
findGlobalAccessesInBody namesInScope body = case body of
  assignment@(Typed _ _ _ (Assignment n _)) : next ->
    findGlobalAccesses namesInScope assignment <>
    findGlobalAccessesInBody (namesInScope <> Set.singleton n) next

  or : next ->
    findGlobalAccesses namesInScope or <>
    findGlobalAccessesInBody namesInScope next

  [] ->
    Set.empty


findGlobalAccesses :: Set.Set String -> Exp -> Set.Set String
findGlobalAccesses namesInScope exp = case exp of
  Typed _ _ _ (Assignment n e) ->
    findGlobalAccesses (namesInScope <> Set.singleton n) e

  Typed _ _ _ (Definition params body) ->
    let namesInScope' = namesInScope <> Set.fromList (map getValue params)
    in  findGlobalAccessesInBody namesInScope' body

  Typed _ _ _ (Do exps) ->
    findGlobalAccessesInBody namesInScope exps

  Typed _ _ _ (Var n _) ->
    if n `Set.member` namesInScope then
      Set.empty
    else
      Set.singleton n

  Typed _ _ _ (Call fn args) ->
    findGlobalAccesses namesInScope fn <>
    mconcat (map (findGlobalAccesses namesInScope) args)

  Typed _ _ _ (Access rec _) ->
    findGlobalAccesses namesInScope rec

  Typed _ _ _ (Export e) ->
    findGlobalAccesses namesInScope e

  Typed _ _ _ (TupleConstructor items) ->
    mconcat (map (findGlobalAccesses namesInScope) items)

  Typed _ _ _ (ListConstructor items) ->
    mconcat $ map
      (\case
        Typed _ _ _ (ListItem e) ->
          findGlobalAccesses namesInScope e

        Typed _ _ _ (ListSpread e) ->
          findGlobalAccesses namesInScope e

        _ ->
          Set.empty
      )
      items

  Typed _ _ _ (Record fields) ->
    mconcat $ map
      (\case
        Typed _ _ _ (Field (_, e)) ->
          findGlobalAccesses namesInScope e

        Typed _ _ _ (FieldSpread e) ->
          findGlobalAccesses namesInScope e

        _ ->
          Set.empty
      )
      fields

  Typed _ _ _ (Where e iss) ->
    findGlobalAccesses namesInScope e <>
    mconcat
      (
        map
          (\(Typed _ _ _ (Is pat e)) ->
            findGlobalAccesses (namesInScope <> Set.fromList (getPatternVars pat)) e
          )
          iss
      )

  Typed _ _ _ (If cond truthy falsy) ->
    findGlobalAccesses namesInScope cond <>
    findGlobalAccesses namesInScope truthy <>
    findGlobalAccesses namesInScope falsy

  _ ->
    Set.empty



-- TODO:
-- - can be used in recursion, but it should then be exactly itself
-- - 
isArgEligible :: FilePath -> Exp -> Propagate Bool
isArgEligible path exp = case exp of
  Typed (_ :=> t) _ _ (Var name _) | isFunctionType t -> do
    foundExp <- findExpByName path name
    return $ Maybe.isJust foundExp

  _ ->
    return False


-- TODO: in case of recursion ( like map ), we also need to drop the arg in the recursive call:
--   map(f, xs) -> map(xs)
--   to do this we need to take the function name as a parameter so that we can transform:
--     Call (Var "map") [Var f, arg]
--   ->
--     Call (Var "map") [arg]
propagateBody :: String -> String -> Qual Type -> Map.Map String Exp -> Exp -> Exp
propagateBody fnName newFnName newQualType propagateMap bodyExp = case bodyExp of
  Typed qt area metadata (Var n isCtor) ->
    case Map.lookup n propagateMap of
      Just replacement ->
        replacement

      _ ->
        -- if n == newFnName then
        Typed qt area metadata (Var n isCtor)

  Typed qt area metadata (Assignment n e) ->
    let e' = propagateBody fnName newFnName newQualType propagateMap e
    in  Typed qt area metadata (Assignment n e')

  Typed qt area metadata (Call (Typed _ area' metadata' (Var calledFn ctor)) args) | calledFn == fnName ->
    let mapKeys = Map.keys propagateMap
        filteredArgs =
          filter
            (\case
              Typed _ _ _ (Var n _) ->
                n `notElem` mapKeys

              _ ->
                True
            )
            args
        args' = map (propagateBody fnName newFnName newQualType propagateMap) filteredArgs
    in  Typed qt area metadata (Call (Typed newQualType area' metadata' (Var newFnName ctor)) args')

  Typed qt area metadata (Call f args) ->
    let f' = propagateBody fnName newFnName newQualType propagateMap f
        args' = map (propagateBody fnName newFnName newQualType propagateMap) args
    in  Typed qt area metadata (Call f' args')

  Typed qt area metadata (Definition params body) ->
    let body' = map (propagateBody fnName newFnName newQualType propagateMap) body
    in  Typed qt area metadata (Definition params body')

  Typed qt area metadata (Do exps) ->
    let exps' = map (propagateBody fnName newFnName newQualType propagateMap) exps
    in  Typed qt area metadata (Do exps')

  Typed qt area metadata (If cond truthy falsy) ->
    let cond' = propagateBody fnName newFnName newQualType propagateMap cond
        truthy' = propagateBody fnName newFnName newQualType propagateMap truthy
        falsy' = propagateBody fnName newFnName newQualType propagateMap falsy
    in  Typed qt area metadata (If cond' truthy' falsy')

  Typed qt area metadata (Access rec field) ->
    let rec' = propagateBody fnName newFnName newQualType propagateMap rec
        field' = propagateBody fnName newFnName newQualType propagateMap field
    in  Typed qt area metadata (Access rec' field')

  Typed qt area metadata (ListConstructor items) ->
    let items' = map (mapListItem $ propagateBody fnName newFnName newQualType propagateMap) items
    in  Typed qt area metadata (ListConstructor items')

  Typed qt area metadata (Record fields) ->
    let fields' = map (mapRecordField $ propagateBody fnName newFnName newQualType propagateMap) fields
    in  Typed qt area metadata (Record fields')

  Typed qt area metadata (Where e iss) ->
    let e' = propagateBody fnName newFnName newQualType propagateMap e
        iss' = map (mapIs $ propagateBody fnName newFnName newQualType propagateMap) iss
    in  Typed qt area metadata (Where e' iss')

  or ->
    or


propagateDefinition :: FilePath -> String -> Qual Type -> [(Bool, Exp)] -> Exp -> Propagate (Maybe Exp)
propagateDefinition path newName newQualType args exp = case exp of
  Typed _ area metadata (Assignment fnName (Typed _ area' metadata' (Definition params body))) | length params == length args -> do
      let propagateMap = Map.fromList $ concat $
            zipWith
              (\(isRemoved, arg) param -> do
                if isRemoved then
                  [(getValue param, arg)]
                else
                  []
              )
              args
              params
      let body' = map (propagateBody fnName newName newQualType propagateMap) body
      let params' = map snd $ filter (\((throwAway, _), _) -> not throwAway) (zip args params)

      return $ Just $ Typed newQualType area metadata (Assignment newName (Typed newQualType area' metadata' (Definition params' body')))

  Typed _ area metadata (Export e) -> do
    e' <- propagateDefinition path newName newQualType args e
    case e' of
      Just e'' ->
        return $ Just $ Typed (getQualType e'') area metadata (Export e'')

      _ ->
        return Nothing

  _ ->
    return Nothing


applyPropagation :: FilePath -> String -> String -> Qual Type -> [(Bool, Exp)] -> Propagate (Maybe Exp)
applyPropagation path fnName newName newQualType args = do
  foreignFunction <- findExpByName path fnName
  case foreignFunction of
    Just (fnDefinition, foreignModule) -> do
      propagated <- propagateDefinition path newName newQualType args fnDefinition
      case propagated of
        Just good -> do
          when (path /= foreignModule) $ do
            let globalAccesses = findGlobalAccesses Set.empty fnDefinition
            pushGlobalAccesses foreignModule globalAccesses
          return $ Just good

        Nothing ->
          return Nothing

    _ ->
      return Nothing


propagate :: FilePath -> Exp -> Propagate Exp
propagate path exp = case exp of
  Typed qt area metadata (Call (Typed (_ :=> t) varArea varMetadata (Var fnName False)) args) -> do
    result <- mapM (isArgEligible path) args
    if or result then do
      newFnName <- makeNewName path fnName
      let newParamTypes = map snd $ filter (\(eligible, _) -> not eligible) (zip result (getParamTypes t))
      let newQualType = [] :=> foldr fn (getReturnType t) newParamTypes
      let newArgs = map snd $ filter (\(eligible, _) -> not eligible) (zip result args)
      applied <- applyPropagation path fnName newFnName newQualType (zip result args)

      case applied of
        Just good -> do
          pushPropagatedExp good
          return $ Typed qt area metadata (Call (Typed newQualType varArea varMetadata (Var newFnName False)) newArgs)

        _ ->
          return exp
    else
      return exp

  Typed qt area metadata (Assignment n e) -> do
    e' <- propagate path e
    return $ Typed qt area metadata (Assignment n e')

  Typed qt area metadata (Definition params body) -> do
    body' <- mapM (propagate path) body
    return $ Typed qt area metadata (Definition params body')

  or ->
    return or


makeNewName :: FilePath -> String -> Propagate String
makeNewName modulePath fnName = do
  PropagationState index propagated accesses <- get
  put $ PropagationState (index + 1) propagated accesses
  let moduleHash = generateHashFromPath modulePath
  return $ fnName <> "_" <> moduleHash <> "_" <> show index


pushPropagatedExp :: Exp -> Propagate ()
pushPropagatedExp exp = do
  PropagationState index propagated accesses <- get
  put $ PropagationState index (exp : propagated) accesses


pushGlobalAccesses :: FilePath -> Set.Set String -> Propagate ()
pushGlobalAccesses modulePath names = do
  PropagationState index propagated accesses <- get
  let accesses' = Map.insertWith (<>) modulePath names accesses
  put $ PropagationState index propagated accesses'



propagateTopLevelExp :: FilePath -> Exp -> Propagate [Exp]
propagateTopLevelExp path e = do
  e' <- propagate path e
  PropagationState index propagated accesses <- get
  put $ PropagationState index [] accesses
  return $ propagated ++ [e']


findImportPathForNameInForeignAST :: String -> AST -> FilePath
findImportPathForNameInForeignAST name ast = case List.find ((== Just name) . getExpName) (aexps ast) of
  Just _ ->
    Maybe.fromMaybe "" $ apath ast

  Nothing ->
    case List.find (\(Untyped _ _ (NamedImport names _ _)) -> any ((== name) . getImportName) names) (aimports ast) of
      Just imp ->
        getImportAbsolutePath imp

      _ ->
        ""




moduleAccessesToImports :: (FilePath, Set.Set String) -> Propagate [Import]
moduleAccessesToImports (modulePath, namesNeeded) = do
  originalAST <- Rock.fetch $ CoreAST modulePath
  let x = Set.map (\n -> (n, findImportPathForNameInForeignAST n originalAST)) namesNeeded
  undefined


accessesToImports :: [(FilePath, Set.Set String)] -> Propagate [Import]
accessesToImports accesses =
  undefined


updateImports :: AST -> Propagate AST
updateImports ast = do
  PropagationState _ _ accesses <- get
  undefined


propagateAST :: AST -> Propagate AST
propagateAST ast = do
  propagatedExps <-
    mapM
      (propagateTopLevelExp (Maybe.fromMaybe "" $ apath ast))
      (aexps ast)
  return ast { aexps = concat propagatedExps }
