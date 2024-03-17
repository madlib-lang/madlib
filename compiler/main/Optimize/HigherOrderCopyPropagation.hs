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
import Explain.Location (emptyArea)


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
            <> Set.fromList (getPatternConstructorNames pat)
          )
          iss
      )

  Typed _ _ _ (If cond truthy falsy) ->
    findGlobalAccesses namesInScope cond <>
    findGlobalAccesses namesInScope truthy <>
    findGlobalAccesses namesInScope falsy

  Typed _ _ _ (While cond body) ->
    findGlobalAccesses namesInScope cond <>
    findGlobalAccesses namesInScope body

  _ ->
    Set.empty



-- TODO:
-- - can be used in recursion, but it should then be exactly itself
--     so the following case should not be eligible:
--        weirdMap :: (a -> a) -> List a -> List a
--        weirdMap f (x:xs) = f x : weirdMap (f . f) xs
--        weirdMap _ [] = []

--        weirdMap (+ 1) [0, 0, 0, 0]
isArgEligible :: FilePath -> Exp -> Propagate Bool
isArgEligible path exp = case exp of
  Typed (_ :=> t) _ _ (Var name _) | isFunctionType t -> do
    foundExp <- findExpByName path name
    return $ Maybe.isJust foundExp

  _ ->
    return False


-- In case of recursion ( like map ), we also need to drop the arg in the recursive call:
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
        Typed qt area metadata (Var n isCtor)

  Typed _ area metadata (Assignment n e) ->
    let e' = propagateBody fnName newFnName newQualType propagateMap e
    in  Typed (getQualType e') area metadata (Assignment n e')

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

  Typed _ area metadata (Do exps) ->
    let exps' = map (propagateBody fnName newFnName newQualType propagateMap) exps
        newQt = getQualType $ last exps
    in  Typed newQt area metadata (Do exps')

  Typed qt area metadata (If cond truthy falsy) ->
    let cond' = propagateBody fnName newFnName newQualType propagateMap cond
        truthy' = propagateBody fnName newFnName newQualType propagateMap truthy
        falsy' = propagateBody fnName newFnName newQualType propagateMap falsy
    in  Typed qt area metadata (If cond' truthy' falsy')

  Typed qt area metadata (While cond body) ->
    let cond' = propagateBody fnName newFnName newQualType propagateMap cond
        body' = propagateBody fnName newFnName newQualType propagateMap body
    in  Typed qt area metadata (While cond' body')

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
  Typed qt area metadata (Call (Typed (ps :=> t) varArea varMetadata (Var fnName False)) args) -> do
    result <- mapM (isArgEligible path) args
    if or result then do
      newFnName <- makeNewName path fnName
      let newParamTypes = map snd $ filter (\(eligible, _) -> not eligible) (zip result (getParamTypes t))
      let newQualType = [] :=> foldr fn (getReturnType t) newParamTypes
      let newArgs = map snd $ filter (\(eligible, _) -> not eligible) (zip result args)
      applied <- applyPropagation path fnName newFnName newQualType (zip result args)

      case applied of
        Just good -> do
          good' <- propagate path good
          pushPropagatedExp good'
          newArgs' <- mapM (propagate path) newArgs
          return $ Typed qt area metadata (Call (Typed newQualType varArea varMetadata (Var newFnName False)) newArgs')

        _ -> do
          args' <- mapM (propagate path) args
          return $ Typed qt area metadata (Call (Typed (ps :=> t) varArea varMetadata (Var fnName False)) args')

    else do
      args' <- mapM (propagate path) args
      return $ Typed qt area metadata (Call (Typed (ps :=> t) varArea varMetadata (Var fnName False)) args')

  Typed qt area metadata (Call fn args) -> do
    args' <- mapM (propagate path) args
    return $ Typed qt area metadata (Call fn args')

  Typed _ area metadata (Assignment n e) -> do
    e' <- propagate path e
    return $ Typed (getQualType e') area metadata (Assignment n e')

  Typed qt area metadata (Definition params body) -> do
    body' <- mapM (propagate path) body
    return $ Typed qt area metadata (Definition params body')

  Typed _ area metadata (Do exps) -> do
    exps' <- mapM (propagate path) exps
    let newQt = getQualType $ last exps'
    return $ Typed newQt area metadata (Do exps')

  Typed _ area metadata (Export e) -> do
    e' <- propagate path e
    return $ Typed (getQualType e') area metadata (Export e')

  Typed qt area metadata (TupleConstructor items) -> do
    items' <- mapM (propagate path) items
    return $ Typed qt area metadata (TupleConstructor items')

  Typed qt area metadata (ListConstructor items) -> do
    items' <-
      mapM
        (\case
          Typed qt' area' metadata' (ListItem e) -> do
            e' <- propagate path e
            return $ Typed qt' area' metadata' (ListItem e')

          Typed qt' area' metadata' (ListSpread e) -> do
            e' <- propagate path e
            return $ Typed qt' area' metadata' (ListSpread e')

          or ->
            return or
        )
        items
    return $ Typed qt area metadata (ListConstructor items')

  Typed qt area metadata (Record fields) -> do
    fields' <-
      mapM
        (\case
          Typed qt' area' metadata' (Field (n, e)) -> do
            e' <- propagate path e
            return $ Typed qt' area' metadata' (Field (n, e'))

          Typed qt' area' metadata' (FieldSpread e) -> do
            e' <- propagate path e
            return $ Typed qt' area' metadata' (FieldSpread e')

          or ->
            return or
        )
        fields
    return $ Typed qt area metadata (Record fields')

  Typed qt area metadata (If cond truthy falsy) -> do
    cond' <- propagate path cond
    truthy' <- propagate path truthy
    falsy' <- propagate path falsy
    return $ Typed qt area metadata (If cond' truthy' falsy')

  Typed qt area metadata (While cond body) -> do
    cond' <- propagate path cond
    body' <- propagate path body
    return $ Typed qt area metadata (While cond' body')

  Typed qt area metadata (Access rec field) -> do
    rec' <- propagate path rec
    field' <- propagate path field
    return $ Typed qt area metadata (Access rec' field')

  Typed qt area metadata (Where e iss) -> do
    e' <- propagate path e
    iss' <-
      mapM
        (\(Typed qt' area' metadata' (Is pat isExp)) -> do
          isExp' <- propagate path isExp
          return $ Typed qt' area' metadata' (Is pat isExp')
        )
        iss
    return $ Typed qt area metadata (Where e' iss')

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


isDef :: Exp -> Bool
isDef exp = case exp of
  Typed _ _ _ (Definition _ _) ->
    True

  Typed _ _ _ (Export e) ->
    isDef e

  Typed _ _ _ (Assignment _ e) ->
    isDef e

  Typed _ _ _ (Extern _ _ _) ->
    True

  _ ->
    False

getDefParamCount :: Exp -> Int
getDefParamCount exp = case exp of
  Typed _ _ _ (Definition params _) ->
    length params

  Typed _ _ _ (Export e) ->
    getDefParamCount e

  Typed _ _ _ (Assignment _ e) ->
    getDefParamCount e

  Typed (_ :=> t) _ _ Extern{} ->
    length $ getParamTypes t

  _ ->
    0


makeImportForNameInForeignAST :: AST -> String -> Import
makeImportForNameInForeignAST ast name = case List.find ((== Just name) . getExpName) (aexps ast) of
  Just found ->
    let path = Maybe.fromMaybe "" $ apath ast
        importType =
          if isDef found then
            DefinitionImport $ getDefParamCount found
          else
            ExpressionImport
    in  Untyped emptyArea [] (NamedImport [Typed (getQualType found) emptyArea [] (ImportInfo name importType)] path path)

  Nothing ->
    case List.find (\(Untyped _ _ (NamedImport names _ _)) -> any ((== name) . getImportName) names) (aimports ast) of
      Just (Untyped area metadata (NamedImport names relPath absPath)) ->
        let neededName = Maybe.fromMaybe undefined $ List.find ((== name) . getImportName) names
        in  Untyped area metadata (NamedImport [neededName] relPath absPath)

      _ ->
        case List.find ((== name) . getConstructorName) (atypedecls ast >>= (adtconstructors . getValue)) of
          Just (Untyped _ _ (Constructor _ _ t)) ->
            let path = Maybe.fromMaybe "" $ apath ast
            in  Untyped emptyArea [] (NamedImport [Typed ([] :=> t) emptyArea [] (ImportInfo name ConstructorImport)] path path)

          _ ->
            undefined


mergeImports :: [Import] -> [Import]
mergeImports imports =
  let groupedByPath = List.groupBy (\a b -> getImportAbsolutePath a == getImportAbsolutePath b) (List.sortBy (\a b -> compare (getImportAbsolutePath a) (getImportAbsolutePath b)) imports)
      mergeImports = map $ \is ->
        let path = (getImportAbsolutePath $ List.head is)
            merged = foldr
              (\(Untyped area _ (NamedImport names1 relPath absPath)) (Untyped _ _ (NamedImport names2 _ _)) ->
                Untyped area [] (NamedImport (names2 ++ filter (\n -> getImportName n `List.notElem` map getImportName names2) names1) relPath absPath)
              )
              (Untyped emptyArea [] (NamedImport [] path path))
              is
        in  merged
  in  mergeImports groupedByPath


moduleAccessesToImports :: (FilePath, Set.Set String) -> Propagate [Import]
moduleAccessesToImports (modulePath, namesNeeded) = do
  originalAST <- Rock.fetch $ CoreAST modulePath
  return $ map (makeImportForNameInForeignAST originalAST) (Set.toList namesNeeded)


accessesToImports :: [(FilePath, Set.Set String)] -> Propagate [Import]
accessesToImports accesses =
  concat <$> mapM moduleAccessesToImports accesses


updateImports :: AST -> Propagate AST
updateImports ast = do
  PropagationState _ _ accesses <- get
  newImports <- accessesToImports $ Map.toList accesses
  let existingImports = aimports ast
  return $ ast { aimports = mergeImports (existingImports ++ newImports) }


propagateAST :: AST -> Propagate AST
propagateAST ast = do
  propagatedExps <-
    mapM
      (propagateTopLevelExp (Maybe.fromMaybe "" $ apath ast))
      (aexps ast)
  updateImports ast { aexps = concat propagatedExps }
