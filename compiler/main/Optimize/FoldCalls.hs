module Optimize.FoldCalls where
import AST.Core
import qualified Data.List as List
import qualified Data.Maybe as Maybe

{-
Main use case is to eliminate extra closures created by
closure conversion which are of the form:
  closure = partiallyApplyLambdaLiftedWithEnv(a, b)
  value = closure(initialArg)
with the following:
  value = partiallyApplyLambdaLiftedWithEnv(a, b, c)
-}


findAllAccesses :: Exp -> [String]
findAllAccesses exp = case exp of
  Typed _ _ _ (Call fn args) ->
    let args' = concatMap findAllAccesses args
    in  findAllAccesses fn ++ args'

  Typed _ _ _ (Assignment _ e) ->
    findAllAccesses e

  Typed _ _ _ (Definition _ body) ->
    concatMap findAllAccesses body

  Typed _ _ _ (Do body) ->
    concatMap findAllAccesses body

  Typed _ _ _ (If cond truthy falsy) ->
    let cond' = findAllAccesses cond
        truthy' = findAllAccesses truthy
        falsy' = findAllAccesses falsy
    in  cond' ++ truthy' ++ falsy'

  Typed _ _ _ (Where e iss) ->
    let e' = findAllAccesses e
        iss' = concatMap (findAllAccesses . getIsExpression) iss
    in  e' ++ iss'

  Typed _ _ _ (ListConstructor items) ->
    concatMap (findAllAccesses . getListItemExp) items

  Typed _ _ _ (TupleConstructor items) ->
    concatMap findAllAccesses items

  Typed _ _ _ (Access rec _) ->
    findAllAccesses rec

  Typed _ _ _ (Record fields) ->
    concatMap (findAllAccesses . getFieldExp) fields

  Typed _ _ _ (Var n _) ->
    [n]

  _ ->
    []


findInvalidAccesses :: Exp -> [String]
findInvalidAccesses exp = case exp of
  Typed _ _ _ (Call (Typed _ _ _ (Var _ _)) args) ->
    concatMap findInvalidAccesses args

  Typed _ _ _ (Call fn args) ->
    let args' = concatMap findInvalidAccesses args
    in  findInvalidAccesses fn ++ args'

  Typed _ _ _ (Assignment _ e) ->
    findInvalidAccesses e

  Typed _ _ _ (Definition _ body) ->
    concatMap findInvalidAccesses body

  Typed _ _ _ (Do body) ->
    concatMap findInvalidAccesses body

  Typed _ _ _ (If cond truthy falsy) ->
    let cond' = findInvalidAccesses cond
        truthy' = findInvalidAccesses truthy
        falsy' = findInvalidAccesses falsy
    in  cond' ++ truthy' ++ falsy'

  Typed _ _ _ (Where e iss) ->
    let e' = findInvalidAccesses e
        iss' = concatMap (findInvalidAccesses . getIsExpression) iss
    in  e' ++ iss'

  Typed _ _ _ (ListConstructor items) ->
    concatMap (findInvalidAccesses . getListItemExp) items

  Typed _ _ _ (TupleConstructor items) ->
    concatMap findInvalidAccesses items

  Typed _ _ _ (Access rec _) ->
    findInvalidAccesses rec

  Typed _ _ _ (Record fields) ->
    concatMap (findInvalidAccesses . getFieldExp) fields

  Typed _ _ _ (Var n _) ->
    [n]

  _ ->
    []


findEligibleCalls :: Exp -> [(String, Exp)] -> [(String, Exp)]
findEligibleCalls exp found = case exp of
  Typed _ _ _ (Assignment n call@(Typed _ _ _ (Call (Typed _ _ metadata (Var fnName _)) _))) | not (isReferenceToMutatingFunction metadata) ->
    if any ((== fnName) . fst) found then
      -- if we have already found the function it'll most likely be inlined and we can
      -- skip it for the current pass
      -- List.foldl' (flip findEligibleCalls) found args
      found
    else
      (n, call) : found

  Typed _ _ _ (Call (Typed _ _ _ (Var _ _)) args) ->
    -- if it's a call to a local function already found we don't discard it
    -- as in the case of simply have the var being passed because most likely
    -- we can propagate to that call as well
    foldr findEligibleCalls found args

  Typed _ _ _ (Assignment _ e) ->
    findEligibleCalls e found

  Typed _ _ _ (Definition _ _) ->
    found

  Typed _ _ _ (Do body) ->
    foldr findEligibleCalls found body

  Typed _ _ _ (If cond truthy falsy) ->
    let cond' = findEligibleCalls cond found
        truthy' = findEligibleCalls truthy cond'
    in  findEligibleCalls falsy truthy'

  Typed _ _ _ (Where e iss) ->
    let e' = findEligibleCalls e found
        iss' = foldr findEligibleCalls e' (getIsExpression <$> iss)
    in  iss'

  Typed _ _ _ (ListConstructor items) ->
    foldr findEligibleCalls found (getListItemExp <$> items)

  Typed _ _ _ (TupleConstructor items) ->
    foldr findEligibleCalls found items

  Typed _ _ _ (Access rec _) ->
    findEligibleCalls rec found

  Typed _ _ _ (Var n _) ->
    filter ((/= n) . fst) found

  _ ->
    found


findEligibleCallsInBody :: [Exp] -> [(String, Exp)] -> [(String, Exp)]
findEligibleCallsInBody exps found = case exps of
  e : es ->
    let found' = findEligibleCalls e found
    in  findEligibleCallsInBody es found'

  [] ->
    found


-- we need to do 2 things here:
--   - remove assignments of candidates
--   - propagate calls to candidates
propagateCalls :: Exp -> [(String, Exp)] -> Maybe Exp
propagateCalls exp candidates = case exp of
  Typed qt area metadata (Call (Typed cQt cArea cMetadata (Var fnName isCtor)) args) ->
    case List.find ((== fnName) . fst) candidates of
      Just (_, initialCall) ->
        let args' = Maybe.mapMaybe (`propagateCalls` candidates) args
        in  Just $ mergeCalls $ Typed qt area metadata (Call initialCall args')

      _ ->
        let args' = Maybe.mapMaybe (`propagateCalls` candidates) args
        in  Just $ Typed qt area metadata (Call (Typed cQt cArea cMetadata (Var fnName isCtor)) args')

  Typed qt area metadata (Call fn args) ->
    let fn' = Maybe.fromMaybe fn $ propagateCalls fn candidates
        args' = Maybe.mapMaybe (`propagateCalls` candidates) args
    in  Just $ Typed qt area metadata (Call fn' args')

  Typed qt area metadata (Assignment n e) ->
    if any ((== n) . fst) candidates then
      -- if that assignment is one of the ones we want to eliminate we remove it
      Nothing
    else
      Just $ Typed qt area metadata (Assignment n (Maybe.fromMaybe e $ propagateCalls e candidates))

  -- Typed qt area metadata (Definition params body) ->
  --   let body' = Maybe.mapMaybe (`propagateCalls` candidates) body
  --   in  Just $ Typed qt area metadata (Definition params body')

  Typed qt area metadata (Do body) ->
    let body' = Maybe.mapMaybe (`propagateCalls` candidates) body
    in  Just $ Typed qt area metadata (Do body')

  Typed qt area metadata (If cond truthy falsy) ->
    let cond' = Maybe.fromMaybe cond $ propagateCalls cond candidates
        truthy' = Maybe.fromMaybe truthy $ propagateCalls truthy candidates
        falsy' = Maybe.fromMaybe falsy $ propagateCalls falsy candidates
    in  Just $ Typed qt area metadata (If cond' truthy' falsy')

  Typed qt area metadata (Where e iss) ->
    let e' = Maybe.fromMaybe e $ propagateCalls e candidates
        iss' = map (mapIs (\e -> Maybe.fromMaybe e $ propagateCalls e candidates)) iss
    in  Just $ Typed qt area metadata (Where e' iss')

  Typed qt area metadata (ListConstructor items) ->
    let items' = map (mapListItem (\e -> Maybe.fromMaybe e $ propagateCalls e candidates)) items
    in  Just $ Typed qt area metadata (ListConstructor items')

  Typed qt area metadata (TupleConstructor items) ->
    let items' = Maybe.mapMaybe (`propagateCalls` candidates) items
    in  Just $ Typed qt area metadata (TupleConstructor items')

  Typed qt area metadata (Access rec field) ->
    let rec' = Maybe.fromMaybe rec $ propagateCalls rec candidates
    in  Just $ Typed qt area metadata (Access rec' field)

  or ->
    Just or



propagateCallsInBody :: [Exp] -> [(String, Exp)] -> [Exp]
propagateCallsInBody exps candidates = case exps of
  e : es ->
    case propagateCalls e candidates of
      Nothing ->
        propagateCallsInBody es candidates

      Just stayed ->
        stayed : propagateCallsInBody es candidates

  [] ->
    []


processBody :: [Exp] -> [Exp]
processBody exps =
  let candidates = findEligibleCallsInBody exps []
      invalidAccesses = concatMap findInvalidAccesses exps
      allAccesses = concatMap findAllAccesses exps
      candidates' = filter ((`List.notElem` invalidAccesses) . fst) candidates
      -- we currently only apply it if it occurs only once in the function
      -- as otherwise we break things for mutating callees such as this:
      --   f = memoize(add)

      --   f(1)
      --   f(1)
      --   f(1)
      -- Because it'd compile to this:
      --   memoize(add, 1)
      --   memoize(add, 1)
      --   memoize(add, 1)
      -- TODO: revisit this soon
      candidates'' = filter ((== 1) . length . (\n -> filter (== n) allAccesses) . fst) candidates'
  in  if null candidates'' then
        exps
      else
        -- as long as we find candidates we keep on doing it
        processBody $ propagateCallsInBody exps candidates''


foldTopLevelExps :: Exp -> Exp
foldTopLevelExps exp = case exp of
  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (foldTopLevelExps e))

  Typed qt area metadata (Assignment n e) ->
    Typed qt area metadata (Assignment n (foldTopLevelExps e))

  Typed qt area metadata (Do body) ->
    Typed qt area metadata (Do (processBody body))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (processBody body))

  or ->
    or


foldAST :: AST -> AST
foldAST ast =
  let foldedExps = foldTopLevelExps <$> aexps ast
  in  ast { aexps = foldedExps }
