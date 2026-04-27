{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use second" #-}
module Infer.Exp where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Maybe
import           Control.Monad.Except
import           Data.Foldable                  ( foldlM )
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Error.Error
import           Error.Context
import           Explain.Location
import           Data.List                      ( (\\)
                                                , partition
                                                , foldl', intersect
                                                )
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.EnvUtils
import           Infer.Typing
import           Infer.Substitute
import           Infer.Unify
import           Infer.Instantiate
import           Infer.Scheme                   ( quantify )
import           Infer.Pattern (inferPattern, updatePatternTypes, fixRestVarTypes)
import           Infer.Pred
import           Infer.Generalize
import           Infer.Placeholder
import           Infer.ToSolved
import qualified Utils.Tuple                   as T
import qualified Control.Monad                 as CM
import           AST.Solved (getType)
import qualified Data.Set as Set
import           Run.Options
import qualified Data.List as List
import           Data.Char (isAlphaNum)


-- | Extract an ErrorOrigin from a function application expression.
-- For operators like +, &&, etc., returns FromOperator.
-- For named functions, returns FromFunctionArgument with name and arg index.
getAppOrigin :: Can.Exp -> ErrorOrigin
getAppOrigin (Can.Canonical _ expr) = case expr of
  Can.Var name
    | isOperatorName name -> FromOperator name
    | otherwise           -> FromFunctionArgument name 1 Nothing
  Can.App (Can.Canonical _ (Can.Var name)) _ _
    | isOperatorName name -> FromOperator name
    | otherwise           -> FromFunctionArgument name 2 Nothing
  Can.App (Can.Canonical _ (Can.App (Can.Canonical _ (Can.Var name)) _ _)) _ _
    | isOperatorName name -> FromOperator name
    | otherwise           -> FromFunctionArgument name 3 Nothing
  _ -> NoOrigin
  where
    isOperatorName []    = False
    isOperatorName (c:_) = not (isAlphaNum c) && c /= '_' && c /= '.'


-- | Check if a type is Maybe a (for JSX optional prop defaulting)
isMaybeType :: Type -> Bool
isMaybeType (TApp (TCon (TC "Maybe" _) _ _) _) = True
isMaybeType _ = False


-- | Collect type variables that appear directly as function parameter types.
-- For `String -> a -> b -> Element`, returns {a, b} (not String or Element which are TCon).
-- For `{ ...r } -> Element`, returns {} (the record is compound, not a plain TVar).
collectTopLevelParamVars :: Type -> S.Set TVar
collectTopLevelParamVars (TApp (TApp _ paramType) returnType) =
  case paramType of
    TVar tv -> S.insert tv (collectTopLevelParamVars returnType)
    _       -> collectTopLevelParamVars returnType
collectTopLevelParamVars _ = S.empty

-- | Check if a type is compound (record or type application, not a plain variable or constructor).
isCompoundBinding :: Type -> Bool
isCompoundBinding (TRecord _ _ _) = True
isCompoundBinding (TApp _ _) = True
isCompoundBinding _ = False


-- | Lift a leaf-style inference (one that does not contribute to the
-- substitution) into the legacy 4-tuple shape. Used during the Phase 1
-- migration to allow individual leaf functions to be rewritten in the new
-- 3-tuple form before the dispatch and all callers are updated.
liftLeaf :: Infer ([Pred], Type, Slv.Exp) -> Infer (Substitution, [Pred], Type, Slv.Exp)
liftLeaf action = do
  (ps, t, e) <- action
  return (M.empty, ps, t, e)


-- | Lift a migrated 3-tuple inference into the legacy 4-tuple shape, using
-- `captureDelta` to recover the substitution the action contributed. Use
-- this for migrated non-leaf inference functions that DO unify or that
-- transitively call `infer`. Non-migrated callers continue to receive a
-- correct delta substitution they can compose with their own.
liftWithDelta :: Infer ([Pred], Type, Slv.Exp) -> Infer (Substitution, [Pred], Type, Slv.Exp)
liftWithDelta action = do
  (s, (ps, t, e)) <- captureDelta action
  return (s, ps, t, e)


-- | The dispatch remains 4-tuple. Each arm's per-arm contribution is captured
-- via liftWithDelta so the legacy-style callers that use the returned
-- substitution explicitly (like inferAssignment with its load-bearing
-- reversed compose order) still work. Migrated callers can ignore the s
-- and read state via getSubst / applyCurrentSubst.
infer :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer options env lexp = do
  let (Can.Canonical area exp) = lexp
  case exp of
    Can.LNum  _               -> do
      t <- newTVar Star
      let ps = [IsIn "Number" [t] Nothing]
      return (M.empty, ps, t, applyLitSolve lexp (ps :=> t))

    Can.LByte _               -> return (M.empty, [], tByte, applyLitSolve lexp ([] :=> tByte))
    Can.LShort _              -> return (M.empty, [], tShort, applyLitSolve lexp ([] :=> tShort))
    Can.LInt _                -> return (M.empty, [], tInteger, applyLitSolve lexp ([] :=> tInteger))
    Can.LFloat _              -> return (M.empty, [], tFloat, applyLitSolve lexp ([] :=> tFloat))
    Can.LStr  _               -> return (M.empty, [], tStr, applyLitSolve lexp ([] :=> tStr))
    Can.LChar  _              -> return (M.empty, [], tChar, applyLitSolve lexp ([] :=> tChar))
    Can.LBool _               -> return (M.empty, [], tBool, applyLitSolve lexp ([] :=> tBool))
    Can.LUnit                 -> return (M.empty, [], tUnit, applyLitSolve lexp ([] :=> tUnit))
    Can.TemplateString _      -> liftWithDelta (inferTemplateString options env lexp)

    Can.Var            _      -> liftLeaf (inferVar options env lexp)
    Can.Abs _ _               -> liftWithDelta (inferAbs options env lexp)
    Can.App{}                 -> liftWithDelta (inferApp options env lexp)
    Can.Assignment _ _        -> liftWithDelta (inferAssignment options env lexp)
    Can.Mutate _ _            -> liftWithDelta (inferMutate options env lexp)
    Can.Do _                  -> liftWithDelta (inferDo options env lexp)
    Can.Where      _ _        -> liftWithDelta (inferWhere options env lexp)
    Can.Record _              -> liftWithDelta (inferRecord options env lexp)
    Can.JsxRecord _           -> liftWithDelta (inferJsxRecord options env lexp)
    Can.Access   _ _          -> liftWithDelta (inferAccess options env lexp)
    Can.ArrayAccess   _ _     -> liftWithDelta (inferArrayAccess options env lexp)
    Can.TypedExp{}            -> liftWithDelta (inferTypedExp options env lexp)
    Can.ListConstructor  _    -> liftWithDelta (inferListConstructor options env lexp)
    Can.TupleConstructor _    -> liftWithDelta (inferTupleConstructor options env lexp)
    Can.Export           _    -> liftWithDelta (inferExport options env lexp)
    Can.NameExport       _    -> liftLeaf (inferNameExport env lexp)
    Can.If{}                  -> liftWithDelta (inferIf options env lexp)
    Can.While{}               -> liftWithDelta (inferWhile options env lexp)
    Can.Extern{}              -> liftLeaf (inferExtern env lexp)
    Can.TypedHole             -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Typed ([] :=> t) area Slv.TypedHole)

    Can.JSExp c               -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Typed ([] :=> t) area (Slv.JSExp c))


applyLitSolve :: Can.Exp -> Qual Type -> Slv.Exp
applyLitSolve (Can.Canonical area exp) qt = case exp of
  Can.LNum  v  -> Slv.Typed qt area $ Slv.LNum v
  Can.LByte  v -> Slv.Typed qt area $ Slv.LNum v
  Can.LShort v -> Slv.Typed qt area $ Slv.LNum v
  Can.LInt  v  -> Slv.Typed qt area $ Slv.LNum v
  Can.LFloat v -> Slv.Typed qt area $ Slv.LFloat v
  Can.LStr  v  -> Slv.Typed qt area $ Slv.LStr v
  Can.LChar  v -> Slv.Typed qt area $ Slv.LChar v
  Can.LBool v  -> Slv.Typed qt area $ Slv.LBool v
  Can.LUnit    -> Slv.Typed qt area Slv.LUnit

applyAbsSolve :: Can.Exp -> Slv.Solved Slv.Name -> [Slv.Exp] -> Qual Type -> Slv.Exp
applyAbsSolve (Can.Canonical loc _) param body qt = Slv.Typed qt loc $ Slv.Abs param body

applyAssignmentSolve :: Can.Exp -> Slv.Name -> Slv.Exp -> Qual Type -> Slv.Exp
applyAssignmentSolve (Can.Canonical loc _) n exp qt = Slv.Typed qt loc $ Slv.Assignment n exp


updateQualType :: Slv.Exp -> Qual Type -> Slv.Exp
updateQualType (Slv.Typed _ a e) qt = Slv.Typed qt a e


-- TODO: handle this properly so that code generation can rely on it
updatePattern :: Qual Type -> Can.Pattern -> Slv.Pattern
updatePattern qt (Can.Canonical area pat) = case pat of
  Can.PVar name             ->
    Slv.Typed qt area $ Slv.PVar name

  Can.PAny                  ->
    Slv.Typed qt area Slv.PAny

  Can.PCon name patterns    ->
    Slv.Typed qt area $ Slv.PCon name (updatePattern qt <$> patterns)

  Can.PNum    n             ->
    Slv.Typed ([] :=> tNumber) area $ Slv.PNum n

  Can.PStr    n             ->
    Slv.Typed ([] :=> tStr) area $ Slv.PStr n

  Can.PChar    n             ->
    Slv.Typed ([] :=> tChar) area $ Slv.PChar n

  Can.PBool   n             ->
    Slv.Typed ([] :=> tBool) area $ Slv.PBool n

  Can.PRecord fieldPatterns restName ->
    Slv.Typed qt area $ Slv.PRecord (updatePattern qt <$> fieldPatterns) restName

  Can.PList   patterns      ->
    Slv.Typed qt area $ Slv.PList (updatePattern qt <$> patterns)

  Can.PTuple  patterns      ->
    Slv.Typed qt area $ Slv.PTuple (updatePattern qt <$> patterns)

  Can.PSpread pat'          ->
    Slv.Typed qt area $ Slv.PSpread (updatePattern qt pat')



-- INFER VAR

-- | Phase 1 migration: returns the new 3-tuple form. Leaf inference (no
-- substitution contribution) so callers bridge with `liftLeaf`.
inferVar :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferVar _ env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s = Forall [Star, Star] $ [] :=> (TRecord (M.fromList [(name, TGen 0)]) (Just $ TGen 1) mempty `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (ps, t, Slv.Typed (ps :=> t) area $ Slv.Var n False)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    (ps :=> t) <- instantiate sc

    let ps' = dedupePreds ps
    let e = Slv.Typed (ps' :=> t) area $ Slv.Var n (isConstructor env n)
    let ps'' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps'

    return (ps'', t, e)

enhanceVarError :: Env -> Can.Exp -> Area -> CompilationError -> Infer Scheme
enhanceVarError env _ area (CompilationError e _) =
  throwError $ CompilationError e (Context (envCurrentPath env) area)


-- INFER NAME EXPORT

-- | Phase 1 migrated: 3-tuple, leaf inference (no substitution contribution).
inferNameExport :: Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferNameExport env exp@(Can.Canonical area (Can.NameExport name)) = do
  sc         <- catchError (lookupVar env name) (enhanceVarError env exp area)
  (ps :=> t) <- instantiate sc

  let e = Slv.Typed (ps :=> t) area $ Slv.NameExport name

  return (ps, t, e)



-- INFER ABSTRACTIONS

-- Param white list for shadowing check
allowedShadows :: [String]
allowedShadows = ["_P_", "__x__", "_"]

extendAbsEnv :: Env -> Type -> Can.Canonical Can.Name -> Infer Env
extendAbsEnv env tv (Can.Canonical area param) = if param `elem` allowedShadows
  then return $ extendVars env (param, Forall [] ([] :=> tv))
  else catchError
    (safeExtendVars env (param, Forall [] ([] :=> tv)))
    (((const $ extendVars env (param, Forall [] ([] :=> tv))) <$>) . pushError . upgradeContext' env area)


-- | Phase 1 migrated: 3-tuple. With the transactional `captureDelta`,
-- inner migrated arms no longer leak into state, so we use applyCurrentSubst
-- (state-based) instead of explicit s threading.
inferAbs :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferAbs options env l@(Can.Canonical _ (Can.Abs p@(Can.Canonical area param) body)) = do
  tv             <- newTVar Star
  env'           <- extendAbsEnv env tv p
  (ps, t, es)    <- inferBody options env' { envInBody = True } body
  s              <- getSubst
  es'            <- postProcessBody options env' s (tv `fn` t) es
  s'             <- getSubst

  let t'        = apply s' (tv `fn` t)
      paramType = apply s' tv

  return (apply s' ps, t', applyAbsSolve l (Slv.Typed (apply s' $ ps :=> paramType) area param) es' (apply s' $ ps :=> t'))


-- | Phase 1 migrated: 3-tuple. With transactional `captureDelta`, state's
-- currentSubst correctly reflects only this frame's contributions while
-- inside the action, so we can use applyCurrentSubst to get the cumulative
-- subst at any point.
inferBody :: Options -> Env -> [Can.Exp] -> Infer ([Pred], Type, [Slv.Exp])
inferBody options env [e] = do
  (s, ps, t, e') <- infer options env e
  extSubst s
  return (ps, t, [e'])

inferBody options env (e : es) = do
  ((returnPreds, _), env', e') <- inferImplicitlyTyped options True env e
  envApplied <- applyCurrentSubst env'
  (ps', tb, eb) <- inferBody options envApplied es

  finalS <- getSubst
  return (apply finalS $ returnPreds ++ ps', tb, e' : eb)


-- | Phase 1 migrated: 3-tuple. The explicit accSubst threading inside the
-- fold is preserved (it's the body-level defaulting accumulator). At the end
-- we extSubst the final s' so callers (inferAbs, inferDo) can read the
-- contribution via state. The legacy explicit `s` parameter still bootstraps
-- the accumulator from the caller's view.
postProcessBody :: Options -> Env -> Substitution -> Type -> [Slv.Exp] -> Infer [Slv.Exp]
postProcessBody options env s expType es = do
  discardError <- isDiscardingErrors
  -- Accumulate reversed (cons O(1)) then reverse at end — avoids O(n²) with ++
  (esRev, s', _) <- foldM
    (\(resultsRev, accSubst, env'') (Slv.Typed (ps' :=> t') area e) -> do
      let ps'' = apply accSubst ps'
          -- Lazily compute fs only when needed (non-empty unsolvedPs)
          fs = S.toList $ ftv (apply accSubst env'') `S.union` ftv (apply accSubst expType) `S.union` ftvForLetGenSet (apply accSubst t')

      (ps''', substFromDefaulting) <- do
        prep <- CM.forM ps'' $ \p -> do
          isResolved <- entail env [] p
          return (p, isResolved)

        let solvedPs = [p | (p, True) <- prep]
        let unsolvedPs = [p | (p, False) <- prep]

        -- Short-circuit: ambiguities is only non-empty if unsolvedPs is non-empty
        if not (null unsolvedPs) && ambiguities fs unsolvedPs /= [] then do
          (sDef, unsolvedPs')   <- tryDefaults env unsolvedPs
          (sDef', unsolvedPs'') <- tryDefaults env (apply sDef unsolvedPs')
          let subst = sDef' `compose` sDef

          if unsolvedPs'' /= [] then do
            CM.forM_ unsolvedPs'' $ \p -> do
              catchError
                (byInst env (apply subst p))
                (\case
                  _ | discardError ->
                    return []

                  (CompilationError FatalError NoContext) ->
                    if ambiguities fs unsolvedPs'' /= [] then
                      case p of
                        IsIn _ (TVar tv : _) _ ->
                          throwError $ CompilationError
                            (AmbiguousType (tv, apply subst unsolvedPs''))
                            (Context (envCurrentPath env) area)

                        _ ->
                          throwError $ CompilationError
                            (AmbiguousType (TV (-1) Star, apply subst unsolvedPs''))
                            (Context (envCurrentPath env) area)
                      else
                        return []
                  or ->
                    throwError or
                )
            return (unsolvedPs'' ++ solvedPs, subst)
          else
            return (unsolvedPs'' ++ solvedPs, subst)
        else
          return (ps'', mempty)

      let sFinal = substFromDefaulting `compose` accSubst
      e' <- updateExpTypes options env False sFinal (Slv.Typed (apply sFinal $ ps''' :=> t') area e)

      return (e' : resultsRev, sFinal, apply sFinal env'')
    )
    (mempty, s, env)
    es

  extSubst s'
  return (reverse esRev)


-- INFER APP

-- | Phase 1 migrated: 3-tuple. extSubsts each sub-inference and unification
-- contribution into state (HM convention).
inferApp :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferApp options env (Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea argContent) final)) = do
  discardError <- isDiscardingErrors
  tv                  <- newTVar Star
  (s1, ps1, t1, eabs) <- infer options env abs
  extSubst s1
  env1 <- applyCurrentSubst env
  (s2, ps2, t2, earg) <- infer options env1 arg
  extSubst s2

  let expForContext = arg  -- Always point at the argument (the "wrong" value)

  -- Enrich the origin with function context (expected type + full signature).
  -- At each curried application level, funcType is the *remaining* function type,
  -- so the expected param is always the 1st parameter of funcType (not the idx-th).
  funcType <- applyCurrentSubst t1
  let baseOrigin = getAppOrigin abs
      origin = case baseOrigin of
        FromFunctionArgument fn idx _ ->
          let params = getParamTypes funcType
              -- Always take the 1st param of the remaining function type
              expectedParam = case params of { (p:_) -> Just p; _ -> Nothing }
              ctx = FunctionContext
                { fcExpectedType  = maybe funcType id expectedParam
                , fcFullSignature = funcType
                , fcTotalParams   = length params
                }
          in  FromFunctionArgument fn idx (Just ctx)
        other -> other

      -- Build secondary location: point at the function expression
      secondaryLoc = case baseOrigin of
        FromFunctionArgument fn _ _ ->
          Just $ SecondaryLocation (envCurrentPath env) absArea
            ("'" <> fn <> "' is applied here")
        _ -> Nothing

  -- Note: legacy passed `apply s2 t1` and `apply s1 t2 `fn` tv`. With state-
  -- based subst, both have already had their relevant prior substs applied.
  t1Applied <- applyCurrentSubst t1
  t2Applied <- applyCurrentSubst t2
  s3 <- contextualUnifyWithOriginAndSecondary (if discardError then Discard else Strict) origin secondaryLoc env expForContext t1Applied (t2Applied `fn` tv)
  extSubst s3

  t <- applyCurrentSubst tv

  -- For JSX records: fill missing Maybe-typed fields with Nothing
  earg' <- case argContent of
    Can.JsxRecord jsxFields -> do
      let explicitNames = S.fromList [ n | Can.Canonical _ (Can.Field (n, _)) <- jsxFields ]
      resolvedArgType <- applyCurrentSubst t2
      case resolvedArgType of
        TRecord allFields _ _ -> do
          let missingFields = M.filterWithKey (\k _ -> k `S.notMember` explicitNames) allFields
          let allMaybe = all isMaybeType (M.elems missingFields)
          if M.null missingFields || not allMaybe then
            return earg
          else do
            -- Synthesize Nothing fields for missing Maybe-typed props
            let nothingFields = map (\(name, fieldType) ->
                  Slv.Typed ([] :=> fieldType) argArea
                    (Slv.Field (name, Slv.Typed ([] :=> fieldType) argArea (Slv.Var "Nothing" True)))
                  ) (M.toList missingFields)
            case earg of
              Slv.Typed qt a (Slv.Record existingFields) ->
                return $ Slv.Typed qt a (Slv.Record (existingFields ++ nothingFields))
              _ -> return earg
        _ -> return earg
    _ -> return earg

  s <- getSubst
  let solved = Slv.Typed (apply s (ps1 ++ ps2) :=> apply s t) area $ Slv.App eabs (updateQualType earg' $ apply s (ps1 ++ ps2) :=> apply s t2) final

  return (ps1 ++ ps2, t, solved)



-- INFER TEMPLATE STRINGS

-- | Phase 1 migrated: 3-tuple. Each element's substitution is composed into
-- state via extSubst (HM convention, matches the legacy `s2 \`compose\` s1
-- \`compose\` subst` order).
inferTemplateString :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferTemplateString options env (Can.Canonical area (Can.TemplateString exps)) = do
  discardError <- isDiscardingErrors
  (elemQsRev, elemExpsRev, elemPSRev) <- foldM
    (\(qsRev, esRev, psRev) exp -> do
      env' <- applyCurrentSubst env
      (s1, ps, t, e) <- infer options env' exp
      extSubst s1
      inferredType <- applyCurrentSubst t
      env'' <- applyCurrentSubst env
      s2 <- contextualUnify' env'' discardError exp inferredType tStr
      extSubst s2
      return ((ps :=> t) : qsRev, e : esRev, ps : psRev)
    )
    ([], [], [])
    exps

  let qs       = reverse elemQsRev
  let elemExps = reverse elemExpsRev
  let elemPS   = concat (reverse elemPSRev)

  fullSubst <- getSubst
  let updatedExp = Slv.Typed
        ([] :=> tStr)
        area
        (Slv.TemplateString ((\(t, e) -> updateQualType e (apply fullSubst t)) <$> zip qs elemExps))

  return (apply fullSubst elemPS, tStr, updatedExp)



-- INFER ASSIGNMENT

-- | Phase 1 migrated: 3-tuple. Note the load-bearing `s1 `compose` s2` order
-- (legacy convention: applies s2 FIRST then s1, opposite to the HM newer-LEFT
-- convention used by inferApp etc.). We compute the composed substitution
-- explicitly (instead of incremental extSubst calls which would produce HM
-- order) and extSubst the result so the dispatch's liftWithDelta sees the
-- delta. Tests are calibrated against this exact ordering.
inferAssignment :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferAssignment options env e@(Can.Canonical area (Can.Assignment name exp)) = do
  discardError <- isDiscardingErrors
  when (name `Set.member` envNamespacesInScope env && not discardError) $ do
    pushError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath env) area)

  currentScheme <- case M.lookup name (envVars env) of
    Just sc ->
      return sc

    _ -> do
      tVar <- newTVar Star
      return $ Forall [] ([] :=> tVar)

  (currentPreds :=> currentType) <- instantiate currentScheme
  let env' = extendVars env (name, currentScheme)
  (s1, ps1, t1, e1) <- infer options env' exp
  s2                <- catchError (contextualUnify Strict env' e currentType t1) (const $ return M.empty)
  --  ^ We can skip this error as we mainly need the substitution. It would fail in inferExplicitlyTyped anyways.
  let s  = s1 `compose` s2
  let t2 = apply s t1
  extSubst s

  mutationPs <-
    if M.member name (envNamesInScope env) && envInBody env && not discardError then do
      pushError $ CompilationError BadMutation (Context (envCurrentPath env) area)
      return []
    else
      return []

  return (currentPreds ++ ps1 ++ mutationPs, apply s t2, applyAssignmentSolve e name e1 (apply s $ (currentPreds ++ ps1) :=> t2))



-- INFER MUTATE

-- | Phase 1 migrated: 3-tuple. Same legacy reversed compose order pattern as
-- inferAssignment (s1 `compose` s2 `compose` s3 — applies s3 first, s1 last).
-- We compute the composition explicitly and extSubst the result so the
-- dispatch's liftWithDelta sees the right delta. Tests are calibrated against
-- this exact ordering.
inferMutate :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferMutate options env e@(Can.Canonical area (Can.Mutate lhs exp)) = do
  discardError <- isDiscardingErrors
  (s1, ps1, t1, e1) <- infer options env lhs
  (s2, ps2, t2, e2) <- infer options (apply s1 env) exp
  s3 <- catchError
    (contextualUnify Strict env e t1 t2)
    (\err -> do
      if discardError then do
        return mempty
      else
        throwError err
    )

  let s  = s1 `compose` s2 `compose` s3
  let t3 = apply s t2
  extSubst s

  case lhs of
    Can.Canonical _ (Can.Var name) | not discardError && name `Set.member` envPatternBoundNames env ->
      throwError $ CompilationError (MutatingPatternBoundVariable name) (Context (envCurrentPath env) area)
    _ ->
      return ()

  case Can.getExpName lhs of
    Just name | not discardError ->
      if M.member name (envNamesInScope env) && envInBody env then
        markMutated name
      else
        throwError $ CompilationError (MutatingNotInScope name) (Context (envCurrentPath env) area)

    _ ->
      return ()

  return
    ( ps1 ++ ps2
    , apply s t3
    , Slv.Typed (apply s $ (ps1 ++ ps2) :=> t3) area (Slv.Mutate e1 e2)
    )



-- INFER EXPORT

-- | Phase 1 migrated: 3-tuple. Pass-through that extSubst's the inner
-- substitution into state so the call's contribution can be recovered via
-- captureDelta (see liftWithDelta).
inferExport :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferExport options env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer options env exp
  extSubst s
  return (ps, t, Slv.Typed (ps :=> t) area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

-- | Phase 1 migrated: 3-tuple. Each element's substitution and the per-element
-- unification are extSubst-ed into state in HM order.
inferListConstructor :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferListConstructor options env listExp@(Can.Canonical area (Can.ListConstructor elems)) = do
  discardError <- isDiscardingErrors
  case elems of
    [] -> do
      tv <- newTVar Star
      let t = tListOf tv
      return ([], t, Slv.Typed ([] :=> t) area (Slv.ListConstructor []))

    elems -> do
      tv               <- newTVar Star

      -- Accumulate list items and pred chunks reversed (cons O(1)) then reverse — avoids O(n²).
      -- The index tracks which element we're at (1-based) for better error messages.
      (psChunksRev, t', esRev, _) <- foldlM
        (\(pssRev, t, lis, idx) elem -> do
          envApplied <- applyCurrentSubst env
          (ps', t'', li) <- inferListItem options envApplied (fromMaybe tv t) elem
          tr <- case t of
            Nothing ->
              return t''

            Just t''' -> do
              t'''Applied <- applyCurrentSubst t'''
              s'''' <- contextualUnifyWithOrigin (if discardError then Discard else Strict) (FromListElement idx) env elem t'''Applied t''
              extSubst s''''
              return (pickJSXChild t''' t'')

          tr' <- applyCurrentSubst tr
          return (ps' : pssRev, Just tr', li : lis, idx + 1)
        )
        ([], Nothing, [], 1)
        elems

      let ps = concat (reverse psChunksRev)
      let (Just t'') = t'
      let es = reverse esRev

      s'' <- contextualUnify' env discardError listExp tv t''
      extSubst s''

      finalTv <- applyCurrentSubst tv
      let t = tListOf finalTv

      return (ps, t, Slv.Typed (ps :=> t) area (Slv.ListConstructor es))


-- | Phase 1 migrated: 3-tuple. Same legacy reversed compose order pattern as
-- inferAssignment (`s = s1 `compose` s2` — applies s2 first, s1 last). We
-- compute the composition explicitly and extSubst the result.
inferListItem :: Options -> Env -> Type -> Can.ListItem -> Infer ([Pred], Type, Slv.ListItem)
inferListItem options env _ (Can.Canonical area li) = do
  discardError <- isDiscardingErrors
  case li of
    Can.ListItem exp -> do
      (s1, ps, t, e) <- infer options env exp
      extSubst s1
      return (ps, t, Slv.Typed (ps :=> t) area $ Slv.ListItem e)

    Can.ListSpread exp -> do
      (s1, ps, t, e) <- infer options env exp
      tv <- newTVar Star
      s2 <- contextualUnify' env discardError exp (tListOf tv) t

      let s = s1 `compose` s2
      extSubst s

      return (ps, apply s tv, Slv.Typed (apply s ps :=> apply s t) area $ Slv.ListSpread e)


pickJSXChild :: Type -> Type -> Type
pickJSXChild t1 t2 = case (t1, t2) of
  (TApp (TCon (TC "Element" _) _ _) _, TCon (TC "String" _) _ _) ->
    t2

  _ ->
    t2



-- INFER TUPLE CONSTRUCTOR

-- | Phase 1 migrated: 3-tuple. The fold accumulates per-element substitutions
-- via extSubst into state instead of explicit threading.
inferTupleConstructor :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferTupleConstructor options env (Can.Canonical area (Can.TupleConstructor elems)) = do
  -- Accumulate types/exps/pred chunks reversed (cons O(1)) then reverse — avoids O(n²).
  (psChunksRev, tsRev, esRev) <-
    foldM
      (\(psRev, ts, es) e -> do
          envApplied <- applyCurrentSubst env
          (s', ps', t', e') <- infer options envApplied e
          extSubst s'
          return (ps' : psRev, t' : ts, e' : es)
      ) ([], [], []) elems
  let ps = concat (reverse psChunksRev)

  let elemTypes = reverse tsRev
  let elemEXPS  = reverse esRev
  let tupleT    = getTupleCtor (length elems)
  let t         = foldl' TApp tupleT elemTypes
  finalT        <- applyCurrentSubst t

  return (ps, finalT, Slv.Typed (ps :=> finalT) area (Slv.TupleConstructor elemEXPS))



-- INFER RECORD

-- | Phase 1 migrated: 3-tuple. Each field's substitution is composed into
-- state via inferRecordField's extSubst.
inferRecord :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferRecord options env exp = do
  discardError <- isDiscardingErrors
  let Can.Canonical area (Can.Record fields) = exp

  -- Accumulate reversed (cons O(1)) then reverse at end — avoids O(n²) with ++
  inferredFieldsRev <- foldM (
        \result field -> do
          envApplied <- applyCurrentSubst env
          (ps, ts, e) <- inferRecordField options envApplied field
          tsApplied   <- mapM (\(n, t) -> (n,) <$> applyCurrentSubst t) ts
          return ((ps, tsApplied, e) : result)
      ) [] fields
  let inferredFields = reverse inferredFieldsRev
  let fieldPS     = (\(ps, _, _) -> ps) <$> inferredFields
  let fieldTypes  = (\(_, t, _) -> t) <$> inferredFields
  let fieldEXPS   = (\(_, _, es) -> es) <$> inferredFields

  let allFieldTypes = concat fieldTypes
  let fieldTypes' = filter (\(k, _) -> k /= "...") allFieldTypes
  let spreads     = snd <$> filter (\(k, _) -> k == "...") allFieldTypes
  let base = case spreads of
        (x : _) -> Just x
        _       -> Nothing

  baseApplied <- maybe (return Nothing) (fmap Just . applyCurrentSubst) base
  recordType <- case baseApplied of
    Just (TRecord spreadFields baseBase optionalFields) -> do
      -- Merge the spread record's fields with our explicit fields
      -- The spread fields take precedence if there are conflicts
      let mergedFields = M.fromList fieldTypes' `M.union` spreadFields
      baseBaseApplied <- maybe (return Nothing) (fmap Just . applyCurrentSubst) baseBase
      return (mkRecord mergedFields baseBaseApplied optionalFields)

    Just tBase -> do
      -- The spread is a type variable or other type - unify it with a record type
      -- that has our fields and a row variable for extension
      baseVar <- newTVar Star
      let recordWithBase = TRecord (M.fromList fieldTypes') (Just baseVar) mempty
      s <- contextualUnify' env discardError exp tBase recordWithBase
      extSubst s
      unifiedBase <- applyCurrentSubst tBase
      case unifiedBase of
        TRecord unifiedFields unifiedBase' unifiedOptionalFields ->
          return (mkRecord unifiedFields unifiedBase' unifiedOptionalFields)
        _ ->
          return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty)

    Nothing ->
      return (TRecord (M.fromList fieldTypes') Nothing mempty)

  let allPS = concat fieldPS
  recordType' <- applyCurrentSubst recordType

  return (allPS, recordType', Slv.Typed (allPS :=> recordType') area (Slv.Record fieldEXPS))


-- | Phase 1 migrated: 3-tuple. Like inferRecord but creates an extensible
-- record (with a base type variable) so that missing Maybe-typed fields can
-- be filled with Nothing after unification.
inferJsxRecord :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferJsxRecord options env exp = do
  discardError <- isDiscardingErrors
  let Can.Canonical area (Can.JsxRecord fields) = exp

  -- Accumulate reversed (cons O(1)) then reverse at end — avoids O(n²) with ++
  inferredFieldsRev <- foldM (
        \result field -> do
          envApplied <- applyCurrentSubst env
          (ps, ts, e) <- inferRecordField options envApplied field
          tsApplied   <- mapM (\(n, t) -> (n,) <$> applyCurrentSubst t) ts
          return ((ps, tsApplied, e) : result)
      ) [] fields
  let inferredFields = reverse inferredFieldsRev
  let fieldPS     = (\(ps, _, _) -> ps) <$> inferredFields
  let fieldTypes  = (\(_, t, _) -> t) <$> inferredFields
  let fieldEXPS   = (\(_, _, es) -> es) <$> inferredFields

  let allFieldTypes = concat fieldTypes
  let fieldTypes' = filter (\(k, _) -> k /= "...") allFieldTypes
  let spreads     = snd <$> filter (\(k, _) -> k == "...") allFieldTypes
  let base = case spreads of
        (x : _) -> Just x
        _       -> Nothing

  baseApplied <- maybe (return Nothing) (fmap Just . applyCurrentSubst) base
  recordType <- case baseApplied of
    Just (TRecord spreadFields baseBase optionalFields) -> do
      let mergedFields = M.fromList fieldTypes' `M.union` spreadFields
      baseBaseApplied <- maybe (return Nothing) (fmap Just . applyCurrentSubst) baseBase
      return (mkRecord mergedFields baseBaseApplied optionalFields)

    Just tBase -> do
      baseVar <- newTVar Star
      let recordWithBase = TRecord (M.fromList fieldTypes') (Just baseVar) mempty
      s <- contextualUnify' env discardError exp tBase recordWithBase
      extSubst s
      unifiedBase <- applyCurrentSubst tBase
      case unifiedBase of
        TRecord unifiedFields unifiedBase' unifiedOptionalFields ->
          return (mkRecord unifiedFields unifiedBase' unifiedOptionalFields)
        _ ->
          return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty)

    Nothing -> do
      -- JSX record without spread: create an EXTENSIBLE record with a base type variable.
      -- This allows unification to absorb missing fields into the base, which we later
      -- check are all Maybe-typed and fill with Nothing.
      baseVar <- newTVar Star
      return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty)

  let allPS = concat fieldPS
  recordType' <- applyCurrentSubst recordType

  return (allPS, recordType', Slv.Typed (allPS :=> recordType') area (Slv.Record fieldEXPS))


-- | Phase 1 migrated: 3-tuple. extSubst-s the inner inference into state.
inferRecordField :: Options -> Env -> Can.Field -> Infer ([Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField options env (Can.Canonical area field) = do
  discardError <- isDiscardingErrors
  case field of
    Can.Field (name, exp) -> do
      (s, ps, t, e) <- infer options env exp
      extSubst s
      return (ps, [(name, t)], Slv.Typed (ps :=> t) area $ Slv.Field (name, e))

    Can.FieldSpread exp -> do
      (s, ps, t, e) <- infer options env exp
      extSubst s
      case t of
        TRecord{} ->
          return (ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

        TVar _ ->
          return (ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

        _ | discardError ->
          return (ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

        _ ->
          throwError $ CompilationError
            (WrongSpreadType $ show t)
            (Context (envCurrentPath env) (Can.getArea exp))



-- INFER ACCESS

-- | Phase 1 migrated: 3-tuple. Both branches (namespace access and field
-- access) are now in 3-tuple form too.
inferAccess :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferAccess options env e@(Can.Canonical _ (Can.Access ns _)) =
  case ns of
    Can.Canonical _ (Can.Var ns') ->
      if ns' `Set.member` envNamespacesInScope env then
        inferNamespaceAccess options env e
      else
        inferFieldAccess options env e

    _ ->
      inferFieldAccess options env e



-- INFER ACCESS

-- | Phase 1 migrated: 3-tuple. extSubsts each sub-inference and unification
-- contribution into state.
inferArrayAccess :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferArrayAccess options env (Can.Canonical area (Can.ArrayAccess arr index)) = do
  discardError <- isDiscardingErrors
  tv <- newTVar Star
  (s1, ps1, t1, earr) <- infer options env arr
  extSubst s1
  env1 <- applyCurrentSubst env
  (s2, ps2, t2, eindex) <- infer options env1 index
  extSubst s2
  t1' <- applyCurrentSubst t1
  s3 <- contextualUnify' env discardError arr t1' (tArrayOf tv)
  extSubst s3
  t2' <- applyCurrentSubst t2
  s4 <- contextualUnify' env discardError index t2' tInteger
  extSubst s4

  t <- applyCurrentSubst tv
  let ps = ps1 ++ ps2

  return (ps, t, Slv.Typed (ps :=> t) area (Slv.ArrayAccess earr eindex))



-- INFER NAMESPACE ACCESS

-- | Phase 1 migrated: 3-tuple, leaf inference (no substitution contribution).
inferNamespaceAccess :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferNamespaceAccess _ env e@(Can.Canonical area (Can.Access (Can.Canonical _ (Can.Var ns)) (Can.Canonical _ (Can.Var field))))
  = do
    sc <-
      catchError
        (lookupVar env (ns <> field))
        (\_ -> enhanceVarError env e area (CompilationError (UnboundVariableFromNamespace ns (tail field)) NoContext))
    (ps :=> t) <- instantiate sc
    let ps' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps

    let e = Slv.Typed (ps :=> t) area $ Slv.Var (ns <> field) (isConstructor env (ns <> field))

    return (ps', t, e)
inferNamespaceAccess _ _ _ = throwError $ CompilationError FatalError NoContext



-- INFER FIELD ACCESS

-- | Phase 1 migrated: 3-tuple. extSubsts each sub-inference and unification
-- contribution into state (HM convention).
inferFieldAccess :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferFieldAccess options env fa@(Can.Canonical area (Can.Access rec@(Can.Canonical _ _) abs))
  = do
    discardError <- isDiscardingErrors
    tv                  <- newTVar Star
    (s1, _  , t1, eabs) <- infer options env abs
    extSubst s1
    env1 <- applyCurrentSubst env
    (s2, ps2, t2, earg) <- infer options env1 rec
    extSubst s2

    t1' <- applyCurrentSubst t1
    t2' <- applyCurrentSubst t2
    s3 <- catchError
      (contextualUnifyAccess env fa t1' (t2' `fn` tv))
      (\err -> do
        if discardError then do
          return $ gentleUnify t1' (t2' `fn` tv)
        else
          throwError err
      )
    extSubst s3

    t <- applyCurrentSubst tv
    let solved = Slv.Typed (ps2 :=> t) area (Slv.Access earg eabs)

    return (ps2, t, solved)



-- INFER IF

-- | Phase 1 migrated: 3-tuple. extSubsts each sub-inference and unification
-- contribution into state. The legacy `apply (s2 \`compose\` s1) env` pattern
-- is replaced by `applyCurrentSubst env` which reads from the cumulative state.
inferIf :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferIf options env (Can.Canonical area (Can.If cond truthy falsy)) = do
  discardError <- isDiscardingErrors
  (s1, ps1, tcond, econd) <- infer options env cond
  extSubst s1
  env1 <- applyCurrentSubst env
  (s2, ps2, ttruthy, etruthy) <- infer options env1 truthy
  extSubst s2
  env2 <- applyCurrentSubst env
  (s3, ps3, tfalsy, efalsy) <- infer options env2 falsy
  extSubst s3

  tfalsy' <- applyCurrentSubst tfalsy
  ttruthy' <- applyCurrentSubst ttruthy
  let unifyBranches = contextualUnifyWithOrigin (if discardError then Discard else Strict) (FromIfBranches ElseBranch) env falsy tfalsy' ttruthy'
  s4 <- catchError unifyBranches (flipUnificationErrorWithBranch ThenBranch)
  extSubst s4
  tcond' <- applyCurrentSubst tcond
  s5 <- contextualUnifyWithOrigin (if discardError then Discard else Strict) FromIfCondition env cond tBool tcond'
  extSubst s5

  t <- applyCurrentSubst ttruthy

  return (ps1 ++ ps2 ++ ps3, t, Slv.Typed ((ps1 ++ ps2 ++ ps3) :=> t) area (Slv.If econd etruthy efalsy))



-- INFER While

-- | Phase 1 migrated: 3-tuple. extSubsts each sub-inference and unification
-- contribution into state.
inferWhile :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferWhile options env (Can.Canonical area (Can.While cond body)) = do
  discardError <- isDiscardingErrors
  (s1, ps1, tcond, econd) <- infer options env cond
  extSubst s1
  env1 <- applyCurrentSubst env
  (s2, ps2, tbody, ebody) <- infer options env1 body
  extSubst s2

  tcond' <- applyCurrentSubst tcond
  s4 <- contextualUnifyWithOrigin (if discardError then Discard else Strict) FromWhileCondition env cond tBool tcond'
  extSubst s4
  tbody' <- applyCurrentSubst tbody
  s5 <- contextualUnify' env discardError body tUnit tbody'
  extSubst s5

  t <- applyCurrentSubst tbody

  return (ps1 ++ ps2, t, Slv.Typed ((ps1 ++ ps2) :=> t) area (Slv.While econd ebody))



-- INFER DO

-- | Phase 1 migrated: 3-tuple. inferBody is now also 3-tuple (state-based);
-- postProcessBody is still legacy and is bridged via extSubst.
inferDo :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferDo options env (Can.Canonical area (Can.Do exps)) = do
  (ps, t, exps') <- inferBody options env exps
  s <- getSubst
  exps''         <- postProcessBody options env s t exps'
  ps'  <- applyCurrentSubst ps
  t'   <- applyCurrentSubst t
  qt'  <- applyCurrentSubst (ps :=> t)

  return (ps', t', Slv.Typed qt' area (Slv.Do exps''))



-- INFER WHERE

-- | Phase 1 migrated: 3-tuple. Each branch's subst is composed into state via
-- extSubst (HM convention).
inferWhere :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferWhere options env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e)          <- infer options env exp
  extSubst s
  tv                     <- newTVar Star
  pssRev <- foldM
    (\(res, idx) is -> do
      env'  <- applyCurrentSubst env
      tv'   <- applyCurrentSubst tv
      t'    <- applyCurrentSubst t
      r <- inferBranch options env' tv' t' idx is
      -- inferBranch is now 2-tuple ([Pred], Slv.Is) and extSubsts state itself.
      return (r : res, idx + 1)
    )
    ([], 1)
    iss >>= \(rs, _) -> return rs
  let pss = reverse pssRev

  let ps' = concat $ fst <$> pss
  issSubstitution <- getSubst
  s' <- contextualUnifyElems env $ zip iss (apply issSubstitution . Slv.getType . snd <$> pss)
  extSubst s'

  s''  <- getSubst
  let iss' = (\(Slv.Typed t a (Slv.Is pat exp)) -> Slv.Typed (apply s'' t) a (Slv.Is (updatePatternTypes s'' mempty pat) exp)) . snd <$> pss
  let wher = Slv.Typed (apply s'' $ (ps ++ ps') :=> tv) area $ Slv.Where (updateQualType e (apply s'' $ ps :=> t)) iss'
  return (ps ++ ps', apply s'' tv, wher)


-- | Phase 1 migrated: 3-tuple. Same legacy reversed compose order pattern as
-- inferAssignment (`s `compose` s' `compose` s''` — applies s'' first, s last).
-- We compute the composition explicitly and extSubst the result so the
-- migrated inferWhere caller picks up the contribution via state.
inferBranch :: Options -> Env -> Type -> Type -> Int -> Can.Is -> Infer ([Pred], Slv.Is)
inferBranch options env tv t branchIdx (Can.Canonical area (Can.Is pat exp)) = do
  discardError <- isDiscardingErrors
  (pat', ps, vars, t') <- inferPattern env pat
  s <- contextualUnifyWithOrigin (if discardError then Discard else Strict) (FromPatternMatch branchIdx) env exp t t'

  -- Fix rest variable types: after unification, row variables get substituted with
  -- records that include ALL fields (because optional fields merge into main fields
  -- during compose). For rest pattern variables like `...g`, we subtract the explicitly
  -- matched fields to get only the "remaining" fields.
  let vars' = fixRestVarTypes s pat vars

  let patternBoundNames = M.keysSet vars'
  let envWithPatternVars = (apply s $ mergeVars env vars')
        { envPatternBoundNames = envPatternBoundNames env <> patternBoundNames }
  (s', ps', t'', e') <- infer options envWithPatternVars exp
  s'' <- contextualUnify' env discardError exp tv (apply (s `compose` s') t'')

  let subst = s `compose` s' `compose` s''
  let allPreds = ps ++ ps'
  extSubst subst

  return
    ( allPreds
    , Slv.Typed (allPreds :=> apply subst (t' `fn` tv)) area
      $ Slv.Is (updatePatternTypes subst (apply s <$> vars') pat') (updateQualType e' (ps' :=> apply subst t''))
    )



-- INFER TYPEDEXP

-- | Phase 1 migrated: 3-tuple. extSubsts both the inner inference's
-- substitution and the unification's substitution into state.
inferTypedExp :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferTypedExp options env e@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  discardError <- isDiscardingErrors
  (_ :=> t) <- instantiate sc
  (s1, ps1, t1, e1) <- infer options env exp
  extSubst s1
  s2 <- contextualUnify' env discardError e t t1
  extSubst s2

  ps1' <- applyCurrentSubst ps1
  t1'  <- applyCurrentSubst t1
  qt'  <- applyCurrentSubst (ps1 :=> t1)

  return
    ( ps1'
    , t1'
    , Slv.Typed qt' area (Slv.TypedExp (updateQualType e1 (ps1 :=> t1)) (updateTyping typing) sc)
    )


-- | Phase 1 migrated: 3-tuple, leaf inference (no substitution contribution).
inferExtern :: Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferExtern _ (Can.Canonical area (Can.Extern scheme name originalName)) = do
  qt@(ps :=> t) <- instantiate scheme
  return (ps, t, Slv.Typed qt area (Slv.Extern qt name originalName))


-- Predicate solving + generalization helpers were moved to Infer.Generalize
-- during Phase 5 of the typechecker rewrite. They are re-imported here so
-- callers in this module continue to use the same names.



-- | Phase 1 migrated: 3-tuple. extSubst's the final substitution at the end
-- so the value is visible to inferBody (which calls applyCurrentSubst).
inferImplicitlyTyped :: Options -> Bool -> Env -> Can.Exp -> Infer (([Pred], [Pred]), Env, Slv.Exp)
inferImplicitlyTyped options isLet env exp@(Can.Canonical area _) = do
  discardError <- isDiscardingErrors
  (env', tv) <- case Can.getExpName exp of
    Just n -> case M.lookup n (envVars env) of
      Just sc -> do
        _ :=> t' <- instantiate sc
        return (env, t')
        --  ^ if a var is already present we don't override its type with a fresh var.

      Nothing -> do
        tv <- newTVar Star
        return (extendVars env (n, Forall [] $ [] :=> tv), tv)

    Nothing -> do
      tv <- newTVar Star
      return (env, tv)

  (s, ps, t, e) <- infer options env' { envNamesInScope = envVars env } exp
  let env'' = apply s env'

  s' <- contextualUnify' env'' discardError exp (apply s tv) t
  let s'' = s `compose` s' `compose` s
      envWithVarsExcluded = env''
        { envVars = M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $ envVars env'' }

      ps' = apply s'' ps
      t'  = apply s'' tv

  (ds, rs', sFinal) <- generalize isLet env area s'' envWithVarsExcluded t' ps' (apply s'' tv)
  extSubst sFinal

  let bindingName = Can.getExpName exp
  bindingMutated <- case bindingName of
    Just n  -> isMutated n
    Nothing -> return False

  let vs = if isLet then ftvForLetGen t' else ftvList t'
      fsSet = ftv (apply sFinal envWithVarsExcluded)
      fs = S.toList fsSet
      gs = filter (not . (`S.member` fsSet)) vs
      sc =
        if bindingMutated then
          -- Mutated bindings are monomorphic (value-restriction-style).
          apply sFinal $ quantify [] (rs' :=> t')
        else if isLet && not (Slv.isNamedAbs e) then
          apply sFinal $ quantify [] (rs' :=> t')
        else
          -- TODO: consider if the apply sFinal should not happen before quantifying
          -- because right now we might miss the defaulted types in the generated
          -- scheme
          apply sFinal $ quantify gs (rs' :=> t')

  when (not isLet && not discardError && bindingMutated && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  case bindingName of
    Just n  ->
      return ((ds, rs'), extendVars env (n, sc), updateQualType e (apply sFinal $ rs' :=> t'))

    Nothing ->
      return ((ds, rs'), env, updateQualType e (apply sFinal $ rs' :=> t'))


-- | Phase 1 migrated: 3-tuple. extSubsts the cumulative substitution at
-- the end so callers (inferBody / inferExp) see this binding's contribution.
-- inferExp's captureDelta wrapper scopes the contribution to a single
-- top-level iteration so nothing leaks across iterations of inferExps.
inferExplicitlyTyped :: Options -> Bool -> Env -> Can.Exp -> Infer ([Pred], Env, Slv.Exp)
inferExplicitlyTyped options isLet env canExp@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  discardError <- isDiscardingErrors
  qt@(qs :=> t') <- instantiate sc

  env' <- case Can.getExpName exp of
        Just n  -> do
          let scWithParents = quantify (ftvList qt) (qs :=> t')
          return $ extendVars env (n, scWithParents)

        Nothing ->
          return env

  (s, ps, t, e) <- infer options env' { envNamesInScope = envVars env } exp
  psFull        <- concat <$> mapM (gatherInstPreds env') ps
  let sNorm = s `compose` s -- resolve internal substitution chains
  s'' <- catchError (contextualUnifyWithOrigin (if discardError then Discard else Strict) FromTypeAnnotation env canExp t' (apply sNorm t)) (throwError . limitContextArea 2)
  let s' = s'' `compose` sNorm

  let envWithVarsExcluded =
        env'
          {
            envVars =
              if isLet then
                M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $ envVars env'
              else
                envVars env'
          }
      qs'  = apply s' qs
      t''  = apply s' t
      t''' = mergeRecords (apply s' t') t''
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' psFull)
  (ds, rs, substDefaultResolution) <- generalize False env area s' envWithVarsExcluded (apply s' t') ps' t

  let bindingName = Can.getExpName exp
  bindingMutated <- case bindingName of
    Just n  -> isMutated n
    Nothing -> return False

  when (not isLet && not discardError && bindingMutated && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  let qs'' = dedupePreds qs'
      fsSet = ftv (apply s' envWithVarsExcluded)
      fs = S.toList fsSet
      gs = filter (not . (`S.member` fsSet)) (ftvList (apply s' t'))
      scCheck  = quantify (ftvList (apply s' t')) (qs' :=> apply substDefaultResolution (apply s' t'))
  sigCheckResult <- if sc /= scCheck then
    -- The inferred scheme differs from the declared scheme.
    -- Check if the declared type subsumes the inferred type.
    -- This handles cases like record spreads where the inference over-constrains
    -- the input type (e.g., { ...input, time: now() } makes input :: { time: ..., ...r }
    -- but the declared type correctly says input :: { ...r }).
    -- However, we reject annotations where a plain type variable is bound to a
    -- compound type (record, applied type), indicating the annotation is too general.
    catchError (do
      (ps1 :=> t1) <- instantiate sc
      let annotationVars = ftv t1
      (ps2 :=> t2) <- instantiate scCheck
      s <- unify t1 t2
      -- Check that no top-level annotation type variable was bound to a compound type.
      -- This catches e.g. `f :: a -> b` where the inferred type is `{ name :: String } -> String`.
      -- But allows row variables in records (e.g. `{ ...r }` annotation where r absorbs extra fields).
      let topLevelVars = collectTopLevelParamVars t1
      let tooGeneral = any (\tv -> isCompoundBinding (apply s (TVar tv))) (S.toList topLevelVars)
      return (not tooGeneral)
    ) (const $ return False)
  else
    return True

  if not sigCheckResult then
    throwError $ CompilationError (SignatureTooGeneral sc scCheck) (Context (envCurrentPath env') area)
  else if not (null rs) then
    throwError $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) area)
  else do
    let e'   = updateQualType e (ds :=> t''')

    let qt'  = qs'' :=> t'''
    let sc'' = quantify gs qt'
    let env'' = case Can.getExpName exp of
          Just n  ->
            extendVars env' (n, sc'')
          Nothing ->
            env'

    extSubst (substDefaultResolution `compose` s')
    return (qs'', env'', Slv.Typed (qs :=> t') area (Slv.TypedExp e' (updateTyping typing) sc))

inferExplicitlyTyped _ _ _ _ = error "inferExplicitlyTyped: unreachable case"


inferExps :: Options -> Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps _ env []       = return ([], env)

inferExps options env (e : es) = do
  -- Strict pass first. On error, push it and retry in best-effort mode
  -- (withDiscardErrors) so the rest of the file still types. If even that
  -- fails, fall back to a placeholder solved expression so subsequent
  -- definitions can still see this binding's name in the env.
  --
  -- State subst is rolled back on error so the failing pass doesn't pollute
  -- the next attempt. Without this rollback, partial substitutions from a
  -- failed strict pass leak into the best-effort retry and produce wrong
  -- types in surrounding bindings.
  let try strict = do
        savedSubst <- getSubst
        catchError
          (if strict then inferExp options env e
                     else withDiscardErrors (inferExp options env e))
          (\err -> do
            putSubst savedSubst
            throwError err)
  (e' , env'   ) <-
    catchError (try True) $ \err -> do
      pushError err
      catchError (try False)
        (\_ -> return (Just (toSolved e), env))
  (es', nextEnv) <- inferExps options env' es

  case e' of
    Just e'' ->
      return (e'' : es', nextEnv)

    Nothing  ->
      return (es', nextEnv)


inferExp :: Options -> Env -> Can.Exp -> Infer (Maybe Slv.Exp, Env)
inferExp _ env (Can.Canonical _ (Can.TypeExport _)) =
  return (Nothing, env)
inferExp options env e = do
  -- Wrap the per-binding inference in captureDelta so substitutions from
  -- this top-level binding are scoped to it: state at the start of the next
  -- inferExps iteration is the same as at the start of this one. Without
  -- this scoping, placeholder tvars bound during one iteration's body
  -- inference (e.g. f's body's reference to forward-declared g) would leak
  -- into a later iteration (g's actual definition) and the compose-on-state
  -- left-bias would silently swallow the new binding.
  (delta, (s, _, env', e')) <-
    captureDelta $
      upgradeContext env (Can.getArea e) $ case e of
        Can.Canonical _ Can.TypedExp{} -> do
          (ps, env'', e') <- inferExplicitlyTyped options False env e
          s <- getSubst
          return (s, ps, env'', e')

        _ -> do
          ((_, placeholderPreds), env'', e') <- inferImplicitlyTyped options False env e
          s <- getSubst
          return (s, placeholderPreds, env'', e')

  -- Use the per-binding delta for the rest of the legacy AST update pass.
  -- We deliberately do NOT re-extSubst delta into state (it's scoped to
  -- this iteration only).
  let _ = (delta, s)  -- 'delta' is the captured per-binding contribution; 's' carries it through legacy code

  e'' <- updateExpTypes options env' False s e'

  return (Just e'', env')


recordError :: Env -> Can.Exp -> CompilationError -> Infer (Maybe Slv.Exp, Env)
recordError env e err = do
  pushError err
  return (Just $ toSolved e, env)


upgradeContext :: Env -> Area -> Infer a -> Infer a
upgradeContext env area a = catchError a (throwError . upgradeContext' env area)


upgradeContext' :: Env -> Area -> CompilationError -> CompilationError
upgradeContext' env area err = case err of
  (CompilationError e NoContext) ->
    CompilationError e $ Context (envCurrentPath env) area

  (CompilationError e r) ->
    CompilationError e r
