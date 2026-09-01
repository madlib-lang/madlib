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
import           Utils.EditDistance              ( findSimilar )
import qualified Control.Monad                 as CM
import           AST.Solved (getType)
import qualified Data.Set as Set
import           Run.Options
import qualified Data.List as List
import           Data.Char (isAlphaNum)


-- | Extract an ErrorOrigin from a function application expression.
-- For operators like +, &&, etc., returns FromOperator.
-- For named functions, returns FromFunctionArgument with name and arg index —
-- the index is the position of the argument being applied at the outermost
-- App node, however deep the application spine goes.
getAppOrigin :: Can.Exp -> ErrorOrigin
getAppOrigin = go 1
  where
    go argIndex (Can.Canonical _ expr) = case expr of
      Can.Var name
        | isOperatorName name -> FromOperator name
        | otherwise           -> FromFunctionArgument name argIndex Nothing
      Can.Access (Can.Canonical _ (Can.Var ns)) (Can.Canonical _ (Can.Var ('.' : field))) ->
        FromFunctionArgument (ns <> "." <> field) argIndex Nothing
      Can.App fn _ _ ->
        go (argIndex + 1) fn
      _ -> NoOrigin

    isOperatorName []    = False
    isOperatorName (c:_) = not (isAlphaNum c) && c /= '_' && c /= '.'


-- | Check if a type is Maybe a (for JSX optional prop defaulting)
isMaybeType :: Type -> Bool
isMaybeType (TApp (TCon (TC "Maybe" _) _ _) _) = True
isMaybeType _ = False


-- | Given the instantiated annotation type and the substitution obtained by
-- unifying it with the inferred type, decide whether the annotation is as
-- polymorphic as it claims. That is the case exactly when the substitution
-- restricted to the annotation's variables is an injective renaming: every
-- annotation variable maps to a distinct type variable. An annotation
-- variable bound to anything concrete (`a -> String` implemented with
-- `(x) => x ++ "!"`) or two annotation variables collapsed into one
-- (`m a -> m b` implemented as identity) mean the signature is too general.
-- Row variables are no exception: a signature is a public contract and must
-- not be silently narrowed by a field access in its implementation.
isInjectiveRenaming :: Type -> Substitution -> Bool
isInjectiveRenaming tAnnotation s =
  let annotationVars = S.toList (ftv tAnnotation)
      imageVars = [tv | TVar tv <- map (\tv -> apply s (TVar tv)) annotationVars]
  in  length imageVars == length annotationVars
        && S.size (S.fromList imageVars) == length imageVars


numericDefaultsForClosedResult :: Type -> Type -> [Pred] -> Substitution
numericDefaultsForClosedResult declared inferred preds
  | not (S.null (ftv (getReturnType declared))) = M.empty
  | otherwise =
      let inferredResultVars = ftv (getReturnType inferred)
          numericResultVars = S.fromList
            [ tv
            | IsIn cls [TVar tv] _ <- preds
            , cls == "Number" || cls == "Bits"
            , tv `S.member` inferredResultVars
            ]
      in  M.fromList [(tv, tInteger) | tv <- S.toList numericResultVars]


numericDefaultsForMatchedScrutinee :: Slv.Exp -> [Pred] -> Substitution
numericDefaultsForMatchedScrutinee solved preds =
  let scrutineeVars = go solved
  in  M.fromList
        [ (tv, tInteger)
        | IsIn cls [TVar tv] _ <- preds
        , cls == "Number" || cls == "Bits"
        , tv `S.member` scrutineeVars
        ]
  where
    go (Slv.Typed _ _ (Slv.Assignment _ rhs)) = go rhs
    go (Slv.Typed _ _ (Slv.Export rhs)) = go rhs
    go (Slv.Typed _ _ (Slv.TypedExp rhs _ _)) = go rhs
    go (Slv.Typed _ _ (Slv.Where scrutinee _)) = ftv (getType scrutinee)
    go _ = S.empty


-- | All inference is state-based. Substitutions accumulate into
-- `currentSubst` via `extSubst` / `unifyM`; nothing flows through the return
-- tuple. Callers that need explicit access to the contribution wrap the call
-- with `captureDelta`.
infer :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
infer options env lexp = do
  let (Can.Canonical area exp) = lexp
  -- Track the nearest enclosing expression's span so that errors thrown deep
  -- inside unification (which have no location of their own) can be stamped
  -- with it instead of reaching the user locationless.
  withCurrentSpan (envCurrentPath env) area $ case exp of
    Can.LNum  _               -> do
      t <- newTVar Star
      let ps = [IsIn "Number" [t] Nothing]
      return (ps, t, applyLitSolve lexp (ps :=> t))

    Can.LByte _               -> return ([], tByte, applyLitSolve lexp ([] :=> tByte))
    Can.LShort _              -> return ([], tShort, applyLitSolve lexp ([] :=> tShort))
    Can.LInt _                -> return ([], tInteger, applyLitSolve lexp ([] :=> tInteger))
    Can.LFloat _              -> return ([], tFloat, applyLitSolve lexp ([] :=> tFloat))
    Can.LStr  _               -> return ([], tStr, applyLitSolve lexp ([] :=> tStr))
    Can.LChar  _              -> return ([], tChar, applyLitSolve lexp ([] :=> tChar))
    Can.LBool _               -> return ([], tBool, applyLitSolve lexp ([] :=> tBool))
    Can.LUnit                 -> return ([], tUnit, applyLitSolve lexp ([] :=> tUnit))
    Can.TemplateString _      -> inferTemplateString options env lexp

    Can.Var            _      -> inferVar options env lexp
    Can.Abs _ _               -> inferAbs options env lexp
    Can.App{}                 -> inferApp options env lexp
    Can.Assignment _ _        -> inferAssignment options env lexp
    Can.Mutate _ _            -> inferMutate options env lexp
    Can.Do _                  -> inferDo options env lexp
    Can.Where      _ _        -> inferWhere options env lexp
    Can.Record _              -> inferRecord options env lexp
    Can.JsxRecord _           -> inferJsxRecord options env lexp
    Can.Access   _ _          -> inferAccess options env lexp
    Can.ArrayAccess   _ _     -> inferArrayAccess options env lexp
    Can.TypedExp{}            -> inferTypedExp options env lexp
    Can.ListConstructor  _    -> inferListConstructor options env lexp
    Can.TupleConstructor _    -> inferTupleConstructor options env lexp
    Can.Export           _    -> inferExport options env lexp
    Can.NameExport       _    -> inferNameExport env lexp
    Can.TypeOf           _    -> inferTypeOf options env lexp
    Can.If{}                  -> inferIf options env lexp
    Can.While{}               -> inferWhile options env lexp
    Can.Extern{}              -> inferExtern env lexp
    Can.TypedHole             -> do
      t <- newTVar Star
      return ([], t, Slv.Typed ([] :=> t) area Slv.TypedHole)

    Can.JSExp c               -> do
      t <- newTVar Star
      return ([], t, Slv.Typed ([] :=> t) area (Slv.JSExp c))


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
    let s = Forall [Star, Row] $ [] :=> (openRecord (M.fromList [(name, TGen 0)]) (TGen 1) `fn` TGen 0)
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


-- INFER TYPEOF

inferTypeOf :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferTypeOf options env (Can.Canonical area (Can.TypeOf inner)) = do
  (_ps, _t, e) <- infer options env inner
  let runtimeType = runtimeTypeAt (envBuiltinsModulePath env)
  let e' = Slv.Typed ([] :=> runtimeType) area (Slv.TypeOf e)
  return ([], runtimeType, e')



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


-- | An explicit record argument type is useful evidence while checking the
-- body: a spread update can otherwise solve its base row before an earlier
-- use establishes that an overwritten label is required.  Restricting this
-- to annotation-backed record parameters keeps ordinary inference fully HM.
inferAbsWithExpectedParam :: Options -> Env -> Type -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferAbsWithExpectedParam options env expected l@(Can.Canonical _ (Can.Abs p@(Can.Canonical area param) body)) = do
  env'        <- extendAbsEnv env expected p
  (ps, t, es) <- inferBody options env' { envInBody = True } body
  s           <- getSubst
  es'         <- postProcessBody options env' s (expected `fn` t) es
  s'          <- getSubst

  let t'        = apply s' (expected `fn` t)
      paramType = apply s' expected

  return (apply s' ps, t', applyAbsSolve l (Slv.Typed (apply s' $ ps :=> paramType) area param) es' (apply s' $ ps :=> t'))
inferAbsWithExpectedParam _ _ _ _ = error "inferAbsWithExpectedParam: expected abstraction"


-- | Phase 1 migrated: 3-tuple. With transactional `captureDelta`, state's
-- currentSubst correctly reflects only this frame's contributions while
-- inside the action, so we can use applyCurrentSubst to get the cumulative
-- subst at any point.
inferBody :: Options -> Env -> [Can.Exp] -> Infer ([Pred], Type, [Slv.Exp])
inferBody options env [e] = do
  (ps, t, e') <- infer options env e
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
  -- Defaulting is a property of the whole lexical block, not whichever
  -- expression happens to be post-processed first.  In particular, an
  -- equality assertion over a local `Nothing` must see a sibling `Number a`
  -- use before it is allowed to choose the legacy Unit default for `a`.
  let blockPreds = concat [ps | Slv.Typed (ps :=> _) _ _ <- es]
      functionProtected = S.unions
        [ ftvForLetGenSet t
        | typed@(Slv.Typed (_ :=> t) _ Slv.Assignment{}) <- es
        , Slv.isNamedAbs typed
        ]
      -- Non-function assignments are deliberately value-restricted.  Their
      -- variables remain monomorphic throughout the block and therefore must
      -- not be specialized to Unit merely to discharge a later Eq/Show use.
      -- Numeric defaulting remains available for them.
      unitProtected = S.unions
        [ ftv t
        | typed@(Slv.Typed (_ :=> t) _ Slv.Assignment{}) <- es
        , not (Slv.isNamedAbs typed)
        ]
  -- Accumulate reversed (cons O(1)) then reverse at end — avoids O(n²) with ++
  (esRev, s', _) <- foldM
    (\(resultsRev, accSubst, env'') (Slv.Typed (ps' :=> t') area e) -> do
      let ps'' = apply accSubst ps'
          -- Function-local variables are part of the binding's inferred
          -- scheme and cannot be defaulted while processing its definition.
          fs = S.toList $
            ftv (apply accSubst env'')
              `S.union` ftv (apply accSubst expType)
              `S.union` ftvForLetGenSet (apply accSubst t')
              `S.union` ftv (apply accSubst (TVar <$> S.toList functionProtected))

      (ps''', substFromDefaulting) <- do
        prep <- CM.forM ps'' $ \p -> do
          isResolved <- entail env [] p
          return (p, isResolved)

        let solvedPs = [p | (p, True) <- prep]
        let unsolvedPs = [p | (p, False) <- prep]

        let currentCandidates = S.fromList (fst <$> ambiguities fs unsolvedPs)

        if not (S.null currentCandidates) then do
          let
              blockVars = ftv (apply accSubst blockPreds)
              blockFs = S.toList $
                S.fromList fs `S.union` (blockVars `S.difference` currentCandidates)
          let defaultableBlockPreds = filter
                (S.null . (`S.intersection` functionProtected) . ftv)
                (apply accSubst blockPreds)
          (sDef, _) <- tryDefaultingAmbiguitiesExcept
            env
            (ftv (apply accSubst (TVar <$> S.toList unitProtected)))
            blockFs
            defaultableBlockPreds
          let fsAfterFirst = S.toList (ftv (apply sDef (TVar <$> fs)))
              unsolvedAfterFirst = apply sDef unsolvedPs
              candidatesAfterFirst = S.fromList (fst <$> ambiguities fsAfterFirst unsolvedAfterFirst)
              blockVarsAfterFirst = ftv (apply sDef (apply accSubst blockPreds))
              blockFsAfterFirst = S.toList $
                S.fromList fsAfterFirst
                  `S.union` (blockVarsAfterFirst `S.difference` candidatesAfterFirst)
          (sDef', _) <- tryDefaultingAmbiguitiesExcept
            env
            (ftv (apply sDef (apply accSubst (TVar <$> S.toList unitProtected))))
            blockFsAfterFirst
            (apply sDef defaultableBlockPreds)
          let subst = sDef' `compose` sDef
              defaultedPs = apply subst unsolvedPs

          -- Defaulting may turn an ambiguous wanted into a concrete, entailed
          -- predicate (for example Number a into Number Integer).  Keep only
          -- the residual wanteds; retaining already-proved predicates here
          -- leaks evidence into every typed child and can make downstream
          -- monomorphization treat solved constraints as live.
          unsolvedPs'' <- filterM ((not <$>) . entail env []) defaultedPs

          if unsolvedPs'' /= [] then do
            CM.forM_ unsolvedPs'' $ \p -> do
              catchError
                (byInst env (apply subst p))
                (\case
                  _ | discardError ->
                    return []

                  (CompilationError FatalError _) ->
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
      e' <- updateExpTypes options env False sFinal
        (Slv.Typed (apply sFinal $ ps''' :=> t') area e)

      return (e' : resultsRev, sFinal, apply sFinal env'')
    )
    (mempty, s, env)
    es

  -- The block result may default variables from later expressions.  Never
  -- let that final substitution export a binding for variables quantified by
  -- an earlier local function: those variables belong to the definition's
  -- scheme, while each later use has its own fresh instantiation.
  let escapingSubst = foldr M.delete s' (S.toList functionProtected)
  extSubst escapingSubst
  return (reverse esRev)


-- INFER APP

-- | Phase 1 migrated: 3-tuple. extSubsts each sub-inference and unification
-- contribution into state (HM convention).
inferApp :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferApp options env (Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea argContent) final)) = do
  discardError <- isDiscardingErrors
  tv                  <- newTVar Star
  (ps1, t1, eabs) <- infer options env abs
  env1 <- applyCurrentSubst env
  (ps2, t2, earg) <- infer options env1 arg

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
  s3 <- catchError
    (contextualUnifyWithOriginAndSecondary (if discardError then Discard else Strict) origin secondaryLoc env expForContext t1Applied (t2Applied `fn` tv))
    (\err -> case err of
      -- funcType (t1) is no longer a function at this application depth: the
      -- caller supplied more arguments than the function accepts. Report the
      -- over-application explicitly rather than letting it read as a mismatch
      -- between the extra argument and whatever funcType happens to be.
      CompilationError (UnificationError tm) ctx
        | FromFunctionArgument fn idx _ <- baseOrigin
        , not (isFunctionType funcType) ->
          throwError $ CompilationError (UnificationError tm { tmOrigin = TooManyArguments fn (idx - 1) }) ctx

      -- The application-level unification pairs the function's remaining type
      -- with `argType -> ret`, but the reader cares about the operand: when
      -- the function type is known, report (arg, param) instead of two
      -- partially applied function types.
      CompilationError (UnificationError tm) ctx
        | originWantsOperandPair (tmOrigin tm)
        , (param : _) <- getParamTypes t1Applied ->
          throwError $ CompilationError (UnificationError tm { tmFound = t2Applied, tmExpected = param }) ctx

      _ ->
        throwError err
    )
  extSubst s3

  t <- applyCurrentSubst tv

  -- For JSX records: fill missing Maybe-typed fields with Nothing
  earg' <- case argContent of
    Can.JsxRecord jsxFields -> do
      let explicitNames = S.fromList [ n | Can.Canonical _ (Can.Field (n, _)) <- jsxFields ]
      resolvedArgType <- applyCurrentSubst t2
      case recordVisibleFields resolvedArgType of
        Just allFields -> do
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
  where
    originWantsOperandPair o = case o of
      FromFunctionArgument{} -> True
      FromOperator{}         -> True
      _                      -> False



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
      (ps, t, e) <- infer options env' exp
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

-- | Phase 1 migrated: 3-tuple. The inferred expression contribution is
-- followed by signature defaulting and then the placeholder unifier.
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
  (s1, (ps1, t1, e1)) <- captureDelta $
    case (exp, getParamTypes currentType) of
      (absExp@(Can.Canonical _ Can.Abs{}), hintedParam : _)
        | Just _ <- recordParts hintedParam ->
            inferAbsWithExpectedParam options env' hintedParam absExp
      _ -> infer options env' exp
  let inferredType = apply s1 t1
      inferredPreds = apply s1 ps1
      signatureDefaults = numericDefaultsForClosedResult
        (apply s1 currentType) inferredType inferredPreds
      checkingSubst = signatureDefaults `compose` s1
  s2 <- catchError
    (contextualUnify Strict env' e
      (apply checkingSubst currentType)
      (apply checkingSubst t1))
    (const $ return M.empty)
  --  ^ We can skip this error as we mainly need the substitution. It would fail in inferExplicitlyTyped anyways.
  let s = s2 `compose` checkingSubst
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

-- | Phase 1 migrated: 3-tuple. Compose contributions in inference order:
-- lhs, rhs, then the assignment equality.
inferMutate :: Options -> Env -> Can.Exp -> Infer ([Pred], Type, Slv.Exp)
inferMutate options env e@(Can.Canonical area (Can.Mutate lhs exp)) = do
  discardError <- isDiscardingErrors
  (s1, (ps1, t1, e1)) <- captureDelta (infer options env lhs)
  (s2, (ps2, t2, e2)) <- captureDelta (infer options (apply s1 env) exp)
  let assignOrigin = maybe NoOrigin FromAssignment (Can.getExpName lhs)
  let inferredSubst = s2 `compose` s1
  s3 <- catchError
    (contextualUnifyWithOrigin Strict assignOrigin env e
      (apply inferredSubst t1)
      (apply inferredSubst t2))
    (\err -> do
      if discardError then do
        return mempty
      else
        throwError err
    )

  let s  = s3 `compose` inferredSubst
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
  (ps, t, e) <- infer options env exp
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


-- | Phase 1 migrated: 3-tuple. The element inference contribution is
-- followed by the list-shape unifier.
inferListItem :: Options -> Env -> Type -> Can.ListItem -> Infer ([Pred], Type, Slv.ListItem)
inferListItem options env _ (Can.Canonical area li) = do
  discardError <- isDiscardingErrors
  case li of
    Can.ListItem exp -> do
      (ps, t, e) <- infer options env exp
      return (ps, t, Slv.Typed (ps :=> t) area $ Slv.ListItem e)

    Can.ListSpread exp -> do
      (s1, (ps, t, e)) <- captureDelta (infer options env exp)
      tv <- newTVar Star
      s2 <- contextualUnify' env discardError exp (tListOf tv) (apply s1 t)

      let s = s2 `compose` s1
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
          (ps', t', e') <- infer options envApplied e
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
    Just (TRecordRow spreadRow optionalFields) ->
      -- Keep the base row verbatim.  `rowFromFields` places explicit fields
      -- outside it, so an equal tail label is shadowed rather than merged.
      return $ TRecordRow (rowFromFields (M.fromList fieldTypes') spreadRow) optionalFields

    Just tBase -> do
      -- Constrain the spread operand to be a record, but do not put the
      -- fields being written into its input row.  Spread is an overwrite:
      -- `{ ...r, x: value }` accepts both rows with x and rows without x;
      -- the outer x shadows any x in the base row.
      baseVar <- newTVar Row
      let recordWithBase = recordRow baseVar
      s <- contextualUnify' env discardError exp tBase recordWithBase
      extSubst s
      return (recordRow (rowFromFields (M.fromList fieldTypes') baseVar))

    Nothing ->
      return (recordRow (rowFromFields (M.fromList fieldTypes') TRowEmpty))

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
    Just (TRecordRow spreadRow optionalFields) ->
      return $ TRecordRow (rowFromFields (M.fromList fieldTypes') spreadRow) optionalFields

    Just tBase -> do
      baseVar <- newTVar Row
      let recordWithBase = recordRow baseVar
      s <- contextualUnify' env discardError exp tBase recordWithBase
      extSubst s
      return (recordRow (rowFromFields (M.fromList fieldTypes') baseVar))

    Nothing -> do
      -- JSX record without spread: create an EXTENSIBLE record with a base type variable.
      -- This allows unification to absorb missing fields into the base, which we later
      -- check are all Maybe-typed and fill with Nothing.
      baseVar <- newTVar Row
      return (recordRow (rowFromFields (M.fromList fieldTypes') baseVar))

  let allPS = concat fieldPS
  recordType' <- applyCurrentSubst recordType

  return (allPS, recordType', Slv.Typed (allPS :=> recordType') area (Slv.Record fieldEXPS))


-- | Phase 1 migrated: 3-tuple. extSubst-s the inner inference into state.
inferRecordField :: Options -> Env -> Can.Field -> Infer ([Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField options env (Can.Canonical area field) = do
  discardError <- isDiscardingErrors
  case field of
    Can.Field (name, exp) -> do
      (ps, t, e) <- infer options env exp
      return (ps, [(name, t)], Slv.Typed (ps :=> t) area $ Slv.Field (name, e))

    Can.FieldSpread exp -> do
      (ps, t, e) <- infer options env exp
      case t of
        TRecordRow _ _ ->
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
  (ps1, t1, earr) <- infer options env arr
  env1 <- applyCurrentSubst env
  (ps2, t2, eindex) <- infer options env1 index
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
        (\_ -> do
          exportNames <- namespaceExportNames env ns
          let suggestions = findSimilar (tail field) exportNames
          enhanceVarError env e area (CompilationError (UnboundVariableFromNamespace ns (tail field) suggestions) NoContext))
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
    tv              <- newTVar Star
    (_  , t1, eabs) <- infer options env abs
    env1 <- applyCurrentSubst env
    (ps2, t2, earg) <- infer options env1 rec

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
  (ps1, tcond, econd) <- infer options env cond
  env1 <- applyCurrentSubst env
  (ps2, ttruthy, etruthy) <- infer options env1 truthy
  env2 <- applyCurrentSubst env
  (ps3, tfalsy, efalsy) <- infer options env2 falsy

  tfalsy' <- applyCurrentSubst tfalsy
  ttruthy' <- applyCurrentSubst ttruthy
  -- The error anchors on the else branch; point at the then branch as a
  -- secondary location so both disagreeing branches are visible.
  let branchSecondary = Just $ SecondaryLocation
        (envCurrentPath env)
        (Can.getArea truthy)
        "the other branch of this 'if' is here"
      unifyBranches = contextualUnifyWithOriginAndSecondary (if discardError then Discard else Strict) (FromIfBranches ElseBranch) branchSecondary env falsy tfalsy' ttruthy'
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
  (ps1, tcond, econd) <- infer options env cond
  env1 <- applyCurrentSubst env
  (ps2, tbody, ebody) <- infer options env1 body

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
  (ps, t, e)             <- infer options env exp
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


-- | Phase 1 migrated: 3-tuple. Compose the pattern/scrutinee unifier, branch
-- body contribution, and result unifier in their evaluation order.
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
  (s', (ps', t'', e')) <- captureDelta (infer options envWithPatternVars exp)
  let bodySubst = s' `compose` s
  s'' <- contextualUnify' env discardError exp
    (apply bodySubst tv)
    (apply bodySubst t'')

  let subst = s'' `compose` bodySubst
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
  (ps1, t1, e1) <- infer options env exp
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

  (s, (ps, t, e)) <- captureDelta (infer options env' { envNamesInScope = envVars env } exp)
  let env'' = apply s env'

  s' <- contextualUnify' env'' discardError exp (apply s tv) t
  let s'' = s' `compose` s
      envWithVarsExcluded = setVars env'' $
        M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $
          envVars env''

      ps' = apply s'' ps
      t'  = apply s'' tv

  let patternDefaults = numericDefaultsForMatchedScrutinee e ps'
      sForGeneralization = patternDefaults `compose` s''
      envForGeneralization = apply patternDefaults envWithVarsExcluded
      psForGeneralization = apply patternDefaults ps'
      tForGeneralization = apply patternDefaults t'

  (ds, rs', sFinal) <- generalize
    isLet env area sForGeneralization envForGeneralization
    tForGeneralization psForGeneralization (apply sForGeneralization tv)

  let vs = if isLet then ftvForLetGen tForGeneralization else ftvList tForGeneralization
      fsSet = ftv (apply sFinal envForGeneralization)
      gs = filter (not . (`S.member` fsSet)) vs
      sc =
        if isLet && not (Slv.isNamedAbs e) then
          apply sFinal $ quantify [] (rs' :=> tForGeneralization)
        else
          -- TODO: consider if the apply sFinal should not happen before quantifying
          -- because right now we might miss the defaulted types in the generated
          -- scheme
          apply sFinal $ quantify gs (rs' :=> tForGeneralization)

  extSubst sFinal

  case Can.getExpName exp of
    Just n  ->
      return ((ds, rs'), extendVars env (n, sc), updateQualType e (apply sFinal $ rs' :=> tForGeneralization))

    Nothing ->
      return ((ds, rs'), env, updateQualType e (apply sFinal $ rs' :=> tForGeneralization))


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

  (s, (ps, t, e)) <- captureDelta (infer options env' { envNamesInScope = envVars env } exp)
  psFull        <- concat <$> mapM (gatherInstPreds env') ps
  let sNorm0 = s `compose` s -- resolve internal substitution chains
      inferred0 = apply sNorm0 t
      -- A closed declared result must not solve a numeric implementation
      -- variable by fiat (for example, binding Number a to Unit in `main`).
      -- Apply Madlib's numeric default before checking that boundary.  A
      -- genuinely polymorphic/qualified result remains governed by its
      -- declared variables and givens.
      signatureDefaults = numericDefaultsForClosedResult t' inferred0 (apply sNorm0 psFull)
      sNorm = signatureDefaults `compose` sNorm0
  let tInferred = apply sNorm t
      -- When annotation and implementation are functions of equal arity that
      -- differ only in their return type, report just the two return types
      -- under FromFunctionReturn instead of the whole function types. Uses
      -- the types actually reported in the thrown error (already normalized
      -- by contextualUnify), not the pre-unification annotation/inferred
      -- pair, so partially-substituted type variables don't defeat the
      -- structural comparison.
      retagReturnMismatch err = case (err, Can.getExpName exp) of
        (CompilationError (UnificationError tm) errCtx, Just name)
          | FromFunctionReturn _ <- tmOrigin tm ->
            CompilationError
              (UnificationError tm { tmOrigin = FromFunctionReturn name })
              errCtx

        (CompilationError (UnificationError tm) errCtx, Just name)
          | isFunctionType t'
          , isFunctionType tInferred
          , getParamTypes t' == getParamTypes tInferred
          , getReturnType t' /= getReturnType tInferred ->
            CompilationError
              (UnificationError tm { tmOrigin = FromFunctionReturn name })
              errCtx

        _ ->
          err
  s'' <- catchError
    (signatureUnify t' tInferred)
    (\err ->
      if discardError
        then return (gentleUnify t' tInferred)
        else addContext env canExp (limitContextArea 2 (retagReturnMismatch err)))
  let s' = s'' `compose` sNorm

  let varsForGeneralization =
        if isLet then
          M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $
            envVars env'
        else
          envVars env'
      envWithVarsExcluded = setVars env' varsForGeneralization
      qs'  = apply s' qs
      t''  = apply s' t
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' psFull)
  (ds, rs, substDefaultResolution) <- generalize False env area s' envWithVarsExcluded (apply s' t') ps' t

  let qs'' = dedupePreds qs'
      scCheck = quantify (ftvList (apply s' t'))
        (qs' :=> apply substDefaultResolution (apply s' t'))
  sigCheckResult <- if sc /= scCheck then
    -- The inferred scheme differs from the declared scheme. Unify a fresh
    -- instance of each and inspect how the annotation's variables were bound:
    -- only an injective renaming means the annotation matches the
    -- implementation's generality. Anything else means it is too general.
    catchError (do
      (_ :=> t1) <- instantiate sc
      (_ :=> t2) <- instantiate scCheck
      -- Re-run the same boundary check on fresh instances.  Ordinary
      -- unification rejects a lawful implementation that accepts a wider
      -- record input than its advertised closed record contract.
      sCheck <- signatureUnify t1 t2
      return (isInjectiveRenaming t1 sCheck)
    ) (const $ return False)
  else
    return True

  if not sigCheckResult then
    throwError $ CompilationError (SignatureTooGeneral sc scCheck) (Context (envCurrentPath env') area)
  else if not (null rs) then
    throwError $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) area)
  else do
    -- Keep the implementation's fully-zonked type in solved metadata.  The
    -- public environment still receives the user-declared scheme below.
    let e' = updateQualType e
          (apply substDefaultResolution ds :=> apply substDefaultResolution t'')
        env'' = case Can.getExpName exp of
          Just n  ->
            -- A successful annotation check publishes the declared scheme,
            -- not a subtly narrowed scheme reconstructed from the body.
            extendVars env' (n, sc)
          Nothing -> env'

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
  (s, _, env', e') <-
    withScopedSubst $
      upgradeContext env (Can.getArea e) $ case e of
        Can.Canonical _ Can.TypedExp{} -> do
          (ps, env'', e') <- inferExplicitlyTyped options False env e
          s <- getSubst
          return (s, ps, env'', e')

        _ -> do
          ((_, placeholderPreds), env'', e') <- inferImplicitlyTyped options False env e
          s <- getSubst
          return (s, placeholderPreds, env'', e')

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
