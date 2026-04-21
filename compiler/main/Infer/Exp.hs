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


mutationInterface :: String
mutationInterface = "__MUTATION__"

mutationPred :: Pred
mutationPred = IsIn mutationInterface [] Nothing

makeMutationPred :: Type -> Area -> Pred
makeMutationPred t area = IsIn mutationInterface [t] (Just area)


infer :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer discardError options env lexp = do
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
    Can.TemplateString _      -> inferTemplateString discardError options env lexp

    Can.Var            _      -> inferVar discardError options env lexp
    Can.Abs _ _               -> inferAbs discardError options env lexp
    Can.App{}                 -> inferApp discardError options env lexp
    Can.Assignment _ _        -> inferAssignment discardError options env lexp
    Can.Mutate _ _            -> inferMutate discardError options env lexp
    Can.Do _                  -> inferDo discardError options env lexp
    Can.Where      _ _        -> inferWhere discardError options env lexp
    Can.Record _              -> inferRecord discardError options env lexp
    Can.JsxRecord _           -> inferJsxRecord discardError options env lexp
    Can.Access   _ _          -> inferAccess discardError options env lexp
    Can.ArrayAccess   _ _     -> inferArrayAccess discardError options env lexp
    Can.TypedExp{}            -> inferTypedExp discardError options env lexp
    Can.ListConstructor  _    -> inferListConstructor discardError options env lexp
    Can.TupleConstructor _    -> inferTupleConstructor discardError options env lexp
    Can.Export           _    -> inferExport discardError options env lexp
    Can.NameExport       _    -> inferNameExport env lexp
    Can.If{}                  -> inferIf discardError options env lexp
    Can.While{}               -> inferWhile discardError options env lexp
    Can.Extern{}              -> inferExtern env lexp
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

inferVar :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar _ _ env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s = Forall [Star, Star] $ [] :=> (TRecord (M.fromList [(name, TGen 0)]) (Just $ TGen 1) mempty `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (M.empty, ps, t, Slv.Typed (ps :=> t) area $ Slv.Var n False)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    (ps :=> t) <- instantiate sc

    let ps' = dedupePreds ps
    let e = Slv.Typed (ps' :=> t) area $ Slv.Var n (isConstructor env n)
    let ps'' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps'

    return (M.empty, ps'', t, e)

enhanceVarError :: Env -> Can.Exp -> Area -> CompilationError -> Infer Scheme
enhanceVarError env _ area (CompilationError e _) =
  throwError $ CompilationError e (Context (envCurrentPath env) area)


-- INFER NAME EXPORT

inferNameExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNameExport env exp@(Can.Canonical area (Can.NameExport name)) = do
  sc         <- catchError (lookupVar env name) (enhanceVarError env exp area)
  (ps :=> t) <- instantiate sc

  let e = Slv.Typed (ps :=> t) area $ Slv.NameExport name

  return (M.empty, ps, t, e)



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


inferAbs :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAbs discardError options env l@(Can.Canonical _ (Can.Abs p@(Can.Canonical area param) body)) = do
  tv             <- newTVar Star
  env'           <- extendAbsEnv env tv p
  (s, ps, t, es) <- inferBody discardError options env' { envInBody = True } body
  (s', es')      <- postProcessBody discardError options env' s (tv `fn` t) es

  let t'        = apply s' (tv `fn` t)
      paramType = apply s' tv
      ps'       = normalizePreds s' ps

  return (s', ps', t', applyAbsSolve l (Slv.Typed (ps' :=> paramType) area param) es' (ps' :=> t'))


inferBody :: Bool -> Options -> Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody discardError options env [e] = do
  (s, ps, t, e') <- infer discardError options env e
  return (s, ps, t, [e'])

inferBody discardError options env (e : es) = do
  (s, (returnPreds, _), env', e') <- inferImplicitlyTyped discardError options True env e
  (sb, ps', tb, eb) <- inferBody discardError options (apply s env') es
  let finalS = sb `compose` s
  return (finalS, dedupePreds (apply finalS $ returnPreds ++ ps'), tb, e' : eb)


-- | Second pass over a body's expressions, run after 'inferBody' has produced
-- a substitution for the whole body.
--
-- Each expression was typed against the substitution known *at its point in
-- the body*, so its stored qual-type still refers to type variables that
-- later expressions constrained. This pass:
--
--   1. Applies the accumulated substitution to every expression's qual-type
--      (via 'updateExpTypes'), so the solved tree reflects what the whole
--      body learned.
--   2. Checks each expression's residual predicates. Anything still
--      unresolvable is an ambiguity local to this body — we try defaulting
--      (which extends the substitution) and, if a predicate is still
--      unreducible, raise 'AmbiguousType' pointing at the *specific*
--      expression's area. Without this per-expression check, the error
--      would bubble up to the enclosing abstraction and lose its precise
--      source location.
--
-- The enclosing 'split' / 'generalize' at the let-binding level still runs
-- afterwards. This pass only handles ambiguities that are scoped strictly
-- within the body.
--
-- Internally the substitution is threaded through the 'Infer' monad's state
-- (Typing-Haskell-in-Haskell style). The body substitution @s@ comes in from
-- the caller; any further defaulting is composed into it and the fully
-- accumulated substitution is returned.
postProcessBody
  :: Bool -> Options -> Env
  -> Substitution         -- ^ substitution from the enclosing 'inferBody'
  -> Type                 -- ^ the expected body type (used for ftv fixing)
  -> [Slv.Exp]
  -> Infer (Substitution, [Slv.Exp])
postProcessBody discardError options env s expType es = do
  -- Install the body substitution in state so the residual-preds machinery
  -- can thread defaulting through it implicitly instead of passing accSubst
  -- around explicitly.
  withSubstRestore $ do
    putSubst s

    esRev <- foldM
      (\resultsRev expr@(Slv.Typed (ps :=> t) area _) -> do
        ps'          <- applyCurrentSubst ps
        resolvedEnv  <- applyCurrentSubst env
        resolvedExpT <- applyCurrentSubst expType
        resolvedT    <- applyCurrentSubst t

        let fs = S.toList $
              ftv resolvedEnv
              `S.union` ftv resolvedExpT
              `S.union` ftvForLetGenSet resolvedT

        ps'' <- resolveResidualPreds discardError env area fs ps'
        e'   <- updateExpTypesWithCurrent options env False
                  (updateQualType expr (ps'' :=> t))

        return (e' : resultsRev)
      )
      []
      es

    finalS <- getSubst
    return (finalS, reverse esRev)


-- | Resolve an expression's residual predicates. Entailable predicates are
-- dropped. Ambiguous predicates trigger a second defaulting pass; anything
-- still unsolvable produces an 'AmbiguousType' at @area@.
resolveResidualPreds
  :: Bool              -- ^ discard errors
  -> Env
  -> Area              -- ^ area of the expression (for error reporting)
  -> [TVar]            -- ^ fixed variables (env + expected type)
  -> [Pred]            -- ^ predicates already reflecting the current substitution
  -> Infer [Pred]
resolveResidualPreds discardError env area fs ps = do
  prep <- CM.forM ps $ \p -> (p,) <$> entail env [] p
  let (solved, unsolved) = partition snd prep
      unsolvedPs         = map fst unsolved
      solvedPs           = map fst solved

  if null unsolvedPs || null (ambiguities fs unsolvedPs) then
    return ps
  else do
    (sDef, unsolvedAfter1)  <- tryDefaults env unsolvedPs
    extendSubst sDef
    unsolvedApplied         <- applyCurrentSubst unsolvedAfter1
    (sDef', unsolvedAfter2) <- tryDefaults env unsolvedApplied
    extendSubst sDef'

    when (not (null unsolvedAfter2) && not discardError) $
      reportUnsolvedBodyPreds env area fs unsolvedAfter2

    return (unsolvedAfter2 ++ solvedPs)


reportUnsolvedBodyPreds :: Env -> Area -> [TVar] -> [Pred] -> Infer ()
reportUnsolvedBodyPreds env area fs unsolved = do
  unsolvedResolved <- applyCurrentSubst unsolved
  CM.forM_ unsolved $ \p -> do
    p' <- applyCurrentSubst p
    catchError (byInst env p' >> return ()) $ \case
      CompilationError FatalError NoContext ->
        when (not (null (ambiguities fs unsolvedResolved))) $
          let tv = case p' of
                IsIn _ (TVar v : _) _ -> v
                _                     -> TV (-1) Star
          in  throwError $ CompilationError
                (AmbiguousType (tv, unsolvedResolved))
                (Context (envCurrentPath env) area)
      err -> throwError err


-- | 'updateExpTypes' using the current substitution from state.
updateExpTypesWithCurrent :: Options -> Env -> Bool -> Slv.Exp -> Infer Slv.Exp
updateExpTypesWithCurrent options env push e = do
  s <- getSubst
  updateExpTypes options env push s e


-- INFER APP

inferApp :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp discardError options env (Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea argContent) final)) = do
  withFreshSubst $ do
    tv <- newTVar Star
    (s1, ps1, t1, eabs) <- infer discardError options env abs
    extendSubst s1

    envApplied <- applyCurrentSubst env
    (s2, ps2, t2, earg) <- infer discardError options envApplied arg
    extendSubst s2

    let expForContext = arg  -- always point at the argument (the "wrong" value)

    -- Enrich the origin with function context (expected type + full signature).
    funcType <- applyCurrentSubst t1
    let baseOrigin = getAppOrigin abs
        origin = case baseOrigin of
          FromFunctionArgument fn idx _ ->
            let params = getParamTypes funcType
                expectedParam = case params of { (p:_) -> Just p; _ -> Nothing }
                ctx = FunctionContext
                  { fcExpectedType  = maybe funcType id expectedParam
                  , fcFullSignature = funcType
                  , fcTotalParams   = length params
                  }
            in  FromFunctionArgument fn idx (Just ctx)
          other -> other
        secondaryLoc = case baseOrigin of
          FromFunctionArgument fn _ _ ->
            Just $ SecondaryLocation (envCurrentPath env) absArea
              ("'" <> fn <> "' is applied here")
          _ -> Nothing

    t2Applied <- applyCurrentSubst t2
    s3 <- contextualUnifyWithOriginAndSecondary
            (if discardError then Discard else Strict) origin secondaryLoc env
            expForContext funcType (t2Applied `fn` tv)
    extendSubst s3

    t <- applyCurrentSubst tv
    s <- getSubst

    -- For JSX records: fill missing Maybe-typed fields with Nothing
    earg' <- case argContent of
      Can.JsxRecord jsxFields -> do
        let explicitNames = S.fromList [ n | Can.Canonical _ (Can.Field (n, _)) <- jsxFields ]
        resolvedArgType <- applyCurrentSubst t2
        case resolvedArgType of
          TRecord allFields _ _ -> do
            let missingFields = M.filterWithKey (\k _ -> k `S.notMember` explicitNames) allFields
                allMaybe = all isMaybeType (M.elems missingFields)
            if M.null missingFields || not allMaybe then
              return earg
            else do
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

    let appPreds = normalizePreds s (ps1 ++ ps2)
        eabs'    = updateQualType eabs (apply s $ ps1 :=> t1)
        solved = Slv.Typed (apply s appPreds :=> apply s t) area $
                   Slv.App eabs' (updateQualType earg' $ apply s appPreds :=> apply s t2) final
    return (s, appPreds, t, solved)



-- INFER TEMPLATE STRINGS

inferTemplateString :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTemplateString discardError options env (Can.Canonical area (Can.TemplateString exps)) = do
  -- Thread the substitution implicitly through state so that bindings learned
  -- while inferring one element are visible to the next (e.g. field accesses
  -- on a shared parameter accumulate fields in its record type).
  withFreshSubst $ do
    inferred <- forM exps $ \e -> do
      envApplied <- applyCurrentSubst env
      (s, ps, t, e') <- infer discardError options envApplied e
      extendSubst s
      return (ps, t, e')

    let elemPS    = (\(ps, _, _) -> ps) <$> inferred
        elemTypes = (\(_, t, _) -> t) <$> inferred
        elemExps  = (\(_, _, es) -> es) <$> inferred

    forM_ (zip exps elemTypes) $ \(exp, t) -> do
      tApplied <- applyCurrentSubst t
      s <- contextualUnify' env discardError exp tApplied tStr
      extendSubst s

    fullSubst <- getSubst

    let qs = uncurry (:=>) <$> zip elemPS elemTypes
        updatedExp = Slv.Typed
          ([] :=> tStr)
          area
          (Slv.TemplateString ((\(t, e) -> updateQualType e (apply fullSubst t)) <$> zip qs elemExps))

    return (fullSubst, concat elemPS, tStr, updatedExp)



-- INFER ASSIGNMENT

inferAssignment :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAssignment discardError options env e@(Can.Canonical area (Can.Assignment name exp)) = do
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

  withFreshSubst $ do
    (s1, ps1, t1, e1) <- infer discardError options env' exp
    extendSubst s1

    t1Applied <- applyCurrentSubst t1
    -- We can skip this error as we mainly need the substitution. It would
    -- fail in inferExplicitlyTyped anyways.
    s2 <- catchError (contextualUnify Strict env' e currentType t1Applied)
                     (const $ return M.empty)
    extendSubst s2

    s <- getSubst
    let t2 = apply s t1

    mutationPs <-
      if M.member name (envNamesInScope env) && envInBody env && not discardError then do
        pushError $ CompilationError BadMutation (Context (envCurrentPath env) area)
        return []
      else
        return []

    let preds = normalizePreds s (currentPreds ++ ps1 ++ mutationPs)
    return (s, preds, apply s t2, applyAssignmentSolve e name e1 (apply s $ preds :=> t2))



-- INFER MUTATE

inferMutate :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferMutate discardError options env e@(Can.Canonical area (Can.Mutate lhs exp)) = do
  withFreshSubst $ do
    (s1, ps1, t1, e1) <- infer discardError options env lhs
    extendSubst s1

    envApplied <- applyCurrentSubst env
    (s2, ps2, t2, e2) <- infer discardError options envApplied exp
    extendSubst s2

    t1' <- applyCurrentSubst t1
    t2' <- applyCurrentSubst t2
    s3  <- catchError (contextualUnify Strict env e t1' t2')
             (\err -> if discardError then return mempty else throwError err)
    extendSubst s3

    s  <- getSubst
    let t3 = apply s t2

    case lhs of
      Can.Canonical _ (Can.Var name) | not discardError && name `Set.member` envPatternBoundNames env ->
        throwError $ CompilationError (MutatingPatternBoundVariable name) (Context (envCurrentPath env) area)
      _ ->
        return ()

    mutationPs <-
      case Can.getExpName lhs of
        Just name | not discardError ->
          if M.member name (envNamesInScope env) && envInBody env then
            return [makeMutationPred (apply s t3) area]
          else
            throwError $ CompilationError (MutatingNotInScope name) (Context (envCurrentPath env) area)

        _ ->
          return []

    let preds = normalizePreds s (ps1 ++ ps2 ++ mutationPs)
    return
      ( s
      , preds
      , apply s t3
      , Slv.Typed (preds :=> apply s t3) area (Slv.Mutate e1 e2)
      )



-- INFER EXPORT

inferExport :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport discardError options env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer discardError options env exp
  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor discardError options env listExp@(Can.Canonical area (Can.ListConstructor elems)) = case elems of
  [] -> do
    tv <- newTVar Star
    let t = tListOf tv
    return (M.empty, [], t, Slv.Typed ([] :=> t) area (Slv.ListConstructor []))

  elems -> withFreshSubst $ do
    tv <- newTVar Star

    -- Use state for cross-element substitution threading (THIH-style).
    (psChunksRev, tLastM, esRev, _) <- foldlM
      (\(pssRev, tLast, lis, idx) elem -> do
        envApplied <- applyCurrentSubst env
        (s1, ps', tE, li) <- inferListItem discardError options envApplied (fromMaybe tv tLast) elem
        extendSubst s1

        tr <- case tLast of
          Nothing -> return tE
          Just tPrev -> do
            tPrevApplied <- applyCurrentSubst tPrev
            s2 <- contextualUnifyWithOrigin
                    (if discardError then Discard else Strict)
                    (FromListElement idx) env elem tPrevApplied tE
            extendSubst s2
            return (pickJSXChild tPrev tE)

        trApplied <- applyCurrentSubst tr
        return (ps' : pssRev, Just trApplied, li : lis, idx + 1)
      )
      ([], Nothing, [], 1)
      elems

    let ps  = concat (reverse psChunksRev)
        (Just tElem) = tLastM
        es  = reverse esRev

    contextualUnifyS
      (if discardError then Discard else Strict) env listExp tv tElem
    resolvedTv <- applyCurrentSubst tv

    localSubst <- getSubst
    let t = tListOf resolvedTv
    return (localSubst, ps, t, Slv.Typed (ps :=> t) area (Slv.ListConstructor es))


inferListItem :: Bool -> Options -> Env -> Type -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem discardError options env _ (Can.Canonical area li) = case li of
  Can.ListItem exp -> do
    (s1, ps, t, e) <- infer discardError options env exp
    return (s1, ps, t, Slv.Typed (ps :=> t) area $ Slv.ListItem e)

  Can.ListSpread exp -> do
    withFreshSubst $ do
      (s1, ps, t, e) <- infer discardError options env exp
      extendSubst s1

      tv <- newTVar Star
      tApplied <- applyCurrentSubst t
      s2 <- contextualUnify' env discardError exp (tListOf tv) tApplied
      extendSubst s2

      s <- getSubst
      return (s, ps, apply s tv, Slv.Typed (apply s ps :=> apply s t) area $ Slv.ListSpread e)


pickJSXChild :: Type -> Type -> Type
pickJSXChild t1 t2 = case (t1, t2) of
  (TApp (TCon (TC "Element" _) _ _) _, TCon (TC "String" _) _ _) ->
    t2

  _ ->
    t2



-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor discardError options env (Can.Canonical area (Can.TupleConstructor elems)) = do
  withFreshSubst $ do
    (psChunksRev, tsRev, esRev) <- foldM
      (\(psRev, ts, es) e -> do
        envApplied <- applyCurrentSubst env
        (s', ps', t', e') <- infer discardError options envApplied e
        extendSubst s'
        return (ps' : psRev, t' : ts, e' : es)
      )
      ([], [], [])
      elems

    s <- getSubst

    let ps        = concat (reverse psChunksRev)
        elemTypes = reverse tsRev
        elemEXPS  = reverse esRev
        tupleT    = getTupleCtor (length elems)
        t         = foldl' TApp tupleT elemTypes
    return (s, ps, apply s t, Slv.Typed (ps :=> apply s t) area (Slv.TupleConstructor elemEXPS))



-- INFER RECORD

inferRecord :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord discardError options env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  -- Infer each field with a fresh local substitution in state, so we can
  -- thread it implicitly across field inferences (THIH-style) rather than
  -- carrying an explicit accumulator through foldM.
  withFreshSubst $ do
    inferredFieldsRev <- foldM
      (\result field -> do
        envApplied <- applyCurrentSubst env
        (s, ps, ts, e) <- inferRecordField discardError options envApplied field
        extendSubst s
        tsApplied <- CM.forM ts (\(n, t) -> (n,) <$> applyCurrentSubst t)
        return ((ps, tsApplied, e) : result)
      )
      []
      fields

    let inferredFields = reverse inferredFieldsRev
        fieldPS   = (\(ps, _, _) -> ps) <$> inferredFields
        fieldTypes = (\(_, t, _) -> t) <$> inferredFields
        fieldEXPS = (\(_, _, es) -> es) <$> inferredFields

        allFieldTypes = concat fieldTypes
        fieldTypes' = filter (\(k, _) -> k /= "...") allFieldTypes
        spreads     = snd <$> filter (\(k, _) -> k == "...") allFieldTypes
        base = case spreads of
          (x : _) -> Just x
          _       -> Nothing

    resolvedBase <- case base of
      Just b  -> Just <$> applyCurrentSubst b
      Nothing -> return Nothing

    recordType <- case resolvedBase of
      Just (TRecord spreadFields baseBase optionalFields) -> do
        -- Merge the spread record's fields with our explicit fields.
        -- Spread fields take precedence if there are conflicts.
        resolvedBaseBase <- case baseBase of
          Just b  -> Just <$> applyCurrentSubst b
          Nothing -> return Nothing
        let mergedFields = M.fromList fieldTypes' `M.union` spreadFields
        return (TRecord mergedFields resolvedBaseBase optionalFields)

      Just tBase -> do
        -- Spread is a type variable: unify it with a record-with-row-var of our fields.
        baseVar <- newTVar Star
        let recordWithBase = TRecord (M.fromList fieldTypes') (Just baseVar) mempty
        contextualUnifyS (if discardError then Discard else Strict) env exp tBase recordWithBase
        unifiedBase <- applyCurrentSubst tBase
        case unifiedBase of
          TRecord unifiedFields unifiedBase' unifiedOptionalFields ->
            return (TRecord unifiedFields unifiedBase' unifiedOptionalFields)
          _ ->
            return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty)

      Nothing ->
        -- No spread: closed record.
        return (TRecord (M.fromList fieldTypes') Nothing mempty)

    localSubst <- getSubst
    let allPS       = concat fieldPS
        resolvedRec = apply localSubst recordType
    return ( localSubst
           , allPS
           , resolvedRec
           , Slv.Typed (allPS :=> recordType) area (Slv.Record fieldEXPS)
           )


-- | Like inferRecord but creates an extensible record (with a base type variable)
-- so that missing Maybe-typed fields can be filled with Nothing after unification.
inferJsxRecord :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferJsxRecord discardError options env exp = do
  let Can.Canonical area (Can.JsxRecord fields) = exp
  withFreshSubst $ do
    inferredFieldsRev <- foldM
      (\result field -> do
        envApplied <- applyCurrentSubst env
        (s, ps, ts, e) <- inferRecordField discardError options envApplied field
        extendSubst s
        tsApplied <- CM.forM ts (\(n, t) -> (n,) <$> applyCurrentSubst t)
        return ((ps, tsApplied, e) : result)
      )
      []
      fields

    let inferredFields = reverse inferredFieldsRev
        fieldPS    = (\(ps, _, _) -> ps) <$> inferredFields
        fieldTypes = (\(_, t, _) -> t) <$> inferredFields
        fieldEXPS  = (\(_, _, es) -> es) <$> inferredFields

        allFieldTypes = concat fieldTypes
        fieldTypes'   = filter (\(k, _) -> k /= "...") allFieldTypes
        spreads       = snd <$> filter (\(k, _) -> k == "...") allFieldTypes
        base = case spreads of
          (x : _) -> Just x
          _       -> Nothing

    resolvedBase <- case base of
      Just b  -> Just <$> applyCurrentSubst b
      Nothing -> return Nothing

    recordType <- case resolvedBase of
      Just (TRecord spreadFields baseBase optionalFields) -> do
        resolvedBaseBase <- case baseBase of
          Just b  -> Just <$> applyCurrentSubst b
          Nothing -> return Nothing
        let mergedFields = M.fromList fieldTypes' `M.union` spreadFields
        return (TRecord mergedFields resolvedBaseBase optionalFields)

      Just tBase -> do
        baseVar <- newTVar Star
        let recordWithBase = TRecord (M.fromList fieldTypes') (Just baseVar) mempty
        contextualUnifyS (if discardError then Discard else Strict) env exp tBase recordWithBase
        unifiedBase <- applyCurrentSubst tBase
        case unifiedBase of
          TRecord unifiedFields unifiedBase' unifiedOptionalFields ->
            return (TRecord unifiedFields unifiedBase' unifiedOptionalFields)
          _ ->
            return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty)

      Nothing -> do
        -- JSX record without spread: create an EXTENSIBLE record so unification
        -- can absorb missing Maybe-typed fields (filled with Nothing later).
        baseVar <- newTVar Star
        return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty)

    localSubst <- getSubst
    let allPS       = concat fieldPS
        resolvedRec = apply localSubst recordType
    return ( localSubst
           , allPS
           , resolvedRec
           , Slv.Typed (allPS :=> recordType) area (Slv.Record fieldEXPS)
           )


inferRecordField :: Bool -> Options -> Env -> Can.Field -> Infer (Substitution, [Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField discardError options env (Can.Canonical area field) = case field of
  Can.Field (name, exp) -> do
    (s, ps, t, e) <- infer discardError options env exp
    return (s, ps, [(name, t)], Slv.Typed (ps :=> t) area $ Slv.Field (name, e))

  Can.FieldSpread exp -> do
    (s, ps, t, e) <- infer discardError options env exp
    case t of
      TRecord{} ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      TVar _ ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      _ | discardError ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      _ ->
        throwError $ CompilationError
          (WrongSpreadType $ show t)
          (Context (envCurrentPath env) (Can.getArea exp))



-- INFER ACCESS

inferAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAccess discardError options env e@(Can.Canonical _ (Can.Access ns _)) =
  case ns of
    Can.Canonical _ (Can.Var ns') ->
      if ns' `Set.member` envNamespacesInScope env then
        inferNamespaceAccess discardError options env e
      else
        inferFieldAccess discardError options env e

    _ ->
      inferFieldAccess discardError options env e



-- INFER ACCESS

inferArrayAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferArrayAccess discardError options env (Can.Canonical area (Can.ArrayAccess arr index)) = do
  withFreshSubst $ do
    tv <- newTVar Star
    (s1, ps1, t1, earr) <- infer discardError options env arr
    extendSubst s1

    envApplied <- applyCurrentSubst env
    (s2, ps2, t2, eindex) <- infer discardError options envApplied index
    extendSubst s2

    t1Applied <- applyCurrentSubst t1
    s3 <- contextualUnify' env discardError arr t1Applied (tArrayOf tv)
    extendSubst s3

    t2Applied <- applyCurrentSubst t2
    s4 <- contextualUnify' env discardError index t2Applied tInteger
    extendSubst s4

    s <- getSubst
    let t  = apply s tv
        ps = ps1 ++ ps2
    return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.ArrayAccess earr eindex))



-- INFER NAMESPACE ACCESS

inferNamespaceAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNamespaceAccess _ _ env e@(Can.Canonical area (Can.Access (Can.Canonical _ (Can.Var ns)) (Can.Canonical _ (Can.Var field))))
  = do
    sc <-
      catchError
        (lookupVar env (ns <> field))
        (\_ -> enhanceVarError env e area (CompilationError (UnboundVariableFromNamespace ns (tail field)) NoContext))
    (ps :=> t) <- instantiate sc
    let ps' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps

    let e = Slv.Typed (ps :=> t) area $ Slv.Var (ns <> field) (isConstructor env (ns <> field))

    return (M.empty, ps', t, e)
inferNamespaceAccess _ _ _ _ = throwError $ CompilationError FatalError NoContext



-- INFER FIELD ACCESS

inferFieldAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess discardError options env fa@(Can.Canonical area (Can.Access rec@(Can.Canonical _ _) abs))
  = do
    withFreshSubst $ do
      tv <- newTVar Star
      (s1, _  , t1, eabs) <- infer discardError options env abs
      extendSubst s1

      (s2, ps2, t2, earg) <- infer discardError options env rec
      extendSubst s2

      t1A <- applyCurrentSubst t1
      t2A <- applyCurrentSubst t2
      s3 <- catchError
              (contextualUnifyAccess env fa t1A (t2A `fn` tv))
              (\err -> if discardError
                       then return $ gentleUnify t1A (t2A `fn` tv)
                       else throwError err)
      extendSubst s3

      s <- getSubst
      let t      = apply s tv
          solved = Slv.Typed (ps2 :=> t) area (Slv.Access earg eabs)
      return (s, ps2, t, solved)



-- INFER IF

inferIf :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf discardError options env (Can.Canonical area (Can.If cond truthy falsy)) = do
  withFreshSubst $ do
    (s1, ps1, tcond, econd) <- infer discardError options env cond
    extendSubst s1

    envApplied1 <- applyCurrentSubst env
    (s2, ps2, ttruthy, etruthy) <- infer discardError options envApplied1 truthy
    extendSubst s2

    envApplied2 <- applyCurrentSubst env
    (s3, ps3, tfalsy, efalsy) <- infer discardError options envApplied2 falsy
    extendSubst s3

    tfalsy'  <- applyCurrentSubst tfalsy
    ttruthy' <- applyCurrentSubst ttruthy
    let unifyBranches = contextualUnifyWithOrigin
          (if discardError then Discard else Strict)
          (FromIfBranches ElseBranch) env falsy tfalsy' ttruthy'
    s4 <- catchError unifyBranches (flipUnificationErrorWithBranch ThenBranch)
    extendSubst s4

    tcond' <- applyCurrentSubst tcond
    s5 <- contextualUnifyWithOrigin
            (if discardError then Discard else Strict)
            FromIfCondition env cond tBool tcond'
    extendSubst s5

    s <- getSubst
    let t = apply s ttruthy
        preds = normalizePreds s (ps1 ++ ps2 ++ ps3)
    return (s, preds, t, Slv.Typed (preds :=> t) area (Slv.If econd etruthy efalsy))



-- INFER While

inferWhile :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhile discardError options env (Can.Canonical area (Can.While cond body)) = do
  withFreshSubst $ do
    (s1, ps1, tcond, econd) <- infer discardError options env cond
    extendSubst s1

    envApplied <- applyCurrentSubst env
    (s2, ps2, tbody, ebody) <- infer discardError options envApplied body
    extendSubst s2

    tcond' <- applyCurrentSubst tcond
    s4 <- contextualUnifyWithOrigin
            (if discardError then Discard else Strict)
            FromWhileCondition env cond tBool tcond'
    extendSubst s4

    tbody' <- applyCurrentSubst tbody
    s5 <- contextualUnify' env discardError body tUnit tbody'
    extendSubst s5

    s <- getSubst
    let t = apply s tbody
        preds = normalizePreds s (ps1 ++ ps2)
    return (s, preds, t, Slv.Typed (preds :=> t) area (Slv.While econd ebody))



-- INFER DO

inferDo :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferDo discardError options env (Can.Canonical area (Can.Do exps)) = do
  withFreshSubst $ do
    (s, ps, t, exps') <- inferBody discardError options env exps
    extendSubst s
    (s', exps'')      <- postProcessBody discardError options env s t exps'
    extendSubst s'

    sFinal <- getSubst
    let preds = normalizePreds sFinal ps
        t' = apply sFinal t
    return (sFinal, preds, t', Slv.Typed (preds :=> t') area (Slv.Do exps''))



-- INFER WHERE

inferWhere :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere discardError options env (Can.Canonical area (Can.Where exp iss)) = do
  withFreshSubst $ do
    (s, ps, t, e) <- infer discardError options env exp
    extendSubst s

    tv <- newTVar Star
    pssRev <- foldM
      (\res (idx, is) -> do
        envNow <- applyCurrentSubst env
        tvNow  <- applyCurrentSubst tv
        tNow   <- applyCurrentSubst t
        r@(subst, _, _) <- inferBranch discardError options envNow tvNow tNow idx is
        extendSubst subst
        return (r : res)
      )
      []
      (zip [1..] iss)
    let pss = reverse pssRev
        ps' = concat $ T.mid <$> pss

    issSubstitution <- getSubst
    s' <- contextualUnifyElems env $
            zip iss (apply issSubstitution . Slv.getType . T.lst <$> pss)
    extendSubst s'

    s'' <- getSubst

    let isResolved = (\(Slv.Typed t' a (Slv.Is pat expB)) ->
                        Slv.Typed (apply s'' t') a (Slv.Is (updatePatternTypes s'' mempty pat) expB))
                     . T.lst <$> pss
        preds = normalizePreds s'' (ps ++ ps')
        wher = Slv.Typed (preds :=> apply s'' tv) area $
                 Slv.Where (updateQualType e (normalizePreds s'' ps :=> apply s'' t)) isResolved
    return (s'', preds, apply s'' tv, wher)


inferBranch :: Bool -> Options -> Env -> Type -> Type -> Int -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch discardError options env tv t branchIdx (Can.Canonical area (Can.Is pat exp)) = do
  withFreshSubst $ do
    (pat', ps, vars, t') <- inferPattern env pat
    s <- contextualUnifyWithOrigin (if discardError then Discard else Strict) (FromPatternMatch branchIdx) env exp t t'
    extendSubst s

    -- Fix rest variable types: after unification, row variables get substituted with
    -- records that include ALL fields (because optional fields merge into main fields
    -- during compose). For rest pattern variables like `...g`, we subtract the explicitly
    -- matched fields to get only the "remaining" fields.
    let vars' = fixRestVarTypes s pat vars
        patternBoundNames = M.keysSet vars'

    envWithPatternVars <- applyCurrentSubst (mergeVars env vars')
    let envWithPatternVars' =
          envWithPatternVars
            { envPatternBoundNames = envPatternBoundNames env <> patternBoundNames }

    (s', ps', t'', e') <- infer discardError options envWithPatternVars' exp
    extendSubst s'

    envResolved <- applyCurrentSubst envWithPatternVars'
    tResolved   <- applyCurrentSubst t''
    psResolved  <- applyCurrentSubst ps'
    let fixedVars = ftv envResolved `S.union` ftv tResolved
    (_branchDs, _branchRs, sBranch) <- split True envResolved (S.toList fixedVars) (ftvList tResolved) psResolved
    extendSubst sBranch

    tvNow  <- applyCurrentSubst tv
    tNow   <- applyCurrentSubst tResolved
    s'' <- contextualUnify' env discardError exp tvNow tNow
    extendSubst s''

    subst <- getSubst
    let isSpuriousBranchNumericPred (IsIn cls ts Nothing) =
          cls `elem` ["Number", "Bits"] && all isConcrete ts
        isSpuriousBranchNumericPred _ = False

        allPreds = normalizePreds subst (ps ++ filter (not . isSpuriousBranchNumericPred) ps')
        varsApplied = M.map (apply s) vars'
    return
      ( subst
      , allPreds
      , Slv.Typed (allPreds :=> apply subst (t' `fn` tv)) area
        $ Slv.Is (updatePatternTypes subst varsApplied pat') (updateQualType e' (ps' :=> apply subst t''))
      )



-- INFER TYPEDEXP

inferTypedExp :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp discardError options env e@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  withFreshSubst $ do
    (_ :=> t) <- instantiate sc
    (s1, ps1, t1, e1) <- infer discardError options env exp
    extendSubst s1

    t1Applied <- applyCurrentSubst t1
    s2 <- contextualUnify' env discardError e t t1Applied
    extendSubst s2

    s <- getSubst
    return
      ( s
      , apply s ps1
      , apply s t1
      , Slv.Typed (apply s $ ps1 :=> t1) area (Slv.TypedExp (updateQualType e1 (ps1 :=> t1)) (updateTyping typing) sc)
      )


inferExtern :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExtern _ (Can.Canonical area (Can.Extern scheme name originalName)) = do
  qt@(ps :=> t) <- instantiate scheme
  return (mempty, ps, t, Slv.Typed qt area (Slv.Extern qt name originalName))


type Ambiguity = (TVar, [Pred])

ambiguities :: [TVar] -> [Pred] -> [Ambiguity]
ambiguities vs ps =
  let vsSet = S.fromList vs
      ambigVars = ftv ps `S.difference` vsSet
  in  [ (v, filter (S.member v . ftv) ps) | v <- S.toList ambigVars ]



hasPredForType :: String -> Type -> [Pred] -> Bool
hasPredForType cls t ps =
  any (\(IsIn cls' ts _) -> t `elem` ts && cls == cls') ps

updateRecordUpdatePreds :: [Pred] -> [Pred]
updateRecordUpdatePreds ps = updateRecordUpdatePreds' ps ps

-- Preds for record with a base should be resolved by the base directly
-- We also emit a closed record pred (without base) so that explicit fields
-- are checked against user-defined type class instances.
-- Empty records (no fields, no base) are filtered out as they don't need instances.
updateRecordUpdatePreds' :: [Pred] -> [Pred] -> [Pred]
updateRecordUpdatePreds' allPreds ps = case ps of
  IsIn _ [TRecord fields Nothing optionalFields] _ : next
    | M.null fields && M.null optionalFields ->
      -- Empty record - filter out the predicate as it doesn't need instances
      updateRecordUpdatePreds next

  IsIn cls [tRec@(TRecord fields (Just base@(TVar _)) optionalFields)] maybeArea : next
    | not (hasPredForType "Number" tRec allPreds)
    && not (hasPredForType "Bits" tRec allPreds)
    && not (hasPredForType "Number" base allPreds)
    && not (hasPredForType "Bits" base allPreds) ->
      if M.null fields && M.null optionalFields then
        -- No explicit fields - just emit the base pred
        IsIn cls [base] maybeArea : updateRecordUpdatePreds next
      else
        -- Emit both: base pred (for base fields) and closed record pred (for explicit fields)
        IsIn cls [base] maybeArea
          : IsIn cls [TRecord fields Nothing optionalFields] maybeArea
          : updateRecordUpdatePreds next

  or : next ->
    or : updateRecordUpdatePreds next

  _ ->
    []

split :: Bool -> Env -> [TVar] -> [TVar] -> [Pred] -> Infer ([Pred], [Pred], Substitution)
split mustCheck env fs gs ps = do
  ps' <- reduce env (updateRecordUpdatePreds ps)
  let fsSet = S.fromList fs
      (ds, rs) = partition ((`S.isSubsetOf` fsSet) . ftv) ps'
  let as = ambiguities (fs ++ gs) rs

  -- if not (null as) then do
  if mustCheck && not (null as) then do
    -- if we have ambiguities we try to resolve them with default instances
    (s, rs')      <- tryDefaults env rs
    (sDef', rs'') <- tryDefaults env (apply s rs')
    let sFinal = sDef' `compose` s
    let (ds', rs''') = partition ((`S.isSubsetOf` fsSet) . ftv) (apply sFinal ds ++ rs'')

    -- and then compute the potential leftover ambiguities
    let as' = ambiguities (fs ++ gs) rs'''
    if not (null as') then
      case head as of
        (_, IsIn _ _ (Just area):_) ->
          throwError $ CompilationError (AmbiguousType (head as)) (Context (envCurrentPath env) area)

        _ ->
          throwError $ CompilationError (AmbiguousType (head as)) NoContext
    else do
      return (ds', rs''', sFinal)
  else
    return (ds, rs, mempty)


tryDefaults :: Env -> [Pred] -> Infer (Substitution, [Pred])
tryDefaults env ps = tryDefaults' env ps ps
  where
    -- Helper that takes the original predicate list to check against
    tryDefaults' :: Env -> [Pred] -> [Pred] -> Infer (Substitution, [Pred])
    tryDefaults' env originalPs remainingPs = case remainingPs of
      (p : next) -> case p of
        IsIn "Number" [TVar tv] _ -> do
          (nextSubst, nextPS) <- tryDefaults' env originalPs next
          let s = M.singleton tv tInteger
          return (s `compose` nextSubst, nextPS)

        IsIn "Bits" [TVar tv] _ -> do
          (nextSubst, nextPS) <- tryDefaults' env originalPs next
          let s = M.singleton tv tInteger
          return (s `compose` nextSubst, nextPS)

        IsIn interface [t] _ | interface == "Eq" || interface == "Show" -> do
          (nextSubst, nextPS) <- tryDefaults' env originalPs next
          
          -- Get vars from type AFTER substitution to see what's left
          let substitutedVars = getTypeVarsInType (apply nextSubst t)
          
          if null substitutedVars || isTVar t then
            return (nextSubst, nextPS)
          else do
            let tvs = getTV <$> substitutedVars
            
            -- Check ORIGINAL predicate list (all predicates) for Number/Bits constraints
            let hasNumberOrBitsConstraint tv = any
                  (\pred -> case pred of
                    IsIn "Number" [TVar tv'] _ -> tv == tv'
                    IsIn "Bits" [TVar tv'] _ -> tv == tv'
                    _ -> False
                  )
                  originalPs
            
            -- Also check if already substituted to Integer
            let isAlreadyInteger tv = case M.lookup tv nextSubst of
                  Just ty | ty == tInteger -> True
                  _ -> False

            let tvs' = filter (\tv -> not (M.member tv nextSubst) && (hasNumberOrBitsConstraint tv || isAlreadyInteger tv)) tvs

            let tvsWithoutNumberOrBits = filter (\tv -> 
                    not (M.member tv nextSubst) && 
                    not (hasNumberOrBitsConstraint tv) &&
                    not (isAlreadyInteger tv)
                  ) tvs
            
            -- Don't default variables in complex types to Unit - they might get Number constraints
            -- through instance resolution. Only default simple type variables.
            let isSimpleTypeVar = isTVar t
            let shouldDefaultToUnit tv = isSimpleTypeVar && not (hasNumberOrBitsConstraint tv) && not (isAlreadyInteger tv)
            
            sList <- mapM (\tv ->
                -- If it has Number/Bits in original, default to Integer
                if hasNumberOrBitsConstraint tv || isAlreadyInteger tv
                  then return (Just (tv, tInteger))
                  else if shouldDefaultToUnit tv
                  then return (Just (tv, tUnit))
                  else
                    -- Don't create a substitution - leave it ambiguous for now
                    return Nothing
              ) (tvs' ++ tvsWithoutNumberOrBits)
            let s = M.fromList $ catMaybes sList
            
            return (s `compose` nextSubst, nextPS)

        _ -> do
          maybeFound <- findInst env p
          case maybeFound of
            Just (Instance (instancePreds :=> pred) _) -> do
              s                   <- unify pred p
              (nextSubst, nextPS) <- tryDefaults' env originalPs (next ++ apply s instancePreds)
              return (nextSubst, nextPS)

            Nothing -> do
              parentPreds <- getParentPredsOnly env p
              (nextSubst, nextPS) <- tryDefaults' env originalPs (parentPreds ++ next)
              return (nextSubst, p : nextPS)

      [] ->
        return (M.empty, [])


dedupePreds :: [Pred] -> [Pred]
dedupePreds = go S.empty []
  where
    -- Use a Set of (class, types) for O(n log n) dedup instead of O(n²) list scan
    go _ acc [] = reverse acc
    go seen acc (p@(IsIn cls ts _) : next) =
      let key = (cls, ts)
      in  if S.member key seen
          then go seen acc next
          else go (S.insert key seen) (p : acc) next


normalizePreds :: Substitution -> [Pred] -> [Pred]
normalizePreds s = dedupePreds . apply s


ftvForLetGenSet :: Type -> S.Set TVar
ftvForLetGenSet t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) tl1) tr1 ->
    ftv tl1 `S.union` ftv tr1

  TApp t1 t2 ->
    ftvForLetGenSet t1 `S.union` ftvForLetGenSet t2

  TRecord fields _ _ ->
    foldMap ftvForLetGenSet (M.elems fields)

  _ ->
    S.empty

ftvForLetGen :: Type -> [TVar]
ftvForLetGen = S.toList . ftvForLetGenSet

-- Shared generalization logic: compute free/generic vars, split predicates, handle mutations
generalize :: Bool -> Bool -> Env -> Area -> Substitution -> Env -> Type -> [Pred] -> Type -> Infer ([Pred], [Pred], Substitution, [Pred])
generalize isLet discardError env area sFinal envWithVarsExcluded t' ps' t = do
  let fs = S.toList (ftv (apply sFinal envWithVarsExcluded))

  (ds, rs, sSplit) <- catchError
    (split (not isLet) envWithVarsExcluded fs (ftvList t') ps')
    (\case
      _ | discardError ->
        return (ps', [], mempty)

      (CompilationError e NoContext) -> do
        throwError $ CompilationError e (Context (envCurrentPath env) area)

      (CompilationError e c) -> do
        throwError $ CompilationError e c
    )

  let rs' = dedupePreds rs
      sFinal' = sSplit `compose` sFinal
      rsFinal = dedupePreds (apply sFinal' rs')

  let mutPS =
        List.filter
          (\(IsIn cls ts _) ->
            cls == mutationInterface && not (S.null (ftv (apply sFinal' ts) `S.intersection` ftv (apply sFinal' t)))
          )
          ps'

  return (ds, rsFinal, sFinal', mutPS)



inferImplicitlyTyped :: Bool -> Options -> Bool -> Env -> Can.Exp -> Infer (Substitution, ([Pred], [Pred]), Env, Slv.Exp)
inferImplicitlyTyped discardError options isLet env exp@(Can.Canonical area _) = do
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

  (s, ps, t, e) <- infer discardError options env' { envNamesInScope = envVars env } exp
  let env'' = apply s env'

  s' <- contextualUnify' env'' discardError exp (apply s tv) t
  let s'' = s' `compose` s
      envWithVarsExcluded = env''
        { envVars = M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $ envVars env'' }

      ps' = apply s'' ps
      t'  = apply s'' tv

  (ds, rsFinal, sFinal, mutPS) <- generalize isLet discardError env area s'' envWithVarsExcluded t' ps' (apply s'' tv)

  let tFinal = apply sFinal t'
      annotateArea p = case p of
        IsIn cls ts Nothing -> IsIn cls ts (Just area)
        _                   -> p
      isSpuriousFinalPred (IsIn cls ts Nothing) =
        cls `elem` ["Number", "Bits"] && all isConcrete ts
      isSpuriousFinalPred _ = False
      rsFinal' = filter (not . isSpuriousFinalPred) (annotateArea <$> rsFinal)
      rsFinalNorm = dedupePreds rsFinal'
      vs = if isLet then ftvForLetGen tFinal else ftvList tFinal
      fsSet = ftv (apply sFinal envWithVarsExcluded)
      gs = filter (not . (`S.member` fsSet)) vs
      qtFinal = (rsFinalNorm ++ mutPS) :=> tFinal
      sc =
        if isLet && not (Slv.isNamedAbs e) then
          quantify [] qtFinal
        else
          quantify gs qtFinal

  when (not isLet && not discardError && not (null mutPS) && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  case Can.getExpName exp of
    Just n  ->
      return (sFinal, (dedupePreds (ds ++ mutPS), rsFinalNorm), extendVars env (n, sc), updateQualType e (rsFinalNorm :=> tFinal))

    Nothing ->
      return (sFinal, (dedupePreds (ds ++ mutPS), rsFinalNorm), env, updateQualType e (rsFinalNorm :=> tFinal))


inferExplicitlyTyped :: Bool -> Options -> Bool -> Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped discardError options isLet env canExp@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  qt@(qs :=> t') <- instantiate sc

  env' <- case Can.getExpName exp of
        Just n  -> do
          let scWithParents = quantify (ftvList qt) (qs :=> t')
          return $ extendVars env (n, scWithParents)

        Nothing ->
          return env

  (s, ps, t, e) <- infer discardError options env' { envNamesInScope = envVars env } exp
  psFull        <- concat <$> mapM (gatherInstPreds env') ps
  s'' <- catchError (contextualUnifyWithOrigin (if discardError then Discard else Strict) FromTypeAnnotation env canExp t' (apply s t)) (throwError . limitContextArea 2)
  let s' = s'' `compose` s

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
      tAnn = apply s' t'
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' psFull)
  (ds, rs, sFinal, mutPS) <- generalize False discardError env area s' envWithVarsExcluded tAnn ps' t

  when (not isLet && not discardError && not (null mutPS) && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  let qs'' = dedupePreds (apply sFinal qs)
      fsSet = ftv (apply sFinal envWithVarsExcluded)
      gs = filter (not . (`S.member` fsSet)) (ftvList (apply sFinal tAnn))
      t''' = mergeRecords (apply sFinal tAnn) (apply sFinal t'')
      scCheck  = quantify (ftvList (apply sFinal tAnn)) (qs'' :=> apply sFinal tAnn)
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

    let qt'  = (qs'' ++ mutPS) :=> t'''
    let sc'' = quantify gs qt'
    let env'' = case Can.getExpName exp of
          Just n  ->
            extendVars env' (n, sc'')
          Nothing ->
            env'

    return (sFinal, qs'' ++ mutPS, env'', Slv.Typed (qs :=> t') area (Slv.TypedExp e' (updateTyping typing) sc))

inferExplicitlyTyped _ _ _ _ _ = error "inferExplicitlyTyped: unreachable case"


inferExps :: Options -> Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps _ env []       = return ([], env)

inferExps options env (e : es) = do
  (e' , env'   ) <-
    catchError
      (inferExp False options env e)
      (\err -> do
        pushError err
        catchError (inferExp True options env e) (\_ -> return (Just $ toSolved e, env))
      )
  (es', nextEnv) <- inferExps options env' es

  case e' of
    Just e'' ->
      return (e'' : es', nextEnv)

    Nothing  ->
      return (es', nextEnv)


inferExp :: Bool -> Options -> Env -> Can.Exp -> Infer (Maybe Slv.Exp, Env)
inferExp _ _ env (Can.Canonical _ (Can.TypeExport _)) =
  return (Nothing, env)
inferExp discardError options env e = do
  (s, _, env', e') <- upgradeContext env (Can.getArea e) $ case e of
    Can.Canonical _ Can.TypedExp{} ->
      inferExplicitlyTyped discardError options False env e

    _ -> do
      (s, (_, placeholderPreds), env'', e') <- inferImplicitlyTyped discardError options False env e
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
