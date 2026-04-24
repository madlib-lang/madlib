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
                                                , foldl'
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
inferAbs discardError options env l@(Can.Canonical absArea (Can.Abs p@(Can.Canonical area param) body)) = do
  tv             <- newTVar Star
  env'           <- extendAbsEnv env tv p
  (s, ps, t, es) <- inferBody discardError options absArea env' { envInBody = True } body

  let t'        = apply s (tv `fn` t)
      paramType = apply s tv

  return (s, apply s ps, t', applyAbsSolve l (Slv.Typed (apply s $ ps :=> paramType) area param) es (apply s $ ps :=> t'))


-- | Body inference with finalization (defaulting + ambiguity check).
-- The 'parentArea' argument is the area of the enclosing expression
-- (function body, do-block, etc.); it is used as a fallback location for
-- ambiguity errors whose offending predicate has no source area.
inferBody :: Bool -> Options -> Area -> Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody discardError options parentArea env es = do
  (s, ps, t, es', bodyEnv) <- inferBodyRaw discardError options env es
  finalizeBody discardError options parentArea env bodyEnv s ps t es'


inferBodyRaw :: Bool -> Options -> Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp], Env)
inferBodyRaw discardError options env [e] = do
  (s, ps, t, e') <- infer discardError options env e
  return (s, ps, t, [e'], apply s env)

inferBodyRaw discardError options env (e : es) = do
  (s, (returnPreds, retainedPreds), env', e') <- inferImplicitlyTyped discardError options True env e
  (sb, ps', tb, eb, bodyEnv) <- inferBodyRaw discardError options (apply s env') es
  let finalS = sb `compose` s
      escapedPreds = case Can.getExpName e of
        Just _  -> returnPreds
        Nothing | discardError -> returnPreds
                | otherwise    -> returnPreds ++ retainedPreds

  return (finalS, apply finalS $ escapedPreds ++ ps', tb, e' : eb, apply finalS bodyEnv)


finalizeBody :: Bool -> Options -> Area -> Env -> Env -> Substitution -> [Pred] -> Type -> [Slv.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
finalizeBody discardError options parentArea env bodyEnv s bodyPreds bodyType es = do
  let appliedBodyType = apply s bodyType
      bodyFixedVars =
        S.toList $
          ftv (apply s env)
          `S.union` ftv appliedBodyType
          `S.union` foldMap (bodyRetainedTypeVars s) es
      allPreds = dedupePreds $ apply s (bodyPreds ++ bodyEnvPreds bodyEnv ++ bodyExpPreds es)

  unsolvedPreds <- filterM (fmap not . isBodyPredEntailed env) allPreds
  let ambiguousPreds = dedupePreds $ concatMap snd (ambiguities bodyFixedVars unsolvedPreds)

  (sDefaults, remainingPreds) <-
    if null ambiguousPreds then
      return (mempty, [])
    else
      defaultBodyPreds ambiguousPreds

  let sFinal = sDefaults `compose` s
      remainingAmbiguities = ambiguities bodyFixedVars remainingPreds

  when (not discardError && not (envDeferBodyAmbiguity env)) $
    case remainingAmbiguities of
      []  -> return ()
      a:_ -> throwBodyAmbiguous a

  finalPreds <- filterBodyPreds env (apply sFinal bodyPreds)
  finalExps  <- mapM (updateBodyExp options env sFinal) es

  return (sFinal, finalPreds, apply sFinal bodyType, finalExps)
  where
    defaultBodyPreds :: [Pred] -> Infer (Substitution, [Pred])
    defaultBodyPreds ps = do
      (sDef, ps')   <- tryDefaults env ps
      (sDef', ps'') <- tryDefaults env (apply sDef ps')
      return (sDef' `compose` sDef, ps'')

    -- Ambiguity errors fall back to the enclosing body's area when no
    -- offending predicate carries a more specific source location.
    throwBodyAmbiguous :: Ambiguity -> Infer a
    throwBodyAmbiguous ambiguity =
      let area = firstPredArea (snd ambiguity) `orElse` parentArea
      in  throwError $ CompilationError (AmbiguousType ambiguity) (Context (envCurrentPath env) area)

    firstPredArea :: [Pred] -> Maybe Area
    firstPredArea []                       = Nothing
    firstPredArea (IsIn _ _ ma : ps)
      | Just _ <- ma                       = ma
      | otherwise                          = firstPredArea ps

    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing  y = y

    bodyRetainedTypeVars :: Substitution -> Slv.Exp -> S.Set TVar
    bodyRetainedTypeVars subst (Slv.Typed (_ :=> t) _ _) =
      ftvForLetGenSet (apply subst t)

    bodyRetainedTypeVars _ _ =
      mempty

    bodyExpPreds :: [Slv.Exp] -> [Pred]
    bodyExpPreds =
      concatMap $ \case
        Slv.Typed (ps :=> _) _ _ -> ps
        _                        -> []

    bodyEnvPreds :: Env -> [Pred]
    bodyEnvPreds env' = concatMap schemePreds (M.elems (envVars env'))

    schemePreds :: Scheme -> [Pred]
    schemePreds (Forall _ (ps :=> _)) = ps

    filterBodyPreds :: Env -> [Pred] -> Infer [Pred]
    filterBodyPreds env' =
      filterM (fmap not . isBodyPredEntailed env')

    isBodyPredEntailed :: Env -> Pred -> Infer Bool
    isBodyPredEntailed env' p =
      catchError
        (entail env' [] p)
        (\err ->
          if envDeferBodyAmbiguity env'
            then return False
            else throwError err
        )

    updateBodyExp :: Options -> Env -> Substitution -> Slv.Exp -> Infer Slv.Exp
    updateBodyExp options' env' subst (Slv.Typed (ps :=> t) area e) = do
      ps' <- filterBodyPreds env' (apply subst ps)
      updateExpTypes options' env' False subst (Slv.Typed (ps' :=> t) area e)

    updateBodyExp options' env' subst e =
      updateExpTypes options' env' False subst e


-- INFER APP

inferApp :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp discardError options env (Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea argContent) final)) = do
  tv                  <- newTVar Star
  (s1, ps1, t1, eabs) <- infer discardError options env abs
  (s2, ps2, t2, earg) <- infer discardError options (apply s1 env) arg

  let expForContext = arg  -- Always point at the argument (the "wrong" value)

  -- Enrich the origin with function context (expected type + full signature).
  -- At each curried application level, funcType is the *remaining* function type,
  -- so the expected param is always the 1st parameter of funcType (not the idx-th).
  let baseOrigin = getAppOrigin abs
      funcType   = apply s2 t1
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

  s3 <- contextualUnifyWithOriginAndSecondary (if discardError then Discard else Strict) origin secondaryLoc env expForContext (apply s2 t1) (apply s1 t2 `fn` tv)

  let t = apply s3 tv
  let s = s3 `compose` s2 `compose` s1

  -- For JSX records: fill missing Maybe-typed fields with Nothing
  earg' <- case argContent of
    Can.JsxRecord jsxFields -> do
      let explicitNames = S.fromList [ n | Can.Canonical _ (Can.Field (n, _)) <- jsxFields ]
      let resolvedArgType = apply s t2
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

  let ps     = apply s (ps1 ++ ps2)
  let solved = Slv.Typed (ps :=> t) area $ Slv.App eabs (updateQualType earg' $ ps :=> apply s t2) final

  return (s, ps, t, solved)



-- INFER TEMPLATE STRINGS

inferTemplateString :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTemplateString discardError options env (Can.Canonical area (Can.TemplateString exps)) = do
  (inferredRev, fullSubst) <- foldM
    (\(acc, subst) exp -> do
      (s, ps, t, e) <- infer discardError options (apply subst env) exp
      let subst' = s `compose` subst
      sString <- contextualUnify' (apply subst' env) discardError exp (apply subst' t) tStr
      let subst'' = sString `compose` subst'
      return ((ps, t, e) : acc, subst'')
    )
    ([], mempty)
    exps

  let inferred  = reverse inferredRev
      elemTypes = (\(_, t, _) -> t) <$> inferred
      elemExps  = (\(_, _, e) -> e) <$> inferred
      elemPS    = (\(ps, _, _) -> ps) <$> inferred
      qs        = uncurry (:=>) <$> zip elemPS elemTypes

  let updatedExp = Slv.Typed
        ([] :=> tStr)
        area
        (Slv.TemplateString ((\(t, e) -> updateQualType e (apply fullSubst t)) <$> zip qs elemExps))

  return (fullSubst, apply fullSubst (concat elemPS), tStr, updatedExp)



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
  (s1, ps1, t1, e1) <- infer discardError options env' exp
  s2                <- catchError (contextualUnify Strict env' e currentType t1) (const $ return M.empty)
  --  ^ We can skip this error as we mainly need the substitution. It would fail in inferExplicitlyTyped anyways.
  let s  = s2 `compose` s1
  let t2 = apply s t1

  mutationPs <-
    if M.member name (envNamesInScope env) && envInBody env && not discardError then do
      pushError $ CompilationError BadMutation (Context (envCurrentPath env) area)
      return []
    else
      return []

  let psOut = apply s (currentPreds ++ ps1 ++ mutationPs)
  return (s, psOut, apply s t2, applyAssignmentSolve e name e1 (apply s $ (currentPreds ++ ps1) :=> t2))



-- INFER MUTATE

inferMutate :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferMutate discardError options env e@(Can.Canonical area (Can.Mutate lhs exp)) = do
  (s1, ps1, t1, e1) <- infer discardError options env lhs
  (s2, ps2, t2, e2) <- infer discardError options (apply s1 env) exp
  s3 <- catchError
    (contextualUnify Strict env e t1 t2)
    (\err -> do
      if discardError then do
        return mempty
      else
        throwError err
    )

  let s  = s3 `compose` s2 `compose` s1
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

  let psOut = apply s (ps1 ++ ps2 ++ mutationPs)
  return
    ( s
    , psOut
    , apply s t3
    , Slv.Typed (psOut :=> apply s t3) area (Slv.Mutate e1 e2)
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

  elems -> do
    tv               <- newTVar Star

    -- Accumulate list items and pred chunks reversed (cons O(1)) then reverse — avoids O(n²).
    -- The index tracks which element we're at (1-based) for better error messages.
    (s', psChunksRev, t', esRev, _) <- foldlM
      (\(s, pssRev, t, lis, idx) elem -> do
        (s', ps', t'', li) <- inferListItem discardError options (apply s env) (fromMaybe tv t) elem
        (s'', tr) <- case t of
          Nothing ->
            return (mempty, t'')

          Just t''' -> do
            s'''' <- contextualUnifyWithOrigin (if discardError then Discard else Strict) (FromListElement idx) env elem (apply s' t''') t''
            return (s'''', pickJSXChild t''' t'')

        let s''' = s'' `compose` s' `compose` s
        return (s''', ps' : pssRev, Just $ apply s''' tr, li : lis, idx + 1)
      )
      (mempty, [], Nothing, [], 1)
      elems

    let psRaw = concat (reverse psChunksRev)
    let (Just t'') = t'
    let es = reverse esRev

    s'' <- contextualUnify' env discardError listExp tv t''
    let s''' = s'' `compose` s'

    let t  = tListOf (apply s''' tv)
    let ps = apply s''' psRaw

    return (s''', ps, t, Slv.Typed (ps :=> t) area (Slv.ListConstructor es))


inferListItem :: Bool -> Options -> Env -> Type -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem discardError options env _ (Can.Canonical area li) = case li of
  Can.ListItem exp -> do
    (s1, ps, t, e) <- infer discardError options env exp
    return (s1, ps, t, Slv.Typed (ps :=> t) area $ Slv.ListItem e)

  Can.ListSpread exp -> do
    (s1, ps, t, e) <- infer discardError options env exp
    tv <- newTVar Star
    s2 <- contextualUnify' env discardError exp (tListOf tv) t

    let s = s2 `compose` s1

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
  -- Accumulate types/exps/pred chunks reversed (cons O(1)) then reverse — avoids O(n²).
  (s, psChunksRev, tsRev, esRev) <-
    foldM
      (\(s, psRev, ts, es) e -> do
          (s', ps', t', e') <- infer discardError options (apply s env) e
          return (s' `compose` s, ps' : psRev, t' : ts, e' : es)
      ) (M.empty, [], [], []) elems
  let ps = concat (reverse psChunksRev)

  let elemTypes = reverse tsRev
  let elemEXPS  = reverse esRev
  let tupleT    = getTupleCtor (length elems)
  let t         = apply s (foldl' TApp tupleT elemTypes)
  let ps'       = apply s ps

  return (s, ps', t, Slv.Typed (ps' :=> t) area (Slv.TupleConstructor elemEXPS))



-- INFER RECORD

inferRecord :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord discardError options env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  -- Accumulate reversed (cons O(1)) then reverse at end — avoids O(n²) with ++
  (subst, inferredFieldsRev) <- foldM (
        \(fieldSubst, result) field -> do
          (s, ps, ts, e) <- inferRecordField discardError options (apply fieldSubst env) field
          let nextSubst = s `compose` fieldSubst
          return (nextSubst, (ps, (\(n, t) -> (n, apply nextSubst t)) <$> ts, e) : result)
      ) (mempty, []) fields
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

  (recordType, extraSubst) <- case apply subst <$> base of
    Just (TRecord spreadFields baseBase optionalFields) -> do
      -- Merge the spread record's fields with our explicit fields
      -- The spread fields take precedence if there are conflicts
      let mergedFields = M.fromList fieldTypes' `M.union` spreadFields
      return (TRecord mergedFields (apply subst <$> baseBase) optionalFields, mempty)

    Just tBase -> do
      -- The spread is a type variable or other type - unify it with a record type
      -- that has our fields and a row variable for extension
      baseVar <- newTVar Star
      let recordWithBase = TRecord (M.fromList fieldTypes') (Just baseVar) mempty
      s <- contextualUnify' env discardError exp (apply subst tBase) recordWithBase
      -- After unification, tBase should be resolved to a record type
      -- Return the unified type with the row variable preserved
      let unifiedBase = apply s tBase
      case unifiedBase of
        TRecord unifiedFields unifiedBase' unifiedOptionalFields ->
          return (TRecord unifiedFields unifiedBase' unifiedOptionalFields, s)
        _ ->
          -- Fallback: use the record we created with the row variable
          return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty, s)

    Nothing ->
      -- No spread - create a closed record (no row variable)
      -- This allows the record to be used in contexts that don't require extensibility
      return (TRecord (M.fromList fieldTypes') Nothing mempty, mempty)

  let finalSubst = extraSubst `compose` subst
  let allPS      = apply finalSubst (concat fieldPS)
  let recordT    = apply finalSubst recordType

  return (finalSubst, allPS, recordT, Slv.Typed (allPS :=> recordT) area (Slv.Record fieldEXPS))


-- | Like inferRecord but creates an extensible record (with a base type variable)
-- so that missing Maybe-typed fields can be filled with Nothing after unification.
inferJsxRecord :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferJsxRecord discardError options env exp = do
  let Can.Canonical area (Can.JsxRecord fields) = exp

  -- Accumulate reversed (cons O(1)) then reverse at end — avoids O(n²) with ++
  (subst, inferredFieldsRev) <- foldM (
        \(fieldSubst, result) field -> do
          (s, ps, ts, e) <- inferRecordField discardError options (apply fieldSubst env) field
          let nextSubst = s `compose` fieldSubst
          return (nextSubst, (ps, (\(n, t) -> (n, apply nextSubst t)) <$> ts, e) : result)
      ) (mempty, []) fields
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

  (recordType, extraSubst) <- case apply subst <$> base of
    Just (TRecord spreadFields baseBase optionalFields) -> do
      let mergedFields = M.fromList fieldTypes' `M.union` spreadFields
      return (TRecord mergedFields (apply subst <$> baseBase) optionalFields, mempty)

    Just tBase -> do
      baseVar <- newTVar Star
      let recordWithBase = TRecord (M.fromList fieldTypes') (Just baseVar) mempty
      s <- contextualUnify' env discardError exp (apply subst tBase) recordWithBase
      let unifiedBase = apply s tBase
      case unifiedBase of
        TRecord unifiedFields unifiedBase' unifiedOptionalFields ->
          return (TRecord unifiedFields unifiedBase' unifiedOptionalFields, s)
        _ ->
          return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty, s)

    Nothing -> do
      -- JSX record without spread: create an EXTENSIBLE record with a base type variable.
      -- This allows unification to absorb missing fields into the base, which we later
      -- check are all Maybe-typed and fill with Nothing.
      baseVar <- newTVar Star
      return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty, mempty)

  let finalSubst = extraSubst `compose` subst
  let allPS      = apply finalSubst (concat fieldPS)
  let recordT    = apply finalSubst recordType

  return (finalSubst, allPS, recordT, Slv.Typed (allPS :=> recordT) area (Slv.Record fieldEXPS))


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
  tv <- newTVar Star
  (s1, ps1, t1, earr)   <- infer discardError options env arr
  (s2, ps2, t2, eindex) <- infer discardError options (apply s1 env) index
  s3 <- contextualUnify' env discardError arr   (apply s2 t1) (tArrayOf tv)
  s4 <- contextualUnify' env discardError index (apply s3 t2) tInteger

  let s  = s4 `compose` s3 `compose` s2 `compose` s1
  let t  = apply s tv
  let ps = apply s (ps1 ++ ps2)

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
    tv                  <- newTVar Star
    (s1, _  , t1, eabs) <- infer discardError options env abs
    (s2, ps2, t2, earg) <- infer discardError options (apply s1 env) rec

    let t1Applied = apply s2 t1
    s3 <- catchError
      (contextualUnifyAccess env fa t1Applied (t2 `fn` tv))
      (\err -> do
        if discardError then do
          return $ gentleUnify t1Applied (t2 `fn` tv)
        else
          throwError err
      )

    let s = s3 `compose` s2 `compose` s1
    let t = apply s tv
    let ps2' = apply s ps2
    let solved = Slv.Typed (ps2' :=> t) area (Slv.Access earg eabs)

    return (s, ps2', t, solved)



-- INFER IF

inferIf :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf discardError options env (Can.Canonical area (Can.If cond truthy falsy)) = do
  (s1, ps1, tcond, econd) <- infer discardError options env cond
  (s2, ps2, ttruthy, etruthy) <- infer discardError options (apply s1 env) truthy
  (s3, ps3, tfalsy, efalsy) <- infer discardError options (apply (s2 `compose` s1) env) falsy

  let s123     = s3 `compose` s2 `compose` s1
      tfalsy'  = apply s123 tfalsy
      ttruthy' = apply s123 ttruthy
      unifyBranches = contextualUnifyWithOrigin (if discardError then Discard else Strict) (FromIfBranches ElseBranch) env falsy tfalsy' ttruthy'
  s4 <- catchError unifyBranches (flipUnificationErrorWithBranch ThenBranch)
  -- Use the full accumulated substitution (not just s4) when checking the
  -- condition's type — earlier branches may have bound variables that flow
  -- into the condition through shared environment names.
  let s4123 = s4 `compose` s123
  s5 <- contextualUnifyWithOrigin (if discardError then Discard else Strict) FromIfCondition env cond tBool (apply s4123 tcond)

  let s  = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
  let t  = apply s ttruthy
  let ps = apply s (ps1 ++ ps2 ++ ps3)

  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.If econd etruthy efalsy))



-- INFER While

inferWhile :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhile discardError options env (Can.Canonical area (Can.While cond body)) = do
  (s1, ps1, tcond, econd) <- infer discardError options env cond
  (s2, ps2, tbody, ebody) <- infer discardError options (apply s1 env) body

  let s3 = s2 `compose` s1

  s4 <- contextualUnifyWithOrigin (if discardError then Discard else Strict) FromWhileCondition env cond tBool (apply s3 tcond)
  s5 <- contextualUnify' env discardError body tUnit (apply s3 tbody)

  let s  = s5 `compose` s4 `compose` s3
  let t  = apply s tbody
  let ps = apply s (ps1 ++ ps2)

  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.While econd ebody))



-- INFER DO

inferDo :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferDo discardError options env (Can.Canonical area (Can.Do exps)) = do
  (s, ps, t, exps') <- inferBody discardError options area env exps

  return (s, apply s ps, apply s t, Slv.Typed (apply s $ ps :=> t) area (Slv.Do exps'))



-- INFER WHERE

inferWhere :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere discardError options env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e)          <- infer discardError options env exp
  tv                     <- newTVar Star
  (pssRev, issSubstitution, _) <- foldM
    (\(res, currSubst, idx) is -> do
      r@(subst, _, _) <- inferBranch discardError options (apply currSubst env) (apply currSubst tv) (apply currSubst t) idx is
      return (r : res, subst `compose` currSubst, idx + 1)
    )
    ([], s, 1)
    iss
  let pss = reverse pssRev

  let ps' = concat $ T.mid <$> pss
  -- move this within the foldM
  s' <- contextualUnifyElems env $ zip iss (apply issSubstitution . Slv.getType . T.lst <$> pss)

  let s''  = s' `compose` issSubstitution
      ps'' = apply s'' (ps ++ ps')

  let iss = (\(Slv.Typed t a (Slv.Is pat exp)) -> Slv.Typed (apply s'' t) a (Slv.Is (updatePatternTypes s'' mempty pat) exp)) . T.lst <$> pss
  let wher = Slv.Typed (ps'' :=> apply s'' tv) area $ Slv.Where (updateQualType e (apply s'' $ ps :=> t)) iss
  return (s'', ps'', apply s'' tv, wher)


inferBranch :: Bool -> Options -> Env -> Type -> Type -> Int -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch discardError options env tv t branchIdx (Can.Canonical area (Can.Is pat exp)) = do
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
  (s', ps', t'', e') <- infer discardError options envWithPatternVars exp
  s'' <- contextualUnify' env discardError exp tv (apply (s' `compose` s) t'')

  let subst    = s'' `compose` s' `compose` s
  let allPreds = apply subst (ps ++ ps')
  let psBody   = apply subst ps'

  return
    ( subst
    , allPreds
    , Slv.Typed (allPreds :=> apply subst (t' `fn` tv)) area
      $ Slv.Is (updatePatternTypes subst (apply subst <$> vars') pat') (updateQualType e' (psBody :=> apply subst t''))
    )



-- INFER TYPEDEXP

inferTypedExp :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp discardError options env e@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  (_ :=> t) <- instantiate sc
  (s1, ps1, t1, e1) <- infer discardError options env exp
  s2 <- contextualUnify' env discardError e t t1
  let s = s2 `compose` s1

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
  let fsSet    = S.fromList fs
      (ds, rs) = partition ((`S.isSubsetOf` fsSet) . ftv) ps'
      as       = ambiguities (fs ++ gs) rs

  if mustCheck && not (null as) then do
    -- Two rounds of defaulting: the first pass instantiates ambiguous numeric
    -- vars; the second resolves any extra predicates that surface as a result.
    -- We deliberately return only s1 (the first pass) to preserve the historical
    -- semantics where the second pass acts as a verification step rather than
    -- committing additional defaults to the surrounding scheme.
    (s1, rs1) <- tryDefaults env rs
    (s2, rs2) <- tryDefaults env (apply s1 rs1)
    -- After defaulting, predicates that became tautologies (e.g. `Number Integer`
    -- once `a` is bound to Integer) must be eliminated, otherwise they'd leak
    -- into the deferred `ds'` list.
    dsClean <- elimTauts env (apply s2 ds)
    let (ds', rs') = partition ((`S.isSubsetOf` fsSet) . ftv) (dedupePreds (dsClean ++ rs2))
        as'        = ambiguities (fs ++ gs) rs'

    case as' of
      [] -> return (ds', rs', s1)
      a:_ ->
        let ctx = case snd a of
                    IsIn _ _ (Just ar) : _ -> Context (envCurrentPath env) ar
                    _                       -> NoContext
        in  throwError $ CompilationError (AmbiguousType a) ctx
  else
    return (ds, rs, mempty)


tryDefaults :: Env -> [Pred] -> Infer (Substitution, [Pred])
tryDefaults env = tryDefaultsWithContext env []


tryDefaultsWithContext :: Env -> [Pred] -> [Pred] -> Infer (Substitution, [Pred])
tryDefaultsWithContext env contextPs ps = do
  expandedPs <- expandDefaultablePreds env (contextPs ++ ps)
  let numericVars = S.fromList
        [ tv
        | IsIn cls [TVar tv] _ <- expandedPs
        , cls == "Number" || cls == "Bits"
        ]
      unitVars = S.fromList
        [ tv
        | IsIn cls [TVar tv] _ <- expandedPs
        , cls == "Eq" || cls == "Show"
        ] `S.difference` numericVars
      subst = M.fromList $
        ((, tInteger) <$> S.toList numericVars) ++
        ((, tUnit) <$> S.toList unitVars)

  if M.null subst then
    return (mempty, ps)
  else do
    let ps' = apply subst ps
    ps'' <- filterM ((not <$>) . entail env []) ps'
    return (subst, ps'')
  where
    expandDefaultablePreds :: Env -> [Pred] -> Infer [Pred]
    expandDefaultablePreds env' = go S.empty
      where
        go _ [] = return []
        go seen (p@(IsIn cls ts _) : rest)
          | S.member key seen = go seen rest
          | cls == "Eq" || cls == "Show" = do
              children <- defaultableInstancePreds env' p
              (p :) <$> go (S.insert key seen) (children ++ rest)
          | otherwise =
              (p :) <$> go (S.insert key seen) rest
          where
            key = (cls, ts)

    defaultableInstancePreds :: Env -> Pred -> Infer [Pred]
    defaultableInstancePreds env' p =
      catchError
        (do
          maybeInst <- findInst env' p
          case maybeInst of
            Just (Instance (instancePreds :=> instanceHead) _) -> do
              s <- unify instanceHead p
              return (apply s instancePreds)
            Nothing ->
              return []
        )
        (const $ return [])


dedupePreds :: [Pred] -> [Pred]
dedupePreds ps =
  -- Two-pass dedup: first pick the best representative per (class, types) key
  -- (preferring one that carries a source Area for error reporting), then emit
  -- in original order with duplicates removed.
  let bestArea = foldr keepBetter M.empty ps
  in  go S.empty [] bestArea ps
  where
    keepBetter p@(IsIn cls ts ma) m =
      let key = (cls, ts)
      in  case M.lookup key m of
            Just (IsIn _ _ existing)
              | isJust existing       -> m
              | isJust ma             -> M.insert key p m
              | otherwise             -> m
            Nothing                   -> M.insert key p m

    go _ acc _ [] = reverse acc
    go seen acc best (p@(IsIn cls ts _) : next) =
      let key = (cls, ts)
      in  if S.member key seen
          then go seen acc best next
          else
            let chosen = M.findWithDefault p key best
            in  go (S.insert key seen) (chosen : acc) best next


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
  let sFinal' = sSplit `compose` sFinal

  let mutPS =
        List.filter
          (\(IsIn cls ts _) ->
            cls == mutationInterface && not (S.null (ftv (apply sFinal ts) `S.intersection` ftv (apply sFinal t)))
          )
          ps'

  return (ds, rs', sFinal', mutPS)



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

  (ds, rs', sFinal, mutPS) <- generalize isLet discardError env area s'' envWithVarsExcluded t' ps' (apply s'' tv)

  -- Apply the full substitution (including defaulting from split) to the type
  -- and predicates *before* quantifying. Otherwise, a type variable that was
  -- defaulted to a concrete type (e.g. `a -> Integer` via Number defaulting)
  -- would still appear as TGen in the resulting scheme, leaving the scheme
  -- spuriously polymorphic in `a`.
  let tFinal       = apply sFinal t'
      qpsFinal     = apply sFinal (rs' ++ mutPS)
      fsSet        = ftv (apply sFinal envWithVarsExcluded)
      vsFinal      = if isLet then ftvForLetGen tFinal else ftvList tFinal
      gs           = filter (not . (`S.member` fsSet)) vsFinal
      sc =
        if isLet && not (Slv.isNamedAbs e) then
          quantify [] (qpsFinal :=> tFinal)
        else
          quantify gs (qpsFinal :=> tFinal)

  when (not isLet && not discardError && not (null mutPS) && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  case Can.getExpName exp of
    Just n  ->
      return (sFinal, (ds ++ mutPS, rs'), extendVars env (n, sc), updateQualType e (apply sFinal $ rs' :=> t'))

    Nothing ->
      return (sFinal, (ds ++ mutPS, rs'), env, updateQualType e (apply sFinal $ rs' :=> t'))


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
  s'' <- catchError (contextualUnifyWithOrigin (if discardError then Discard else Strict) FromTypeAnnotation env canExp t' (apply s t)) (throwError . limitContextArea 2)
  let s' = s'' `compose` s

  -- Gather instance predicates *after* the full substitution is computed so
  -- predicates whose type variables were just bound by the signature unification
  -- get expanded against the right (concrete) types instead of stale fresh vars.
  psFull <- concat <$> mapM (gatherInstPreds env') (apply s' ps)

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
  ps'      <- filterM ((not <$>) . entail env' qs') psFull
  (ds, rs, substDefaultResolution, mutPS) <- generalize False discardError env area s' envWithVarsExcluded (apply s' t') ps' t

  when (not isLet && not discardError && not (null mutPS) && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  let qs'' = dedupePreds qs'
      fsSet = ftv (apply s' envWithVarsExcluded)
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
      (_ :=> t1) <- instantiate sc
      (_ :=> t2) <- instantiate scCheck
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

    return (substDefaultResolution `compose` s', qs'' ++ mutPS, env'', Slv.Typed (qs :=> t') area (Slv.TypedExp e' (updateTyping typing) sc))

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
