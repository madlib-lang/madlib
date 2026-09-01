{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Infer.Unify where


import           Infer.Type
import           Infer.Substitute
import           Infer.Instantiate
import           Error.Error
import           Error.Context
import           Infer.Infer
import           Infer.Env
import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified AST.Canonical                 as Can



varBind :: TVar -> Type -> Infer Substitution
varBind tv t | t == TVar tv        = return M.empty
             | occursCheck tv t    = throwError $ CompilationError (InfiniteType tv t) NoContext
             | kind tv /= kind t   = throwError $ CompilationError (KindError (TVar tv, kind tv) (t, kind t)) NoContext
             | otherwise           = return $ M.singleton tv t

class Unify t where
  unify :: t -> t -> Infer Substitution

instance Unify Type where
  unify (l `TApp` r) (l' `TApp` r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return $ compose s2 s1

  unify (TRecordRow row optionalFields) (TRecordRow row' optionalFields') =
    unifyRows (rowFromFields optionalFields row) (rowFromFields optionalFields' row')

  unify (TVar tv) t         = varBind tv t
  unify t         (TVar tv) = varBind tv t
  unify t1@(TCon a fpa _) t2@(TCon b fpb _)
    | a == b && fpa == fpb = return M.empty
    | a == b && (fpa == "JSX" || fpb == "JSX") = return M.empty
    | a /= b               = throwError $ CompilationError (UnificationError TypeMismatch { tmFound = t2, tmExpected = t1, tmOrigin = NoOrigin, tmSecondaries = [] }) NoContext
    | fpa /= fpb           = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId a) fpa fpb) NoContext

  unify (TCon (TC tNameA _) _ _) (TApp (TCon (TC tNameB _) _ _) _)
    | tNameA == "String" && tNameB == "Element" =
        return mempty

  unify (TApp (TCon (TC tNameB _) _ _) _) (TCon (TC tNameA _) _ _)
    | tNameB == "Element" && tNameA == "String" =
        return mempty

  unify t1 t2 =
    throwError $ CompilationError (UnificationError TypeMismatch { tmFound = t2, tmExpected = t1, tmOrigin = NoOrigin, tmSecondaries = [] }) NoContext


-- | Scoped-label row unification.  `rewriteRow` removes the first visible
-- occurrence of a label, rebuilding the intervening row extensions.  That is
-- what makes `{ ...r, x: a }` lawful even when `r` itself has an `x`: the
-- outer occurrence is selected and the tail stays intact.
unifyRows :: Type -> Type -> Infer Substitution
unifyRows left right = case left of
  TRowEmpty -> case right of
    TRowEmpty -> return M.empty
    TVar tv | kind tv == Row -> varBind tv TRowEmpty
    _ -> unifyRows right TRowEmpty

  TVar tv | kind tv == Row -> varBind tv right

  TRowExtend label fieldType tail -> do
    (otherFieldType, residual, rewriteSubst) <- rewriteRow label right
    fieldSubst <- unify (apply rewriteSubst fieldType) (apply rewriteSubst otherFieldType)
    -- An outer label shadows every equal label below it.  Compare the
    -- residual rows only after masking those hidden occurrences; otherwise
    -- `{ ...r, x: a }` unified with a closed record containing `x` would
    -- incorrectly require a second `x` from `r`.
    let visibleTail = maskRowLabel label (apply fieldSubst (apply rewriteSubst tail))
    tailSubst <- unifyRows visibleTail
                           (apply fieldSubst (apply rewriteSubst residual))
    return $ tailSubst `compose` fieldSubst `compose` rewriteSubst

  _ -> throwError $ CompilationError FatalError NoContext


rewriteRow :: Id -> Type -> Infer (Type, Type, Substitution)
rewriteRow label row = case row of
  TRowExtend name fieldType tail
    | name == label -> return (fieldType, maskRowLabel label tail, M.empty)
    | otherwise -> do
        (foundType, residual, subst) <- rewriteRow label tail
        return (apply subst foundType, TRowExtend name (apply subst fieldType) residual, subst)

  TVar tv | kind tv == Row -> do
    fieldType <- newTVar Star
    residual  <- newTVar Row
    subst     <- varBind tv (TRowExtend label fieldType residual)
    return (apply subst fieldType, apply subst residual, subst)

  TRowEmpty ->
    throwError $ CompilationError (RecordMissingFields [label] []) NoContext

  _ -> throwError $ CompilationError FatalError NoContext


-- | Remove concrete occurrences of a label that are hidden by an outer
-- scoped extension.  A row variable is deliberately left untouched: it may
-- later be instantiated with that label, which remains hidden by the outer
-- scope and therefore needs no lacks constraint.
maskRowLabel :: Id -> Type -> Type
maskRowLabel label = go
  where
    go (TRowExtend name fieldType tail)
      | name == label = go tail
      | otherwise     = TRowExtend name fieldType (go tail)
    go row = row


-- | Check that an inferred implementation can inhabit an explicitly declared
-- function type.  Function arguments are contravariant: an implementation
-- which only reads `{ body, headers }` can safely be published as accepting a
-- larger closed `{ body, headers, url }` request.  This is essential for
-- spread updates, where the overwritten field deliberately is not required
-- from the input row.  Results remain checked in the ordinary direction.
--
-- This is deliberately narrower than general subtyping.  It only relaxes
-- record *arguments* at a signature boundary; every other type continues to
-- use HM unification.
signatureUnify :: Type -> Type -> Infer Substitution
signatureUnify declared inferred
  | isFunctionType declared && isFunctionType inferred
  , let declaredArgs = getParamTypes declared
  , let inferredArgs = getParamTypes inferred
  , length declaredArgs == length inferredArgs
  , not (null declaredArgs) = do
      argSubst <- signatureArguments M.empty (zip inferredArgs declaredArgs)
      resultSubst <- catchError
        (signatureUnify (apply argSubst (getReturnType declared))
                        (apply argSubst (getReturnType inferred)))
        (throwError . markSignatureReturnMismatch)
      return (resultSubst `compose` argSubst)
  | otherwise = unify declared inferred
  where
    -- `(implementation, declaration)`: every field the implementation reads
    -- must be supplied by the public declaration.  A row tail is open-ended
    -- and therefore contributes no additional required labels here.
    signatureArguments subst [] = return subst
    signatureArguments subst ((implementation, declaration) : rest) = do
      current <- inputUnify (apply subst implementation) (apply subst declaration)
      signatureArguments (current `compose` subst) rest

    inputUnify implementation declaration = case (recordVisibleParts implementation, recordVisibleParts declaration) of
      (Just (implementationFields, _, implementationOptional), Just (declarationFields, _, declarationOptional)) -> do
        let required = implementationFields <> implementationOptional
            supplied = declarationFields <> declarationOptional
            missing = M.keys (M.difference required supplied)
        unless (null missing) $
          throwError $ CompilationError (RecordMissingFields missing (M.keys supplied)) NoContext
        unifyVars' M.empty
          [ fieldType | (fieldName, fieldType) <- M.toAscList required
                      , M.member fieldName supplied
          ]
          [ supplied M.! fieldName | (fieldName, _) <- M.toAscList required
                                  , M.member fieldName supplied
          ]
      _ -> unify declaration implementation

    -- Preserve which phase of signature checking failed.  The caller replaces
    -- the empty name with the declared binding name when it adds source
    -- context; without this marker a return mismatch is indistinguishable from
    -- an argument mismatch after recursive function unification.
    markSignatureReturnMismatch err = case err of
      CompilationError (UnificationError tm) errCtx ->
        CompilationError
          (UnificationError tm { tmOrigin = FromFunctionReturn "" })
          errCtx
      other -> other




instance (Unify t, Show t, Substitutable t) => Unify [t] where
  unify (x : xs) (y : ys) = do
    s1 <- unify x y
    s2 <- unify (apply s1 xs) (apply s1 ys)
    return (compose s2 s1)
  unify [] [] = return nullSubst
  unify _  _  = throwError $ CompilationError Error NoContext


unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify (apply s tp) (apply s tp')
  unifyVars (compose s1 s) xs
unifyVars s [] = return s

unifyVars' :: Substitution -> [Type] -> [Type] -> Infer Substitution
unifyVars' s (tp : xs) (tp' : xs') = do
  s1 <- unify (apply s tp) (apply s tp')
  unifyVars' (compose s1 s) xs xs'
unifyVars' s [] [] = return s
unifyVars' _ _ _ = throwError $ CompilationError Error NoContext


unifyElems :: Env -> [Type] -> Infer Substitution
unifyElems _ []      = return M.empty
unifyElems _ (h : r) = unifyElems' h r

unifyElems' :: Type -> [Type] -> Infer Substitution
unifyElems' _ []        = return M.empty
unifyElems' t (t' : xs) = do
  s1 <- unify t' t
  s2 <- unifyElems' t xs
  return $ compose s2 s1



class Match t where
  match :: t -> t -> Infer Substitution

instance Match Type where
  match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match (apply sl r) (apply sl r')
    merge sl sr
  match (TVar u) t | kind u == kind t =
    return $ M.singleton u t
  match (TCon tc1 fp1 _) (TCon tc2 fp2 _)
    | tc1 == tc2 && fp1 == fp2 = return nullSubst
    | tc1 == tc2 && (fp1 == "JSX" || fp2 == "JSX") = return M.empty
    | fp1 /= fp2 = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId tc1) fp1 fp2) NoContext
  match (TRecordRow row optionalFields) (TRecordRow row' optionalFields') =
    matchRows (rowFromFields optionalFields row)
              (rowFromFields optionalFields' row')
  match t1 t2 = throwError $ CompilationError (UnificationError TypeMismatch { tmFound = t2, tmExpected = t1, tmOrigin = NoOrigin, tmSecondaries = [] }) NoContext


-- | Asymmetric row matching used for instance heads. Only variables in the
-- first row are bound; goal variables in the second row remain rigid. This is
-- intentionally different from unification, which may bind either side.
matchRows :: Type -> Type -> Infer Substitution
matchRows (TVar tv) row
  | kind tv == Row = return (M.singleton tv row)
matchRows TRowEmpty TRowEmpty = return M.empty
matchRows patternRow@(TRowExtend label fieldType tail) goalRow =
  case rewriteKnownRow label goalRow of
    Nothing -> rowMatchError patternRow goalRow
    Just (goalFieldType, residual) -> do
      fieldSubst <- match fieldType goalFieldType
      tailSubst <- matchRows
        (maskRowLabel label (apply fieldSubst tail))
        (apply fieldSubst residual)
      return (tailSubst `compose` fieldSubst)
matchRows patternRow goalRow = rowMatchError patternRow goalRow


rowMatchError :: Type -> Type -> Infer a
rowMatchError patternRow goalRow =
  throwError $ CompilationError
    (UnificationError TypeMismatch
      { tmFound = goalRow
      , tmExpected = patternRow
      , tmOrigin = NoOrigin
      , tmSecondaries = []
      })
    NoContext

instance Match t => Match [t] where
  -- Fast path for the common single-element case
  match [t] [t'] = match t t'
  match []  []   = return nullSubst
  match ts  ts'
    | length ts /= length ts' = throwError $ CompilationError Error NoContext
    | otherwise = do
        ss <- zipWithM match ts ts'
        let totalKeys = sum (map M.size ss)
            merged    = M.unions ss
        if M.size merged == totalKeys
          then return merged
          else foldM merge nullSubst ss


data UnifyStrategy = Strict | Discard | AccessStyle
  deriving (Eq)

contextualUnify :: UnifyStrategy -> Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnify strategy env exp t1 t2 = catchError
  (unify t1 t2)
  (\case
    _ | strategy == Discard ->
      return $ gentleUnify t1 t2

    (CompilationError (UnificationError tm) ctx) -> do
      -- Report the full types being unified at this call site, never a
      -- sub-part: truncating (e.g. to the first function parameter) hides
      -- where in the type the conflict actually is.
      (tFound, tExpected) <- improveRecordErrorTypes t2 t1
      addContext env exp (CompilationError (UnificationError tm { tmFound = tFound, tmExpected = tExpected, tmSecondaries = [] }) ctx)

    e ->
      addContext env exp e
  )

-- Convenience wrappers for backward compatibility
contextualUnifyAccess :: Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnifyAccess = contextualUnify AccessStyle

contextualUnify' :: Env -> Bool -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnify' env discardError = contextualUnify (if discardError then Discard else Strict) env


-- | State-based unify. Reads the current substitution from state, unifies the
-- substitution-applied versions of t1 and t2, and composes the resulting
-- substitution into state. Returns no substitution — callers read from state
-- via getSubst when they need to reify a final type.
--
-- This is the THiH-style `unify` (see paper §7) and is the preferred entry
-- point for new-style state-based inference. Old code that explicitly threads
-- substitutions still uses the pure `unify` and composes by hand.
unifyM :: Type -> Type -> Infer ()
unifyM t1 t2 = do
  s <- getSubst
  s' <- catchError (unify (apply s t1) (apply s t2)) (\err -> stampContext err >>= throwError)
  extSubst s'

-- | Like contextualUnify but embeds an ErrorOrigin into any UnificationError thrown.
-- Use this at call sites that have semantic context about what is being unified.
contextualUnifyWithOrigin :: UnifyStrategy -> ErrorOrigin -> Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnifyWithOrigin strategy origin env exp t1 t2 =
  contextualUnifyWithOriginAndSecondary strategy origin Nothing env exp t1 t2


-- | Like contextualUnifyWithOrigin but also attaches a secondary source location
-- for multi-span error display (e.g. pointing at the function definition AND the wrong argument).
contextualUnifyWithOriginAndSecondary :: UnifyStrategy -> ErrorOrigin -> Maybe SecondaryLocation -> Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnifyWithOriginAndSecondary strategy origin secondaryLoc env exp t1 t2 = catchError
  (contextualUnify strategy env exp t1 t2)
  (\case
    CompilationError (UnificationError tm) ctx ->
      throwError $ CompilationError (UnificationError tm { tmOrigin = origin, tmSecondaries = maybe [] pure secondaryLoc }) ctx
    e ->
      throwError e
  )


contextualUnifyElems :: Env -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems _ []        = return M.empty
contextualUnifyElems env (h : r) = contextualUnifyElems' env h r

contextualUnifyElems' :: Env -> (Can.Canonical a, Type) -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems' _   _      []              = return M.empty
contextualUnifyElems' env (e, t) ((e', t') : xs) = do
  s1 <- catchError (contextualUnify Strict env e' t' t) flipUnificationError
  s2 <- contextualUnifyElems' (apply s1 env) (e, apply s1 t) xs
  return $ compose s2 s1

flipUnificationError :: CompilationError -> Infer b
flipUnificationError e@(CompilationError err x) = case err of
  UnificationError tm -> throwError $ CompilationError (UnificationError tm { tmFound = tmExpected tm, tmExpected = tmFound tm }) x
  _                   -> throwError e


-- | Like flipUnificationError but also replaces the ErrorOrigin with a specific BranchSide.
-- Used for if-expression branch unification where flipping means the other branch is the culprit.
flipUnificationErrorWithBranch :: BranchSide -> CompilationError -> Infer b
flipUnificationErrorWithBranch side (CompilationError (UnificationError tm) ctx) =
  throwError $ CompilationError (UnificationError tm { tmFound = tmExpected tm, tmExpected = tmFound tm, tmOrigin = FromIfBranches side }) ctx
flipUnificationErrorWithBranch _ e = throwError e


addContext :: Env -> Can.Canonical a -> CompilationError -> Infer b
addContext env (Can.Canonical area _) (CompilationError err _) =
  throwError $ CompilationError err (Context (envCurrentPath env) area)



-- Improve record related errors

improveRecordErrorTypes :: Type -> Type -> Infer (Type, Type)
improveRecordErrorTypes t1 t2 = do
  let s1 = gentleUnify t1 t2
  let s2 = gentleUnify t2 t1
  let t1' = cleanBase $ apply (s1 `compose` s2) t1
  let t2' = cleanBase $ apply (s1 `compose` s2) t2
  return (t1', t2')


cleanBase :: Type -> Type
cleanBase t = case t of
  TApp l r ->
    TApp (cleanBase l) (cleanBase r)

  TRecordRow row optionalFields ->
    TRecordRow (cleanBase row) (cleanBase <$> optionalFields)

  TRowExtend label fieldType tail ->
    TRowExtend label (cleanBase fieldType) (cleanBase tail)

  _ ->
    t

skipBase :: Type -> Type
skipBase t = case t of
  TRecordRow row optionalFields ->
    closedRecord (fst (visibleRow row) <> optionalFields)

  TApp l r ->
    TApp (skipBase l) (skipBase r)

  _ ->
    t


gentleUnify :: Type -> Type -> Substitution
gentleUnify (l `TApp` r) (l' `TApp` r') =
  let s1 = gentleUnify l l'
      s2 = gentleUnify (apply s1 r) (apply s1 r')
  in  compose s2 s1

-- Monomorphization uses this total, best-effort matcher after inference has
-- already proved the program.  Row variables must nevertheless remain
-- kind-correct: flattening a TRecordRow through a record view bound a
-- Row variable to a whole Star-kinded record and made LLVM use the field
-- layout of only the visible projection.  Match rows directly and bind an
-- open tail to the residual concrete row.
gentleUnify (TRecordRow row optionalFields) (TRecordRow row' optionalFields') =
  gentleUnifyRows (rowFromFields optionalFields row)
                  (rowFromFields optionalFields' row')

gentleUnify (TVar tv) t         = M.singleton tv t
gentleUnify t         (TVar tv) = M.singleton tv t
gentleUnify (TCon a fpa _) (TCon b fpb _)
  | a == b && fpa == fpb = M.empty
  | a == b && (fpa == "JSX" || fpb == "JSX") = M.empty
  | a /= b               = M.empty
  | fpa /= fpb           = M.empty

gentleUnify _ _ = M.empty


gentleUnifyRows :: Type -> Type -> Substitution
gentleUnifyRows (TVar tv) row
  | kind tv == Row = M.singleton tv row
gentleUnifyRows row (TVar tv)
  | kind tv == Row = M.singleton tv row
gentleUnifyRows TRowEmpty TRowEmpty = M.empty
gentleUnifyRows (TRowExtend label fieldType tail) row =
  case rewriteKnownRow label row of
    Just (otherFieldType, residual) ->
      let fieldSubst = gentleUnify fieldType otherFieldType
          tailSubst = gentleUnifyRows
            (maskRowLabel label (apply fieldSubst tail))
            (apply fieldSubst residual)
      in  tailSubst `compose` fieldSubst
    Nothing -> M.empty
gentleUnifyRows _ _ = M.empty


-- Pure counterpart of rewriteRow for the already-known rows seen during
-- monomorphization.  Inference has eliminated any need to invent fresh row
-- variables here; encountering an unknown tail simply means no useful
-- best-effort substitution can be learned from this orientation.
rewriteKnownRow :: Id -> Type -> Maybe (Type, Type)
rewriteKnownRow label = go
  where
    go (TRowExtend name fieldType tail)
      | name == label = Just (fieldType, maskRowLabel label tail)
      | otherwise = do
          (foundType, residual) <- go tail
          return (foundType, TRowExtend name fieldType residual)
    go _ = Nothing


-- Should that be called roughMatch?
quickMatch :: Type -> Type -> Bool
quickMatch (l `TApp` r) (l' `TApp` r') =
  quickMatch l l' && quickMatch r r'

quickMatch (TRecordRow row optionalFields) (TRecordRow row' optionalFields') =
  quickMatchRows (rowFromFields optionalFields row)
                 (rowFromFields optionalFields' row')

quickMatch (TVar _) _ = True
quickMatch _ (TVar _) = True
quickMatch (TCon a fpa _) (TCon b fpb _)
  | a == b && fpa == fpb = True
  | a == b && (fpa == "JSX" || fpb == "JSX") = True
  | a /= b = False
  | fpa /= fpb = False

quickMatch (TCon (TC tNameA _) _ _) (TApp (TCon (TC tNameB _) _ _) _)
  | tNameA == "String" && tNameB == "Element" =
      True

quickMatch (TApp (TCon (TC tNameB _) _ _) _) (TCon (TC tNameA _) _ _)
  | tNameB == "Element" && tNameA == "String" =
      True

quickMatch _ _ =
  False


quickMatchRows :: Type -> Type -> Bool
quickMatchRows left right =
  let (leftFields, leftTail) = visibleRow left
      (rightFields, rightTail) = visibleRow right
      sharedFieldsMatch =
        and $ M.elems $ M.intersectionWith quickMatch leftFields rightFields
      onlyOnLeft = M.difference leftFields rightFields
      onlyOnRight = M.difference rightFields leftFields
  in  sharedFieldsMatch
      && (M.null onlyOnLeft || maybe False ((== Row) . kind) rightTail)
      && (M.null onlyOnRight || maybe False ((== Row) . kind) leftTail)
