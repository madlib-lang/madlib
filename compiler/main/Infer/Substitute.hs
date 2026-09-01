{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Infer.Substitute where

import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Error.Error
import           Error.Context
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State            ( modify )
import           Control.Monad.Except
import           Control.Applicative


class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> S.Set TVar

{-# SPECIALIZE apply :: Substitution -> Type -> Type #-}
{-# SPECIALIZE apply :: Substitution -> Scheme -> Scheme #-}
{-# SPECIALIZE apply :: Substitution -> Pred -> Pred #-}
{-# SPECIALIZE apply :: Substitution -> (Qual Type) -> (Qual Type) #-}
{-# SPECIALIZE apply :: Substitution -> [Type] -> [Type] #-}
{-# SPECIALIZE apply :: Substitution -> [Pred] -> [Pred] #-}


instance Substitutable Pred where
  apply s p@(IsIn i ts maybeArea)
    | M.null s  = p
    | otherwise = IsIn i (apply s ts) maybeArea
  ftv (IsIn _ ts _) = ftv ts

instance Substitutable t => Substitutable (Qual t) where
  apply s qt@(ps :=> t)
    | M.null s  = qt
    | otherwise = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `S.union` ftv t

instance Substitutable Type where
  {-# INLINABLE apply #-}
  apply _ tc@(TCon _ _ _) =
    tc

  apply s t@(TVar a) =
    case M.lookup a s of
      Nothing -> t
      -- Follow substitution chains so every caller observes a zonked type.
      -- Removing the variable currently expanded prevents a malformed cycle
      -- from looping forever while proper occurs checks keep such cycles from
      -- being constructed in the first place.
      Just t' -> apply (M.delete a s) t'

  apply s (t1 `TApp` t2) =
    let !t1' = apply s t1
        !t2' = apply s t2
    in  TApp t1' t2'

  apply _ TRowEmpty = TRowEmpty

  apply s (TRowExtend name fieldType tail) =
    TRowExtend name (apply s fieldType) (apply s tail)

  -- Rows are substituted structurally.  Do not flatten through the legacy
  -- record view: doing so would erase an outer label that shadows the same
  -- label in its tail.
  apply s (TRecordRow row optionalFields) =
    TRecordRow (apply s row) (apply s <$> optionalFields)

  apply _ t = t

  ftv TCon{} =
    S.empty

  ftv (TVar a) =
    S.singleton a

  ftv (t1 `TApp` t2) =
    ftv t1 `S.union` ftv t2

  ftv TRowEmpty = S.empty

  ftv (TRowExtend _ fieldType tail) =
    ftv fieldType `S.union` ftv tail

  ftv (TRecordRow row optionalFields) =
    ftv row `S.union` foldMap ftv (M.elems optionalFields)

  ftv _ =
    S.empty


instance Substitutable Scheme where
  {-# INLINABLE apply #-}
  apply s sc@(Forall ks t)
    | M.null s  = sc
    | S.null (ftv t `S.intersection` M.keysSet s) = sc
    | otherwise = Forall ks $ apply s t
  ftv (Forall _ t) = ftv t

instance Substitutable a => Substitutable [a] where
  {-# INLINABLE apply #-}
  apply s xs | M.null s  = xs
             | otherwise = fmap (apply s) xs
  ftv   = foldMap ftv

instance Substitutable Env where
  apply s env
    | M.null s = env
    -- Fast path: the Env's cached union of free TVars in `envVars` is
    -- maintained by extendVars / mergeVars / mergeEnv. If it doesn't
    -- intersect the substitution domain, no scheme needs rebuilding.
    | S.null (envFreeTVars env `S.intersection` M.keysSet s) = env
    -- Slow path: walk only the open schemes (those tracked in
    -- envOpenVarNames). Closed schemes (TGen-only) can never be affected
    -- by a substitution, so iterating them just to short-circuit is pure
    -- waste. envOpenVarNames is typically <10 entries (function params +
    -- pre-generalization let-bindings) vs envVars's hundreds of imports
    -- + post-generalization names.
    | otherwise =
        -- A substitution may introduce fresh free variables in a scheme
        -- (for example a ↦ List b).  Keeping the old cache in that case is
        -- an under-approximation, so a later substitution for b can be
        -- incorrectly skipped.  Rebuild both caches from the resulting map.
        let openMap  = M.restrictKeys (envVars env) (envOpenVarNames env)
            applied  = M.map (apply s) openMap
            vars'    = M.union applied (envVars env)
            openVars = M.filter (not . S.null . ftv) vars'
        in  env { envVars = vars'
                , envFreeTVars = foldMap ftv openVars
                , envOpenVarNames = M.keysSet openVars
                }
  ftv env = ftv $ M.elems $ envVars env


-- protect against infinite types
-- Direct recursive check avoids allocating an intermediate Set
{-# INLINE occursCheck #-}
occursCheck :: TVar -> Type -> Bool
occursCheck tv = go
  where
    go (TVar a)          = tv == a
    go TCon{}            = False
    go TGen{}            = False
    go (TApp l r)        = go l || go r
    go TRowEmpty         = False
    go (TRowExtend _ t r) = go t || go r
    go (TRecordRow row optionalFields) =
      go row || any go optionalFields
    go _                 = False


-- Free type variables in structural traversal order (for TGen index-sensitive quantification)
class FtvOrdered a where
  ftvList :: a -> [TVar]

instance FtvOrdered Type where
  ftvList TCon{}                         = []
  ftvList (TVar a)                       = [a]
  ftvList (t1 `TApp` t2)                = ftvList t1 ++ ftvList t2
  ftvList TRowEmpty                      = []
  ftvList (TRowExtend _ t r)             = ftvList t ++ ftvList r
  ftvList (TRecordRow row optionalFields) =
    ftvList row ++ concatMap ftvList (M.elems optionalFields)
  ftvList _                              = []

instance FtvOrdered Pred where
  ftvList (IsIn _ ts _) = ftvList ts

-- | Deduplicate a list preserving first-occurrence order, in O(n log n).
orderedNub :: Ord a => [a] -> [a]
orderedNub = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
      | S.member x seen = go seen xs
      | otherwise       = x : go (S.insert x seen) xs

instance FtvOrdered a => FtvOrdered [a] where
  ftvList = orderedNub . concatMap ftvList

instance FtvOrdered t => FtvOrdered (Qual t) where
  ftvList (ps :=> t) = orderedNub (ftvList ps ++ ftvList t)


compose :: Substitution -> Substitution -> Substitution
compose !s1 s2
  | M.null s1 = s2
  | otherwise =
      -- `compose new old` satisfies
      --   apply (compose new old) t == apply new (apply old t)
      -- for every type t.  On an overlapping domain the transformed `old`
      -- binding must win: `old` is applied first, so `new`'s binding for the
      -- same input variable is unreachable.  Overlap is never an invitation
      -- to structurally merge two unrelated records or functions.
      let new' = M.map (apply s1) s1
          old' = M.map (apply s1) s2
      in  M.union old' new'

merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2
  | M.null s1 = return s2
  | M.null s2 = return s1
  | S.null shared = return (s1 <> s2)  -- fast path: disjoint domains, no agreement check needed
  | agree     = return (s1 <> s2)
  | otherwise = throwError $ CompilationError FatalError NoContext
 where
  shared = M.keysSet s1 `S.intersection` M.keysSet s2
  agree  = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (S.toList shared)


buildVarSubsts :: Type -> Substitution
buildVarSubsts t = case t of
  TVar (TV n _) ->
    M.singleton (TV n Star) t

  TApp l r ->
    M.union (buildVarSubsts l) (buildVarSubsts r)

  TRowEmpty ->
    mempty

  TRowExtend _ fieldType tail ->
    buildVarSubsts fieldType `compose` buildVarSubsts tail

  TRecordRow row optionalFields ->
    foldr
      (\fieldType s -> buildVarSubsts fieldType `compose` s)
      (buildVarSubsts row)
      (M.elems optionalFields)

  TAlias _ _ _ aliased ->
    buildVarSubsts aliased

  _ ->
    mempty


-- | Compose a substitution into the monad state's `currentSubst`. Existing
-- bindings are transformed by the new substitution, matching sequential
-- application (`new` after `current`).
extSubst :: Substitution -> Infer ()
extSubst s = modify $ \st -> st
  { currentSubst = s `compose` currentSubst st
  }


-- | Apply the current state substitution to a Substitutable value.
applyCurrentSubst :: Substitutable a => a -> Infer a
applyCurrentSubst x = do
  s <- getSubst
  return (apply s x)


-- | Run an action with a transactional view of `currentSubst` and return the
-- delta the action contributed (i.e. bindings that are new or whose value
-- changed during the action). The frame restores `currentSubst` to its
-- pre-action value on exit, so the action's contributions do NOT leak into
-- the caller's state automatically — callers that want them propagated must
-- `extSubst` the returned delta.
--
-- The delta is computed by diffing pre-state and post-state: a binding is in
-- the delta iff it's absent from oldSubst, or present with a different value.
-- This preserves the legacy 4-tuple semantics where `s` was the contribution,
-- not the cumulative state.
captureDelta :: Infer a -> Infer (Substitution, a)
captureDelta action = do
  oldSubst <- getSubst
  r <- action
  newSubst <- getSubst
  putSubst oldSubst
  let delta = M.filterWithKey
        (\k v -> case M.lookup k oldSubst of
                   Nothing -> True
                   Just v' -> v /= v')
        newSubst
  return (delta, r)


-- | Like `captureDelta` but discards the delta. Use when the caller only
-- needs the transactional state restoration (current `currentSubst` at frame
-- exit will be the same as at frame entry), not the contribution itself.
-- Avoids the O(|currentSubst|) filter.
withScopedSubst :: Infer a -> Infer a
withScopedSubst action = do
  oldSubst <- getSubst
  r <- action
  putSubst oldSubst
  return r
