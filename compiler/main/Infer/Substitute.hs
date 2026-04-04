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
import           Control.Monad.Except
import           Control.Applicative


class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> S.Set TVar

{-# SPECIALIZE apply :: Substitution -> Type -> Type #-}
{-# SPECIALIZE apply :: Substitution -> Scheme -> Scheme #-}
{-# SPECIALIZE apply :: Substitution -> [Type] -> [Type] #-}
{-# SPECIALIZE apply :: Substitution -> [Pred] -> [Pred] #-}


instance Substitutable Pred where
  apply s (IsIn i ts maybeArea) = IsIn i (apply s ts) maybeArea
  ftv (IsIn _ ts _) = ftv ts

instance Substitutable t => Substitutable (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `S.union` ftv t

instance Substitutable Type where
  {-# INLINABLE apply #-}
  apply _ tc@(TCon _ _ _) =
    tc

  apply s t@(TVar a) =
    case M.lookup a s of
      Nothing -> t
      Just t' -> if occursCheck a t' then t else t'

  apply s (t1 `TApp` t2) =
    apply s t1 `TApp` apply s t2

  apply s (TRecord fields (Just (TVar tv)) optionalFields) = case M.lookup tv s of
    Just newBase@(TVar _) ->
      -- Row variable substituted with another row variable - preserve it
      TRecord (apply s <$> fields) (Just newBase) (apply s <$> optionalFields)

    Just (TRecord fields' Nothing optionalFields') ->
      -- Row variable substituted with a closed record - merge fields and remove row variable
      TRecord (apply s <$> (fields <> fields')) Nothing (apply s <$> (optionalFields <> optionalFields'))

    Just (TRecord fields' base' optionalFields') ->
      -- Row variable substituted with an open record - merge fields and preserve the base
      let appliedBase = apply s <$> base'
      in  if appliedBase /= base' then
            -- Base changed after substitution, recurse to handle nested substitutions
            apply s $ TRecord (fields <> fields') appliedBase (optionalFields <> optionalFields')
          else
            -- Base unchanged, just merge fields
            TRecord (apply s <$> (fields <> fields')) appliedBase (apply s <$> (optionalFields <> optionalFields'))

    Nothing ->
      -- Row variable not in substitution - keep it as is
      TRecord (apply s <$> fields) (Just (TVar tv)) (apply s <$> optionalFields)

    Just (TGen x) ->
      -- Row variable substituted with a generic type variable - preserve it
      TRecord (apply s <$> fields) (Just $ TGen x) (apply s <$> optionalFields)

    Just otherType ->
      -- Row variable substituted with a non-record type - try to apply substitution recursively
      -- This handles cases where the substitution might resolve to a record after further application
      let appliedOther = apply s otherType
      in  case appliedOther of
            TRecord fields' base' optionalFields' ->
              -- After substitution, it became a record - merge fields
              TRecord (apply s <$> (fields <> fields')) base' (apply s <$> (optionalFields <> optionalFields'))
            _ ->
              -- Still not a record - keep original row variable but apply substitution to fields
              TRecord (apply s <$> fields) (Just (TVar tv)) (apply s <$> optionalFields)

  apply s (TRecord fields (Just (TRecord fields' base optionalFields')) optionalFields) =
    -- Base is already a record - merge and recurse
    apply s $ TRecord (fields <> fields') base (optionalFields <> optionalFields')

  apply s (TRecord fields Nothing optionalFields)
    | M.null optionalFields =
      -- No row variable, no optional fields - just apply substitution to main fields
      TRecord (apply s <$> fields) Nothing mempty
    | otherwise =
      -- No row variable - merge optional fields into main fields and apply substitution
      TRecord (apply s <$> (fields <> optionalFields)) Nothing mempty

  apply _ t = t

  ftv TCon{} =
    S.empty

  ftv (TVar a) =
    S.singleton a

  ftv (t1 `TApp` t2) =
    ftv t1 `S.union` ftv t2

  ftv (TRecord fields Nothing optionalFields) =
    foldMap ftv (M.elems fields) `S.union` foldMap ftv (M.elems optionalFields)

  ftv (TRecord fields (Just base) optionalFields) =
    foldMap ftv (M.elems fields)
    `S.union` ftv base
    `S.union` foldMap ftv (M.elems optionalFields)

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
  apply s env | M.null s  = env
              | otherwise =
    let ks = M.keysSet s
        applyScheme sc@(Forall _ t)
          | S.null (ftv t `S.intersection` ks) = sc
          | otherwise = apply s sc
    in  env { envVars = M.map applyScheme $ envVars env }
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
    go (TRecord fields base optionalFields) =
      any go (M.elems fields) || maybe False go base || any go (M.elems optionalFields)
    go _                 = False


-- Free type variables in structural traversal order (for TGen index-sensitive quantification)
class FtvOrdered a where
  ftvList :: a -> [TVar]

instance FtvOrdered Type where
  ftvList TCon{}                         = []
  ftvList (TVar a)                       = [a]
  ftvList (t1 `TApp` t2)                = ftvList t1 ++ ftvList t2
  ftvList (TRecord fields Nothing optionalFields) =
    concatMap ftvList (M.elems fields)
    ++ concatMap ftvList (M.elems optionalFields)
  ftvList (TRecord fields (Just base) optionalFields) =
    concatMap ftvList (M.elems fields)
    ++ ftvList base
    ++ concatMap ftvList (M.elems optionalFields)
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
  | M.null s2 = M.map (apply s1) s1
  | otherwise = M.map (apply s1) $ M.unionWith mergeTypes s2 (M.map (apply s1) s1)
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1 base1 optionalFields1, TRecord fields2 base2 optionalFields2) ->
      let base = base1 <|> base2
      in  TRecord (M.unionWith mergeTypes fields1 fields2) base (optionalFields1 <> optionalFields2)

    (TRecord fields base optionalFields, TVar _) ->
      TRecord fields base optionalFields

    (TVar _, TRecord fields base optionalFields) ->
      TRecord fields base optionalFields

    (TApp tl tr, TApp tl' tr') ->
      let tl'' = mergeTypes tl tl'
          tr'' = mergeTypes tr tr'
      in  TApp tl'' tr''

    (_, t) ->
      t

merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2 = if agree then return (s1 <> s2) else throwError $ CompilationError FatalError NoContext
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (S.toList (M.keysSet s1 `S.intersection` M.keysSet s2))


buildVarSubsts :: Type -> Substitution
buildVarSubsts t = case t of
  TVar (TV n _) ->
    M.singleton (TV n Star) t

  TApp l r ->
    M.union (buildVarSubsts l) (buildVarSubsts r)

  TRecord fields base optionalFields ->
    foldr (\t s -> buildVarSubsts t `compose` s) nullSubst (M.elems fields <> baseToList base <> M.elems optionalFields)

  _ ->
    mempty
