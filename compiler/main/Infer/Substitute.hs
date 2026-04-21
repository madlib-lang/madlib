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
      Just t' -> if occursCheck a t' then t else apply s t'

  apply s (t1 `TApp` t2) =
    apply s t1 `TApp` apply s t2

  apply s (TRecord fields base optionalFields) =
    normalizeRecord
      (apply s <$> fields)
      (apply s <$> base)
      (apply s <$> optionalFields)

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
  | M.null s2 = s1
  | otherwise = M.map (apply s1) s2 `M.union` s1

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


normalizeRecord :: M.Map Id Type -> Maybe Type -> M.Map Id Type -> Type
normalizeRecord fields maybeBase optionalFields = case maybeBase of
  Nothing ->
    if M.null optionalFields then
      TRecord fields Nothing mempty
    else
      TRecord (fields <> optionalFields) Nothing mempty

  Just base -> case base of
    TVar _ ->
      TRecord fields (Just base) optionalFields

    TGen _ ->
      TRecord fields (Just base) optionalFields

    _ | Just (baseFields, nextBase, baseOptionalFields) <- expandRowBase base ->
      normalizeRecord (fields <> baseFields) nextBase (optionalFields <> baseOptionalFields)

    _ ->
      error $
        "Compiler bug: row variable substituted with a non-record base: " <> show base


expandRowBase :: Type -> Maybe (M.Map Id Type, Maybe Type, M.Map Id Type)
expandRowBase t = case t of
  TRecord fields base optionalFields ->
    Just (fields, base, optionalFields)

  TAlias _ _ _ aliasType ->
    expandRowBase aliasType

  _ ->
    Nothing
