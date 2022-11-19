{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Infer.Substitute where

import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Error.Error
import           Error.Context
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Foldable                  ( Foldable(foldl') )
import           Control.Monad.Except
import           Data.List                      ( nub
                                                , union
                                                , intersect
                                                )

import Debug.Trace
import Text.Show.Pretty
import Control.Applicative


class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> [TVar]


instance Substitutable Pred where
  apply s (IsIn i ts maybeArea) = IsIn i (apply s ts) maybeArea
  ftv (IsIn _ ts _) = ftv ts

instance Substitutable t => Substitutable (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `union` ftv t

instance Substitutable Type where
  apply _ tc@(TCon _ _) =
    tc

  apply s t@(TVar a) =
    let t' = M.findWithDefault t a s
    in  if occursCheck a t' then
          t
        else
          t'

  apply s (t1 `TApp` t2) =
    apply s t1 `TApp` apply s t2

  apply s (TRecord fields (Just (TVar tv))) = case M.lookup tv s of
    Just newBase@(TVar _) ->
      TRecord (apply s <$> fields) (Just newBase)

    Just (TRecord fields' Nothing) ->
      TRecord (apply s <$> (fields <> fields')) Nothing

    Just (TRecord fields' base') ->
      let appliedBase = apply s <$> base'
      in  if appliedBase /= base' then
            apply s $ TRecord (fields <> fields') appliedBase
          else
            TRecord (apply s <$> (fields <> fields')) appliedBase

    Nothing ->
      TRecord (apply s <$> fields) (Just (TVar tv))

    Just (TGen x) ->
      TRecord (apply s <$> fields) (Just $ TGen x)

    bad ->
      error $ "found: " <> ppShow bad

  apply s (TRecord fields (Just (TRecord fields' base))) =
    apply s $ TRecord (fields <> fields') base

  apply s (TRecord fields Nothing) =
    TRecord (apply s <$> fields) Nothing

  apply _ t = t

  ftv TCon{}                       = []
  ftv (TVar a                    ) = [a]
  ftv (t1      `TApp` t2         ) = ftv t1 `union` ftv t2
  ftv (TRecord fields Nothing    ) = foldr (\v s -> union s $ ftv v) [] (M.elems fields)
  ftv (TRecord fields (Just base)) = foldr (\v s -> union s $ ftv v) [] (M.elems fields) `union` ftv base
  ftv _                            = []


instance Substitutable Scheme where
  apply s (Forall ks t) = Forall ks $ apply s t
  ftv (Forall _ t) = ftv t

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = nub . concatMap ftv

instance Substitutable Env where
  apply s env = env { envVars = M.map (apply s) $ envVars env }
  ftv env = ftv $ M.elems $ envVars env


-- protect against infinite types
occursCheck :: TVar -> Type -> Bool
occursCheck tv t =
  tv `elem` ftv t


compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, apply s1 <$> s1]
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1 base1, TRecord fields2 base2) ->
      let base = base1 <|> base2
      in  TRecord (M.unionWith mergeTypes fields1 fields2) base

    (TRecord fields _, TVar _) ->
      TRecord fields (Just t2)

    (TVar _, TRecord fields _) ->
      TRecord fields (Just t1)

    (TApp tl tr, TApp tl' tr') ->
      let tl'' = mergeTypes tl tl'
          tr'' = mergeTypes tr tr'
      in  TApp tl'' tr''

    (_, t) ->
      t

merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2 = if agree then return (s1 <> s2) else throwError $ CompilationError FatalError NoContext
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (M.keys s1 `intersect` M.keys s2)


buildVarSubsts :: Type -> Substitution
buildVarSubsts t = case t of
  TVar (TV n _)   -> M.singleton (TV n Star) t
  TApp    l  r    -> M.union (buildVarSubsts l) (buildVarSubsts r)
  TRecord ts base -> foldr (\t s -> buildVarSubsts t `compose` s) nullSubst (M.elems ts <> baseToList base)
  _               -> mempty
