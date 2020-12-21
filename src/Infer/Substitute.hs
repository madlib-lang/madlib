{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Infer.Substitute where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Foldable                  ( Foldable(foldl') )
import           Infer.Type
import           Debug.Trace                    ( trace )
import           Text.Show.Pretty               ( ppShow )
import           Infer.Infer
import           Control.Monad.Except
import           Error.Error
import           Explain.Reason
import           Data.List                      ( nub
                                                , union
                                                , intersect
                                                )


class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> [TVar]


instance Substitutable Pred where
  apply s (IsIn i ts) = IsIn i (apply s ts)
  ftv (IsIn i ts) = ftv ts

instance Substitutable t => Substitutable (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `union` ftv t

instance Substitutable Type where
  apply _ (  TCon a      ) = TCon a
  apply s t@(TVar a      ) = M.findWithDefault t a s
  apply s (  t1 `TApp` t2) = apply s t1 `TApp` apply s t2
  apply s rec@(TRecord fields open) =
    let applied = TRecord (apply s <$> fields) open
    in  if rec == applied then applied else apply s applied
  apply s t = t

  ftv TCon{}              = []
  ftv (TVar a           ) = [a]
  ftv (t1      `TApp` t2) = ftv t1 `union` ftv t2
  ftv (TRecord fields _ ) = foldl' (\s v -> union s $ ftv v) [] fields
  ftv t                   = []

instance Substitutable Scheme where
  apply s (Forall ks t) = Forall ks $ apply s t
  ftv (Forall _ t) = ftv t

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = nub . concatMap ftv

instance Substitutable Env where
  apply s env = env { envvars = M.map (apply s) $ envvars env }
  ftv env = ftv $ M.elems $ envvars env

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, s1]
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1 open1, TRecord fields2 open2) ->
      TRecord (M.union fields1 fields2) (open1 || open2)
    (t, _) -> t


merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2 = if agree
  then return (s1 <> s2)
  else throwError $ InferError FatalError NoReason
 where
  agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
              (M.keys s1 `intersect` M.keys s2)

buildVarSubsts :: Type -> Substitution
buildVarSubsts t = case t of
  TVar (TV n k) -> M.singleton (TV n Star) t
  TCon _        -> mempty
  TApp l r      -> M.union (buildVarSubsts l) (buildVarSubsts r)
  TRecord ts _  -> foldl (\s t -> buildVarSubsts t `compose` s) nullSubst ts
