{-# LANGUAGE LambdaCase #-}
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
import           Text.Show.Pretty               ( ppShow )
import           Debug.Trace


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
  apply _ tc@(TCon a fp   ) = tc
  apply s t@( TVar a      ) = M.findWithDefault t a s
  apply s (   t1 `TApp` t2) = apply s t1 `TApp` apply s t2
  apply s rec@(TRecord fields base open) =
    let appliedFields                        = apply s <$> fields
        appliedBase                          = apply s <$> base
        (allFields', baseResolved, nextBase) = case appliedBase of
          Just (TRecord fields' base' _) -> (fields' <> appliedFields, True, base')
          _                              -> (appliedFields, False, appliedBase)

        applied = if baseResolved then TRecord allFields' nextBase open else TRecord allFields' appliedBase open
    in  if rec == applied then applied else apply s applied
  apply s t = t

  ftv TCon{}               = []
  ftv (TVar a            ) = [a]
  ftv (t1 `TApp` t2      ) = ftv t1 `union` ftv t2
  ftv (TRecord fields _ _) = foldl' (\s v -> union s $ ftv v) [] (M.elems fields)
  -- ftv (TRecord fields spreads _ ) = foldl' (\s v -> union s $ ftv v) [] (M.elems fields ++ S.toList spreads)
  ftv t                    = []

instance Substitutable Scheme where
  apply s (Forall ks t) = Forall ks $ apply s t
  ftv (Forall _ t) = ftv t

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = nub . concatMap ftv

instance Substitutable Env where
  apply s env = env { envVars = M.map (apply s) $ envVars env }
  ftv env = ftv $ M.elems $ envVars env

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, s1]
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1 base1 open1, TRecord fields2 base2 open2) ->
      let base = case (base1, base2) of
            (Just tBase1, _          ) -> Just tBase1
            (_          , Just tBase2) -> Just tBase2
            _                          -> Nothing
      in  TRecord (M.unionWith mergeTypes fields1 fields2) base (open1 || open2)

    (t, _) -> t

merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2 = if agree then return (s1 <> s2) else throwError $ CompilationError FatalError NoContext
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (M.keys s1 `intersect` M.keys s2)

buildVarSubsts :: Type -> Substitution
buildVarSubsts t = case t of
  TVar (TV n k)     -> M.singleton (TV n Star) t
  TCon _ _          -> mempty
  TApp l r          -> M.union (buildVarSubsts l) (buildVarSubsts r)
  TRecord ts base _ -> foldl (\s t -> buildVarSubsts t `compose` s) nullSubst (M.elems ts <> baseToList base)

removeRecordTypes :: Substitution -> Substitution
removeRecordTypes = M.filter notRecord
 where
  notRecord :: Type -> Bool
  notRecord t = case t of
    TRecord _ _ _ -> False
    _             -> True
