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
  apply _ tc@(TCon _ _   ) = tc
  apply s t@( TVar a      ) = M.findWithDefault t a s
    -- TRecord fields (Just t'@(TVar tv)) ->
    --   let deepUpdateRecord initialFields base tyvar = 
    --         case M.findWithDefault base tyvar s of
    --           TRecord fields' newBase ->
    --             TRecord (M.union initialFields fields') newBase

    --           t'' ->
    --             TRecord fields (Just t'')

    --   in  deepUpdateRecord fields t' tv

    -- TRecord fields (Just base) ->
    --   TRecord fields (Just base)

  apply s (   t1 `TApp` t2) = apply s t1 `TApp` apply s t2
  apply s (TRecord fields (Just (TVar tv))) = case M.lookup tv s of
    Just newTV@(TVar _) ->
      TRecord (apply s <$> fields) (Just newTV)

    Just (TRecord fields' Nothing) ->
      TRecord (apply s <$> (fields <> fields')) Nothing

    Just (TRecord fields' base') ->
      TRecord (apply s <$> (fields <> fields')) base'

    Nothing ->
      TRecord (apply s <$> fields) (Just (TVar tv))

    Just (TGen x) ->
       TRecord (apply s <$> fields) (Just $ TGen x)

    bad ->
      error $ "found: " <> ppShow bad
  apply s (TRecord fields base) =
    let appliedFields          = apply s <$> fields
        appliedBase            = apply s <$> base
        (allFields', nextBase) = case appliedBase of
          Just (TRecord fields' base') ->
            (M.union appliedFields (apply s <$> fields'), base')

          _                            ->
            (appliedFields, appliedBase)

        applied = TRecord allFields' nextBase
    in  applied
    -- in  if rec == applied then applied else apply s applied
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

-- compose :: Substitution -> Substitution -> Substitution
-- compose s1 s2 = M.map (apply s1) $ s2 <> s1

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, apply s1 <$> s1]
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1 base1, TRecord fields2 base2) ->
      let base = case (base1, base2) of
            (Just tBase1, _          ) -> Just tBase1
            (_          , Just tBase2) -> Just tBase2
            _                          -> Nothing
      in  TRecord (M.unionWith mergeTypes fields1 fields2) base

    (TRecord fields _, TVar _) ->
      TRecord fields (Just t2)

    (TVar _, TRecord fields _) ->
      TRecord fields (Just t1)

    (TApp tl tr, TApp tl' tr') ->
      let tl'' = mergeTypes tl tl'
          tr'' = mergeTypes tr tr'
      in  TApp tl'' tr''

    (_, t) -> t

merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2 = if agree then return (s1 <> s2) else throwError $ CompilationError FatalError NoContext
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (M.keys s1 `intersect` M.keys s2)


buildVarSubsts :: Type -> Substitution
buildVarSubsts t = case t of
  TVar (TV n _)   -> M.singleton (TV n Star) t
  TApp    l  r    -> M.union (buildVarSubsts l) (buildVarSubsts r)
  TRecord ts base -> foldr (\t s -> buildVarSubsts t `compose` s) nullSubst (M.elems ts <> baseToList base)
  _               -> mempty
