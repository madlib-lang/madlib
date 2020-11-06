module Infer.Substitute where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Foldable                  ( Foldable(foldl') )
import           Infer.Type


class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> S.Set TVar

instance Substitutable Type where
  apply _ (  TCon a           ) = TCon a
  apply s t@(TVar a           ) = M.findWithDefault t a s
  apply s (  t1    `TArr` t2  ) = apply s t1 `TArr` apply s t2
  apply s (  TComp main   vars) = TComp main (apply s <$> vars)
  apply s (  TRecord fields   ) = TRecord (apply s <$> fields)

  ftv TCon{}              = S.empty
  ftv (TVar a           ) = S.singleton a
  ftv (t1    `TArr` t2  ) = ftv t1 `S.union` ftv t2
  ftv (TComp _      vars) = foldl' (\s v -> S.union s $ ftv v) S.empty vars
  ftv (TRecord fields   ) = foldl' (\s v -> S.union s $ ftv v) S.empty fields

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr M.delete s as
  ftv (Forall as t) = S.difference (ftv t) (S.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable Env where
  apply s env = env { envvars = M.map (apply s) $ envvars env }
  ftv env = ftv $ M.elems $ envvars env

compose :: Substitution -> Substitution -> Substitution
s1 `compose` s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, s1]
 where
  mergeTypes :: Type -> Type -> Type
  mergeTypes t1 t2 = case (t1, t2) of
    (TRecord fields1, TRecord fields2) -> TRecord $ M.union fields1 fields2
    (t              , _              ) -> t
