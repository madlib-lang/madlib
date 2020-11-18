{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Unify where

import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           Infer.Type
import           Infer.Substitute
import           Error.Error


occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = S.member a $ ftv t


bind :: TVar -> Type -> Either TypeError Substitution
bind a t | t == TVar a     = return M.empty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ M.singleton a t


unify :: Type -> Type -> Either TypeError Substitution
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TComp astPath main vars) (TComp astPath' main' vars')
  | main == main' && astPath == astPath' && length vars == length vars'
  = let z = zip vars vars' in unifyVars M.empty z
  | otherwise
  = throwError
    $ UnificationError (TComp astPath main vars) (TComp astPath' main' vars')

unify (TRecord fields open) (TRecord fields' open')
  | open || open' = do
    let extraFields    = M.difference fields fields'
        extraFields'   = M.difference fields' fields
        updatedFields  = M.union fields extraFields'
        updatedFields' = M.union fields' extraFields
        types          = M.elems updatedFields
        types'         = M.elems updatedFields'
        z              = zip types types'
    unifyVars M.empty z
  | M.difference fields fields' /= M.empty = throwError
  $ UnificationError (TRecord fields open) (TRecord fields' open')
  | otherwise = do
    let types  = M.elems fields
        types' = M.elems fields'
        z      = zip types types'
    unifyVars M.empty z

unify (TVar a) t                 = bind a t
unify t        (TVar a)          = bind a t
unify (TCon a) (TCon b) | a == b = return M.empty
unify t1 t2                      = throwError $ UnificationError t1 t2


unifyVars :: Substitution -> [(Type, Type)] -> Either TypeError Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify tp tp'
  unifyVars (s1 `compose` s) xs
unifyVars s [(tp, tp')] = (`compose` s) <$> unify tp tp'
unifyVars s _           = return s


unifyElems :: Type -> [Type] -> Either TypeError Substitution
unifyElems _ []        = return M.empty
unifyElems t [t'     ] = unify t t'
unifyElems t (t' : xs) = do
  s1 <- unify t t'
  s2 <- unifyElems t xs
  return $ s1 `compose` s2
