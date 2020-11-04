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

unify (TComp main vars) (TComp main' vars')
  | main == main' = let z = zip vars vars' in unifyVars M.empty z
  | otherwise = throwError
  $ UnificationError (TComp main vars) (TComp main' vars')

unify (TRecord fields) (TRecord fields')
  | M.difference fields fields' /= M.empty = throwError
  $ UnificationError (TRecord fields) (TRecord fields')
  | otherwise = do
    let types  = M.elems fields
        types' = M.elems fields'
        z      = zip types types'
    unifyVars M.empty z

unify (TVar a) t                 = bind a t
unify t        (TVar a)          = bind a t
unify (TCon a) (TCon b) | a == b = return M.empty
unify TAny _                     = return M.empty
unify _    TAny                  = return M.empty
unify t1   t2                    = throwError $ UnificationError t1 t2


unifyVars :: Substitution -> [(Type, Type)] -> Either TypeError Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify (apply s tp) (apply s tp')
  unifyVars s1 xs
unifyVars s [(tp, tp')] = unify (apply s tp) (apply s tp')
unifyVars s _           = return s


unifyElems :: Type -> [Type] -> Either TypeError Substitution
unifyElems _ []        = return M.empty
unifyElems t [t'     ] = unify t t'
unifyElems t (t' : xs) = do
  s1 <- unify t t'
  s2 <- unifyElems t xs
  return $ s1 `compose` s2
