{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Unify where


import           Infer.Type
import           Infer.Substitute
import           Error.Error
import           Infer.Infer
import           Infer.Env
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Debug.Trace
import           Text.Show.Pretty
import qualified AST.Canonical                 as Can



varBind :: TVar -> Type -> Infer Substitution
varBind tv t | t == TVar tv      = return M.empty
             | tv `elem` ftv t   = throwError $ InferError (InfiniteType tv t) NoContext
             | kind tv /= kind t = throwError $ InferError (KindError (TVar tv, kind tv) (t, kind t)) NoContext
             | otherwise         = return $ M.singleton tv t

class Unify t where
  unify :: t -> t -> Infer Substitution

instance Unify Type where
  unify (l `TApp` r) (l' `TApp` r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return $ compose s1 s2

  unify l@(TRecord fields open) r@(TRecord fields' open')
    | open || open' = do
      let extraFields  = M.difference fields fields'
          extraFields' = M.difference fields' fields
      if not open && extraFields' /= mempty || not open' && extraFields /= mempty
        then throwError $ InferError (UnificationError r l) NoContext
        else do
          let updatedFields  = M.union fields extraFields'
              updatedFields' = M.union fields' extraFields
              types          = M.elems updatedFields
              types'         = M.elems updatedFields'
              z              = zip types types'
          unifyVars M.empty z
    | M.difference fields fields' /= M.empty || M.difference fields' fields /= M.empty = throwError
    $ InferError (UnificationError r l) NoContext
    | otherwise = do
      let types  = M.elems fields
          types' = M.elems fields'
          z      = zip types types'
      unifyVars M.empty z

  unify (TVar tv) t                = varBind tv t
  unify t         (TVar tv)        = varBind tv t
  unify (TCon a) (TCon b) | a == b = return M.empty
  unify t1 t2                      = throwError $ InferError (UnificationError t2 t1) NoContext


instance (Unify t, Show t, Substitutable t) => Unify [t] where
  unify (x : xs) (y : ys) = do
    s1 <- unify x y
    s2 <- unify (apply s1 xs) (apply s1 ys)
    return (s2 <> s1)
  unify [] [] = return nullSubst
  unify a  b  = throwError $ InferError Error NoContext


unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify tp tp'
  unifyVars (compose s1 s) xs
unifyVars s _ = return s


unifyElems :: Env -> [Type] -> Infer Substitution
unifyElems env []      = return M.empty
unifyElems env (h : r) = unifyElems' h r

unifyElems' :: Type -> [Type] -> Infer Substitution
unifyElems' _ []        = return M.empty
unifyElems' t (t' : xs) = do
  s1 <- unify t' t
  s2 <- unifyElems' t xs
  return $ compose s1 s2



class Match t where
  match :: t -> t -> Infer Substitution

instance Match Type where
  match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
  match (TVar u) t | kind u == kind t      = return $ M.singleton u t
  match (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
  match t1 t2                              = throwError $ InferError (UnificationError t1 t2) NoContext

instance Match t => Match [t] where
  match ts ts' = do
    ss <- zipWithM match ts ts'
    foldM merge nullSubst ss


contextualUnify :: Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnify env exp t1 t2 = catchError (unify t1 t2) (addContext env exp)


contextualUnifyElems :: Env -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems env []      = return M.empty
contextualUnifyElems env (h : r) = contextualUnifyElems' env h r

contextualUnifyElems' :: Env -> (Can.Canonical a, Type) -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems' _   _      []              = return M.empty
contextualUnifyElems' env (e, t) ((e', t') : xs) = do
  s1 <- contextualUnify env e' t t'
  s2 <- contextualUnifyElems' env (e, t) xs
  return $ compose s1 s2


addContext :: Env -> Can.Canonical a -> InferError -> Infer b
addContext env can@(Can.Canonical area e) (InferError err _) =
  throwError $ InferError err (Context (envCurrentPath env) area (envBacktrace env))
