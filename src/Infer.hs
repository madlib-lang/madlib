{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Infer where

import Grammar
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Except
import Control.Monad.State

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

type Env = M.Map String Scheme

type Substitution = M.Map TVar Type

data InferError = InfiniteType TVar Type
                | UnboundVariable
                | UnificationError Type Type
                deriving (Show, Eq, Ord)

newtype Unique = Unique { count :: Int }
  deriving (Show, Eq, Ord)

type Infer a = forall m. (MonadError InferError m, MonadState Unique m) => m a

class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> S.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = M.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = S.empty
  ftv (TVar a)       = S.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr M.delete s as
  ftv (Forall as t) =  S.difference (ftv t) (S.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable Env where
  apply s env =  M.map (apply s) env
  ftv env = ftv $ M.elems env


extend :: Env -> (String, Scheme) -> Env
extend env (x, s) = M.insert x s env

lookupEnv :: Env -> String -> Infer (Substitution, Type)
lookupEnv env x = do
  case M.lookup x env of
    Nothing -> throwError UnboundVariable --(show x)
    Just s  -> do t <- instantiate s
                  return (M.empty, t)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

newTVar :: Infer Type
newTVar = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const newTVar) as
  let s = M.fromList $ zip as as'
  return $ apply s t

compose :: Substitution -> Substitution -> Substitution
s1 `compose` s2 = M.map (apply s1) $ M.union s2 s1

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = S.member a $ ftv t

bind ::  TVar -> Type -> Infer Substitution
bind a t | t == TVar a     = return M.empty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ M.singleton a t

unify :: Type -> Type -> Infer Substitution
unify (l `TArr` r) (l' `TArr` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return M.empty
unify t1 t2 = throwError $ UnificationError t1 t2

infer :: Env -> Exp -> Infer (Substitution, Type)
infer env Var{ ename } = lookupEnv env ename

infer env Abs { eparam, ebody } = do
    tv <- newTVar
    let env' = env `extend` (eparam, Forall [] tv)
    (s1, t1) <- infer env' ebody
    return (s1, apply s1 tv `TArr` t1)

infer env App { eabs, earg } = do
    tv <- newTVar
    (s1, t1) <- infer env eabs
    (s2, t2) <- infer (apply s1 env) earg
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

infer env LInt {} = return (M.empty, TCon "Num")

infer _ _ = undefined
