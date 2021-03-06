{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Unify where


import           Infer.Type
import           Infer.Substitute
import           Error.Error
import           Error.Context
import           Infer.Infer
import           Infer.Env
import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Debug.Trace
import           Text.Show.Pretty
import qualified AST.Canonical                 as Can



varBind :: TVar -> Type -> Infer Substitution
varBind tv t@(TRecord fields (Just base)) = return $ M.singleton tv t
varBind tv t | t == TVar tv      = return M.empty
             | tv `elem` ftv t   = throwError $ CompilationError (InfiniteType tv t) NoContext
             | kind tv /= kind t = throwError $ CompilationError (KindError (TVar tv, kind tv) (t, kind t)) NoContext
             | otherwise         = return $ M.singleton tv t

class Unify t where
  unify :: t -> t -> Infer Substitution

instance Unify Type where
  unify (l `TApp` r) (l' `TApp` r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return $ compose s1 s2

  unify l@(TRecord fields base) r@(TRecord fields' base') = case (base, base') of
    (Just tBase, Just tBase') -> do
      s1 <- unify tBase (TRecord fields' base)
      s2 <- unify tBase' (TRecord fields base')

      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
          z              = zip (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      s3 <- unifyVars M.empty z
      s4 <- unify tBase tBase'

      return $ s4 `compose` s3 `compose` s1 `compose` s2

    (Just tBase, Nothing) -> do
      s1 <- unify tBase (TRecord fields Nothing)
      s2 <- unify tBase (TRecord fields' Nothing)

      unless (null (M.difference fields fields')) $ throwError (CompilationError (UnificationError r l) NoContext)

      let fieldsToCheck = M.mapWithKey (\k _ -> fromMaybe undefined $ M.lookup k fields') fields
          z             = zip (M.elems fields) (M.elems fieldsToCheck)
      s3 <- unifyVars M.empty z

      return $ s3 `compose` s1 `compose` s2

    (Nothing, Just tBase') -> do
      s1 <- unify tBase' (TRecord fields Nothing)
      s2 <- unify tBase' (TRecord fields' Nothing)

      unless (null (M.difference fields' fields)) $ throwError (CompilationError (UnificationError r l) NoContext)

      let fieldsToCheck = M.mapWithKey (\k _ -> fromMaybe undefined $ M.lookup k fields) fields'
          z             = zip (M.elems fields') (M.elems fieldsToCheck)
      s3 <- unifyVars M.empty z

      return $ s3 `compose` s1 `compose` s2

    _ -> do
      let extraFields  = M.difference fields fields'
          extraFields' = M.difference fields' fields
      if extraFields' /= mempty || extraFields /= mempty
        then throwError $ CompilationError (UnificationError r l) NoContext
        else do
          let updatedFields  = M.union fields extraFields'
              updatedFields' = M.union fields' extraFields
              types          = M.elems updatedFields
              types'         = M.elems updatedFields'
              z              = zip types types'
          unifyVars M.empty z

  unify (TVar tv) t         = varBind tv t
  unify t         (TVar tv) = varBind tv t
  unify t1@(TCon a fpa) t2@(TCon b fpb)
    | a == b && fpa == fpb = return M.empty
    | a /= b               = throwError $ CompilationError (UnificationError t2 t1) NoContext
    | fpa /= fpb           = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId a) fpa fpb) NoContext
  unify t1 t2 = throwError $ CompilationError (UnificationError t2 t1) NoContext


instance (Unify t, Show t, Substitutable t) => Unify [t] where
  unify (x : xs) (y : ys) = do
    s1 <- unify x y
    s2 <- unify (apply s1 xs) (apply s1 ys)
    return (s2 <> s1)
  unify [] [] = return nullSubst
  unify a  b  = throwError $ CompilationError Error NoContext


unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify tp tp'
  unifyVars (compose s s1) xs
unifyVars s [] = return s


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
  match (TVar u) t | kind u == kind t = return $ M.singleton u t
  match (TCon tc1 fp1) (TCon tc2 fp2)
    | tc1 == tc2 && fp1 == fp2 = return nullSubst
    | fp1 /= fp2 = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId tc1) fp1 fp2) NoContext
  match t1 t2 = throwError $ CompilationError (UnificationError t1 t2) NoContext

instance Match t => Match [t] where
  match ts ts' = do
    ss <- zipWithM match ts ts'
    foldM merge nullSubst ss


contextualUnify :: Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnify env exp t1 t2 = catchError
  (unify t1 t2)
  (\case
    e@(CompilationError TypesHaveDifferentOrigin{} _) -> addContext env exp e
    e -> addContext env exp e
  )


contextualUnifyElems :: Env -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems env []      = return M.empty
contextualUnifyElems env (h : r) = contextualUnifyElems' env h r

contextualUnifyElems' :: Env -> (Can.Canonical a, Type) -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems' _   _      []              = return M.empty
contextualUnifyElems' env (e, t) ((e', t') : xs) = do
  s1 <- catchError (contextualUnify env e' t' t) flipUnificationError
  s2 <- contextualUnifyElems' (apply s1 env) (e, apply s1 t) xs
  return $ compose s1 s2

flipUnificationError :: CompilationError -> Infer b
flipUnificationError e@(CompilationError err x) = case err of
  UnificationError l r -> throwError $ CompilationError (UnificationError r l) x
  _                    -> throwError e


addContext :: Env -> Can.Canonical a -> CompilationError -> Infer b
addContext env can@(Can.Canonical area e) (CompilationError err _) =
  throwError $ CompilationError err (Context (envCurrentPath env) area (envBacktrace env))
