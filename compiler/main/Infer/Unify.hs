{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Infer.Unify where


import           Infer.Type
import           Infer.Substitute
import           Infer.Instantiate
import           Error.Error
import           Error.Context
import           Infer.Infer
import           Infer.Env
import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified AST.Canonical                 as Can
import qualified Control.Monad as Monad



varBind :: TVar -> Type -> Infer Substitution
varBind tv t@(TRecord fields (Just base) optionalFields)
  | tv == TV "__NO_ARG__" Star = return M.empty
  | tv `elem` concat (ftv <$> fields) && tv /= TV "__NO_ARG__" Star = throwError $ CompilationError (InfiniteType tv t) NoContext
  | otherwise                         = return $ M.singleton tv (TRecord fields (Just base) optionalFields)
varBind tv t | t == TVar tv      = return M.empty
             | tv == TV "__NO_ARG__" Star = return M.empty
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

  unify l@(TRecord fields base optionalFields) r@(TRecord fields' base' optionalFields') = case (base, base') of
    (Just tBase, Just tBase') -> do
      newBase <- newTVar Star
      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
          fieldsForLeft  = M.difference fields fields'
          fieldsForRight = M.difference fields' fields

      s1 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
      s2 <- unify (TRecord fieldsForLeft (Just newBase) mempty) tBase'
      s3 <- unify (TRecord fieldsForRight (Just newBase) mempty) tBase

      return $ s1 `compose` s2 `compose` s3

    (Just tBase, Nothing) -> do
      let fieldsDiff = M.difference fields' fields
          commonFields = M.intersection fields fields'
      -- newBase <- newTVar Star
      -- s1 <- unify (TRecord mempty (Just tBase)) (TRecord fieldsDiff (Just newBase))
      s1 <- unify tBase (TRecord fieldsDiff Nothing commonFields)

      unless (M.null (M.difference fields fields')) $ throwError (CompilationError (UnificationError r l) NoContext)

      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
      s2 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      return $ s1 `compose` s2

    (Nothing, Just tBase') -> do
      let fieldsDiff = M.difference fields fields'
          commonFields = M.intersection fields' fields
      -- newBase <- newTVar Star
      -- s1 <- unify (TRecord mempty (Just tBase')) (TRecord fieldsDiff (Just newBase))
      s1 <- unify tBase' (TRecord fieldsDiff Nothing commonFields)

      unless (M.null (M.difference fields' fields)) $ throwError (CompilationError (UnificationError r l) NoContext)

      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
      s2 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      return $ s1 `compose` s2

    _ -> do
      let extraFields  = M.difference fields (fields' <> optionalFields')
          extraFields' = M.difference fields' (fields <> optionalFields)
      if extraFields' /= mempty || extraFields /= mempty then
        throwError $ CompilationError (UnificationError r l) NoContext
      else do
        unifyVars' M.empty (M.elems fields) (M.elems fields')

  unify (TVar tv) t         = varBind tv t
  unify t         (TVar tv) = varBind tv t
  unify t1@(TCon a fpa) t2@(TCon b fpb)
    | a == b && fpa == fpb = return M.empty
    | a == b && (fpa == "JSX" || fpb == "JSX") = return M.empty
    | a /= b               = throwError $ CompilationError (UnificationError t2 t1) NoContext
    | fpa /= fpb           = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId a) fpa fpb) NoContext

  unify (TCon (TC tNameA _) _) (TApp (TCon (TC tNameB _) _) _)
    | tNameA == "String" && tNameB == "Element" =
        return mempty

  unify (TApp (TCon (TC tNameB _) _) _) (TCon (TC tNameA _) _)
    | tNameB == "Element" && tNameA == "String" =
        return mempty

  unify t1 t2 =
    throwError $ CompilationError (UnificationError t2 t1) NoContext




instance (Unify t, Show t, Substitutable t) => Unify [t] where
  unify (x : xs) (y : ys) = do
    s1 <- unify x y
    s2 <- unify (apply s1 xs) (apply s1 ys)
    return (s2 <> s1)
  unify [] [] = return nullSubst
  unify _  _  = throwError $ CompilationError Error NoContext


unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify (apply s tp) (apply s tp')
  unifyVars (compose s1 s) xs
unifyVars s [] = return s

unifyVars' :: Substitution -> [Type] -> [Type] -> Infer Substitution
unifyVars' s (tp : xs) (tp' : xs') = do
  s1 <- unify (apply s tp) (apply s tp')
  unifyVars' (compose s1 s) xs xs'
unifyVars' s _ _  = return s


unifyElems :: Env -> [Type] -> Infer Substitution
unifyElems _ []      = return M.empty
unifyElems _ (h : r) = unifyElems' h r

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
    sr <- match (apply sl r) (apply sl r')
    merge sl sr
  match (TVar u) t | kind u == kind t =
    return $ M.singleton u t
  match (TCon tc1 fp1) (TCon tc2 fp2)
    | tc1 == tc2 && fp1 == fp2 = return nullSubst
    | tc1 == tc2 && (fp1 == "JSX" || fp2 == "JSX") = return M.empty
    | fp1 /= fp2 = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId tc1) fp1 fp2) NoContext
  match t1@(TRecord fields1 _ optionalFields1) t2@(TRecord fields2 _ optionalFields2) = do
    let allFields1 = fields1 <> optionalFields1
    let allFields2 = fields2 <> optionalFields2
    Monad.when (M.size allFields1 /= M.size allFields2) $
      throwError $ CompilationError (UnificationError t2 t1) NoContext
    unify (TRecord allFields1 Nothing mempty) (TRecord allFields2 Nothing mempty)
  match t1 t2 = throwError $ CompilationError (UnificationError t2 t1) NoContext

instance Match t => Match [t] where
  match ts ts' = do
    ss <- zipWithM match ts ts'
    foldM merge nullSubst ss


contextualUnifyAccess :: Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnifyAccess env exp t1 t2 = catchError
  (unify t1 t2)
  (\case
    (CompilationError (UnificationError _ _) ctx) -> do
      let t2' = getParamTypeOrSame t2
          t1' = getParamTypeOrSame t1
          hasNotChanged = t2' == t2 || t1' == t1
          t2'' = if hasNotChanged then t2 else t2'
          t1'' = if hasNotChanged then t1 else t1'

      (t2''', t1''')   <- catchError (unify t1'' t2'' >> return (t2, t1)) (\_ -> return (t2'', t1''))
      (t2'''', t1'''') <- improveRecordErrorTypes t2''' t1'''
      addContext env exp (CompilationError (UnificationError t2'''' t1'''') ctx)

    e ->
      addContext env exp e
  )

contextualUnify :: Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
contextualUnify env exp t1 t2 = catchError
  (unify t1 t2)
  (\case
    (CompilationError (UnificationError _ _) ctx) -> do
      let t2' = getParamTypeOrSame t2
          t1' = getParamTypeOrSame t1
          hasNotChanged = t2' == t2 || t1' == t1
          t2'' = if hasNotChanged then t2 else t2'
          t1'' = if hasNotChanged then t1 else t1'
      (t2''', t1''') <- catchError (unify t1'' t2'' >> return (t2, t1)) (\_ -> return (t2'', t1''))
      (t2'''', t1'''') <- improveRecordErrorTypes t2''' t1'''
      addContext env exp (CompilationError (UnificationError t2'''' t1'''') ctx)

    e ->
      addContext env exp e
  )


contextualUnifyElems :: Env -> [(Can.Canonical a, Type)] -> Infer Substitution
contextualUnifyElems _ []        = return M.empty
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
addContext env (Can.Canonical area _) (CompilationError err _) =
  throwError $ CompilationError err (Context (envCurrentPath env) area)



-- Improve record related errors

improveRecordErrorTypes :: Type -> Type -> Infer (Type, Type)
improveRecordErrorTypes t1 t2 = do
  let s1 = gentleUnify t1 t2
  let s2 = gentleUnify t2 t1
  let t1' = cleanBase $ apply (s1 `compose` s2) t1
  let t2' = cleanBase $ apply (s1 `compose` s2) t2
  return (t1', t2')


cleanBase :: Type -> Type
cleanBase t = case t of
  TRecord fields (Just (TRecord extraFields _ _)) _ ->
    TRecord (extraFields <> fields) Nothing mempty

  TApp l r ->
    TApp (cleanBase l) (cleanBase r)

  _ ->
    t

skipBase :: Type -> Type
skipBase t = case t of
  TRecord fields (Just (TRecord extraFields _ _)) _ ->
    TRecord (extraFields <> fields) Nothing mempty

  TRecord fields _ _ ->
    TRecord fields Nothing mempty

  TApp l r ->
    TApp (skipBase l) (skipBase r)

  _ ->
    t


gentleUnifyVars :: Substitution -> [Type] -> [Type] -> Substitution
gentleUnifyVars s (tp : xs) (tp' : xs') =
  let s1 = gentleUnify tp tp'
  in  gentleUnifyVars (compose s s1) xs xs'
gentleUnifyVars s _ _  = s


gentleUnify :: Type -> Type -> Substitution
gentleUnify (l `TApp` r) (l' `TApp` r') =
  let s1 = gentleUnify l l'
      s2 = gentleUnify (apply s1 r) (apply s1 r')
  in  compose s1 s2

gentleUnify (TRecord fields base _) (TRecord fields' base' _) = case (base, base') of
  (Just tBase, Just tBase') ->
    let s1 = gentleUnify tBase' (TRecord (M.union fields fields') base mempty)
        fieldsToCheck  = M.intersection fields fields'
        fieldsToCheck' = M.intersection fields' fields

        s2 = gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
        s3 = gentleUnify tBase tBase'

    in  s3 `compose` s2 `compose` s1

  (Just tBase, Nothing) ->
    let s1 = gentleUnify tBase (TRecord fields' base mempty)
    in  if not $ M.null (M.difference fields fields') then
          M.empty
        else
          let fieldsToCheck  = M.intersection fields fields'
              fieldsToCheck' = M.intersection fields' fields
              s2 = gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
          in  s2 `compose` s1

  (Nothing, Just tBase') ->
    let s1 = gentleUnify tBase' (TRecord fields base' mempty)
    in  if not $ M.null (M.difference fields' fields) then
          M.empty
        else
          let fieldsToCheck  = M.intersection fields fields'
              fieldsToCheck' = M.intersection fields' fields
              s2 = gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
          in  s2 `compose` s1

  _ ->
    let fieldsToCheck  = M.intersection fields fields'
        fieldsToCheck' = M.intersection fields' fields
    in  gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

gentleUnify (TVar tv) t         = M.singleton tv t
gentleUnify t         (TVar tv) = M.singleton tv t
gentleUnify (TCon a fpa) (TCon b fpb)
  | a == b && fpa == fpb = M.empty
  | a == b && (fpa == "JSX" || fpb == "JSX") = M.empty
  | a /= b               = M.empty
  | fpa /= fpb           = M.empty

gentleUnify _ _ = M.empty


-- Should that be called roughMatch?
quickMatch :: Type -> Type -> Bool
quickMatch (l `TApp` r) (l' `TApp` r') =
  quickMatch l l' && quickMatch r r'

quickMatch (TRecord fields _ _) (TRecord fields' _ _) =
  quickMatchFields (M.toList fields) (M.toList fields')

quickMatch (TVar _) _ = True
quickMatch _ (TVar _) = True
quickMatch (TCon a fpa) (TCon b fpb)
  | a == b && fpa == fpb = True
  | a == b && (fpa == "JSX" || fpb == "JSX") = True
  | a /= b = False
  | fpa /= fpb = False

quickMatch (TCon (TC tNameA _) _) (TApp (TCon (TC tNameB _) _) _)
  | tNameA == "String" && tNameB == "Element" =
      True

quickMatch (TApp (TCon (TC tNameB _) _) _) (TCon (TC tNameA _) _)
  | tNameB == "Element" && tNameA == "String" =
      True

quickMatch _ _ =
  False

quickMatchFields :: [(String, Type)] -> [(String, Type)] -> Bool
quickMatchFields fields1 fields2 = case fields1 of
  (name1, t1) : next1 ->
    case fields2 of
      [] ->
        False

      (name2, t2) : next2 ->
        name1 == name2 && quickMatch t1 t2 && quickMatchFields next1 next2

  [] ->
    null fields2
