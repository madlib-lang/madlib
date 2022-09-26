{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Unify where


import           Infer.Type
import           Infer.Substitute
import           Infer.Instantiate
import           Error.Error
import           Error.Context
import           Infer.Infer
import           Infer.Env
import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified AST.Canonical                 as Can



varBind :: TVar -> Type -> Infer Substitution
varBind tv t@(TRecord fields (Just base))
  | tv `elem` concat (ftv <$> fields) = throwError $ CompilationError (InfiniteType tv t) NoContext
  | otherwise                         = return $ M.singleton tv (TRecord fields (Just base))
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
      s1 <- unify tBase' (TRecord (M.union fields fields') base)

      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
          -- z              = zip (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      s2 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      s3 <- unify tBase tBase'

      return $ s3 `compose` s2 `compose` s1

    (Just tBase, Nothing) -> do
      s1 <- unify tBase (TRecord fields' base)

      unless (M.null (M.difference fields fields')) $ throwError (CompilationError (UnificationError r l) NoContext)

      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
          -- z              = zip (M.elems fieldsToCheck) (M.elems fieldsToCheck')
      s2 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      return $ s2 `compose` s1

    (Nothing, Just tBase') -> do
      s1 <- unify tBase' (TRecord fields base')

      unless (M.null (M.difference fields' fields)) $ throwError (CompilationError (UnificationError r l) NoContext)

      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
          -- z              = zip (M.elems fieldsToCheck) (M.elems fieldsToCheck')
      s2 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      return $ s2 `compose` s1

    _ -> do
      let extraFields  = M.difference fields fields'
          extraFields' = M.difference fields' fields
      if extraFields' /= mempty || extraFields /= mempty
        then throwError $ CompilationError (UnificationError r l) NoContext
        else do
          -- let updatedFields  = M.union fields extraFields'
          --     updatedFields' = M.union fields' extraFields
          --     types          = M.elems updatedFields
          --     types'         = M.elems updatedFields'
          --     z              = zip types types'
          -- unifyVars M.empty z
          let updatedFields  = M.union fields extraFields'
              updatedFields' = M.union fields' extraFields
              types          = M.elems updatedFields
              types'         = M.elems updatedFields'
          unifyVars' M.empty types types'

  unify (TVar tv) t         = varBind tv t
  unify t         (TVar tv) = varBind tv t
  unify t1@(TCon a fpa) t2@(TCon b fpb)
    | a == b && fpa == fpb = return M.empty
    | a == b && (fpa == "JSX" || fpb == "JSX") = return M.empty
    | a /= b               = throwError $ CompilationError (UnificationError t2 t1) NoContext
    | fpa /= fpb           = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId a) fpa fpb) NoContext

  unify (TCon (TC tNameA _) _) t2@(TApp (TCon (TC tNameB k) fpb) _)
    | tNameA == "String" && tNameB == "Element" = do
        tv <- newTVar Star
        unify (TApp (TCon (TC "Element" k) fpb) tv) t2

  unify t1@(TApp (TCon (TC tNameB k) fpb) _) (TCon (TC tNameA _) _)
    | tNameB == "Element" && tNameA == "String" = do
        tv <- newTVar Star
        unify (TApp (TCon (TC "Element" k) fpb) tv) t1

  unify t1 t2 = throwError $ CompilationError (UnificationError t2 t1) NoContext




instance (Unify t, Show t, Substitutable t) => Unify [t] where
  unify (x : xs) (y : ys) = do
    s1 <- unify x y
    s2 <- unify (apply s1 xs) (apply s1 ys)
    return (s2 <> s1)
  unify [] [] = return nullSubst
  unify _  _  = throwError $ CompilationError Error NoContext


unifyVars :: Substitution -> [(Type, Type)] -> Infer Substitution
unifyVars s ((tp, tp') : xs) = do
  s1 <- unify tp tp'
  unifyVars (compose s s1) xs
unifyVars s [] = return s

unifyVars' :: Substitution -> [Type] -> [Type] -> Infer Substitution
unifyVars' s (tp : xs) (tp' : xs') = do
  s1 <- unify tp tp'
  unifyVars' (compose s s1) xs xs'
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
    sr <- match r r'
    merge sl sr
  match (TVar u) t | kind u == kind t =
    return $ M.singleton u t
  match (TCon tc1 fp1) (TCon tc2 fp2)
    | tc1 == tc2 && fp1 == fp2 = return nullSubst
    | tc1 == tc2 && (fp1 == "JSX" || fp2 == "JSX") = return M.empty
    | fp1 /= fp2 = throwError $ CompilationError (TypesHaveDifferentOrigin (getTConId tc1) fp1 fp2) NoContext
  match (TRecord fields1 _) (TRecord fields2 _) =
    -- Not complete but that's all we need for now as we don't support userland
    -- record instances. An instance for a record would be matched for all records.
    unify (TRecord fields1 Nothing) (TRecord fields2 Nothing)
    -- return M.empty
  match t1 t2 = throwError $ CompilationError (UnificationError t1 t2) NoContext

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
      (t2', t1') <- improveRecordErrorTypes t2 t1
      addContext env exp (CompilationError (UnificationError t2' t1') ctx)

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
  s1 <- gentleUnify t1 t2
  s2 <- gentleUnify t2 t1
  let t1' = cleanBase $ apply (s1 `compose` s2) t1
  let t2' = cleanBase $ apply (s1 `compose` s2) t2
  -- let t1' = skipBase $ apply (s1 `compose` s2) t1
  -- let t2' = skipBase $ apply (s1 `compose` s2) t2
  return (t1', t2')


-- TODO: need to copy unify but use gentleUnify everywhere or else we only do it on the surface
-- gentleUnify :: Type -> Type -> Infer Substitution
-- gentleUnify t1 t2 = catchError (unify t1 t2) (const $ return mempty)

-- TODO: this should probably not always happen
cleanBase :: Type -> Type
cleanBase t = case t of
  TRecord fields (Just (TRecord extraFields _)) ->
    TRecord (extraFields <> fields) Nothing

  TApp l r ->
    TApp (cleanBase l) (cleanBase r)

  _ ->
    t

skipBase :: Type -> Type
skipBase t = case t of
  TRecord fields (Just (TRecord extraFields _)) ->
    TRecord (extraFields <> fields) Nothing

  TRecord fields _ ->
    TRecord fields Nothing

  TApp l r ->
    TApp (skipBase l) (skipBase r)

  _ ->
    t


gentleUnifyVars :: Substitution -> [Type] -> [Type] -> Infer Substitution
gentleUnifyVars s (tp : xs) (tp' : xs') = do
  s1 <- gentleUnify tp tp'
  gentleUnifyVars (compose s s1) xs xs'
gentleUnifyVars s _ _  = return s


gentleUnify :: Type -> Type -> Infer Substitution
gentleUnify (l `TApp` r) (l' `TApp` r') = do
  s1 <- gentleUnify l l'
  s2 <- gentleUnify (apply s1 r) (apply s1 r')
  return $ compose s1 s2

gentleUnify l@(TRecord fields base) r@(TRecord fields' base') = case (base, base') of
  (Just tBase, Just tBase') -> do
    s1 <- gentleUnify tBase' (TRecord (M.union fields fields') base)

    let fieldsToCheck  = M.intersection fields fields'
        fieldsToCheck' = M.intersection fields' fields

    s2 <- gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
    s3 <- gentleUnify tBase tBase'

    return $ s3 `compose` s2 `compose` s1

  (Just tBase, Nothing) -> do
    s1 <- gentleUnify tBase (TRecord fields' base)

    if not $ M.null (M.difference fields fields') then
      return M.empty
    else do
      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
      s2 <- gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')

      return $ s2 `compose` s1

  (Nothing, Just tBase') -> do
    s1 <- gentleUnify tBase' (TRecord fields base')

    if not $ M.null (M.difference fields' fields) then
      return M.empty
    else do
      let fieldsToCheck  = M.intersection fields fields'
          fieldsToCheck' = M.intersection fields' fields
      s2 <- gentleUnifyVars M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
      return $ s2 `compose` s1

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
        gentleUnifyVars M.empty types types'

gentleUnify (TVar tv) t         = return $ M.singleton tv t
gentleUnify t         (TVar tv) = return $ M.singleton tv t
gentleUnify (TCon a fpa) (TCon b fpb)
  | a == b && fpa == fpb = return M.empty
  | a == b && (fpa == "JSX" || fpb == "JSX") = return M.empty
  | a /= b               = return M.empty
  | fpa /= fpb           = return M.empty

gentleUnify (TCon (TC tNameA _) _) t2@(TApp (TCon (TC tNameB k) fpb) _)
  | tNameA == "String" && tNameB == "Element" = do
      tv <- newTVar Star
      gentleUnify (TApp (TCon (TC "Element" k) fpb) tv) t2

gentleUnify t1@(TApp (TCon (TC tNameB k) fpb) _) (TCon (TC tNameA _) _)
  | tNameB == "Element" && tNameA == "String" = do
      tv <- newTVar Star
      gentleUnify (TApp (TCon (TC "Element" k) fpb) tv) t1

gentleUnify _ _ = return M.empty

