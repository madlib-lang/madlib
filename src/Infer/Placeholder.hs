{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Placeholder where

import Infer.Type
import Infer.Infer
import qualified AST.Solved as Slv
import qualified Data.Map as M
import Infer.Pred
import Infer.Unify
import Control.Monad.Except
import Infer.Substitute


insertVarPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
insertVarPlaceholders _   exp                    []       = return exp

insertVarPlaceholders env exp@(Slv.Solved t a e) (p : ps) = do
  ts <- getCanonicalPlaceholderTypes env p
  if isMethod env exp
    then case e of
      Slv.Var n -> do
        var <- shouldInsert env $ IsIn (predClass p) (predTypes p)
        return $ Slv.Solved t a $ Slv.Placeholder
          (Slv.MethodRef (predClass p) n var, ts)
          exp
      _ -> return exp
    else do
      insert <- shouldInsert env p

      let exp' = if insert
            then Slv.Solved t a
              $ Slv.Placeholder
                  (Slv.ClassRef (predClass p) [] True insert, ts)
                  exp
            else exp
      insertVarPlaceholders env exp' ps



insertClassPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
insertClassPlaceholders _   exp []       = return exp
insertClassPlaceholders env exp (p : ps) = do
  insert <- shouldInsert env p
  if not insert
    then insertClassPlaceholders env exp ps
    else case exp of
      Slv.Solved a t (Slv.Assignment n e) ->
        let exp' = Slv.Solved a t $ Slv.Assignment
              n
              ( Slv.Solved a t
              $ Slv.Placeholder
                  (Slv.ClassRef (predClass p) [] False True, predTypes p)
                  e
              )
        in  insertClassPlaceholders env exp' ps

      Slv.Solved a t (Slv.TypedExp (Slv.Solved a' t' (Slv.Assignment n e)) _)
        -> let exp' = Slv.Solved a t $ Slv.Assignment
                 n
                 ( Slv.Solved a t
                 $ Slv.Placeholder
                     (Slv.ClassRef (predClass p) [] False True, predTypes p)
                     e
                 )
           in  insertClassPlaceholders env exp' ps

      Slv.Solved a t (Slv.Export (Slv.Solved a' t' (Slv.Assignment n e))) ->
        let exp' = Slv.Solved a t $ Slv.Export
              (Slv.Solved
                a'
                t'
                (Slv.Assignment
                  n
                  ( Slv.Solved a t
                  $ Slv.Placeholder
                      (Slv.ClassRef (predClass p) [] False True, predTypes p)
                      e
                  )
                )
              )
        in  insertClassPlaceholders env exp' ps

      Slv.Solved a t (Slv.TypedExp (Slv.Solved a' t' (Slv.Export (Slv.Solved a'' t'' (Slv.Assignment n e)))) _)
        -> let exp' = Slv.Solved
                 a
                 t
                 (Slv.Export
                   (Slv.Solved a t $ Slv.Assignment
                     n
                     ( Slv.Solved a t
                     $ Slv.Placeholder
                         ( Slv.ClassRef (predClass p) [] False True
                         , predTypes p
                         )
                         e
                     )
                   )
                 )
           in  insertClassPlaceholders env exp' ps

      _ -> return exp


shouldInsert :: Env -> Pred -> Infer Bool
shouldInsert env p = do
  let cls = predClass p
      ts  = predTypes p
  inst <- findInst env $ IsIn cls ts

  let insert = case inst of
        Just (Instance (_ :=> p')) ->
          let (_, IsIn _ withoutVars) = removeInstanceVars p' p
          in  any isTVar withoutVars
        Nothing -> True
  return insert


getCanonicalPlaceholderTypes :: Env -> Pred -> Infer [Type]
getCanonicalPlaceholderTypes env p@(IsIn cls ts) = do
  inst <- findInst env p
  return $ case inst of
    Just (Instance (_ :=> (IsIn _ ts''))) -> ts''
    Nothing                               -> ts

updateMethodPlaceholder :: Env -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateMethodPlaceholder env s ph@(Slv.Solved t a (Slv.Placeholder (Slv.MethodRef cls method var, instanceTypes) exp))
  = do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env (IsIn cls instanceTypes')
    var'  <- shouldInsert env $ IsIn cls instanceTypes' -- Reconsider if the instance is fully resolved

    ps <- catchError (byInst env $ IsIn cls instanceTypes')
                      (const $ return [])
    ps' <- getAllParentInterfaces env ps
    pushPlaceholders
      env
      (Slv.Solved
        t
        a
        (Slv.Placeholder (Slv.MethodRef cls method var', types) exp)
      )
      ps'

pushPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
pushPlaceholders _   exp                    []                   = return exp
pushPlaceholders env exp@(Slv.Solved t a _) (p@(IsIn cls ts) : ps) = do
  var <- shouldInsert env $ IsIn cls ts
  ps'  <- buildClassRefPreds env cls ts
  ts' <- getCanonicalPlaceholderTypes env p
  let ph =
        Slv.Solved t a (Slv.Placeholder (Slv.ClassRef cls ps' True var, ts') exp)
  pushPlaceholders env ph ps


updateClassPlaceholder :: Env -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateClassPlaceholder env s ph@(Slv.Solved t a (Slv.Placeholder (Slv.ClassRef cls [] call _, instanceTypes) exp))
  = do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env $ IsIn cls instanceTypes'
    var   <- shouldInsert env $ IsIn cls instanceTypes'
    exp' <- updatePlaceholders env s exp
    ps'  <- buildClassRefPreds env cls instanceTypes'

    return $ Slv.Solved
      t
      a
      (Slv.Placeholder (Slv.ClassRef cls ps' call var, types) exp')


buildClassRefPreds :: Env -> String -> [Type] -> Infer [Slv.ClassRefPred]
buildClassRefPreds env cls ts = do
  maybeInst <- findInst env $ IsIn cls ts
  instTypes <- case maybeInst of
    Just (Instance (_ :=> (IsIn _ x))) -> return x
    Nothing -> return ts

  s <- unify instTypes ts
  ps <- catchError (byInst env $ IsIn cls ts) (const $ return [])
  pps' <- mapM (getParentInterfaces env) (reverse (apply s ps))
  let ps'  = concat $ reverse <$> pps'
  mapM
    (\(IsIn cls' ts') -> do
      next <- buildClassRefPreds env cls' ts'
      ts'' <- getCanonicalPlaceholderTypes env $ IsIn cls' ts'
      var <- shouldInsert env (IsIn cls' ts')
      return $ Slv.CRPNode cls' ts'' var next
    )
    ps'


updatePlaceholders :: Env -> Substitution -> Slv.Exp -> Infer Slv.Exp
updatePlaceholders env s fullExp@(Slv.Solved t a e) = case e of
  Slv.Placeholder (ref, t) exp -> case ref of
    Slv.MethodRef{} -> updateMethodPlaceholder env s fullExp
    _               -> updateClassPlaceholder env s fullExp

  Slv.App abs arg final -> do
    abs' <- updatePlaceholders env s abs
    arg' <- updatePlaceholders env s arg
    return $ Slv.Solved t a $ Slv.App abs' arg' final

  Slv.Abs param es -> do
    es' <- mapM (updatePlaceholders env s) es
    return $ Slv.Solved t a $ Slv.Abs param es'

  Slv.Where exp iss -> do
    exp' <- updatePlaceholders env s exp
    iss' <- mapM (updateIs s) iss
    return $ Slv.Solved t a $ Slv.Where exp' iss'

  Slv.Assignment n exp -> do
    exp' <- updatePlaceholders env s exp
    return $ Slv.Solved t a $ Slv.Assignment n exp'

  Slv.ListConstructor li -> do
    li' <- mapM (updateListItem s) li
    return $ Slv.Solved t a $ Slv.ListConstructor li'

  Slv.TypedExp exp typing -> do
    exp' <- updatePlaceholders env s exp
    return $ Slv.Solved t a $ Slv.TypedExp exp' typing

  Slv.Export exp -> do
    exp' <- updatePlaceholders env s exp
    return $ Slv.Solved t a $ Slv.Export exp'

  Slv.If econd eif eelse -> do
    econd' <- updatePlaceholders env s econd
    eif'   <- updatePlaceholders env s eif
    eelse' <- updatePlaceholders env s eelse
    return $ Slv.Solved t a $ Slv.If econd' eif' eelse'

  Slv.TupleConstructor es -> do
    es' <- mapM (updatePlaceholders env s) es
    return $ Slv.Solved t a $ Slv.TupleConstructor es'

  _ -> return $ Slv.Solved t a e
 where
  updateIs :: Substitution -> Slv.Is -> Infer Slv.Is
  updateIs s (Slv.Solved t a is) = case is of
    Slv.Is pat exp -> do
      exp' <- updatePlaceholders env s exp
      return $ Slv.Solved t a $ Slv.Is pat exp'

  updateListItem :: Substitution -> Slv.ListItem -> Infer Slv.ListItem
  updateListItem s li = case li of
    Slv.ListItem   e -> Slv.ListItem <$> updatePlaceholders env s e
    Slv.ListSpread e -> Slv.ListSpread <$> updatePlaceholders env s e

isMethod :: Env -> Slv.Exp -> Bool
isMethod env (Slv.Solved _ _ e) = case e of
  Slv.Var n -> Just True == (M.lookup n (envmethods env) >> return True)
  _         -> False
