{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Infer.Placeholder where

import           Infer.Type
import           Infer.Env
import           Infer.Infer
import           Infer.Pred
import           Infer.Unify
import           Infer.Substitute
import qualified AST.Solved                    as Slv
import qualified Data.Map                      as M
import           Control.Monad.Except
import           Error.Error
import           Error.Context
import           Infer.Instantiate


insertVarPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
insertVarPlaceholders _   exp                    []       = return exp

insertVarPlaceholders env exp@(Slv.Solved t a e) (p : ps) = do
  ts <- getCanonicalPlaceholderTypes env p
  if isMethod env exp
    then case e of
      Slv.Var n -> do
        var <- shouldInsert env $ IsIn (predClass p) (predTypes p) Nothing
        return $ Slv.Solved t a $ Slv.Placeholder (Slv.MethodRef (predClass p) n var, ts) exp
      _ -> return exp
    else do
      insert <- shouldInsert env p

      let exp' =
            if insert then
              Slv.Solved t a $ Slv.Placeholder (Slv.ClassRef (predClass p) [] True insert, ts) exp
            else
              exp
      insertVarPlaceholders env exp' ps



insertClassPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
insertClassPlaceholders _   exp []       = return exp
insertClassPlaceholders env exp (p : ps) = do
  insert <- shouldInsert env p
  if not insert
    then insertClassPlaceholders env exp ps
    else case exp of
      Slv.Solved a t (Slv.Assignment n e) ->
        let exp' = Slv.Solved a t
              $ Slv.Assignment
                  n
                  (Slv.Solved a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)
        in  insertClassPlaceholders env exp' ps

      Slv.Solved a t (Slv.TypedExp (Slv.Solved a' t' (Slv.Assignment n e)) _ _) ->
        let exp' = Slv.Solved a t
              $ Slv.Assignment
                  n
                  (Slv.Solved a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)
        in  insertClassPlaceholders env exp' ps

      Slv.Solved a t (Slv.Export (Slv.Solved a' t' (Slv.Assignment n e))) ->
        let exp' = Slv.Solved a t $ Slv.Export
              (Slv.Solved
                a'
                t'
                (Slv.Assignment
                  n
                  (Slv.Solved a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)
                )
              )
        in  insertClassPlaceholders env exp' ps

      Slv.Solved a t (Slv.TypedExp (Slv.Solved a' t' (Slv.Export (Slv.Solved a'' t'' (Slv.Assignment n e)))) _ _) ->
        let exp' = Slv.Solved
              a
              t
              (Slv.Export
                ( Slv.Solved a t
                $ Slv.Assignment
                    n
                    (Slv.Solved a t $ Slv.Placeholder (Slv.ClassRef (predClass p) [] False True, predTypes p) e)
                )
              )
        in  insertClassPlaceholders env exp' ps

      _ -> return exp


shouldInsert :: Env -> Pred -> Infer Bool
shouldInsert env p = do
  let cls = predClass p
      ts  = predTypes p
  inst <- findInst env $ IsIn cls ts Nothing

  let insert = case inst of
        Just (Instance (_ :=> p') _) ->
          let (_, IsIn _ withoutVars _) = removeInstanceVars p' p in any isTVar withoutVars
        Nothing -> True
  return insert


getCanonicalPlaceholderTypes :: Env -> Pred -> Infer [Type]
getCanonicalPlaceholderTypes env p@(IsIn cls ts _) = do
  inst <- findInst env p
  return $ case inst of
    Just (Instance (_ :=> (IsIn _ ts'' _)) _) -> ts''
    Nothing -> ts

updateMethodPlaceholder :: Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateMethodPlaceholder env push s ph@(Slv.Solved qt@(_ :=> t) a (Slv.Placeholder (Slv.MethodRef cls method var, instanceTypes) (Slv.Solved qt' a' exp)))
  = do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env (IsIn cls instanceTypes' Nothing)
    var'  <- shouldInsert env $ IsIn cls instanceTypes' Nothing -- Reconsider if the instance is fully resolved

    -- The following block serves to check that the inferred type for a method
    -- which was unified in type check with the class' scheme, is actually a
    -- type that is correct, given the actual instance's specific type.
    ss    <- do
      maybeInst <- findInst env (IsIn cls instanceTypes' Nothing)
      case maybeInst of
        Just (Instance _ methods) -> case M.lookup method methods of
          Just methodScheme -> do
            (_ :=> mtdT) <- instantiate methodScheme
            catchError
              (match mtdT (apply s t))
              (\(CompilationError e _) ->
                throwError $ CompilationError e (Context (envCurrentPath env) a (envBacktrace env))
              )
          Nothing -> return mempty
        Nothing -> return mempty

    ps  <- catchError (byInst env $ IsIn cls instanceTypes' Nothing) (const $ return [])
    ps' <- getAllParentPreds env ps
    pushPlaceholders env
                     (Slv.Solved qt a (Slv.Placeholder (Slv.MethodRef cls method var', types) (Slv.Solved qt' a' exp)))
                     ps'


pushPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
pushPlaceholders _   exp                    []                     = return exp
pushPlaceholders env exp@(Slv.Solved qt a _) (p@(IsIn cls ts _) : ps) = do
  var <- shouldInsert env $ IsIn cls ts Nothing
  ps' <- buildClassRefPreds env cls ts
  ts' <- getCanonicalPlaceholderTypes env p

  let ph = Slv.Solved qt a (Slv.Placeholder (Slv.ClassRef cls ps' True var, ts') exp)
  pushPlaceholders env ph ps


updateClassPlaceholder :: Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateClassPlaceholder env push s ph = case ph of
  Slv.Solved qt a (Slv.Placeholder (Slv.ClassRef cls [] call var, instanceTypes) exp) -> do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env $ IsIn cls instanceTypes' Nothing
    var'  <- shouldInsert env $ IsIn cls instanceTypes' Nothing
    exp'  <- updatePlaceholders env push s exp
    ps'   <- buildClassRefPreds env cls instanceTypes'

    if not call then
      return $ Slv.Solved qt a (Slv.Placeholder (Slv.ClassRef cls [] call var, instanceTypes) exp')
    else
      return $ Slv.Solved qt a (Slv.Placeholder (Slv.ClassRef cls ps' call var', types) exp')

  _ -> return ph


buildClassRefPreds :: Env -> String -> [Type] -> Infer [Slv.ClassRefPred]
buildClassRefPreds env cls ts = do
  maybeInst <- findInst env $ IsIn cls ts Nothing
  instTypes <- case maybeInst of
    Just (Instance (_ :=> (IsIn _ x _)) _) -> return x
    Nothing                              -> return ts

  s    <- unify instTypes ts
  ps   <- catchError (byInst env $ IsIn cls ts Nothing) (const $ return [])
  pps' <- mapM (getParentPreds env) (reverse (apply s ps))
  let ps' = concat $ reverse <$> pps'
  mapM
    (\(IsIn cls' ts' _) -> do
      next <- buildClassRefPreds env cls' ts'
      ts'' <- getCanonicalPlaceholderTypes env $ IsIn cls' ts' Nothing
      var  <- shouldInsert env (IsIn cls' ts' Nothing)
      return $ Slv.CRPNode cls' ts'' var next
    )
    ps'


updatePlaceholders :: Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updatePlaceholders env push s fullExp@(Slv.Untyped _ _)   = return fullExp
updatePlaceholders env push s fullExp@(Slv.Solved qt a e) = case e of
  Slv.Placeholder (ref, t) exp -> case ref of
    Slv.MethodRef{} -> updateMethodPlaceholder env push s fullExp
    _               -> updateClassPlaceholder env push s fullExp

  Slv.App abs arg final -> do
    abs' <- updatePlaceholders env push s abs
    arg' <- updatePlaceholders env push s arg
    return $ Slv.Solved qt a $ Slv.App abs' arg' final

  Slv.Abs (Slv.Solved paramType paramArea param) es -> do
    es' <- mapM (updatePlaceholders env push s) es
    let param' = Slv.Solved paramType paramArea param
    return $ Slv.Solved qt a $ Slv.Abs param' es'

  Slv.Where exp iss -> do
    exp' <- updatePlaceholders env push s exp
    iss' <- mapM (updateIs s) iss
    return $ Slv.Solved qt a $ Slv.Where exp' iss'

  Slv.Assignment n exp -> do
    exp' <- updatePlaceholders env push s exp
    return $ Slv.Solved qt a $ Slv.Assignment n exp'

  Slv.ListConstructor li -> do
    li' <- mapM (updateListItem s) li
    return $ Slv.Solved qt a $ Slv.ListConstructor li'

  Slv.TypedExp exp typing sc -> do
    exp' <- updatePlaceholders env push s exp
    return $ Slv.Solved qt a $ Slv.TypedExp exp' typing sc

  Slv.Export exp -> do
    exp' <- updatePlaceholders env push s exp
    return $ Slv.Solved qt a $ Slv.Export exp'

  Slv.If econd eif eelse -> do
    econd' <- updatePlaceholders env push s econd
    eif'   <- updatePlaceholders env push s eif
    eelse' <- updatePlaceholders env push s eelse
    return $ Slv.Solved qt a $ Slv.If econd' eif' eelse'

  Slv.TupleConstructor es -> do
    es' <- mapM (updatePlaceholders env push s) es
    return $ Slv.Solved qt a $ Slv.TupleConstructor es'

  Slv.TemplateString es -> do
    es' <- mapM (updatePlaceholders env push s) es
    return $ Slv.Solved qt a $ Slv.TemplateString es'

  Slv.Access rec field -> do
    rec'   <- updatePlaceholders env push s rec
    field' <- updatePlaceholders env push s field
    return $ Slv.Solved qt a $ Slv.Access rec' field'

  Slv.Record fields -> do
    fields' <- mapM (updateField s) fields
    return $ Slv.Solved qt a $ Slv.Record fields'

  _ -> return $ Slv.Solved qt a e
 where
  updateIs :: Substitution -> Slv.Is -> Infer Slv.Is
  updateIs s (Slv.Solved t a is) = case is of
    Slv.Is pat exp -> do
      exp' <- updatePlaceholders env push s exp
      return $ Slv.Solved t a $ Slv.Is pat exp'

  updateListItem :: Substitution -> Slv.ListItem -> Infer Slv.ListItem
  updateListItem s (Slv.Solved t area li) = case li of
    Slv.ListItem e ->
      Slv.Solved t area . Slv.ListItem <$> updatePlaceholders env push s e

    Slv.ListSpread e ->
      Slv.Solved t area . Slv.ListSpread <$> updatePlaceholders env push s e

  updateField :: Substitution -> Slv.Field -> Infer Slv.Field
  updateField s (Slv.Solved t area field) = case field of
    Slv.Field       (n, e) -> Slv.Solved t area . Slv.Field . (n, ) <$> updatePlaceholders env push s e
    Slv.FieldSpread e      -> Slv.Solved t area . Slv.FieldSpread <$> updatePlaceholders env push s e



isMethod :: Env -> Slv.Exp -> Bool
isMethod env (Slv.Untyped _ _)  = False
isMethod env (Slv.Solved _ _ e) = case e of
  Slv.Var n -> Just True == (M.lookup n (envMethods env) >> return True)
  _         -> False
