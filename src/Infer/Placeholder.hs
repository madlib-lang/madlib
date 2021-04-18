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
        var <- shouldInsert env $ IsIn (predClass p) (predTypes p)
        return $ Slv.Solved t a $ Slv.Placeholder (Slv.MethodRef (predClass p) n var, ts) exp
      _ -> return exp
    else do
      insert <- shouldInsert env p

      let exp' =
            if insert then Slv.Solved t a $ Slv.Placeholder (Slv.ClassRef (predClass p) [] True insert, ts) exp else exp
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

      Slv.Solved a t (Slv.TypedExp (Slv.Solved a' t' (Slv.Assignment n e)) _) ->
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

      Slv.Solved a t (Slv.TypedExp (Slv.Solved a' t' (Slv.Export (Slv.Solved a'' t'' (Slv.Assignment n e)))) _) ->
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
  inst <- findInst env $ IsIn cls ts

  let insert = case inst of
        Just (Instance (_ :=> p') _) ->
          let (_, IsIn _ withoutVars) = removeInstanceVars p' p in any isTVar withoutVars
        Nothing -> True
  return insert


getCanonicalPlaceholderTypes :: Env -> Pred -> Infer [Type]
getCanonicalPlaceholderTypes env p@(IsIn cls ts) = do
  inst <- findInst env p
  return $ case inst of
    Just (Instance (_ :=> (IsIn _ ts'')) _) -> ts''
    Nothing -> ts

updateMethodPlaceholder :: Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateMethodPlaceholder env push s ph@(Slv.Solved t a (Slv.Placeholder (Slv.MethodRef cls method var, instanceTypes) (Slv.Solved t' a' exp)))
  = do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env (IsIn cls instanceTypes')
    var'  <- shouldInsert env $ IsIn cls instanceTypes' -- Reconsider if the instance is fully resolved

    -- The following block serves to check that the inferred type for a method
    -- which was unified in type check with the class' scheme, is actually a
    -- type that is correct, given the actual instance's specific type.
    ss    <- do
      maybeInst <- findInst env (IsIn cls instanceTypes')
      case maybeInst of
        Just (Instance _ methods) -> case M.lookup method methods of
          Just methodScheme -> do
            (_ :=> mtdT) <- instantiate methodScheme
            catchError
              (match mtdT (apply s t))
              (\(CompilationError e _) -> throwError $ CompilationError e (Context (envCurrentPath env) a (envBacktrace env)))
          Nothing -> return mempty
        Nothing -> return mempty

    ps  <- catchError (byInst env $ IsIn cls instanceTypes') (const $ return [])
    ps' <- getAllParentPreds env ps
    pushPlaceholders
      env
      (Slv.Solved (apply s t)
                  a
                  (Slv.Placeholder (Slv.MethodRef cls method var', types) (Slv.Solved (apply s t') a' exp))
      )
      ps'

pushPlaceholders :: Env -> Slv.Exp -> [Pred] -> Infer Slv.Exp
pushPlaceholders _   exp                    []                     = return exp
pushPlaceholders env exp@(Slv.Solved t a _) (p@(IsIn cls ts) : ps) = do
  var <- shouldInsert env $ IsIn cls ts
  ps' <- buildClassRefPreds env cls ts
  ts' <- getCanonicalPlaceholderTypes env p
  let ph = Slv.Solved t a (Slv.Placeholder (Slv.ClassRef cls ps' True var, ts') exp)
  pushPlaceholders env ph ps


updateClassPlaceholder :: Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updateClassPlaceholder env push s ph = case ph of
  Slv.Solved t a (Slv.Placeholder (Slv.ClassRef cls [] call _, instanceTypes) exp) -> do
    let instanceTypes' = apply s instanceTypes
    types <- getCanonicalPlaceholderTypes env $ IsIn cls instanceTypes'
    var   <- shouldInsert env $ IsIn cls instanceTypes'
    exp'  <- updatePlaceholders env push s exp
    ps'   <- buildClassRefPreds env cls instanceTypes'

    if not var && not call
      then return ph
      else return $ Slv.Solved (apply s t) a (Slv.Placeholder (Slv.ClassRef cls ps' call var, types) exp')

  _ -> return ph


buildClassRefPreds :: Env -> String -> [Type] -> Infer [Slv.ClassRefPred]
buildClassRefPreds env cls ts = do
  maybeInst <- findInst env $ IsIn cls ts
  instTypes <- case maybeInst of
    Just (Instance (_ :=> (IsIn _ x)) _) -> return x
    Nothing                              -> return ts

  s    <- unify instTypes ts
  ps   <- catchError (byInst env $ IsIn cls ts) (const $ return [])
  pps' <- mapM (getParentPreds env) (reverse (apply s ps))
  let ps' = concat $ reverse <$> pps'
  mapM
    (\(IsIn cls' ts') -> do
      next <- buildClassRefPreds env cls' ts'
      ts'' <- getCanonicalPlaceholderTypes env $ IsIn cls' ts'
      var  <- shouldInsert env (IsIn cls' ts')
      return $ Slv.CRPNode cls' ts'' var next
    )
    ps'


updatePlaceholders :: Env -> Bool -> Substitution -> Slv.Exp -> Infer Slv.Exp
updatePlaceholders env push s fullExp@(Slv.Solved t a e) = case e of
  Slv.Placeholder (ref, t) exp -> case ref of
    Slv.MethodRef{} -> updateMethodPlaceholder env push s fullExp
    _               -> updateClassPlaceholder env push s fullExp

  Slv.App abs arg final -> do
    abs' <- updatePlaceholders env push s abs
    arg' <- updatePlaceholders env push s arg
    return $ Slv.Solved t a $ Slv.App abs' arg' final

  Slv.Abs (Slv.Solved paramType paramArea param) es -> do
    es' <- if push || length es == 1
      then mapM (updatePlaceholders env push s) es
      else do
        let start = init es
        let l     = last es
        l' <- updatePlaceholders env push s l
        return $ start <> [l']
    let param' = Slv.Solved paramType paramArea param
    return $ Slv.Solved t a $ Slv.Abs param' es'

  Slv.Where exp iss -> do
    exp' <- updatePlaceholders env push s exp
    iss' <- mapM (updateIs s) iss
    return $ Slv.Solved t a $ Slv.Where exp' iss'

  Slv.Assignment n exp -> do
    exp' <- updatePlaceholders env push s exp
    return $ Slv.Solved t a $ Slv.Assignment n exp'

  Slv.ListConstructor li -> do
    li' <- mapM (updateListItem s) li
    return $ Slv.Solved t a $ Slv.ListConstructor li'

  Slv.TypedExp exp typing -> do
    exp' <- updatePlaceholders env push s exp
    return $ Slv.Solved t a $ Slv.TypedExp exp' typing

  Slv.Export exp -> do
    exp' <- updatePlaceholders env push s exp
    return $ Slv.Solved t a $ Slv.Export exp'

  Slv.If econd eif eelse -> do
    econd' <- updatePlaceholders env push s econd
    eif'   <- updatePlaceholders env push s eif
    eelse' <- updatePlaceholders env push s eelse
    return $ Slv.Solved t a $ Slv.If econd' eif' eelse'

  Slv.TupleConstructor es -> do
    es' <- mapM (updatePlaceholders env push s) es
    return $ Slv.Solved t a $ Slv.TupleConstructor es'

  Slv.TemplateString es -> do
    es' <- mapM (updatePlaceholders env push s) es
    return $ Slv.Solved t a $ Slv.TemplateString es'

  Slv.Access rec field -> do
    rec'   <- updatePlaceholders env push s rec
    field' <- updatePlaceholders env push s field
    return $ Slv.Solved t a $ Slv.Access rec' field'

  Slv.Record fields -> do
    fields' <- mapM (updateField s) fields
    return $ Slv.Solved t a $ Slv.Record fields'

  _ -> return $ Slv.Solved t a e
 where
  updateIs :: Substitution -> Slv.Is -> Infer Slv.Is
  updateIs s (Slv.Solved t a is) = case is of
    Slv.Is pat exp -> do
      exp' <- updatePlaceholders env push s exp
      return $ Slv.Solved t a $ Slv.Is pat exp'

  updateListItem :: Substitution -> Slv.ListItem -> Infer Slv.ListItem
  updateListItem s (Slv.Solved t area li) = case li of
    Slv.ListItem e -> do
      updated <- updatePlaceholders env push s e
      case updated of
        Slv.Solved _ _ (Slv.App (Slv.Solved tAbs _ (Slv.Var "__tmp_jsx_children__")) elem _) -> case tAbs of
          TApp (TApp (TCon (TC "(->)" _) _) (TCon (TC "String" Star) "prelude")) _ ->
            return $ Slv.Solved t area $ Slv.ListItem
              (Slv.Solved t area (Slv.App (Slv.Solved tAbs area (Slv.Var "text")) elem True))

          TApp (TApp (TCon (TC "(->)" _) _) (TApp (TCon (TC "List" _) "prelude") (TCon (TC "String" Star) "prelude"))) _
            -> return $ Slv.Solved t area $ Slv.ListSpread
              (Slv.Solved t area (Slv.App (Slv.Solved tAbs area (Slv.Var "text")) elem True))

          TApp (TApp (TCon (TC "(->)" _) _) (TApp (TCon (TC "List" _) "prelude") tSpread)) tElem -> do
            catchError
              (unify tElem tSpread)
              (\(CompilationError err _) -> throwError $ CompilationError err (Context (envCurrentPath env) area (envBacktrace env))
              )
            return $ Slv.Solved t area $ Slv.ListSpread elem

          TApp (TApp (TCon (TC "(->)" _) _) tSingleChild) tElem -> do
            catchError
              (unify tElem tSingleChild)
              (\(CompilationError err _) -> throwError $ CompilationError err (Context (envCurrentPath env) area (envBacktrace env))
              )
            return $ Slv.Solved t area $ Slv.ListItem elem

        _ -> Slv.Solved t area . Slv.ListItem <$> updatePlaceholders env push s e

    Slv.ListSpread e -> Slv.Solved t area . Slv.ListSpread <$> updatePlaceholders env push s e

  updateField :: Substitution -> Slv.Field -> Infer Slv.Field
  updateField s (Slv.Solved t area field) = case field of
    Slv.Field       (n, e) -> Slv.Solved t area . Slv.Field . (n, ) <$> updatePlaceholders env push s e
    Slv.FieldSpread e      -> Slv.Solved t area . Slv.FieldSpread <$> updatePlaceholders env push s e



isMethod :: Env -> Slv.Exp -> Bool
isMethod env (Slv.Solved _ _ e) = case e of
  Slv.Var n -> Just True == (M.lookup n (envMethods env) >> return True)
  _         -> False
