{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Exp where

import qualified Data.Map                      as M
import           Control.Monad.Except
import           Data.Foldable                  ( foldrM
                                                , foldlM
                                                )
import qualified Parse.Madlib.AST              as AST
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Substitute
import           Infer.Unify
import           Infer.Instantiate
import           Error.Error
import           Error.Context
import           Explain.Location
import           Utils.Tuple
import           Data.List                      ( (\\)
                                                , union
                                                , partition
                                                , foldl'
                                                )
import           Infer.Scheme                   ( quantify
                                                , toScheme
                                                )
import qualified Utils.Tuple                   as T
import           Infer.Pattern
import           Infer.Pred
import           Infer.Placeholder
import qualified Control.Monad                 as CM
import           Text.Show.Pretty               ( ppShow )
import           Debug.Trace
import           Infer.ToSolved


infer :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer env lexp = do
  let (Can.Canonical area exp) = lexp
      env'                     = pushExpToBT env lexp
  case exp of
    Can.LNum  _               -> return (M.empty, [], tNumber, applyLitSolve lexp tNumber)
    Can.LStr  _               -> return (M.empty, [], tStr, applyLitSolve lexp tStr)
    Can.LBool _               -> return (M.empty, [], tBool, applyLitSolve lexp tBool)
    Can.LUnit                 -> return (M.empty, [], tUnit, applyLitSolve lexp tUnit)
    Can.TemplateString _      -> inferTemplateString env' lexp

    Can.Var            _      -> inferVar env' lexp
    Can.Abs _ _               -> inferAbs env' lexp
    Can.App{}                 -> inferApp env' lexp
    Can.Assignment _ _        -> inferAssignment env' lexp
    Can.Where      _ _        -> inferWhere env' lexp
    Can.Record _              -> inferRecord env' lexp
    Can.Access   _ _          -> inferAccess env' lexp
    Can.TypedExp _ _          -> inferTypedExp env' lexp
    Can.ListConstructor  _    -> inferListConstructor env' lexp
    Can.TupleConstructor _    -> inferTupleConstructor env' lexp
    Can.Export           _    -> inferExport env' lexp
    Can.NameExport       name -> inferNameExport env' lexp
    Can.If{}                  -> inferIf env' lexp
    Can.JSExp c               -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Solved t area (Slv.JSExp c))


applyLitSolve :: Can.Exp -> Type -> Slv.Exp
applyLitSolve (Can.Canonical area exp) t = case exp of
  Can.LNum  v -> Slv.Solved t area $ Slv.LNum v
  Can.LStr  v -> Slv.Solved t area $ Slv.LStr v
  Can.LBool v -> Slv.Solved t area $ Slv.LBool v
  Can.LUnit   -> Slv.Solved t area Slv.LUnit

applyAbsSolve :: Can.Exp -> Slv.Solved Slv.Name -> [Slv.Exp] -> Type -> Slv.Exp
applyAbsSolve (Can.Canonical loc _) param body t = Slv.Solved t loc $ Slv.Abs param body

applyAssignmentSolve :: Can.Exp -> Slv.Name -> Slv.Exp -> Type -> Slv.Exp
applyAssignmentSolve (Can.Canonical loc _) n exp t = Slv.Solved t loc $ Slv.Assignment n exp


updateType :: Slv.Exp -> Type -> Slv.Exp
updateType (Slv.Solved _ a e) t' = Slv.Solved t' a e


updatePattern :: Type -> Can.Pattern -> Slv.Pattern
updatePattern t (Can.Canonical area pat) = case pat of
  Can.PVar name             -> Slv.Solved t area $ Slv.PVar name
  Can.PAny                  -> Slv.Solved t area Slv.PAny
  Can.PCtor name patterns   -> Slv.Solved t area $ Slv.PCtor name (updatePattern t <$> patterns)
  Can.PNum    n             -> Slv.Solved t area $ Slv.PNum n
  Can.PStr    n             -> Slv.Solved t area $ Slv.PStr n
  Can.PBool   n             -> Slv.Solved t area $ Slv.PBool n
  Can.PCon    n             -> Slv.Solved t area $ Slv.PCon n
  Can.PRecord fieldPatterns -> Slv.Solved t area $ Slv.PRecord (updatePattern t <$> fieldPatterns)
  Can.PList   patterns      -> Slv.Solved t area $ Slv.PList (updatePattern t <$> patterns)
  Can.PTuple  patterns      -> Slv.Solved t area $ Slv.PTuple (updatePattern t <$> patterns)
  Can.PSpread pat'          -> Slv.Solved t area $ Slv.PSpread (updatePattern t pat')



-- INFER VAR

inferVar :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s = Forall [Star] $ [] :=> (TRecord (M.fromList [(name, TGen 0)]) True `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (M.empty, ps, t, Slv.Solved t area $ Slv.Var n)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    (ps :=> t) <- instantiate sc

    let e = Slv.Solved t area $ Slv.Var n
    e' <- insertVarPlaceholders env e ps

    return (M.empty, ps, t, e')

enhanceVarError :: Env -> Can.Exp -> Area -> CompilationError -> Infer Scheme
enhanceVarError env exp area (CompilationError e _) =
  throwError $ CompilationError e (Context (envCurrentPath env) area (envBacktrace env))



-- INFER NAME EXPORT

inferNameExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNameExport env exp@(Can.Canonical area (Can.NameExport name)) = do
  sc         <- catchError (lookupVar env name) (enhanceVarError env exp area)
  (ps :=> t) <- instantiate sc

  let e = Slv.Solved t area $ Slv.NameExport name

  return (M.empty, ps, t, e)



-- INFER ABSTRACTIONS

-- Param white list for shadowing check
allowedShadows :: [String]
allowedShadows = ["_P_", "__x__", "_"]

extendAbsEnv :: Env -> Type -> Can.Canonical Can.Name -> Infer Env
extendAbsEnv env tv (Can.Canonical area param) = if param `elem` allowedShadows
  then return $ extendVars env (param, Forall [] ([] :=> tv))
  else catchError
    (safeExtendVars env (param, Forall [] ([] :=> tv)))
    (((const $ extendVars env (param, Forall [] ([] :=> tv))) <$>) . pushError . upgradeContext' env area)

inferAbs :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAbs env l@(Can.Canonical _ (Can.Abs p@(Can.Canonical area param) body)) = do
  tv             <- newTVar Star

  env'           <- extendAbsEnv env tv p
  (s, ps, t, es) <- inferBody env' body
  let t'        = apply s (tv `fn` t)
      paramType = apply s tv
  return (s, ps, t', applyAbsSolve l (Slv.Solved paramType area param) es t')


inferBody :: Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody env [e] = do
  (s, ps, t, e) <- infer env e
  return (s, ps, t, [e])

inferBody env (e : xs) = do
  (s, ps, env', e') <- case e of
    Can.Canonical _ (Can.TypedExp _ _) -> inferExplicitlyTyped env e
    _ -> do
      (s, (ds, ps), env, e') <- inferImplicitlyTyped True env e
      return (s, ps, env, e')

  e''               <- insertClassPlaceholders env' e' ps
  e'''              <- updatePlaceholders env' True s e''

  (sb, ps', tb, eb) <- inferBody (updateBodyEnv s env') xs
  return (sb `compose` s, ps', tb, e''' : eb)

-- Applies a substitution only to types in the env that are not a function.
-- This is needed for function bodies, so that we can define a function that is generic,
-- but other types should not. Say if we have a var xs = [] that has type List a and that
-- later in the function we can infer that this list is a List Number, then that substitution
-- should be applied to the env so that correct type can be inferred.
updateBodyEnv :: Substitution -> Env -> Env
updateBodyEnv s e =
  e { envVars = M.map (\sc@(Forall _ (_ :=> t)) -> if isFunctionType t then sc else apply s sc) (envVars e) }




-- INFER APP

inferApp' :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp, [(Substitution, [Pred], Type)])
inferApp' env app@(Can.Canonical area (Can.App abs arg final)) = do
  tv                               <- newTVar Star
  (s1, ps1, t1, eabs, skippedArgs) <- if isApp abs && not (isFinalApp abs)
    then inferApp' env abs
    else do
      (s1, ps1, t1, eabs) <- infer env abs
      return (s1, ps1, t1, eabs, [])
  (s2, ps2, t2, earg) <- infer (apply (removeRecordTypes s1) env) arg

  s3                  <- contextualUnify env app t1 (apply s1 t2 `fn` tv)

  let t          = apply s3 tv
  let s          = s3 `compose` s2 `compose` s1

  let solved = Slv.Solved (apply s t) area $ Slv.App eabs (updateType earg $ apply s t2) final

  let skippedArg = [ (s2, ps2, apply s t2) | isPlaceholder arg ]

  return (s, ps1 ++ ps2, t, solved, skippedArg <> skippedArgs)

inferApp :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp env app@(Can.Canonical area (Can.App abs arg final)) = do
  (s, ps, t, e, skipped) <- inferApp' env app
  let subst        = foldr compose s $ T.beg <$> skipped
  let preds        = concat (T.mid <$> skipped) <> ps
  let skippedTypes = T.lst <$> skipped
  let realType     = apply subst $ foldr fn t skippedTypes

  return (subst, preds, realType, updateType e realType)


isPlaceholder :: Can.Exp -> Bool
isPlaceholder (Can.Canonical _ exp) = exp == Can.Var "$"

isApp :: Can.Exp -> Bool
isApp (Can.Canonical _ exp) = case exp of
  Can.App{} -> True
  _         -> False

isFinalApp :: Can.Exp -> Bool
isFinalApp (Can.Canonical _ exp) = case exp of
  Can.App _ _ final -> final
  _                 -> False



-- INFER TEMPLATE STRINGS

inferTemplateString :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTemplateString env e@(Can.Canonical area (Can.TemplateString exps)) = do
  inferred <- mapM (infer env) exps

  let elemSubsts = (\(s, _, _, _) -> s) <$> inferred
  let elemTypes  = (\(_, _, t, _) -> t) <$> inferred
  let elemExps   = (\(_, _, _, es) -> es) <$> inferred
  let elemPS     = (\(_, ps, _, _) -> ps) <$> inferred

  ss <- mapM (\(exp, t) -> contextualUnify env exp t tStr) (zip exps elemTypes)

  let fullSubst = foldl' compose M.empty (elemSubsts <> ss)

  let updatedExp = Slv.Solved
        tStr
        area
        (Slv.TemplateString ((\(t, e) -> updateType e (apply fullSubst t)) <$> zip elemTypes elemExps))

  return (fullSubst, concat elemPS, tStr, updatedExp)



-- INFER ASSIGNMENT

inferAssignment :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAssignment env e@(Can.Canonical _ (Can.Assignment name exp)) = do
  currentScheme <- case M.lookup name (envVars env) of
    Just sc -> return sc
    _       -> (\t -> Forall [] ([] :=> t)) <$> newTVar Star

  (currentPreds :=> currentType) <- instantiate currentScheme
  let env' = extendVars env (name, currentScheme)
  (s1, ps1, t1, e1) <- infer env' exp
  s2                <- contextualUnify env' exp currentType t1
  return (s2 `compose` s1, currentPreds ++ ps1, apply s2 t1, applyAssignmentSolve e name e1 t1)



-- INFER EXPORT

inferExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer env exp
  return (s, ps, t, Slv.Solved t area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor env (Can.Canonical area (Can.ListConstructor elems)) = case elems of
  [] -> do
    tv <- newTVar Star
    let t = TApp (TCon (TC "List" (Kfun Star Star)) "prelude") tv
    return (M.empty, [], t, Slv.Solved t area (Slv.ListConstructor []))

  elems -> do
    tv               <- newTVar Star

    (s', ps, ts, es) <- foldlM
      (\(s, pss, ts, lis) elem -> do
        (s', ps', t, li) <- inferListItem (apply s env) tv elem
        return (s `compose` s', pss ++ ps', ts ++ [t], lis ++ [li])
      )
      (mempty, [], [], [])
      elems

    s <- contextualUnifyElems env (zip elems ts)
    let s'' = s `compose` s'

    let t = TApp (TCon (TC "List" (Kfun Star Star)) "prelude") (apply s'' tv)

    return (s'', ps, t, Slv.Solved t area (Slv.ListConstructor es))


inferListItem :: Env -> Type -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem env ty (Can.Canonical area li) = case li of
  Can.ListItem exp -> case exp of
    Can.Canonical _ (Can.JSXExpChild exp') -> do
      (s1, ps, t, e) <- infer env exp'

      case t of
        (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") (TCon (TC "String" Star) "prelude")) -> do
          let exp'' = Can.Canonical area $ Can.App
                (Can.Canonical
                  area
                  (Can.App (Can.Canonical area (Can.JSExp "((f) => (xs) => xs.map(f))"))
                           (Can.Canonical area (Can.Var "text"))
                           False
                  )
                )
                exp'
                True
          (s1, ps, t, e) <- infer env exp''
          s2             <- unify t (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") ty)
          let s = s1 `compose` s2
          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListSpread e)

        (TApp (TVar (TV _ (Kfun Star Star))) (TCon (TC "String" Star) "prelude")) -> do
          let exp'' = Can.Canonical area $ Can.App
                (Can.Canonical
                  area
                  (Can.App (Can.Canonical area (Can.JSExp "((f) => (xs) => xs.map(f))"))
                           (Can.Canonical area (Can.Var "text"))
                           False
                  )
                )
                exp'
                True
          (s1, ps, t, e) <- infer env exp''
          s2             <- unify t (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") ty)
          let s = s1 `compose` s2
          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListSpread e)

        (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") t') -> do
          s2 <- unify t (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") ty)
          let s = s1 `compose` s2
          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListSpread e)

        (TApp (TVar (TV _ (Kfun Star Star))) t') -> do
          s2 <- unify t (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") ty)
          let s = s1 `compose` s2
          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListSpread e)

        TCon (TC "String" Star) "prelude" -> do
          let exp'' = Can.Canonical area $ Can.App (Can.Canonical area (Can.Var "text")) exp' True
          (s1, ps, t, e) <- infer env exp''
          s2             <- unify t ty
          let s = s1 `compose` s2

          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListItem e)

        t'@(TVar _) -> do
          let exp'' = Can.Canonical area $ Can.App (Can.Canonical area (Can.Var "__tmp_jsx_children__")) exp' True
          (s1, ps, t, e') <- infer (extendVars env ("__tmp_jsx_children__", Forall [] $ [] :=> (t' `fn` ty))) exp''
          s2              <- unify t ty
          let s = s1 `compose` s2

          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListItem e')

        _ -> do
          s2 <- unify t ty
          let s = s1 `compose` s2
          return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListItem e)

    _ -> do
      (s1, ps, t, e) <- infer env exp
      s2             <- unify t ty
      let s = s1 `compose` s2
      return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListItem e)

  Can.ListSpread exp -> do
    (s1, ps, t, e) <- infer env exp
    s2             <- unify t (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") ty)
    let s = s1 `compose` s2

    return (s, ps, apply s ty, Slv.Solved (apply s ty) area $ Slv.ListSpread e)



-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor env (Can.Canonical area (Can.TupleConstructor elems)) = do
  inferredElems <- mapM (infer env) elems
  let elemSubsts = (\(s, _, _, _) -> s) <$> inferredElems
  let elemTypes  = (\(_, _, t, _) -> t) <$> inferredElems
  let elemEXPS   = (\(_, _, _, es) -> es) <$> inferredElems
  let elemPS     = (\(_, ps, _, _) -> ps) <$> inferredElems

  let s          = foldr compose M.empty elemSubsts

  let tupleT     = getTupleCtor (length elems)
  let t          = foldl' TApp tupleT elemTypes

  return (s, concat elemPS, t, Slv.Solved t area (Slv.TupleConstructor elemEXPS))



-- INFER RECORD

inferRecord :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  inferredFields <- mapM (inferRecordField env) fields
  (open, maybeT) <- shouldBeOpen env fields

  let fieldSubsts = (\(s, _, _, _) -> s) <$> inferredFields
  let fieldTypes  = (\(_, _, t, _) -> t) <$> inferredFields
  let fieldEXPS   = (\(_, _, _, es) -> es) <$> inferredFields
  let fieldPS     = (\(_, ps, _, _) -> ps) <$> inferredFields
  let subst       = foldr compose M.empty fieldSubsts

  (recordType, extraSubst) <- if not open
    then return (TRecord (M.fromList $ concat fieldTypes) open, mempty)
    else do
      let Just t' = maybeT
          t''     = TRecord (M.fromList $ concat fieldTypes) True
      s <- unify t' t''
      return (t', s)

  return (extraSubst `compose` subst, concat fieldPS, recordType, Slv.Solved recordType area (Slv.Record fieldEXPS))



inferRecordField :: Env -> Can.Field -> Infer (Substitution, [Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField env (Can.Canonical area field) = case field of
  Can.Field (name, exp) -> do
    (s, ps, t, e) <- infer env exp
    return (s, ps, [(name, t)], Slv.Solved t area $ Slv.Field (name, e))

  Can.FieldSpread exp -> do
    (s, ps, t, e) <- infer env exp
    case t of
      TRecord tfields _ -> return (s, ps, M.toList tfields, Slv.Solved t area $ Slv.FieldSpread e)

      TVar _ -> return (s, ps, [], Slv.Solved t area $ Slv.FieldSpread e)

      _ -> throwError $ CompilationError (WrongSpreadType $ show t)
                                         (Context (envCurrentPath env) (Can.getArea exp) (envBacktrace env))

shouldBeOpen :: Env -> [Can.Field] -> Infer (Bool, Maybe Type)
shouldBeOpen env = foldrM
  (\(Can.Canonical _ field) (r, _) -> case field of
    Can.Field       _ -> return (r, Nothing)
    Can.FieldSpread e -> do
      (_, _, t, _) <- infer env e
      case t of
        TRecord _ _ -> return (r, Nothing)
        TVar _      -> return (True, Just t)
  )
  (False, Nothing)



-- INFER ACCESS

inferAccess :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAccess env e@(Can.Canonical area (Can.Access ns field)) =
  let inferredFieldAccess = inferFieldAccess env e
  in  catchError inferredFieldAccess (\err -> catchError (inferNamespaceAccess env e) (throwError . const err))



-- INFER NAMESPACE ACCESS

inferNamespaceAccess :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNamespaceAccess env e@(Can.Canonical area (Can.Access (Can.Canonical _ (Can.Var ns)) (Can.Canonical _ (Can.Var field))))
  = do
    sc         <- catchError (lookupVar env (ns <> field)) (enhanceVarError env e area)
    (ps :=> t) <- instantiate sc

    let e = Slv.Solved t area $ Slv.Var (ns <> field)
    e' <- insertVarPlaceholders env e ps

    return (M.empty, ps, t, e')
inferNamespaceAccess env _ = throwError $ CompilationError FatalError NoContext



-- INFER FIELD ACCESS

inferFieldAccess :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess env fa@(Can.Canonical area (Can.Access rec@(Can.Canonical _ re) abs@(Can.Canonical _ (Can.Var ('.' : name)))))
  = do
    (fieldSubst , fieldPs , fieldType , fieldExp ) <- infer env abs
    (recordSubst, recordPs, recordType, recordExp) <- infer env rec

    let foundFieldType = case recordType of
          TRecord fields _ -> M.lookup name fields
          _                -> Nothing

    case foundFieldType of
      Just t -> do
        (ps :=> t') <- instantiate $ Forall [kind t] ([] :=> t)
        let solved = Slv.Solved t' area (Slv.Access recordExp fieldExp)
        return (fieldSubst, ps, t', solved)

      Nothing -> case recordType of
        TRecord _ False ->
          throwError $ CompilationError (FieldNotExisting name) (Context (envCurrentPath env) area (envBacktrace env))
        _ -> do
          tv <- newTVar Star
          s3 <- contextualUnify env fa (apply recordSubst fieldType) (recordType `fn` tv)

          let s          = compose s3 recordSubst
          let t          = apply s tv

          let recordExp' = updateType recordExp (apply s3 recordType)
          let solved = Slv.Solved t area (Slv.Access recordExp' fieldExp)

          return (s, [], t, solved)



-- INFER IF

inferIf :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf env exp@(Can.Canonical area (Can.If cond truthy falsy)) = do
  (s1, ps1, tcond  , econd  ) <- infer env cond
  (s2, ps2, ttruthy, etruthy) <- infer env truthy
  (s3, ps3, tfalsy , efalsy ) <- infer env falsy

  s4                          <- contextualUnify (pushExpToBT env cond) cond tBool tcond
  s5                          <- contextualUnify (pushExpToBT env falsy) falsy ttruthy tfalsy

  let t = apply s5 ttruthy

  return
    ( s1 `compose` s2 `compose` s3 `compose` s4 `compose` s5
    , ps1 ++ ps2 ++ ps3
    , t
    , Slv.Solved t area (Slv.If econd etruthy efalsy)
    )


-- INFER WHERE

inferWhere :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e) <- infer env exp
  tv            <- newTVar Star
  pss           <- mapM (inferBranch env tv t) iss

  let ps'             = concat $ T.mid <$> pss

  let issSubstitution = foldr1 compose $ (beg <$> pss) <> [s]

  s' <- contextualUnifyElems env $ zip iss (apply issSubstitution . Slv.getType . lst <$> pss)

  let s''  = compose s' issSubstitution

  let iss = (\(Slv.Solved t a is) -> Slv.Solved (apply s'' t) a is) . lst <$> pss
  let wher = Slv.Solved (apply s'' tv) area $ Slv.Where (updateType e (apply s'' t)) iss
  return (s'', ps ++ ps', apply s'' tv, wher)


inferBranch :: Env -> Type -> Type -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch env tv t (Can.Canonical area (Can.Is pat exp)) = do
  (ps, vars, t')     <- inferPattern env pat
  s                  <- contextualUnify env exp t t'

  (s', ps', t'', e') <- infer (apply s $ mergeVars env vars) exp
  s''                <- contextualUnify env exp tv t''

  let subst = s `compose` s' `compose` s''

  return
    ( subst
    , ps ++ ps'
    , Slv.Solved (apply subst (t' `fn` t'')) area
      $ Slv.Is (updatePattern (apply subst t') pat) (updateType e' (apply subst t''))
    )




-- INFER TYPEDEXP

inferTypedExp :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp env e@(Can.Canonical area (Can.TypedExp exp sc)) = do
  infer env exp
  (ps :=> t)        <- instantiate sc

  (s1, ps1, t1, e1) <- infer env exp
  s2                <- contextualUnify env e t t1

  return (s1 `compose` s2, ps, t, Slv.Solved t area (Slv.TypedExp (updateType e1 t) sc))



type Ambiguity = (TVar, [Pred])

ambiguities :: [TVar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [ (v, filter (elem v . ftv) ps) | v <- ftv ps \\ vs ]

split :: Env -> [TVar] -> [TVar] -> [Pred] -> Infer ([Pred], [Pred])
split env fs gs ps = do
  ps' <- reduce env ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  let as       = ambiguities (fs ++ gs) rs
  if not (null as) then throwError $ CompilationError (AmbiguousType (head as)) NoContext else return (ds, rs)


inferImplicitlyTyped :: Bool -> Env -> Can.Exp -> Infer (Substitution, ([Pred], [Pred]), Env, Slv.Exp)
inferImplicitlyTyped isLet env exp@(Can.Canonical area _) = do
  tv <- newTVar Star

  let env' = case Can.getExpName exp of
        Just n -> case M.lookup n (envVars env) of
          Just _  -> env
          --  ^ if a var is already present we don't override its type with a fresh var.
          Nothing -> extendVars env (n, toScheme tv)
        Nothing -> env

  (s, ps, t, e) <- infer env' exp
  s'            <- contextualUnify env exp t tv
  let s'' = s `compose` s'
      ps' = apply s'' ps
      t'  = closeRecords $ apply s'' tv
      fs  = ftv (apply s'' env)
      vs  = ftv t'
      gs  = vs \\ fs
  (ds, rs) <- catchError
    (split env' fs vs ps')
    (\(CompilationError e _) -> throwError $ CompilationError e (Context (envCurrentPath env) area (envBacktrace env)))

  CM.when (not isLet && not (null (rs ++ ds)) && not (Can.isAssignment exp)) $ throwError $ CompilationError
    (AmbiguousType (TV "-" Star, rs ++ ds))
    (Context (envCurrentPath env) area (envBacktrace env))

  case Can.getExpName exp of
    Just n  -> return (s'', (ds, ps'), extendVars env' (n, Forall [] $ ps' :=> t'), e)

    Nothing -> return (s'', (ds, ps'), env', e)


inferExplicitlyTyped :: Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped env canExp@(Can.Canonical area (Can.TypedExp exp sc)) = do
  qt@(qs :=> t') <- instantiate sc

  let env' = case Can.getExpName exp of
        Just n  -> extendVars env (n, sc)
        Nothing -> env

  (s, ps, t, e) <- infer env' exp
  s''           <- contextualUnify env canExp t t'
  let s' = s'' `compose` s `compose` s''

  let qs'  = apply s' qs
      t''  = apply s' t
      t''' = closeRecords $ mergeRecords t'' (apply s' t')
      fs   = ftv (apply s' env)
      gs   = ftv t''' \\ fs
      sc'  = quantify gs (qs' :=> t''')
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' ps)
  (ds, rs) <- catchError
    (split env' fs gs ps')
    (\(CompilationError e _) -> throwError $ CompilationError e (Context (envCurrentPath env) area (envBacktrace env)))

  qs'' <- getAllParentPreds env qs'

  if sc /= sc'
    then throwError
      $ CompilationError (SignatureTooGeneral sc sc') (Context (envCurrentPath env') area (envBacktrace env))
    else if not (null rs)
      then throwError $ CompilationError ContextTooWeak (Context (envCurrentPath env) area (envBacktrace env))
      else do
        let e'   = updateType e t'''

        let qt'  = qs'' :=> t'''
        let sc'' = quantify (ftv qt') qt'
        let env'' = case Can.getExpName exp of
              Just n  -> extendVars env' (n, sc'')
              Nothing -> env'

        return (s', qs'', env'', Slv.Solved t''' area (Slv.TypedExp e' sc))


inferExps :: Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps env []       = return ([], env)

inferExps env (e : es) = do
  (e' , env'   ) <- catchError (inferExp (pushExpToBT env e) e) (recordError env e)
  (es', nextEnv) <- inferExps env' es

  case e' of
    Just e'' -> return (e'' : es', nextEnv)
    Nothing  -> return (es', nextEnv)


inferExp :: Env -> Can.Exp -> Infer (Maybe Slv.Exp, Env)
inferExp env (Can.Canonical _ (Can.TypeExport _)) = return (Nothing, env)
inferExp env e = do
  (s, ps, env', e') <- upgradeContext env (Can.getArea e) $ case e of
    Can.Canonical _ (Can.TypedExp _ _) -> inferExplicitlyTyped env e
    _ -> do
      (s, (ds, ps), env, e) <- inferImplicitlyTyped False env e
      return (s, ps, env, e)

  e''  <- insertClassPlaceholders env e' ps
  e''' <- updatePlaceholders env False s e''

  return (Just e''', env')


recordError :: Env -> Can.Exp -> CompilationError -> Infer (Maybe Slv.Exp, Env)
recordError env e err = do
  pushError err
  case Can.getExportNameAndScheme e of
    (Just name, Just sc) -> do
      (_ :=> t) <- instantiate sc
      return (Just $ toSolved e, env)

    (Just name, Nothing) -> do
      tv <- newTVar Star
      return (Just $ toSolved e, env)

    _ -> return (Just $ toSolved e, env)



upgradeContext :: Env -> Area -> Infer a -> Infer a
upgradeContext env area a = catchError a (throwError . upgradeContext' env area)


upgradeContext' :: Env -> Area -> CompilationError -> CompilationError
upgradeContext' env area err = case err of
  (CompilationError e NoContext) -> CompilationError e $ Context (envCurrentPath env) area (envBacktrace env)
  (CompilationError e r        ) -> CompilationError e r
