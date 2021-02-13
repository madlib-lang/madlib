{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Exp where

import qualified Data.Map                      as M
import           Control.Monad.Except
import           Data.Foldable                  ( foldrM )
import qualified Parse.AST                     as AST
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Substitute
import           Infer.Unify
import           Infer.Instantiate
import           Error.Error
import           Explain.Reason
import           Explain.Location
import           Utils.Tuple
import           Data.List                      ( (\\)
                                                , union
                                                , partition
                                                )
import           Infer.Scheme                   ( quantify
                                                , toScheme
                                                )
import qualified Utils.Tuple                   as T
import           Infer.Pattern
import           Infer.Pred
import           Infer.Placeholder


infer :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer env lexp = do
  let (Can.Canonical area exp) = lexp
  r@(s, ps, t, e) <- case exp of
    Can.LNum _ -> return (M.empty, [], tNumber, applyLitSolve lexp tNumber)
    Can.LStr _ -> return (M.empty, [], tStr, applyLitSolve lexp tStr)
    Can.LBool _ -> return (M.empty, [], tBool, applyLitSolve lexp tBool)
    Can.LUnit -> return (M.empty, [], tUnit, applyLitSolve lexp tUnit)
    Can.TemplateString _   -> inferTemplateString env lexp

    Can.Var            _   -> inferVar env lexp
    Can.Abs _ _            -> inferAbs env lexp
    Can.App{}              -> inferApp env lexp
    Can.Assignment _ _     -> inferAssignment env lexp
    Can.Where      _ _     -> inferWhere env lexp
    Can.Record _           -> inferRecord env lexp
    Can.FieldAccess _ _    -> inferFieldAccess env lexp
    Can.NamespaceAccess _  -> inferNamespaceAccess env lexp
    Can.TypedExp _ _       -> inferTypedExp env lexp
    Can.ListConstructor  _ -> inferListConstructor env lexp
    Can.TupleConstructor _ -> inferTupleConstructor env lexp
    Can.Export           _ -> inferExport env lexp
    Can.If{}               -> inferIf env lexp
    Can.JSExp c            -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Solved t area (Slv.JSExp c))

  return r


applyLitSolve :: Can.Exp -> Type -> Slv.Exp
applyLitSolve (Can.Canonical area exp) t = case exp of
  Can.LNum  v -> Slv.Solved t area $ Slv.LNum v
  Can.LStr  v -> Slv.Solved t area $ Slv.LStr v
  Can.LBool v -> Slv.Solved t area $ Slv.LBool v
  Can.LUnit   -> Slv.Solved t area Slv.LUnit

applyAbsSolve :: Can.Exp -> Slv.Name -> [Slv.Exp] -> Type -> Slv.Exp
applyAbsSolve (Can.Canonical loc _) param body t =
  Slv.Solved t loc $ Slv.Abs param body

applyAssignmentSolve :: Can.Exp -> Slv.Name -> Slv.Exp -> Type -> Slv.Exp
applyAssignmentSolve (Can.Canonical loc _) n exp t =
  Slv.Solved t loc $ Slv.Assignment n exp


updateType :: Slv.Exp -> Type -> Slv.Exp
updateType (Slv.Solved _ a e) t' = Slv.Solved t' a e


updatePattern :: Can.Pattern -> Slv.Pattern
updatePattern (Can.Canonical _ pat) = case pat of
  Can.PVar name             -> Slv.PVar name
  Can.PAny                  -> Slv.PAny
  Can.PCtor name patterns   -> Slv.PCtor name (updatePattern <$> patterns)
  Can.PNum    n             -> Slv.PNum n
  Can.PStr    n             -> Slv.PStr n
  Can.PBool   n             -> Slv.PBool n
  Can.PCon    n             -> Slv.PCon n
  Can.PRecord fieldPatterns -> Slv.PRecord (updatePattern <$> fieldPatterns)
  Can.PList   patterns      -> Slv.PList (updatePattern <$> patterns)
  Can.PTuple  patterns      -> Slv.PTuple (updatePattern <$> patterns)
  Can.PSpread pat'          -> Slv.PSpread (updatePattern pat')



-- INFER VAR

inferVar :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s =
          Forall [Star]
            $   []
            :=> (TRecord (M.fromList [(name, TGen 0)]) True `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (M.empty, ps, t, Slv.Solved t area $ Slv.Var n)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    (ps :=> t) <- instantiate sc

    let e = Slv.Solved t area $ Slv.Var n
    e' <- insertVarPlaceholders env e ps

    return (M.empty, ps, t, e')

enhanceVarError :: Env -> Can.Exp -> Area -> InferError -> Infer Scheme
enhanceVarError env exp area (InferError e _) = throwError
  $ InferError e (Reason (VariableNotDeclared exp) (envcurrentpath env) area)



-- INFER ABSTRACTIONS

inferAbs :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAbs env l@(Can.Canonical _ (Can.Abs param body)) = do
  tv <- newTVar Star
  let env' = extendVars env (param, Forall [] ([] :=> tv))
  (s, ps, t, es) <- inferBody env' body
  let t' = apply s (tv `fn` t)
  return (s, apply s ps, t', applyAbsSolve l param es t')


inferBody :: Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody env [e     ] = (\(s, ps, t, e) -> (s, ps, t, [e])) <$> infer env e

inferBody env (e : xs) = do
  (_, _ , t   , _ ) <- infer env e
  (s, ps, env', e') <- case e of
    Can.Canonical _ (Can.TypedExp _ _) -> inferExplicitlyTyped env e
    _ -> inferImplicitlyTyped True env e

  e''  <- insertClassPlaceholders env e' ps
  e''' <- updatePlaceholders env s e''

  let exp = Slv.extractExp e''
  let env'' = case exp of
        Slv.Assignment name _ ->
          extendVars env (name, Forall [kind t] (ps :=> t))

        Slv.TypedExp (Slv.Solved _ _ (Slv.Assignment name _)) _ ->
          extendVars env (name, Forall [kind t] (ps :=> t))

        _ -> env

  (\(sb, ps', tb, eb) -> (sb `compose` s, ps `union` ps', tb, e' : eb))
    <$> inferBody env'' xs



-- INFER APP

inferApp :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp env (Can.Canonical area (Can.App abs arg final)) = do
  tv                  <- newTVar Star
  (s1, ps1, t1, eabs) <- infer env abs
  (s2, ps2, t2, earg) <- infer env arg

  s3                  <- catchError
    (unify (apply s2 t1) (t2 `fn` tv))
    (\case
      InferError (UnificationError _ _) _ ->
        throwError
          $ InferError (UnificationError (apply s2 t1) (t2 `fn` tv))
          $ Reason (WrongTypeApplied abs arg)
                   (envcurrentpath env)
                   (Can.getArea arg)
      InferError e _ -> throwError $ InferError e $ Reason
        (WrongTypeApplied abs arg)
        (envcurrentpath env)
        (Can.getArea arg)
    )

  let t = apply s3 tv

  let solved =
        Slv.Solved t area $ Slv.App eabs (updateType earg $ apply s3 t2) final

  return (s3 `compose` s2 `compose` s1, ps1 ++ ps2, t, solved)



-- INFER TEMPLATE STRINGS

inferTemplateString
  :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTemplateString env e@(Can.Canonical area (Can.TemplateString exps)) = do
  inferred <- mapM (infer env) exps

  let elemSubsts = (\(s, _, _, _) -> s) <$> inferred
  let elemTypes  = (\(_, _, t, _) -> t) <$> inferred
  let elemExps   = (\(_, _, _, es) -> es) <$> inferred
  let elemPS     = (\(_, ps, _, _) -> ps) <$> inferred

  ss <- mapM (`unify` tStr) elemTypes

  let fullSubst = foldl compose M.empty (elemSubsts <> ss)

  let updatedExp = Slv.Solved
        tStr
        area
        (Slv.TemplateString
          (   (\(t, e) -> updateType e (apply fullSubst t))
          <$> zip elemTypes elemExps
          )
        )

  return (fullSubst, concat elemPS, tStr, updatedExp)



-- INFER ASSIGNMENT

inferAssignment :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAssignment env e@(Can.Canonical _ (Can.Assignment name exp)) = do
  t <- newTVar Star
  let env' = extendVars env (name, Forall [] ([] :=> t))
  (s1, ps1, t1, e1) <- infer env' exp
  return (s1, ps1, t1, applyAssignmentSolve e name e1 t1)



-- INFER EXPORT

inferExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer env exp
  return (s, ps, t, Slv.Solved t area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor
  :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor env (Can.Canonical area (Can.ListConstructor elems)) =
  case elems of
    [] -> do
      tv <- newTVar Star
      let t = TApp (TCon (TC "List" $ Kfun Star Star)) tv
      return (M.empty, [], t, Slv.Solved t area (Slv.ListConstructor []))

    elems -> do
      inferred <- mapM (inferListItem env) elems
      let (_, _, t1, _) = head inferred
      s <- unifyElems env ((\(_, _, t, _) -> t) <$> inferred)
      let s' = foldr (<>) M.empty ((\(s, _, _, _) -> s) <$> inferred)
      let t  = TApp (TCon (TC "List" $ Kfun Star Star)) (apply s t1)
      let ps = concat $ (\(_, x, _, _) -> x) <$> inferred
      return
        ( s `compose` s'
        , ps
        , t
        , Slv.Solved
          t
          area
          (Slv.ListConstructor ((\(_, _, _, es) -> es) <$> inferred))
        )


inferListItem
  :: Env -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem env li = case li of
  Can.ListItem exp -> do
    (s, ps, t, e) <- infer env exp
    return (s, ps, t, Slv.ListItem e)

  Can.ListSpread exp -> do
    (s, ps, t, e) <- infer env exp
    case t of
      TApp (TCon (TC "List" _)) t' -> return (s, ps, t', Slv.ListSpread e)

      TVar _ -> return (s, ps, t, Slv.ListSpread e)

      _ -> throwError $ InferError (WrongSpreadType $ show t) NoReason



-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor
  :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor env (Can.Canonical area (Can.TupleConstructor elems)) =
  do
    inferredElems <- mapM (infer env) elems
    let elemSubsts = (\(s, _, _, _) -> s) <$> inferredElems
    let elemTypes  = (\(_, _, t, _) -> t) <$> inferredElems
    let elemEXPS   = (\(_, _, _, es) -> es) <$> inferredElems
    let elemPS     = (\(_, ps, _, _) -> ps) <$> inferredElems

    let s          = foldr compose M.empty elemSubsts

    let tupleT     = getTupleCtor (length elems)
    let t          = foldl TApp tupleT elemTypes

    return
      (s, concat elemPS, t, Slv.Solved t area (Slv.TupleConstructor elemEXPS))



-- INFER RECORD

inferRecord :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  inferred <- mapM (inferRecordField env) fields
  open     <- shouldBeOpen env fields
  let inferredFields = lst <$> inferred
      subst          = foldr compose M.empty (beg <$> inferred)
      recordType     = TRecord (M.fromList $ concat $ mid <$> inferred) open
  return
    ( subst
    , []
    , recordType
    , Slv.Solved recordType area (Slv.Record inferredFields)
    )

inferRecordField
  :: Env -> Can.Field -> Infer (Substitution, [(Slv.Name, Type)], Slv.Field)
inferRecordField env field = case field of
  Can.Field (name, exp) -> do
    (s, ps, t, e) <- infer env exp
    return (s, [(name, t)], Slv.Field (name, e))

  Can.FieldSpread exp -> do
    (s, ps, t, e) <- infer env exp
    case t of
      TRecord tfields _ -> return (s, M.toList tfields, Slv.FieldSpread e)

      TVar _ -> return (s, [], Slv.FieldSpread e)

      _ -> throwError $ InferError (WrongSpreadType $ show t) NoReason

shouldBeOpen :: Env -> [Can.Field] -> Infer Bool
shouldBeOpen env = foldrM
  (\field r -> case field of
    Can.Field       _ -> return r
    Can.FieldSpread e -> do
      (_, _, t, _) <- infer env e
      case t of
        TRecord _ _ -> return r
        TVar _      -> return True
  )
  False



-- INFER NAMESPACE ACCESS

inferNamespaceAccess
  :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNamespaceAccess env e@(Can.Canonical area (Can.NamespaceAccess var)) = do
  sc         <- catchError (lookupVar env var) (enhanceVarError env e area)
  (ps :=> t) <- instantiate sc

  let e = Slv.Solved t area $ Slv.NamespaceAccess var
  e' <- insertVarPlaceholders env e ps

  return (M.empty, ps, t, e')



-- INFER FIELD ACCESS

inferFieldAccess
  :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess env (Can.Canonical area (Can.FieldAccess rec@(Can.Canonical _ re) abs@(Can.Canonical _ (Can.Var ('.' : name)))))
  = do
    (fieldSubst , fieldPs , fieldType , fieldExp ) <- infer env abs
    (recordSubst, recordPs, recordType, recordExp) <- infer env rec

    let foundFieldType = case recordType of
          TRecord fields _ -> M.lookup name fields
          _                -> Nothing

    case foundFieldType of
      Just t -> do
        (ps :=> t') <- instantiate $ Forall [kind t] ([] :=> t)
        let solved = Slv.Solved t' area (Slv.FieldAccess recordExp fieldExp)
        return (fieldSubst, ps, t', solved)

      Nothing -> case recordType of
        TRecord _ False -> throwError $ InferError
          (FieldNotExisting name)
          (SimpleReason (envcurrentpath env) area)
        _ -> do
          tv <- newTVar Star
          s3 <- unify (apply recordSubst fieldType) (recordType `fn` tv)

          let s          = compose s3 recordSubst
          let t          = apply s tv

          let recordExp' = updateType recordExp (apply s3 recordType)
          let solved = Slv.Solved t area (Slv.FieldAccess recordExp' fieldExp)

          return (s, [], t, solved)



-- INFER IF

inferIf :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf env exp@(Can.Canonical area (Can.If cond truthy falsy)) = do
  (s1, ps1, tcond, econd) <- infer env cond
  (s2, ps2, ttruthy, etruthy) <- infer env truthy
  (s3, ps3, tfalsy, efalsy) <- infer env falsy

  s4 <- catchError (unify tBool tcond) (addConditionReason env exp cond area)
  s5 <- catchError (unify ttruthy tfalsy) (addBranchReason env exp falsy area)

  let t = apply s5 ttruthy

  return
    ( s1 `compose` s2 `compose` s3 `compose` s4 `compose` s5
    , ps1 ++ ps2 ++ ps3
    , t
    , Slv.Solved t area (Slv.If econd etruthy efalsy)
    )

addConditionReason
  :: Env -> Can.Exp -> Can.Exp -> Area -> InferError -> Infer Substitution
addConditionReason env ifExp condExp area (InferError e _) =
  throwError $ InferError
    e
    (Reason (IfElseCondIsNotBool ifExp condExp) (envcurrentpath env) area)

addBranchReason
  :: Env -> Can.Exp -> Can.Exp -> Area -> InferError -> Infer Substitution
addBranchReason env ifExp falsyExp area (InferError e _) =
  throwError $ InferError
    e
    (Reason (IfElseBranchTypesDontMatch ifExp falsyExp)
            (envcurrentpath env)
            area
    )



-- INFER WHERE

inferWhere :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e) <- infer env exp
  tv            <- newTVar Star
  pss           <- mapM (inferBranch env tv t) iss

  let ps'             = concat $ T.mid <$> pss

  let issSubstitution = foldr1 compose $ (beg <$> pss) <> [s]

  s' <- unifyElems env (Slv.getType . lst <$> pss)

  let s'' = compose s' issSubstitution

  let iss =
        (\(Slv.Solved t a is) -> Slv.Solved (apply s'' t) a is) . lst <$> pss
  let wher = Slv.Solved (apply s'' tv) area
        $ Slv.Where (updateType e (apply s'' t)) iss
  return (s'', ps ++ ps', apply s'' tv, wher)


inferBranch
  :: Env -> Type -> Type -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch env tv t (Can.Canonical area (Can.Is pat exp)) = do
  (ps, vars, t') <- inferPattern env pat
  s              <- catchError
    (unify t (removeSpread t'))
    (\(InferError e _) -> throwError $ InferError
      e
      (Reason (PatternTypeError exp pat) (envcurrentpath env) area)
    )

  (s', ps', t'', e') <- infer (mergeVars env vars) exp
  s''                <- catchError
    (unify tv t'')
    (\(InferError e _) -> throwError $ InferError
      e
      (Reason (PatternTypeError exp pat) (envcurrentpath env) area)
    )

  let t''' = extendRecord (s <> s'' <> s') t'
  s''' <- catchError
    (unify t t''')
    (\(InferError e _) -> throwError $ InferError
      e
      (Reason (PatternTypeError exp pat) (envcurrentpath env) area)
    )

  let subst = s `compose` s' `compose` s'' `compose` s'''

  return
    ( subst
    , ps ++ ps'
    , Slv.Solved (apply subst (t''' `fn` t'')) area
      $ Slv.Is (updatePattern pat) (updateType e' (apply subst t''))
    )

removeSpread :: Type -> Type
removeSpread t = case t of
  TRecord fs o -> TRecord (M.filterWithKey (\k _ -> k /= "...") fs) o
  _            -> t

extendRecord :: Substitution -> Type -> Type
extendRecord s t = case t of
  TRecord fs _ ->
    let spread = M.lookup "..." fs
        fs'    = M.map (extendRecord s) fs
    in  case spread of
          Just (TVar tv) -> case M.lookup tv s of
            Just t' -> do
              case extendRecord s t' of
                TRecord fs'' _ -> TRecord
                  (M.filterWithKey (\k _ -> k /= "...") (fs' <> fs''))
                  True
                _ -> t
            _ -> t
          _ -> t

  _ -> t



-- INFER TYPEDEXP

inferTypedExp :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp env (Can.Canonical area (Can.TypedExp exp sc)) = do
  infer env exp
  (ps :=> t)        <- instantiate sc

  (s1, ps1, t1, e1) <- infer env exp
  s2                <- catchError
    (unify t t1)
    (\(InferError e _) ->
      throwError $ InferError e (SimpleReason (envcurrentpath env) area)
    )

  return
    ( s1 `compose` s2
    , ps
    , t
    , Slv.Solved t area (Slv.TypedExp (updateType e1 t) sc)
    )



type Ambiguity = (TVar, [Pred])

ambiguities :: [TVar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [ (v, filter (elem v . ftv) ps) | v <- ftv ps \\ vs ]

split :: Env -> [TVar] -> [TVar] -> [Pred] -> Infer ([Pred], [Pred])
split env fs gs ps = do
  ps' <- reduce env ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  let as       = ambiguities (fs ++ gs) rs
  if not (null as)
    then throwError $ InferError (AmbiguousType (head as)) NoReason
    else return (ds, rs)


inferImplicitlyTyped
  :: Bool -> Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferImplicitlyTyped isLet env exp@(Can.Canonical area _) = do
  tv <- newTVar Star

  let env' = case Can.getExpName exp of
        Just n  -> extendVars env (n, toScheme tv)
        Nothing -> env

  (s, ps, t, e) <- infer env' exp
  s'            <- unify t tv
  let s'' = s `compose` s'
      ps' = apply s'' ps
      t'  = apply s'' tv
      fs  = ftv (apply s'' env)
      vs  = ftv t'
      gs  = vs \\ fs
  (ds, rs) <- catchError
    (split env' fs vs ps')
    (\(InferError e _) ->
      throwError $ InferError e (SimpleReason (envcurrentpath env) area)
    )

  if not isLet && not (null (rs ++ ds)) && not (Can.isAssignment exp)
    then throwError $ InferError
      (AmbiguousType (TV "err" Star, [IsIn "n" [t']]))
      (SimpleReason (envcurrentpath env) area)
    else return ""

  case Can.getExpName exp of
    Just n  -> return (s'', ps', extendVars env' (n, Forall [] $ ps' :=> t'), e)

    Nothing -> return (s'', ps', env', e)


inferExplicitlyTyped
  :: Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped env e@(Can.Canonical area (Can.TypedExp exp sc)) = do
  qt@(qs :=> t') <- instantiate sc

  let env' = case Can.getExpName exp of
        Just n  -> extendVars env (n, sc)
        Nothing -> env

  (s, ps, t, e) <- infer env' exp
  s'            <- (`compose` s) <$> unify t' t

  let qs' = apply s' qs
      t'' = apply s' t'
      fs  = ftv (apply s' env)
      gs  = ftv t'' \\ fs
      sc' = quantify gs (qs' :=> t'')
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' ps)
  (ds, rs) <- catchError
    (split env' fs gs ps')
    (\(InferError e _) ->
      throwError $ InferError e (SimpleReason (envcurrentpath env) area)
    )

  qs'' <- getAllParentPreds env qs'

  if sc /= sc'
    then throwError $ InferError (SignatureTooGeneral sc sc')
                                 (SimpleReason (envcurrentpath env') area)
    else if not (null rs)
      then throwError
        $ InferError ContextTooWeak (SimpleReason (envcurrentpath env) area)
      else do
        let e'   = updateType e t''

        let qt   = ps :=> t
        let qt'  = qs'' :=> apply s' t
        let sc'' = quantify (ftv qt') qt'
        let env'' = case Can.getExpName exp of
              Just n  -> extendVars env' (n, sc'')
              Nothing -> env'

        return (s', qs'', env'', Slv.Solved t'' area (Slv.TypedExp e' sc))


inferExps :: Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps env []       = return ([], env)

inferExps env (e : es) = do
  (s, ps, env', e') <- upgradeReason env (Can.getArea e) $ case e of
    Can.Canonical _ (Can.TypedExp _ _) -> inferExplicitlyTyped env e
    _ -> inferImplicitlyTyped False env e

  e''            <- insertClassPlaceholders env e' ps
  e'''           <- updatePlaceholders env s e''

  (es', nextEnv) <- inferExps env' es

  return (e''' : es', nextEnv)



upgradeReason :: Env -> Area -> Infer a -> Infer a
upgradeReason env area a = catchError
  a
  (\case
    (InferError e NoReason) ->
      throwError $ InferError e $ SimpleReason (envcurrentpath env) area
    (InferError e r) -> throwError $ InferError e r
  )
