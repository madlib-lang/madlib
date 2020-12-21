{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Solve where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable                  ( foldlM
                                                , foldrM
                                                )
import qualified Parse.AST                     as AST
import qualified AST.Source                    as Src
import qualified AST.Solved                    as Slv
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Typing
import           Infer.Substitute
import           Infer.Unify
import           Infer.Instantiate
import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Explain.Location
import           Utils.Tuple
import           Data.List                      ( intersect
                                                , (\\)
                                                , union
                                                , partition
                                                , find
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe, catMaybes
                                                )
import           Debug.Trace                    ( trace )
import           Text.Show.Pretty               ( ppShow )
import           Infer.Scheme                   ( quantify
                                                , toScheme
                                                )
import qualified Utils.Tuple                   as T
import           Infer.Pattern
import           Infer.Pred
import           Data.Char                      ( isUpper )
import Infer.Placeholder


infer :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer env lexp = do
  let (Meta _ area exp) = lexp
  r@(s, ps, t, e) <- case exp of
    Src.LNum _ -> return (M.empty, [], tNumber, applyLitSolve lexp tNumber)
    Src.LStr _ -> return (M.empty, [], tStr, applyLitSolve lexp tStr)
    Src.LBool _ -> return (M.empty, [], tBool, applyLitSolve lexp tBool)
    Src.LUnit -> return (M.empty, [], tUnit, applyLitSolve lexp tUnit)

    Src.Var _              -> inferVar env lexp
    Src.Abs _ _            -> inferAbs env lexp
    Src.App{}              -> inferApp env lexp
    Src.Assignment _ _     -> inferAssignment env lexp
    Src.Where      _ _     -> inferWhere env lexp
    Src.Record _           -> inferRecord env lexp
    Src.FieldAccess _ _    -> inferFieldAccess env lexp
    Src.TypedExp    _ _    -> inferTypedExp env lexp
    Src.ListConstructor  _ -> inferListConstructor env lexp
    Src.TupleConstructor _ -> inferTupleConstructor env lexp
    Src.Export           _ -> inferExport env lexp
    Src.If{}               -> inferIf env lexp
    Src.JSExp c            -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Solved t area (Slv.JSExp c))

  return r


applyLitSolve :: Src.Exp -> Type -> Slv.Exp
applyLitSolve (Meta _ area exp) t = case exp of
  Src.LNum  v -> Slv.Solved t area $ Slv.LNum v
  Src.LStr  v -> Slv.Solved t area $ Slv.LStr v
  Src.LBool v -> Slv.Solved t area $ Slv.LBool v
  Src.LUnit   -> Slv.Solved t area $ Slv.LUnit

applyAbsSolve :: Src.Exp -> Slv.Name -> [Slv.Exp] -> Type -> Slv.Exp
applyAbsSolve (Meta _ loc _) param body t =
  Slv.Solved t loc $ Slv.Abs param body

applyAssignmentSolve :: Src.Exp -> Slv.Name -> Slv.Exp -> Type -> Slv.Exp
applyAssignmentSolve (Meta _ loc _) n exp t =
  Slv.Solved t loc $ Slv.Assignment n exp


updateType :: Slv.Exp -> Type -> Slv.Exp
updateType (Slv.Solved _ a e) t' = Slv.Solved t' a e


updatePattern :: Src.Pattern -> Slv.Pattern
updatePattern (Meta _ _ pat) = case pat of
  Src.PVar name             -> Slv.PVar name
  Src.PAny                  -> Slv.PAny
  Src.PCtor name patterns   -> Slv.PCtor name (updatePattern <$> patterns)
  Src.PNum    n             -> Slv.PNum n
  Src.PStr    n             -> Slv.PStr n
  Src.PBool   n             -> Slv.PBool n
  Src.PCon    n             -> Slv.PCon n
  Src.PRecord fieldPatterns -> Slv.PRecord (updatePattern <$> fieldPatterns)
  Src.PList   patterns      -> Slv.PList (updatePattern <$> patterns)
  Src.PTuple  patterns      -> Slv.PTuple (updatePattern <$> patterns)
  Src.PSpread pat'          -> Slv.PSpread (updatePattern pat')



-- INFER VAR

inferVar :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar env exp@(Meta _ area (Src.Var n)) = case n of
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

enhanceVarError :: Env -> Src.Exp -> Area -> InferError -> Infer Scheme
enhanceVarError env exp area (InferError e _) = throwError
  $ InferError e (Reason (VariableNotDeclared exp) (envcurrentpath env) area)



-- INFER ABSTRACTIONS

inferAbs :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAbs env l@(Meta _ _ (Src.Abs param body)) = do
  tv <- newTVar Star
  let env' = extendVars env (param, Forall [] ([] :=> tv))
  (s, ps, t, es) <- inferBody env' body
  let t' = apply s (tv `fn` t)
  return (s, apply s ps, t', applyAbsSolve l param es t')


inferBody :: Env -> [Src.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody env [e] =
  (\(s, ps, t, e) -> (s, ps, t, [e])) <$> infer env e

inferBody env (e : xs) = do
  (_, _ , t   , _ ) <- infer env e
  (s, ps, env', e') <- case e of
    Meta _ _ (Src.TypedExp _ _) -> inferExplicitlyTyped env e
    _                           -> inferImplicitlyTyped True env e

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

inferApp :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp env (Meta _ area (Src.App abs arg final)) = do
  tv                  <- newTVar Star
  (s1, ps1, t1, eabs) <- infer env abs
  (s2, ps2, t2, earg) <- infer env arg

  s3                  <- catchError
    (unify (apply s2 t1) (t2 `fn` tv))
    (\case
      InferError (UnificationError _ _) _ ->
        throwError
          $ InferError (UnificationError (apply s2 t1) (t2 `fn` tv))
          $ Reason (WrongTypeApplied abs arg) (envcurrentpath env) (getArea arg)
      InferError e _ -> throwError $ InferError e $ Reason
        (WrongTypeApplied abs arg)
        (envcurrentpath env)
        (getArea arg)
    )

  let t = apply s3 tv

  let solved =
        Slv.Solved t area $ Slv.App eabs (updateType earg $ apply s3 t2) final

  return (s3 `compose` s2 `compose` s1, ps1 ++ ps2, t, solved)


-- INFER ASSIGNMENT

inferAssignment :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAssignment env e@(Meta _ _ (Src.Assignment name exp)) = do
  t <- newTVar Star
  let env' = extendVars env (name, Forall [] ([] :=> t))
  (s1, ps1, t1, e1) <- infer env' exp
  return (s1, ps1, t1, applyAssignmentSolve e name e1 t1)



-- INFER EXPORT

inferExport :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport env (Meta _ area (Src.Export exp)) = do
  (s, ps, t, e) <- infer env exp
  return (s, ps, t, Slv.Solved t area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor
  :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor env (Meta _ area (Src.ListConstructor elems)) =
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
  :: Env -> Src.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem env li = case li of
  Src.ListItem exp -> do
    (s, ps, t, e) <- infer env exp
    return (s, ps, t, Slv.ListItem e)

  Src.ListSpread exp -> do
    (s, ps, t, e) <- infer env exp
    case t of
      TApp (TCon (TC "List" _)) t' -> return (s, ps, t', Slv.ListSpread e)

      TVar _ -> return (s, ps, t, Slv.ListSpread e)

      _ -> throwError $ InferError (WrongSpreadType $ show t) NoReason



-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor
  :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor env (Meta _ area (Src.TupleConstructor elems)) = do
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

inferRecord :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord env exp = do
  let Meta _ area (Src.Record fields) = exp

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
  :: Env -> Src.Field -> Infer (Substitution, [(Slv.Name, Type)], Slv.Field)
inferRecordField env field = case field of
  Src.Field (name, exp) -> do
    (s, ps, t, e) <- infer env exp
    return (s, [(name, t)], Slv.Field (name, e))

  Src.FieldSpread exp -> do
    (s, ps, t, e) <- infer env exp
    case t of
      TRecord tfields _ -> return (s, M.toList tfields, Slv.FieldSpread e)

      TVar _ -> return (s, [], Slv.FieldSpread e)

      _ -> throwError $ InferError (WrongSpreadType $ show t) NoReason

shouldBeOpen :: Env -> [Src.Field] -> Infer Bool
shouldBeOpen env = foldrM
  (\field r -> case field of
    Src.Field       _ -> return r
    Src.FieldSpread e -> do
      (_, _, t, _) <- infer env e
      case t of
        TRecord _ _ -> return r
        TVar _      -> return True
  )
  False


-- INFER FIELD ACCESS

inferFieldAccess
  :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess env e@(Meta t area (Src.FieldAccess rec@(Meta _ _ (Src.Var namespace)) abs@(Meta _ _ (Src.Var ('.' : name)))))
  = do
    if isUpper $ head namespace
      then do
        sc <- catchError (lookupVar env (namespace <> "." <> name))
                         (enhanceVarError env e area)
        (ps :=> t) <- instantiate sc

        let e = Slv.Solved t area $ Slv.Var (namespace <> "." <> name)
        e' <- insertVarPlaceholders env e ps

        return (M.empty, ps, t, e')
      else inferFieldAccess' env e
inferFieldAccess env e = inferFieldAccess' env e



inferFieldAccess'
  :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess' env (Meta _ area (Src.FieldAccess rec@(Meta _ _ re) abs@(Meta _ _ (Src.Var ('.' : name)))))
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

inferIf :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf env exp@(Meta _ area (Src.If cond truthy falsy)) = do
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
  :: Env -> Src.Exp -> Src.Exp -> Area -> InferError -> Infer Substitution
addConditionReason env ifExp condExp area (InferError e _) =
  throwError $ InferError
    e
    (Reason (IfElseCondIsNotBool ifExp condExp) (envcurrentpath env) area)

addBranchReason
  :: Env -> Src.Exp -> Src.Exp -> Area -> InferError -> Infer Substitution
addBranchReason env ifExp falsyExp area (InferError e _) =
  throwError $ InferError
    e
    (Reason (IfElseBranchTypesDontMatch ifExp falsyExp)
            (envcurrentpath env)
            area
    )


-- INFER WHERE

inferWhere :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere env (Meta _ area (Src.Where exp iss)) = do
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
  :: Env -> Type -> Type -> Src.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch env tv t (Meta _ area (Src.Is pat exp)) = do
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

inferTypedExp :: Env -> Src.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp env (Meta _ area (Src.TypedExp exp typing)) = do
  infer env exp
  sc                <- typingToScheme env typing
  (ps :=> t)        <- instantiate sc

  (s1, ps1, t1, e1) <- infer env exp
  s2                <- catchError
    (unify t t1)
    (\(InferError e _) -> throwError $ InferError
      e
      (Reason (TypeAndTypingMismatch exp typing t t1) (envcurrentpath env) area)
    )

  return
    ( s1 `compose` s2
    , ps -- `union` ps1
    , t
    , Slv.Solved t area (Slv.TypedExp (updateType e1 t) (updateTyping typing))
    )



getExpName :: Src.Exp -> Maybe String
getExpName (Meta _ _ exp) = case exp of
  Src.Assignment name                               _ -> return name

  Src.TypedExp   (Meta _ _ (Src.Assignment name _)) _ -> return name

  Src.TypedExp (Meta _ _ (Src.Export (Meta _ _ (Src.Assignment name _)))) _ ->
    return name

  Src.Export (Meta _ _ (Src.Assignment name _)) -> return name

  _ -> Nothing


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
  :: Bool -> Env -> Src.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferImplicitlyTyped isLet env exp@(Meta _ area _) = do
  tv <- newTVar Star

  let env' = case getExpName exp of
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

  if not isLet && not (null (rs ++ ds)) && not (Src.isAssignment exp)
    then throwError $ InferError
      (AmbiguousType (TV "err" Star, [IsIn "n" [t']]))
      (SimpleReason (envcurrentpath env) area)
    else return ""

  case getExpName exp of
    Just n  -> return (s'', ps', extendVars env' (n, Forall [] $ ps' :=> t'), e)

    Nothing -> return (s'', ps', env', e)


inferExplicitlyTyped
  :: Env -> Src.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped env e@(Meta _ area (Src.TypedExp exp typing)) = do
  sc             <- typingToScheme env typing
  qt@(qs :=> t') <- instantiate sc

  let env' = case getExpName exp of
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

  qs'' <- getAllParentInterfaces env qs'

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
        let env'' = case getExpName exp of
              Just n  -> extendVars env' (n, sc'')
              Nothing -> env'

        return
          ( s'
          , qs''
          , env''
          , Slv.Solved t'' area (Slv.TypedExp e' (updateTyping typing))
          )


inferExps :: Env -> [Src.Exp] -> Infer ([Slv.Exp], Env)
inferExps env []       = return ([], env)

inferExps env (e : es) = do
  (s, ps, env', e') <- upgradeReason env (getArea e) $ case e of
    Meta _ _ (Src.TypedExp _ _) -> inferExplicitlyTyped env e
    _                           -> inferImplicitlyTyped False env e

  e''            <- insertClassPlaceholders env e' ps
  e'''           <- updatePlaceholders env s e''

  (es', nextEnv) <- inferExps env' es

  return (e''' : es', nextEnv)



solveTable :: Src.Table -> Src.AST -> Infer Slv.Table
solveTable table ast = do
  (table, _) <- solveTable' table ast
  return table

solveTable' :: Src.Table -> Src.AST -> Infer (Slv.Table, Env)
solveTable' table ast@Src.AST { Src.aimports } = do
  -- First we resolve imports to update the env
  (inferredASTs, adts, vars, interfaces, methods) <- solveImports table aimports

  let importEnv = Env { envtypes       = adts
                      , envvars        = vars
                      , envcurrentpath = fromMaybe "" (Src.apath ast)
                      , envinterfaces  = interfaces
                      , envmethods     = methods
                      }

  -- Then we infer the ast
  env <- buildInitialEnv importEnv ast
  let envWithImports = env { envtypes = M.union (envtypes env) adts
                           , envvars  = M.union (envvars env) vars
                          --  , envmethods = envmethods env <> methods
                           }

  (inferredAST, env) <- inferAST envWithImports ast

  case Slv.apath inferredAST of
    Just fp -> return (M.insert fp inferredAST inferredASTs, env)



exportedExps :: Slv.AST -> Infer [(Slv.Name, Slv.Exp)]
exportedExps Slv.AST { Slv.aexps, Slv.apath } = case apath of
  Just p -> mapM (bundleExports p) $ filter isExport aexps

 where
  bundleExports _ exp = return $ case exp of
    e'@(Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)))) ->
      (n, e')
    e'@(Slv.Solved _ _ (Slv.TypedExp (Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)))) _))
      -> (n, e')

  isExport :: Slv.Exp -> Bool
  isExport a = case a of
    (Slv.Solved _ _ (Slv.Export _)) -> True
    (Slv.Solved _ _ (Slv.TypedExp (Slv.Solved _ _ (Slv.Export _)) _)) -> True

    _                               -> False


exportedADTs :: Slv.AST -> [Slv.Name]
exportedADTs Slv.AST { Slv.atypedecls } =
  Slv.adtName <$> filter Slv.adtExported atypedecls


solveImports
  :: Src.Table
  -> [Src.Import]
  -> Infer (Slv.Table, TypeDecls, Vars, Interfaces, Methods)
solveImports table (imp : is) = do
  let modulePath = Src.getImportAbsolutePath imp


  (solvedAST, solvedTable, envADTs, envSolved) <-
    case AST.findAST table modulePath of
      Right ast -> do
        (solvedTable, env) <- solveTable' table ast
        solvedAST          <- case M.lookup modulePath solvedTable of
          Just a -> return a
          Nothing ->
            throwError $ InferError (ImportNotFound modulePath) NoReason
        return (solvedAST, solvedTable, envtypes env, env)

      Left e -> throwError e

  exportedExps <- M.fromList <$> exportedExps solvedAST
  let exportedTypes    = mapM (return . Slv.getType) exportedExps

  let exportedADTNames = exportedADTs solvedAST
  let adtExports = M.filterWithKey (\k _ -> k `elem` exportedADTNames) envADTs

  let exportedConstructorNames = Slv.getConstructorName <$> concat
        (Slv.adtconstructors <$> filter (\x -> isADT x && Slv.adtExported x)
                                        (Slv.atypedecls solvedAST)
        )
  let buildConstructorVars = M.filterWithKey
        (\k v -> k `elem` exportedConstructorNames)
        (envvars envSolved)

  (exports, vars) <- case (exportedTypes, imp) of
    (Just exports, Meta _ _ (Src.DefaultImport namespace _ _)) -> do

      let constructors = M.mapKeys ((namespace <> ".") <>) buildConstructorVars
      exports' <- M.fromList <$> mapM
        (\(k, _) -> (namespace <> "." <> k, ) <$> lookupVar envSolved k)
        (M.toList exports)

      return (exports', constructors)

    (Just exports, Meta _ _ (Src.NamedImport names _ _)) -> do
      exports' <- M.fromList
        <$> mapM (\(k, _) -> (k, ) <$> lookupVar envSolved k) (M.toList exports)

      let exports'' = M.mapMaybeWithKey
            (\k _ -> if k `elem` names then M.lookup k exports' else Nothing)
            exports

      return (exports'', buildConstructorVars)

    (Nothing, _) ->
      throwError $ InferError (ImportNotFound modulePath) NoReason


  (nextTable, nextADTs, nextVars, nextInterfaces, nextMethods) <- solveImports
    table
    is

  mergedADTs <- do
    withNamespaces <- case imp of
      Meta _ _ (Src.DefaultImport namespace _ absPath) -> return
        $ M.mapKeys (mergeTypeDecl namespace absPath adtExports) adtExports

      _ -> return adtExports

    if M.intersection withNamespaces nextADTs == M.empty
      then return $ M.union withNamespaces nextADTs
      else throwError $ InferError FatalError NoReason

  let allVars = nextVars <> vars <> exports

  return
    ( M.insert modulePath solvedAST (M.union solvedTable nextTable)
    , mergedADTs
    , allVars
    , mergeInterfaces (envinterfaces envSolved) nextInterfaces
    , M.union (envmethods envSolved) nextMethods
    )

solveImports _ [] = return
  ( M.empty
  , envtypes initialEnv
  , envvars initialEnv
  , envinterfaces initialEnv
  , envmethods initialEnv
  )

mergeInterfaces :: Interfaces -> Interfaces -> Interfaces
mergeInterfaces = M.foldrWithKey mergeInterface

-- (k -> a -> b -> b)
mergeInterface :: Id -> Interface -> Interfaces -> Interfaces
mergeInterface = M.insertWith
  (\(Interface tvs ps is) (Interface _ _ is') ->
    Interface tvs ps $ is `union` is'
  )


isADT :: Slv.TypeDecl -> Bool
isADT Slv.ADT{} = True
isADT _         = False

mergeTypeDecl :: String -> FilePath -> M.Map String Type -> String -> String
mergeTypeDecl namespace absPath adts key = namespace <> "." <> key


updateInterface :: Src.Interface -> Slv.Interface
updateInterface (Src.Interface constraints name vars methods) = Slv.Interface
  (updateTyping <$> constraints)
  name
  vars
  (updateTyping <$> methods)

inferAST :: Env -> Src.AST -> Infer (Slv.AST, Env)
inferAST env Src.AST { Src.aexps, Src.apath, Src.aimports, Src.atypedecls, Src.ainstances, Src.ainterfaces }
  = do
    (inferredExps, env') <- inferExps env aexps
    inferredInstances    <- mapM (resolveInstance env') ainstances
    let updatedInterfaces = updateInterface <$> ainterfaces

    return
      ( Slv.AST
        { Slv.aexps       = inferredExps
        , Slv.apath       = apath
        , Slv.atypedecls  = updateADT <$> atypedecls
        , Slv.aimports    =
          (\case
            g@Slv.DefaultImport{}            -> g
            i@(Slv.NamedImport names fp afp) -> Slv.NamedImport
              (mapMaybe (\n -> M.lookup n (envvars env) >> Just n) names)
              fp
              afp
          )
          .   updateImport
          <$> aimports
        , Slv.ainterfaces = updatedInterfaces
        , Slv.ainstances  = inferredInstances
        }
      , env'
      )


buildInstanceConstraint ::Env -> Src.Typing -> Infer Pred
buildInstanceConstraint env typing = case typing of
  (Meta _ _ (Src.TRComp n [Meta _ _ (Src.TRSingle var)])) ->
      case M.lookup n (envinterfaces env) of
                Just (Interface ts _ _) ->
                  let tvs = catMaybes $ searchVarInType var . TVar <$> ts--IsIn n [TVar $ TV var k]
                  in  if not (null tvs) then
                    return $ IsIn n [head tvs]
                  else return $ IsIn n [TVar $ TV var Star]

                x -> return $ IsIn n [TVar $ TV var Star]
  (Meta _ _ (Src.TRComp n args)) ->
    case M.lookup n (envinterfaces env) of
        Just (Interface tvs _ _) -> do
          vars <- mapM  (\case
                          (Meta _ _ (Src.TRSingle v), TV _ k) -> return $ TVar $ TV v k
                          (typing, _) -> typingToType env typing
                        ) (zip args tvs)
          return $ IsIn n vars


resolveInstance :: Env -> Src.Instance -> Infer Slv.Instance
resolveInstance env (Src.Instance constraints instName tys dict) = do
  instanceTypes <- mapM (typingToType env) tys
  let subst = foldl (\s t -> s `compose` buildVarSubsts t) mempty instanceTypes
  (Interface _ ps _) <- lookupInterface env instName
  let instancePreds   = apply subst $ [IsIn instName instanceTypes] <> ps
  constraintPreds <- apply subst <$> mapM (buildInstanceConstraint env) constraints
  let psTypes = concat $ (\(IsIn _ ts) -> ts) <$> constraintPreds
  let subst'  = foldl (\s t -> s `compose` buildVarSubsts t) mempty psTypes
  inferredMethods <- mapM (inferMethod env (apply subst' instancePreds) (apply subst' constraintPreds))
                          (M.toList dict)
  let dict' = M.fromList $ (\(a, b, c) -> (a, (b, c))) <$> inferredMethods
  return $ Slv.Instance (updateTyping <$> constraints)
                        instName
                        (updateTyping <$> tys)
                        dict'

inferMethod
  :: Env
  -> [Pred]
  -> [Pred]
  -> (Src.Name, Src.Exp)
  -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod env instancePreds constraintPreds (mn, m) = upgradeReason
  env
  (getArea m)
  (inferMethod' env instancePreds constraintPreds (mn, m))

-- createInstanceConstraintError :: InferError -> [Pred] -> [Pred] -> 

inferMethod'
  :: Env
  -> [Pred]
  -> [Pred]
  -> (Src.Name, Src.Exp)
  -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod' env instancePreds constraintPreds (mn, m) = do
  sc'             <- lookupVar env mn
  qt@(mps :=> mt) <- instantiate sc'
  s1              <- specialMatchMany mps instancePreds
  let (mps' :=> mt')  = apply s1 qt
  let qt'@(qs :=> t') = constraintPreds :=> mt'

  let sc              = quantify (ftv qt') qt'

  (s, ps, t, e) <- infer env m
  (qs :=> t')   <- instantiate sc
  s'            <- (`compose` s) <$> unify t' t

  let qs' = apply s' qs
      t'' = apply s' t'
      fs  = ftv (apply s' env)
      gs  = ftv t'' \\ fs
      sc' = quantify (ftv t'') (qs' :=> t'')
  ps'         <- filterM ((not <$>) . entail env qs') (apply s' ps)

  (ds, rs)    <- split env fs gs ps'

  withParents <- getAllParentInterfaces env qs'

  if sc /= sc'
    then throwError $ InferError
      (SignatureTooGeneral sc sc')
      (SimpleReason (envcurrentpath env) (getArea m))
    else if not (null rs)
      then throwError $ InferError
        ContextTooWeak
        (SimpleReason (envcurrentpath env) (getArea m))
      else do
        let e' = updateType e t''
        tmp <- insertClassPlaceholders
          env
          (Slv.Solved t emptyArea $ Slv.Assignment mn e')
          (apply s' withParents)
        let (Slv.Solved _ _ (Slv.Assignment _ e'')) = tmp
        e''' <- updatePlaceholders env s' e''

        return (mn, e''', sc)


upgradeReason :: Env -> Area -> Infer a -> Infer a
upgradeReason env area a = catchError
  a
  (\case
    (InferError e NoReason) ->
      throwError $ InferError e $ SimpleReason (envcurrentpath env) area
    (InferError e r) -> throwError $ InferError e r
  )


updateImport :: Src.Import -> Slv.Import
updateImport i = case i of
  Meta _ _ (Src.NamedImport   ns p fp) -> Slv.NamedImport ns p fp

  Meta _ _ (Src.DefaultImport n  p fp) -> Slv.DefaultImport n p fp


updateADT :: Src.TypeDecl -> Slv.TypeDecl
updateADT adt@Src.ADT{} =
  let name     = Src.adtname adt
      params   = Src.adtparams adt
      ctors    = Src.adtconstructors adt
      exported = Src.adtexported adt
  in  Slv.ADT { Slv.adtname         = name
              , Slv.adtparams       = params
              , Slv.adtconstructors = updateADTConstructor <$> ctors
              , Slv.adtexported     = exported
              }
updateADT alias@Src.Alias{} =
  let name     = Src.aliasname alias
      params   = Src.aliasparams alias
      t        = Src.aliastype alias
      exported = Src.aliasexported alias
  in  Slv.Alias { Slv.aliasname     = name
                , Slv.aliasparams   = params
                , Slv.aliastype     = updateTyping t
                , Slv.aliasexported = exported
                }

updateADTConstructor :: Src.Constructor -> Slv.Constructor
updateADTConstructor (Src.Constructor cname cparams) =
  Slv.Constructor cname $ updateTyping <$> cparams


-- -- TODO: Make it call inferAST so that inferAST can return an (Infer TBD)
-- -- Well, or just adapt it somehow
runInfer :: Env -> Src.AST -> Either InferError Slv.AST
runInfer env ast =
  fst . fst <$> runExcept (runStateT (inferAST env ast) Unique { count = 0 })

