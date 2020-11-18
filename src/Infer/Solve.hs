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
import           Data.Foldable                  ( foldrM )
import qualified Parse.AST                     as AST
import qualified AST.Source                    as Src
import qualified AST.Solved                    as Slv
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Substitute
import           Infer.Unify
import           Infer.Instantiate
import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Explain.Location
import           Data.Char                      ( isLower )
import           Data.List                      ( find )


infer :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
infer env lexp =
  let (Meta _ area exp) = lexp
  in  case exp of
        Src.LInt  _           -> return (M.empty, num, applyLitSolve lexp num)
        Src.LStr  _           -> return (M.empty, str, applyLitSolve lexp str)
        Src.LBool _           -> return (M.empty, bool, applyLitSolve lexp bool)

        Src.Var   _           -> inferVar env lexp
        Src.Abs        _ _    -> inferAbs env lexp
        Src.App        _ _    -> inferApp env lexp
        Src.Assignment _ _    -> inferAssignment env lexp
        Src.Where      _ _    -> inferWhere env lexp
        Src.Record _          -> inferRecord env lexp
        Src.FieldAccess _ _   -> inferFieldAccess env lexp
        Src.TypedExp    _ _   -> inferTypedExp env lexp
        Src.ListConstructor _ -> inferListConstructor env lexp
        Src.Export          _ -> inferExport env lexp
        Src.If _ _ _          -> inferIf env lexp
        Src.JSExp c           -> do
          v <- newTVar
          return (M.empty, v, Slv.Solved v area (Slv.JSExp c))


-- TODO: Should probably just take a Loc instead of the old Expression !
applyLitSolve :: Src.Exp -> Type -> Slv.Exp
applyLitSolve (Meta _ area exp) t = case exp of
  Src.LInt  v -> Slv.Solved t area $ Slv.LInt v
  Src.LStr  v -> Slv.Solved t area $ Slv.LStr v
  Src.LBool v -> Slv.Solved t area $ Slv.LBool v

applyAbsSolve :: Src.Exp -> Slv.Name -> Slv.Exp -> Type -> Slv.Exp
applyAbsSolve (Meta _ loc _) param body t =
  Slv.Solved t loc $ Slv.Abs param body

applyAssignmentSolve :: Src.Exp -> Slv.Name -> Slv.Exp -> Type -> Slv.Exp
applyAssignmentSolve (Meta _ loc _) n exp t =
  Slv.Solved t loc $ Slv.Assignment n exp


updateType :: Slv.Exp -> Type -> Slv.Exp
updateType (Slv.Solved _ a e) t' = Slv.Solved t' a e


updatePattern :: Src.Pattern -> Slv.Pattern
updatePattern (Meta _ _ p) = case p of
  Src.PVar name           -> Slv.PVar name

  Src.PAny                -> Slv.PAny

  Src.PCtor name patterns -> Slv.PCtor name (updatePattern <$> patterns)

  Src.PNum    n           -> Slv.PNum n
  Src.PStr    n           -> Slv.PStr n
  Src.PBool   n           -> Slv.PBool n

  Src.PCon    n           -> Slv.PCon n

  Src.PRecord fields      -> Slv.PRecord (updatePattern <$> fields)

  Src.PList   patterns    -> Slv.PList (updatePattern <$> patterns)

  Src.PSpread pattern     -> Slv.PSpread (updatePattern pattern)


updateTyping :: Src.Typing -> Slv.Typing
updateTyping t = case t of
  Meta _ _ (Src.TRSingle name   ) -> Slv.TRSingle name

  Meta _ _ (Src.TRComp name vars) -> Slv.TRComp name (updateTyping <$> vars)

  Meta _ _ (Src.TRArr  l    r   ) -> Slv.TRArr (updateTyping l) (updateTyping r)

  Meta _ _ (Src.TRRecord fields ) -> Slv.TRRecord (updateTyping <$> fields)


-- INFER VAR

inferVar :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferVar env exp =
  let Meta _ area (Src.Var n) = exp
  in  case n of
        ('.' : name) -> do
          let s = Forall [TV "a"] $ TArr
                (TRecord (M.fromList [(name, TVar $ TV "a")]) True)
                (TVar $ TV "a")
          t <- instantiate s
          return (M.empty, t, Slv.Solved t area $ Slv.Var n)

        _ -> do
          (s, t) <- catchError (lookupVar env n) (enhanceVarError env exp area)
          return (s, t, Slv.Solved t area $ Slv.Var n)

enhanceVarError
  :: Env -> Src.Exp -> Area -> InferError -> Infer (Substitution, Type)
enhanceVarError env exp area (InferError e _) = throwError
  $ InferError e (Reason (VariableNotDeclared exp) (envcurrentpath env) area)



-- INFER ABS

inferAbs :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferAbs env l@(Meta _ _ (Src.Abs param body)) = do
  tv <- newTVar
  let env' = extendVars env (param, Forall [] tv)
  (s1, t1, e) <- infer env' body
  let t = apply s1 (tv `TArr` t1)
  return (s1, t, applyAbsSolve l param e t)



-- INFER APP

inferApp :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferApp env (Meta _ area (Src.App abs arg)) = do
  tv             <- newTVar
  (s1, t1, eabs) <- infer env abs
  (s2, t2, earg) <- infer (apply (removeRecordTypes s1) env) arg

  s3             <- case unify (apply s2 t1) (TArr t2 tv) of
    Right s -> return s
    Left  e -> throwError $ InferError e $ Reason (WrongTypeApplied abs arg)
                                                  (envcurrentpath env)
                                                  (getArea arg)
  let t      = apply s3 tv
  let solved = Slv.Solved t area $ Slv.App eabs (updateType earg $ apply s3 t2)

  return (s3 `compose` s2 `compose` s1, t, solved)



-- INFER ASSIGNMENT

inferAssignment :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferAssignment env e@(Meta _ _ (Src.Assignment name exp)) = do
  let env' = extendVars env (name, Forall [TV "a"] $ TVar $ TV "a")
  (s1, t1, e1) <- infer env' exp
  return (s1, t1, applyAssignmentSolve e name e1 t1)



-- INFER EXPORT

inferExport :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferExport env (Meta _ area (Src.Export exp)) = do
  (s, t, e) <- infer env exp
  return (s, t, Slv.Solved t area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferListConstructor env (Meta _ loc (Src.ListConstructor elems)) =
  case elems of
    [] ->
      let t = TComp "Prelude" "List" [TVar $ TV "a"]
      in  return (M.empty, t, Slv.Solved t loc (Slv.ListConstructor []))

    elems -> do
      inferred <- mapM (inferListItem env) elems
      let (_, t1, _) = head inferred
      let t          = TComp "Prelude" "List" [t1]
      -- TODO: Error should be handled
      s <- unifyToInfer env $ unifyElems t1 (mid <$> inferred)
      return (s, t, Slv.Solved t loc (Slv.ListConstructor (trd <$> inferred)))


inferListItem :: Env -> Src.ListItem -> Infer (Substitution, Type, Slv.ListItem)
inferListItem env li = case li of
  Src.ListItem exp -> do
    (s, t, e) <- infer env exp
    return (s, t, Slv.ListItem e)

  Src.ListSpread exp -> do
    (s, t, e) <- infer env exp
    case t of
      TComp "Prelude" "List" [t'] -> return (s, t', Slv.ListSpread e)

      TVar _ -> return (s, t, Slv.ListSpread e)

      _ -> throwError $ InferError (UnknownType $ show t) NoReason



-- INFER RECORD

inferRecord :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferRecord env exp = do
  let Meta _ area (Src.Record fields) = exp

  inferred <- mapM (inferRecordField env) fields
  let inferredFields = trd <$> inferred
      subst          = foldr compose M.empty (beg <$> inferred)
      recordType     = TRecord (M.fromList $ concat $ mid <$> inferred) False
  return
    (subst, recordType, Slv.Solved recordType area (Slv.Record inferredFields))

inferRecordField
  :: Env -> Src.Field -> Infer (Substitution, [(Slv.Name, Type)], Slv.Field)
inferRecordField env field = case field of
  Src.Field (name, exp) -> do
    (s, t, e) <- infer env exp
    return (s, [(name, t)], Slv.Field (name, e))

  Src.FieldSpread exp -> do
    (s, t, e) <- infer env exp
    case t of
      TRecord tfields _ -> return (s, M.toList tfields, Slv.FieldSpread e)

      TVar _            -> return (s, [], Slv.FieldSpread e)

      -- TODO: This needs to be a new error type maybe ?
      _                 -> throwError $ InferError FatalError NoReason



-- INFER FIELD ACCESS

inferFieldAccess :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferFieldAccess env (Meta _ area (Src.FieldAccess rec@(Meta _ _ re) abs@(Meta _ _ (Src.Var ('.' : name)))))
  = do
    (fieldSubst , fieldType , fieldExp ) <- infer env abs
    (recordSubst, recordType, recordExp) <- infer env rec

    let foundFieldType = case recordType of
          TRecord fields _ -> M.lookup name fields
          _                -> Nothing

    case foundFieldType of
      Just t -> do
        let freeVars = ftv t
        t' <- instantiate $ Forall (S.toList freeVars) t
        let solved = Slv.Solved t' area (Slv.FieldAccess recordExp fieldExp)
        return (fieldSubst, t', solved)

      Nothing -> do
        tv <- newTVar
        s3 <- case unify (apply recordSubst fieldType) (TArr recordType tv) of
          Right s -> return s
          Left  e -> throwError $ InferError e NoReason

        let t          = apply s3 tv
        let rs         = recordSubstForVar re fieldType

        let recordExp' = updateType recordExp (apply s3 recordType)
        let solved = Slv.Solved t area (Slv.FieldAccess recordExp' fieldExp)

        return (s3 `compose` rs `compose` recordSubst, t, solved)

recordSubstForVar :: Src.Exp_ -> Type -> Substitution
recordSubstForVar (Src.Var n) fieldType =
  let (TArr recordType' _) = fieldType in M.fromList [(TV n, recordType')]
recordSubstForVar _ _ = M.empty



-- INFER IF

inferIf :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferIf env exp@(Meta _ area (Src.If cond truthy falsy)) = do
  (s1, tcond  , econd  ) <- infer env cond
  (s2, ttruthy, etruthy) <- infer env truthy
  (s3, tfalsy , efalsy ) <- infer env falsy

  s4 <- catchError (unifyToInfer env $ unify (TCon CBool) tcond)
                   (addConditionReason env exp cond area)
  s5 <- catchError (unifyToInfer env $ unify ttruthy tfalsy)
                   (addBranchReason env exp falsy area)

  let t = apply s5 ttruthy

  return
    ( s1 `compose` s2 `compose` s3 `compose` s4 `compose` s5
    , t
    , Slv.Solved t area (Slv.If econd etruthy efalsy)
    )

addConditionReason
  :: Env -> Src.Exp -> Src.Exp -> Area -> InferError -> Infer (Substitution)
addConditionReason env ifExp condExp area (InferError e _) =
  throwError $ InferError
    e
    (Reason (IfElseCondIsNotBool ifExp condExp) (envcurrentpath env) area)

addBranchReason
  :: Env -> Src.Exp -> Src.Exp -> Area -> InferError -> Infer (Substitution)
addBranchReason env ifExp falsyExp area (InferError e _) =
  throwError $ InferError
    e
    (Reason (IfElseBranchTypesDontMatch ifExp falsyExp)
            (envcurrentpath env)
            area
    )

-- INFER WHERE

inferWhere :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferWhere env whereExp@(Meta _ loc (Src.Where exp iss)) = do
  (se, te, ee) <- infer env exp

  inferredIss  <- mapM (inferIs env te) iss
  let issSubstitution = foldr1 compose $ se : (beg <$> inferredIss)
  let issTypes        = mid <$> inferredIss
  let iss             = trd <$> inferredIss

  let typeMatrix      = (, issTypes) <$> issTypes
  s <-
    foldr1 compose
      <$> mapM
            (\(t, ts) ->
              -- TODO: Error should be handled
              unifyToInfer env $ unifyElems (apply issSubstitution t) ts
            )
            typeMatrix

  let updatedIss =
        (\(t, (Slv.Solved _ a e)) -> Slv.Solved (apply s t) a e)
          <$> zip issTypes iss

  let (TArr _ whereType) = (apply s . head) issTypes

  return
    ( s
    , whereType
    , Slv.Solved whereType loc
      $ Slv.Where (updateType ee (apply s te)) updatedIss
    )

 where
  inferIs :: Env -> Type -> Src.Is -> Infer (Substitution, Type, Slv.Is)
  inferIs e tinput c@(Meta _ area (Src.Is pattern exp)) = do
    tp   <- buildPatternType e pattern
    env' <- case tinput of
      TVar _ -> generateIsEnv tp e pattern
      _      -> generateIsEnv tinput e pattern

    (se, te, ee) <- infer env' exp

    -- TODO: Ugly fix for now, we'll have to rethink and remodel the way records and record patterns
    -- are currently implemented.
    let newTP = completePatternType tp pattern se

    let tarr  = TArr (apply se tinput) te
    let tarr' = TArr (apply se newTP) te
    su <- catchError (unifyToInfer env' $ unify tarr tarr')
                     (addPatternReason e whereExp pattern area)

    let sf = su `compose` se

    return
      ( sf
      , tarr
      , Slv.Solved tarr area
        $ Slv.Is (updatePattern pattern) (updateType ee $ apply sf te)
      )


  completePatternType :: Type -> Src.Pattern -> Substitution -> Type
  completePatternType tp pattern se =
    let step1   = findSpreadTypes tp pattern se
        applied = apply se step1
        step2   = spreadSpreads applied
    in  step2

  findSpreadTypes :: Type -> Src.Pattern -> Substitution -> Type
  findSpreadTypes tp@(TRecord ff' _) pattern se = case pattern of
    (Meta _ _ (Src.PRecord fields)) -> do
      let ft = M.mapWithKey (assignSpreadType ff' se) fields
      TRecord ft True

  findSpreadTypes tp _ _ = tp

  assignSpreadType ff' se key field = case field of
    (Meta _ _ (Src.PSpread (Meta _ _ (Src.PVar n)))) -> TVar $ TV n

    (Meta _ _ (Src.PRecord _                      )) -> case M.lookup key ff' of
      Just x -> findSpreadTypes x field se
      _      -> findSpreadTypes (TRecord M.empty True) field se

    _ -> case M.lookup key ff' of
      Just x -> x
      -- TODO: Put a number to be sure it's unique, but it should use newTVar
      _      -> TVar $ TV "a0"

  spreadSpreads :: Type -> Type
  spreadSpreads (TRecord fields open) =
    let (TRecord spreadFields _) = case M.lookup "..." fields of
          Just (TRecord spreadFields _) ->
            let withoutSpread = M.filterWithKey (\k v -> k /= "...") fields
            in  TRecord (M.union withoutSpread spreadFields) open
          Just _  -> TRecord fields open
          Nothing -> TRecord fields open
    in  TRecord (M.map updateSubRecord spreadFields) open
  spreadSpreads t = t

  updateSubRecord field = case field of
    (TRecord _ _) -> spreadSpreads field
    _             -> field


  buildPatternType :: Env -> Src.Pattern -> Infer Type
  buildPatternType e@Env { envvars } pattern@(Meta _ area pat) = case pat of
    Src.PVar  _         -> newTVar

    Src.PCon  "String"  -> return $ TCon CString
    Src.PCon  "Boolean" -> return $ TCon CBool
    Src.PCon  "Number"  -> return $ TCon CNum

    Src.PStr  _         -> return $ TCon CString
    Src.PBool _         -> return $ TCon CBool
    Src.PNum  _         -> return $ TCon CNum

    Src.PAny            -> newTVar

    Src.PRecord fields ->
      let fieldsWithoutSpread = M.filterWithKey (\k v -> k /= "...") fields
      in  (\fields -> TRecord fields True) . M.fromList <$> mapM
            (\(k, v) -> (k, ) <$> buildPatternType e v)
            (M.toList fieldsWithoutSpread)

    Src.PCtor n as -> do
      (Forall fv ctor) <- if elem '.' n
        then do
          t <- findNamespacedConstructorInIs e pattern whereExp n
          return $ Forall (S.toList $ ftv t) t
        else case M.lookup n envvars of
          Just x  -> return x
          Nothing -> throwError $ InferError
            (UnknownType n)
            (Reason (PatternConstructorDoesNotExist whereExp pattern)
                    (envcurrentpath e)
                    area
            )

      let rt = arrowReturnType ctor
      ctor'  <- argPatternsToArrowType rt as
      ctor'' <- instantiate $ Forall fv ctor
      -- TODO: Error should be handled
      s      <- unifyToInfer env $ unify ctor' ctor''
      return $ apply s rt

     where
      argPatternsToArrowType :: Type -> [Src.Pattern] -> Infer Type
      argPatternsToArrowType rt (f : xs) = do
        l <- buildPatternType e f
        r <- argPatternsToArrowType rt xs
        return $ TArr l r
      argPatternsToArrowType rt [] = return rt

    Src.PSpread pattern -> buildPatternType env pattern

    -- TODO: Need to iterate through items and unify them
    Src.PList   []      -> return $ TComp "Prelude" "List" [TVar $ TV "a"]
    Src.PList patterns ->
      TComp "Prelude" "List" . (: []) <$> buildPatternType e (head patterns)

    _ -> newTVar


  generateIsEnv :: Type -> Env -> Src.Pattern -> Infer Env
  generateIsEnv t e@Env { envvars = vars } pattern@(Meta _ area pat) =
    case (pat, t) of
      (Src.PVar    v     , t'  ) -> return $ extendVars e (v, Forall [] t')

      (Src.PRecord fields, tipe) -> do
        let fields' = case tipe of
              TVar _       -> M.empty
              TRecord f' _ -> f'

        let fieldsWithoutSpread = M.filterWithKey (\k v -> k /= "...") fields
        let spreadField =
              snd <$> find (\(k, v) -> k == "...") (M.toList fields)

        declaredFields <- mapM (\(k, pat) -> (pat, ) <$> lookupType k fields')
                               (M.toList fieldsWithoutSpread)
        let fieldsEnv =
              foldrM (\(p, t') e' -> generateIsEnv t' e' p) e declaredFields

        let
          envForSpread = case spreadField of
            Just (Meta _ _ (Src.PSpread (Meta _ _ (Src.PVar n)))) ->
              let
                keysToRemove = M.keys fieldsWithoutSpread
                -- TODO: It should likely not be opened and would cause weird issues like accessing non spread properties
                spreadType   = TRecord
                  (foldr (\k f -> M.delete k f) fields' keysToRemove)
                  False
              in
                extendVars e (n, Forall [] spreadType)
            Nothing -> e
        (\e' -> e' { envvars = M.union (envvars e') (envvars envForSpread) })
          <$> fieldsEnv
       where
        lookupType fieldName fields = case M.lookup fieldName fields of
          Just x  -> return x
          Nothing -> newTVar

      (Src.PSpread pattern, t) -> generateIsEnv t e pattern

      (Src.PList items, TComp "Prelude" "List" [t]) ->
        foldrM (\p e' -> generateIsEnv t e' p) e items
      (Src.PList items   , t) -> foldrM (\p e' -> generateIsEnv t e' p) e items

      (Src.PCtor cname as, t) -> do
        -- ctor <- findConstructor cname
        ctor <- if elem '.' cname
          then findNamespacedConstructorInIs e pattern whereExp cname
          else findConstructor cname

        let adtT = arrowReturnType ctor
        s <- case unify adtT t of
          Right a -> return a
          Left  e -> throwError $ InferError
            e
            (Reason (PatternTypeError whereExp pattern)
                    (envcurrentpath env)
                    area
            )

        case (apply s ctor, as) of
          (TArr a _, [a']) -> do
            generateIsEnv a e a'

          (TArr a (TArr b _), [a', b']) -> do
            e1 <- generateIsEnv a e a'
            generateIsEnv b e1 b'

          (TArr a (TArr b (TArr c _)), [a', b', c']) -> do
            e1 <- generateIsEnv a e a'
            e2 <- generateIsEnv b e1 b'
            generateIsEnv c e2 c'

          _ -> return e

      _ -> return e

   where
    findConstructor :: String -> Infer Type
    findConstructor cname = case M.lookup cname vars of
      Just s  -> instantiate s

      Nothing -> throwError $ InferError
        (UnknownType cname)
        (Reason (PatternConstructorDoesNotExist whereExp pattern)
                (envcurrentpath e)
                area
        )

findNamespacedConstructorInIs
  :: Env -> Src.Pattern -> Src.Exp -> String -> Infer Type
findNamespacedConstructorInIs e@Env { envvars } pattern@(Meta _ area _) whereExp cname
  = do
    let (namespace, cname') = break (== '.') cname
    case M.lookup namespace envvars of
      Just s -> do
        h <- instantiate s
        let (TRecord fields _) = h

        case M.lookup (tail cname') fields of
          Just t  -> return t
          Nothing -> throwError $ InferError
            (UnknownType cname)
            (Reason (PatternConstructorDoesNotExist whereExp pattern)
                    (envcurrentpath e)
                    area
            )

      Nothing -> throwError $ InferError
        (UnknownType cname)
        (Reason (PatternConstructorDoesNotExist whereExp pattern)
                (envcurrentpath e)
                area
        )


addPatternReason
  :: Env -> Src.Exp -> Src.Pattern -> Area -> InferError -> Infer (Substitution)
addPatternReason env whereExp pattern area (InferError e _) =
  throwError $ InferError
    e
    (Reason (PatternTypeError whereExp pattern) (envcurrentpath env) area)



-- INFER TYPEDEXP

inferTypedExp :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferTypedExp env (Meta _ area (Src.TypedExp exp typing)) = do
  t <- typingToType env typing
  let freevars = ftv t

  t'           <- instantiate $ Forall (S.toList freevars) t

  (s1, t1, e1) <- infer env exp
  s2           <- case unify t' t1 of
    Right solved -> return solved

    Left  err    -> throwError $ InferError
      err
      (Reason (TypeAndTypingMismatch exp typing t' t1) (envcurrentpath env) area
      )

  return
    ( s1 `compose` s2
    , t'
    , Slv.Solved t area (Slv.TypedExp (updateType e1 t') (updateTyping typing))
    )


typingToType :: Env -> Src.Typing -> Infer Type
typingToType env (Meta _ _ (Src.TRSingle t))
  | t == "Number" = return $ TCon CNum
  | t == "Boolean" = return $ TCon CBool
  | t == "String" = return $ TCon CString
  | t == "Void" = return $ TCon CVoid
  | isLower $ head t = return $ TVar $ TV t
  | otherwise = do
    h <- lookupADT env t
    let (TComp astPath realName _) = h
    return $ TComp astPath realName []

typingToType env (Meta _ _ (Src.TRComp t ts)) = do
  -- fetch ADT from env, and verify that the args applied match it or ERR
  h <- lookupADT env t
  let (TComp astPath realName _) = h
  params <- mapM (typingToType env) ts
  return $ TComp astPath realName params

typingToType env (Meta _ _ (Src.TRArr l r)) = do
  l' <- typingToType env l
  r' <- typingToType env r
  return $ TArr l' r'

typingToType env (Meta _ _ (Src.TRRecord f)) = do
  f' <- mapM (typingToType env) f
  return $ TRecord f' False



inferExps :: Env -> [Src.Exp] -> Infer [Slv.Exp]
inferExps _   []       = return []

inferExps env [exp   ] = (: []) . trd <$> infer env exp

inferExps env (e : xs) = do
  (_, t, e') <- infer env e
  let exp = Slv.extractExp e'
  let
    env' = case exp of
      Slv.Assignment name _ ->
        extendVars env (name, Forall ((S.toList . ftv) t) t)

      Slv.TypedExp (Slv.Solved _ _ (Slv.Assignment name _)) _ ->
        extendVars env (name, Forall ((S.toList . ftv) t) t)

      Slv.TypedExp (Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment name _)))) _
        -> extendVars env (name, Forall ((S.toList . ftv) t) t)

      Slv.Export (Slv.Solved _ _ (Slv.Assignment name _)) ->
        extendVars env (name, Forall ((S.toList . ftv) t) t)

      _ -> env

  (e' :) <$> inferExps env' xs


trd :: (a, b, c) -> c
trd (_, _, x) = x

mid :: (a, b, c) -> b
mid (_, b, _) = b

beg :: (a, b, c) -> a
beg (a, _, _) = a


solveTable :: Src.Table -> Src.AST -> Infer Slv.Table
solveTable table ast = solveTable' table ast

solveTable' :: Src.Table -> Src.AST -> Infer Slv.Table
solveTable' table ast@Src.AST { Src.aimports } = do
  -- First we resolve imports to update the env
  (inferredASTs, imports, adts, vars) <- solveImports table aimports

  -- Then we infer the ast
  env <- buildInitialEnv ast
  let envWithImports = env { envimports = imports
                           , envadts    = M.union (envadts env) adts
                           , envvars    = M.union (envvars env) vars
                           }

  inferredAST <- inferAST envWithImports ast

  case Slv.apath inferredAST of
    Just fp -> return $ M.insert fp inferredAST inferredASTs

    Nothing -> throwError $ InferError ASTHasNoPath NoReason



exportedExps :: Slv.AST -> Infer [(Slv.Name, Slv.Exp)]
exportedExps Slv.AST { Slv.aexps, Slv.apath } = case apath of
  Just p  -> mapM (bundleExports p) $ filter isExport aexps

  Nothing -> throwError $ InferError ASTHasNoPath NoReason

 where
  bundleExports _ exp = return $ case exp of
    e'@(Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)))) ->
      (n, e')
    (Slv.Solved _ _ (Slv.TypedExp e'@(Slv.Solved _ _ (Slv.Export (Slv.Solved _ _ (Slv.Assignment n _)))) _))
      -> (n, e')

  isExport :: Slv.Exp -> Bool
  isExport a = case a of
    (Slv.Solved _ _ (Slv.Export _)) -> True
    (Slv.Solved _ _ (Slv.TypedExp (Slv.Solved _ _ (Slv.Export _)) _)) -> True

    _                               -> False


exportedADTs :: Slv.AST -> [Slv.Name]
exportedADTs Slv.AST { Slv.aadts } =
  Slv.adtname <$> filter Slv.adtexported aadts


solveImports
  :: Src.Table -> [Src.Import] -> Infer (Slv.Table, Imports, ADTs, Vars)
solveImports table (imp : is) = do
  let modulePath = Src.getImportAbsolutePath imp


  (solvedAST, solvedTable, envADTs, envSolved) <-
    case AST.findAST table modulePath of
      Right ast -> do
        env         <- buildInitialEnv ast
        solvedTable <- solveTable' table ast
        solvedAST   <- case M.lookup modulePath solvedTable of
          Just a -> return a
          Nothing ->
            throwError $ InferError (ImportNotFound modulePath) NoReason
        return (solvedAST, solvedTable, envadts env, env)

      Left e -> throwError e

  exportedExps <- M.fromList <$> exportedExps solvedAST
  let exportedTypes    = mapM (return . Slv.getType) exportedExps

  let exportedADTNames = exportedADTs solvedAST
  let adtExports = M.filterWithKey (\k _ -> elem k exportedADTNames) envADTs

  let exportedConstructorNames =
        Slv.getConstructorName
          <$> concat
                (   Slv.adtconstructors
                <$> filter Slv.adtexported (Slv.aadts solvedAST)
                )
  let buildConstructorVars = M.filterWithKey
        (\k v -> k `elem` exportedConstructorNames)
        (envvars envSolved)

  (exports, vars) <- case (exportedTypes, imp) of
    (Just exports, Meta _ _ (Src.DefaultImport alias _ _)) -> do
      constructorVars <- mapM instantiate buildConstructorVars
      return
        ( M.fromList [(alias, TRecord exports False)]
        , M.fromList
          [(alias, Forall [] $ TRecord (M.union exports constructorVars) False)]
        )

    (Just exports, _) -> return (exports, buildConstructorVars)

    (Nothing, _) ->
      throwError $ InferError (ImportNotFound modulePath) NoReason


  (nextTable, nextExports, nextADTs, nextVars) <- solveImports table is

  mergedADTs <- do
    adtExports <- if M.intersection adtExports nextADTs == M.empty
      then return $ M.union adtExports nextADTs
      else throwError $ InferError FatalError NoReason
    case imp of
      Meta _ _ (Src.DefaultImport alias _ _) ->
        return $ M.mapKeys ((alias <> ".") <>) adtExports
      _ -> return adtExports

  return
    ( M.insert modulePath solvedAST (M.union solvedTable nextTable)
    , M.union exports nextExports
    , mergedADTs
    , M.union nextVars vars
    )

solveImports _ [] = return (M.empty, M.empty, M.empty, M.empty)


inferAST :: Env -> Src.AST -> Infer Slv.AST
inferAST env Src.AST { Src.aexps, Src.apath, Src.aimports, Src.aadts } = do
  inferredExps <- inferExps env aexps
  return Slv.AST { Slv.aexps    = inferredExps
                 , Slv.apath    = apath
                 , Slv.aadts    = updateADT <$> aadts
                 , Slv.aimports = updateImport <$> aimports
                 }

updateImport :: Src.Import -> Slv.Import
updateImport i = case i of
  Meta _ _ (Src.NamedImport   ns p fp) -> Slv.NamedImport ns p fp

  Meta _ _ (Src.DefaultImport n  p fp) -> Slv.DefaultImport n p fp


updateADT :: Src.ADT -> Slv.ADT
updateADT Src.ADT { Src.adtname, Src.adtparams, Src.adtconstructors, Src.adtexported }
  = Slv.ADT { Slv.adtname         = adtname
            , Slv.adtparams       = adtparams
            , Slv.adtconstructors = updateADTConstructor <$> adtconstructors
            , Slv.adtexported     = adtexported
            }

updateADTConstructor :: Src.Constructor -> Slv.Constructor
updateADTConstructor (Src.Constructor cname cparams) =
  Slv.Constructor cname $ updateTyping <$> cparams

-- TODO: Should get rid of that and handle the Either correctly where it is called
unifyToInfer :: Env -> Either TypeError Substitution -> Infer Substitution
unifyToInfer _ u = case u of
  Right s -> return s
  Left  e -> throwError $ InferError e NoReason


-- -- TODO: Make it call inferAST so that inferAST can return an (Infer TBD)
-- -- Well, or just adapt it somehow
runInfer :: Env -> Src.AST -> Either InferError Slv.AST
runInfer env ast =
  fst <$> runExcept (runStateT (inferAST env ast) Unique { count = 0 })

