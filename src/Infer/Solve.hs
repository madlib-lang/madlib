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
import qualified AST
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
import           Debug.Trace                    ( trace )
import           Text.Show.Pretty               ( ppShow )


infer :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
infer env lexp =
  let (Meta _ area exp) = lexp
  in
    case exp of
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
      Src.JSExp c ->
        return
          ( M.empty
          , TVar $ TV "a"
          , Slv.Solved (TVar $ TV "a") area (Slv.JSExp c)
          )


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
  let t = apply s1 tv `TArr` t1
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
      let t = TComp "List" [TVar $ TV "a"]
      in  return (M.empty, t, Slv.Solved t loc (Slv.ListConstructor []))

    elems -> do
      inferred <- mapM (inferListItem env) elems
      let (_, t1, _) = head inferred
      let t          = TComp "List" [t1]
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
      TComp "List" [t'] -> return (s, t', Slv.ListSpread e)

      TVar _ -> return (s, t, Slv.ListSpread e)

      _ -> throwError $ InferError (UnknownType $ show t) NoReason



-- INFER RECORD

inferRecord :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferRecord env exp = do
  let Meta _ area (Src.Record fields) = exp

  inferred <- mapM (inferRecordField env) fields
  let inferredFields = trd <$> inferred
      recordType     = TRecord (M.fromList $ concat $ mid <$> inferred) False
  return
    ( M.empty
    , recordType
    , Slv.Solved recordType area (Slv.Record inferredFields)
    )

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
      Just t ->
        let solved = Slv.Solved t area (Slv.FieldAccess recordExp fieldExp)
        in  return (fieldSubst, t, solved)

      Nothing -> do
        tv <- newTVar
        s3 <- case unify (apply recordSubst fieldType) (TArr recordType tv) of
          Right s -> return s
          Left  e -> throwError $ InferError e NoReason

        let t          = apply s3 tv
        let rs         = recordSubstForVar re fieldType

        let recordExp' = updateType recordExp (apply s3 recordType)
        let solved = Slv.Solved t area (Slv.FieldAccess recordExp' fieldExp)

        return (s3 `compose` rs, t, solved)

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
    let newTP = case pattern of
          (Meta _ _ (Src.PRecord fields)) ->
            case find (isSpread) (snd <$> M.toList fields) of
              Just (Meta _ _ (Src.PSpread (Meta _ _ (Src.PVar n)))) ->
                case M.lookup (TV n) se of
                  Just (TRecord ff _) ->
                    let (TRecord ff' _) = tp in TRecord (M.union ff ff') True
                  Nothing -> tp

              Nothing -> tp
          _ -> tp
         where
          isSpread fPat = case fPat of
            (Meta _ _ (Src.PSpread _)) -> True
            _                          -> False

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

  buildPatternType :: Env -> Src.Pattern -> Infer Type
  buildPatternType e@Env { envvars } pattern@(Meta _ area pat) = case pat of
    Src.PVar  _        -> newTVar

    Src.PCon  "String" -> return $ TCon CString
    Src.PCon  "Bool"   -> return $ TCon CBool
    Src.PCon  "Num"    -> return $ TCon CNum

    Src.PStr  _        -> return $ TCon CString
    Src.PBool _        -> return $ TCon CBool
    Src.PNum  _        -> return $ TCon CNum

    Src.PAny           -> newTVar

    Src.PRecord fields ->
      let fieldsWithoutSpread = M.filterWithKey (\k v -> k /= "...") fields
      in  (\fields -> TRecord fields True) . M.fromList <$> mapM
            (\(k, v) -> (k, ) <$> buildPatternType e v)
            (M.toList fieldsWithoutSpread)

    Src.PCtor n as -> do
      (Forall fv ctor) <- case M.lookup n envvars of
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
    Src.PList   []      -> return $ TComp "List" [TVar $ TV "a"]
    Src.PList patterns ->
      TComp "List" . (: []) <$> buildPatternType e (head patterns)

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

      (Src.PList items, TComp "List" [t]) ->
        foldrM (\p e' -> generateIsEnv t e' p) e items
      (Src.PList items   , t) -> foldrM (\p e' -> generateIsEnv t e' p) e items

      (Src.PCtor cname as, t) -> do
        ctor <- findConstructor cname
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


addPatternReason
  :: Env -> Src.Exp -> Src.Pattern -> Area -> InferError -> Infer (Substitution)
addPatternReason env whereExp pattern area (InferError e _) =
  throwError $ InferError
    e
    (Reason (PatternTypeError whereExp pattern) (envcurrentpath env) area)



-- INFER TYPEDEXP

inferTypedExp :: Env -> Src.Exp -> Infer (Substitution, Type, Slv.Exp)
inferTypedExp env (Meta _ area (Src.TypedExp exp typing)) = do
  t <- typingToType typing
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
    , t
    , Slv.Solved t area (Slv.TypedExp (updateType e1 t) (updateTyping typing))
    )


typingToType :: Src.Typing -> Infer Type
typingToType (Meta _ _ (Src.TRSingle t))
  | t == "Num"       = return $ TCon CNum
  | t == "Bool"      = return $ TCon CBool
  | t == "String"    = return $ TCon CString
  | t == "Void"      = return $ TCon CVoid
  | isLower $ head t = return $ TVar $ TV t
  | otherwise        = return $ TComp t []

typingToType (Meta _ _ (Src.TRComp t ts)) = do
  params <- mapM typingToType ts
  return $ TComp t params

typingToType (Meta _ _ (Src.TRArr l r)) = do
  l' <- typingToType l
  r' <- typingToType r
  return $ TArr l' r'

typingToType (Meta _ _ (Src.TRRecord f)) = do
  f' <- mapM typingToType f
  return $ TRecord f' False



inferExps :: Env -> [Src.Exp] -> Infer [Slv.Exp]
inferExps _   []       = return []

inferExps env [exp   ] = (: []) . trd <$> infer env exp

inferExps env (e : xs) = do
  (_, t, e') <- infer env e
  let exp = Slv.extractExp e'
  let
    env' = case exp of
      -- TODO: We need to add a case where that name is already in the env.
      -- Reassigning a name should not be allowed.
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


inferAST :: FilePath -> Src.Table -> Src.AST -> Infer Slv.Table
inferAST rootPath table ast@Src.AST { Src.aimports } = do
  env                     <- buildInitialEnv ast

  (inferredASTs, imports) <- resolveImports rootPath table aimports
  let envWithImports = env { envimports = imports }

  inferredAST <- inferASTExps envWithImports ast

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


-- -- TODO: Needs to handle data types as well.
resolveImports
  :: FilePath -> Src.Table -> [Src.Import] -> Infer (Slv.Table, Imports)
resolveImports root table (imp : is) = do
  let modulePath = case imp of
        Meta _ _ (Src.NamedImport   _ n) -> n

        Meta _ _ (Src.DefaultImport _ n) -> n

  let path = root <> modulePath <> ".mad"

  solvedAST <- case AST.findAST table path of
    Right ast -> do
      env <- buildInitialEnv ast
      inferASTExps env ast

    Left e -> throwError e

  exportedExps <- M.fromList <$> exportedExps solvedAST
  let exportedTypes = mapM (return . Slv.getType) exportedExps

  exports <- case (exportedTypes, imp) of
    (Just exports, Meta _ _ (Src.DefaultImport alias _)) ->
      return $ M.fromList [(alias, TRecord exports False)]

    (Just exports, _) -> return exports

    (Nothing     , _) -> throwError $ InferError (ImportNotFound path) NoReason

  (nextTable, nextExports) <- resolveImports root table is

  return (M.insert path solvedAST nextTable, M.union exports nextExports)

resolveImports _ _ [] = return (M.empty, M.empty)


inferASTExps :: Env -> Src.AST -> Infer Slv.AST
inferASTExps env Src.AST { Src.aexps, Src.apath, Src.aimports, Src.aadts } = do
  inferredExps <- inferExps env aexps
  return Slv.AST { Slv.aexps    = inferredExps
                 , Slv.apath    = apath
                 , Slv.aadts    = updateADT <$> aadts
                 , Slv.aimports = updateImport <$> aimports
                 }

updateImport :: Src.Import -> Slv.Import
updateImport i = case i of
  Meta _ _ (Src.NamedImport   ns fp) -> Slv.NamedImport ns fp

  Meta _ _ (Src.DefaultImport n  fp) -> Slv.DefaultImport n fp


updateADT :: Src.ADT -> Slv.ADT
updateADT Src.ADT { Src.adtname, Src.adtparams, Src.adtconstructors } = Slv.ADT
  { Slv.adtname         = adtname
  , Slv.adtparams       = adtparams
  , Slv.adtconstructors = updateADTConstructor <$> adtconstructors
  }

updateADTConstructor :: Src.ADTConstructor -> Slv.ADTConstructor
updateADTConstructor Src.ADTConstructor { Src.adtcname, Src.adtcargs } =
  Slv.ADTConstructor { Slv.adtcname = adtcname
                     , Slv.adtcargs = (updateTyping <$>) <$> adtcargs
                     }

-- TODO: Should get rid of that and handle the Either correctly where it is called
unifyToInfer :: Env -> Either TypeError Substitution -> Infer Substitution
unifyToInfer env u = case u of
  Right s -> return s
  Left  e -> throwError $ InferError e NoReason


-- -- TODO: Make it call inferAST so that inferAST can return an (Infer TBD)
-- -- Well, or just adapt it somehow
runInfer :: Env -> Src.AST -> Either InferError Slv.AST
runInfer env ast =
  fst <$> runExcept (runStateT (inferASTExps env ast) Unique { count = 0 })

