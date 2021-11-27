{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Infer.Exp where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Maybe
import           Control.Monad.Except
import           Data.Foldable                  ( foldlM )
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Error.Error
import           Error.Context
import           Explain.Location
import           Data.List                      ( (\\)
                                                , partition
                                                , foldl'
                                                )
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Typing
import           Infer.Substitute
import           Infer.Unify
import           Infer.Instantiate
import           Infer.Scheme                   ( quantify )
import           Infer.Pattern
import           Infer.Pred
import           Infer.Placeholder
import           Infer.ToSolved
import qualified Utils.Tuple                   as T
import qualified Control.Monad                 as CM
import Debug.Trace
import Text.Show.Pretty
import AST.Solved (getType)
import Data.Int (Int8)


infer :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer env lexp = do
  let (Can.Canonical area exp) = lexp
      env'                     = pushExpToBT env lexp
  case exp of
    Can.LNum  _               -> do
      t <- newTVar Star
      let ps = [IsIn "Number" [t] Nothing]
      return (M.empty, ps, t, applyLitSolve lexp (ps :=> t))

    Can.LFloat _              -> return (M.empty, [], tFloat, applyLitSolve lexp ([] :=> tFloat))
    Can.LStr  _               -> return (M.empty, [], tStr, applyLitSolve lexp ([] :=> tStr))
    Can.LBool _               -> return (M.empty, [], tBool, applyLitSolve lexp ([] :=> tBool))
    Can.LUnit                 -> return (M.empty, [], tUnit, applyLitSolve lexp ([] :=> tUnit))
    Can.TemplateString _      -> inferTemplateString env' lexp

    Can.Var            _      -> inferVar env' lexp
    Can.Abs _ _               -> inferAbs env' lexp
    Can.App{}                 -> inferApp env' lexp
    Can.Assignment _ _        -> inferAssignment env' lexp
    Can.Do _                  -> inferDo env' lexp
    Can.Where      _ _        -> inferWhere env' lexp
    Can.Record _              -> inferRecord env' lexp
    Can.Access   _ _          -> inferAccess env' lexp
    Can.TypedExp{}            -> inferTypedExp env' lexp
    Can.ListConstructor  _    -> inferListConstructor env' lexp
    Can.TupleConstructor _    -> inferTupleConstructor env' lexp
    Can.Export           _    -> inferExport env' lexp
    Can.NameExport       name -> inferNameExport env' lexp
    Can.If{}                  -> inferIf env' lexp
    Can.Extern{}              -> inferExtern env' lexp
    Can.JSExp c               -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Solved ([] :=> t) area (Slv.JSExp c))


applyLitSolve :: Can.Exp -> Qual Type -> Slv.Exp
applyLitSolve (Can.Canonical area exp) qt = case exp of
  Can.LNum  v  -> Slv.Solved qt area $ Slv.LNum v
  Can.LFloat v -> Slv.Solved qt area $ Slv.LFloat v
  Can.LStr  v  -> Slv.Solved qt area $ Slv.LStr v
  Can.LBool v  -> Slv.Solved qt area $ Slv.LBool v
  Can.LUnit    -> Slv.Solved qt area Slv.LUnit

applyAbsSolve :: Can.Exp -> Slv.Solved Slv.Name -> [Slv.Exp] -> Qual Type -> Slv.Exp
applyAbsSolve (Can.Canonical loc _) param body qt = Slv.Solved qt loc $ Slv.Abs param body

applyAssignmentSolve :: Can.Exp -> Slv.Name -> Slv.Exp -> Qual Type -> Slv.Exp
applyAssignmentSolve (Can.Canonical loc _) n exp qt = Slv.Solved qt loc $ Slv.Assignment n exp


updateQualType :: Slv.Exp -> Qual Type -> Slv.Exp
updateQualType (Slv.Solved _ a e) qt = Slv.Solved qt a e


-- TODO: handle this properly so that code generation can rely on it
updatePattern :: Qual Type -> Can.Pattern -> Slv.Pattern
updatePattern qt (Can.Canonical area pat) = case pat of
  Can.PVar name             ->
    Slv.Solved qt area $ Slv.PVar name

  Can.PAny                  ->
    Slv.Solved qt area Slv.PAny

  Can.PCon name patterns    ->
    Slv.Solved qt area $ Slv.PCon name (updatePattern qt <$> patterns)

  Can.PNum    n             ->
    Slv.Solved ([] :=> tNumber) area $ Slv.PNum n

  Can.PStr    n             ->
    Slv.Solved ([] :=> tStr) area $ Slv.PStr n

  Can.PBool   n             ->
    Slv.Solved ([] :=> tBool) area $ Slv.PBool n

  Can.PRecord fieldPatterns ->
    Slv.Solved qt area $ Slv.PRecord (updatePattern qt <$> fieldPatterns)

  Can.PList   patterns      ->
    Slv.Solved qt area $ Slv.PList (updatePattern qt <$> patterns)

  Can.PTuple  patterns      ->
    Slv.Solved qt area $ Slv.PTuple (updatePattern qt <$> patterns)

  Can.PSpread pat'          ->
    Slv.Solved qt area $ Slv.PSpread (updatePattern qt pat')



-- INFER VAR

inferVar :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s = Forall [Star, Star] $ [] :=> (TRecord (M.fromList [(name, TGen 0)]) (Just $ TGen 1) `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (M.empty, ps, t, Slv.Solved (ps :=> t) area $ Slv.Var n)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    (ps :=> t) <- instantiate sc

    -- let ps' = ps
    let ps' = dedupePreds ps

    let e = Slv.Solved (ps' :=> t) area $ Slv.Var n
    e' <- insertVarPlaceholders env e ps'

    let ps'' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps'

    return (M.empty, ps'', t, e')

enhanceVarError :: Env -> Can.Exp -> Area -> CompilationError -> Infer Scheme
enhanceVarError env exp area (CompilationError e _) =
  throwError $ CompilationError e (Context (envCurrentPath env) area (envBacktrace env))


-- INFER NAME EXPORT

inferNameExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNameExport env exp@(Can.Canonical area (Can.NameExport name)) = do
  sc         <- catchError (lookupVar env name) (enhanceVarError env exp area)
  (ps :=> t) <- instantiate sc

  let e = Slv.Solved (ps :=> t) area $ Slv.NameExport name

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

  -- es'' <- mapM (updatePlaceholders env' True s) es

  return (s, apply s ps, t', applyAbsSolve l (Slv.Solved (apply s ps :=> paramType) area param) es (apply s ps :=> t'))


inferBody :: Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody env [e] = do
  (s, ps, t, e) <- infer env e
  e'            <- insertClassPlaceholders env e (dedupePreds ps)
  
  return (s, ps, t, [e'])

inferBody env (e : es) = do
  (s, ps, env', e') <- case e of
    Can.Canonical _ Can.TypedExp{} ->
      inferExplicitlyTyped env e

    _ -> do
      (s, (_, ps), env, e') <- inferImplicitlyTyped True env e
      return (s, ps, env, e')

  e''               <- insertClassPlaceholders env' e' (dedupePreds ps)
  e'''              <- updatePlaceholders env' (CleanUpEnv False [] [] []) True s e''
  (sb, ps', tb, eb) <- inferBody (updateBodyEnv s env') es

  let finalS = s `compose` sb

  return (finalS, apply finalS $ ps ++ ps', tb, e''' : eb)

-- Applies a substitution only to types in the env that are not a function.
-- This is needed for function bodies, so that we can define a function that is generic,
-- but other types should not. Say if we have a var xs = [] that has type List a and that
-- later in the function we can infer that this list is a List Number, then that substitution
-- should be applied to the env so that correct type can be inferred.
updateBodyEnv :: Substitution -> Env -> Env
updateBodyEnv s e =
  -- e { envVars = M.map (\sc@(Forall _ (_ :=> t)) -> apply s sc) (envVars e) }
  e { envVars = M.map (\sc@(Forall _ (_ :=> t)) -> if isFunctionType t then sc else apply s sc) (envVars e) }




-- INFER APP

inferApp' :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp, [(Substitution, [Pred], Type)])
inferApp' env app@(Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea _) final)) = do
  tv                               <- newTVar Star
  (s1, ps1, t1, eabs, skippedArgs) <- if isApp abs && not (isFinalApp abs)
    then inferApp' env abs
    else do
      (s1, ps1, t1, eabs) <- infer env abs
      return (s1, ps1, t1, eabs, [])
  (s2, ps2, t2, earg) <- infer (apply s1 env) arg

  let expForContext =
        if getLineFromStart argArea < getLineFromStart absArea then
          abs
        else
          arg

  s3                  <- contextualUnify env expForContext t1 (apply s1 t2 `fn` tv)

  let t          = apply s3 tv
  let s          = s3 `compose` s2 `compose` s1

  let solved = Slv.Solved (apply s (ps1 ++ ps2) :=> apply s t) area $ Slv.App eabs (updateQualType earg $ apply s (ps1 ++ ps2) :=> apply s t2) final

  let skippedArg = [ (s2, ps2, apply s t2) | isPlaceholder arg ]

  return (s, ps1 ++ ps2, t, solved, skippedArg <> skippedArgs)

inferApp :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp env app = do
  (s, ps, t, e, skipped) <- inferApp' env app
  let subst        = foldr compose s $ T.beg <$> skipped
  let preds        = concat (T.mid <$> skipped) <> ps
  let skippedTypes = T.lst <$> skipped
  let realType     = apply subst $ foldr fn t skippedTypes


  let appArea = Can.getArea app
  let preds' = (\(IsIn c ts _) -> IsIn c ts (Just appArea)) <$> preds

  return (subst, preds, realType, updateQualType e (preds :=> realType))


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

  let qs = uncurry (:=>) <$> zip elemPS elemTypes

  let updatedExp = Slv.Solved
        ([] :=> tStr)
        area
        (Slv.TemplateString ((\(t, e) -> updateQualType e (apply fullSubst t)) <$> zip qs elemExps))

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
  s2                <- catchError (contextualUnify env' e currentType t1) (const $ return M.empty)
  --  ^ We can skip this error as we mainly need the substitution. It would fail in inferExplicitlyTyped anyways.
  let s  = s2 `compose` s1
  let t2 = apply s t1
  return (s, currentPreds ++ ps1, t2, applyAssignmentSolve e name e1 ((currentPreds ++ ps1) :=> t2))



-- INFER EXPORT

inferExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer env exp
  return (s, ps, t, Slv.Solved (ps :=> t) area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor env (Can.Canonical area (Can.ListConstructor elems)) = case elems of
  [] -> do
    tv <- newTVar Star
    let t = tListOf tv
    return (M.empty, [], t, Slv.Solved ([] :=> t) area (Slv.ListConstructor []))

  elems -> do
    tv               <- newTVar Star

    (s', ps, t', es) <- foldlM
      (\(s, pss, t, lis) elem -> do
        (s', ps', t'', li) <- inferListItem (apply s env) (fromMaybe tv t) elem
        (s'', tr) <- case t of
          Nothing ->
            return (mempty, t'')

          Just t''' ->
            (, pickJSXChild t''' t'') <$> contextualUnify env elem (apply s' t''') t''

        let s''' = s `compose` s' `compose` s''
        return (s''', pss ++ ps', Just $ apply s''' tr, lis ++ [li])
      )
      (mempty, [], Nothing, [])
      elems

    let (Just t'') = t'

    s'' <- unify tv t''
    let s''' = s' `compose` s''

    let t = tListOf (apply s''' tv)

    return (s''', ps, t, Slv.Solved (ps :=> t) area (Slv.ListConstructor es))


inferListItem :: Env -> Type -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem env ty (Can.Canonical area li) = case li of
  Can.ListItem exp -> do
    (s1, ps, t, e) <- infer env exp
    return (s1, ps, t, Slv.Solved (ps :=> t) area $ Slv.ListItem e)

  Can.ListSpread exp -> do
    (s1, ps, t, e) <- infer env exp
    tv <- newTVar Star
    s2 <- unify t (tListOf tv)

    let s = s1 `compose` s2

    return (s, ps, apply s tv, Slv.Solved (apply s ps :=> apply s t) area $ Slv.ListSpread e)


pickJSXChild :: Type -> Type -> Type
pickJSXChild t1 t2 = case (t1, t2) of
  (TApp (TCon (TC "Element" _) _) _, TCon (TC "String" _) _) ->
    t1

  _ ->
    t2


-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor env (Can.Canonical area (Can.TupleConstructor elems)) = do
  inferredElems <-
    foldM
      (\(s, ps, ts, es) e -> do
          (s', ps', t', e') <- infer (apply s env) e
          return (s `compose` s', ps ++ ps', ts ++ [t'], es ++ [e'])
      ) (M.empty, [], [], []) elems

  let s         = (\(s, _, _, _) -> s) inferredElems
  let elemTypes = (\(_, _, t, _) -> t) inferredElems
  let elemEXPS  = (\(_, _, _, es) -> es) inferredElems
  let ps        = (\(_, ps, _, _) -> ps) inferredElems
  let tupleT    = getTupleCtor (length elems)
  let t         = foldl' TApp tupleT elemTypes


  return (s, ps, apply s t, Slv.Solved (ps :=> apply s t) area (Slv.TupleConstructor elemEXPS))


-- INFER RECORD

inferRecord :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  inferredFields <- mapM (inferRecordField env) fields

  let fieldSubsts = (\(s, _, _, _) -> s) <$> inferredFields
  let fieldTypes  = (\(_, _, t, _) -> t) <$> inferredFields
  let fieldEXPS   = (\(_, _, _, es) -> es) <$> inferredFields
  let fieldPS     = (\(_, ps, _, _) -> ps) <$> inferredFields
  let subst       = foldr compose M.empty fieldSubsts

  let fieldTypes' = filter (\(k, _) -> k /= "...") (concat fieldTypes)
  let spreads     = snd <$> filter (\(k, _) -> k == "...") (concat fieldTypes)

  let base = case spreads of
        (x : _) -> Just x
        _       -> Nothing

  (recordType, extraSubst) <- do
    (s, extraFields, newBase) <- case base of
      Just tBase -> do
        case tBase of
          TRecord fields base -> do
            s <- unify (TRecord (M.intersection fields $ M.fromList fieldTypes') base) (TRecord (M.fromList fieldTypes') base)
            return (s, fields, base)

          _                   -> do
            s <- unify tBase (TRecord (M.fromList fieldTypes') base)
            return (s, mempty, base)

      Nothing ->
        return (mempty, mempty, base)
    return (TRecord (M.fromList fieldTypes' <> extraFields) newBase, s)

  let allPS = concat fieldPS

  return (subst `compose` extraSubst, allPS, recordType, Slv.Solved (allPS :=> recordType) area (Slv.Record fieldEXPS))


inferRecordField :: Env -> Can.Field -> Infer (Substitution, [Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField env (Can.Canonical area field) = case field of
  Can.Field (name, exp) -> do
    (s, ps, t, e) <- infer env exp
    return (s, ps, [(name, t)], Slv.Solved (ps :=> t) area $ Slv.Field (name, e))

  Can.FieldSpread exp -> do
    (s, ps, t, e) <- infer env exp
    case t of
      TRecord _ _ ->
        return (s, ps, [("...", t)], Slv.Solved (ps :=> t) area $ Slv.FieldSpread e)

      TVar _      ->
        return (s, ps, [("...", t)], Slv.Solved (ps :=> t) area $ Slv.FieldSpread e)

      _ ->
        throwError $ CompilationError
          (WrongSpreadType $ show t)
          (Context (envCurrentPath env) (Can.getArea exp) (envBacktrace env))



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

    let e = Slv.Solved (ps :=> t) area $ Slv.Var (ns <> field)
    e' <- insertVarPlaceholders env e ps

    return (M.empty, ps, t, e')
inferNamespaceAccess env _ = throwError $ CompilationError FatalError NoContext



-- INFER FIELD ACCESS

inferFieldAccess :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess env fa@(Can.Canonical area (Can.Access rec@(Can.Canonical _ re) abs@(Can.Canonical _ (Can.Var ('.' : name)))))
  = do
    tv                  <- newTVar Star
    (s1, _  , t1, eabs) <- infer env abs
    (s2, ps2, t2, earg) <- infer env rec

    s3                  <- contextualUnify env fa t1 (t2 `fn` tv)

    let s      = s3 `compose` s2 `compose` s1
    let t      = apply s tv

    let solved = Slv.Solved (ps2 :=> t) area (Slv.Access earg eabs)

    return (s, ps2, t, solved)



-- INFER IF

inferIf :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf env exp@(Can.Canonical area (Can.If cond truthy falsy)) = do
  (s1, ps1, tcond  , econd  ) <- infer env cond
  (s2, ps2, ttruthy, etruthy) <- infer (apply s1 env) truthy
  (s3, ps3, tfalsy , efalsy ) <- infer (apply (s1 `compose` s2) env) falsy

  s4                          <- contextualUnify (pushExpToBT env cond) cond tcond tBool
  s5                          <- contextualUnify (pushExpToBT env falsy) falsy ttruthy tfalsy

  let s = s4 `compose` s5 `compose` s1 `compose` s2 `compose` s3
  let t = apply s ttruthy

  return (s, ps1 ++ ps2 ++ ps3, t, Slv.Solved ((ps1 ++ ps2 ++ ps3) :=> t) area (Slv.If econd etruthy efalsy))



-- INFER DEFINE IN

inferDo :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferDo env (Can.Canonical area (Can.Do exps)) = do
  (s, ps, t, exps') <- inferBody env exps

  return (s, ps, t, Slv.Solved (ps :=> t) area (Slv.Do exps'))


-- INFER WHERE

inferWhere :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e)          <- infer env exp
  tv                     <- newTVar Star
  (pss, issSubstitution) <- foldM
    (\(res, currSubst) is -> do
      r@(subst, _, _) <- inferBranch (apply currSubst env) (apply currSubst tv) t is
      return (res <> [r], subst `compose` currSubst)
    )
    ([], s)
    iss

  let ps' = concat $ T.mid <$> pss
  s' <- contextualUnifyElems env $ zip iss (apply issSubstitution . Slv.getType . T.lst <$> pss)

  let s''  = s' `compose` issSubstitution

  let iss = (\(Slv.Solved t a is) -> Slv.Solved (apply s'' t) a is) . T.lst <$> pss
  let wher = Slv.Solved (apply s (ps ++ ps') :=> apply s'' tv) area $ Slv.Where (updateQualType e (apply s'' ps :=> apply s'' t)) iss
  return (s'', ps ++ ps', apply s'' tv, wher)


inferBranch :: Env -> Type -> Type -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch env tv t (Can.Canonical area (Can.Is pat exp)) = do
  (pat', ps, vars, t') <- inferPattern env pat
  s                    <- contextualUnify env exp t' t
  (s', ps', t'', e')   <- infer (apply s $ mergeVars env vars) exp
  -- s''                  <- contextualUnify env exp tv  t''
  s''                  <- contextualUnify env exp tv (apply (s `compose` s') t'')

  let subst = s `compose` s' `compose` s''

  return
    ( subst
    , ps ++ ps'
    , Slv.Solved ((ps ++ ps') :=> apply subst (t' `fn` tv)) area
      $ Slv.Is (updatePatternTypes subst vars pat') (updateQualType e' (ps' :=> apply subst t''))
    )

updatePatternTypes :: Substitution -> Vars -> Slv.Pattern -> Slv.Pattern
updatePatternTypes s vars pat = case pat of
  Slv.Solved t area (Slv.PCon n pats) ->
    Slv.Solved (apply s t) area (Slv.PCon n (updatePatternTypes s vars <$> pats))

  Slv.Solved t area (Slv.PRecord fields) ->
    Slv.Solved (apply s t) area (Slv.PRecord (updatePatternTypes s vars <$> fields))

  Slv.Solved t area (Slv.PList items) ->
    Slv.Solved (apply s t) area (Slv.PList (updatePatternTypes s vars <$> items))

  Slv.Solved t area (Slv.PTuple items) ->
    Slv.Solved (apply s t) area (Slv.PTuple (updatePatternTypes s vars <$> items))

  Slv.Solved t area (Slv.PSpread pat) ->
    Slv.Solved (apply s t) area (Slv.PSpread (updatePatternTypes s vars pat))

  Slv.Solved t area (Slv.PVar n) ->
    case M.lookup n vars of
      Just (Forall _ qt) ->
        Slv.Solved (apply s qt) area (Slv.PVar n)

      Nothing ->
        Slv.Solved (apply s t) area (Slv.PVar n)

  Slv.Solved t area p ->
    Slv.Solved (apply s t) area p


-- INFER TYPEDEXP

inferTypedExp :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp env e@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  (ps :=> t)        <- instantiate sc

  (s1, ps1, t1, e1) <- infer env exp
  s2                <- contextualUnify env e t t1

  return (s1 `compose` s2, ps, t, Slv.Solved (ps :=> t) area (Slv.TypedExp (updateQualType e1 (ps1 :=> t)) (updateTyping typing) sc))


inferExtern :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExtern env (Can.Canonical area (Can.Extern scheme name originalName)) = do
  qt@(ps :=> t) <- instantiate scheme
  return (mempty, ps, t, Slv.Solved qt area (Slv.Extern qt name originalName))


type Ambiguity = (TVar, [Pred])

ambiguities :: [TVar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [ (v, filter (elem v . ftv) ps) | v <- ftv ps \\ vs ]

split :: Bool -> Env -> [TVar] -> [TVar] -> [Pred] -> Infer ([Pred], [Pred], Substitution)
split mustCheck env fs gs ps = do
  ps' <- reduce env ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  let as = ambiguities (fs ++ gs) rs
  if mustCheck && not (null as) then do
    -- if we have ambiguities we try to resolve them with default instances
    (s, rs') <- tryDefaults env rs
    (sDef', rs'') <- tryDefaults env (apply s rs')
    let (ds', rs''') = partition (all (`elem` fs) . ftv) (apply sDef' ds ++ rs'')

    -- and then compute the potential leftover ambiguities
    let as' = ambiguities (fs ++ gs) rs'''
    if not (null as') then
      case head as of
        (_, IsIn c ts (Just area):_) ->
          throwError $ CompilationError (AmbiguousType (head as)) (Context (envCurrentPath env) area (envBacktrace env))

        _ ->
          throwError $ CompilationError (AmbiguousType (head as)) NoContext
    else
      return (ds', rs''', s)
  else
    return (ds, rs, mempty)


tryDefaults :: Env -> [Pred] -> Infer (Substitution, [Pred])
tryDefaults env ps = case ps of
  (p : next) -> case p of
    IsIn "Number" [TVar tv] _ -> do
      (nextSubst, nextPS) <- tryDefaults env next
      let s = M.singleton tv tInteger
      return (s `compose` nextSubst, nextPS)

    IsIn "Eq" [t] _ -> do
      (nextSubst, nextPS) <- tryDefaults env next

      let vars = getTypeVarsInType t
      if null vars then
        return (nextSubst, nextPS)
      else do
        let tvs  = getTV <$> vars
            tvs' = filter (`M.notMember` nextSubst) tvs
            s    = M.fromList $ zip tvs' (tUnit <$ tvs')
        return (nextSubst `compose` s, nextPS)

    _ -> do
      maybeFound <- findInst env p
      case maybeFound of
        Just (Instance (instancePreds :=> pred) _) -> do
          -- if the instance found has predicates eg. (Show a, Show b) => Show #[a, b]
          -- we unify the found pred with the one we try to resolve to get a mapping of
          -- the actual instantiated types and add this to the list of predicates to resolve.
          s                   <- unify pred p
          (nextSubst, nextPS) <- tryDefaults env (next ++ apply s instancePreds)
          return (nextSubst, nextPS)

        Nothing -> do
          (nextSubst, nextPS) <- tryDefaults env next
          return (nextSubst, p : nextPS)

  [] ->
    return (M.empty, [])



dedupePreds :: [Pred] -> [Pred]
dedupePreds = reverse . dedupePreds' []

dedupePreds' :: [Pred] -> [Pred] -> [Pred]
dedupePreds' acc ps = case ps of
  (p : next) -> case p of
    IsIn cls ts _ ->
      if any (\(IsIn cls' ts' _) -> cls == cls' && ts == ts') acc then
        dedupePreds' acc next
      else
        dedupePreds' (p:acc) next

  [] ->
    acc


inferImplicitlyTyped :: Bool -> Env -> Can.Exp -> Infer (Substitution, ([Pred], [Pred]), Env, Slv.Exp)
inferImplicitlyTyped isLet env exp@(Can.Canonical area _) = do
  (env', tv) <- case Can.getExpName exp of
    Just n -> case M.lookup n (envVars env) of
      Just sc -> do
        _ :=> t' <- instantiate sc
        return (env, t')
      --  ^ if a var is already present we don't override its type with a fresh var.
      Nothing                   -> do
        tv <- newTVar Star
        return (extendVars env (n, Forall [] $ [] :=> tv), tv)
    Nothing -> do
      tv <- newTVar Star
      return (env, tv)

  (s1, ps1, t1, _) <- infer env' exp

  -- We need to update the env again in case the inference of the function resulted in overloading so that
  -- we can have the predicates to generate the correct placeholders when fetching the var from the env
  env'' <- case Can.getExpName exp of
    Just n ->
      return $ extendVars env' (n, Forall [] $ ps1 :=> t1)

    Nothing ->
      return env'

  -- Once we have gattered clues we update the env types and infer it again
  -- to handle recursion errors. We probably need to improve that solution at
  -- some point!
  (s, ps, t, e) <- infer (apply s1 env'') exp

  s'            <- contextualUnify env'' exp (apply s tv) t
  let s'' = s `compose` s1 `compose` s'
      ps' = apply s'' ps
      t'  = apply s'' tv
      fs  = ftv (apply s'' env'')
      vs  = ftv t'
      gs  = vs \\ fs
  (ds, rs, _) <- catchError
    (split False env'' fs vs ps')
    (\case
      (CompilationError e NoContext) -> throwError $ CompilationError e (Context (envCurrentPath env) area (envBacktrace env))
      (CompilationError e c) -> throwError $ CompilationError e c
    )


  (ds', sDefaults) <-
    if not isLet && not (null (rs ++ ds)) && not (Can.isNamedAbs exp) then do
      (sDef, rs')   <- tryDefaults env'' (rs ++ ds)
          -- TODO: tryDefaults should handle such a case so that we only call it once.
          -- What happens is that defaulting may solve some types ( like Number a -> Integer )
          -- and then it could resolve instances like Show where before we still had a type var
          -- but after the first pass we have Integer instead.
      (sDef', rs'') <- tryDefaults env'' (apply sDef rs')
      CM.unless (null rs'') $ throwError $ CompilationError
        (AmbiguousType (TV "-" Star, rs'))
        (Context (envCurrentPath env) area (envBacktrace env))
      return ([], sDef' `compose` sDef)
    else do
      return (ds ++ rs, M.empty)

  let ds'' = dedupePreds ds'
  let sFinal = sDefaults `compose` s''

  let sc =
        if isLet then
          Forall [] $ apply sFinal (ds'' :=> t')
        else
          quantify fs $ apply sFinal (ds'' :=> t')

  case Can.getExpName exp of
    Just n  ->
      return (sFinal, (ds'', ds''), extendVars env (n, sc), updateQualType e (ds'' :=> t'))

    Nothing ->
      return (sFinal, (ds'', ds''), env, updateQualType e (ds'' :=> t'))



inferExplicitlyTyped :: Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped env canExp@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  qt@(qs :=> t') <- instantiate sc

  let env' = case Can.getExpName exp of
        Just n  -> extendVars env (n, sc)
        Nothing -> env

  (s, ps, t, e) <- infer env' exp
  s''           <- catchError (contextualUnify env canExp t t') (flipUnificationError . limitContextArea 2)
  let s' = s'' `compose` s `compose` s''

  let qs'  = apply s' qs
      t''  = apply s' t
      t''' = mergeRecords (apply s' t') t''
      fs   = ftv (apply s' env)
      gs   = ftv t''' \\ fs
      sc'  = quantify gs (qs' :=> t''')
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' ps)
  (ds, rs, substDefaultResolution) <- catchError
    (split True env' fs gs ps')
    (\case
      (CompilationError e NoContext) -> throwError $ CompilationError e (Context (envCurrentPath env) area (envBacktrace env))
      (CompilationError e c) -> throwError $ CompilationError e c
    )

  qs'' <- dedupePreds <$> getAllParentPreds env (dedupePreds qs')

  let scCheck  = quantify (ftv t''') (qs' :=> apply substDefaultResolution t''')
  if sc /= scCheck then
    throwError $ CompilationError (SignatureTooGeneral sc scCheck) (Context (envCurrentPath env') area (envBacktrace env))
  else if not (null rs) then
    throwError $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) area (envBacktrace env))
  else do
    let e'   = updateQualType e (ds :=> t''')

    let qt'  = qs'' :=> t'''
    let sc'' = quantify gs qt'
    -- let sc'' = quantify (ftv qt') qt'
    let env'' = case Can.getExpName exp of
          Just n  -> extendVars env' (n, sc'')
          Nothing -> env'

    return (substDefaultResolution `compose` s', qs'', env'', Slv.Solved (qs :=> t') area (Slv.TypedExp e' (updateTyping typing) sc))

inferExplicitlyTyped env _ = undefined



inferExps :: Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps env []       = return ([], env)

inferExps env (e : es) = do
  (e' , env'   ) <- catchError (inferExp (pushExpToBT env e) e) (recordError env e)
  (es', nextEnv) <- inferExps (resetBT env') es

  case e' of
    Just e'' -> return (e'' : es', nextEnv)
    Nothing  -> return (es', nextEnv)


inferExp :: Env -> Can.Exp -> Infer (Maybe Slv.Exp, Env)
inferExp env (Can.Canonical area (Can.TypeExport name)) =
  return (Just (Slv.Untyped area (Slv.TypeExport name)), env)
inferExp env e = do
  (s, ps, env', e') <- upgradeContext env (Can.getArea e) $ case e of
    Can.Canonical _ Can.TypedExp{} ->
      inferExplicitlyTyped env e

    _ -> do
      (s, (ds, ps), env', e') <- inferImplicitlyTyped False env e
      return (s, ps, env', e')

  e''  <- insertClassPlaceholders env e' ps
  e''' <- updatePlaceholders env (CleanUpEnv False [] [] []) False s e''

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
