{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
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
import           Infer.EnvUtils
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
import qualified Data.Set as Set
import Run.Options


infer :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer options env lexp = do
  let (Can.Canonical area exp) = lexp
  case exp of
    Can.LNum  _               -> do
      t <- newTVar Star
      let ps = [IsIn "Number" [t] Nothing]
      return (M.empty, ps, t, applyLitSolve lexp (ps :=> t))

    Can.LFloat _              -> return (M.empty, [], tFloat, applyLitSolve lexp ([] :=> tFloat))
    Can.LStr  _               -> return (M.empty, [], tStr, applyLitSolve lexp ([] :=> tStr))
    Can.LChar  _              -> return (M.empty, [], tChar, applyLitSolve lexp ([] :=> tChar))
    Can.LBool _               -> return (M.empty, [], tBool, applyLitSolve lexp ([] :=> tBool))
    Can.LUnit                 -> return (M.empty, [], tUnit, applyLitSolve lexp ([] :=> tUnit))
    Can.TemplateString _      -> inferTemplateString options env lexp

    Can.Var            _      -> inferVar options env lexp
    Can.Abs _ _               -> inferAbs options env lexp
    Can.App{}                 -> inferApp options env lexp
    Can.Assignment _ _        -> inferAssignment options env lexp
    Can.Do _                  -> inferDo options env lexp
    Can.Where      _ _        -> inferWhere options env lexp
    Can.Record _              -> inferRecord options env lexp
    Can.Access   _ _          -> inferAccess options env lexp
    Can.TypedExp{}            -> inferTypedExp options env lexp
    Can.ListConstructor  _    -> inferListConstructor options env lexp
    Can.TupleConstructor _    -> inferTupleConstructor options env lexp
    Can.Export           _    -> inferExport options env lexp
    Can.NameExport       _    -> inferNameExport env lexp
    Can.If{}                  -> inferIf options env lexp
    Can.Extern{}              -> inferExtern env lexp
    Can.JSExp c               -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Typed ([] :=> t) area (Slv.JSExp c))


applyLitSolve :: Can.Exp -> Qual Type -> Slv.Exp
applyLitSolve (Can.Canonical area exp) qt = case exp of
  Can.LNum  v  -> Slv.Typed qt area $ Slv.LNum v
  Can.LFloat v -> Slv.Typed qt area $ Slv.LFloat v
  Can.LStr  v  -> Slv.Typed qt area $ Slv.LStr v
  Can.LChar  v -> Slv.Typed qt area $ Slv.LChar v
  Can.LBool v  -> Slv.Typed qt area $ Slv.LBool v
  Can.LUnit    -> Slv.Typed qt area Slv.LUnit

applyAbsSolve :: Can.Exp -> Slv.Solved Slv.Name -> [Slv.Exp] -> Qual Type -> Slv.Exp
applyAbsSolve (Can.Canonical loc _) param body qt = Slv.Typed qt loc $ Slv.Abs param body

applyAssignmentSolve :: Can.Exp -> Slv.Name -> Slv.Exp -> Qual Type -> Slv.Exp
applyAssignmentSolve (Can.Canonical loc _) n exp qt = Slv.Typed qt loc $ Slv.Assignment n exp


updateQualType :: Slv.Exp -> Qual Type -> Slv.Exp
updateQualType (Slv.Typed _ a e) qt = Slv.Typed qt a e


-- TODO: handle this properly so that code generation can rely on it
updatePattern :: Qual Type -> Can.Pattern -> Slv.Pattern
updatePattern qt (Can.Canonical area pat) = case pat of
  Can.PVar name             ->
    Slv.Typed qt area $ Slv.PVar name

  Can.PAny                  ->
    Slv.Typed qt area Slv.PAny

  Can.PCon name patterns    ->
    Slv.Typed qt area $ Slv.PCon name (updatePattern qt <$> patterns)

  Can.PNum    n             ->
    Slv.Typed ([] :=> tNumber) area $ Slv.PNum n

  Can.PStr    n             ->
    Slv.Typed ([] :=> tStr) area $ Slv.PStr n

  Can.PChar    n             ->
    Slv.Typed ([] :=> tChar) area $ Slv.PChar n

  Can.PBool   n             ->
    Slv.Typed ([] :=> tBool) area $ Slv.PBool n

  Can.PRecord fieldPatterns ->
    Slv.Typed qt area $ Slv.PRecord (updatePattern qt <$> fieldPatterns)

  Can.PList   patterns      ->
    Slv.Typed qt area $ Slv.PList (updatePattern qt <$> patterns)

  Can.PTuple  patterns      ->
    Slv.Typed qt area $ Slv.PTuple (updatePattern qt <$> patterns)

  Can.PSpread pat'          ->
    Slv.Typed qt area $ Slv.PSpread (updatePattern qt pat')



-- INFER VAR

inferVar :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar options env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s = Forall [Star, Star] $ [] :=> (TRecord (M.fromList [(name, TGen 0)]) (Just $ TGen 1) `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (M.empty, ps, t, Slv.Typed (ps :=> t) area $ Slv.Var n False)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    -- liftIO $ putStrLn $ "SC("<>n<>"): " <> ppShow sc
    (ps :=> t) <- instantiate sc

    let ps' = dedupePreds ps

    let e = Slv.Typed (ps' :=> t) area $ Slv.Var n (isConstructor env n)
    e' <- insertVarPlaceholders options env e ps'

    let ps'' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps'

    return (M.empty, ps'', t, e')

enhanceVarError :: Env -> Can.Exp -> Area -> CompilationError -> Infer Scheme
enhanceVarError env _ area (CompilationError e _) =
  throwError $ CompilationError e (Context (envCurrentPath env) area)


-- INFER NAME EXPORT

inferNameExport :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNameExport env exp@(Can.Canonical area (Can.NameExport name)) = do
  sc         <- catchError (lookupVar env name) (enhanceVarError env exp area)
  (ps :=> t) <- instantiate sc

  let e = Slv.Typed (ps :=> t) area $ Slv.NameExport name

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


inferAbs :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAbs options env l@(Can.Canonical _ (Can.Abs p@(Can.Canonical area param) body)) = do
  tv             <- newTVar Star
  env'           <- extendAbsEnv env tv p
  (s, ps, t, es) <- inferBody options env' body
  let t'        = apply s (tv `fn` t)
      paramType = apply s tv

  return (s, apply s ps, t', applyAbsSolve l (Slv.Typed (apply s ps :=> paramType) area param) es (apply s ps :=> t'))


inferBody :: Options -> Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody options env [e] = do
  (s, ps, t, e) <- infer options env e
  e'            <- insertClassPlaceholders options env e (dedupePreds ps)
  
  return (s, ps, t, [e'])

inferBody options env (e : es) = do
  (s, (returnPreds, placeholderPreds), env', e') <- case  e of
    Can.Canonical _ Can.TypedExp{} -> do
      (s, ps, env', e') <- inferExplicitlyTyped options True env e
      return (s, (ps, ps), env', e')

    _ -> do
      (s, allPreds, env, e') <- inferImplicitlyTyped options True env e
      return (s, allPreds, env, e')

  e''               <- insertClassPlaceholders options env' e' (dedupePreds placeholderPreds)

  (sb, ps', tb, eb) <- inferBody options (updateBodyEnv s env') es

  let finalS = s `compose` sb

  return (finalS, apply finalS $ returnPreds ++ ps', tb, e'' : eb)

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

inferApp :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp options env (Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea _) final)) = do
  tv                  <- newTVar Star
  (s1, ps1, t1, eabs) <- infer options env abs
  (s2, ps2, t2, earg) <- infer options (apply s1 env) arg

  let expForContext =
        if getLineFromStart argArea < getLineFromStart absArea then
          abs
        else
          arg

  s3 <- contextualUnify env expForContext t1 (apply s1 t2 `fn` tv)
  let t = apply s3 tv
  let s = s3 `compose` s2 `compose` s1

  let solved = Slv.Typed (apply s (ps1 ++ ps2) :=> apply s t) area $ Slv.App eabs (updateQualType earg $ apply s (ps1 ++ ps2) :=> apply s t2) final

  return (s, ps1 ++ ps2, t, solved)



-- INFER TEMPLATE STRINGS

inferTemplateString :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTemplateString options env (Can.Canonical area (Can.TemplateString exps)) = do
  inferred <- mapM (infer options env) exps

  let elemSubsts = (\(s, _, _, _) -> s) <$> inferred
  let elemTypes  = (\(_, _, t, _) -> t) <$> inferred
  let elemExps   = (\(_, _, _, es) -> es) <$> inferred
  let elemPS     = (\(_, ps, _, _) -> ps) <$> inferred

  ss <- mapM (\(exp, t) -> contextualUnify env exp t tStr) (zip exps elemTypes)

  let fullSubst = foldl' compose M.empty (elemSubsts <> ss)

  let qs = uncurry (:=>) <$> zip elemPS elemTypes

  let updatedExp = Slv.Typed
        ([] :=> tStr)
        area
        (Slv.TemplateString ((\(t, e) -> updateQualType e (apply fullSubst t)) <$> zip qs elemExps))

  return (fullSubst, concat elemPS, tStr, updatedExp)



-- INFER ASSIGNMENT

inferAssignment :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAssignment options env e@(Can.Canonical _ (Can.Assignment name exp)) = do
  currentScheme <- case M.lookup name (envVars env) of
    Just sc -> return sc
    _       -> (\t -> Forall [] ([] :=> t)) <$> newTVar Star

  (currentPreds :=> currentType) <- instantiate currentScheme
  let env' = extendVars env (name, currentScheme)
  (s1, ps1, t1, e1) <- infer options env' exp
  s2                <- catchError (contextualUnify env' e currentType t1) (const $ return M.empty)
  --  ^ We can skip this error as we mainly need the substitution. It would fail in inferExplicitlyTyped anyways.
  let s  = s1 `compose` s2
  let t2 = apply s t1

  -- return (s1, ps1, t1, applyAssignmentSolve e name e1 (ps1 :=> t1))
  -- return (s1, currentPreds ++ ps1, t1, applyAssignmentSolve e name e1 ((currentPreds ++ ps1) :=> t1))
  return (s, currentPreds ++ ps1, apply s t2, applyAssignmentSolve e name e1 (apply s $ (currentPreds ++ ps1) :=> t2))



-- INFER EXPORT

inferExport :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport options env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer options env exp
  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor options env listExp@(Can.Canonical area (Can.ListConstructor elems)) = case elems of
  [] -> do
    tv <- newTVar Star
    let t = tListOf tv
    return (M.empty, [], t, Slv.Typed ([] :=> t) area (Slv.ListConstructor []))

  elems -> do
    tv               <- newTVar Star

    (s', ps, t', es) <- foldlM
      (\(s, pss, t, lis) elem -> do
        (s', ps', t'', li) <- inferListItem options (apply s env) (fromMaybe tv t) elem
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

    s'' <- contextualUnify env listExp tv t''
    let s''' = s' `compose` s''

    let t = tListOf (apply s''' tv)

    return (s''', ps, t, Slv.Typed (ps :=> t) area (Slv.ListConstructor es))


inferListItem :: Options -> Env -> Type -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem options env _ (Can.Canonical area li) = case li of
  Can.ListItem exp -> do
    (s1, ps, t, e) <- infer options env exp
    return (s1, ps, t, Slv.Typed (ps :=> t) area $ Slv.ListItem e)

  Can.ListSpread exp -> do
    (s1, ps, t, e) <- infer options env exp
    tv <- newTVar Star
    s2 <- contextualUnify env exp (tListOf tv) t

    let s = s1 `compose` s2

    return (s, ps, apply s tv, Slv.Typed (apply s ps :=> apply s t) area $ Slv.ListSpread e)


pickJSXChild :: Type -> Type -> Type
pickJSXChild t1 t2 = case (t1, t2) of
  (TApp (TCon (TC "Element" _) _) _, TCon (TC "String" _) _) ->
    t1

  _ ->
    t2


-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor options env (Can.Canonical area (Can.TupleConstructor elems)) = do
  inferredElems <-
    foldM
      (\(s, ps, ts, es) e -> do
          (s', ps', t', e') <- infer options (apply s env) e
          return (s `compose` s', ps ++ ps', ts ++ [t'], es ++ [e'])
      ) (M.empty, [], [], []) elems

  let s         = (\(s, _, _, _) -> s) inferredElems
  let elemTypes = (\(_, _, t, _) -> t) inferredElems
  let elemEXPS  = (\(_, _, _, es) -> es) inferredElems
  let ps        = (\(_, ps, _, _) -> ps) inferredElems
  let tupleT    = getTupleCtor (length elems)
  let t         = foldl' TApp tupleT elemTypes


  return (s, ps, apply s t, Slv.Typed (ps :=> apply s t) area (Slv.TupleConstructor elemEXPS))


-- INFER RECORD

inferRecord :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord options env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  (subst, inferredFields) <- foldM (
        \(fieldSubst, result) field -> do
          (s, ps, ts, e) <- inferRecordField options (apply fieldSubst env) field
          return (fieldSubst `compose` s, result ++ [(ps, ts, e)])
      ) (mempty, []) fields
  let fieldPS     = (\(ps, _, _) -> ps) <$> inferredFields
  let fieldTypes  = (\(_, t, _) -> t) <$> inferredFields
  let fieldEXPS   = (\(_, _, es) -> es) <$> inferredFields

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
            s <- contextualUnify env exp (TRecord (M.intersection fields $ M.fromList fieldTypes') base) (TRecord (M.fromList fieldTypes') base)
            return (s, fields, base)

          _                   -> do
            s <- contextualUnify env exp tBase (TRecord (M.fromList fieldTypes') base)
            return (s, mempty, base)

      Nothing ->
        return (mempty, mempty, base)
    return (TRecord (M.fromList fieldTypes' <> extraFields) newBase, s)

  let allPS = concat fieldPS

  return (subst `compose` extraSubst, allPS, recordType, Slv.Typed (allPS :=> recordType) area (Slv.Record fieldEXPS))


inferRecordField :: Options -> Env -> Can.Field -> Infer (Substitution, [Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField options env (Can.Canonical area field) = case field of
  Can.Field (name, exp) -> do
    (s, ps, t, e) <- infer options env exp
    return (s, ps, [(name, t)], Slv.Typed (ps :=> t) area $ Slv.Field (name, e))

  Can.FieldSpread exp -> do
    (s, ps, t, e) <- infer options env exp
    case t of
      TRecord _ _ ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      TVar _      ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      _ ->
        throwError $ CompilationError
          (WrongSpreadType $ show t)
          (Context (envCurrentPath env) (Can.getArea exp))



-- INFER ACCESS

inferAccess :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAccess options env e@(Can.Canonical _ (Can.Access ns _)) =
  case ns of
    Can.Canonical _ (Can.Var ns') ->
      if ns' `Set.member` envNamespacesInScope env then
        inferNamespaceAccess options env e
      else
        inferFieldAccess options env e

    _ ->
      inferFieldAccess options env e


-- INFER NAMESPACE ACCESS

inferNamespaceAccess :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNamespaceAccess options env e@(Can.Canonical area (Can.Access (Can.Canonical _ (Can.Var ns)) (Can.Canonical _ (Can.Var field))))
  = do
    sc <-
      catchError
        (lookupVar env (ns <> field))
        (\_ -> enhanceVarError env e area (CompilationError (UnboundVariableFromNamespace ns (tail field)) NoContext))
    (ps :=> t) <- instantiate sc

    let e = Slv.Typed (ps :=> t) area $ Slv.Var (ns <> field) (isConstructor env (ns <> field))
    e' <- insertVarPlaceholders options env e ps

    return (M.empty, ps, t, e')
inferNamespaceAccess _ _ _ = throwError $ CompilationError FatalError NoContext



-- INFER FIELD ACCESS

inferFieldAccess :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess options env fa@(Can.Canonical area (Can.Access rec@(Can.Canonical _ _) abs@(Can.Canonical _ (Can.Var ('.' : _)))))
  = do
    tv                  <- newTVar Star
    (s1, _  , t1, eabs) <- infer options env abs
    (s2, ps2, t2, earg) <- infer options env rec

    s3                  <- contextualUnifyAccess env fa t1 (t2 `fn` tv)

    let s      = s3 `compose` s2 `compose` s1
    let t      = apply s tv

    let solved = Slv.Typed (ps2 :=> t) area (Slv.Access earg eabs)

    return (s, ps2, t, solved)



-- INFER IF

inferIf :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf options env (Can.Canonical area (Can.If cond truthy falsy)) = do
  (s1, ps1, tcond  , econd  ) <- infer options env cond
  (s2, ps2, ttruthy, etruthy) <- infer options (apply s1 env) truthy
  (s3, ps3, tfalsy , efalsy ) <- infer options (apply (s1 `compose` s2) env) falsy

  s4                          <- contextualUnify env falsy tfalsy ttruthy
  s5                          <- contextualUnify env cond tBool (apply s4 tcond)

  let s = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
  let t = apply s ttruthy

  return (s, ps1 ++ ps2 ++ ps3, t, Slv.Typed ((ps1 ++ ps2 ++ ps3) :=> t) area (Slv.If econd etruthy efalsy))



-- INFER DEFINE IN

inferDo :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferDo options env (Can.Canonical area (Can.Do exps)) = do
  (s, ps, t, exps') <- inferBody options env exps

  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.Do exps'))


-- INFER WHERE

inferWhere :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere options env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e)          <- infer options env exp
  tv                     <- newTVar Star
  (pss, issSubstitution) <- foldM
    (\(res, currSubst) is -> do
      r@(subst, _, _) <- inferBranch options (apply currSubst env) (apply currSubst tv) t is
      return (res <> [r], subst `compose` currSubst)
    )
    ([], s)
    iss

  let ps' = concat $ T.mid <$> pss
  s' <- contextualUnifyElems env $ zip iss (apply issSubstitution . Slv.getType . T.lst <$> pss)

  let s''  = s' `compose` issSubstitution

  let iss = (\(Slv.Typed t a (Slv.Is pat exp)) -> Slv.Typed (apply s'' t) a (Slv.Is (updatePatternTypes s'' mempty pat) exp)) . T.lst <$> pss
  let wher = Slv.Typed (apply s'' $ (ps ++ ps') :=> tv) area $ Slv.Where (updateQualType e (apply s'' $ ps :=> t)) iss
  return (s'', ps ++ ps', apply s'' tv, wher)


inferBranch :: Options -> Env -> Type -> Type -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch options env tv t (Can.Canonical area (Can.Is pat exp)) = do
  (pat', ps, vars, t') <- inferPattern env pat
  s                    <- contextualUnify env exp t' t
  (s', ps', t'', e')   <- infer options (apply s $ mergeVars env vars) exp
  s''                  <- contextualUnify env exp tv (apply (s `compose` s') t'')

  let subst = s `compose` s' `compose` s''
  let allPreds = ps ++ ps'

  return
    ( subst
    , allPreds
    , Slv.Typed (allPreds :=> apply subst (t' `fn` tv)) area
      $ Slv.Is (updatePatternTypes subst (apply s <$> vars) pat') (updateQualType e' (ps' :=> apply subst t''))
    )

updatePatternTypes :: Substitution -> Vars -> Slv.Pattern -> Slv.Pattern
updatePatternTypes s vars pat = case pat of
  Slv.Typed t area (Slv.PCon n pats) ->
    Slv.Typed (apply s t) area (Slv.PCon n (updatePatternTypes s vars <$> pats))

  Slv.Typed t area (Slv.PRecord fields) ->
    Slv.Typed (apply s t) area (Slv.PRecord (updatePatternTypes s vars <$> fields))

  Slv.Typed t area (Slv.PList items) ->
    Slv.Typed (apply s t) area (Slv.PList (updatePatternTypes s vars <$> items))

  Slv.Typed t area (Slv.PTuple items) ->
    Slv.Typed (apply s t) area (Slv.PTuple (updatePatternTypes s vars <$> items))

  Slv.Typed t area (Slv.PSpread pat) ->
    Slv.Typed (apply s t) area (Slv.PSpread (updatePatternTypes s vars pat))

  Slv.Typed t area (Slv.PVar n) ->
    case M.lookup n vars of
      Just (Forall _ qt) ->
        Slv.Typed (apply s qt) area (Slv.PVar n)

      Nothing ->
        Slv.Typed (apply s t) area (Slv.PVar n)

  Slv.Typed t area p ->
    Slv.Typed (apply s t) area p


-- INFER TYPEDEXP

inferTypedExp :: Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp options env e@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  (_ :=> t)        <- instantiate sc

  (s1, ps1, t1, e1) <- infer options env exp
  s2                <- contextualUnify env e t t1

  return (s1 `compose` s2, apply s2 ps1, apply s2 t1, Slv.Typed (apply s2 $ ps1 :=> t1) area (Slv.TypedExp (updateQualType e1 (ps1 :=> t1)) (updateTyping typing) sc))
  -- return (s1, ps1, t1, Slv.Typed (ps1 :=> t1) area (Slv.TypedExp (updateQualType e1 (ps1 :=> t1)) (updateTyping typing) sc))
  -- return (s1 `compose` s2, ps, t, Slv.Typed (ps :=> t) area (Slv.TypedExp (updateQualType e1 (ps1 :=> t)) (updateTyping typing) sc))


inferExtern :: Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExtern _ (Can.Canonical area (Can.Extern scheme name originalName)) = do
  qt@(ps :=> t) <- instantiate scheme
  return (mempty, ps, t, Slv.Typed qt area (Slv.Extern qt name originalName))


type Ambiguity = (TVar, [Pred])

ambiguities :: [TVar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [ (v, filter (elem v . ftv) ps) | v <- ftv ps \\ vs ]



hasPredForType :: String -> Type -> [Pred] -> Bool
hasPredForType cls t ps =
  any (\(IsIn cls' ts _) -> t `elem` ts && cls == cls') ps

updateRecordUpdatePreds :: [Pred] -> [Pred]
updateRecordUpdatePreds ps = updateRecordUpdatePreds' ps ps

-- Preds for record with a base should be resolved by the base directly
updateRecordUpdatePreds' :: [Pred] -> [Pred] -> [Pred]
updateRecordUpdatePreds' allPreds ps = case ps of
  IsIn cls [tRec@(TRecord _ (Just base@(TVar _)))] maybeArea : next
    | not (hasPredForType "Number" tRec allPreds)
    && not (hasPredForType "Bits" tRec allPreds)
    && not (hasPredForType "Number" base allPreds)
    && not (hasPredForType "Bits" base allPreds) ->
      IsIn cls [base] maybeArea : updateRecordUpdatePreds next

  or : next ->
    or : updateRecordUpdatePreds next

  _ ->
    []

split :: Bool -> Env -> [TVar] -> [TVar] -> [Pred] -> Infer ([Pred], [Pred], Substitution)
split mustCheck env fs gs ps = do
  ps' <- reduce env (updateRecordUpdatePreds ps)
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  let as = ambiguities (fs ++ gs) rs
  if mustCheck && not (null as) then do
    -- if we have ambiguities we try to resolve them with default instances
    (s, rs')      <- tryDefaults env rs
    (sDef', rs'') <- tryDefaults env (apply s rs')
    let (ds', rs''') = partition (all (`elem` fs) . ftv) (apply sDef' ds ++ rs'')

    -- and then compute the potential leftover ambiguities
    let as' = ambiguities (fs ++ gs) rs'''
    if not (null as') then
      case head as of
        (_, IsIn _ _ (Just area):_) ->
          throwError $ CompilationError (AmbiguousType (head as)) (Context (envCurrentPath env) area)

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

    IsIn "Bits" [TVar tv] _ -> do
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

    IsIn "Inspect" [t] _ -> do
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


ftvForLetGen :: Type -> [TVar]
ftvForLetGen t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) tl1) tr1 ->
    ftv tl1 ++ ftv tr1

  TApp t1 t2 ->
    ftvForLetGen t1 ++ ftvForLetGen t2

  TRecord fields _ ->
    M.elems fields >>= ftvForLetGen

  _ ->
    []


inferImplicitlyTyped :: Options -> Bool -> Env -> Can.Exp -> Infer (Substitution, ([Pred], [Pred]), Env, Slv.Exp)
inferImplicitlyTyped options isLet env exp@(Can.Canonical area _) = do
  (env', tv) <- case Can.getExpName exp of
    Just n -> case M.lookup n (envVars env) of
      Just sc -> do
        _ :=> t' <- instantiate sc
        return (env, t')
      --  ^ if a var is already present we don't override its type with a fresh var.

      Nothing -> do
        tv <- newTVar Star
        return (extendVars env (n, Forall [] $ [] :=> tv), tv)

    Nothing -> do
      tv <- newTVar Star
      return (env, tv)

  (s1, ps1, t1, _) <- infer options env' exp
  ps1' <- concat <$> mapM (gatherInstPreds env') ps1

  -- We need to update the env again in case the inference of the function resulted in overloading so that
  -- we can have the predicates to generate the correct placeholders when fetching the var from the env
  env''' <- case Can.getExpName exp of
    Just n ->
      return $ extendVars env' (n, Forall [] $ ps1' :=> t1)

    Nothing ->
      return env'

  -- Once we have gattered clues we update the env types and infer it again
  -- to handle recursion errors. We probably need to improve that solution at
  -- some point!
  (s2, ps, t, e) <- infer options (apply s1 env''') exp
  let s = s1 `compose` s2

  env'' <- case Can.getExpName exp of
    Just n ->
      return $ extendVars env''' (n, Forall [] $ apply s $ ps :=> t)

    Nothing ->
      return $ apply s env'''

  s' <- contextualUnify env'' exp (apply s tv) t
  let s'' = s `compose` s1 `compose` s'
      envWithVarsExcluded = env''
        { envVars = M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $ envVars env'' }

      ps' = apply s'' ps
      t'  = apply s'' tv
      vs  =
        if isLet then
          ftvForLetGen t'
        else
          ftv t'
      fs  = ftv (apply s'' envWithVarsExcluded)
      gs  = vs \\ fs
  (ds, rs, _) <- catchError
    (split False env'' fs vs ps')
    (\case
      (CompilationError e NoContext) ->
        throwError $ CompilationError e (Context (envCurrentPath env) area)

      (CompilationError e c) ->
        throwError $ CompilationError e c
    )

  (ds', rs', sDefaults) <-
    if not isLet && not (Slv.isExtern e) && not (null (ds ++ rs)) && not (Can.isNamedAbs exp) && not (isFunctionType t') then do
      (sDef, rs')   <- tryDefaults env'' (ds ++ rs)
          -- TODO: tryDefaults should handle such a case so that we only call it once.
          -- What happens is that defaulting may solve some types ( like Number a -> Integer )
          -- and then it could resolve instances like Show where before we still had a type var
          -- but after the first pass we have Integer instead.
      (sDef', rs'') <- tryDefaults env'' (apply sDef rs')
      CM.unless (null rs'') $ throwError $ CompilationError
        (AmbiguousType (TV "-" Star, rs'))
        (Context (envCurrentPath env) area)
      return ([], [], sDef' `compose` sDef)
    -- else if isFunctionType t then do
    --   (sDef, rs')   <- tryDefaults env'' rs
    --   (sDef', rs'') <- tryDefaults env'' (apply sDef rs')
    --   return (ds ++ rs'', sDef' `compose` sDef)
    else do
      return (ds, rs, mempty)

  -- if a predicate refers to a type variable that is not present in the type
  -- itself we will never be able to find a matching instance and thus have
  -- an ambiguity
  -- (extraS, rs'') <-
  --   if (isFunctionType t' && (ftv (ds' ++ rs') \\ ftv t') /= mempty) then do
  --     (sDef, rs')   <- tryDefaults env'' rs
  --     (sDef', rs'') <- tryDefaults env'' (apply sDef rs')
  --     return (ds ++ rs'', sDef' `compose` sDef)
  --     throwError $ CompilationError
  --         (AmbiguousType (TV "-" Star, rs'))
  --         (Context (envCurrentPath env) area)
  --   else
  --     return mempty


  rs'' <- dedupePreds <$> getAllParentPreds env rs'
  let sFinal = sDefaults `compose` s''

  let sc =
        -- if isLet then
        --   Forall [] $ apply sFinal (rs'' :=> t')
        -- else
          quantify gs $ apply sFinal (rs'' :=> t')
        -- if isLet && not (isFunctionType t') then
        --   Forall [] $ apply sFinal (rs'' :=> t')
        -- else
        --   quantify gs $ apply sFinal (rs'' :=> t')

  liftIO $ putStrLn $ "Exp:" <> ppShow (Can.getExpName exp)
  liftIO $ putStrLn $ "rs''" <> ppShow rs''
  liftIO $ putStrLn $ "ds'" <> ppShow ds'
  liftIO $ putStrLn $ "t'" <> ppShow t'
  liftIO $ putStrLn $ "sc" <> ppShow sc

  -- when (isFunctionType t' && (ftv (ds' ++ rs') \\ ftv t') /= mempty) $ do
  --   throwError $ CompilationError
  --       (AmbiguousType (TV "-" Star, rs'))
  --       (Context (envCurrentPath env) area)

  -- let returnPreds = if isFunctionType t' then [] else ds'
  let returnPreds = ds' ++ rs'

  case Can.getExpName exp of
    Just n  ->
      return (sFinal, (returnPreds, rs''), extendVars env (n, sc), updateQualType e (apply sFinal $ rs'' :=> t'))

    Nothing ->
      return (sFinal, (returnPreds, rs''), env, updateQualType e (apply sFinal $ rs'' :=> t'))


inferExplicitlyTyped :: Options -> Bool -> Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped options isLet env canExp@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  qt@(qs :=> t') <- instantiate sc

  env' <- case Can.getExpName exp of
        Just n  -> do
          -- We convert say Applicative f => .. to (Functor f, Applicative f) => ..
          -- so that we generate the right dictionary placeholders.
          psWithParents <- getAllParentPreds env (dedupePreds qs)
          let scWithParents = quantify (ftv qt) (psWithParents :=> t')
          return $ extendVars env (n, scWithParents)

        Nothing ->
          return env

  (s, ps, t, e) <- infer options env' exp
  psFull        <- concat <$> mapM (gatherInstPreds env') ps
  s''           <- catchError (contextualUnify env canExp t' (apply (s `compose` s) t)) (throwError . limitContextArea 2)
  let s' = s `compose` s'' `compose` s''

  let envWithVarsExcluded =
        env'
          {
            envVars =
              if isLet then
                M.filterWithKey (\k _ -> fromMaybe "" (Can.getExpName exp) /= k) $ envVars env'
              else
                envVars env'
          }
      qs'  = apply s' qs
      t''  = apply s' t
      t''' = mergeRecords (apply s' t') t''
      fs   = ftv (apply s' envWithVarsExcluded)
      gs   = ftv (apply s' t') \\ fs
  ps'      <- filterM ((not <$>) . entail env' qs') (apply s' psFull)
  (ds, rs, substDefaultResolution) <- catchError
    (split True env' fs gs ps')
    (\case
      (CompilationError e NoContext) ->
        throwError $ CompilationError e (Context (envCurrentPath env) area)

      (CompilationError e c) ->
        throwError $ CompilationError e c
    )

  qs'' <- dedupePreds <$> getAllParentPreds env (dedupePreds qs')

  let scCheck  = quantify (ftv (apply s' t')) (qs' :=> apply substDefaultResolution (apply s' t'))
  if sc /= scCheck then
    throwError $ CompilationError (SignatureTooGeneral sc scCheck) (Context (envCurrentPath env') area)
  else if not (null rs) then
    throwError $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) area)
  else do
    let e'   = updateQualType e (ds :=> t''')

    let qt'  = qs'' :=> t'''
    let sc'' = quantify gs qt'
    let env'' = case Can.getExpName exp of
          Just n  ->
            extendVars env' (n, sc'')
          Nothing ->
            env'

    return (substDefaultResolution `compose` s', qs'', env'', Slv.Typed (qs :=> t') area (Slv.TypedExp e' (updateTyping typing) sc))

inferExplicitlyTyped _ _ _ _ = undefined


inferExps :: Options -> Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps _ env []       = return ([], env)

inferExps options env (e : es) = do
  (e' , env'   ) <- catchError (inferExp options env e) (recordError env e)
  (es', nextEnv) <- inferExps options env' es

  case e' of
    Just e'' ->
      return (e'' : es', nextEnv)

    Nothing  ->
      return (es', nextEnv)


inferExp :: Options -> Env -> Can.Exp -> Infer (Maybe Slv.Exp, Env)
inferExp _ env (Can.Canonical _ (Can.TypeExport _)) =
  return (Nothing, env)
inferExp options env e = do
  (s, placeholderPreds, env', e') <- upgradeContext env (Can.getArea e) $ case e of
    Can.Canonical _ Can.TypedExp{} -> do
      inferExplicitlyTyped options False env e

    _ -> do
      -- NB: Currently handles Extern nodes as well
      -- (_, _, env', _)                       <- inferImplicitlyTyped options False env e
      (s, (_, placeholderPreds), env'', e') <- inferImplicitlyTyped options False env e
      return (s, placeholderPreds, env'', e')

  e''  <- insertClassPlaceholders options env' e' placeholderPreds
  e''' <- updatePlaceholders options env' (CleanUpEnv False [] [] []) False s e''

  return (Just e''', env')


recordError :: Env -> Can.Exp -> CompilationError -> Infer (Maybe Slv.Exp, Env)
recordError env e err = do
  pushError err
  return (Just $ toSolved e, env)


upgradeContext :: Env -> Area -> Infer a -> Infer a
upgradeContext env area a = catchError a (throwError . upgradeContext' env area)


upgradeContext' :: Env -> Area -> CompilationError -> CompilationError
upgradeContext' env area err = case err of
  (CompilationError e NoContext) -> CompilationError e $ Context (envCurrentPath env) area
  (CompilationError e r        ) -> CompilationError e r
