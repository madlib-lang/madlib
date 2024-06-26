{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use second" #-}
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
                                                , foldl', intersect
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
import           Debug.Trace
import           Text.Show.Pretty
import           AST.Solved (getType)
import qualified Data.Set as Set
import           Run.Options
import qualified Data.List as List


mutationInterface :: String
mutationInterface = "__MUTATION__"

mutationPred :: Pred
mutationPred = IsIn mutationInterface [] Nothing

makeMutationPred :: Type -> Area -> Pred
makeMutationPred t area = IsIn mutationInterface [t] (Just area)


infer :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
infer discardError options env lexp = do
  let (Can.Canonical area exp) = lexp
  case exp of
    Can.LNum  _               -> do
      t <- newTVar Star
      let ps = [IsIn "Number" [t] Nothing]
      return (M.empty, ps, t, applyLitSolve lexp (ps :=> t))

    Can.LByte _               -> return (M.empty, [], tByte, applyLitSolve lexp ([] :=> tByte))
    Can.LShort _              -> return (M.empty, [], tShort, applyLitSolve lexp ([] :=> tShort))
    Can.LInt _                -> return (M.empty, [], tInteger, applyLitSolve lexp ([] :=> tInteger))
    Can.LFloat _              -> return (M.empty, [], tFloat, applyLitSolve lexp ([] :=> tFloat))
    Can.LStr  _               -> return (M.empty, [], tStr, applyLitSolve lexp ([] :=> tStr))
    Can.LChar  _              -> return (M.empty, [], tChar, applyLitSolve lexp ([] :=> tChar))
    Can.LBool _               -> return (M.empty, [], tBool, applyLitSolve lexp ([] :=> tBool))
    Can.LUnit                 -> return (M.empty, [], tUnit, applyLitSolve lexp ([] :=> tUnit))
    Can.TemplateString _      -> inferTemplateString discardError options env lexp

    Can.Var            _      -> inferVar discardError options env lexp
    Can.Abs _ _               -> inferAbs discardError options env lexp
    Can.App{}                 -> inferApp discardError options env lexp
    Can.Assignment _ _        -> inferAssignment discardError options env lexp
    Can.Mutate _ _            -> inferMutate discardError options env lexp
    Can.Do _                  -> inferDo discardError options env lexp
    Can.Where      _ _        -> inferWhere discardError options env lexp
    Can.Record _              -> inferRecord discardError options env lexp
    Can.Access   _ _          -> inferAccess discardError options env lexp
    Can.ArrayAccess   _ _     -> inferArrayAccess discardError options env lexp
    Can.TypedExp{}            -> inferTypedExp discardError options env lexp
    Can.ListConstructor  _    -> inferListConstructor discardError options env lexp
    Can.TupleConstructor _    -> inferTupleConstructor discardError options env lexp
    Can.Export           _    -> inferExport discardError options env lexp
    Can.NameExport       _    -> inferNameExport env lexp
    Can.If{}                  -> inferIf discardError options env lexp
    Can.While{}               -> inferWhile discardError options env lexp
    Can.Extern{}              -> inferExtern env lexp
    Can.TypedHole             -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Typed ([] :=> t) area Slv.TypedHole)

    Can.JSExp c               -> do
      t <- newTVar Star
      return (M.empty, [], t, Slv.Typed ([] :=> t) area (Slv.JSExp c))


applyLitSolve :: Can.Exp -> Qual Type -> Slv.Exp
applyLitSolve (Can.Canonical area exp) qt = case exp of
  Can.LNum  v  -> Slv.Typed qt area $ Slv.LNum v
  Can.LByte  v -> Slv.Typed qt area $ Slv.LNum v
  Can.LShort v -> Slv.Typed qt area $ Slv.LNum v
  Can.LInt  v  -> Slv.Typed qt area $ Slv.LNum v
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

inferVar :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferVar discardError _ env exp@(Can.Canonical area (Can.Var n)) = case n of
  ('.' : name) -> do
    let s = Forall [Star, Star] $ [] :=> (TRecord (M.fromList [(name, TGen 0)]) (Just $ TGen 1) mempty `fn` TGen 0)
    (ps :=> t) <- instantiate s
    return (M.empty, ps, t, Slv.Typed (ps :=> t) area $ Slv.Var n False)

  _ -> do
    sc         <- catchError (lookupVar env n) (enhanceVarError env exp area)
    (ps :=> t) <- instantiate sc

    let ps' = dedupePreds ps
    let e = Slv.Typed (ps' :=> t) area $ Slv.Var n (isConstructor env n)
    let ps'' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps'

    return (M.empty, ps'', t, e)

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


inferAbs :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAbs discardError options env l@(Can.Canonical _ (Can.Abs p@(Can.Canonical area param) body)) = do
  tv             <- newTVar Star
  env'           <- extendAbsEnv env tv p
  (s, ps, t, es) <- inferBody discardError options env' { envInBody = True } body
  (s', es')      <- postProcessBody discardError options env' s (tv `fn` t) es

  let t'        = apply s' (tv `fn` t)
      paramType = apply s' tv

  return (s', apply s' ps, t', applyAbsSolve l (Slv.Typed (apply s' $ ps :=> paramType) area param) es' (apply s' $ ps :=> t'))


inferBody :: Bool -> Options -> Env -> [Can.Exp] -> Infer (Substitution, [Pred], Type, [Slv.Exp])
inferBody discardError options env [e] = do
  (s, ps, t, e') <- infer discardError options env e
  return (s, ps, t, [e'])

inferBody discardError options env (e : es) = do
  (s, (returnPreds, _), env', e') <- inferImplicitlyTyped discardError options True env e
  (sb, ps', tb, eb) <- inferBody discardError options (apply s env') es
  let finalS = s `compose` (sb `compose` s)

  return (finalS, apply finalS $ returnPreds ++ ps', tb, e' : eb)


-- TODO: find out and comment why we need this
postProcessBody :: Bool -> Options -> Env -> Substitution -> Type -> [Slv.Exp] -> Infer (Substitution, [Slv.Exp])
postProcessBody discardError options env s expType es = do
  (es', s', _) <- foldM
    (\(results, accSubst, env'') (Slv.Typed (ps' :=> t') area e) -> do
      let fs = ftv (apply accSubst env'') `List.union` ftv (apply accSubst expType) `List.union` ftvForLetGen (apply accSubst t')
      let ps'' = apply accSubst ps'

      (ps''', substFromDefaulting) <- do
        prep <- forM ps'' $ \p -> do
          isResolved <- entail env [] p
          if isResolved then
            return $ Just p
          else
            return Nothing

        let solvedPs = catMaybes prep
        let unsolvedPs = ps'' \\ solvedPs

        -- if ambiguities fs unsolvedPs /= [] && not discardError then do
        if ambiguities fs unsolvedPs /= [] then do
          (sDef, unsolvedPs')   <- tryDefaults env unsolvedPs
          (sDef', unsolvedPs'') <- tryDefaults env (apply sDef unsolvedPs')
          let subst = sDef' `compose` sDef

          if unsolvedPs'' /= [] then do
            forM_ unsolvedPs'' $ \p -> do
              catchError
                (byInst env (apply subst p))
                (\case
                  _ | discardError ->
                    return []

                  (CompilationError FatalError NoContext) ->
                    if ambiguities fs unsolvedPs'' /= [] then
                      case p of
                        IsIn _ (TVar tv : _) _ ->
                          throwError $ CompilationError
                            (AmbiguousType (tv, apply subst unsolvedPs''))
                            (Context (envCurrentPath env) area)

                        _ ->
                          throwError $ CompilationError
                            (AmbiguousType (TV (-1) Star, apply subst unsolvedPs''))
                            (Context (envCurrentPath env) area)
                      else
                        return []
                  or ->
                    throwError or
                )
            return (unsolvedPs'' ++ solvedPs, subst)
          else
            return (unsolvedPs'' ++ solvedPs, subst)
          return (unsolvedPs'' ++ solvedPs, subst)
        else
          return (ps'', mempty)

      let sFinal = substFromDefaulting `compose` accSubst
      e' <- updateExpTypes options env False sFinal (Slv.Typed (apply sFinal $ ps''' :=> t') area e)

      return (results ++ [e'], sFinal, apply sFinal env'')
    )
    (mempty, s, env)
    es

  return (s', es')


-- INFER APP

inferApp :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferApp discardError options env (Can.Canonical area (Can.App abs@(Can.Canonical absArea _) arg@(Can.Canonical argArea _) final)) = do
  tv                  <- newTVar Star
  (s1, ps1, t1, eabs) <- infer discardError options env abs
  (s2, ps2, t2, earg) <- infer discardError options (apply s1 env) arg

  let expForContext =
        if getLineFromStart argArea < getLineFromStart absArea then
          abs
        else
          arg

  s3 <- contextualUnify' env discardError expForContext (apply s2 t1) (apply s1 t2 `fn` tv)

  let t = apply s3 tv
  let s = s3 `compose` s2 `compose` s1

  let solved = Slv.Typed (apply s (ps1 ++ ps2) :=> apply s t) area $ Slv.App eabs (updateQualType earg $ apply s (ps1 ++ ps2) :=> apply s t2) final

  return (s, ps1 ++ ps2, t, solved)



-- INFER TEMPLATE STRINGS

inferTemplateString :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTemplateString discardError options env (Can.Canonical area (Can.TemplateString exps)) = do
  inferred <- mapM (infer discardError options env) exps

  let elemSubsts = (\(s, _, _, _) -> s) <$> inferred
  let elemTypes  = (\(_, _, t, _) -> t) <$> inferred
  let elemExps   = (\(_, _, _, es) -> es) <$> inferred
  let elemPS     = (\(_, ps, _, _) -> ps) <$> inferred

  ss <- mapM (\(exp, t) -> contextualUnify' env discardError exp t tStr) (zip exps elemTypes)

  let fullSubst = foldl' compose M.empty (elemSubsts <> ss)

  let qs = uncurry (:=>) <$> zip elemPS elemTypes

  let updatedExp = Slv.Typed
        ([] :=> tStr)
        area
        (Slv.TemplateString ((\(t, e) -> updateQualType e (apply fullSubst t)) <$> zip qs elemExps))

  return (fullSubst, concat elemPS, tStr, updatedExp)



-- INFER ASSIGNMENT

inferAssignment :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAssignment discardError options env e@(Can.Canonical area (Can.Assignment name exp)) = do
  when (name `Set.member` envNamespacesInScope env && not discardError) $ do
    pushError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath env) area)

  currentScheme <- case M.lookup name (envVars env) of
    Just sc ->
      return sc

    _ -> do
      tVar <- newTVar Star
      return $ Forall [] ([] :=> tVar)

  (currentPreds :=> currentType) <- instantiate currentScheme
  let env' = extendVars env (name, currentScheme)
  (s1, ps1, t1, e1) <- infer discardError options env' exp
  s2                <- catchError (contextualUnify env' e currentType t1) (const $ return M.empty)
  --  ^ We can skip this error as we mainly need the substitution. It would fail in inferExplicitlyTyped anyways.
  let s  = s1 `compose` s2
  let t2 = apply s t1

  mutationPs <-
    if name `Set.member` envNamesInScope env && envInBody env && not discardError then do
      pushError $ CompilationError BadMutation (Context (envCurrentPath env) area)
      return []
    else
      return []

  return (s, currentPreds ++ ps1 ++ mutationPs, apply s t2, applyAssignmentSolve e name e1 (apply s $ (currentPreds ++ ps1) :=> t2))



-- INFER MUTATE

inferMutate :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferMutate discardError options env e@(Can.Canonical area (Can.Mutate lhs exp)) = do
  (s1, ps1, t1, e1) <- infer discardError options env lhs
  (s2, ps2, t2, e2) <- infer discardError options (apply s1 env) exp
  s3 <- catchError
    (contextualUnify env e t1 t2)
    (\err -> do
      if discardError then do
        return mempty
      else
        throwError err
    )

  let s  = s1 `compose` s2 `compose` s3
  let t3 = apply s t2

  mutationPs <-
    case Can.getExpName lhs of
      Just name | not discardError ->
        if name `Set.member` envNamesInScope env && envInBody env then
          return [makeMutationPred (apply s t3) area]
        else
          throwError $ CompilationError (MutatingNotInScope name) (Context (envCurrentPath env) area)

      _ ->
        return []

  return
    ( s
    , ps1 ++ ps2 ++ mutationPs
    , apply s t3
    , Slv.Typed (apply s $ (ps1 ++ ps2 ++ mutationPs) :=> t3) area (Slv.Mutate e1 e2)
    )



-- INFER EXPORT

inferExport :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferExport discardError options env (Can.Canonical area (Can.Export exp)) = do
  (s, ps, t, e) <- infer discardError options env exp
  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.Export e))



-- INFER LISTCONSTRUCTOR

inferListConstructor :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferListConstructor discardError options env listExp@(Can.Canonical area (Can.ListConstructor elems)) = case elems of
  [] -> do
    tv <- newTVar Star
    let t = tListOf tv
    return (M.empty, [], t, Slv.Typed ([] :=> t) area (Slv.ListConstructor []))

  elems -> do
    tv               <- newTVar Star

    (s', ps, t', es) <- foldlM
      (\(s, pss, t, lis) elem -> do
        (s', ps', t'', li) <- inferListItem discardError options (apply s env) (fromMaybe tv t) elem
        (s'', tr) <- case t of
          Nothing ->
            return (mempty, t'')

          Just t''' -> do
            s'''' <- contextualUnify' env discardError elem (apply s' t''') t''
            return (s'''', pickJSXChild t''' t'')

        let s''' = s'' `compose` s' `compose` s
        return (s''', pss ++ ps', Just $ apply s''' tr, lis ++ [li])
      )
      (mempty, [], Nothing, [])
      elems

    let (Just t'') = t'

    s'' <- contextualUnify' env discardError listExp tv t''
    let s''' = s'' `compose` s'

    let t = tListOf (apply s''' tv)

    return (s''', ps, t, Slv.Typed (ps :=> t) area (Slv.ListConstructor es))


inferListItem :: Bool -> Options -> Env -> Type -> Can.ListItem -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferListItem discardError options env _ (Can.Canonical area li) = case li of
  Can.ListItem exp -> do
    (s1, ps, t, e) <- infer discardError options env exp
    return (s1, ps, t, Slv.Typed (ps :=> t) area $ Slv.ListItem e)

  Can.ListSpread exp -> do
    (s1, ps, t, e) <- infer discardError options env exp
    tv <- newTVar Star
    s2 <- contextualUnify' env discardError exp (tListOf tv) t

    let s = s1 `compose` s2

    return (s, ps, apply s tv, Slv.Typed (apply s ps :=> apply s t) area $ Slv.ListSpread e)


pickJSXChild :: Type -> Type -> Type
pickJSXChild t1 t2 = case (t1, t2) of
  (TApp (TCon (TC "Element" _) _) _, TCon (TC "String" _) _) ->
    t2

  _ ->
    t2



-- INFER TUPLE CONSTRUCTOR

inferTupleConstructor :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTupleConstructor discardError options env (Can.Canonical area (Can.TupleConstructor elems)) = do
  inferredElems <-
    foldM
      (\(s, ps, ts, es) e -> do
          (s', ps', t', e') <- infer discardError options (apply s env) e
          return (s' `compose` s, ps ++ ps', ts ++ [t'], es ++ [e'])
      ) (M.empty, [], [], []) elems

  let s         = (\(s, _, _, _) -> s) inferredElems
  let elemTypes = (\(_, _, t, _) -> t) inferredElems
  let elemEXPS  = (\(_, _, _, es) -> es) inferredElems
  let ps        = (\(_, ps, _, _) -> ps) inferredElems
  let tupleT    = getTupleCtor (length elems)
  let t         = foldl' TApp tupleT elemTypes


  return (s, ps, apply s t, Slv.Typed (ps :=> apply s t) area (Slv.TupleConstructor elemEXPS))



-- INFER RECORD

inferRecord :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferRecord discardError options env exp = do
  let Can.Canonical area (Can.Record fields) = exp

  (subst, inferredFields) <- foldM (
        \(fieldSubst, result) field -> do
          (s, ps, ts, e) <- inferRecordField discardError options (apply fieldSubst env) field
          let nextSubst = s `compose` fieldSubst
          return (nextSubst, result ++ [(ps, (\(n, t) -> (n, apply nextSubst t)) <$> ts, e)])
      ) (mempty, []) fields
  let fieldPS     = (\(ps, _, _) -> ps) <$> inferredFields
  let fieldTypes  = (\(_, t, _) -> t) <$> inferredFields
  let fieldEXPS   = (\(_, _, es) -> es) <$> inferredFields

  let fieldTypes' = filter (\(k, _) -> k /= "...") (concat fieldTypes)
  let spreads     = snd <$> filter (\(k, _) -> k == "...") (concat fieldTypes)

  let base = case spreads of
        (x : _) -> Just x
        _       -> Nothing

  (recordType, extraSubst) <- case apply subst <$> base of
    Just (TRecord fields baseBase optionalFields) ->
      return (TRecord (M.fromList fieldTypes' <> fields) (apply subst <$> baseBase) optionalFields, mempty)

    Just tBase -> do
      baseVar <- newTVar Star
      s <- contextualUnify' env discardError exp (apply subst tBase) (TRecord mempty (Just baseVar) mempty)
      -- return (TRecord (M.fromList fieldTypes') (Just baseVar) mempty, s)
      return (TRecord (M.fromList fieldTypes') (Just tBase) mempty, s)

    Nothing ->
      return (TRecord (M.fromList fieldTypes') Nothing mempty, mempty)

  let allPS = concat fieldPS
  let finalSubst = subst `compose` extraSubst

  return (finalSubst, allPS, apply finalSubst recordType, Slv.Typed (allPS :=> recordType) area (Slv.Record fieldEXPS))


inferRecordField :: Bool -> Options -> Env -> Can.Field -> Infer (Substitution, [Pred], [(Slv.Name, Type)], Slv.Field)
inferRecordField discardError options env (Can.Canonical area field) = case field of
  Can.Field (name, exp) -> do
    (s, ps, t, e) <- infer discardError options env exp
    return (s, ps, [(name, t)], Slv.Typed (ps :=> t) area $ Slv.Field (name, e))

  Can.FieldSpread exp -> do
    (s, ps, t, e) <- infer discardError options env exp
    case t of
      TRecord{} ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      TVar _ ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      _ | discardError ->
        return (s, ps, [("...", t)], Slv.Typed (ps :=> t) area $ Slv.FieldSpread e)

      _ ->
        throwError $ CompilationError
          (WrongSpreadType $ show t)
          (Context (envCurrentPath env) (Can.getArea exp))



-- INFER ACCESS

inferAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferAccess discardError options env e@(Can.Canonical _ (Can.Access ns _)) =
  case ns of
    Can.Canonical _ (Can.Var ns') ->
      if ns' `Set.member` envNamespacesInScope env then
        inferNamespaceAccess discardError options env e
      else
        inferFieldAccess discardError options env e

    _ ->
      inferFieldAccess discardError options env e



-- INFER ACCESS

inferArrayAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferArrayAccess discardError options env (Can.Canonical area (Can.ArrayAccess arr index)) = do
  tv <- newTVar Star
  (s1, ps1, t1, earr) <- infer discardError options env arr
  (s2, ps2, t2, eindex) <- infer discardError options env index
  s3 <- contextualUnify' env discardError arr t1 (tArrayOf tv)
  s4 <- contextualUnify' env discardError index t2 tInteger

  let s = s4 `compose` s3 `compose` s2 `compose` s1
  let t = apply s tv
  let ps = ps1 ++ ps2

  return (s, ps, t, Slv.Typed (ps :=> t) area (Slv.ArrayAccess earr eindex))



-- INFER NAMESPACE ACCESS

inferNamespaceAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferNamespaceAccess discardError _ env e@(Can.Canonical area (Can.Access (Can.Canonical _ (Can.Var ns)) (Can.Canonical _ (Can.Var field))))
  = do
    sc <-
      catchError
        (lookupVar env (ns <> field))
        (\_ -> enhanceVarError env e area (CompilationError (UnboundVariableFromNamespace ns (tail field)) NoContext))
    (ps :=> t) <- instantiate sc
    let ps' = (\(IsIn c ts _) -> IsIn c ts (Just area)) <$> ps

    let e = Slv.Typed (ps :=> t) area $ Slv.Var (ns <> field) (isConstructor env (ns <> field))

    return (M.empty, ps', t, e)
inferNamespaceAccess _ _ _ _ = throwError $ CompilationError FatalError NoContext



-- INFER FIELD ACCESS

inferFieldAccess :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferFieldAccess discardError options env fa@(Can.Canonical area (Can.Access rec@(Can.Canonical _ _) abs))
  = do
    tv                  <- newTVar Star
    (s1, _  , t1, eabs) <- infer discardError options env abs
    (s2, ps2, t2, earg) <- infer discardError options env rec

    s3 <- catchError
      (contextualUnifyAccess env fa t1 (t2 `fn` tv))
      (\err -> do
        if discardError then do
          return $ gentleUnify t1 (t2 `fn` tv)
        else
          throwError err
      )

    let s = s3 `compose` s2 `compose` s1
    let t = apply s tv
    let solved = Slv.Typed (ps2 :=> t) area (Slv.Access earg eabs)

    return (s, ps2, t, solved)



-- INFER IF

inferIf :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferIf discardError options env (Can.Canonical area (Can.If cond truthy falsy)) = do
  (s1, ps1, tcond, econd) <- infer discardError options env cond
  (s2, ps2, ttruthy, etruthy) <- infer discardError options (apply s1 env) truthy
  (s3, ps3, tfalsy, efalsy) <- infer discardError options (apply (s2 `compose` s1) env) falsy

  let tfalsy' = apply (s3 `compose` s2 `compose` s1) tfalsy
  let ttruthy' = apply (s3 `compose` s2 `compose` s1) ttruthy
  s4 <- catchError (contextualUnify' env discardError falsy tfalsy' ttruthy') flipUnificationError
  s5 <- contextualUnify' env discardError cond tBool (apply s4 tcond)

  let s = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
  let t = apply s ttruthy

  return (s, ps1 ++ ps2 ++ ps3, t, Slv.Typed ((ps1 ++ ps2 ++ ps3) :=> t) area (Slv.If econd etruthy efalsy))



-- INFER While

inferWhile :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhile discardError options env (Can.Canonical area (Can.While cond body)) = do
  (s1, ps1, tcond, econd) <- infer discardError options env cond
  (s2, ps2, tbody, ebody) <- infer discardError options (apply s1 env) body

  let s3 = s2 `compose` s1

  s4 <- contextualUnify' env discardError cond tBool (apply s3 tcond)
  s5 <- contextualUnify' env discardError body tUnit (apply s3 tbody)

  let s = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
  let t = apply s tbody

  return (s, ps1 ++ ps2, t, Slv.Typed ((ps1 ++ ps2) :=> t) area (Slv.While econd ebody))



-- INFER DO

inferDo :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferDo discardError options env (Can.Canonical area (Can.Do exps)) = do
  (s, ps, t, exps') <- inferBody discardError options env exps
  (s', exps'')      <- postProcessBody discardError options env s t exps'

  return (s', apply s' ps, apply s' t, Slv.Typed (apply s' $ ps :=> t) area (Slv.Do exps''))



-- INFER WHERE

inferWhere :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferWhere discardError options env (Can.Canonical area (Can.Where exp iss)) = do
  (s, ps, t, e)          <- infer discardError options env exp
  tv                     <- newTVar Star
  (pss, issSubstitution) <- foldM
    (\(res, currSubst) is -> do
      r@(subst, _, _) <- inferBranch discardError options (apply currSubst env) (apply currSubst tv) t is
      return (res <> [r], subst `compose` currSubst)
    )
    ([], s)
    iss

  let ps' = concat $ T.mid <$> pss
  -- move this within the foldM
  s' <- contextualUnifyElems env $ zip iss (apply issSubstitution . Slv.getType . T.lst <$> pss)

  let s''  = s' `compose` issSubstitution

  let iss = (\(Slv.Typed t a (Slv.Is pat exp)) -> Slv.Typed (apply s'' t) a (Slv.Is (updatePatternTypes s'' mempty pat) exp)) . T.lst <$> pss
  let wher = Slv.Typed (apply s'' $ (ps ++ ps') :=> tv) area $ Slv.Where (updateQualType e (apply s'' $ ps :=> t)) iss
  return (s'', ps ++ ps', apply s'' tv, wher)


inferBranch :: Bool -> Options -> Env -> Type -> Type -> Can.Is -> Infer (Substitution, [Pred], Slv.Is)
inferBranch discardError options env tv t (Can.Canonical area (Can.Is pat exp)) = do
  (pat', ps, vars, t') <- inferPattern env pat
  s <- contextualUnify' env discardError exp t t'
  (s', ps', t'', e') <- infer discardError options (apply s $ mergeVars env vars) exp
  s'' <- contextualUnify' env discardError exp tv (apply (s `compose` s') t'')

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

inferTypedExp :: Bool -> Options -> Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)
inferTypedExp discardError options env e@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  (_ :=> t) <- instantiate sc
  (s1, ps1, t1, e1) <- infer discardError options env exp
  s2 <- contextualUnify' env discardError e t t1

  return
    ( s1 `compose` s2
    , apply s2 ps1
    , apply s2 t1
    , Slv.Typed (apply s2 $ ps1 :=> t1) area (Slv.TypedExp (updateQualType e1 (ps1 :=> t1)) (updateTyping typing) sc)
    )


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
  IsIn cls [tRec@(TRecord _ (Just base@(TVar _)) _)] maybeArea : next
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

  -- if not (null as) then do
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
    else do
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
      if null vars || isTVar t then
        return (nextSubst, nextPS)
      else do
        let tvs  = getTV <$> vars
            tvs' = filter (`M.notMember` nextSubst) tvs
            s    = M.fromList $ zip tvs' (tUnit <$ tvs')
        return (s `compose` nextSubst, nextPS)

    IsIn "Show" [t] _ -> do
      (nextSubst, nextPS) <- tryDefaults env next

      let vars = getTypeVarsInType t

      -- if it's a tvar we don't want to force it to {}
      -- because it should actually never be called
      if null vars || isTVar t then
        return (nextSubst, nextPS)
      else do
        let tvs  = getTV <$> vars
            tvs' = filter (`M.notMember` nextSubst) tvs
            s    = M.fromList $ zip tvs' (tUnit <$ tvs')
        return (s `compose` nextSubst, nextPS)

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
          parentPreds <- getParentPredsOnly env p
          (nextSubst, nextPS) <- tryDefaults env (parentPreds ++ next)
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
    ftv tl1 `List.union` ftv tr1

  TApp t1 t2 ->
    ftvForLetGen t1 `List.union` ftvForLetGen t2

  TRecord fields _ _ ->
    M.elems fields >>= ftvForLetGen

  _ ->
    []



inferImplicitlyTyped :: Bool -> Options -> Bool -> Env -> Can.Exp -> Infer (Substitution, ([Pred], [Pred]), Env, Slv.Exp)
inferImplicitlyTyped discardError options isLet env exp@(Can.Canonical area _) = do
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

  (s, ps, t, e) <- infer discardError options env' { envNamesInScope = M.keysSet (envVars env) } exp
  let env'' = apply s env'

  s' <- contextualUnify' env'' discardError exp (apply s tv) t
  let s'' = s `compose` s' `compose` s
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

  (ds, rs, sSplit) <- catchError
    (split (not isLet) envWithVarsExcluded fs (ftv t') ps')
    (\case
      _ | discardError ->
        return (ps', [], mempty)

      (CompilationError e NoContext) -> do
        throwError $ CompilationError e (Context (envCurrentPath env) area)

      (CompilationError e c) -> do
        throwError $ CompilationError e c
    )

  let rs' = dedupePreds rs
  let sFinal = sSplit `compose` s''

  let mutPS =
        List.filter
          (\(IsIn cls ts _) ->
            let freeTVs = ftv (apply s' ts) `intersect` ftv (apply s' t)
            in  cls == mutationInterface && not (null freeTVs)
          )
          ps

  let sc =
        if isLet && not (Slv.isNamedAbs e) then
          apply sFinal $ quantify [] ((rs' ++ mutPS) :=> t')
        else
          -- TODO: consider if the apply sFinal should not happen before quantifying
          -- because right now we might miss the defaulted types in the generated
          -- scheme
          apply sFinal $ quantify gs ((rs' ++ mutPS) :=> t')

  when (not isLet && not discardError && not (null mutPS) && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  case Can.getExpName exp of
    Just n  ->
      return (sFinal, (ds ++ mutPS, rs'), extendVars env (n, sc), updateQualType e (apply sFinal $ rs' :=> t'))

    Nothing ->
      return (sFinal, (ds ++ mutPS, rs'), env, updateQualType e (apply sFinal $ rs' :=> t'))


inferExplicitlyTyped :: Bool -> Options -> Bool -> Env -> Can.Exp -> Infer (Substitution, [Pred], Env, Slv.Exp)
inferExplicitlyTyped discardError options isLet env canExp@(Can.Canonical area (Can.TypedExp exp typing sc)) = do
  qt@(qs :=> t') <- instantiate sc

  env' <- case Can.getExpName exp of
        Just n  -> do
          let scWithParents = quantify (ftv qt) (qs :=> t')
          return $ extendVars env (n, scWithParents)

        Nothing ->
          return env

  (s, ps, t, e) <- infer discardError options env' { envNamesInScope = M.keysSet (envVars env) } exp
  psFull        <- concat <$> mapM (gatherInstPreds env') ps
  s'' <- catchError (contextualUnify' env discardError canExp t' (apply (s `compose` s) t)) (throwError . limitContextArea 2)
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
      _ | discardError ->
        return (ps', [], mempty)

      (CompilationError e NoContext) ->
        throwError $ CompilationError e (Context (envCurrentPath env) area)

      (CompilationError e c) ->
        throwError $ CompilationError e c
    )

  let mutPS =
        List.filter
          (\(IsIn cls ts _) ->
            let freeTVs = ftv (apply s' ts) `intersect` ftv (apply s t)
            in  cls == mutationInterface && not (null freeTVs)
          )
          ps

  when (not isLet && not discardError && not (null mutPS) && not (Slv.isNamedAbs e)) $ do
    throwError $ CompilationError MutationRestriction (Context (envCurrentPath env) area)

  let qs'' = dedupePreds qs'

  let scCheck  = quantify (ftv (apply s' t')) (qs' :=> apply substDefaultResolution (apply s' t'))
  if sc /= scCheck then
    throwError $ CompilationError (SignatureTooGeneral sc scCheck) (Context (envCurrentPath env') area)
  else if not (null rs) then
    throwError $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) area)
  else do
    let e'   = updateQualType e (ds :=> t''')

    let qt'  = (qs'' ++ mutPS) :=> t'''
    let sc'' = quantify gs qt'
    let env'' = case Can.getExpName exp of
          Just n  ->
            extendVars env' (n, sc'')
          Nothing ->
            env'

    return (substDefaultResolution `compose` s', qs'' ++ mutPS, env'', Slv.Typed (qs :=> t') area (Slv.TypedExp e' (updateTyping typing) sc))

inferExplicitlyTyped _ _ _ _ _ = undefined


inferExps :: Options -> Env -> [Can.Exp] -> Infer ([Slv.Exp], Env)
inferExps _ env []       = return ([], env)

inferExps options env (e : es) = do
  (e' , env'   ) <-
    catchError
      (inferExp False options env e)
      (\err -> do
        pushError err
        catchError (inferExp True options env e) (\_ -> return (Just $ toSolved e, env))
      )
  (es', nextEnv) <- inferExps options env' es

  case e' of
    Just e'' ->
      return (e'' : es', nextEnv)

    Nothing  ->
      return (es', nextEnv)


inferExp :: Bool -> Options -> Env -> Can.Exp -> Infer (Maybe Slv.Exp, Env)
inferExp _ _ env (Can.Canonical _ (Can.TypeExport _)) =
  return (Nothing, env)
inferExp discardError options env e = do
  (s, _, env', e') <- upgradeContext env (Can.getArea e) $ case e of
    Can.Canonical _ Can.TypedExp{} ->
      inferExplicitlyTyped discardError options False env e

    _ -> do
      (s, (_, placeholderPreds), env'', e') <- inferImplicitlyTyped discardError options False env e
      return (s, placeholderPreds, env'', e')


  e'' <- updateExpTypes options env' False s e'

  return (Just e'', env')


recordError :: Env -> Can.Exp -> CompilationError -> Infer (Maybe Slv.Exp, Env)
recordError env e err = do
  pushError err
  return (Just $ toSolved e, env)


upgradeContext :: Env -> Area -> Infer a -> Infer a
upgradeContext env area a = catchError a (throwError . upgradeContext' env area)


upgradeContext' :: Env -> Area -> CompilationError -> CompilationError
upgradeContext' env area err = case err of
  (CompilationError e NoContext) ->
    CompilationError e $ Context (envCurrentPath env) area

  (CompilationError e r) ->
    CompilationError e r
