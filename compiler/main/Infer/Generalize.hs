{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Generalize
  ( Ambiguity
  , ambiguities
  , split
  , tryDefaults
  , dedupePreds
  , ftvForLetGenSet
  , ftvForLetGen
  , generalize
  , hasPredForType
  , updateRecordUpdatePreds
  ) where

-- | Predicate solving + generalization. Extracted from Infer.Exp during the
-- typechecker rewrite (Phase 5) so the main expression-inference module
-- doesn't have to carry the constraint-solving logic too.

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( partition )
import           Control.Monad.Except           ( catchError, throwError )

import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Substitute               ( apply, compose, ftv, ftvList )
import           Infer.Unify                    ( unify )
import           Infer.Pred                     ( reduce, findInst, getParentPredsOnly )
import           Error.Error
import           Error.Context
import           Explain.Location               ( Area )


type Ambiguity = (TVar, [Pred])


ambiguities :: [TVar] -> [Pred] -> [Ambiguity]
ambiguities vs ps =
  let vsSet = S.fromList vs
      ambigVars = ftv ps `S.difference` vsSet
  in  [ (v, filter (S.member v . ftv) ps) | v <- S.toList ambigVars ]


hasPredForType :: String -> Type -> [Pred] -> Bool
hasPredForType cls t ps =
  any (\(IsIn cls' ts _) -> t `elem` ts && cls == cls') ps


updateRecordUpdatePreds :: [Pred] -> [Pred]
updateRecordUpdatePreds ps = updateRecordUpdatePreds' ps ps

-- Preds for record with a base should be resolved by the base directly
-- We also emit a closed record pred (without base) so that explicit fields
-- are checked against user-defined type class instances.
-- Empty records (no fields, no base) are filtered out as they don't need instances.
updateRecordUpdatePreds' :: [Pred] -> [Pred] -> [Pred]
updateRecordUpdatePreds' allPreds ps = case ps of
  IsIn _ [TRecord fields Nothing optionalFields] _ : next
    | M.null fields && M.null optionalFields ->
      updateRecordUpdatePreds next

  IsIn cls [tRec@(TRecord fields (Just base@(TVar _)) optionalFields)] maybeArea : next
    | not (hasPredForType "Number" tRec allPreds)
    && not (hasPredForType "Bits" tRec allPreds)
    && not (hasPredForType "Number" base allPreds)
    && not (hasPredForType "Bits" base allPreds) ->
      if M.null fields && M.null optionalFields then
        IsIn cls [base] maybeArea : updateRecordUpdatePreds next
      else
        IsIn cls [base] maybeArea
          : IsIn cls [TRecord fields Nothing optionalFields] maybeArea
          : updateRecordUpdatePreds next

  or : next ->
    or : updateRecordUpdatePreds next

  _ ->
    []


split :: Bool -> Env -> [TVar] -> [TVar] -> [Pred] -> Infer ([Pred], [Pred], Substitution)
split mustCheck env fs gs ps = do
  ps' <- reduce env (updateRecordUpdatePreds ps)
  let fsSet = S.fromList fs
      (ds, rs) = partition ((`S.isSubsetOf` fsSet) . ftv) ps'
  let as = ambiguities (fs ++ gs) rs

  if mustCheck && not (null as) then do
    -- if we have ambiguities we try to resolve them with default instances
    (s, rs')      <- tryDefaults env rs
    (sDef', rs'') <- tryDefaults env (apply s rs')
    let (ds', rs''') = partition ((`S.isSubsetOf` fsSet) . ftv) (apply sDef' ds ++ rs'')

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
tryDefaults env ps = tryDefaults' env ps ps
  where
    tryDefaults' :: Env -> [Pred] -> [Pred] -> Infer (Substitution, [Pred])
    tryDefaults' env originalPs remainingPs = case remainingPs of
      (p : next) -> case p of
        IsIn "Number" [TVar tv] _ -> do
          (nextSubst, nextPS) <- tryDefaults' env originalPs next
          let s = M.singleton tv tInteger
          return (s `compose` nextSubst, nextPS)

        IsIn "Bits" [TVar tv] _ -> do
          (nextSubst, nextPS) <- tryDefaults' env originalPs next
          let s = M.singleton tv tInteger
          return (s `compose` nextSubst, nextPS)

        IsIn interface [t] _ | interface == "Eq" || interface == "Show" -> do
          (nextSubst, nextPS) <- tryDefaults' env originalPs next

          -- Get vars from type AFTER substitution to see what's left
          let substitutedVars = getTypeVarsInType (apply nextSubst t)

          if null substitutedVars || isTVar t then
            return (nextSubst, nextPS)
          else do
            let tvs = getTV <$> substitutedVars

            -- Check ORIGINAL predicate list (all predicates) for Number/Bits constraints
            let hasNumberOrBitsConstraint tv = any
                  (\pred -> case pred of
                    IsIn "Number" [TVar tv'] _ -> tv == tv'
                    IsIn "Bits" [TVar tv'] _ -> tv == tv'
                    _ -> False
                  )
                  originalPs

            -- Also check if already substituted to Integer
            let isAlreadyInteger tv = case M.lookup tv nextSubst of
                  Just ty | ty == tInteger -> True
                  _ -> False

            let tvs' = filter (\tv -> not (M.member tv nextSubst) && (hasNumberOrBitsConstraint tv || isAlreadyInteger tv)) tvs

            let tvsWithoutNumberOrBits = filter (\tv ->
                    not (M.member tv nextSubst) &&
                    not (hasNumberOrBitsConstraint tv) &&
                    not (isAlreadyInteger tv)
                  ) tvs

            -- Don't default variables in complex types to Unit - they might get Number constraints
            -- through instance resolution. Only default simple type variables.
            let isSimpleTypeVar = isTVar t
            let shouldDefaultToUnit tv = isSimpleTypeVar && not (hasNumberOrBitsConstraint tv) && not (isAlreadyInteger tv)

            sList <- mapM (\tv ->
                if hasNumberOrBitsConstraint tv || isAlreadyInteger tv
                  then return (Just (tv, tInteger))
                  else if shouldDefaultToUnit tv
                  then return (Just (tv, tUnit))
                  else
                    return Nothing
              ) (tvs' ++ tvsWithoutNumberOrBits)
            let s = M.fromList $ catMaybes sList

            return (s `compose` nextSubst, nextPS)

        _ -> do
          maybeFound <- findInst env p
          case maybeFound of
            Just (Instance (instancePreds :=> pred) _) -> do
              s                   <- unify pred p
              (nextSubst, nextPS) <- tryDefaults' env originalPs (next ++ apply s instancePreds)
              return (nextSubst, nextPS)

            Nothing -> do
              parentPreds <- getParentPredsOnly env p
              (nextSubst, nextPS) <- tryDefaults' env originalPs (parentPreds ++ next)
              return (nextSubst, p : nextPS)

      [] ->
        return (M.empty, [])


dedupePreds :: [Pred] -> [Pred]
dedupePreds = go S.empty []
  where
    -- Use a Set of (class, types) for O(n log n) dedup instead of O(n²) list scan
    go _ acc [] = reverse acc
    go seen acc (p@(IsIn cls ts _) : next) =
      let key = (cls, ts)
      in  if S.member key seen
          then go seen acc next
          else go (S.insert key seen) (p : acc) next


ftvForLetGenSet :: Type -> S.Set TVar
ftvForLetGenSet t = case t of
  TApp (TApp (TCon (TC "(->)" _) _ _) tl1) tr1 ->
    ftv tl1 `S.union` ftv tr1

  TApp t1 t2 ->
    ftvForLetGenSet t1 `S.union` ftvForLetGenSet t2

  TRecord fields _ _ ->
    foldMap ftvForLetGenSet (M.elems fields)

  _ ->
    S.empty

ftvForLetGen :: Type -> [TVar]
ftvForLetGen = S.toList . ftvForLetGenSet


-- | Shared generalization logic: compute free/generic vars, split predicates.
-- Mutation tracking lives in InferState.mutatedNames; callers consult it
-- separately via isMutated to decide whether to quantify or to error out.
generalize :: Bool -> Env -> Area -> Substitution -> Env -> Type -> [Pred] -> Type -> Infer ([Pred], [Pred], Substitution)
generalize isLet env area sFinal envWithVarsExcluded t' ps' _ = do
  discardError <- isDiscardingErrors
  let fs = S.toList (ftv (apply sFinal envWithVarsExcluded))

  (ds, rs, sSplit) <- catchError
    (split (not isLet) envWithVarsExcluded fs (ftvList t') ps')
    (\case
      _ | discardError ->
        return (ps', [], mempty)

      (CompilationError e NoContext) -> do
        throwError $ CompilationError e (Context (envCurrentPath env) area)

      (CompilationError e c) -> do
        throwError $ CompilationError e c
    )

  let rs' = dedupePreds rs
  let sFinal' = sSplit `compose` sFinal

  return (ds, rs', sFinal')
