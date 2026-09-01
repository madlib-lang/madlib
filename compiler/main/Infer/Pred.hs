{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Infer.Pred where

import           Infer.Type
import           Infer.Env
import           Infer.Substitute
import           Infer.Unify
import           Infer.Infer
import           Error.Error
import           Error.Context
import           Control.Monad                  ( msum )
import           Control.Monad.Except
import           Data.List
import qualified Data.Map                      as M
import qualified Data.Set                      as Set
import Infer.EnvUtils
import Utils.Record (generateRecordPredsAndType)
import qualified Data.Maybe as Maybe


getAllParentPreds :: Env -> [Pred] -> Infer [Pred]
getAllParentPreds env ps = concat <$> mapM (getParentPreds env) ps

getParentPreds :: Env -> Pred -> Infer [Pred]
getParentPreds env = go Set.empty
  where
    go seen p@(IsIn cls ts maybeArea)
      | Set.member (predKey p) seen = return []
      | otherwise = do
          (Interface tvs parents _) <- lookupInterface env cls
          s <- unify (TVar <$> tvs) ts
          let parents' = (\(IsIn parentCls parentTs _) -> IsIn parentCls (apply s parentTs) maybeArea) <$> parents
          nested <- concat <$> mapM (go (Set.insert (predKey p) seen)) parents'
          return (p : parents' ++ nested)

getParentPredsOnly :: Env -> Pred -> Infer [Pred]
getParentPredsOnly env p = filter (/= p) <$> getParentPreds env p


getAllInstancePreds :: Env -> Pred -> Infer [Pred]
getAllInstancePreds env = go Set.empty
  where
    go seen p
      | Set.member (predKey p) seen = return []
      | otherwise = do
          ps <- catchError (byInst env p) (const $ return [])
          more <- concat <$> mapM (go (Set.insert (predKey p) seen)) ps
          return (ps ++ more)


liftPred :: ([Type] -> [Type] -> Infer a) -> Pred -> Pred -> Infer a
liftPred m (IsIn i ts _) (IsIn i' ts' _) | i == i'   = m ts ts'
                                         | otherwise = throwWithContext FatalError

instance Unify Pred where
  unify = liftPred unify

instance Match Pred where
  match = liftPred match

insts :: Env -> Id -> [Instance]
insts env i = case M.lookup i (envInterfaces env) of
  Just (Interface _ _ insts) ->
    insts

  Nothing ->
    []


bySuper :: Env -> Pred -> [Pred]
bySuper env = go Set.empty
  where
    go seen p@(IsIn i ts maybeArea)
      | Set.member (predKey p) seen = []
      | otherwise = case M.lookup i (envInterfaces env) of
          Just (Interface vars supers _)
            | length vars == length ts ->
                let seen' = Set.insert (predKey p) seen
                    subst = M.fromList (zip vars ts)
                    supers' = map (\(IsIn cls args inheritedArea) ->
                      IsIn cls
                        (apply subst args)
                        (if Maybe.isNothing inheritedArea then maybeArea else inheritedArea)
                      ) supers
                in p : concatMap (go seen') supers'
          -- Arity is validated at predicate construction boundaries. Do not
          -- derive evidence from a malformed predicate if an invariant is
          -- nevertheless violated here.
          _ -> [p]


findInst :: Env -> Pred -> Infer (Maybe Instance)
findInst env p@(IsIn interface ts _) =
  catchError
    (Just <$> tryInsts candidates)
    (const $ case ts of
      [TRecordRow row optionalFields]
        | interface == "Eq" || interface == "Show"
        , (requiredFields, Nothing) <- visibleRow row -> do
        let fields = requiredFields <> optionalFields
        let (fieldsPreds, tRec) = generateRecordPredsAndType (envCurrentPath env) interface (M.keys fields)
            qp = fieldsPreds :=> IsIn interface [tRec] Nothing
        return $ Just (Instance qp mempty)

      _ ->
        return Nothing
    )
 where
  candidates
    | isOpenRecordWanted interface ts = []
    | otherwise = filter (\(Instance (_ :=> h) _) -> quickMatchPred h p) (insts env interface)
  tryInst i@(Instance (_ :=> h) _) = do
    isInstanceOf h p
    return i
  tryInsts []          =
    case p of
        IsIn _ _ (Just area) ->
          throwError $ CompilationError (NoInstanceFound interface ts []) (Context (envCurrentPath env) area)
        _ ->
          throwWithContext (NoInstanceFound interface ts [])
  tryInsts (inst : is) = catchError (tryInst inst) (\_ -> tryInsts is)

gatherInstPreds :: Env -> Pred -> Infer [Pred]
gatherInstPreds env p =
  catchError (byInst env p) (\_ -> return [p])


specialMatch :: Pred -> Pred -> Infer Substitution
specialMatch (IsIn cls ts _) (IsIn cls' ts' _)
  | cls == cls' = match ts ts'
  | otherwise = throwWithContext FatalError


quickMatchPred :: Pred -> Pred -> Bool
quickMatchPred (IsIn i ts _) (IsIn i' ts' _) =
  i == i' && length ts == length ts' && and (zipWith quickMatch ts ts')


isConcrete :: Type -> Bool
isConcrete t = case t of
  TCon _ _ _ ->
    True

  TApp l r ->
    isConcrete l && isConcrete r

  TRecordRow row optionalFields
    | (fields, Nothing) <- visibleRow row ->
        all isConcrete (M.elems fields) && all isConcrete (M.elems optionalFields)

  -- Aliases are transparent to instance resolution.  Treating every alias as
  -- non-concrete makes a fully-known goal look deferred forever and can hide
  -- a real missing-instance error behind the generic inference fallback.
  TAlias _ _ _ t' ->
    isConcrete t'

  _ ->
    False


isInstanceOf :: Pred -> Pred -> Infer Substitution
isInstanceOf (IsIn interface ts _) (IsIn interface' ts' _)
  | interface == interface' = match ts ts'
  | otherwise               = throwWithContext FatalError


byInst :: Env -> Pred -> Infer [Pred]
byInst env p@(IsIn interface ts maybeArea) =
  catchError
    (tryInsts candidates)
    (\err -> case ts of
      [TRecordRow row optionalFields]
        | interface == "Eq" || interface == "Show"
        , (requiredFields, Nothing) <- visibleRow row -> do
        let fields = requiredFields <> optionalFields
        pushExtensibleRecordToDerive (M.keys fields)
        let (fieldsPreds, ts') = generateRecordPredsAndType (envCurrentPath env) interface (M.keys fields)
        u <- isInstanceOf (IsIn interface [ts'] Nothing) p
        return $ apply u fieldsPreds

      _ ->
        throwError err
    )
 where
  candidates
    | isOpenRecordWanted interface ts = []
    | otherwise = filter (\(Instance (_ :=> h) _) -> quickMatchPred h p) (insts env interface)
  tryInst (Instance (ps :=> h) _) = do
    u <- isInstanceOf h p
    return $ apply u <$> ps
  tryInsts [] =
    if all isConcrete $ predTypes p then
      case maybeArea of
        Just area ->
          throwError $ CompilationError (NoInstanceFound interface ts []) (Context (envCurrentPath env) area)
        _ ->
          throwWithContext (NoInstanceFound interface ts [])
    else
      throwWithContext FatalError

  tryInsts (inst : is) = catchError (tryInst inst) (const $ tryInsts is)


-- A closed, shape-specific record instance is not evidence for an open
-- record.  Matching it would bind the unknown tail to the empty row and
-- silently discard constraints for fields supplied later by a caller.
isOpenRecordWanted :: Id -> [Type] -> Bool
isOpenRecordWanted interface ts =
  (interface == "Eq" || interface == "Show")
  && case ts of
    [TRecordRow row _] -> case visibleRow row of
      (_, Just _) -> True
      _           -> False
    _ -> False


allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM f = foldM (\b a -> f a >>= (return . (&& b))) True

entail :: Env -> [Pred] -> Pred -> Infer Bool
entail env ps = go Set.empty []
  where
    go seen path p
      -- Givens are evidence; do not unfold an instance before consulting
      -- them.  Apart from being cheaper this prevents a recursive instance
      -- graph from diverging on a goal that is already available.
      | any ((p `elem`) . bySuper env) ps = return True
      | Set.member (predKey p) seen = throwWithContext (InstanceResolutionCycle (reverse (p : path)))
      | otherwise = do
          tt <- catchError
            (byInst env p >>= allM (\q -> catchError (go (Set.insert (predKey p) seen) (p : path) q) (throwError . addRequiredBy p)))
            (\case
              CompilationError FatalError _ -> return False
              e                             -> throwError e
            )
          return tt

    -- A nested predicate `q` (required by `p`'s instance) failed to resolve:
    -- record that `p` is what required it, building a required-by chain
    -- (innermost first) as the error unwinds back through each caller.
    -- `byInst env p` failing for `p` itself is handled by the caller of
    -- `entail`, not here, so `p` is never appended to its own failure.
    addRequiredBy parent err = case err of
      CompilationError (NoInstanceFound cls ts chain) ctx ->
        CompilationError (NoInstanceFound cls ts (chain ++ [parent])) ctx

      _ ->
        err


-- | ent takes two separate lists (retained, remaining) and tests if p is
-- entailed — avoids O(n²) allocation of (rs ++ ps) at each loop step.
simplify :: ([Pred] -> [Pred] -> Pred -> Bool) -> [Pred] -> [Pred]
simplify ent = loop []
 where
  loop rs [] = rs
  loop rs (p : ps) | ent rs ps p = loop rs ps
                   | otherwise   = loop (p : rs) ps

reduce :: Env -> [Pred] -> Infer [Pred]
reduce env ps = do
  withoutTauts <- elimTauts env ps
  let superCache    = M.fromList [(p, bySuper env p) | p <- withoutTauts]
      cachedBySuper p = M.findWithDefault (bySuper env p) p superCache
      scEntailFast retained remaining p =
        any ((p `elem`) . cachedBySuper) retained
        || any ((p `elem`) . cachedBySuper) remaining
  return $ simplify scEntailFast withoutTauts

elimTauts :: Env -> [Pred] -> Infer [Pred]
elimTauts env = filterM ((not <$>) . entail env [])

scEntail :: Env -> [Pred] -> Pred -> Bool
scEntail env ps p = let supers = map (bySuper env) ps in any (p `elem`) supers
