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
import Infer.EnvUtils
import Utils.Record (generateRecordPredsAndType)


getAllParentPreds :: Env -> [Pred] -> Infer [Pred]
getAllParentPreds env ps = concat <$> mapM (getParentPreds env) ps

getParentPreds :: Env -> Pred -> Infer [Pred]
getParentPreds env p@(IsIn cls ts maybeArea) = do
  (Interface tvs ps _) <- lookupInterface env cls

  s <- unify (TVar <$> tvs) ts
  let ps' = (\(IsIn cls ts' _) -> IsIn cls (apply s ts') maybeArea) <$> ps
  nextPreds <- mapM (getParentPreds env) ps'

  return $ [p] `union` ps' `union` concat nextPreds


getAllInstancePreds :: Env -> Pred -> Infer [Pred]
getAllInstancePreds env p = do
  ps <- catchError (byInst env p) (const $ return [])
  more <- mapM (getAllInstancePreds env) ps
  return $ ps ++ concat more


liftPred :: ([Type] -> [Type] -> Infer a) -> Pred -> Pred -> Infer a
liftPred m (IsIn i ts _) (IsIn i' ts' _) | i == i'   = m ts ts'
                                         | otherwise = throwError $ CompilationError FatalError NoContext

instance Unify Pred where
  unify = liftPred unify

instance Match Pred where
  match = liftPred match

sig :: Env -> Id -> [TVar]
sig env i = case M.lookup i (envInterfaces env) of
  Just (Interface vs _ _) ->
    vs

  Nothing ->
    []


super :: Env -> Id -> [Pred]
super env i = case M.lookup i (envInterfaces env) of
  Just (Interface _ is _) ->
    is

  Nothing ->
    []


insts :: Env -> Id -> [Instance]
insts env i = case M.lookup i (envInterfaces env) of
  Just (Interface _ _ insts) ->
    insts

  Nothing ->
    []


bySuper :: Env -> Pred -> [Pred]
bySuper env p@(IsIn i ts maybeArea) =
  p : ((\(IsIn c ts' _) -> IsIn c ts' maybeArea) <$> concatMap (bySuper env) supers)
  where
    supers = apply s (super env i)
    s      = M.fromList $ zip (sig env i) ts


findInst :: Env -> Pred -> Infer (Maybe Instance)
findInst env p@(IsIn interface ts _) =
  catchError
    (Just <$> tryInsts (insts env interface))
    (const $ case ts of
      [TRecord fields _ _] | interface == "Eq" || interface == "Inspect" -> do
        let (fieldsPreds, tRec) = generateRecordPredsAndType (envCurrentPath env) interface (M.keys fields)
            qp = fieldsPreds :=> IsIn interface [tRec] Nothing
        return $ Just (Instance qp mempty)

      _ ->
        return Nothing
    )
 where
  tryInst i@(Instance (_ :=> h) _) = do
    isInstanceOf h p
    return i
  tryInsts []          =
    case p of
        IsIn _ _ (Just area) ->
          throwError $ CompilationError (NoInstanceFound interface ts) (Context (envCurrentPath env) area)
        _ ->
          throwError $ CompilationError (NoInstanceFound interface ts) NoContext
  tryInsts (inst : is) = catchError (tryInst inst) (\_ -> tryInsts is)

gatherInstPreds :: Env -> Pred -> Infer [Pred]
gatherInstPreds env p =
  catchError (byInst env p) (\_ -> return [p])


removeInstanceVars :: Pred -> Pred -> (Pred, Pred)
removeInstanceVars (IsIn cls ts maybeArea) (IsIn cls' ts' maybeArea') =
  let groupped = zip ts ts'
      filtered = filter (not . isTVar . fst) groupped
  in  (IsIn cls (fst <$> filtered) maybeArea, IsIn cls' (snd <$> filtered) maybeArea')


specialMatch :: Pred -> Pred -> Infer Substitution
specialMatch (IsIn cls ts _) (IsIn cls' ts' _) = do
  if cls == cls'
    then do
      let zipped = zip ts ts'
      foldM (\s (t, t') -> (s `compose`) <$> match t (apply s t')) M.empty zipped
    else throwError $ CompilationError FatalError NoContext


specialMatchMany :: [Pred] -> [Pred] -> Infer Substitution
specialMatchMany ps ps' = foldM (\s (a, b) -> M.union s <$> specialMatch a b) mempty (zip ps ps')


isConcrete :: Type -> Bool
isConcrete t = case t of
  TCon _ _ ->
    True

  TApp l _ ->
    isConcrete l

  TRecord _ _ _ ->
    True

  _ ->
    False


isInstanceOf :: Pred -> Pred -> Infer Substitution
isInstanceOf (IsIn interface ts _) (IsIn interface' ts' _) = do
  if interface == interface'
    then do
      let r  = zip ts ts'
      match (IsIn interface (fst <$> r) Nothing) (IsIn interface (snd <$> r) Nothing)
    else throwError $ CompilationError FatalError NoContext


byInst :: Env -> Pred -> Infer [Pred]
byInst env p@(IsIn "__MUTATION__" ts maybeArea) = return []
byInst env p@(IsIn interface ts maybeArea) =
  catchError
    (tryInsts (insts env interface))
    (\err -> case ts of
      [TRecord fields _ _] | interface == "Eq" || interface == "Inspect" -> do
        pushExtensibleRecordToDerive (M.keys fields)
        let (fieldsPreds, ts') = generateRecordPredsAndType (envCurrentPath env) interface (M.keys fields)
        u <- isInstanceOf (IsIn interface [ts'] Nothing) p
        return $ apply u fieldsPreds

      _ ->
        throwError err
    )
 where
  tryInst (Instance (ps :=> h) _) = do
    u <- isInstanceOf h p
    return $ apply u <$> ps
  tryInsts [] =
    if all isConcrete $ predTypes p then
      case maybeArea of
        Just area ->
          throwError $ CompilationError (NoInstanceFound interface ts) (Context (envCurrentPath env) area)
        _ ->
          throwError $ CompilationError (NoInstanceFound interface ts) NoContext
    else
      throwError $ CompilationError FatalError NoContext

  tryInsts (inst : is) = catchError (tryInst inst) (const $ tryInsts is)


allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM f = foldM (\b a -> f a >>= (return . (&& b))) True

entail :: Env -> [Pred] -> Pred -> Infer Bool
entail env ps p = do
  tt <- catchError
    (byInst env p >>= allM (entail env ps))
    (\case
      CompilationError FatalError _ -> return False
      e                             -> throwError e
    )
  return $ any ((p `elem`) . bySuper env) ps || tt

simplify :: ([Pred] -> Pred -> Bool) -> [Pred] -> [Pred]
simplify ent = loop []
 where
  loop rs [] = rs
  loop rs (p : ps) | ent (rs ++ ps) p = loop rs ps
                   | otherwise        = loop (p : rs) ps

reduce :: Env -> [Pred] -> Infer [Pred]
reduce env ps = do
  withoutTauts <- elimTauts env ps
  let r = simplify (scEntail env) withoutTauts
  return r

elimTauts :: Env -> [Pred] -> Infer [Pred]
elimTauts env = filterM ((not <$>) . entail env [])

scEntail :: Env -> [Pred] -> Pred -> Bool
scEntail env ps p = let supers = map (bySuper env) ps in any (p `elem`) supers
