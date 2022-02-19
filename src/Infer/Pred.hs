{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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
import Debug.Trace
import Text.Show.Pretty


getAllParentPreds :: Env -> [Pred] -> Infer [Pred]
getAllParentPreds env ps = concat <$> mapM (getParentPreds env) ps

getParentPreds :: Env -> Pred -> Infer [Pred]
getParentPreds env p@(IsIn cls ts maybeArea) = do
  (Interface tvs ps _) <- case M.lookup cls (envInterfaces env) of
    Just x  -> return x
    Nothing -> throwError $ CompilationError (InterfaceNotExisting cls) NoContext

  s <- unify (TVar <$> tvs) ts

  let ps' = (\(IsIn cls ts' _) -> IsIn cls (apply s ts') maybeArea) <$> ps

  nextPreds <- mapM (getParentPreds env) ps'

  return $ [p] `union` ps' `union` concat nextPreds


liftPred :: ([Type] -> [Type] -> Infer a) -> Pred -> Pred -> Infer a
liftPred m (IsIn i ts _) (IsIn i' ts' _) | i == i'   = m ts ts'
                                         | otherwise = throwError $ CompilationError FatalError NoContext

instance Unify Pred where
  unify = liftPred unify

instance Match Pred where
  match = liftPred match

sig :: Env -> Id -> [TVar]
sig env i = case M.lookup i (envInterfaces env) of
  Just (Interface vs _ _) -> vs
  Nothing                 -> []

super :: Env -> Id -> [Pred]
super env i = case M.lookup i (envInterfaces env) of
  Just (Interface _ is _) -> is
  Nothing                 -> []

insts :: Env -> Id -> [Instance]
insts env i = case M.lookup i (envInterfaces env) of
  Just (Interface _ _ insts) -> insts
  Nothing                    -> []

bySuper :: Env -> Pred -> [Pred]
bySuper env p@(IsIn i ts maybeArea) =
  p : ((\(IsIn c ts' _) -> IsIn c ts' maybeArea) <$> concatMap (bySuper env) supers)
  where
    supers = apply s (super env i)
    s      = M.fromList $ zip (sig env i) ts

findInst :: Env -> Pred -> Infer (Maybe Instance)
findInst env p@(IsIn interface t _) = do
  catchError (Just <$> tryInsts (insts env interface)) (const $ return Nothing)
 where
  tryInst i@(Instance (ps :=> h) _) = do
    u <- isInstanceOf h p
    return i
  tryInsts []          =
    case p of
        IsIn _ _ (Just area) ->
          throwError $ CompilationError (NoInstanceFound interface t) (Context (envCurrentPath env) area (envBacktrace env))
        _ ->
          throwError $ CompilationError (NoInstanceFound interface t) NoContext
  tryInsts (inst : is) = catchError (tryInst inst) (\e -> tryInsts is)

gatherInstPreds :: Env -> Pred -> Infer [Pred]
gatherInstPreds env p =
  catchError (byInst env p) (\_ -> return [p])


removeInstanceVars :: Pred -> Pred -> (Pred, Pred)
removeInstanceVars ip@(IsIn cls ts maybeArea) p@(IsIn cls' ts' maybeArea') =
  let groupped = zip ts ts'
      filtered = filter (not . isTVar . fst) groupped
  in  (IsIn cls (fst <$> filtered) maybeArea, IsIn cls' (snd <$> filtered) maybeArea')


specialMatch :: Pred -> Pred -> Infer Substitution
specialMatch p@(IsIn cls ts _) p'@(IsIn cls' ts' _) = do
  if cls == cls'
    then do
      let zipped = zip ts ts'
      foldM (\s (t, t') -> (s `compose`) <$> match t (apply s t')) M.empty zipped
    else throwError $ CompilationError FatalError NoContext

specialMatchMany :: [Pred] -> [Pred] -> Infer Substitution
specialMatchMany ps ps' = foldM (\s (a, b) -> M.union s <$> specialMatch a b) mempty (zip ps ps')


isConcrete :: Type -> Bool
isConcrete t = case t of
  TVar _      -> False
  TCon    _ _ -> True
  TApp    l r -> isConcrete l
  TRecord _ _ -> True


isInstanceOf :: Pred -> Pred -> Infer Substitution
isInstanceOf p@(IsIn interface ts _) p'@(IsIn interface' ts' _) = do
  if interface == interface'
    then do
      let r  = filter (\(t1, t2) -> not (isTVar t1)) (zip ts ts')
      let r' = filter (\(t1, t2) -> isTVar t1) (zip ts ts')
      s1 <- unify (IsIn interface (fst <$> r') Nothing) (IsIn interface (snd <$> r') Nothing)
      s2 <- match (IsIn interface (fst <$> r) Nothing) (IsIn interface (snd <$> r) Nothing)
      return $ s1 <> s2
    else throwError $ CompilationError FatalError NoContext

byInst :: Env -> Pred -> Infer [Pred]
byInst env p@(IsIn interface ts maybeArea) = tryInsts (insts env interface)
 where
  tryInst (Instance (ps :=> h) _) = do
    u <- isInstanceOf h p
    let ps' = apply u <$> ps
    return ps'
  tryInsts [] =
    if all isConcrete $ predTypes p then
      case maybeArea of
        Just area ->
          throwError $ CompilationError (NoInstanceFound interface ts) (Context (envCurrentPath env) area (envBacktrace env))
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
