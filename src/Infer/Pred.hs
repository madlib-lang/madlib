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
import           Explain.Reason
import           Control.Monad                  ( msum )
import           Control.Monad.Except
import           Data.List
import qualified Data.Map                      as M



getAllParentPreds :: Env -> [Pred] -> Infer [Pred]
getAllParentPreds env ps = concat <$> mapM (getParentPreds env) ps

getParentPreds :: Env -> Pred -> Infer [Pred]
getParentPreds env p@(IsIn cls ts) = do
  (Interface tvs ps _) <- case M.lookup cls (envinterfaces env) of
    Just x  -> return x
    Nothing -> throwError $ InferError (InterfaceNotExisting cls) NoReason

  s <- unify (TVar <$> tvs) ts

  let ps' = (\(IsIn cls ts') -> IsIn cls (apply s ts')) <$> ps

  nextPreds <- mapM (getParentPreds env) ps'

  return $ [p] `union` ps' `union` concat nextPreds


liftPred :: ([Type] -> [Type] -> Infer a) -> Pred -> Pred -> Infer a
liftPred m (IsIn i ts) (IsIn i' ts')
  | i == i'   = m ts ts'
  | otherwise = throwError $ InferError FatalError NoReason

instance Unify Pred where
  unify = liftPred unify

instance Match Pred where
  match = liftPred match

sig :: Env -> Id -> [TVar]
sig env i = case M.lookup i (envinterfaces env) of
  Just (Interface vs _ _) -> vs

super :: Env -> Id -> [Pred]
super env i = case M.lookup i (envinterfaces env) of
  Just (Interface _ is _) -> is

insts :: Env -> Id -> [Instance]
insts env i = case M.lookup i (envinterfaces env) of
  Just (Interface _ _ insts) -> insts

bySuper :: Env -> Pred -> [Pred]
bySuper env p@(IsIn i ts) = p : concatMap (bySuper env) supers
 where
  supers = apply s (super env i)
  s      = M.fromList $ zip (sig env i) ts

findInst :: Env -> Pred -> Infer (Maybe Instance)
findInst env p@(IsIn interface t) = do
  catchError (Just <$> tryInsts (insts env interface)) (const $ return Nothing)
 where
  tryInst i@(Instance (ps :=> h)) = do
    u <- match h p
    return i
  tryInsts []          = throwError $ InferError FatalError NoReason
  tryInsts (inst : is) = catchError (tryInst inst) (\e -> tryInsts is)


removeInstanceVars :: Pred -> Pred -> (Pred, Pred)
removeInstanceVars ip@(IsIn cls ts) p@(IsIn cls' ts') =
  let groupped = zip ts ts'
      filtered = filter (not . isTVar . fst) groupped
  in  (IsIn cls (fst <$> filtered), IsIn cls' (snd <$> filtered))


specialMatch :: Pred -> Pred -> Infer Substitution
specialMatch p@(IsIn cls ts) p'@(IsIn cls' ts') = do
  if cls == cls'
    then do
      let zipped = zip ts ts'
      foldM (\s (t, t') -> (s `compose`) <$> match t (apply s t'))
            M.empty
            zipped
    else throwError $ InferError FatalError NoReason

specialMatchMany :: [Pred] -> [Pred] -> Infer Substitution
specialMatchMany ps ps' =
  foldM (\s (a, b) -> M.union s <$> specialMatch a b) mempty (zip ps ps')


isConcrete :: Type -> Bool
isConcrete t = case t of
  TVar _      -> False
  TCon _      -> True
  TApp    l r -> isConcrete l
  TRecord _ _ -> True


byInst :: Env -> Pred -> Infer [Pred]
byInst env p@(IsIn interface ts) = tryInsts (insts env interface)
 where
  tryInst (Instance (ps :=> h)) = do
    u <- match h p
    let ps' = apply u <$> ps
    return ps'
  tryInsts [] = if all isConcrete $ predTypes p
    then throwError $ InferError (NoInstanceFound interface ts) NoReason
    else throwError $ InferError FatalError NoReason

  tryInsts (inst : is) = catchError (tryInst inst) (const $ tryInsts is)


allM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m Bool
allM f = foldM (\b a -> f a >>= (return . (&& b))) True

entail :: Env -> [Pred] -> Pred -> Infer Bool
entail env ps p = do
  tt <- catchError
    (byInst env p >>= allM (entail env ps))
    (\case
      InferError FatalError _ -> return False
      e                       -> throwError e
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
elimTauts env ps = filterM ((not <$>) . entail env []) ps

scEntail :: Env -> [Pred] -> Pred -> Bool
scEntail env ps p = let supers = map (bySuper env) ps in any (p `elem`) supers
