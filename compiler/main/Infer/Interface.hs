{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Infer.Interface where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Env
import           Infer.EnvUtils
import           Infer.Infer
import           Infer.Type
import           Infer.Exp
import           Infer.Instantiate
import           Infer.Pred
import           Infer.Substitute
import           Infer.Scheme
import           Infer.Unify
import           Infer.Placeholder
import           Error.Error
import           Error.Context
import qualified Data.Map                      as M
import           Data.List
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import Debug.Trace
import Text.Show.Pretty
import Run.Options


-- defined :: Maybe a -> Bool
-- defined (Just x) = True
-- defined Nothing  = False

-- overlap       :: Env -> Pred -> Pred -> Bool
-- overlap env p q = defined (unify env p q)

addInterface :: Env -> Id -> [TVar] -> [Pred] -> Infer Env
addInterface env id tvs ps = case M.lookup id (envInterfaces env) of
  Just _  ->
    throwError $ CompilationError (InterfaceAlreadyDefined id) NoContext

  Nothing ->
    return env { envInterfaces = M.insert id (Interface tvs ps []) (envInterfaces env) }


verifyInstancePredicates :: Env -> Pred -> Pred -> Infer Bool
verifyInstancePredicates env p' p@(IsIn cls ts _) = do
  (Interface tvs _ _) <- lookupInterface env cls
  let tvs' = (\(TV n k) -> TV ("_" <> n) k) <$> tvs
  catchError
    (unify (TVar <$> tvs') ts >> return True)
    (\_ -> throwError $ CompilationError (InstancePredicateError p' p (IsIn cls (TVar <$> tvs) Nothing)) NoContext)

-- Add test for overlap that should also test for kind of the given type !!
addInstance :: Env -> [Pred] -> Pred -> Infer Env
addInstance env ps p@(IsIn cls ts _) = do
  (Interface tvs ps' is) <- lookupInterface env cls

  mapM_ (verifyInstancePredicates env p) ps

  let ts'    = TVar <$> tvs
  s <- match ts' ts
  catchError (mapM_ (isInstanceDefined env s) ps')
              (\e@(CompilationError (NoInstanceFound _ ts) _) -> when (all isConcrete ts) (throwError e))
  return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance (ps :=> p) mempty : is)) (envInterfaces env)
              }

addInstanceMethod :: Env -> [Pred] -> Pred -> (String, Scheme) -> Infer Env
addInstanceMethod env _ p@(IsIn cls _ _) (methodName, methodScheme) = do
  (Interface tvs ps' is) <- lookupInterface env cls

  maybeInstance <- findInst env p
  case maybeInstance of
    Just (Instance qp methods) -> do
      let methods'    = M.insert methodName methodScheme methods
      return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance qp methods' : is)) (envInterfaces env) }

setInstanceMethods :: Env -> Pred -> Vars -> Infer Env
setInstanceMethods env p@(IsIn cls _ _) methods = do
  (Interface tvs ps' is) <- lookupInterface env cls

  maybeInstance <- findInst env p
  case maybeInstance of
    Just (Instance qp _) -> do
      return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance qp methods : is)) (envInterfaces env) }


findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

isInstanceDefined :: Env -> Substitution -> Pred -> Infer Bool
isInstanceDefined env subst (IsIn id ts _) = do
  let is = insts env id
  found <- findM
    (\(Instance (_ :=> (IsIn _ ts' _)) _) ->
      catchError (match ts' (apply subst ts) >>= \_ -> return $ Just True) (const $ return Nothing)
    )
    is
  case found of
    Just _  -> return True
    Nothing -> throwError $ CompilationError (NoInstanceFound id (apply subst ts)) NoContext


resolveInstances :: Options -> Env -> [Can.Instance] -> Infer (Env, [Slv.Instance])
resolveInstances _ env []       = return (env, [])
resolveInstances options env (i : is) = do
  curr <- catchError
    (Just <$> resolveInstance options env i)
    (\err -> do
      pushError err
      return Nothing
    )

  case curr of
    Just (env', inst) -> do
      (nextEnv, insts) <- resolveInstances options env' is
      return (nextEnv, inst : insts)

    Nothing ->
      resolveInstances options env is


resolveInstance :: Options -> Env -> Can.Instance -> Infer (Env, Slv.Instance)
resolveInstance options env inst@(Can.Canonical area (Can.Instance name constraintPreds pred methods)) = do
  let instanceTypes = predTypes pred
  let subst = foldr (\t s -> s `compose` buildVarSubsts t) mempty instanceTypes
  (Interface _ ps _) <- catchError (lookupInterface env name) (addContext env inst)
  let instancePreds = apply subst $ [IsIn name instanceTypes Nothing] <> ps
  let psTypes       = concat $ predTypes <$> constraintPreds
  let subst'        = foldr (\t s -> s `compose` buildVarSubsts t) mempty psTypes
  inferredMethods <- mapM
    (inferMethod options env (apply subst' instancePreds) (apply subst' constraintPreds))
    (M.toList methods)
  let dict'    = M.fromList $ (\(a, b, c) -> (a, (b, c))) <$> inferredMethods
  let methods' = M.fromList $ (\(a, _, c) -> (a, c)) <$> inferredMethods
  envWithMethods <- setInstanceMethods env pred methods'
  return (envWithMethods, Slv.Untyped area $ Slv.Instance name constraintPreds pred dict')


inferMethod :: Options -> Env -> [Pred] -> [Pred] -> (Can.Name, Can.Exp) -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod options env instancePreds constraintPreds (mn, m) =
  upgradeContext env (Can.getArea m) (inferMethod' options env instancePreds constraintPreds (mn, m))


inferMethod' :: Options -> Env -> [Pred] -> [Pred] -> (Can.Name, Can.Exp) -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod' options env instancePreds constraintPreds (mn, Can.Canonical area (Can.Assignment _ m)) = do
  sc'            <- lookupVar env mn
  qt@(mps :=> _) <- instantiate sc'
  s1             <- specialMatchMany mps instancePreds
  let (_ :=> mt') = apply s1 qt
  let qt'         = constraintPreds :=> mt'

  let sc          = quantify (ftv qt') qt'

  (s, ps, t, e) <- infer options env m
  (qs :=> t')   <- instantiate sc
  s'            <- (`compose` s) <$> unify t' t

  let qs' = apply s' qs
      t'' = apply s' t'
      fs  = ftv (apply s' env)
      gs  = ftv t'' \\ fs
      sc' = quantify (ftv t'') (qs' :=> t'')
  ps'         <- filterM ((not <$>) . entail env qs') (apply s' ps)

  (ds, rs, _) <- split True env fs gs ps'

  withParents <- getAllParentPreds env qs'

  if sc /= sc'
    then throwError $ CompilationError (SignatureTooGeneral sc sc')
                                       (Context (envCurrentPath env) (Can.getArea m))
    else if not (null rs)
      then throwError
        $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) (Can.getArea m))
      else do
        let e' = updateQualType e (qs :=> t'')
        e''  <- insertClassPlaceholders options env (Slv.Typed (apply s' ds :=> apply s' t) area $ Slv.Assignment mn e') (apply s' withParents)
        e''' <- updatePlaceholders options env True s' e''

        return (mn, e''', sc)
