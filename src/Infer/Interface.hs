{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Interface where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Env
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
import           Explain.Location
import qualified Data.Map                      as M
import           Data.List
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import Debug.Trace
import Text.Show.Pretty


-- defined :: Maybe a -> Bool
-- defined (Just x) = True
-- defined Nothing  = False

-- overlap       :: Env -> Pred -> Pred -> Bool
-- overlap env p q = defined (unify env p q)

addInterface :: Env -> Id -> [TVar] -> [Pred] -> Infer Env
addInterface env id tvs ps = case M.lookup id (envInterfaces env) of
  Just x -> return env
  -- Just x  -> throwError $ InferError (InterfaceAlreadyDefined id) NoContext
  Nothing -> return env { envInterfaces = M.insert id (Interface tvs ps []) (envInterfaces env) }


verifyInstancePredicates :: Env -> Pred -> Pred -> Infer Bool
verifyInstancePredicates env p' p@(IsIn cls ts) = do
  case M.lookup cls (envInterfaces env) of
    Nothing                     -> throwError $ InferError (InterfaceNotExisting cls) NoContext

    Just (Interface tvs ps' is) -> catchError
      (unify (TVar <$> tvs) ts >> return True)
      (\_ -> throwError $ InferError (InstancePredicateError p' p (IsIn cls (TVar <$> tvs))) NoContext)

-- Add test for overlap that should also test for kind of the given type !!
addInstance :: Env -> [Pred] -> Pred -> Infer Env
addInstance env ps p@(IsIn cls ts) = case M.lookup cls (envInterfaces env) of
  Nothing                     -> throwError $ InferError (InterfaceNotExisting cls) NoContext

  Just (Interface tvs ps' is) -> do
    mapM_ (verifyInstancePredicates env p) ps

    let ts'    = TVar <$> tvs
    let zipped = zip ts' ts
    s <- match ts' ts
    catchError (mapM_ (isInstanceDefined env s) ps')
               (\e@(InferError (NoInstanceFound _ ts) _) -> when (all isConcrete ts) (throwError e))
    return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance (ps :=> p) : is)) (envInterfaces env) }


findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

isInstanceDefined :: Env -> Substitution -> Pred -> Infer Bool
isInstanceDefined env subst (IsIn id ts) = do
  let is = insts env id
  found <- findM
    (\(Instance (_ :=> (IsIn _ ts'))) ->
      catchError (match ts' (apply subst ts) >>= \_ -> return $ Just True) (const $ return Nothing)
    )
    is
  case found of
    Just _  -> return True
    Nothing -> throwError $ InferError (NoInstanceFound id (apply subst ts)) NoContext


resolveInstances :: Env -> [Can.Instance] -> Infer [Slv.Instance]
resolveInstances _   []       = return []
resolveInstances env (i : is) = do
  next <- resolveInstances env is
  curr <- catchError
    (Just <$> resolveInstance env i)
    (\err -> do
      pushError err
      return Nothing
    )
  case curr of
    Just x  -> return $ x : next
    Nothing -> return next


resolveInstance :: Env -> Can.Instance -> Infer Slv.Instance
resolveInstance env inst@(Can.Canonical area (Can.Instance name constraintPreds pred methods)) = do
  let instanceTypes = predTypes pred
  let subst = foldl (\s t -> s `compose` buildVarSubsts t) mempty instanceTypes
  (Interface _ ps _) <- catchError (lookupInterface env name) (addContext env inst)
  let instancePreds = apply subst $ [IsIn name instanceTypes] <> ps
  let psTypes       = concat $ predTypes <$> constraintPreds
  let subst'        = foldl (\s t -> s `compose` buildVarSubsts t) mempty psTypes
  inferredMethods <- mapM
    (inferMethod (pushInstanceToBT env inst) (apply subst' instancePreds) (apply subst' constraintPreds))
    (M.toList methods)
  let dict' = M.fromList $ (\(a, b, c) -> (a, (b, c))) <$> inferredMethods
  return $ Slv.Untyped area $ Slv.Instance name constraintPreds pred dict'


inferMethod :: Env -> [Pred] -> [Pred] -> (Can.Name, Can.Exp) -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod env instancePreds constraintPreds (mn, m) =
  upgradeContext (pushExpToBT env m) (Can.getArea m) (inferMethod' env instancePreds constraintPreds (mn, m))


inferMethod' :: Env -> [Pred] -> [Pred] -> (Can.Name, Can.Exp) -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod' env instancePreds constraintPreds (mn, Can.Canonical area (Can.Assignment n' m)) = do
  sc'             <- lookupVar env mn
  qt@(mps :=> mt) <- instantiate sc'
  s1              <- specialMatchMany mps instancePreds
  let (mps' :=> mt')  = apply s1 qt
  let qt'@(qs :=> t') = constraintPreds :=> mt'

  let sc              = quantify (ftv qt') qt'

  (s, ps, t, e) <- infer env m
  (qs :=> t')   <- instantiate sc
  s'            <- (`compose` s) <$> unify t' t

  let qs' = apply s' qs
      t'' = apply s' t'
      fs  = ftv (apply s' env)
      gs  = ftv t'' \\ fs
      sc' = quantify (ftv t'') (qs' :=> t'')
  ps'         <- filterM ((not <$>) . entail env qs') (apply s' ps)

  (ds, rs)    <- split env fs gs ps'

  withParents <- getAllParentPreds env qs'

  if sc /= sc'
    then throwError
      $ InferError (SignatureTooGeneral sc sc') (Context (envCurrentPath env) (Can.getArea m) (envBacktrace env))
    else if not (null rs)
      then throwError $ InferError ContextTooWeak (Context (envCurrentPath env) (Can.getArea m) (envBacktrace env))
      else do
        let e' = updateType e t''
        e''  <- insertClassPlaceholders env (Slv.Solved t area $ Slv.Assignment mn e') (apply s' withParents)
        e''' <- updatePlaceholders env s' e''

        return (mn, e''', sc)
