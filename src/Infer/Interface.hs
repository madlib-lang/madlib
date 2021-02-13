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
import           Explain.Reason
import           Explain.Location
import qualified Data.Map                      as M
import           Data.List
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe


-- defined :: Maybe a -> Bool
-- defined (Just x) = True
-- defined Nothing  = False

-- overlap       :: Env -> Pred -> Pred -> Bool
-- overlap env p q = defined (unify env p q)

addInterface :: Env -> Id -> [TVar] -> [Pred] -> Infer Env
addInterface env id tvs ps = case M.lookup id (envinterfaces env) of
  Just x  -> throwError $ InferError (InterfaceAlreadyDefined id) NoReason
  Nothing -> return env
    { envinterfaces = M.insert id (Interface tvs ps []) (envinterfaces env)
    }


verifyInstancePredicates :: Env -> Pred -> Pred -> Infer Bool
verifyInstancePredicates env p' p@(IsIn cls ts) = do
  case M.lookup cls (envinterfaces env) of
    Nothing -> throwError $ InferError (InterfaceNotExisting cls) NoReason

    Just (Interface tvs ps' is) -> catchError
      (unify (TVar <$> tvs) ts >> return True)
      (\_ -> throwError $ InferError
        (InstancePredicateError p' p (IsIn cls (TVar <$> tvs)))
        NoReason
      )

-- Add test for overlap that should also test for kind of the given type !!
addInstance :: Env -> [Pred] -> Pred -> Infer Env
addInstance env ps p@(IsIn cls ts) = case M.lookup cls (envinterfaces env) of
  Nothing -> throwError $ InferError (InterfaceNotExisting cls) NoReason

  Just (Interface tvs ps' is) -> do
    mapM_ (verifyInstancePredicates env p) ps

    let ts'    = TVar <$> tvs
    let zipped = zip ts' ts
    s <- match ts' ts
    catchError
      (mapM_ (isInstanceDefined env s) ps')
      (\e@(InferError (NoInstanceFound _ ts) _) ->
        when (all isConcrete ts) (throwError e)
      )
    return env
      { envinterfaces = M.insert
                          cls
                          (Interface tvs ps' (Instance (ps :=> p) : is))
                          (envinterfaces env)
      }


findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

isInstanceDefined :: Env -> Substitution -> Pred -> Infer Bool
isInstanceDefined env subst (IsIn id ts) = do
  let is = insts env id
  found <- findM
    (\(Instance (_ :=> (IsIn _ ts'))) -> catchError
      (match ts' (apply subst ts) >>= \_ -> return $ Just True)
      (const $ return Nothing)
    )
    is
  case found of
    Just _ -> return True
    Nothing ->
      throwError $ InferError (NoInstanceFound id (apply subst ts)) NoReason



resolveInstance :: Env -> Can.Instance -> Infer Slv.Instance
resolveInstance env (Can.Instance name constraintPreds pred methods) = do
  let instanceTypes = predTypes pred
  let subst = foldl (\s t -> s `compose` buildVarSubsts t) mempty instanceTypes
  (Interface _ ps _) <- lookupInterface env name
  let instancePreds = apply subst $ [IsIn name instanceTypes] <> ps
  let psTypes       = concat $ predTypes <$> constraintPreds
  let subst' = foldl (\s t -> s `compose` buildVarSubsts t) mempty psTypes
  inferredMethods <- mapM
    (inferMethod env (apply subst' instancePreds) (apply subst' constraintPreds)
    )
    (M.toList methods)
  let dict' = M.fromList $ (\(a, b, c) -> (a, (b, c))) <$> inferredMethods
  return $ Slv.Instance name constraintPreds pred dict'


inferMethod
  :: Env
  -> [Pred]
  -> [Pred]
  -> (Can.Name, Can.Exp)
  -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod env instancePreds constraintPreds (mn, m) = upgradeReason
  env
  (Can.getArea m)
  (inferMethod' env instancePreds constraintPreds (mn, m))


inferMethod'
  :: Env
  -> [Pred]
  -> [Pred]
  -> (Can.Name, Can.Exp)
  -> Infer (Slv.Name, Slv.Exp, Scheme)
inferMethod' env instancePreds constraintPreds (mn, m) = do
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
    then throwError $ InferError
      (SignatureTooGeneral sc sc')
      (SimpleReason (envcurrentpath env) (Can.getArea m))
    else if not (null rs)
      then throwError $ InferError
        ContextTooWeak
        (SimpleReason (envcurrentpath env) (Can.getArea m))
      else do
        let e' = updateType e t''
        tmp <- insertClassPlaceholders
          env
          (Slv.Solved t emptyArea $ Slv.Assignment mn e')
          (apply s' withParents)
        let (Slv.Solved _ _ (Slv.Assignment _ e'')) = tmp
        e''' <- updatePlaceholders env s' e''

        return (mn, e''', sc)

