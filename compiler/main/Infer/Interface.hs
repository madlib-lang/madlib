{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Interface where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Env
import           Infer.EnvUtils
import           Infer.Infer
import           Infer.Type
import           Infer.Exp
import           Infer.Generalize
import           Infer.Instantiate
import           Infer.Pred
import           Infer.Substitute
import           Infer.Scheme
import           Infer.Unify
import           Infer.Placeholder
import           Error.Error
import           Error.Context
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import Run.Options
import Explain.Location


-- defined :: Maybe a -> Bool
-- defined (Just x) = True
-- defined Nothing  = False

-- overlap       :: Env -> Pred -> Pred -> Bool
-- overlap env p q = defined (unify env p q)

addInterface :: Env -> Area -> Id -> [TVar] -> [Pred] -> Infer Env
addInterface env area id tvs ps = case M.lookup id (envInterfaces env) of
  Just _  ->
    throwError $ CompilationError (InterfaceAlreadyDefined id) (Context (envCurrentPath env) area)

  Nothing ->
    return env { envInterfaces = M.insert id (Interface tvs ps []) (envInterfaces env) }


verifyInstancePredicates :: Env -> Pred -> Pred -> Infer Bool
verifyInstancePredicates env p' p@(IsIn cls ts _) = do
  (Interface tvs _ _) <- lookupInterface env cls
  let tvs' = (\(TV n k) -> TV (n + 100000) k) <$> tvs
  catchError
    (unify (TVar <$> tvs') ts >> return True)
    (\_ -> throwWithContext (InstancePredicateError p' p (IsIn cls (TVar <$> tvs) Nothing)))

-- Add test for overlap that should also test for kind of the given type !!
addInstance :: Env -> [Pred] -> Pred -> Bool -> Infer Env
addInstance env ps p@(IsIn cls ts _) isDerived = do
  (Interface tvs ps' is) <- lookupInterface env cls

  -- Context variables must be determined by the instance head.  Without
  -- coverage an instance can manufacture an unconstrained dictionary goal
  -- that neither inference nor code generation can solve coherently.
  let headVars = ftv ts
      typeSize t = case t of
        TVar{}                    -> 1
        TCon{}                    -> 1
        TGen{}                    -> 1
        TApp l r                  -> 1 + typeSize l + typeSize r
        TRowEmpty                 -> 1
        TRowExtend _ field tail   -> 1 + typeSize field + typeSize tail
        TRecordRow row extra      -> 1 + typeSize row
                                      + sum (typeSize <$> M.elems extra)
        TAlias _ _ _ t'           -> 1 + typeSize t'
      headSize = sum (typeSize <$> ts)
  unless isDerived $ do
    -- Compiler-derived dictionaries are generated structurally from ADT
    -- fields.  Their context can be syntactically larger than the instance
    -- head (for example Eq (m #[a, w]) for a transformer wrapper), even
    -- though recursive derivation follows a finite field tree.  Apply the
    -- user-facing coverage/Paterson checks only to source declarations.
    mapM_ (\ctx -> do
      unless (ftv ctx `S.isSubsetOf` headVars) $
        throwWithContext (InvalidInstanceContext ctx)
      unless (sum (typeSize <$> predTypes ctx) < headSize) $
        throwWithContext (InvalidInstanceContext ctx)
      ) ps

    -- Reject self-referential instances: instance C a => C a
    -- Use asymmetric `match ts ts'` (head types onto constraint types).
    -- quickMatch is symmetric so it gives false positives for e.g. Show a => Show (List a).
    -- `match ts ts'` succeeds only when head can be specialized to the constraint, indicating a loop.
    selfRef <- filterM
      (\(IsIn cls' ts' _) ->
        if cls' /= cls
          then return False
          else catchError (match ts ts' >> return True) (\_ -> return False)
      )
      ps
    case selfRef of
      (_ : _) ->
        throwWithContext (SelfReferentialInstance p)
      [] -> return ()

    -- Reject overlap with fully-resolved (non-stub) instances from other modules.
    -- Stubs from initialEnv have empty method maps; real imported instances have non-empty ones.
    overlapping <- findM
      (\(Instance (_ :=> h) methods) ->
        if M.null methods then return Nothing else do
          overlaps <- headsOverlap h p
          return $ if overlaps then Just h else Nothing
      )
      is
    case overlapping of
      Just h  -> throwWithContext (OverlappingInstances p h)
      Nothing -> return ()

  mapM_ (verifyInstancePredicates env p) ps

  let ts' = TVar <$> tvs
  s <- match ts' ts
  -- Imported superclass interfaces are resolved lazily while the prelude is
  -- being bootstrapped.  Retain the existing deferred head check here; the
  -- terminating entailment solver validates evidence at use sites.
  catchError (mapM_ (isInstanceDefined env s) ps')
              (\e@(CompilationError (NoInstanceFound _ missingTs _) _) -> when (all isConcrete missingTs) (throwError e))
  return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance (ps :=> p) mempty : is)) (envInterfaces env)
              }

-- | Check pairwise overlaps among the user-written (non-derived) instances within a single module.
-- This is called from buildInitialEnv after all instances have been added; it detects duplicates
-- and overlaps that `addInstance` can't see because same-module instances start with empty methods.
checkIntraModuleOverlaps :: Env -> [Can.Instance] -> Infer ()
checkIntraModuleOverlaps env instances = do
  let userPreds = [(p, inst) | inst@(Can.Canonical _ (Can.Instance _ _ p _ False)) <- instances]
  foldM_
    (\seen (p, inst) -> do
      overlap <- findM (\p' -> do
        overlaps <- headsOverlap p' p
        return $ if overlaps then Just p' else Nothing
        ) seen
      case overlap of
        Just p' -> throwError $ CompilationError (OverlappingInstances p p') (Context (envCurrentPath env) (Can.getArea inst))
        Nothing -> return ()
      return (p : seen)
    )
    []
    userPreds


addInstanceMethod :: Env -> [Pred] -> Pred -> (String, Scheme) -> Infer Env
addInstanceMethod env _ p@(IsIn cls _ _) (methodName, methodScheme) = do
  (Interface tvs ps' is) <- lookupInterface env cls

  maybeInstance <- findInst env p
  case maybeInstance of
    Just (Instance qp methods) -> do
      let methods'    = M.insert methodName methodScheme methods
      return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance qp methods' : is)) (envInterfaces env) }
    Nothing ->
      return env

setInstanceMethods :: Env -> Pred -> Vars -> Infer Env
setInstanceMethods env p@(IsIn cls _ _) methods = do
  (Interface tvs ps' is) <- lookupInterface env cls

  maybeInstance <- findInst env p
  case maybeInstance of
    Just (Instance qp _) -> do
      let is' = Instance qp methods : filter (\(Instance qp' _) -> qp' /= qp) is
      return env { envInterfaces = M.insert cls (Interface tvs ps' is') (envInterfaces env) }

    _ ->
      return env


findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)


-- | Overlap is unifiability of two independently freshened instance heads.
-- `quickMatch` is a useful optimisation at most; it is not a decision
-- procedure because it forgets repeated-variable equalities.
headsOverlap :: Pred -> Pred -> Infer Bool
headsOverlap p q = do
  p' <- freshenPred p
  q' <- freshenPred q
  catchError (unify p' q' >> return True) (const $ return False)


freshenPred :: Pred -> Infer Pred
freshenPred p = do
  pairs <- mapM (\tv@(TV _ k) -> do
    fresh <- newTVar k
    return (tv, fresh)
    ) (S.toList (ftv p))
  return $ apply (M.fromList pairs) p

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
    Nothing -> throwWithContext (NoInstanceFound id (apply subst ts) [])


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
resolveInstance options env inst@(Can.Canonical area (Can.Instance name constraintPreds pred methods _)) = do
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
  -- `instancePreds` carries the instance head *and* its specialized
  -- superclasses, which are evidence for the method body.  A method scheme
  -- must be specialized by its owning class predicate only; pairing its
  -- predicate list positionally with all available evidence used to silently
  -- truncate with `zip`, while the exact-arity fix correctly exposed it as an
  -- internal error for every `Comparable` method.
  instanceHead <- case instancePreds of
    (p : _) -> return p
    []      -> throwWithContext FatalError
  methodHead <- case find ((== predClass instanceHead) . predClass) mps of
    Just p  -> return p
    Nothing -> throwWithContext FatalError
  s1             <- specialMatch methodHead instanceHead
  let (_ :=> mt') = apply s1 qt
  let qt'         = constraintPreds :=> mt'

  let sc          = quantify (ftvList qt') qt'

  (s, (ps, t, e)) <- captureDelta (infer options env { envDeferBodyAmbiguity = True } m)
  (qs :=> t')   <- instantiate sc
  su            <- unify t' t
  let s' = su `compose` s

  let qs' = apply s' qs
      t'' = apply s' t'
      methodGivens = apply s' (instancePreds ++ qs)
      fsSet = ftv (apply s' env)
      fs  = S.toList fsSet
      gs  = filter (not . (`S.member` fsSet)) (ftvList t'')
      sc' = quantify (ftvList t'') (qs' :=> t'')
  ps' <- filterM ((not <$>) . entail env methodGivens) (apply s' ps)

  (ds, rs, _) <- split True env fs gs ps'

  if sc /= sc'
    then throwError $ CompilationError (SignatureTooGeneral sc sc')
                                       (Context (envCurrentPath env) (Can.getArea m))
    else if not (null rs)
      then throwError
        $ CompilationError (ContextTooWeak rs) (Context (envCurrentPath env) (Can.getArea m))
      else do
        let e' = updateQualType e (qs :=> t'')
        let e'' = Slv.Typed (apply s' ds :=> apply s' t) area $ Slv.Assignment mn e'
        e''' <- updateExpTypes options env True s' e''

        return (mn, e''', sc)
