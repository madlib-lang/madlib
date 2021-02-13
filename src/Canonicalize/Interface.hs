{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Canonicalize.Interface where

import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Canonicalize.Env
import           Canonicalize.CanonicalM
import           Canonicalize.Typing
import           Canonicalize.Canonicalize
import           Infer.Type
import           Infer.Substitute
import           Infer.Scheme
import           Target
import           Error.Error
import           Explain.Reason
import           Control.Monad
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Data.Maybe



canonicalizeInterfaces
  :: Env -> [Src.Interface] -> CanonicalM (Env, [Can.Interface])
canonicalizeInterfaces env = foldM
  (\(env, interfaces) interface -> do
    (env'', interface') <- canonicalizeInterface env interface
    return (env'', interfaces ++ [interface'])
  )
  (env, [])


canonicalizeInterface :: Env -> Src.Interface -> CanonicalM (Env, Can.Interface)
canonicalizeInterface env interface = case interface of
  Src.Interface constraints n vars ms -> do
    ts <- mapM (typingToType env) ms

    let ts' = addConstraints n vars <$> ts
    let tvs =
          rmdups $ catMaybes $ concat $ mapM searchVarInType vars <$> M.elems ts

    let
      supers = mapMaybe
        (\(Src.Source _ _ (Src.TRComp interface' [Src.Source _ _ (Src.TRSingle v)])) ->
          (\tv -> IsIn interface' [tv]) <$> findTypeVar tvs v
        )
        constraints

    let psTypes = concat $ (\(IsIn _ ts) -> ts) <$> supers
    let subst   = foldl (\s t -> s `compose` buildVarSubsts t) mempty psTypes

    let scs =
          (\(ps :=> t) -> quantify
              (collectVars (apply subst t))
              (apply subst (ps <> supers) :=> apply subst t)
            )
            <$> ts'

    let tvs' = (\(TVar tv) -> tv) <$> tvs

    env' <- if null tvs'
      then throwError $ InferError FatalError NoReason
      else return $ env
        { envInterfaces = M.insert n (Interface tvs' supers) (envInterfaces env)
        }

    return (env', Can.Interface n supers tvs' scs)


rmdups :: (Eq a) => [a] -> [a]
rmdups []       = []
rmdups [x     ] = [x]
rmdups (x : xs) = x : [ k | k <- rmdups xs, k /= x ]


findTypeVar :: [Type] -> String -> Maybe Type
findTypeVar []       n = Nothing
findTypeVar (t : ts) n = case t of
  TVar (TV n' _) -> if n == n' then Just t else findTypeVar ts n
  _              -> findTypeVar ts n


addConstraints :: Id -> [Id] -> Type -> Qual Type
addConstraints n tvs t =
  let tvs'  = (`searchVarInType` t) <$> tvs
      tvs'' = catMaybes tvs'
      ps    = [IsIn n tvs'']
      vars  = collectVars t
  in  ps :=> t


canonicalizeInstances
  :: Env -> Target -> [Src.Instance] -> CanonicalM [Can.Instance]
canonicalizeInstances env target = mapM (canonicalizeInstance env target)


canonicalizeInstance :: Env -> Target -> Src.Instance -> CanonicalM Can.Instance
canonicalizeInstance env target inst = case inst of
  Src.Instance constraints n typing methods -> do
    ts <- mapM (typingToType env) typing

    let subst = foldl (\s t -> s `compose` buildVarSubsts t) mempty ts

    ps <-
      apply subst
        <$> mapM
              (\(Src.Source _ _ (Src.TRComp interface' args)) ->
                case M.lookup interface' (envInterfaces env) of
                  Just (Interface tvs _) -> do
                    vars <- mapM
                      (\case
                        (Src.Source _ _ (Src.TRSingle v), TV _ k) ->
                          return $ TVar $ TV v k
                        (typing, _) -> typingToType env typing
                      )
                      (zip args tvs)
                    return $ IsIn interface' vars

                  Nothing -> throwError
                    $ InferError (InterfaceNotExisting interface') NoReason
              )
              constraints

    let psTypes = concat $ (\(IsIn _ ts) -> ts) <$> ps
    let subst' = foldl (\s t -> s `compose` buildVarSubsts t) mempty psTypes

    let ps'     = apply subst' ps
    let p       = IsIn n (apply subst' ts)
    methods' <- mapM (canonicalize env target) methods

    return $ Can.Instance n ps' p methods'
