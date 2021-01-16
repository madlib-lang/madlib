{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Env where

import qualified Data.Map                      as M
import           Infer.Type                    as Ty
import           AST.Source                    as Src
import           Control.Monad.Except           ( MonadError(throwError) )
import           Infer.Instantiate
import           Infer.ADT
import           Infer.Infer
import           Explain.Reason
import           Error.Error
import qualified Data.Set                      as S
import           Infer.Substitute               ( Substitutable(ftv, apply)
                                                , compose
                                                , buildVarSubsts
                                                )
import           Infer.Typing                   ( typingToScheme
                                                , typingToType
                                                , qualTypingToQualType
                                                )
import           Data.List                      ( isInfixOf
                                                , find
                                                , union
                                                )
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                , fromMaybe
                                                )
import           Text.Show.Pretty               ( ppShow )
import           Debug.Trace                    ( trace )
import           Infer.Scheme                   ( quantify
                                                , toScheme
                                                )
import           Control.Monad                  ( foldM )
import           Explain.Meta
import           Infer.Pred

lookupVar :: Env -> String -> Infer Scheme
lookupVar env x = case M.lookup x (envvars env <> envmethods env) of
  Just x  -> return x
  Nothing -> throwError $ InferError (UnboundVariable x) NoReason


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envvars = M.insert x s $ envvars env }


safeExtendVars :: Env -> (String, Scheme) -> Infer Env
safeExtendVars env (i, sc) = case M.lookup i (envvars env) of
  Just _  -> throwError $ InferError (NameAlreadyDefined i) NoReason
  Nothing -> return $ extendVars env (i, sc)


lookupInterface :: Env -> Name -> Infer Ty.Interface
lookupInterface env n = case M.lookup n (envinterfaces env) of
  Just i -> return i
  _      -> throwError $ InferError (InterfaceNotExisting n) NoReason


mergeVars :: Env -> Vars -> Env
mergeVars env vs = env { envvars = envvars env <> vs }


initialEnv :: Env
initialEnv = Env
  { envvars        = M.fromList
    [ ("==", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
    , ("&&", Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
    , ("||", Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
    , ("!" , Forall [] $ [] :=> (tBool `fn` tBool))
    , (">" , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
    , ("<" , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
    , (">=", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
    , ("<=", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
    , ("++", Forall [] $ [] :=> (tStr `fn` tStr `fn` tStr))
    , ("+" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
    , ("-" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
    , ("*" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
    , ("/" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
    , ("%" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
    , ( "|>"
      , Forall [Star, Star]
      $   []
      :=> (TGen 0 `fn` (TGen 0 `fn` TGen 1) `fn` TGen 1)
      )
    ]
  , envtypes       = M.fromList
                       [ ("List" , tList)
                       , ("(,)"  , tTuple2)
                       , ("(,,)" , tTuple3)
                       , ("(,,,)", tTuple4)
                       ]
  , envinterfaces  = M.empty
  , envmethods     = M.empty
  , envcurrentpath = ""
  }

solveInterfaces :: Env -> [Src.Interface] -> Infer Env
solveInterfaces = foldM solveInterface


solveInterface :: Env -> Src.Interface -> Infer Env
solveInterface env interface = case interface of
  Src.Interface constraints n vars ms -> do
    ts <- mapM (typingToType env) ms

    let ts' = addConstraints n vars <$> ts
    let tvs =
          rmdups $ catMaybes $ concat $ mapM searchVarInType vars <$> M.elems ts

    let supers = mapMaybe
          (\(Meta _ _ (Src.TRComp interface' [Meta _ _ (Src.TRSingle v)])) ->
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

    env' <- if null tvs
      then throwError $ InferError FatalError NoReason
      else addInterface env n ((\(TVar tv) -> tv) <$> tvs) supers

    return env' { envmethods = envmethods env <> scs }


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


solveInstances :: Env -> [Src.Instance] -> Infer Env
solveInstances = foldM solveInstance


solveInstance :: Env -> Src.Instance -> Infer Env
solveInstance env inst = case inst of
  Src.Instance constraints n typing _ -> do
    ts <- mapM (typingToType env) typing

    let subst = foldl (\s t -> s `compose` buildVarSubsts t) mempty ts

    ps <-
      apply subst
        <$> mapM
              (\(Meta _ _ (Src.TRComp interface' args)) ->
                case M.lookup interface' (envinterfaces env) of
                  Just (Ty.Interface tvs _ _) -> do
                    vars <- mapM
                      (\case
                        (Meta _ _ (Src.TRSingle v), TV _ k) ->
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
    let subst'  = foldl (\s t -> s `compose` buildVarSubsts t) mempty psTypes

    addInstance env (apply subst' ps) $ IsIn n (apply subst' ts)


populateTopLevelTypings :: Env -> [Src.Exp] -> Infer Env
populateTopLevelTypings env []                  = return env
populateTopLevelTypings env ((Meta _ _ e) : es) = do
  nextEnv <- case e of
    Src.TypedExp (Meta _ _ (Src.Assignment name _)) typing -> do
      sc <- typingToScheme env typing
      return $ extendVars env (name, sc)

    Src.TypedExp (Meta _ _ (Src.Export (Meta _ _ (Src.Assignment name _)))) typing
      -> do
        sc <- typingToScheme env typing
        return $ extendVars env (name, sc)

    _ -> return env

  populateTopLevelTypings nextEnv es


buildInitialEnv :: Env -> AST -> Infer Env
buildInitialEnv priorEnv AST { aexps, atypedecls, ainterfaces, ainstances, apath = Just apath }
  = do
    tadts <- buildTypeDecls priorEnv apath atypedecls
    env   <- resolveTypeDecls
      priorEnv { envtypes = M.union (envtypes priorEnv) tadts }
      apath
      tadts
      atypedecls
    env'  <- solveInterfaces env ainterfaces
    env'' <- solveInstances env' ainstances
    let env''' = env'' { envvars        = envvars initialEnv <> envvars env''
                       , envcurrentpath = apath
                       }
    populateTopLevelTypings env''' aexps
