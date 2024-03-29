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
import           Run.Target
import           Error.Error
import           Error.Context
import           Control.Monad
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.List
import           Utils.List
import qualified Rock
import qualified Driver.Query                  as Query
import           Explain.Location (emptyArea)
import           Error.Warning
import Data.Hashable (hash)


canonicalizeInterfaces :: Env -> [Src.Interface] -> CanonicalM (Env, [Can.Interface])
canonicalizeInterfaces env = foldM
  (\(env, interfaces) interface -> do
    (env'', interface') <- canonicalizeInterface env interface
    return (env'', interfaces ++ [interface'])
  )
  (env, [])


getAllInterfaceMethodNames :: Env -> [String]
getAllInterfaceMethodNames env =
  concatMap (\case { Interface _ _ ms -> ms}) (M.elems $ envInterfaces env)


canonicalizeInterface :: Env -> Src.Interface -> CanonicalM (Env, Can.Interface)
canonicalizeInterface env (Src.Source area _ interface) = case interface of
  Src.Interface constraints n vars ms -> do
    let vars' = hash <$> vars
    let allKnownMethodNames = getAllInterfaceMethodNames env
    when (any (`elem` allKnownMethodNames) (M.keys ms)) $
      throwError $ CompilationError MethodNameAlreadyDefined (Context (envCurrentPath env) area)

    ts <- mapM (typingToType env AnyKind) ms

    let ts' = addConstraints n vars' <$> ts
    let tvs = removeDuplicates $ catMaybes $ concat $ mapM searchVarInType vars' <$> M.elems ts

    let supers = mapMaybe
          (\(Src.Source _ _ (Src.TRComp interface' [Src.Source _ _ (Src.TRSingle v)])) ->
            (\tv -> IsIn interface' [tv] Nothing) <$> findTypeVar tvs (hash v)
          )
          constraints

    let psTypes = concat $ (\(IsIn _ ts _) -> ts) <$> supers
    let subst   = foldr (\t s -> s `compose` buildVarSubsts t) mempty psTypes

    let scs =
          (\(ps :=> t) -> quantify (collectVars (apply subst t)) (apply subst (ps <> supers) :=> apply subst t)) <$> ts'

    let tvs' = (\(TVar tv) -> tv) <$> tvs

    env' <-
      if null tvs' then
        throwError $ CompilationError FatalError (Context (envCurrentPath env) area)
      else
        return $ env { envInterfaces = M.insert n (Interface tvs' supers (M.keys ms)) (envInterfaces env) }

    canMs <- mapM canonicalizeTyping ms
    return (env', Can.Canonical area $ Can.Interface n supers tvs' scs canMs)


findTypeVar :: [Type] -> Int -> Maybe Type
findTypeVar []       _ = Nothing
findTypeVar (t : ts) n = case t of
  TVar (TV n' _) -> if n == n' then Just t else findTypeVar ts n
  _              -> findTypeVar ts n


addConstraints :: Id -> [Int] -> Type -> Qual Type
addConstraints n tvs t =
  let tvs'  = (`searchVarInType` t) <$> tvs
      tvs'' = catMaybes tvs'
      ps    = [IsIn n tvs'' Nothing]
  in  ps :=> t


canonicalizeInstances :: Env -> Target -> [Src.Instance] -> CanonicalM [Can.Instance]
canonicalizeInstances _   _      []       = return []
canonicalizeInstances env target (i : is) = do
  next    <- canonicalizeInstances env target is
  current <- canonicalizeInstance env target i
  return $ current : next



lookupInterface :: Env -> String -> CanonicalM Interface
lookupInterface env name = case M.lookup name (envInterfaces env) of
  Just found ->
    return found

  Nothing -> do
    maybeInterface <- Rock.fetch $ Query.CanonicalizedInterface (envCurrentPath env) name
    case maybeInterface of
      Just found ->
        return found

      Nothing ->
        throwError $ CompilationError (InterfaceNotExisting name) (Context (envCurrentPath env) emptyArea)


lookupInterface' :: Rock.MonadFetch Query.Query m => Env -> String -> m (Maybe Interface)
lookupInterface' env name = case M.lookup name (envInterfaces env) of
  Just found ->
    return $ Just found

  Nothing -> do
    Rock.fetch $ Query.CanonicalizedInterface (envCurrentPath env) name



canonicalizeInstance :: Env -> Target -> Src.Instance -> CanonicalM Can.Instance
canonicalizeInstance env target (Src.Source area _ inst) = case inst of
  Src.Instance constraints n typings methods -> do
    (Interface tvs _ methodNames) <- lookupInterface env n
    let missingMethods = methodNames \\ M.keys methods
        missingMethods' =
          if n == "Eq" then
            filter (/= "==") missingMethods
          else
            missingMethods

    when (missingMethods' /= []) $ do
      pushWarning $ CompilationWarning (MissingMethods missingMethods) (Context (envCurrentPath env) area)

    ts <- zipWithM (typingToType env) (KindRequired . kind <$> tvs) typings
    let subst = foldr (\t s -> s `compose` buildVarSubsts t) mempty ts

    ps <-
      apply subst
        <$> mapM
              (\(Src.Source _ _ (Src.TRComp interface' args)) -> do
                (Interface tvs _ _) <- lookupInterface env interface'
                vars <- mapM
                    (\case
                      (Src.Source _ _ (Src.TRSingle v), TV _ k) -> return $ TVar $ TV (hash v) k
                      (typing                         , TV _ k) -> typingToType env (KindRequired k) typing
                    )
                    (zip args tvs)
                return $ IsIn interface' vars Nothing
              )
              constraints

    let psTypes = concat $ (\(IsIn _ ts _) -> ts) <$> ps
    let subst' = foldr (\t s -> s `compose` buildVarSubsts t) mempty psTypes

    let ps'     = apply subst' ps
    let p       = IsIn n (apply subst' ts) Nothing
    methods' <- mapM (canonicalize env target) methods
    let methods'' =
          if n == "Eq" then
            M.mapKeys
              (\methodName ->
                  if methodName == "__EQ__" then
                    "=="
                  else
                    methodName
              )
              methods'
          else
            methods'

    return $ Can.Canonical area $ Can.Instance n ps' p methods''
