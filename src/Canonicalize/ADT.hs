{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Canonicalize.ADT where

import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Canonicalize.Env
import           Canonicalize.CanonicalM
import           Canonicalize.Typing
import           Infer.Substitute
import           Infer.Scheme
import           Infer.Type
import           Error.Error
import           Error.Context
import qualified Data.Map                      as M
import           Control.Monad.Except
import           Data.List
import           Data.Char
import Debug.Trace
import Text.Show.Pretty
import Explain.Location


canonicalizeTypeDecls :: Env -> FilePath -> [Src.TypeDecl] -> CanonicalM (Env, [Can.TypeDecl])
canonicalizeTypeDecls env _       []         = return (env, [])
canonicalizeTypeDecls env astPath [typeDecl] = do
  (env', tds') <- canonicalizeTypeDecl env astPath typeDecl
  return (env', [tds'])
canonicalizeTypeDecls env astPath (typeDecl : tds) = do
  (env' , tds' ) <- canonicalizeTypeDecls env astPath [typeDecl]
  (env'', tds'') <- canonicalizeTypeDecls env' astPath tds
  return (env'', tds' <> tds'')


verifyTypeVars :: Area -> FilePath -> Name -> [Name] -> CanonicalM ()
verifyTypeVars area astPath adtname ps = do
  mapM_
    (\case
      f:rest | isUpper f ->
        throwError $ CompilationError (CapitalizedADTTVar adtname (f:rest)) (Context astPath area [])
      _ ->
        return ()
    )
    ps

canonicalizeTypeDecl :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeTypeDecl env astPath td@(Src.Source area typeDecl) = case typeDecl of
  adt@Src.ADT{} ->
    if isLower . head $ Src.adtname adt then
      throwError $ CompilationError (NotCapitalizedADTName $ Src.adtname adt) (Context astPath area [])
    else case M.lookup (Src.adtname adt) (envTypeDecls env) of
      Just t  -> throwError $ CompilationError (ADTAlreadyDefined t) (Context astPath area [])
      Nothing -> do
        verifyTypeVars area astPath (Src.adtname adt) (Src.adtparams adt)

        let t    = TCon (TC (Src.adtname adt) (buildKind (length $ Src.adtparams adt))) astPath
            vars = (\n -> TVar (TV n Star)) <$> Src.adtparams adt
            t'   = foldl1 TApp (t : vars)
            env' = addADT env (Src.adtname adt) t'
        canonicalizeConstructors env' astPath td

  alias@Src.Alias{} -> do
    verifyTypeVars area astPath (Src.aliasname alias) (Src.aliasparams alias)
    let name   = Src.aliasname alias
    let params = (`TV` Star) <$> Src.aliasparams alias
    let typing = Src.aliastype alias
    typingType <- typingToType env (KindRequired Star) typing
    let env' = addADT env name (TAlias astPath name params typingType)
    typing' <- canonicalizeTyping typing
    let alias' = Can.Canonical
          area
          Can.Alias { Can.aliasname     = name
                    , Can.aliasparams   = Src.aliasparams alias
                    , Can.aliastype     = typing'
                    , Can.aliasexported = Src.aliasexported alias
                    }
    if isLower . head $ name then
      throwError $ CompilationError (NotCapitalizedAliasName name) (Context astPath area [])
    else
      return (env', alias')


canonicalizeConstructors :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeConstructors env astPath (Src.Source area adt@Src.ADT{}) = do
  let name   = Src.adtname adt
      ctors  = Src.adtconstructors adt
      params = Src.adtparams adt
  is <- mapM (resolveADTConstructorParams env astPath name params) ctors

  let s = foldl' (\s' -> compose s' . getSubstitution) mempty is
  let rt = foldl' TApp
                  (TCon (TC name (buildKind $ length params)) astPath)
                  ((\x -> apply s $ TVar (TV x Star)) <$> params)
  ctors' <- mapM
    (\(n, ts, _, Src.Source area (Src.Constructor name typings)) -> do
      let cf = foldr1 fn $ ts <> [rt]
          sc = quantify (collectVars (apply s cf)) ([] :=> apply s cf)
      typings' <- mapM canonicalizeTyping typings
      return $ Can.Canonical area $ Can.Constructor name typings' sc
    )
    is

  let adt' = Can.Canonical
        area
        Can.ADT { Can.adtname         = name
                , Can.adtparams       = params
                , Can.adtconstructors = ctors'
                , Can.adtType         = rt
                , Can.adtexported     = Src.adtexported adt
                }
  return (addADT env name rt, adt')

canonicalizeConstructors _ _ _ = undefined


getConstructor :: (Src.Name, [Type], Substitution, Src.Constructor) -> Src.Constructor
getConstructor (_, _, _, ctor) = ctor

getSubstitution :: (Src.Name, [Type], Substitution, Src.Constructor) -> Substitution
getSubstitution (_, _, subst, _) = subst


resolveADTConstructorParams
  :: Env
  -> FilePath
  -> Src.Name
  -> [Src.Name]
  -> Src.Constructor
  -> CanonicalM (Src.Name, [Type], Substitution, Src.Constructor)
resolveADTConstructorParams env astPath n params c@(Src.Source area (Src.Constructor cname cparams)) = do
  ts <- mapM (typingToType env (KindRequired Star)) cparams

  mapM_
    (\case
      TVar (TV n _) ->
        if n `elem` params then
          return ()
        else
          throwError (CompilationError (UnboundVariable n) (Context astPath area []))

      _ ->
        return ()
    )
    ts

  let s = foldl' (\s t -> buildCtorSubst t <> s) M.empty ts

  if isLower . head $ cname then
    throwError $ CompilationError (NotCapitalizedConstructorName cname) (Context astPath area [])
  else
    return (cname, ts, s, c)

buildCtorSubst :: Type -> Substitution
buildCtorSubst t = case t of
  TVar (TV n _) -> M.singleton (TV n Star) t
  TApp l r      -> buildCtorSubst l <> buildCtorSubst r
  _             -> M.empty
