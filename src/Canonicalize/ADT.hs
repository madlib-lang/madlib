{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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



canonicalizeTypeDecls :: Env -> FilePath -> [Src.TypeDecl] -> CanonicalM (Env, [Can.TypeDecl])
canonicalizeTypeDecls env _       []         = return (env, [])
canonicalizeTypeDecls env astPath [typeDecl] = do
  (env', tds') <- canonicalizeTypeDecl env astPath typeDecl
  return (env', [tds'])
canonicalizeTypeDecls env astPath (typeDecl : tds) = do
  (env' , tds' ) <- canonicalizeTypeDecls env astPath [typeDecl]
  (env'', tds'') <- canonicalizeTypeDecls env' astPath tds
  return (env'', tds' <> tds'')


canonicalizeTypeDecl :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeTypeDecl env astPath td@(Src.Source _ area typeDecl) = case typeDecl of
  adt@Src.ADT{} -> case M.lookup (Src.adtname adt) (envTypeDecls env) of
    Just t  -> throwError $ CompilationError (ADTAlreadyDefined t) (Context astPath area [])
    Nothing -> do
      let t    = TCon (TC (Src.adtname adt) (buildKind (length $ Src.adtparams adt))) astPath
          vars = (\n -> TVar (TV n Star)) <$> Src.adtparams adt
          t'   = foldl1 TApp (t : vars)
          env' = addADT env (Src.adtname adt) t'
      canonicalizeConstructors env' astPath td

  alias@Src.Alias{} -> do
    let name   = Src.aliasname alias
    let params = (`TV` Star) <$> Src.aliasparams alias
    let typing = Src.aliastype alias
    typingType <- typingToType env typing
    let env' = addADT env name (TAlias astPath name params typingType)
    typing' <- canonicalizeTyping typing
    let alias' = Can.Canonical
          area
          Can.Alias { Can.aliasname     = name
                    , Can.aliasparams   = Src.aliasparams alias
                    , Can.aliastype     = typing'
                    , Can.aliasexported = Src.aliasexported alias
                    }
    return (env', alias')


canonicalizeConstructors :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeConstructors env astPath (Src.Source _ area adt@Src.ADT{}) = do
  let name   = Src.adtname adt
      ctors  = Src.adtconstructors adt
      params = Src.adtparams adt
  is <- mapM (resolveADTConstructorParams env astPath name params) ctors

  let s = foldl' (\s' -> compose s' . getSubstitution) mempty is
  let rt = foldl' TApp
                  (TCon (TC name (buildKind $ length params)) astPath)
                  ((\x -> apply s $ TVar (TV x Star)) <$> params)
  ctors' <- mapM
    (\(n, ts, _, Src.Source _ area (Src.Constructor name typings)) -> do
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
resolveADTConstructorParams env astPath n params c@(Src.Source _ area (Src.Constructor cname cparams)) = do
  ts <- mapM (typingToType env) cparams
  let s = foldl' (\s t -> buildCtorSubst t <> s) M.empty ts

  return (cname, ts, s, c)

buildCtorSubst :: Type -> Substitution
buildCtorSubst t = case t of
  TVar (TV n _) -> M.singleton (TV n Star) t
  TApp l r      -> buildCtorSubst l <> buildCtorSubst r
  _             -> M.empty
