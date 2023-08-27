{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Canonicalize.ADT where

import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Canonicalize.Env
import           Canonicalize.EnvUtils
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
import Data.Hashable (hash)


canonicalizeTypeDecls :: Env -> FilePath -> [Src.TypeDecl] -> CanonicalM (Env, [Can.TypeDecl])
canonicalizeTypeDecls env astPath tds = do
  env' <- foldM (addTypeToEnv astPath) env tds
  let tds' =
        sortBy
          (\a b -> case (a, b) of
            (Src.Source _ _ Src.ADT{}, Src.Source _ _ Src.Alias{}) ->
              GT

            (Src.Source _ _ Src.Alias{}, Src.Source _ _ Src.ADT{}) ->
              LT

            _ ->
              EQ
          )
          tds

  (env'', tds'', toRetry) <- canonicalizeTypeDecls' True env' astPath tds'
  if null toRetry then
    return (env'', tds'')
  else do
    (env''', tds''', _) <- canonicalizeTypeDecls' False env'' astPath toRetry
    return (env''', tds'' <> tds''')


-- Last value in the tuple is the failed types that should be retried, mainly for forward use of type aliases
-- if after a first pass that value isn't empty, we just retry all failed ones
canonicalizeTypeDecls' :: Bool -> Env -> FilePath -> [Src.TypeDecl] -> CanonicalM (Env, [Can.TypeDecl], [Src.TypeDecl])
canonicalizeTypeDecls' _ env _       []         = return (env, [], [])
canonicalizeTypeDecls' firstPass env astPath [typeDecl] = do
  if firstPass then
    catchError
      (do
        (env', tds') <- canonicalizeTypeDecl env astPath typeDecl
        return (env', [tds'], [])
      )
      (\_ ->
        return (env, [], [typeDecl])
      )
  else do
    (env', tds') <- canonicalizeTypeDecl env astPath typeDecl
    return (env', [tds'], [])
canonicalizeTypeDecls' firstPass env astPath (typeDecl : tds) = do
  (env' , tds', toRetry') <- canonicalizeTypeDecls' firstPass env astPath [typeDecl]
  (env'', tds'', toRetry'') <- canonicalizeTypeDecls' firstPass env' astPath tds
  return (env'', tds' <> tds'', toRetry' <> toRetry'')


verifyTypeVars :: Area -> FilePath -> Name -> [Name] -> CanonicalM ()
verifyTypeVars area astPath adtname ps = do
  mapM_
    (\case
      f:rest | isUpper f ->
        throwError $ CompilationError (CapitalizedADTTVar adtname (f:rest)) (Context astPath area)
      _ ->
        return ()
    )
    ps


addTypeToEnv :: FilePath -> Env -> Src.TypeDecl -> CanonicalM Env
addTypeToEnv astPath env (Src.Source area _ typeDecl) = case typeDecl of
  adt@Src.ADT{} ->
    if isLower . head $ Src.adtname adt then
      throwError $ CompilationError (NotCapitalizedADTName $ Src.adtname adt) (Context astPath area)
    else case M.lookup (Src.adtname adt) (envTypeDecls env) of
      Just t  ->
        throwError $ CompilationError (ADTAlreadyDefined t) (Context astPath area)

      Nothing -> do
        verifyTypeVars area astPath (Src.adtname adt) (Src.adtparams adt)

        let t     = TCon (TC (Src.adtname adt) (buildKind (length $ Src.adtparams adt))) astPath
            vars  = (\n -> TVar (TV (hash n) Star)) <$> Src.adtparams adt
            t'    = foldl1 TApp (t : vars)
            env'  = addADT env (Src.adtname adt) t'
            -- env'' = addConstructorInfos env' (Src.adtname adt) (map (\(Src.Source _ _ (Src.Constructor name params)) -> ConstructorInfo name (length params)) (Src.adtconstructors adt))
        return env'

  Src.Alias{} -> do
    return env


canonicalizeTypeDecl :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeTypeDecl env astPath td@(Src.Source area _ typeDecl) = case typeDecl of
  adt@Src.ADT{} ->
    if isLower . head $ Src.adtname adt then
      throwError $ CompilationError (NotCapitalizedADTName $ Src.adtname adt) (Context astPath area)
    else do
      verifyTypeVars area astPath (Src.adtname adt) (Src.adtparams adt)

      let t     = TCon (TC (Src.adtname adt) (buildKind (length $ Src.adtparams adt))) astPath
          vars  = (\n -> TVar (TV (hash n) Star)) <$> Src.adtparams adt
          t'    = foldl1 TApp (t : vars)
          env'  = addADT env (Src.adtname adt) t'
          env'' = addConstructorInfos env' (Src.adtname adt) (map (\(Src.Source _ _ (Src.Constructor name params)) -> ConstructorInfo name (length params)) (Src.adtconstructors adt))
      canonicalizeConstructors env'' astPath td

  alias@Src.Alias{} -> do
    verifyTypeVars area astPath (Src.aliasname alias) (Src.aliasparams alias)
    let name   = Src.aliasname alias
    let params = ((`TV` Star) . hash) <$> Src.aliasparams alias
    let typing = Src.aliastype alias
    typingType <- typingToType env (KindRequired Star) typing
    when (isRecordType typingType) $ do
      pushRecordToDerive (getTRecordFieldNames typingType)
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
      throwError $ CompilationError (NotCapitalizedAliasName name) (Context astPath area)
    else
      return (env', alias')


canonicalizeConstructors :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeConstructors env astPath (Src.Source area _ adt@Src.ADT{}) = do
  let name   = Src.adtname adt
      ctors  = Src.adtconstructors adt
      params = Src.adtparams adt
      importedNames = map iiName $ filter (not . isTypeImport) (envImportInfo env)
  is <- mapM (resolveADTConstructorParams env astPath name params) ctors

  let s = foldl' (\s' -> compose s' . getSubstitution) mempty is
  let rt = foldl' TApp
                  (TCon (TC name (buildKind $ length params)) astPath)
                  ((\x -> apply s $ TVar (TV (hash x) Star)) <$> params)
  ctors' <- mapM
    (\(_, ts, _, Src.Source area _ (Src.Constructor name typings)) -> do
      when (name `elem` importedNames) $
        throwError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath env) area)

      let cf = foldr1 fn $ ts <> [rt]
          sc = quantify (collectVars (apply s cf)) ([] :=> apply s cf)
      typings' <- mapM canonicalizeTyping typings
      return $ Can.Canonical area $ Can.Constructor name typings' sc cf
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

  pushTypeDeclToDerive env adt'

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
resolveADTConstructorParams env astPath _ params c@(Src.Source area _ (Src.Constructor cname cparams)) = do
  let params' = hash <$> params
  ts <- mapM (typingToType env (KindRequired Star)) cparams

  forM_ ts $ \t -> do
    let varNames = map (\(TV n _) -> n) (ftv t)
    forM_ varNames $ \n ->
      if n `elem` params' then
        return ()
      else
        throwError (CompilationError (UnboundVariable (show n)) (Context astPath area))

  let s = foldr (\t s -> buildCtorSubst t <> s) M.empty ts

  if isLower . head $ cname then
    throwError $ CompilationError (NotCapitalizedConstructorName cname) (Context astPath area)
  else
    return (cname, ts, s, c)

buildCtorSubst :: Type -> Substitution
buildCtorSubst t = case t of
  TVar (TV n _) -> M.singleton (TV n Star) t
  TApp l r      -> buildCtorSubst l <> buildCtorSubst r
  _             -> M.empty
