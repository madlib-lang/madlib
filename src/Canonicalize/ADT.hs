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
import           Explain.Reason
import qualified Data.Map                      as M
import           Data.Char
import           Control.Monad.Except



canonicalizeTypeDecls
  :: Env -> FilePath -> [Src.TypeDecl] -> CanonicalM (Env, [Can.TypeDecl])
canonicalizeTypeDecls env _       []         = return (env, [])
canonicalizeTypeDecls env astPath [typeDecl] = do
  (env', tds') <- canonicalizeTypeDecl env astPath typeDecl
  return (env', [tds'])
canonicalizeTypeDecls env astPath (typeDecl : tds) = do
  (env' , tds' ) <- canonicalizeTypeDecls env astPath [typeDecl]
  (env'', tds'') <- canonicalizeTypeDecls env' astPath tds
  return (env'', tds' <> tds'')


canonicalizeTypeDecl
  :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeTypeDecl env astPath typeDecl = case typeDecl of
  adt@Src.ADT{} -> case M.lookup (Src.adtname adt) (envTypeDecls env) of
    Just t  -> throwError $ InferError (ADTAlreadyDefined t) NoReason
    Nothing -> do
      let t =
            TCon $ TC (Src.adtname adt) (buildKind (length $ Src.adtparams adt))
          env' = addADT env (Src.adtname adt) t
      canonicalizeConstructors env astPath adt

  alias@Src.Alias{} -> do
    let name   = Src.aliasname alias
    let params = (`TV` Star) <$> Src.aliasparams alias
    let typing = Src.aliastype alias
    typingType <- typingToType env typing
    let env' = addADT env name (TAlias astPath name params typingType)
    typing' <- canonicalizeTyping typing
    let alias' = Can.Alias { Can.aliasname     = name
                           , Can.aliasparams   = Src.aliasparams alias
                           , Can.aliastype     = typing'
                           , Can.aliasexported = Src.aliasexported alias
                           }
    return (env', alias')




-- From above, run the stuff below for constructors and make it return Env directly.

-- resolveTypeDecls :: Env -> FilePath -> TypeDecls -> [TypeDecl] -> Infer Env
-- resolveTypeDecls priorEnv astPath priorTypeDecls =
--   foldM (\env td -> resolveTypeDecl env astPath priorTypeDecls td) priorEnv


canonicalizeConstructors
  :: Env -> FilePath -> Src.TypeDecl -> CanonicalM (Env, Can.TypeDecl)
canonicalizeConstructors env astPath adt@Src.ADT{} = do
  let name   = Src.adtname adt
      ctors  = Src.adtconstructors adt
      params = Src.adtparams adt
  is <- mapM (resolveADTConstructorParams env astPath name params) ctors

  let s = foldl (\s' -> compose s' . getSubstitution) mempty is
  let rt = foldl TApp
                 (TCon $ TC name (buildKind $ length params))
                 ((\x -> apply s $ TVar (TV x Star)) <$> params)
  ctors' <- mapM
    (\(n, ts, _, Src.Constructor name typings) -> do
      let cf = foldr1 fn $ ts <> [rt]
          sc = quantify (collectVars (apply s cf)) ([] :=> apply s cf)
      typings' <- mapM canonicalizeTyping typings
      return $ Can.Constructor name typings' sc
    )
    is

  let adt' = Can.ADT { Can.adtname         = name
                     , Can.adtparams       = params
                     , Can.adtconstructors = ctors'
                     , Can.adtType         = rt
                     , Can.adtexported     = Src.adtexported adt
                     }
  return (addADT env name rt, adt')


getConstructor
  :: (Src.Name, [Type], Substitution, Src.Constructor) -> Src.Constructor
getConstructor (_, _, _, ctor) = ctor

getSubstitution
  :: (Src.Name, [Type], Substitution, Src.Constructor) -> Substitution
getSubstitution (_, _, subst, _) = subst


resolveADTConstructorParams
  :: Env
  -> FilePath
  -> Src.Name
  -> [Src.Name]
  -> Src.Constructor
  -> CanonicalM (Src.Name, [Type], Substitution, Src.Constructor)
resolveADTConstructorParams env astPath n params c@(Src.Constructor cname cparams)
  = do
    let gens = zip params (map TGen [0 ..])
    ts <- mapM (argToType env gens n params) cparams
    let s = foldl (\s t -> buildCtorSubst t <> s) M.empty ts

    return (cname, ts, s, c)

buildCtorSubst :: Type -> Substitution
buildCtorSubst t = case t of
  TVar (TV n _) -> M.singleton (TV n Star) t
  TApp l r      -> buildCtorSubst l <> buildCtorSubst r
  _             -> M.empty


argToType
  :: Env
  -> [(Src.Name, Type)]
  -> Src.Name
  -> [Src.Name]
  -> Src.Typing
  -> CanonicalM Type
argToType env gens _ params (Src.Source _ _ (Src.TRSingle n))
  | n == "Number" = return tNumber
  | n == "Boolean" = return tBool
  | n == "String" = return tStr
  | n == "()" = return tUnit
  | isLower (head n) = return $ TVar (TV n Star)
  | otherwise = case M.lookup n (envTypeDecls env) of
    Just a  -> return a
    Nothing -> throwError $ InferError (UnknownType n) NoReason

argToType env gens name params (Src.Source _ _ (Src.TRComp tname targs)) =
  case M.lookup tname (envTypeDecls env) of
    Just t -> foldM
      (\prev a -> do
        arg <- argToType env gens name params a
        return $ TApp prev arg
      )
      (getConstructorCon t)
      targs
    Nothing -> if isLower (head tname)
      then do
        let t = TVar (TV tname $ buildKind (length targs))
        foldM
          (\prev a -> do
            arg <- argToType env gens name params a
            return $ TApp prev arg
          )
          t
          targs
      else throwError $ InferError (UnknownType tname) NoReason

argToType env gens name params (Src.Source _ _ (Src.TRArr l r)) = do
  l' <- argToType env gens name params l
  r' <- argToType env gens name params r
  return $ l' `fn` r'

argToType env gens name params (Src.Source _ _ (Src.TRRecord f)) = do
  f' <- mapM (argToType env gens name params) f
  return $ TRecord f' False

argToType env gens name params (Src.Source _ _ (Src.TRTuple elems)) = do
  elems' <- mapM (argToType env gens name params) elems
  let tupleT = getTupleCtor (length elems)
  return $ foldl TApp tupleT elems'
