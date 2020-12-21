{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.ADT where

import qualified Data.Map                      as M
import           Control.Monad.Except
import           Data.Char                      ( isLower )

import           AST.Source
import           Infer.Type
import           Infer.Infer
import           Infer.Typing
import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Infer.Instantiate              ( newTVar )
import           Data.Maybe                     ( fromMaybe )
import           Text.Show.Pretty               ( ppShow )
import           Debug.Trace                    ( trace )
import           Data.List                      ( isInfixOf )
import           Infer.Scheme                   ( quantify )
import           Infer.Substitute               ( Substitutable(apply)
                                                , compose
                                                )
import           Utils.Tuple                    ( lst
                                                , mid
                                                , beg
                                                )


buildTypeDecls :: Env -> FilePath -> [TypeDecl] -> Infer TypeDecls
buildTypeDecls priorEnv astPath = buildTypeDecls' priorEnv astPath M.empty


buildTypeDecls' :: Env -> FilePath -> TypeDecls -> [TypeDecl] -> Infer TypeDecls
buildTypeDecls' _        _       _         []         = return M.empty
buildTypeDecls' priorEnv astPath typeDecls [typeDecl] = do
  (k, v) <- buildTypeDecl priorEnv astPath typeDecls typeDecl
  return $ M.singleton k v
buildTypeDecls' priorEnv astPath typeDecls (typeDecl : xs) = do
  a    <- buildTypeDecls' priorEnv astPath typeDecls [typeDecl]
  next <- buildTypeDecls' priorEnv astPath (M.union a typeDecls) xs
  return $ M.union a next


buildTypeDecl
  :: Env -> FilePath -> TypeDecls -> TypeDecl -> Infer (String, Type)
buildTypeDecl _ astPath typeDecls adt@ADT{} =
  case M.lookup (adtname adt) typeDecls of
    Just t  -> throwError $ InferError (ADTAlreadyDefined t) NoReason
    Nothing -> return
      ( adtname adt
      , TCon $ TC (adtname adt) (buildKind (length $ adtparams adt))
      )
buildTypeDecl priorEnv astPath typeDecls alias@Alias{} = do
  let name   = aliasname alias
  let params = (`TV` Star) <$> aliasparams alias
  let typing = aliastype alias
  typingType <- typingToType
    priorEnv { envtypes = M.union (envtypes priorEnv) typeDecls }
    typing
  return (name, TAlias astPath name params typingType)


resolveTypeDecls :: Env -> FilePath -> TypeDecls -> [TypeDecl] -> Infer Env
resolveTypeDecls priorEnv astPath priorTypeDecls =
  foldM (\env td -> resolveTypeDecl env astPath priorTypeDecls td) priorEnv


resolveTypeDecl :: Env -> FilePath -> TypeDecls -> TypeDecl -> Infer Env
resolveTypeDecl priorEnv astPath typeDecls adt@ADT{} = do
  let name   = adtname adt
      ctors  = adtconstructors adt
      params = adtparams adt
  is <- mapM
    (resolveADTConstructorParams priorEnv astPath typeDecls name params)
    ctors
  let s = foldl (\s' -> compose s' . lst) M.empty is

  let rt = foldl TApp
                 (TCon $ TC name (buildKind $ length params))
                 ((\x -> apply s $ TVar (TV x Star)) <$> params)
  let full =
        (\(n, ts, _) ->
            let cf = foldr1 fn $ ts <> [rt]
                sc = quantify (collectVars (apply s cf)) ([] :=> apply s cf)
            in  (n, sc)
          )
          <$> is

  return $ priorEnv { envvars  = M.union (envvars priorEnv) (M.fromList full)
                    , envtypes = M.insert name rt (envtypes priorEnv)
                    }

resolveTypeDecl priorEnv astPath typeDecls alias@Alias{} = do
  let name   = aliasname alias
  let params = (`TV` Star) <$> aliasparams alias
  let typing = aliastype alias
  typingType <- typingToType
    priorEnv { envtypes = M.union (envtypes priorEnv) typeDecls }
    typing

  let t = TAlias astPath name params typingType
  return $ priorEnv { envtypes = M.insert name t (envtypes priorEnv) }


resolveADTConstructorParams
  :: Env
  -> FilePath
  -> TypeDecls
  -> Name
  -> [Name]
  -> Constructor
  -> Infer (Name, [Type], Substitution)
resolveADTConstructorParams priorEnv astPath typeDecls n params (Constructor cname cparams)
  = do
    let gens = zip params (map TGen [0 ..])
    ts <- mapM (argToType priorEnv gens typeDecls n params) cparams
    let s = foldl (\s t -> buildCtorSubst t <> s) M.empty ts

    return (cname, ts, s)

buildCtorSubst :: Type -> Substitution
buildCtorSubst t = case t of
  TVar (TV n _) -> M.singleton (TV n Star) t
  TApp l r      -> buildCtorSubst l <> buildCtorSubst r
  _             -> M.empty


argToType
  :: Env
  -> [(Name, Type)]
  -> TypeDecls
  -> Name
  -> [Name]
  -> Typing
  -> Infer Type
argToType _ gens typeDecls _ params (Meta _ _ (TRSingle n))
  | n == "Number" = return tNumber
  | n == "Boolean" = return tBool
  | n == "String" = return tStr
  | n == "()" = return tUnit
  | isLower (head n) = return $ TVar (TV n Star)
  | otherwise = case M.lookup n typeDecls of
    Just a  -> return a
    Nothing -> throwError $ InferError (UnknownType n) NoReason

argToType priorEnv gens typeDecls name params (Meta _ _ (TRComp tname targs)) =
  case M.lookup tname (envtypes priorEnv <> typeDecls) of
    Just t -> foldM
      (\prev a -> do
        arg <- argToType priorEnv gens typeDecls name params a
        return $ TApp prev arg
      )
      (getConstructorCon t)
      targs
    Nothing -> if isLower (head tname)
      then do
        let t = TVar (TV tname $ buildKind (length targs))
        foldM
          (\prev a -> do
            arg <- argToType priorEnv gens typeDecls name params a
            return $ TApp prev arg
          )
          t
          targs
      else throwError $ InferError (UnknownType tname) NoReason

argToType priorEnv gens typeDecls name params (Meta _ _ (TRArr l r)) = do
  l' <- argToType priorEnv gens typeDecls name params l
  r' <- argToType priorEnv gens typeDecls name params r
  return $ l' `fn` r'

argToType priorEnv gens typeDecls name params (Meta _ _ (TRRecord f)) = do
  f' <- mapM (argToType priorEnv gens typeDecls name params) f
  return $ TRecord f' False

argToType priorEnv gens typeDecls name params (Meta _ _ (TRTuple elems)) = do
  elems' <- mapM (argToType priorEnv gens typeDecls name params) elems
  let tupleT = getTupleCtor (length elems)
  return $ foldl TApp tupleT elems'
