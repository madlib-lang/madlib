{-# LANGUAGE NamedFieldPuns #-}
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
      (adtname adt, TComp astPath (adtname adt) (TVar . TV <$> adtparams adt))
buildTypeDecl priorEnv astPath typeDecls alias@Alias{} = do
  let name   = aliasname alias
  let params = TV <$> aliasparams alias
  let typing = aliastype alias
  typingType <- typingToType
    priorEnv { envtypes = M.union (envtypes priorEnv) typeDecls }
    typing
  return (name, TAlias astPath name params typingType)


resolveTypeDecls :: Env -> FilePath -> TypeDecls -> [TypeDecl] -> Infer Vars
resolveTypeDecls priorEnv astPath priorTypeDecls typeDecls =
  mergeVars <$> mapM (resolveTypeDecl priorEnv astPath priorTypeDecls) typeDecls
 where
  mergeVars []   = M.empty
  mergeVars vars = foldr1 M.union vars


resolveTypeDecl :: Env -> FilePath -> TypeDecls -> TypeDecl -> Infer Vars
resolveTypeDecl priorEnv astPath typeDecls adt@ADT{} =
  let name   = adtname adt
      ctors  = adtconstructors adt
      params = adtparams adt
  in  foldr1 M.union
        <$> mapM
              (resolveADTConstructor priorEnv astPath typeDecls name params)
              ctors
resolveTypeDecl _ _ _ Alias{} = return M.empty


-- TODO: Verify that Constructors aren't already in the global space or else throw a name clash error
-- Use lookupADT for that
resolveADTConstructor
  :: Env -> FilePath -> TypeDecls -> Name -> [Name] -> Constructor -> Infer Vars
resolveADTConstructor priorEnv astPath typeDecls n params (Constructor cname cparams)
  = do
    let t = buildADTConstructorReturnType astPath n params
    t' <- mapM (argToType priorEnv typeDecls n params) cparams
    let ctype = foldr1 TArr (t' <> [t])
    return $ M.fromList [(cname, Forall (TV <$> params) ctype)]

buildADTConstructorReturnType :: FilePath -> Name -> [Name] -> Type
buildADTConstructorReturnType astPath tname tparams =
  TComp astPath tname $ TVar . TV <$> tparams


-- TODO: This should probably be merged with typingToType somehow
argToType :: Env -> TypeDecls -> Name -> [Name] -> Typing -> Infer Type
argToType _ typeDecls _ params (Meta _ _ (TRSingle n))
  | n == "Number" = return $ TCon CNum
  | n == "Boolean" = return $ TCon CBool
  | n == "String" = return $ TCon CString
  | isLower (head n) && (n `elem` params) = return $ TVar $ TV n
  | isLower (head n) = newTVar
  | -- A free var that is not in type params
    otherwise = case M.lookup n typeDecls of
    Just a  -> return a
    Nothing -> throwError $ InferError (UnknownType n) NoReason

argToType priorEnv typeDecls name params (Meta _ _ (TRComp tname targs)) =
  case M.lookup tname typeDecls of
  -- TODO: Verify the length of tparams and make sure it matches the one of targs ! otherwise
  -- we have a type application error.
    Just (TComp fp n _) ->
      TComp fp n <$> mapM (argToType priorEnv typeDecls name params) targs
    Nothing -> if tname == "List"
      then
        TComp "Prelude" tname
          <$> mapM (argToType priorEnv typeDecls name params) targs
      else case M.lookup tname (envtypes priorEnv) of
        Just (TComp path tname _) ->
          TComp path tname
            <$> mapM (argToType priorEnv typeDecls name params) targs
        Nothing -> throwError $ InferError (UnknownType tname) NoReason

argToType priorEnv typeDecls name params (Meta _ _ (TRArr l r)) = do
  l' <- argToType priorEnv typeDecls name params l
  r' <- argToType priorEnv typeDecls name params r
  return $ TArr l' r'

argToType priorEnv typeDecls name params (Meta _ _ (TRRecord f)) = do
  f' <- mapM (argToType priorEnv typeDecls name params) f
  return $ TRecord f' False

argToType priorEnv typeDecls name params (Meta _ _ (TRTuple elems)) = do
  elems' <- mapM (argToType priorEnv typeDecls name params) elems
  return $ TTuple elems'
