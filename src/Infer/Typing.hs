{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Typing where

import qualified AST.Source                    as Src
import           Infer.Type
import           Explain.Meta
import           Infer.Infer
import           Data.Char                      ( isLower )
import qualified Data.Map                      as M
import           Control.Monad.Except           ( MonadError(throwError) )
import           Error.Error
import           Explain.Reason
import           Infer.Substitute
import           Infer.Instantiate
import qualified Data.Set                      as S


typingToType :: Env -> Src.Typing -> Infer Type
typingToType env (Meta _ _ (Src.TRSingle t))
  | t == "Number" = return $ TCon CNum
  | t == "Boolean" = return $ TCon CBool
  | t == "String" = return $ TCon CString
  | isLower $ head t = return $ TVar $ TV t
  | otherwise = do
    h <- lookupADT env t
    case h of
      (TComp astPath realName _) -> return $ TComp astPath realName []

      -- TODO: need to apply params to the type
      (TAlias _ _ _ t          ) -> return t


typingToType env (Meta _ _ (Src.TRComp t ts)) = do
  -- fetch ADT from env, and verify that the args applied match it or ERR
  h <- lookupADT env t
  case h of
    (TComp astPath realName _) -> do
      params <- mapM (typingToType env) ts
      return $ TComp astPath realName params

    (TAlias _ _ vars t) -> do
      params <- mapM (typingToType env) ts
      let subst = M.fromList $ zip vars params
      return $ apply subst t

typingToType env (Meta _ _ (Src.TRArr l r)) = do
  l' <- typingToType env l
  r' <- typingToType env r
  return $ TArr l' r'

typingToType env (Meta _ _ (Src.TRRecord fields)) = do
  fields' <- mapM (typingToType env) fields
  return $ TRecord fields' False

typingToType env (Meta _ _ (Src.TRTuple elems)) = do
  elems' <- mapM (typingToType env) elems
  return $ TTuple elems'

lookupADT :: Env -> String -> Infer Type
lookupADT env x = do
  case M.lookup x $ envtypes env of
    Nothing -> throwError $ InferError (UnknownType x) NoReason
    Just x  -> return x
