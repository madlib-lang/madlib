{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Env where

import qualified Data.Map                      as M
import           Infer.Type
import           AST.Source
import           Control.Monad.Except
import           Infer.Instantiate
import           Infer.ADT
import           Infer.Infer
import           Explain.Reason
import           Error.Error
import           Data.Maybe                     ( fromMaybe )


lookupVar :: Env -> String -> Infer (Substitution, Type)
lookupVar env x = do
  case M.lookup x $ envvars env of
    Nothing -> case M.lookup x $ envimports env of
      Nothing -> throwError $ InferError (UnboundVariable x) NoReason
      Just s  -> do
        t <- instantiate $ Forall [] s
        return (M.empty, t)

    Just s -> do
      t <- instantiate s
      return (M.empty, t)


lookupADT :: Env -> String -> Infer Type
lookupADT env x = do
  case M.lookup x $ envadts env of
    Nothing -> throwError $ InferError (UnknownType x) NoReason
    Just x  -> return x


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envvars = M.insert x s $ envvars env }


initialEnv :: Env
initialEnv = Env
  { envvars        = M.fromList
    [ ( "==="
      , Forall [TV "a"] $ TVar (TV "a") `TArr` TVar (TV "a") `TArr` TCon CBool
      )
    , ("+", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ("-", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ("*", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ("/", Forall [] $ TCon CNum `TArr` TCon CNum `TArr` TCon CNum)
    , ( "|>"
      , Forall [TV "a", TV "b"]
      $      TVar (TV "a")
      `TArr` (TVar (TV "a") `TArr` TVar (TV "b"))
      `TArr` TVar (TV "b")
      )
    , ( "asList"
      , Forall [TV "a"] $ TArr (TVar $ TV "a") $ TComp "List" [TVar $ TV "a"]
      )
    , ( "List"
      , Forall [TV "a"] $ TArr (TVar $ TV "a") $ TComp "List" [TVar $ TV "a"]
      )
    ]
  , envadts        = M.fromList [("List", TComp "List" [TVar $ TV "a"])]
  , envimports     = M.empty
  , envcurrentpath = ""
  }


-- TODO: Should we build imported names here ?
buildInitialEnv :: AST -> Infer Env
buildInitialEnv AST { aadts, apath } = do
  tadts <- buildADTTypes aadts
  vars  <- resolveADTs tadts aadts
  let allVars = M.union (envvars initialEnv) vars
  return Env { envvars        = allVars
             , envadts        = tadts
             , envimports     = M.empty
             , envcurrentpath = fromMaybe "" apath
             }
