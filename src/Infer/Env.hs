{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Env where


import qualified AST.Canonical                 as Can
import           Infer.Type
import           Infer.Infer
import           Explain.Reason
import           Error.Error
import qualified Data.Map                      as M
import           Control.Monad.Except           ( MonadError(throwError) )


data Interface = Interface [TVar] [Pred] [Instance] deriving(Eq, Show)

newtype Instance = Instance (Qual Pred) deriving(Eq, Show)


type Vars = M.Map String Scheme
type Interfaces = M.Map Id Interface
type Methods = M.Map String Scheme
type TypeDecls = M.Map String Type

data Env
  = Env
    { envvars         :: Vars
    , envinterfaces   :: Interfaces
    , envmethods      :: Methods
    , envcurrentpath  :: FilePath
    }
    deriving(Eq, Show)

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


lookupInterface :: Env -> Can.Name -> Infer Interface
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
  , envinterfaces  = M.empty
  , envmethods     = M.empty
  , envcurrentpath = ""
  }

