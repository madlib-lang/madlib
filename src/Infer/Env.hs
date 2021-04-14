{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Env where


import qualified AST.Canonical                 as Can
import           Infer.Type
import           Infer.Infer
import           Error.Error
import qualified Data.Map                      as M
import           Control.Monad.Except           ( MonadError(throwError) )


data Interface = Interface [TVar] [Pred] [Instance] deriving(Eq, Show)

data Instance = Instance (Qual Pred) Vars deriving(Eq, Show)


type Vars = M.Map String Scheme
type Interfaces = M.Map Id Interface
type Methods = M.Map String Scheme
type TypeDecls = M.Map String Type

data Env
  = Env
    { envVars         :: Vars
    , envInterfaces   :: Interfaces
    , envMethods      :: Methods
    , envCurrentPath  :: FilePath
    , envBacktrace    :: Backtrace
    }
    deriving(Eq, Show)

lookupVar :: Env -> String -> Infer Scheme
lookupVar env x = case M.lookup x (envVars env <> envMethods env) of
  Just x  -> return x
  Nothing -> throwError $ InferError (UnboundVariable x) NoContext


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envVars = M.insert x s $ envVars env }


safeExtendVars :: Env -> (String, Scheme) -> Infer Env
safeExtendVars env (i, sc) = case M.lookup i (envVars env) of
  Just _  -> throwError $ InferError (NameAlreadyDefined i) NoContext
  Nothing -> return $ extendVars env (i, sc)


lookupInterface :: Env -> Can.Name -> Infer Interface
lookupInterface env n = case M.lookup n (envInterfaces env) of
  Just i -> return i
  _      -> throwError $ InferError (InterfaceNotExisting n) NoContext


mergeVars :: Env -> Vars -> Env
mergeVars env vs = env { envVars = envVars env <> vs }


mergeEnv :: Env -> Env -> Env
mergeEnv initial env = Env { envVars        = envVars initial <> envVars env
                           , envMethods     = envMethods initial <> envMethods env
                           , envInterfaces  = envInterfaces initial <> envInterfaces env
                           , envBacktrace   = mempty
                           , envCurrentPath = envCurrentPath env
                           }


initialEnv :: Env
initialEnv = Env
  { envVars        = M.fromList
                       [ ("==", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("!=", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("&&", Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("||", Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("!" , Forall [] $ [] :=> (tBool `fn` tBool))
                       , (">" , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("<" , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , (">=", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("<=", Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("++", Forall [] $ [] :=> (tStr `fn` tStr `fn` tStr))
                       , ("+" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
                       , ("unary-minus" , Forall [] $ [] :=> (tNumber `fn` tNumber))
                       , ("-" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
                       , ("*" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
                       , ("/" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
                       , ("%" , Forall [] $ [] :=> (tNumber `fn` tNumber `fn` tNumber))
                       , ("|>", Forall [Star, Star] $ [] :=> (TGen 0 `fn` (TGen 0 `fn` TGen 1) `fn` TGen 1))
                       , ("$" , Forall [Star] $ [] :=> TGen 0)
                       ]
  , envInterfaces  = mempty
  , envMethods     = mempty
  , envCurrentPath = ""
  , envBacktrace   = mempty
  }

pushExpToBT :: Env -> Can.Exp -> Env
pushExpToBT env exp = env { envBacktrace = BTExp exp : envBacktrace env }

pushInstanceToBT :: Env -> Can.Instance -> Env
pushInstanceToBT env inst = env { envBacktrace = BTInstance inst : envBacktrace env }
