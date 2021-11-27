{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Env where


import qualified AST.Canonical                 as Can
import           Infer.Type
import           Infer.Infer
import           Infer.Instantiate
import           Error.Error
import           Error.Backtrace
import           Error.Context
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
  Nothing -> throwError $ CompilationError (UnboundVariable x) NoContext


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envVars = M.insert x s $ envVars env }


safeExtendVars :: Env -> (String, Scheme) -> Infer Env
safeExtendVars env (i, sc) = case M.lookup i (envVars env) of
  Just _  -> throwError $ CompilationError (NameAlreadyDefined i) NoContext
  Nothing -> return $ extendVars env (i, sc)


safeExtendVarsForAbsParam :: Env -> (String, Scheme) -> Infer Env
safeExtendVarsForAbsParam env (i, sc) = case M.lookup i (envVars env) of
  Just sc'  -> do
    (_ :=> t) <- instantiate sc'
    if isTVar t then
      throwError $ CompilationError (NameAlreadyDefined i) NoContext
    else
      return $ extendVars env (i, sc)
  Nothing -> return $ extendVars env (i, sc)


lookupInterface :: Env -> Can.Name -> Infer Interface
lookupInterface env n = case M.lookup n (envInterfaces env) of
  Just i -> return i
  _      -> throwError $ CompilationError (InterfaceNotExisting n) NoContext


mergeVars :: Env -> Vars -> Env
mergeVars env vs = env { envVars = vs <> envVars env }


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
                       [ ("=="           , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("!="           , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("&&"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("||"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("!"            , Forall [] $ [] :=> (tBool `fn` tBool))
                       , (">"            , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("<"            , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , (">="           , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("<="           , Forall [Star] $ [] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

                       , ("++"           , Forall [] $ [] :=> (tStr `fn` tStr `fn` tStr))

                       , ("/"            , Forall [] $ [] :=> (tFloat `fn` tFloat `fn` tFloat))
                       , ("%"            , Forall [] $ [] :=> (tFloat `fn` tFloat `fn` tFloat))
                       , ("^"            , Forall [] $ [] :=> (tFloat `fn` tFloat `fn` tFloat))
                       , ("|"            , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("&"            , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("~"            , Forall [] $ [] :=> (tInteger `fn` tInteger))
                       , ("<<"           , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , (">>"           , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , (">>>"          , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("|>"           , Forall [Star, Star] $ [] :=> (TGen 0 `fn` (TGen 0 `fn` TGen 1) `fn` TGen 1))
                       , ("$"            , Forall [Star] $ [] :=> TGen 0)
                       , ("__dict_ctor__", Forall [Star, Star] $ [] :=> (tListOf (TApp (TApp tTuple2 (TGen 0)) (TGen 1)) `fn` tDictionaryOf (TGen 0) (TGen 1)))
                       ]
                      --  Instance (Qual Pred) Vars
  , envInterfaces = M.singleton "Number" (Interface [TV "a" Star] [] [Instance ([] :=> IsIn "Number" [tFloat] Nothing) M.empty, Instance ([] :=> IsIn "Number" [tByte] Nothing) M.empty, Instance ([] :=> IsIn "Number" [tInteger] Nothing) M.empty])
  , envMethods = M.fromList
      [ ("+"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("-"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("*"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("unary-minus"  , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0))
      ]
  , envCurrentPath = ""
  , envBacktrace   = mempty
  }

pushExpToBT :: Env -> Can.Exp -> Env
pushExpToBT env exp = env { envBacktrace = BTExp exp : envBacktrace env }

resetBT :: Env -> Env
resetBT env = env { envBacktrace = [] }

pushInstanceToBT :: Env -> Can.Instance -> Env
pushInstanceToBT env inst = env { envBacktrace = BTInstance inst : envBacktrace env }
