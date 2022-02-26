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
import qualified Data.Set as Set


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
    , envConstructors :: Set.Set String
    , envMethods      :: Methods
    , envCurrentPath  :: FilePath
    , envBacktrace    :: Backtrace
    , envNamespacesInScope :: Set.Set String
    }
    deriving(Eq, Show)


isConstructor :: Env -> String -> Bool
isConstructor env name =
  Set.member name (envConstructors env)

lookupVar :: Env -> String -> Infer Scheme
lookupVar env x = case M.lookup x (envVars env <> envMethods env) of
  Just x  -> return x
  Nothing -> throwError $ CompilationError (UnboundVariable x) NoContext


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envVars = M.insert x s $ envVars env }


safeExtendVars :: Env -> (String, Scheme) -> Infer Env
safeExtendVars env (i, sc) = case M.lookup i (envVars env <> envMethods env) of
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


setNamespacesInScope :: Env -> Set.Set String -> Env
setNamespacesInScope env ns = env { envNamespacesInScope = ns }


mergeEnv :: Env -> Env -> Env
mergeEnv initial env = Env { envVars              = envVars initial <> envVars env
                           , envMethods           = envMethods initial <> envMethods env
                           , envInterfaces        = envInterfaces initial <> envInterfaces env
                           , envConstructors      = envConstructors initial <> envConstructors env
                           , envBacktrace         = mempty
                           , envCurrentPath       = envCurrentPath env
                           , envNamespacesInScope = envNamespacesInScope initial <> envNamespacesInScope env
                           }


initialEnv :: Env
initialEnv = Env
  { envVars        = M.fromList
                       [ ("&&"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("!="           , Forall [Star] $ [IsIn "Eq" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("||"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("!"            , Forall [] $ [] :=> (tBool `fn` tBool))

                       , ("++"           , Forall [] $ [] :=> (tStr `fn` tStr `fn` tStr))

                       , ("/"            , Forall [] $ [] :=> (tFloat `fn` tFloat `fn` tFloat))
                       , ("%"            , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("^"            , Forall [] $ [] :=> (tFloat `fn` tFloat `fn` tFloat))
                       , ("|"            , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("&"            , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("~"            , Forall [] $ [] :=> (tInteger `fn` tInteger))
                       , ("<<"           , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , (">>"           , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , (">>>"          , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("|>"           , Forall [Star, Star] $ [] :=> (TGen 0 `fn` (TGen 0 `fn` TGen 1) `fn` TGen 1))
                       , ("$"            , Forall [Star] $ [] :=> TGen 0)
                       , ("__dict_ctor__", Forall [Star, Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (tListOf (TApp (TApp tTuple2 (TGen 0)) (TGen 1)) `fn` tDictionaryOf (TGen 0) (TGen 1)))
                       ]
  , envInterfaces = M.fromList
      [ ("Number", Interface [TV "a" Star] []
                    [ Instance ([] :=> IsIn "Number" [tFloat] Nothing) M.empty
                    , Instance ([] :=> IsIn "Number" [tByte] Nothing) M.empty
                    , Instance ([] :=> IsIn "Number" [tInteger] Nothing) M.empty
                    ]
        )
      , ("Inspect", Interface [TV "a" Star] []
                [ Instance ([] :=> IsIn "Inspect" [tStr] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [tInteger] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [tByte] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [tFloat] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [tBool] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [tUnit] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [tByteArray] Nothing) M.empty
                , Instance ([IsIn "Inspect" [TVar (TV "a" Star)] Nothing] :=> IsIn "Inspect" [tListOf (TVar (TV "a" Star))] Nothing) M.empty
                , Instance ([IsIn "Inspect" [TVar (TV "a" Star)] Nothing] :=> IsIn "Inspect" [tArrayOf (TVar (TV "a" Star))] Nothing) M.empty
                , Instance ([
                    IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                    IsIn "Inspect" [TVar (TV "b" Star)] Nothing
                  ] :=> IsIn "Inspect" [tDictionaryOf (TVar (TV "a" Star)) (TVar (TV "b" Star))] Nothing) M.empty
                , Instance ([] :=> IsIn "Inspect" [TVar (TV "a" Star) `fn` TVar (TV "b" Star)] Nothing) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp tTuple2 (TVar (TV "a" Star))) (TVar (TV "b" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp tTuple3 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp tTuple4 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "e" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp (TApp tTuple5 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "e" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "f" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp (TApp (TApp tTuple6 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "e" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "f" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "g" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple7 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "e" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "f" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "g" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "h" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple8 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "e" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "f" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "g" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "h" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "i" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple9 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))) (TVar (TV "i" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Inspect" [TVar (TV "a" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "b" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "c" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "d" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "e" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "f" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "g" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "h" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "i" Star)] Nothing,
                      IsIn "Inspect" [TVar (TV "j" Star)] Nothing
                    ] :=> IsIn "Inspect" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple10 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))) (TVar (TV "i" Star))) (TVar (TV "j" Star))
                    ] Nothing
                  ) M.empty
                ]
        )
      , ("Eq", Interface [TV "a" Star] []
                [ Instance ([] :=> IsIn "Eq" [tInteger] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tFloat] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tByte] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tStr] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tBool] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tUnit] Nothing) M.empty
                , Instance ([
                    IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                    IsIn "Eq" [TVar (TV "b" Star)] Nothing
                  ] :=> IsIn "Eq" [tDictionaryOf (TVar (TV "a" Star)) (TVar (TV "b" Star))] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [TVar (TV "a" Star) `fn` TVar (TV "b" Star)] Nothing) M.empty
                , Instance ([IsIn "Eq" [TVar (TV "a" Star)] Nothing] :=> IsIn "Eq" [tListOf (TVar (TV "a" Star))] Nothing) M.empty
                , Instance ([IsIn "Eq" [TVar (TV "a" Star)] Nothing] :=> IsIn "Eq" [tArrayOf (TVar (TV "a" Star))] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tByteArray] Nothing) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp tTuple2 (TVar (TV "a" Star))) (TVar (TV "b" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp tTuple3 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp tTuple4 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "e" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp (TApp tTuple5 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "e" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "f" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp (TApp (TApp tTuple6 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "e" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "f" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "g" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple7 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "e" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "f" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "g" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "h" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple8 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "e" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "f" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "g" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "h" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "i" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple9 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))) (TVar (TV "i" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Eq" [TVar (TV "a" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "b" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "c" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "d" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "e" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "f" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "g" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "h" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "i" Star)] Nothing,
                      IsIn "Eq" [TVar (TV "j" Star)] Nothing
                    ] :=> IsIn "Eq" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple10 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))) (TVar (TV "i" Star))) (TVar (TV "j" Star))
                    ] Nothing
                  ) M.empty
                ]
        )
      ]
  , envConstructors = Set.empty
  , envMethods = M.fromList
      [ ("+"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("-"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("*"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("unary-minus"  , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0))
      , (">"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("<"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , (">="           , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("<="           , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("=="           , Forall [Star] $ [IsIn "Eq" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("inspect"      , Forall [Star] $ [IsIn "Inspect" [TGen 0] Nothing] :=> (TGen 0 `fn` tStr))
      ]
  , envCurrentPath = ""
  , envBacktrace   = mempty
  , envNamespacesInScope = mempty
  }

pushExpToBT :: Env -> Can.Exp -> Env
pushExpToBT env exp = env { envBacktrace = BTExp exp : envBacktrace env }

resetBT :: Env -> Env
resetBT env = env { envBacktrace = [] }

pushInstanceToBT :: Env -> Can.Instance -> Env
pushInstanceToBT env inst = env { envBacktrace = BTInstance inst : envBacktrace env }
