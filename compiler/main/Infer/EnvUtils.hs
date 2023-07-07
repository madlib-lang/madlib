{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Infer.EnvUtils where


import qualified AST.Canonical                 as Can
import           Infer.Type
import           Infer.Infer
import           Infer.Instantiate
import           Error.Error
import           Error.Context
import qualified Data.Map                      as M
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.Set as Set
import           Infer.Env
import qualified Rock
import qualified Driver.Query as Query
import qualified Data.List as List
import Control.Applicative



isConstructor :: Env -> String -> Bool
isConstructor env name =
  Set.member name (envConstructors env)


isNameInImport :: String -> ImportInfo -> Bool
isNameInImport name ImportInfo { iiType, iiName }
  | iiType == NameImport && iiName == name = True
  | iiType == NamespaceImport && iiName == takeWhile (/= '.') name = True
  | otherwise = False


lookupVar :: Env -> String -> Infer Scheme
lookupVar env name = do
  maybeType <- case List.find (isNameInImport name) $ envImportInfo env of
    Just (ImportInfo path NameImport name) ->
      Rock.fetch $ Query.ForeignScheme path name

    Just (ImportInfo path NamespaceImport _) ->
      let afterNamespace = dropWhile (/= '.') name
      in  if not (null afterNamespace) then
            Rock.fetch $ Query.ForeignScheme path (tail afterNamespace)
          else
            Rock.fetch $ Query.ForeignScheme path ""

    _ ->
      return $ M.lookup name (envVars env) <|> M.lookup name (envMethods env)

  case maybeType of
    Just sc ->
      return sc

    Nothing ->
      throwError $ CompilationError (UnboundVariable name) NoContext


extendVars :: Env -> (String, Scheme) -> Env
extendVars env (x, s) = env { envVars = M.insert x s $ envVars env }


safeExtendVars :: Env -> (String, Scheme) -> Infer Env
safeExtendVars env (i, sc) = case M.lookup i (envVars env) <|> M.lookup i (envMethods env) of
  Just _ ->
    throwError $ CompilationError (NameAlreadyDefined i) NoContext

  Nothing ->
    return $ extendVars env (i, sc)


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
lookupInterface env name = case M.lookup name (envInterfaces env) of
  Just found ->
    return found

  Nothing -> do
    res <- Rock.fetch $ Query.SolvedInterface (envCurrentPath env) name
    case res of
      Just found ->
        return found

      Nothing ->
        throwError $ CompilationError (InterfaceNotExisting name) NoContext



mergeVars :: Env -> Vars -> Env
mergeVars env vs = env { envVars = vs <> envVars env }


setNamespacesInScope :: Env -> Set.Set String -> Env
setNamespacesInScope env ns = env { envNamespacesInScope = ns }


mergeEnv :: Env -> Env -> Env
mergeEnv initial env = Env { envVars                 = envVars initial <> envVars env
                           , envMethods              = envMethods initial <> envMethods env
                           , envInterfaces           = envInterfaces initial <> envInterfaces env
                           , envConstructors         = envConstructors initial <> envConstructors env
                           , envCurrentPath          = envCurrentPath env
                           , envInBody               = False
                           , envNamesInScope         = mempty
                           , envNamespacesInScope    = envNamespacesInScope initial <> envNamespacesInScope env
                           , envImportInfo           = envImportInfo initial
                           , envPlaceholdersToDelete = mempty
                           , envPlaceholdersInScope  = []
                           }


initialEnv :: Env
initialEnv = Env
  { envVars        = M.fromList
                       [ ("&&"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("!="           , Forall [Star] $ [IsIn "Eq" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                       , ("||"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                       , ("!"            , Forall [] $ [] :=> (tBool `fn` tBool))

                       , ("++"           , Forall [] $ [] :=> (tStr `fn` tStr `fn` tStr))

                       , ("%"            , Forall [] $ [] :=> (tInteger `fn` tInteger `fn` tInteger))
                       , ("|>"           , Forall [Star, Star] $ [] :=> (TGen 0 `fn` (TGen 0 `fn` TGen 1) `fn` TGen 1))
                       , ("$"            , Forall [Star] $ [] :=> TGen 0)
                       , ("__dict_ctor__", Forall [Star, Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (tListOf (TApp (TApp tTuple2 (TGen 0)) (TGen 1)) `fn` tDictionaryOf (TGen 0) (TGen 1)))
                       ]
  , envInterfaces = M.fromList
      [ ("Number", Interface [TV "a" Star] []
                    [ Instance ([] :=> IsIn "Number" [tFloat] Nothing) M.empty
                    , Instance ([] :=> IsIn "Number" [tByte] Nothing) M.empty
                    , Instance ([] :=> IsIn "Number" [tShort] Nothing) M.empty
                    , Instance ([] :=> IsIn "Number" [tInteger] Nothing) M.empty
                    ]
        )
      , ("Bits", Interface [TV "a" Star] [IsIn "Number" [TVar $ TV "a" Star] Nothing]
                  [ Instance ([] :=> IsIn "Bits" [tByte] Nothing) M.empty
                  , Instance ([] :=> IsIn "Bits" [tShort] Nothing) M.empty
                  , Instance ([] :=> IsIn "Bits" [tInteger] Nothing) M.empty
                  ]
        )
      , ("Show", Interface [TV "a" Star] []
                [ Instance ([] :=> IsIn "Show" [tStr] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tChar] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tInteger] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tShort] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tByte] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tFloat] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tBool] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tUnit] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [tByteArray] Nothing) M.empty
                , Instance ([IsIn "Show" [TVar (TV "a" Star)] Nothing] :=> IsIn "Show" [tListOf (TVar (TV "a" Star))] Nothing) M.empty
                , Instance ([IsIn "Show" [TVar (TV "a" Star)] Nothing] :=> IsIn "Show" [tArrayOf (TVar (TV "a" Star))] Nothing) M.empty
                , Instance ([
                    IsIn "Show" [TVar (TV "a" Star)] Nothing,
                    IsIn "Show" [TVar (TV "b" Star)] Nothing
                  ] :=> IsIn "Show" [tDictionaryOf (TVar (TV "a" Star)) (TVar (TV "b" Star))] Nothing) M.empty
                , Instance ([] :=> IsIn "Show" [TVar (TV "a" Star) `fn` TVar (TV "b" Star)] Nothing) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp tTuple2 (TVar (TV "a" Star))) (TVar (TV "b" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp tTuple3 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp tTuple4 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing,
                      IsIn "Show" [TVar (TV "e" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp (TApp tTuple5 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing,
                      IsIn "Show" [TVar (TV "e" Star)] Nothing,
                      IsIn "Show" [TVar (TV "f" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp (TApp (TApp tTuple6 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing,
                      IsIn "Show" [TVar (TV "e" Star)] Nothing,
                      IsIn "Show" [TVar (TV "f" Star)] Nothing,
                      IsIn "Show" [TVar (TV "g" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple7 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing,
                      IsIn "Show" [TVar (TV "e" Star)] Nothing,
                      IsIn "Show" [TVar (TV "f" Star)] Nothing,
                      IsIn "Show" [TVar (TV "g" Star)] Nothing,
                      IsIn "Show" [TVar (TV "h" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple8 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing,
                      IsIn "Show" [TVar (TV "e" Star)] Nothing,
                      IsIn "Show" [TVar (TV "f" Star)] Nothing,
                      IsIn "Show" [TVar (TV "g" Star)] Nothing,
                      IsIn "Show" [TVar (TV "h" Star)] Nothing,
                      IsIn "Show" [TVar (TV "i" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple9 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))) (TVar (TV "i" Star))
                    ] Nothing
                  ) M.empty
                , Instance ([
                      IsIn "Show" [TVar (TV "a" Star)] Nothing,
                      IsIn "Show" [TVar (TV "b" Star)] Nothing,
                      IsIn "Show" [TVar (TV "c" Star)] Nothing,
                      IsIn "Show" [TVar (TV "d" Star)] Nothing,
                      IsIn "Show" [TVar (TV "e" Star)] Nothing,
                      IsIn "Show" [TVar (TV "f" Star)] Nothing,
                      IsIn "Show" [TVar (TV "g" Star)] Nothing,
                      IsIn "Show" [TVar (TV "h" Star)] Nothing,
                      IsIn "Show" [TVar (TV "i" Star)] Nothing,
                      IsIn "Show" [TVar (TV "j" Star)] Nothing
                    ] :=> IsIn "Show" [
                      TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp tTuple10 (TVar (TV "a" Star))) (TVar (TV "b" Star))) (TVar (TV "c" Star))) (TVar (TV "d" Star))) (TVar (TV "e" Star))) (TVar (TV "f" Star))) (TVar (TV "g" Star))) (TVar (TV "h" Star))) (TVar (TV "i" Star))) (TVar (TV "j" Star))
                    ] Nothing
                  ) M.empty
                ]
        )
      , ("Eq", Interface [TV "a" Star] []
                [ Instance ([] :=> IsIn "Eq" [tInteger] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tShort] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tFloat] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tByte] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tStr] Nothing) M.empty
                , Instance ([] :=> IsIn "Eq" [tChar] Nothing) M.empty
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
      , ("/"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tFloat))
      , ("unary-minus"  , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0))
      , (">"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("<"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , (">="           , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("<="           , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("=="           , Forall [Star] $ [IsIn "Eq" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      , ("show"      , Forall [Star] $ [IsIn "Show" [TGen 0] Nothing] :=> (TGen 0 `fn` tStr))

      , ("|"            , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("&"            , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("^"            , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , ("~"            , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0))
      , ("<<"           , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , (">>"           , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      , (">>>"          , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
      ]
  , envCurrentPath = ""
  , envInBody = False
  , envNamesInScope = mempty
  , envNamespacesInScope = mempty
  , envImportInfo = mempty
  , envPlaceholdersToDelete = mempty
  , envPlaceholdersInScope = []
  }
