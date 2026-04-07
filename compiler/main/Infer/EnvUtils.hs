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
import qualified Data.HashMap.Strict           as HM
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.Set                      as Set
import           Infer.Env
import qualified Rock
import qualified Driver.Query                  as Query
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import           Control.Applicative
import qualified AST.Solved                    as Slv
import           Data.Foldable                  ( foldl' )
import           Utils.EditDistance             ( findSimilar )



isConstructor :: Env -> String -> Bool
isConstructor env name =
  Set.member name (envConstructors env)


isNameInImport :: String -> ImportInfo -> Bool
isNameInImport name ImportInfo { iiType, iiName }
  | iiType == NameImport && iiName == name = True
  | iiType == NamespaceImport && iiName == takeWhile (/= '.') name = True
  | otherwise = False


isNameInImportFast :: String -> Bool -> ImportInfo -> Bool
isNameInImportFast name hasDot ImportInfo { iiType, iiName }
  | iiType == NameImport     = iiName == name
  | hasDot                   = iiName == takeWhile (/= '.') name
  | otherwise                = False

lookupVar :: Env -> String -> Infer Scheme
lookupVar env name = do
  -- Fast path: check local env first. Since solveImport adds all imported names to envVars,
  -- most lookups (local vars, named imports) can be satisfied without touching envImportInfo.
  let localResult = M.lookup name (envVars env) <|> M.lookup name (envMethods env)
  maybeType <- case localResult of
    Just _ -> return localResult
    Nothing -> do
      let hasDot = '.' `elem` name
      case List.find (isNameInImportFast name hasDot) $ envImportInfo env of
        Just (ImportInfo path NameImport name) ->
          Rock.fetch $ Query.ForeignScheme path name

        Just (ImportInfo path NamespaceImport _) ->
          let afterNamespace = dropWhile (/= '.') name
          in  if not (null afterNamespace) then do
                let realName = tail afterNamespace
                sc <- Rock.fetch $ Query.ForeignFunctionScheme path realName
                exp <- Rock.fetch $ Query.ForeignExp path realName
                ctor <- Rock.fetch $ Query.ForeignExportedConstructor path realName
                (ast, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
                let nameExports = Maybe.mapMaybe Slv.maybeExportName (Slv.aexps ast)
                case exp of
                  Just e | Slv.isExport e ->
                    return sc

                  _ ->
                    if Maybe.isJust ctor || realName `elem` nameExports then
                      return sc
                    else
                      return Nothing
              else
                Rock.fetch $ Query.ForeignFunctionScheme path ""

        _ ->
          return Nothing

  case maybeType of
    Just sc ->
      return sc

    Nothing ->
      let candidates = Set.toList $ Set.fromList $ M.keys (envVars env) ++ M.keys (envMethods env) ++ M.keys (envNamesInScope env)
          suggestions = findSimilar name candidates
      in  throwError $ CompilationError (UnboundVariable name suggestions) NoContext


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


tDictionaryOf :: FilePath -> Type -> Type -> Type
tDictionaryOf builtinsPath keyType = TApp (TApp (mkTCon (TC "Dictionary" (Kfun Star (Kfun Star Star))) builtinsPath) keyType)


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
                           , envPatternBoundNames    = mempty
                           }

mkTupleInstance :: String -> Int -> Instance
mkTupleInstance cls n =
  let tvs = [TVar (TV i Star) | i <- [0..n-1]]
      ps  = [IsIn cls [tv] Nothing | tv <- tvs]
      t   = foldl' TApp (getTupleCtor n) tvs
  in  Instance (ps :=> IsIn cls [t] Nothing) M.empty

initialEnv :: Infer Env
initialEnv = do
  builtinsModulePath  <- Rock.fetch $ Query.AbsolutePreludePath "__BUILTINS__"
  jsonValueModulePath <- Rock.fetch $ Query.AbsolutePreludePath "Json/Value"
  jsonParseModulePath <- Rock.fetch $ Query.AbsolutePreludePath "Json/Parse"
  let tComparison  = mkTCon (TC "Comparison" Star) builtinsModulePath
      tJsonValue   = mkTCon (TC "Value"  Star)                    jsonValueModulePath
      tJsonParser  = mkTCon (TC "Parser" (Kfun Star Star))        jsonParseModulePath
  return Env
    { envVars        = M.fromList
                        [ ("&&"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                        , ("!="           , Forall [Star] $ [IsIn "Eq" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                        , ("||"           , Forall [] $ [] :=> (tBool `fn` tBool `fn` tBool))
                        , ("!"            , Forall [] $ [] :=> (tBool `fn` tBool))

                        , (">"            , Forall [Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                        , ("<"            , Forall [Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                        , (">="           , Forall [Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                        , ("<="           , Forall [Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

                        , ("%"            , Forall [Star] $ [IsIn "Bits" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
                        , ("|>"           , Forall [Star, Star] $ [] :=> (TGen 0 `fn` (TGen 0 `fn` TGen 1) `fn` TGen 1))
                        , ("$"            , Forall [Star] $ [] :=> TGen 0)
                        ]
    , envInterfaces = M.fromList
        [ ("Number", Interface [TV 0 Star] []
                      [ Instance ([] :=> IsIn "Number" [tFloat] Nothing) M.empty
                      , Instance ([] :=> IsIn "Number" [tByte] Nothing) M.empty
                      , Instance ([] :=> IsIn "Number" [tShort] Nothing) M.empty
                      , Instance ([] :=> IsIn "Number" [tInteger] Nothing) M.empty
                      ]
          )
        , ("Bits", Interface [TV 0 Star] [IsIn "Number" [TVar $ TV 0 Star] Nothing]
                    [ Instance ([] :=> IsIn "Bits" [tByte] Nothing) M.empty
                    , Instance ([] :=> IsIn "Bits" [tShort] Nothing) M.empty
                    , Instance ([] :=> IsIn "Bits" [tInteger] Nothing) M.empty
                    ]
          )
        , ("Show", Interface [TV 0 Star] []
                  -- These are needed for the tests at the moment otherwise the generated instances
                  -- fail if they can't find an instance for one of those
                  ( [ Instance ([] :=> IsIn "Show" [tStr] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tChar] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tInteger] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tShort] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tByte] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tFloat] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tBool] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tUnit] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [tByteArray] Nothing) M.empty
                    , Instance ([IsIn "Show" [TVar (TV 0 Star)] Nothing] :=> IsIn "Show" [tListOf (TVar (TV 0 Star))] Nothing) M.empty
                    , Instance ([IsIn "Show" [TVar (TV 0 Star)] Nothing] :=> IsIn "Show" [tArrayOf (TVar (TV 0 Star))] Nothing) M.empty
                    , Instance ([
                        IsIn "Show" [TVar (TV 0 Star)] Nothing,
                        IsIn "Show" [TVar (TV 1 Star)] Nothing
                      ] :=> IsIn "Show" [tDictionaryOf builtinsModulePath (TVar (TV 0 Star)) (TVar (TV 1 Star))] Nothing) M.empty
                    , Instance ([] :=> IsIn "Show" [TVar (TV 0 Star) `fn` TVar (TV 1 Star)] Nothing) M.empty
                    ]
                    ++ [mkTupleInstance "Show" n | n <- [2..10]]
                  )
          )
        , ("Comparable", Interface [TV 0 Star] [IsIn "Eq" [TVar $ TV 0 Star] Nothing]
                  -- These are needed for some tests
                  [ Instance ([] :=> IsIn "Comparable" [tInteger] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tShort] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tFloat] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tByte] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tStr] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tChar] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tBool] Nothing) M.empty
                  , Instance ([] :=> IsIn "Comparable" [tUnit] Nothing) M.empty
                  ]
          )
        , ("Json", Interface [TV 0 Star] []
                  -- No built-in instances; all instances are defined in Json.mad
                  []
          )
        , ("Eq", Interface [TV 0 Star] []
                  -- These are needed for the JS backend where Eq is a special generic function
                  ( [ Instance ([] :=> IsIn "Eq" [tInteger] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tShort] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tFloat] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tByte] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tStr] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tChar] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tBool] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tUnit] Nothing) M.empty
                    , Instance ([
                        IsIn "Eq" [TVar (TV 0 Star)] Nothing,
                        IsIn "Eq" [TVar (TV 1 Star)] Nothing
                      ] :=> IsIn "Eq" [tDictionaryOf builtinsModulePath (TVar (TV 0 Star)) (TVar (TV 1 Star))] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [TVar (TV 0 Star) `fn` TVar (TV 1 Star)] Nothing) M.empty
                    , Instance ([IsIn "Eq" [TVar (TV 0 Star)] Nothing] :=> IsIn "Eq" [tListOf (TVar (TV 0 Star))] Nothing) M.empty
                    , Instance ([IsIn "Eq" [TVar (TV 0 Star)] Nothing] :=> IsIn "Eq" [tArrayOf (TVar (TV 0 Star))] Nothing) M.empty
                    , Instance ([] :=> IsIn "Eq" [tByteArray] Nothing) M.empty
                    ]
                    ++ [mkTupleInstance "Eq" n | n <- [2..10]]
                  )
          )
        ]
    , envConstructors = Set.empty
    , envMethods = M.fromList
        [ ("+"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
        , ("-"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
        , ("*"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
        , ("/"            , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tFloat))
        , ("unary-minus"  , Forall [Star] $ [IsIn "Number" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0))
        , ("=="           , Forall [Star] $ [IsIn "Eq" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
        , ("compare"      , Forall [Star] $ [IsIn "Comparable" [TGen 0] Nothing] :=> (TGen 0 `fn` TGen 0 `fn` tComparison))
        , ("show"         , Forall [Star] $ [IsIn "Show" [TGen 0] Nothing] :=> (TGen 0 `fn` tStr))
        , ("toJson"       , Forall [Star] $ [IsIn "Json" [TGen 0] Nothing] :=> (TGen 0 `fn` tJsonValue))
        , ("fromJson"     , Forall [Star] $ [IsIn "Json" [TGen 0] Nothing] :=> TApp tJsonParser (TGen 0))

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
    , envPatternBoundNames = mempty
    }
