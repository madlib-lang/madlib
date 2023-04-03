{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Infer.Monomorphize where

import qualified Data.Map                       as Map
import           Data.IORef
import           Infer.MonomorphizationState
import           AST.Solved
import qualified Rock
import           Driver.Query
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Text.Show.Pretty
import           Infer.Unify (gentleUnify)
import           Infer.Type
import Infer.Substitute


findExpByName :: Rock.MonadFetch Query m => FilePath -> String -> m (Maybe (Exp, FilePath))
findExpByName moduleWhereItsUsed expName = do
  maybeExp <- Rock.fetch $ ForeignExp moduleWhereItsUsed expName
  case maybeExp of
    Just found ->
      return $ Just (found, moduleWhereItsUsed)

    Nothing -> do
      (ast, _) <- Rock.fetch $ SolvedASTWithEnv moduleWhereItsUsed
      case findForeignModuleForImportedName expName ast of
        Just foreignModulePath -> do
          found <- Rock.fetch $ ForeignExp moduleWhereItsUsed expName
          return $ (,foreignModulePath) <$> found

        _ ->
          return Nothing

type Monomorphize a = forall m . (Rock.MonadFetch Query m, MonadIO m) => m a

data Env
  = Env
  { envCurrentModulePath :: FilePath
  , envSubstitution :: Substitution
  }


monomorphizeDefinition :: Env -> String -> Type -> Monomorphize String
monomorphizeDefinition env@Env{ envCurrentModulePath } fnName typeItIsCalledWith = do
  state <- liftIO $ readIORef monomorphizationState
  foundExp <- findExpByName envCurrentModulePath fnName

  case foundExp of
    Just (fnDefinition, fnModulePath) -> do
      let s = gentleUnify typeItIsCalledWith (getType fnDefinition)
      let fnId = FunctionId fnName fnModulePath typeItIsCalledWith

      case Map.lookup fnId state of
        Just MonomorphizationRequest {} ->
          liftIO $ makeMonomorphizedName fnName fnModulePath typeItIsCalledWith

        Nothing -> do
          monomorphicName <- liftIO $ newRequest fnName fnModulePath typeItIsCalledWith
          monomorphize env{ envSubstitution = s, envCurrentModulePath = fnModulePath } fnDefinition
          return monomorphicName

    Nothing ->
      return fnName
      -- error $ "This should not happen but function: '" ++ fnName ++ "' was not found."


-- TODO: we need special handling for monomorphizing local functions
-- TODO: we need to apply the substitution to all types in the AST
monomorphize :: Env -> Exp -> Monomorphize Exp
monomorphize env@Env{ envSubstitution } exp = case exp of
  Typed _ _ (App _ _ _) -> do
    let (fn, args) = collectAppArgs True exp

    case fn of
      Typed _ _ (Placeholder _ (Typed qt area (Var fnName False))) -> do
        monomorphicName <- monomorphizeDefinition env fnName (getType fn)
        liftIO $ putStrLn $ "MONO NAME: " ++ monomorphicName

        -- TODO: we need to proceed otherwise as we need to rebuild the App tree with all args
        return $ Typed qt area (Var monomorphicName False)

      _ ->
        return exp

  Typed qt area (TypedExp e typing sc) -> do
    e' <- monomorphize env e
    return $ Typed (apply envSubstitution qt) area (TypedExp e' typing sc)

  Typed qt area (Assignment n e) -> do
    e' <- monomorphize env e
    return $ Typed (apply envSubstitution qt) area (Assignment n e')

  Typed qt area (Abs p es) -> do
    es' <- mapM (monomorphize env) es
    return $ Typed (apply envSubstitution qt) area (Abs p es')

  Typed qt area (Placeholder ref e) -> do
    e' <- monomorphize env e
    return $ Typed (apply envSubstitution qt) area (Placeholder ref e')

  e ->
    return e
