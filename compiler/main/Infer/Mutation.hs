{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.Mutation where

import           Infer.Env
import           Explain.Location
import           AST.Solved
import           Infer.Type
import           Infer.Infer
import           Error.Error
import           Control.Monad.Except
import           Error.Context
import           Text.Show.Pretty
import           Infer.Pred


-- TODO: we need to improve the search and keep track of where the preds are introduced
-- since between two levels that don't have any preds it's fine.
verifyMutations :: Env -> [String] -> Maybe Area -> Bool -> [Exp] -> Infer ()
verifyMutations env definedNames lastAbsArea hasPreds exps = case exps of
  (Typed _ area (Abs _ body) : next) -> do
    verifyMutations env definedNames (Just area) hasPreds body
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (TypedExp (Typed (ps :=> _) _ (Assignment n (Typed _ area (Abs _ body)))) _ _) : next) | not (null ps) -> do
    entailed <- forM ps $ entail env []
    verifyMutations env definedNames (Just area) (not (and entailed)) body
    verifyMutations env (n : definedNames) lastAbsArea hasPreds next

  (Typed (ps :=> _) _ (Assignment n (Typed ((_ : _) :=> _) area (Abs _ body))) : next) | not (null ps) -> do
    entailed <- forM ps $ entail env []
    verifyMutations env definedNames (Just area) (not (and entailed)) body
    verifyMutations env (n : definedNames) lastAbsArea hasPreds next

  (Typed (ps :=> _) _ (Assignment n e) : next) -> do
    entailed <- forM ps $ entail env []
    when (hasPreds && n `elem` definedNames) $ case lastAbsArea of
      Just area ->
        throwError $ CompilationError (OverloadedMutation n []) (Context (envCurrentPath env) area)

      Nothing ->
        throwError $ CompilationError (OverloadedMutation n []) NoContext

    verifyMutations env definedNames lastAbsArea (not (and entailed)) [e]
    verifyMutations env (n : definedNames) lastAbsArea hasPreds next

  (Typed (ps :=> _) _ (TypedExp ((Typed _ _ (Assignment n _))) _ _) : next) -> do
    entailed <- forM ps $ entail env []
    when (hasPreds && n `elem` definedNames) $ case lastAbsArea of
      Just area ->
        throwError $ CompilationError (OverloadedMutation n []) (Context (envCurrentPath env) area)

      Nothing ->
        throwError $ CompilationError (OverloadedMutation n []) NoContext

    verifyMutations env (n : definedNames) lastAbsArea (not (and entailed)) next

  (Typed _ _ (Do body) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds body
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (Placeholder _ e) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [e]
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (TypedExp e _ _) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [e]
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (Export e) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [e]
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (If cond truthy falsy) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [cond]
    verifyMutations env definedNames lastAbsArea hasPreds [truthy]
    verifyMutations env definedNames lastAbsArea hasPreds [falsy]
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (App fn arg _) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [fn]
    verifyMutations env definedNames lastAbsArea hasPreds [arg]
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (TemplateString es) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea hasPreds . (:[])) es
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (ListConstructor lis) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea hasPreds . (:[]) . getListItemExp) lis
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (Record fields) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea hasPreds . (:[]) . getFieldExp) fields
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (TupleConstructor es) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea hasPreds . (:[])) es
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (Where e branches) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [e]
    mapM_ (verifyMutations env definedNames lastAbsArea hasPreds . (:[]) . getIsExpression) branches
    verifyMutations env definedNames lastAbsArea hasPreds next

  (Typed _ _ (Access rec _) : next) -> do
    verifyMutations env definedNames lastAbsArea hasPreds [rec]
    verifyMutations env definedNames lastAbsArea hasPreds next

  (_ : next) ->
    verifyMutations env definedNames lastAbsArea hasPreds next

  [] ->
    return ()

