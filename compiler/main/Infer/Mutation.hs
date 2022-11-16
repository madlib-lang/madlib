{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.Mutation where

import           Infer.Env
import           Explain.Location
import qualified AST.Solved                 as Slv
import           Infer.Type
import           Infer.Infer
import           Error.Error
import           Control.Monad.Except
import           Error.Context
import Text.Show.Pretty
import Infer.Pred



verifyMutations :: Env -> [String] -> Maybe Area -> Bool -> [Slv.Exp] -> Infer ()
verifyMutations env definedNames lastAbsArea hasPreds exps = case exps of
  (Slv.Typed (ps :=> _) area (Slv.Abs _ body) : next) | not (null ps) -> do
    entailed <- forM ps $ entail env []
    verifyMutations env definedNames (Just area) (not $ and entailed) body
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.TypedExp (Slv.Typed (ps :=> _) _ (Slv.Assignment n (Slv.Typed _ area (Slv.Abs _ body)))) _ _) : next) | not (null ps) -> do
    entailed <- forM ps $ entail env []
    verifyMutations env definedNames (Just area) (not $ and entailed) body
    verifyMutations env (n : definedNames) lastAbsArea False next

  (Slv.Typed (ps :=> _) _ (Slv.Assignment n (Slv.Typed ((_ : _) :=> _) area (Slv.Abs _ body))) : next) | not (null ps) -> do
    entailed <- forM ps $ entail env []
    verifyMutations env definedNames (Just area) (not $ and entailed) body
    verifyMutations env (n : definedNames) lastAbsArea False next

  (Slv.Typed _ _ (Slv.Assignment n _) : next) -> do
    when (hasPreds && n `elem` definedNames) $ case lastAbsArea of
      Just area ->
        throwError $ CompilationError (OverloadedMutation n []) (Context (envCurrentPath env) area)

      Nothing ->
        throwError $ CompilationError (OverloadedMutation n []) NoContext

    verifyMutations env (n : definedNames) lastAbsArea False next

  (Slv.Typed _ _ (Slv.TypedExp ((Slv.Typed _ _ (Slv.Assignment n _))) _ _) : next) -> do
    when (hasPreds && n `elem` definedNames) $ case lastAbsArea of
      Just area ->
        throwError $ CompilationError (OverloadedMutation n []) (Context (envCurrentPath env) area)

      Nothing ->
        throwError $ CompilationError (OverloadedMutation n []) NoContext

    verifyMutations env (n : definedNames) lastAbsArea False next

  (Slv.Typed _ _ (Slv.Do body) : next) -> do
    verifyMutations env definedNames lastAbsArea False body
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.Placeholder _ e) : next) -> do
    verifyMutations env definedNames lastAbsArea False [e]
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.TypedExp e _ _) : next) -> do
    verifyMutations env definedNames lastAbsArea False [e]
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.If cond truthy falsy) : next) -> do
    verifyMutations env definedNames lastAbsArea False [cond]
    verifyMutations env definedNames lastAbsArea False [truthy]
    verifyMutations env definedNames lastAbsArea False [falsy]
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.App fn arg _) : next) -> do
    verifyMutations env definedNames lastAbsArea False [fn]
    verifyMutations env definedNames lastAbsArea False [arg]
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.TemplateString es) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea False . (:[])) es
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.ListConstructor lis) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea False . (:[]) . Slv.getListItemExp) lis
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.Record fields) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea False . (:[]) . Slv.getFieldExp) fields
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.TupleConstructor es) : next) -> do
    mapM_ (verifyMutations env definedNames lastAbsArea False . (:[])) es
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.Where e branches) : next) -> do
    verifyMutations env definedNames lastAbsArea False [e]
    mapM_ (verifyMutations env definedNames lastAbsArea False . (:[]) . Slv.getIsExpression) branches
    verifyMutations env definedNames lastAbsArea False next

  (Slv.Typed _ _ (Slv.Access rec _) : next) -> do
    verifyMutations env definedNames lastAbsArea False [rec]
    verifyMutations env definedNames lastAbsArea False next

  (_ : next) ->
    verifyMutations env definedNames lastAbsArea False next

  [] ->
    return ()

