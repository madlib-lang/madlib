{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.Test where
import           AST.Solved
import           Infer.Infer
import           Control.Monad.Except
import           Error.Error
import           Error.Context
import           Data.Maybe
import qualified Data.List                      as List
import           Text.Show.Pretty (ppShow)
import           Infer.Type


qtTypecheckFailed :: Qual Type
qtTypecheckFailed = [] :=> TVar (TV (-1) Star)


verifyTests :: AST -> Infer ()
verifyTests ast = case apath ast of
  Just p | ".spec.mad" `List.isSuffixOf` p ->
    mapM_ (verifyTest p) (aexps ast)

  _ ->
    return ()


verifyTest :: FilePath -> Exp -> Infer ()
verifyTest modulePath exp = case exp of
  Typed _ _ (Assignment _ _) ->
    return ()

  Typed _ _ (Extern _ _ _) ->
    return ()

  Typed _ _ (Export e) ->
    verifyTest modulePath e

  Typed _ _ (JSExp _) ->
    return ()

  Typed _ _ (TypedExp e _ _) ->
    verifyTest modulePath e

  Typed qt _ _ ->
    if qt == qtTypecheckFailed then
      return ()
    else
      throwError $ CompilationError (TestNotValid (getType exp)) (Context modulePath (getArea exp))

  _ ->
    throwError $ CompilationError (TestNotValid (getType exp)) (Context modulePath (getArea exp))
