{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import qualified Rock
import Driver.Query


data InferState = InferState { count :: Int, errors :: [CompilationError] }
  deriving (Show, Eq)


getErrors :: Infer [CompilationError]
getErrors = gets errors


pushError :: CompilationError -> Infer ()
pushError err = do
  s <- get
  put s { errors = errors s ++ [err] }


type Infer a = forall m . (Rock.MonadFetch Query m, MonadError CompilationError m, MonadState InferState m) => m a

unsafeRun :: (Rock.MonadFetch Query m) => Infer a -> m a
unsafeRun i = do
  x <- runExceptT (runStateT i InferState { count = 0, errors = [] })
  case x of
    Right (a, _) ->
      return a

simpleRun :: (Rock.MonadFetch Query m) => Infer a -> m (Either CompilationError a)
simpleRun i = do
  x <- runExceptT (runStateT i InferState { count = 0, errors = [] })
  case x of
    Right (a, _) ->
      return $ Right a

    Left e       ->
      return $ Left e

