{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error


data InferState = InferState { count :: Int, errors :: [CompilationError] }
  deriving (Show, Eq)


getErrors :: Infer [CompilationError]
getErrors = gets errors


pushError :: CompilationError -> Infer ()
pushError err = do
  s <- get
  put s { errors = errors s ++ [err] }


type Infer a = forall m . (MonadError CompilationError m, MonadState InferState m) => m a

unsafeRun :: Infer a -> a
unsafeRun i = case runExcept (runStateT i InferState { count = 0, errors = [] }) of
  Right (a, _) -> a
