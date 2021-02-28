{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error


data InferState = InferState { count :: Int, errors :: [InferError] }
  deriving (Show, Eq)


pushError :: InferError -> Infer ()
pushError err = do
  s <- get
  put s { errors = errors s ++ [err] }


type Infer a = forall m . (MonadError InferError m, MonadState InferState m) => m a
