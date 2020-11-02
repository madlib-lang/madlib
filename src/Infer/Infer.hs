{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error


newtype Unique = Unique { count :: Int }
  deriving (Show, Eq, Ord)


type Infer a = forall m . (MonadError InferError m, MonadState Unique m) => m a
