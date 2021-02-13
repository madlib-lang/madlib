{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.CanonicalM where

import           Control.Monad.Except
import           Error.Error


type CanonicalM a = forall m . MonadError InferError m => m a
