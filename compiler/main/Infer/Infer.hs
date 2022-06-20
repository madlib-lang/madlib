{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import qualified Rock
import Driver.Query

type Infer a = forall m . (Rock.MonadFetch Query m, MonadError CompilationError m, MonadState InferState m) => m a

data InferState = InferState { count :: Int, errors :: [CompilationError] }


getErrors :: Infer [CompilationError]
getErrors = gets errors


pushError :: CompilationError -> Infer ()
pushError err = do
  s <- get
  put s { errors = errors s ++ [err] }
