{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import qualified Data.Set as Set
import qualified Rock
import Driver.Query
import Error.Warning
import Canonicalize.InstanceToDerive
import Canonicalize.CanonicalM (pushRecordToDerive)

type Infer a = forall m . (Rock.MonadFetch Query m, MonadIO m, MonadError CompilationError m, MonadState InferState m) => m a

data InferState
  = InferState
  { extensibleRecordsToDerive :: Set.Set InstanceToDerive
  , count :: Int
  , errors :: [CompilationError]
  , warnings :: [CompilationWarning]
  }


getErrors :: Infer [CompilationError]
getErrors = gets errors


pushError :: CompilationError -> Infer ()
pushError err = do
  s <- get
  put s { errors = errors s ++ [err] }


pushWarning :: CompilationWarning -> Infer ()
pushWarning warning = do
  s <- get
  put s { warnings = warnings s <> [warning] }


pushExtensibleRecordToDerive :: [String] -> Infer ()
pushExtensibleRecordToDerive fieldNames = do
  s <- get
  put s { extensibleRecordsToDerive =
            Set.singleton (RecordToDerive (Set.fromList fieldNames))
            <> extensibleRecordsToDerive s
        }
