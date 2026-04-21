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
import Infer.Type (Substitution, nullSubst)

type Infer a = forall m . (Rock.MonadFetch Query m, MonadIO m, MonadError CompilationError m, MonadState InferState m) => m a

-- | The inference monad's mutable state.
--
-- 'currentSubst' is the accumulated type-level substitution produced by every
-- unification run so far in the current inference pass. It is threaded
-- implicitly rather than being passed around as a return value. The helpers
-- 'getSubst', 'extendSubst' and 'applyCurrentSubst' (defined in
-- "Infer.Substitute" to avoid a module cycle) are the primary interface.
--
-- This mirrors the "Typing Haskell in Haskell" design where the substitution
-- lives in the inference monad rather than being an explicit return value.
data InferState
  = InferState
  { extensibleRecordsToDerive :: !(Set.Set InstanceToDerive)
  , count :: !Int
  , errors :: [CompilationError]
  , warnings :: [CompilationWarning]
  , currentSubst :: !Substitution
  }


getErrors :: Infer [CompilationError]
getErrors = gets errors


pushError :: CompilationError -> Infer ()
pushError err = do
  s <- get
  put s { errors = err : errors s }


pushWarning :: CompilationWarning -> Infer ()
pushWarning warning = do
  s <- get
  put s { warnings = warning : warnings s }


pushExtensibleRecordToDerive :: [String] -> Infer ()
pushExtensibleRecordToDerive fieldNames = do
  s <- get
  put s { extensibleRecordsToDerive =
            Set.singleton (RecordToDerive (Set.fromList fieldNames))
            <> extensibleRecordsToDerive s
        }


-- * Substitution state (raw accessors)
--
-- These live here because they only need 'Substitution' (from "Infer.Type"),
-- not the full 'Substitutable' class. Higher-level helpers that require
-- 'apply' and 'compose' live in "Infer.Substitute".

-- | The current accumulated substitution.
getSubst :: Infer Substitution
getSubst = gets currentSubst


-- | Replace the current substitution. Prefer 'extendSubst' for everyday use;
-- this is here so callers that need to save/restore (speculative branches)
-- can do so.
putSubst :: Substitution -> Infer ()
putSubst s = do
  st <- get
  put st { currentSubst = s }


-- | Clear the current substitution back to the identity.
clearSubst :: Infer ()
clearSubst = putSubst nullSubst
