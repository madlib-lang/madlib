{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Rock
import Driver.Query
import Error.Warning
import Canonicalize.InstanceToDerive
import Canonicalize.CanonicalM (pushRecordToDerive)
import Infer.Type (Substitution)

type Infer a = forall m . (Rock.MonadFetch Query m, MonadIO m, MonadError CompilationError m, MonadState InferState m) => m a

data InferState
  = InferState
  { extensibleRecordsToDerive :: !(Set.Set InstanceToDerive)
  , count :: !Int
  , errors :: [CompilationError]
  , warnings :: [CompilationWarning]
  , mutatedNames :: !(Set.Set String)
  -- ^ Names that have been mutated via the := operator. Used to block
  -- generalization of these bindings (value-restriction-style). Replaces
  -- the previous __MUTATION__ pseudo-typeclass encoding.
  , currentSubst :: !Substitution
  -- ^ Active substitution. Threaded implicitly via getSubst/extSubst/unifyM.
  -- Currently coexists with the explicit-substitution-threading style — old
  -- inference functions still return their own Substitution and compose by
  -- hand. Phase 1 of the typechecker rewrite migrates them one by one to
  -- read this field instead of accumulating substitutions explicitly.
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


markMutated :: String -> Infer ()
markMutated name = modify $ \s -> s { mutatedNames = Set.insert name (mutatedNames s) }


isMutated :: String -> Infer Bool
isMutated name = gets (Set.member name . mutatedNames)


getMutatedNames :: Infer (Set.Set String)
getMutatedNames = gets mutatedNames


-- | Read the current substitution from state.
getSubst :: Infer Substitution
getSubst = gets currentSubst


-- | Replace the current substitution wholesale. Use sparingly — most callers
-- should compose via extSubst.
putSubst :: Substitution -> Infer ()
putSubst s = modify $ \st -> st { currentSubst = s }
