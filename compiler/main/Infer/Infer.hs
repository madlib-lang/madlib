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
  -- ^ Active substitution. Read with `getSubst`, composed into via `extSubst`
  -- (Infer.Substitute), and added to via `unifyM` (Infer.Unify). All
  -- inference functions read and write this field; nothing flows through
  -- return tuples.
  , discardErrors :: !Bool
  -- ^ Best-effort inference flag. When True, errors are silently caught at
  -- internal decision points so subsequent code can still produce a partial
  -- AST for IDE/error-recovery scenarios. Replaces the discardError Bool
  -- parameter that used to be threaded through every infer* function.
  -- Toggled with `withDiscardErrors`.
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


-- | Read the discard-errors flag. Used at decision points inside inference
-- functions where we used to branch on the threaded `discardError` parameter.
isDiscardingErrors :: Infer Bool
isDiscardingErrors = gets discardErrors


-- | Run an action with discardErrors set to True; restore prior value on
-- success or exception. Used by inferExps to retry a failing definition in
-- best-effort mode.
withDiscardErrors :: Infer a -> Infer a
withDiscardErrors action = do
  prev <- gets discardErrors
  modify $ \s -> s { discardErrors = True }
  let restore = modify $ \s -> s { discardErrors = prev }
  result <- action `catchError` (\err -> restore >> throwError err)
  restore
  return result


-- | Catch errors thrown by the action: push them onto the error list and
-- continue with the supplied fallback value. Used at definition boundaries so
-- one bad binding doesn't abort the rest of a file.
recover :: Infer a -> a -> Infer a
recover action fallback = catchError action $ \err -> do
  pushError err
  return fallback
