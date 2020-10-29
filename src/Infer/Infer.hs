{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Infer where

import           Control.Monad.Except
import           Control.Monad.State
import           Infer.Type

data InferError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnificationError Type Type
  | ADTAlreadyDefined Type
  | UnknownType String
  | FieldNotExisting String
  | FieldNotInitialized String
  | ImportNotFound String String
  | FatalError
  | ASTHasNoPath
  deriving (Show, Eq, Ord)


newtype Unique = Unique { count :: Int }
  deriving (Show, Eq, Ord)


type Infer a = forall m . (MonadError InferError m, MonadState Unique m) => m a
