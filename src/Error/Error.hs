{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
-- import Infer.Env
import           Explain.Reason


data InferError = InferError TypeError Reason deriving(Eq, Show)

data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnificationError Type Type
  | ADTAlreadyDefined Type
  | UnknownType String
  | FieldNotExisting String
  | ImportNotFound String
  | GrammarError FilePath String
  | FatalError
  | ASTHasNoPath
  deriving (Show, Eq, Ord)

