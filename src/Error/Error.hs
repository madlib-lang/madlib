{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
import           Explain.Reason


data InferError = InferError TypeError Reason deriving(Eq, Show)

data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnificationError Type Type
  -- Pred: The instance we add
  -- Pred: The wrong predicate
  -- Pred: The predicate from the interface declaration
  | InstancePredicateError Pred Pred Pred
  | KindError (Type, Kind) (Type, Kind)
  | NoInstanceFound String Type
  | InterfaceAlreadyDefined String
  | InterfaceNotExisting String
  | MethodDoesNotMatchInterfaceType Type Type
  | AmbiguousType (TVar, [Pred])
  | ADTAlreadyDefined Type
  | UnknownType String
  | WrongSpreadType String
  | FieldNotExisting String
  | ImportNotFound String
  | GrammarError FilePath String
  | NameAlreadyDefined String
  | SignatureTooGeneral Scheme Scheme
  | ContextTooWeak
  | FatalError
  | ASTHasNoPath
  | Error
  deriving (Show, Eq, Ord)

