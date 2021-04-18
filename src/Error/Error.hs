{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
import           Error.Context
import           Error.Backtrace


data CompilationError = CompilationError TypeError Context deriving(Eq, Show)

data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnificationError Type Type
  -- Pred: The instance we add
  -- Pred: The wrong predicate
  -- Pred: The predicate from the interface declaration
  | InstancePredicateError Pred Pred Pred
  | KindError (Type, Kind) (Type, Kind)
  | NoInstanceFound String [Type]
  | InterfaceAlreadyDefined String
  | InterfaceNotExisting String
  | MethodDoesNotMatchInterfaceType Type Type
  | AmbiguousType (TVar, [Pred])
  | ADTAlreadyDefined Type
  | UnknownType String
  | WrongSpreadType String
  | FieldNotExisting String
  | ImportNotFound String
  | NotExported String String
  | GrammarError FilePath String
  | NameAlreadyDefined String
  | SignatureTooGeneral Scheme Scheme
  | NameAlreadyExported String
  | ContextTooWeak
  | FatalError
  | ASTHasNoPath
  | Error
  | ImportCycle [FilePath]
  deriving (Show, Eq, Ord)

