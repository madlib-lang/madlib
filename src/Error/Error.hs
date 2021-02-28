{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
import           Explain.Location
import qualified AST.Canonical                 as Can


type Backtrace = [BTNode]

data BTNode
  = BTExp Can.Exp
  | BTInstance Can.Instance
  deriving(Eq, Show)

data Context
  = NoContext
  | Context { ctxAstPath :: FilePath, ctxArea :: Area, ctxBacktrace :: Backtrace }
  deriving(Eq, Show)

getCtxArea :: Context -> Maybe Area
getCtxArea ctx = case ctx of
  NoContext        -> Nothing
  Context _ area _ -> Just area

getCtxPath :: Context -> Maybe FilePath
getCtxPath ctx = case ctx of
  NoContext        -> Nothing
  Context path _ _ -> Just path


data InferError = InferError TypeError Context deriving(Eq, Show)

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
  | GrammarError FilePath String
  | NameAlreadyDefined String
  | SignatureTooGeneral Scheme Scheme
  | ContextTooWeak
  | FatalError
  | ASTHasNoPath
  | Error
  | ImportCycle [FilePath]
  deriving (Show, Eq, Ord)

