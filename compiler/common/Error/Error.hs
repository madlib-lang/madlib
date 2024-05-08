{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
import           Error.Context
import           Explain.Location


data CompilationError = CompilationError TypeError Context deriving(Eq, Ord, Show)

data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnboundUnknownTypeVariable
  | UnboundVariableFromNamespace String String
  | UnboundType String
  | UnificationError Type Type
  | BadEscapeSequence
  | EmptyChar
  | TypeAlreadyDefined String
  | ImportCollision String
  -- Pred: The instance we add
  -- Pred: The wrong predicate
  -- Pred: The predicate from the interface declaration
  | InstancePredicateError Pred Pred Pred
  | KindError (Type, Kind) (Type, Kind)
  | NoInstanceFound String [Type]
  | InterfaceAlreadyDefined String
  | InterfaceNotExisting String
  | AmbiguousType (TVar, [Pred])
  | ADTAlreadyDefined Type
  | CapitalizedADTTVar String String
  | NotCapitalizedADTName String
  | NotCapitalizedAliasName String
  | NotCapitalizedConstructorName String
  | TypingHasWrongKind Type Kind Kind
  | WrongAliasArgCount String Int Int
  | UnknownType String
  | WrongSpreadType String
  | ImportNotFound String
  | NotExported String String
  | GrammarError FilePath String
  | NameAlreadyDefined String
  | TypesHaveDifferentOrigin String String String
  | RecursiveVarAccess String
  | IllegalSkipAccess
  | NotInScope String Loc
  | SignatureTooGeneral Scheme Scheme
  | NameAlreadyExported String
  | ShouldBeTypedOrAbove String
  | ContextTooWeak [Pred]
  | DerivingAliasNotAllowed String
  | InvalidInterfaceDerived String
  | FatalError
  | ASTHasNoPath
  | Error
  | ImportCycle [FilePath]
  | NoMain
  | MainInvalidTyping
  | NotADefinition
  | ConstructorAccessBadIndex String String Int Int
  | ConstructorAccessNoConstructorFound String
  | ConstructorAccessTooManyConstructors String Int
  | MutationRestriction
  | OverloadedMutation String [Pred]
  | BadMutation
  | MutatingNotInScope String
  | InvalidLhs
  | TypeAnnotationNameMismatch String String
  | MutatingFunction String
  | MethodNameAlreadyDefined
  | NotAConstructor String
  | RecordDuplicateFields [String]
  | TestNotValid Type
  | ByteOutOfBounds String
  | ShortOutOfBounds String
  | IntOutOfBounds String
  | NegatedByte
  deriving (Show, Eq, Ord)


limitContextArea :: Int -> CompilationError -> CompilationError
limitContextArea _ err = case err of
  CompilationError _ NoContext ->
    err

  CompilationError e (Context fp (Area (Loc a l c) (Loc a' l' c'))) ->
    if l' > l + 1 then
      CompilationError e (Context fp (Area (Loc a l c) (Loc a' (l + 2) 1)))
    else
      CompilationError e (Context fp (Area (Loc a l c) (Loc a' l' c')))


getContext :: CompilationError -> Context
getContext err = case err of
  CompilationError _ ctx ->
    ctx


getPath :: CompilationError -> FilePath
getPath err = case err of
  CompilationError _ (Context path _) ->
    path