{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
import           Error.Context
import           Explain.Location


data CompilationError = CompilationError TypeError Context deriving(Eq, Show)

data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String
  | UnboundVariableFromNamespace String String
  | UnboundType String
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
  | CapitalizedADTTVar String String
  | NotCapitalizedADTName String
  | NotCapitalizedAliasName String
  | NotCapitalizedConstructorName String
  | TypingHasWrongKind Type Kind Kind
  | WrongAliasArgCount String Int Int
  | UnknownType String
  | WrongSpreadType String
  | FieldNotExisting String
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
  | FatalError
  | ASTHasNoPath
  | Error
  | ImportCycle [FilePath]
  deriving (Show, Eq, Ord)


limitContextArea :: Int -> CompilationError -> CompilationError
limitContextArea maxLines err = case err of
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