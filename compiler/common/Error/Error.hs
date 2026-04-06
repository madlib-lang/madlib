{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Error.Error where

import           Infer.Type
import           Error.Context
import           Explain.Location


data CompilationError = CompilationError TypeError Context deriving(Eq, Ord, Show)


-- | Extra context about the function whose argument failed type-checking.
data FunctionContext = FunctionContext
  { fcExpectedType :: Type    -- ^ expected type for this specific parameter
  , fcFullSignature :: Type   -- ^ the function's full inferred type
  , fcTotalParams :: Int      -- ^ total number of parameters
  } deriving (Show, Eq, Ord)

-- | Which branch of an if-expression has the mismatch.
data BranchSide = ThenBranch | ElseBranch
  deriving (Show, Eq, Ord)

-- | A secondary source location for multi-span error display.
data SecondaryLocation = SecondaryLocation
  { slPath :: FilePath
  , slArea :: Area
  , slMessage :: String
  } deriving (Show, Eq, Ord)

-- | Describes where an expected type originated from, for richer error messages.
data ErrorOrigin
  = FromFunctionArgument String Int (Maybe FunctionContext)
                                       -- ^ function name, argument index (1-based), optional context
  | FromFunctionReturn String          -- ^ return type of named function
  | FromOperator String                -- ^ operator like +, &&, <>
  | FromIfCondition                    -- ^ if condition must be Boolean
  | FromIfBranches BranchSide          -- ^ which branch has the mismatch
  | FromListElement Int                -- ^ 1-based index (0 = unknown)
  | FromTypeAnnotation                 -- ^ user-provided type annotation
  | FromPatternMatch Int               -- ^ 1-based branch index (0 = unknown)
  | FromWhileCondition                 -- ^ while condition must be Boolean
  | FromAssignment String              -- ^ assigning to a typed variable
  | NoOrigin
  deriving (Show, Eq, Ord)


data TypeError
  = InfiniteType TVar Type
  | UnboundVariable String [String]
  | UnboundUnknownTypeVariable
  | UnboundVariableFromNamespace String String
  | UnboundType String [String]
  | UnificationError Type Type ErrorOrigin (Maybe SecondaryLocation)
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
  | UnknownType String [String]
  | WrongSpreadType String
  | ImportNotFound String
  | NotExported String String [String]  -- ^ name, path, similar exported names
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
  | MutatingPatternBoundVariable String
  | InvalidLhs
  | TypeAnnotationNameMismatch String String
  | MutatingFunction String
  | MethodNameAlreadyDefined
  | NotAConstructor String
  | RecordDuplicateFields [String]
  | RecordDuplicateRestPattern
  | RecordMissingFields [String]
  | RecordExtraFields [String] [String]  -- ^ extra fields, available fields in expected record
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