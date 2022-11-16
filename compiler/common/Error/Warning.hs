module Error.Warning where

import           Error.Context
import           Infer.Type


data CompilationWarning = CompilationWarning WarningKind Context deriving(Eq, Ord, Show)


data WarningKind
  = UnusedImport String FilePath
  | UnusedParameter String
  | UnusedDeclaration String
  | UnusedType String
  | UnusedConstructor String
  | UnusedTopLevelDeclaration String
  | MadlibVersionMajorDiffer (Maybe String) String String
  | MadlibVersionMinorTooLow (Maybe String) String String
  -- ^ pkgName versionRequired versionUsed
  | MissingMethods [String]
  | IncompletePattern [String]
  | RedundantPattern
  | TypedHoleFound Type
  deriving(Eq, Show, Ord)


getContext :: CompilationWarning -> Context
getContext warning = case warning of
  CompilationWarning _ ctx ->
    ctx


getPath :: CompilationWarning -> FilePath
getPath err = case err of
  CompilationWarning _ (Context path _) ->
    path


isUnusedWarning :: CompilationWarning -> Bool
isUnusedWarning warning = case warning of
  CompilationWarning (UnusedImport _ _) _ ->
    True

  CompilationWarning (UnusedParameter _) _ ->
    True

  CompilationWarning (UnusedDeclaration _) _ ->
    True

  CompilationWarning (UnusedTopLevelDeclaration _) _ ->
    True

  CompilationWarning (UnusedType _) _ ->
    True

  CompilationWarning (UnusedConstructor _) _ ->
    True

  CompilationWarning RedundantPattern _ ->
    True

  _ ->
    False
