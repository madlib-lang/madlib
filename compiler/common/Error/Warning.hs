module Error.Warning where

import           Error.Context


data CompilationWarning = CompilationWarning WarningKind Context deriving(Eq, Show)


data WarningKind
  = UnusedImport String FilePath
  | MadlibVersionMajorDiffer (Maybe String) String String
  | MadlibVersionMinorTooLow (Maybe String) String String
  -- ^ pkgName versionRequired versionUsed
  deriving(Eq, Show, Ord)


getContext :: CompilationWarning -> Context
getContext warning = case warning of
  CompilationWarning _ ctx ->
    ctx


getPath :: CompilationWarning -> FilePath
getPath err = case err of
  CompilationWarning _ (Context path _) ->
    path


isUnusedImport :: CompilationWarning -> Bool
isUnusedImport warning = case warning of
  CompilationWarning (UnusedImport _ _) _ ->
    True

  _ ->
    False
