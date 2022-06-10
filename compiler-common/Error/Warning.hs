module Error.Warning where

import           Error.Context
import           Error.Backtrace

data CompilationWarning = CompilationWarning WarningKind Context deriving(Eq, Show)

data WarningKind
  = UnusedImport String FilePath
  | MadlibVersionMajorDiffer (Maybe String) String String
  | MadlibVersionMinorTooLow (Maybe String) String String
  -- ^ pkgName versionRequired versionUsed
  deriving(Eq, Show, Ord)
