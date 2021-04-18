module Error.Warning where

import           Error.Context
import           Error.Backtrace

data CompilationWarning = CompilationWarning WarningKind Context deriving(Eq, Show)

data WarningKind
  = UnusedImport String FilePath
  deriving(Eq, Show, Ord)
