module Run.PGOMode where

data PGOMode
  = NoPGO
  | PGOInstrument
  | PGOOptimize FilePath
  deriving (Eq, Show)
