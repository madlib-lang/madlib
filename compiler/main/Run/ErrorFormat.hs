module Run.ErrorFormat where

data ErrorFormat
  = TextFormat
  | JsonFormat
  deriving (Eq, Show)
