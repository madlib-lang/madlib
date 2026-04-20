module Run.SourceMapMode where

data SourceMapMode
  = NoSourceMap
  | ExternalSourceMap
  | InlineSourceMap
  deriving (Eq, Show)
