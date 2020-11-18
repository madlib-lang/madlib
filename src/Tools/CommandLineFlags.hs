module Tools.CommandLineFlags where

import           Options.Applicative -- provided by optparse-applicative

data FlagInput   = FileInput FilePath
                  | StdInput
                  deriving (Show)
data FlagOutput   = FileOutput FilePath
                  | StdOutput
                  deriving (Show)
data FlagConfig   = FileConfig FilePath
                  deriving (Show)
data FlagLiterate = LiterateAll
                  | LiterateCode
                  | LiterateNoCode
                  deriving (Bounded, Enum, Show)

data Severity     = Info
                  | Warning
                  | Error
                  deriving (Bounded, Enum, Show)
