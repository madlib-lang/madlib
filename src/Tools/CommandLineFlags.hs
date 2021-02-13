module Tools.CommandLineFlags where

import           Target

data Command
  = Compile
      { compileInput :: FilePath
      , compileOutput :: FilePath
      , compileConfig :: FilePath
      , compileVerbose :: Bool
      , compileDebug :: Bool
      , compileBundle :: Bool
      , compileOptimize :: Bool
      , compileTarget :: Target
      }
  | Test { testInput :: FilePath, coverage :: Bool }
  | Install
  deriving (Eq, Show)
