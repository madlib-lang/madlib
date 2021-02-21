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
      , compileJson :: Bool
      }
  | Test { testInput :: FilePath, coverage :: Bool }
  | Install
  | New { newFolder :: FilePath }
  deriving (Eq, Show)
