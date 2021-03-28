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
      , compileTestFilesOnly :: Bool
      }
  | Test { testInput :: FilePath, coverage :: Bool }
  | Install
  | New { newFolder :: FilePath }
  | Doc { docInput :: FilePath }
  deriving (Eq, Show)
