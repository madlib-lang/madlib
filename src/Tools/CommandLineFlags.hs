module Tools.CommandLineFlags where

data Command
  = Compile
      { compileInput :: FilePath
      , compileOutput :: FilePath
      , compileConfig :: FilePath
      , compileVerbose :: Bool
      , compileDebug :: Bool
      , compileBundle :: Bool
      }
  | Test { testInput :: FilePath }
  | Install
  deriving (Eq, Show)
