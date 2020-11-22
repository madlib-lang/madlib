module Tools.CommandLine where

import           Options.Applicative

import           Tools.CommandLineFlags

hashBar = "################################################"
h1 = " ____    __  ____   _____   ____    ____  ______"
h2 = "|    \\  /  ||    \\  |    \\ |    |  |    ||      )"
h3 = "|     \\/   ||     \\ |     \\|    |_ |    ||     <"
h4 = "|__/\\__/|__||__|\\__\\|_____/|______||____||______)"
madlibAscii = h1 ++ "\n" ++ h2 ++ "\n" ++ h3 ++ "\n" ++ h4

data TransformFlags = TransformFlags
  { input      :: FlagInput
  , output     :: FlagOutput
  , config     :: FlagConfig
  }

parseConfig :: Parser FlagConfig
parseConfig = FileConfig <$> strOption
  (  long "config"
  <> short 'c'
  <> metavar "CONFIG"
  <> help "What config to use"
  <> showDefault
  <> value "madlib.json"
  )

fileInput :: Parser FlagInput
fileInput = FileInput <$> strOption
  (long "input" <> short 'i' <> metavar "INPUT" <> help "What source to compile"
  )

stdInput :: Parser FlagInput
stdInput = flag' StdInput (long "stdin" <> short 's' <> help "Read from stdin")

parseInput :: Parser FlagInput
parseInput = fileInput <|> stdInput

parseOutput :: Parser FlagOutput
parseOutput = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "What path to compile to"
  <> showDefault
  <> value "./build"
  )

parseTransform :: Parser TransformFlags
parseTransform = TransformFlags <$> parseInput <*> parseOutput <*> parseConfig

