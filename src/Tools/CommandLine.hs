module Tools.CommandLine where

import           Options.Applicative

import           Tools.CommandLineFlags
import           Data.Version                   ( showVersion )
import           Paths_madlib                   ( version )
import           Text.PrettyPrint.ANSI.Leijen   ( string )

hashBar = "################################################"
h1 = " ____    __  ____   _____   ____    ____  ______"
h2 = "|    \\  /  ||    \\  |    \\ |    |  |    ||      )"
h3 = "|     \\/   ||     \\ |     \\|    |_ |    ||     <"
h4 = "|__/\\__/|__||__|\\__\\|_____/|______||____||______)"
madlibAscii = unlines [h1, h2, h3, h4]


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseConfig :: Parser FilePath
parseConfig = strOption
  (  long "config"
  <> short 'c'
  <> metavar "CONFIG"
  <> help "What config to use"
  <> showDefault
  <> value "madlib.json"
  )

parseInput :: Parser FilePath
parseInput = strOption
  (long "input" <> short 'i' <> metavar "INPUT" <> help "What source to compile"
  )

parseOutput :: Parser FilePath
parseOutput = strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "What path to compile to"
  <> showDefault
  <> value "./build/"
  )

parseVerbose :: Parser Bool
parseVerbose =
  switch (long "verbose" <> short 'v' <> help "Verbose output" <> showDefault)

parseDebug :: Parser Bool
parseDebug =
  switch (long "debug" <> short 'd' <> help "Print AST info" <> showDefault)

parseBundle :: Parser Bool
parseBundle = switch
  (  long "bundle"
  <> short 'b'
  <> help "Bundle the compile js in one file"
  <> showDefault
  )

parseOptimize :: Parser Bool
parseOptimize = switch
  (  long "optimize"
  <> help "Optimize the output to generate smaller js files"
  <> showDefault
  )


parseInstall :: Parser Command
parseInstall = pure Install


parseCompile :: Parser Command
parseCompile =
  Compile
    <$> parseInput
    <*> parseOutput
    <*> parseConfig
    <*> parseVerbose
    <*> parseDebug
    <*> parseBundle
    <*> parseOptimize

parseCoverage :: Parser Bool
parseCoverage = switch
  (  long "coverage"
  <> short 'c'
  <> help
       "Runs tests with coverage report and saves the report in .coverage/lcov.info"
  <> showDefault
  )

parseTest :: Parser Command
parseTest = Test <$> parseInput <*> parseCoverage


parseCommand :: Parser Command
parseCommand =
  subparser
    $  command "compile" (parseCompile `withInfo` "compile madlib code to js")
    <> command "test"    (parseTest `withInfo` "test tools")
    <> command "install" (parseInstall `withInfo` "install madlib packages")

parseTransform :: Parser Command
parseTransform = parseCommand

parseVersion :: Parser (a -> a)
parseVersion = infoOption
  formattedVersion
  (long "version" <> short 'v' <> help "Show version" <> hidden)

formattedVersion :: String
formattedVersion = "madlib@" <> showVersion version

opts = info
  (parseTransform <**> helper <**> parseVersion)
  (  fullDesc
  <> headerDoc (Just $ string (unlines [hashBar, madlibAscii]))
  <> progDesc formattedVersion
  )
