{-# LANGUAGE LambdaCase #-}
module Run.CommandLine where

import           Options.Applicative

import           Data.Version                   ( showVersion )
import           Paths_madlib                   ( version )
import           Text.PrettyPrint.ANSI.Leijen   ( string )
import           Run.Target
import Run.Target (Target(TLLVM))


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
  | Format { formatInput :: FilePath, formatTextInput :: String, fix :: Bool, width :: Int }
  | Run { runInput :: FilePath, runArgs :: [String] }
  | Package { packageSubCommand :: PackageSubCommand, rebuild :: Bool }
  deriving (Eq, Show)

data PackageSubCommand
  = GenerateHash { generateHashInput :: FilePath }
  | NoPackageSubCommand
  deriving (Eq, Show)


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
  (long "config" <> short 'c' <> metavar "CONFIG" <> help "What config to use" <> showDefault <> value "madlib.json")

parseInput :: Parser FilePath
parseInput = strOption (long "input" <> short 'i' <> metavar "INPUT" <> help "What source to compile")

parseOutput :: Parser FilePath
parseOutput = strOption
  (long "output" <> short 'o' <> metavar "OUTPUT" <> help "What path to compile to" <> showDefault <> value "./build/")

parseVerbose :: Parser Bool
parseVerbose = switch (long "verbose" <> short 'v' <> help "Verbose output" <> showDefault)

parseDebug :: Parser Bool
parseDebug = switch (long "debug" <> short 'd' <> help "Print AST info" <> showDefault)

parseBundle :: Parser Bool
parseBundle = switch (long "bundle" <> short 'b' <> help "Bundle the compile js in one file" <> showDefault)

parseOptimize :: Parser Bool
parseOptimize = switch (long "optimize" <> help "Optimize the output to generate smaller js files" <> showDefault)

parseJson :: Parser Bool
parseJson = switch (long "json" <> help "compiles to a JSON ast with types" <> showDefault)

parseTestFilesOnly :: Parser Bool
parseTestFilesOnly =
  switch (long "test-files-only" <> help "compiles only test files when compiling a path" <> showDefault)


parseTargetOption :: ReadM Target
parseTargetOption = eitherReader $ \case
  "node"    -> Right TNode
  "browser" -> Right TBrowser
  "llvm"    -> Right TLLVM
  s         -> Left $ "'" <> s <> "' is not a valid target option, possible values are 'browser' or 'node'."

parseTarget :: Parser Target
parseTarget = option
  parseTargetOption
  (  long "target"
  <> short 't'
  <> metavar "TARGET"
  <> help "What target it should compile to, possible values are: browser or node"
  <> showDefault
  <> value TNode
  )


parseInstall :: Parser Command
parseInstall = pure Install


parseGenerateHashInput :: Parser FilePath
parseGenerateHashInput =
  strOption (long "input" <> short 'i' <> metavar "INPUT" <> help "Path to package" <> showDefault <> value ".")

parseGenerateHash :: Parser PackageSubCommand
parseGenerateHash = subparser $ command
  "generate-hash"
  ((GenerateHash <$> parseGenerateHashInput) `withInfo` "generates the md5 hash for a package")
  <> internal

parseRebuild :: Parser Bool
parseRebuild = switch
  (  long "rebuild"
  <> short 'r'
  <> help "Rebuilds a package for an already built version and only bumps if there's a bigger change than the initial one"
  <> showDefault
  )

parsePackage :: Parser Command
parsePackage =
    (Package <$> parseGenerateHash <*> pure False)
    <|> Package NoPackageSubCommand <$> parseRebuild



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
    <*> parseTarget
    <*> parseJson
    <*> parseTestFilesOnly

parseCoverage :: Parser Bool
parseCoverage = switch
  (  long "coverage"
  <> short 'c'
  <> help "Runs tests with coverage report and saves the report in .coverage/lcov.info"
  <> showDefault
  )

parseTestInput :: Parser FilePath
parseTestInput =
  strOption (long "input" <> short 'i' <> metavar "INPUT" <> help "What to test" <> showDefault <> value ".")

parseTest :: Parser Command
parseTest = Test <$> parseTestInput <*> parseCoverage


parseRunInput :: Parser FilePath
parseRunInput = argument str (metavar "PROGRAM" <> help "Package or module to run")

parseRunArguments :: Parser [String]
parseRunArguments = many
  (argument
    str
    (  metavar "ARGS"
    <> help "Arguments and options to pass to the program, note that to pass flags you should use '--' before"
    )
  )

parseRun :: Parser Command
parseRun = Run <$> parseRunInput <*> parseRunArguments


parseFolder :: Parser FilePath
parseFolder = strArgument (metavar "FOLDER" <> help "Folder where to create the new project")

parseNew :: Parser Command
parseNew = New <$> parseFolder

parseDocInput :: Parser FilePath
parseDocInput = strOption
  (long "input" <> short 'i' <> metavar "INPUT" <> help "What source(s) you want to generate documentation for")

parseDoc :: Parser Command
parseDoc = Doc <$> parseDocInput

parseWidth :: Parser Int
parseWidth = option auto
  (  long "width"
  <> value 100
  <> metavar "WIDTH"
  <> help "target width of document"
  <> showDefault
  )

parseFix :: Parser Bool
parseFix = switch
  (  long "fix"
  <> help "Applies the new formatting to the file"
  <> showDefault
  )

parseFormatInput :: Parser FilePath
parseFormatInput = strOption
  (  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> value "./src"
  <> help "What source(s) you want to format"
  )

parseFormatTextInput :: Parser String
parseFormatTextInput = strOption
  (  long "text"
  <> value "--EMPTY--"
  <> metavar "CODE"
  <> help "Code you want to format"
  )

parseFormat :: Parser Command
parseFormat = Format <$> parseFormatInput <*> parseFormatTextInput <*> parseFix <*> parseWidth

parseCommand :: Parser Command
parseCommand =
  subparser
    $  command "compile" (parseCompile `withInfo` "compile madlib code to js")
    <> command "run"     (parseRun `withInfo` "run a madlib module or package")
    <> command "test"    (parseTest `withInfo` "test tools")
    <> command "install" (parseInstall `withInfo` "install madlib packages")
    <> command "package" (parsePackage `withInfo` "packages a library")
    <> command "new"     (parseNew `withInfo` "create a new project")
    <> command "doc"     (parseDoc `withInfo` "generate documentation")
    <> command "format"  (parseFormat `withInfo` "format code")

parseTransform :: Parser Command
parseTransform = parseCommand

parseVersion :: Parser (a -> a)
parseVersion = infoOption formattedVersion (long "version" <> short 'v' <> help "Show version" <> hidden)

formattedVersion :: String
formattedVersion = "madlib@" <> showVersion version

opts = info (parseTransform <**> helper <**> parseVersion)
            (fullDesc <> headerDoc (Just $ string (unlines [hashBar, madlibAscii])) <> progDesc formattedVersion)

runCommandParser :: IO Command
runCommandParser = execParser opts
