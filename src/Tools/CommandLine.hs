module Tools.CommandLine where

import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Exit                    ( exitFailure )
import           Data.Version
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text                     as T

import           Constants.Flags
import           Paths_madlib                  as Lib

hashBar = "################################################"
h1 = " ____    __  ____   _____   ____    ____  ______"
h2 = "|    \\  /  ||    \\  |    \\ |    |  |    ||      )"
h3 = "|     \\/   ||     \\ |     \\|    |_ |    ||     <"
h4 = "|__/\\__/|__||__|\\__\\|_____/|______||____||______)"
headerString = h1 ++ "\n" ++ h2 ++ "\n" ++ h3 ++ "\n" ++ h4

-- -- parseCommand :: Parser Command
-- -- parseCommand = subparser $ command "transform" parseTransform
-- -- parseCommand = subparser
--   -- $ command "transform" (parseTransform `withInfo` "Transform source code")

-- parseOptions :: Parser Options
-- parseOptions = Options <*> parseInput

-- run :: Options -> IO ()
-- run (Options command) = do
--   case command of
--     Transform i o c l -> (return ())
--  where
--   printit :: Show a => (Command -> a) -> Maybe Command -> IO ()
--   printit acc = maybe (err "fucked up") (print . acc)

-- main :: IO ()
-- main = run =<< execParser (parseOptions `withInfo` "Cool!")

data TransformFlags = TransformFlags
  { input      :: FlagInput
  , output     :: FlagOutput
  , config     :: FlagConfig
  -- , stdin      :: Bool
  -- , literate   :: Literate
  }

parseConfig :: Parser FlagConfig
parseConfig = FileConfig <$> strOption
  (  long "config"
  <> short 'c'
  <> metavar "CONFIG"
  <> help "What config to use"
  <> showDefault
  <> value "madlib.yaml"
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
  <> value "STANDARD_OUT"
  )

parseTransform :: Parser TransformFlags
parseTransform = TransformFlags <$> parseInput <*> parseOutput <*> parseConfig

-- main :: IO ()
-- main = smokeTest =<< execParser opts
--  where
--   opts = info (parseTransform <**> helper)
--               (fullDesc <> progDesc "madlib transform" <> header hashBar)

-- smokeTest :: TransformFlags -> IO ()
-- smokeTest (TransformFlags i o c) =
--   putStrLn
--     $  headerString
--     ++ "\ninput: "
--     ++ show i
--     ++ ",\noutput: "
--     ++ show o
--     ++ ",\nconfig: "
--     ++ show c
-- smokeTest _ = return ()
