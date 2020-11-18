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
  <> value "STANDARD_OUT"
  )

parseTransform :: Parser TransformFlags
parseTransform = TransformFlags <$> parseInput <*> parseOutput <*> parseConfig

-- TODO: move this into a `--debug` flag or similar
-- main :: IO ()
-- main = smokeTest =<< execParser opts
--  where
--   opts = info (parseTransform <**> helper)
--               (fullDesc <> progDesc "madlib transform" <> header "#####")

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
