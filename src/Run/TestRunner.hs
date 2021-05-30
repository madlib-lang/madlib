module Run.TestRunner where

import           GHC.IO                         ( )
import           System.FilePath                ( takeDirectory )
import           Control.Monad                  ( when )
import           System.Process
import           Control.Exception              ( try )
import           System.Environment             ( setEnv
                                                , getEnv
                                                )
import           System.Environment.Executable  ( getExecutablePath )


runTests :: String -> Bool -> IO ()
runTests entrypoint coverage = do
  executablePath        <- getExecutablePath
  testRunnerPath        <- try $ getEnv "TEST_RUNNER_PATH"
  testRunnerPathChecked <- case (testRunnerPath :: Either IOError String) of
    Left _ -> do
      return $ takeDirectory executablePath <> "/test-runner.js"
    Right p -> return p

  setEnv "MADLIB_PATH" executablePath
  when coverage $ do
    setEnv "COVERAGE_MODE" "on"
  testOutput <- try $ callCommand $ "node " <> testRunnerPathChecked <> " " <> entrypoint
  case (testOutput :: Either IOError ()) of
    Left  _ -> return ()
    Right _ -> return ()
