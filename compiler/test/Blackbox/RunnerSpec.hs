module Blackbox.RunnerSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec, shouldBe, beforeAll_, before
                                                )
import           System.Directory
import           GHC.IO                         (unsafePerformIO)
import           Control.Monad
import           System.FilePath (joinPath)
import           Run.Options
import qualified Utils.PathUtils as Path
import Run.Target
import qualified Driver
import Run.Compile
import Control.Exception (try)
import System.Process
import System.Exit (ExitCode)
import Text.Show.Pretty (ppShow)
import qualified Explain.Format as Explain
import qualified Data.List as List
import Error.Warning (CompilationWarning)
import Error.Error (CompilationError)
import Driver.Query (Query)
import Rock (Cyclic)
import Driver (Prune(..))


printInfo :: IO ()
printInfo = do
  dir <- getCurrentDirectory
  putStrLn $ "DIR: " <> dir


-- sanitizeExpected :: String -> String
-- sanitizeExpected s = case s of
--   '\\' : '\\' : more ->
--     '\\' : sanitizeExpected more

--   a : more ->
--     a : sanitizeExpected more

--   "" ->
--     ""

sanitizeExpected :: String -> String
sanitizeExpected s = read $ "\"" <> s <> "\""


compileAndRun :: FilePath -> IO (String, String)
compileAndRun casePath = do
  expected <- sanitizeExpected <$> readFile (joinPath [casePath, "expected"])
  let entrypoint   = joinPath [casePath, "Entrypoint.mad"]
  let outputPath   = joinPath [casePath, ".tests/run"]
  let outputFolder = joinPath [casePath, ".tests"]

  let options = Options
          { optPathUtils = Path.defaultPathUtils
          , optEntrypoint = entrypoint
          , optRootPath = "./"
          , optOutputPath = outputPath
          , optTarget = TLLVM
          , optOptimized = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          }

  state <- Driver.initialState
  errorsAndWarnings <- compile state options [entrypoint]

  if errorsAndWarnings == "" then do
    runResult <- try $ readProcessWithExitCode outputPath [] ""
    callCommand $ "rm -r " <> outputFolder
    case (runResult :: Either IOError (ExitCode, String, String)) of
        Right (_, result, _) ->
          return (expected, result)

        Left e ->
          return (ppShow e, "")
  else
    return (expected, errorsAndWarnings)
spec :: Spec
spec = do
  let cases =
        [ "compiler/test/Blackbox/test-cases/hello-world"
        , "compiler/test/Blackbox/test-cases/basic-type-error"
        , "compiler/test/Blackbox/test-cases/parse-madlib-dot-json"
        , "compiler/test/Blackbox/test-cases/fibonacci"
        , "compiler/test/Blackbox/test-cases/mtl"
        , "compiler/test/Blackbox/test-cases/trmc-constructor"
        , "compiler/test/Blackbox/test-cases/unused-imports"
        -- , "compiler/test/Blackbox/test-cases/import-cycle"
        ]
  forM_ cases $ \casePath -> do
    before (compileAndRun casePath) $ describe "" $ do
      it ("case: " <> casePath) $ \(expected, result) -> do
        result `shouldBe` expected



compile :: Driver.State CompilationError -> Options -> [FilePath] -> IO String
compile state options invalidatedPaths = do
  result <-
    try $ Driver.runIncrementalTask
      state
      options
      invalidatedPaths
      mempty
      Don'tPrune
      (Driver.typeCheckFileTask $ optEntrypoint options)
      :: IO (Either (Cyclic Query) ((), [CompilationWarning], [CompilationError]))

  (warnings, errors) <- case result of
    Right (_, warnings, []) -> do
      Driver.runIncrementalTask
        state
        options
        invalidatedPaths
        mempty
        Don'tPrune
        (Driver.compilationTask $ optEntrypoint options)
      return (warnings, [])

    Right (_, warnings, errors) ->
      return (warnings, errors)

    Left _ -> do
      (_, warnings, errors) <- Driver.runIncrementalTask
        state
        options
        invalidatedPaths
        mempty
        Don'tPrune
        (Driver.detectCyleTask (optEntrypoint options))

      return (warnings, errors)

  formattedWarnings <- mapM (Explain.formatWarning readFile False) warnings
  let ppWarnings =
        if null warnings then
          ""
        else
          List.intercalate "\n\n\n" formattedWarnings <> "\n"

  formattedErrors   <- mapM (Explain.format readFile False) errors
  let ppErrors =
        if null errors then
          ""
        else
          List.intercalate "\n\n\n" formattedErrors <> "\n"

  return $ ppWarnings ++ ppErrors

-- spec :: Spec
-- spec = do
--   beforeAll_ printInfo $ describe "blackbox tests" $ do
--     let _ = unsafePerformIO $ do
--             dir <- getCurrentDirectory
--             putStrLn $ "DIR: " <> dir
--     -- let cases = unsafePerformIO $ getDirectoryContents "compiler/test/Blackbox/test-cases"
--     let cases = ["compiler/test/Blackbox/test-cases/hello-world"]
--     forM_ cases $ \casePath -> do
--       it ("case: " <> casePath) $ do
--         let expected = unsafePerformIO $ readFile (joinPath [casePath, "expected"])
--         let entrypoint = joinPath [casePath, "Entrypoint.mad"]
--         let outputPath = joinPath [casePath, ".tests/run"]
--         let options = Options
--                 { optPathUtils = Path.defaultPathUtils
--                 , optEntrypoint = entrypoint
--                 , optRootPath = "./"
--                 , optOutputPath = ".tests/blackboxed"
--                 , optTarget = TLLVM
--                 , optOptimized = False
--                 , optBundle = False
--                 , optCoverage = False
--                 , optGenerateDerivedInstances = True
--                 , optInsertInstancePlaholders = True
--                 }
--         let _ = unsafePerformIO $ do
--               state <- Driver.initialState
--               runCompilationTask state options [entrypoint]

--         let runResult = unsafePerformIO $ try $ readProcessWithExitCode outputPath [] ""
--         case (runResult :: Either IOError (ExitCode, String, String)) of
--             Right (_, result, _) ->
--               result `shouldBe` expected

--             Left _ ->
--               undefined
