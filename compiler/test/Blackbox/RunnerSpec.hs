module Blackbox.RunnerSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec, shouldBe, beforeAll_, before
                                                )
import           System.Directory
import           GHC.IO                         (unsafePerformIO)
import           Control.Monad
import           System.FilePath (joinPath, takeFileName, splitPath)
import           Run.Options
import qualified Utils.PathUtils as Path
import           Run.Target
import           Run.OptimizationLevel
import qualified Driver
import           Control.Exception (try)
import           System.Process
import           System.Exit (ExitCode)
import           Text.Show.Pretty (ppShow)
import qualified Explain.Format as Explain
import qualified Data.List as List
import           Error.Warning (CompilationWarning)
import           Error.Error (CompilationError (CompilationError), TypeError (ImportCycle))
import           Driver.Query (Query)
import           Rock (Cyclic)
import           Driver (Prune(..))
import           Error.Context (Context(ctxAstPath))
import Debug.Trace
import Data.List (isInfixOf)
import System.Environment (setEnv)


printInfo :: IO ()
printInfo = do
  dir <- getCurrentDirectory
  putStrLn $ "DIR: " <> dir


sanitizeExpected :: String -> String
sanitizeExpected s = read $ "\"" <> s <> "\""


compileAndRun :: FilePath -> IO (String, String)
compileAndRun casePath = do
  setEnv "NO_COLOR" "true"
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
          , optDebug = False
          , optCoverage = False
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          , optParseOnly = False
          , optMustHaveMain = True
          , optOptimizationLevel = O3
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


sanitizeError :: CompilationError -> CompilationError
sanitizeError err = case err of
  CompilationError (ImportCycle paths) ctx ->
    let updatedPaths = takeFileName <$> paths
    in  CompilationError (ImportCycle updatedPaths) ctx { ctxAstPath = joinPath $ dropWhile (not . ("compiler" `isInfixOf`)) $ splitPath $ ctxAstPath ctx }

  or ->
    or


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
        , "compiler/test/Blackbox/test-cases/import-cycle"
        , "compiler/test/Blackbox/test-cases/operators"
        , "compiler/test/Blackbox/test-cases/recursion-from-closure"
        , "compiler/test/Blackbox/test-cases/record-instances"
        , "compiler/test/Blackbox/test-cases/record-instance-not-found"
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

  formattedErrors   <- mapM (Explain.formatError readFile False) (sanitizeError <$> errors)
  -- We drop the first line for now as it contains paths that are system-dependent
  let formattedErrors' = unlines . drop 1 . lines <$> formattedErrors
  let ppErrors =
        if null errors then
          ""
        else
          List.intercalate "\n\n\n" formattedErrors' <> "\n"

  return $ ppWarnings ++ ppErrors
