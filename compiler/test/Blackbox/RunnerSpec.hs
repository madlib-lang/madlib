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


llvmCompileAndRun :: FilePath -> IO (String, String)
llvmCompileAndRun casePath = do
  setEnv "NO_COLOR" "true"
  expected <- sanitizeExpected <$> readFile (joinPath [casePath, "expected-llvm"])
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
          , optLspMode = False
          , optEmitLLVM = False
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


llvmCompileAndRunWithCoverage :: FilePath -> IO (String, String)
llvmCompileAndRunWithCoverage casePath = do
  setEnv "NO_COLOR" "true"
  expected <- sanitizeExpected <$> readFile (joinPath [casePath, "expected-llvm"])
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
          , optCoverage = True
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          , optParseOnly = False
          , optMustHaveMain = True
          , optOptimizationLevel = O3
          , optLspMode = False
          , optEmitLLVM = False
          }

  state <- Driver.initialState
  errorsAndWarnings <- compile state options [entrypoint]

  errorsOnly <- compileIgnoreWarnings state options [entrypoint]

  if errorsOnly == "" then do
    runResult <- try $ readProcessWithExitCode outputPath [] ""
    callCommand $ "rm -r " <> outputFolder
    case (runResult :: Either IOError (ExitCode, String, String)) of
        Right (_, result, _) ->
          return (expected, result)

        Left e ->
          return (ppShow e, "")
  else
    return (expected, errorsOnly)


jsCompileAndRun :: FilePath -> IO (String, String)
jsCompileAndRun casePath = do
  setEnv "NO_COLOR" "true"
  expected <- sanitizeExpected <$> readFile (joinPath [casePath, "expected-js"])
  let entrypoint   = joinPath [casePath, "Entrypoint.mad"]
  let outputPath   = joinPath [casePath, ".tests/Entrypoint.mjs"]
  let runPath      = joinPath [casePath, ".tests", casePath, "Entrypoint.mjs"]
  let outputFolder = joinPath [casePath, ".tests"]

  let options = Options
          { optPathUtils = Path.defaultPathUtils
          , optEntrypoint = entrypoint
          , optRootPath = casePath
          , optOutputPath = outputPath
          , optTarget = TNode
          , optOptimized = False
          , optBundle = False
          , optDebug = False
          , optCoverage = False
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          , optParseOnly = False
          , optMustHaveMain = True
          , optOptimizationLevel = O3
          , optLspMode = False
          , optEmitLLVM = False
          }

  state <- Driver.initialState
  errorsAndWarnings <- compile state options [entrypoint]

  if errorsAndWarnings == "" then do
    runResult <- try $ readProcessWithExitCode "node" [outputPath] ""
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
        , "compiler/test/Blackbox/test-cases/string-and-char-literals"
        , "compiler/test/Blackbox/test-cases/derive-comparable"
        , "compiler/test/Blackbox/test-cases/number-inference-error"
        , "compiler/test/Blackbox/test-cases/while"
        , "compiler/test/Blackbox/test-cases/arrays-and-mutations"
        , "compiler/test/Blackbox/test-cases/type-import-collision"
        , "compiler/test/Blackbox/test-cases/import-collision"
        , "compiler/test/Blackbox/test-cases/redefined-type"
        , "compiler/test/Blackbox/test-cases/record-rest-pattern"
        , "compiler/test/Blackbox/test-cases/pattern-safe-mutation"
        , "compiler/test/Blackbox/test-cases/pattern-mutation-error"
        , "compiler/test/Blackbox/test-cases/dictionary-operations"
        , "compiler/test/Blackbox/test-cases/pipe-operator"
        , "compiler/test/Blackbox/test-cases/closures-and-higher-order"
        , "compiler/test/Blackbox/test-cases/type-aliases"
        , "compiler/test/Blackbox/test-cases/do-notation"
        , "compiler/test/Blackbox/test-cases/template-strings"
        , "compiler/test/Blackbox/test-cases/tuples"
        , "compiler/test/Blackbox/test-cases/pattern-matching-advanced"
        , "compiler/test/Blackbox/test-cases/record-spread-update"
        , "compiler/test/Blackbox/test-cases/list-operations"
        , "compiler/test/Blackbox/test-cases/extern-ffi"
        , "compiler/test/Blackbox/test-cases/derived-instances-extended"
        , "compiler/test/Blackbox/test-cases/scope-and-shadowing"
        , "compiler/test/Blackbox/test-cases/if-else-chains"
        , "compiler/test/Blackbox/test-cases/custom-type-classes"
        , "compiler/test/Blackbox/test-cases/multi-module-happy-path"
        , "compiler/test/Blackbox/test-cases/export-closure-converted"
        , "compiler/test/Blackbox/test-cases/mutation-patterns"
        , "compiler/test/Blackbox/test-cases/closure-rest-pattern"
        , "compiler/test/Blackbox/test-cases/optimization-edge-cases"
        , "compiler/test/Blackbox/test-cases/simplifycalls-pipe-order"
        , "compiler/test/Blackbox/test-cases/simplifycalls-shadowed-repeat"
        , "compiler/test/Blackbox/test-cases/simplifycalls-length-repeat-effects"
        , "compiler/test/Blackbox/test-cases/simplifycalls-nth-repeat-oob-effects"
        , "compiler/test/Blackbox/test-cases/simplifycalls-repeat-count-capture"
        , "compiler/test/Blackbox/test-cases/simplifycalls-repeat-item-capture"
        , "compiler/test/Blackbox/test-cases/simplifycalls-repeat-binding-retain"
        , "compiler/test/Blackbox/test-cases/simplifycalls-nth-repeat-after-call-mutation"
        , "compiler/test/Blackbox/test-cases/simplifycalls-nth-repeat-after-if-mutation"
        , "compiler/test/Blackbox/test-cases/simplifycalls-nth-repeat-after-while-mutation"
        , "compiler/test/Blackbox/test-cases/foldcalls-side-effect-order"
        , "compiler/test/Blackbox/test-cases/foldcalls-branch-effects"
        , "compiler/test/Blackbox/test-cases/foldcalls-closure-constructor-effects"
        , "compiler/test/Blackbox/test-cases/foldcalls-capture-time"
        , "compiler/test/Blackbox/test-cases/pap-arity-31"
        , "compiler/test/Blackbox/test-cases/pap-arity-50"
        , "compiler/test/Blackbox/test-cases/pap-over-under-chain"
        , "compiler/test/Blackbox/test-cases/pap-overapply-returned-closure"
        , "compiler/test/Blackbox/test-cases/pap-overapply-order"
        , "compiler/test/Blackbox/test-cases/inline-duplicates-arg-effects"
        , "compiler/test/Blackbox/test-cases/inline-drops-unused-arg-effects"
        , "compiler/test/Blackbox/test-cases/tco-advanced"
        , "compiler/test/Blackbox/test-cases/higher-order-inlining"
        , "compiler/test/Blackbox/test-cases/bool-precedence"
        , "compiler/test/Blackbox/test-cases/do-in-branches"
        , "compiler/test/Blackbox/test-cases/byte-scan-hex-parity"
        , "compiler/test/Blackbox/test-cases/scan-strict-parity"
        , "compiler/test/Blackbox/test-cases/short-overflow-parity"
        , "compiler/test/Blackbox/test-cases/inplace-trmc"
        , "compiler/test/Blackbox/test-cases/trmc-two-item-branch"
        , "compiler/test/Blackbox/test-cases/maybe-operators"
        , "compiler/test/Blackbox/test-cases/jsx-maybe-props"
        , "compiler/test/Blackbox/test-cases/jsx-children"
        , "compiler/test/Blackbox/test-cases/signature-too-general"
        , "compiler/test/Blackbox/test-cases/inline-mutation-double"
        , "compiler/test/Blackbox/test-cases/inplace-dispatch-shared"
        , "compiler/test/Blackbox/test-cases/nested-lambda-shadow"
        , "compiler/test/Blackbox/test-cases/tce-mixed-recursion"
        , "compiler/test/Blackbox/test-cases/tce-addition-with-normalizing-branch"
        , "compiler/test/Blackbox/test-cases/tce-boolean-left-recursion"
        , "compiler/test/Blackbox/test-cases/tce-float-product"
        , "compiler/test/Blackbox/test-cases/tce-double-recursion-not-optimized"
        , "compiler/test/Blackbox/test-cases/tce-addition-with-do"
        , "compiler/test/Blackbox/test-cases/tce-addition-in-where"
        , "compiler/test/Blackbox/test-cases/tce-addition-with-plain-branch"
        , "compiler/test/Blackbox/test-cases/tce-addition-branch-order"
        , "compiler/test/Blackbox/test-cases/tce-multiplication-with-normalizing-branch"
        , "compiler/test/Blackbox/test-cases/trmc-concat-empty"
        , "compiler/test/Blackbox/test-cases/hocp-pattern-var-shadow"
        , "compiler/test/Blackbox/test-cases/hocp-where-pattern-shadows-hof"
        ]

  forM_ cases $ \casePath -> do
    before (llvmCompileAndRun casePath) $ describe "" $ do
      it ("llvm case: " <> casePath) $ \(expected, result) -> do
        result `shouldBe` expected

  forM_ cases $ \casePath -> do
    before (jsCompileAndRun casePath) $ describe "" $ do
      it ("js case: " <> casePath) $ \(expected, result) -> do
        result `shouldBe` expected

  let coverageCases =
        [ "compiler/test/Blackbox/test-cases/hocp-coverage-tce"
        , "compiler/test/Blackbox/test-cases/tce-addition-with-coverage"
        ]

  forM_ coverageCases $ \casePath -> do
    before (llvmCompileAndRunWithCoverage casePath) $ describe "" $ do
      it ("llvm coverage case: " <> casePath) $ \(expected, result) -> do
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


compileIgnoreWarnings :: Driver.State CompilationError -> Options -> [FilePath] -> IO String
compileIgnoreWarnings state options invalidatedPaths = do
  result <-
    try $ Driver.runIncrementalTask
      state
      options
      invalidatedPaths
      mempty
      Don'tPrune
      (Driver.typeCheckFileTask $ optEntrypoint options)
      :: IO (Either (Cyclic Query) ((), [CompilationWarning], [CompilationError]))

  errors <- case result of
    Right (_, _, []) -> do
      Driver.runIncrementalTask
        state
        options
        invalidatedPaths
        mempty
        Don'tPrune
        (Driver.compilationTask $ optEntrypoint options)
      return []

    Right (_, _, errors) ->
      return errors

    Left _ -> do
      (_, _, errors) <- Driver.runIncrementalTask
        state
        options
        invalidatedPaths
        mempty
        Don'tPrune
        (Driver.detectCyleTask (optEntrypoint options))

      return errors

  formattedErrors   <- mapM (Explain.formatError readFile False) (sanitizeError <$> errors)
  let formattedErrors' = unlines . drop 1 . lines <$> formattedErrors
  let ppErrors =
        if null errors then
          ""
        else
          List.intercalate "\n\n\n" formattedErrors' <> "\n"

  return ppErrors
