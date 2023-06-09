module Run.TestRunner where

import           System.FilePath
import           Control.Monad                  ( when )
import           System.Process
import           Control.Exception              ( try )
import           System.Environment             ( setEnv
                                                , getEnv
                                                )
import qualified System.Environment.Executable  as Executable
import           Run.Target
import           Run.Utils
import           System.Directory
import           AST.Source
import qualified AST.Solved                     as Slv
import qualified Canonicalize.AST               as Can
import qualified Canonicalize.Env               as Can
import qualified Generate.LLVM.LLVM             as LLVM
import qualified Generate.LLVM.ClosureConvert   as ClosureConvert
import qualified Generate.LLVM.Rename           as Rename
import qualified Utils.PathUtils                as PathUtils
import qualified Utils.Path                     as PathUtils
import           Explain.Location
import qualified Data.Map                       as Map
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import           Control.Monad.State
import qualified Distribution.System            as DistributionSystem
import qualified Explain.Format                 as Explain
import qualified System.Exit                    as Exit
import           Utils.Path
import           Utils.List
import qualified Driver
import qualified Driver.Query as Query
import           Run.Options
import           Utils.PathUtils (defaultPathUtils)
import qualified AST.Source as Src
import           Error.Error
import qualified MadlibDotJson.MadlibDotJson as MadlibDotJson
import           Rock (Cyclic)
import           Error.Warning

import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import Run.OptimizationLevel (OptimizationLevel)



resetCode :: String
resetCode = "\x1b[H\x1b[J"

backToTopCode :: String
backToTopCode = "\x1b[0;0H"


runTests :: String -> Target -> Bool -> Bool -> Bool -> OptimizationLevel -> IO ()
runTests entrypoint target debug watchMode coverage optLevel = do
  canonicalEntrypoint <- canonicalizePath entrypoint
  rootPath            <- canonicalizePath "./"

  let mainTestPath   =
        if takeExtension canonicalEntrypoint == "" then
          joinPath [canonicalEntrypoint, "__TestMain__.mad"]
        else
          joinPath [takeDirectory canonicalEntrypoint, "__TestMain__.mad"]

  outputPath <-
    if target == TLLVM then
      canonicalizePath ".tests/runTests"
    else
      canonicalizePath ".tests/runTests.mjs"

  state <- Driver.initialState
  let options =
        Options
          { optPathUtils = defaultPathUtils
          , optEntrypoint = mainTestPath
          , optRootPath = rootPath
          , optOutputPath = outputPath
          , optTarget = target
          , optOptimized = False
          , optBundle = False
          , optDebug = debug
          , optCoverage = coverage
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          , optMustHaveMain = True
          , optOptimizationLevel = optLevel
          }

  runTestTask watchMode state options canonicalEntrypoint []

  when watchMode $ do
    Driver.watch rootPath (runTestTask watchMode state options canonicalEntrypoint)
    return ()


runTestTask :: Bool -> Driver.State CompilationError -> Options -> FilePath -> [FilePath] -> IO ()
runTestTask watchMode state options canonicalEntrypoint invalidatedPaths = do
  Driver.recordAndPrintDuration "Tests built and run in " $ do
    let rf p =
          if "__TestMain__.mad" `List.isSuffixOf` p then
            return ""
          else
            readFile p

    when watchMode $ do
      putStr resetCode
      putStr backToTopCode
      hFlush stdout

    sourcesToCompile    <- getFilesToCompile True canonicalEntrypoint
    Just listModulePath <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "List"
    Just testModulePath <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "Test"
    let testSuitePaths = filter (".spec.mad" `List.isSuffixOf`) sourcesToCompile
        testMainAST    = generateTestMainAST (optEntrypoint options) (listModulePath, testModulePath) testSuitePaths

    when (null testSuitePaths) $ do
      putStrLn "No test found, exiting."
      Exit.exitSuccess

    Driver.setQueryResult (Driver._startedVar state) (Query.ParsedAST (optEntrypoint options)) testMainAST

    result <-
      try $ Driver.runIncrementalTask
        state
        options
        invalidatedPaths
        mempty
        Driver.Don'tPrune
        (Driver.typeCheckFileTask $ optEntrypoint options)
        :: IO (Either (Cyclic Query.Query) ((), [CompilationWarning], [CompilationError]))

    (warnings, errors) <- case result of
      Right (_, warnings, []) -> do
        unless (null warnings) $ do
          formattedWarnings <- mapM (Explain.formatWarning rf False) $ removeDuplicates warnings
          putStrLn $ List.intercalate "\n" formattedWarnings

        (_, _, _) <- Driver.runIncrementalTask
          state
          options
          invalidatedPaths
          mempty
          Driver.Don'tPrune
          (Driver.compilationTask $ optEntrypoint options)
        return ([], [])

      Right (_, warnings, errors) ->
        return (warnings, errors)

      Left _ -> do
        (_, warnings, errors) <- Driver.runIncrementalTask
          state
          options
          invalidatedPaths
          mempty
          Driver.Don'tPrune
          (Driver.detectCyleTask (optEntrypoint options))

        return (warnings, errors)

    unless (null warnings) $ do
      formattedWarnings <- mapM (Explain.formatWarning rf False) warnings
      putStrLn $ List.intercalate "\n" formattedWarnings

    if null errors then do
      if watchMode then do
        putStr resetCode
        putStr backToTopCode
        hFlush stdout
      else
        putStrLn ""

      setEnv "COVERAGE_TEXT" "ON"
      let jsExePath = computeTargetPath (optOutputPath options) (optRootPath options) (optEntrypoint options)
      testOutput <- case DistributionSystem.buildOS of
        DistributionSystem.Windows ->
          if optTarget options == TLLVM then
            try $ callCommand "\".tests\\runTests\""
          else
            try $ callCommand $ "node \"" <> jsExePath <> "\""

        _ ->
          if optTarget options == TLLVM then
            try $ callCommand ".tests/runTests"
          else
            try $ callCommand $ "node " <> jsExePath

      case (testOutput :: Either IOError ()) of
        Left _ ->
          unless watchMode Exit.exitFailure

        Right _ ->
          return ()
    else do
      formattedErrors <- mapM (Explain.formatError rf False) $ removeDuplicates errors
      putStrLn $ List.intercalate "\n\n\n" formattedErrors
      unless watchMode Exit.exitFailure

  when watchMode $ putStrLn "\nWatching... (press ctrl-C to quit)"


generateTestSuiteName :: Int -> String
generateTestSuiteName index =
  "__TestSuite" ++ show index ++ "__"

generateTestSuiteImport :: (Int, FilePath) -> Import
generateTestSuiteImport (index, path) =
  let importName = generateTestSuiteName index
  in  Source emptyArea TargetAll (DefaultImport (Source emptyArea TargetAll importName) path path)


generateTestSuiteItemExp :: Int -> FilePath -> ListItem
generateTestSuiteItemExp index testSuitePath =
  let testsAccess = Source emptyArea TargetAll (Access (Source emptyArea TargetAll (Var $ generateTestSuiteName index)) (Source emptyArea TargetAll (Var ".__tests__")))
      beforeAll = Source emptyArea TargetAll (Access (Source emptyArea TargetAll (Var $ generateTestSuiteName index)) (Source emptyArea TargetAll (Var ".beforeAll")))
      afterAll = Source emptyArea TargetAll (Access (Source emptyArea TargetAll (Var $ generateTestSuiteName index)) (Source emptyArea TargetAll (Var ".afterAll")))
  in  Source emptyArea TargetAll (ListItem (Source emptyArea TargetAll (TupleConstructor [
        Source emptyArea TargetAll (LStr $ "\"" <> testSuitePath <> "\""),
        beforeAll,
        afterAll,
        testsAccess
      ])))

generateTestSuiteListExp :: [ListItem] -> Exp
generateTestSuiteListExp items = Source emptyArea TargetAll (ListConstructor items)

generateStaticTestMainImports :: (FilePath, FilePath) -> [Import]
generateStaticTestMainImports (listModulePath, testModulePath) =
  let listImports = Source emptyArea TargetAll (NamedImport [] "List" listModulePath)
      testImports = Source emptyArea TargetAll (NamedImport [Source emptyArea TargetAll "runAllTestSuites"] "Test" testModulePath)
  in  [listImports, testImports]


generateRunTestSuitesExp :: Exp -> Exp
generateRunTestSuitesExp testSuites =
  Source emptyArea TargetAll (
    Assignment
      "main"
      (
        Source emptyArea TargetAll (Abs [Source emptyArea TargetAll "_"] [
          Source emptyArea TargetAll (App (Source emptyArea TargetAll (Var "runAllTestSuites")) [testSuites]),
          Source emptyArea TargetAll LUnit
        ])
      )
  )

generateTestMainAST :: FilePath -> (FilePath, FilePath) -> [FilePath] -> AST
generateTestMainAST testMainPath preludeModulePaths suitePaths =
  let indexedSuitePaths = zip [0..] suitePaths
      imports           = generateTestSuiteImport <$> indexedSuitePaths
      preludeImports    = generateStaticTestMainImports preludeModulePaths
      testSuiteItems    = uncurry generateTestSuiteItemExp <$> indexedSuitePaths
      testSuiteList     = generateTestSuiteListExp testSuiteItems
      runAllTestSuites  = generateRunTestSuitesExp testSuiteList
  in  AST
        { aimports    = preludeImports ++ imports
        , aexps       = [runAllTestSuites]
        , atypedecls  = []
        , ainterfaces = []
        , ainstances  = []
        , apath       = Just testMainPath
        }
