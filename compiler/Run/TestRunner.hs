{-# LANGUAGE NamedFieldPuns #-}
module Run.TestRunner where

import           GHC.IO                         ( )
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
import           Text.Show.Pretty
import           Parse.Madlib.AST
import           AST.Source
import qualified AST.Solved                    as Slv
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import qualified Generate.LLVM.LLVM            as LLVM
import qualified Generate.LLVM.ClosureConvert  as ClosureConvert
import qualified Generate.LLVM.Rename          as Rename
import           Infer.AST
import           Infer.Infer
import qualified Utils.PathUtils                as PathUtils
import qualified Utils.Path                     as PathUtils
import           Explain.Location
import qualified Data.Map                       as Map
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import           Control.Monad.State
import           Control.Monad.Except
import qualified Distribution.System as DistributionSystem
import qualified Explain.Format as Explain
import qualified System.Exit as Exit
import           Optimize.ToCore
import qualified Optimize.TCE as TCE
import qualified Optimize.EtaReduction as EtaReduction
import Utils.Path
import Infer.Type
import qualified Driver
import qualified Driver.Query as Query
import Run.Options
import Utils.PathUtils (defaultPathUtils)
import qualified AST.Source as Src



runTests :: String -> Bool -> Target -> IO ()
runTests entrypoint coverage target = do
  canonicalEntrypoint <- canonicalizePath entrypoint
  rootPath            <- canonicalizePath "./"
  Just listModulePath <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "List"
  Just testModulePath <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "Test"
  sourcesToCompile    <- getFilesToCompile True canonicalEntrypoint

  let testSuitePaths = filter (".spec.mad" `List.isSuffixOf`) sourcesToCompile
      mainTestPath   =
        if takeExtension canonicalEntrypoint == "" then
          joinPath [canonicalEntrypoint, "__TestMain__.mad"]
        else
          joinPath [takeDirectory canonicalEntrypoint, "__TestMain__.mad"]
      testMainAST    = generateTestMainAST mainTestPath (listModulePath, testModulePath) testSuitePaths
      outputPath     =
        if target == TLLVM then
          ".tests/runTests"
        else
          ".tests/runTests.mjs"

  state <- Driver.initialState
  Driver.setQueryResult (Driver._startedVar state) (Query.ParsedAST mainTestPath) testMainAST

  (_, warnings, errors) <- Driver.runIncrementalTask
    state
    Options
      { optPathUtils = defaultPathUtils
      , optEntrypoint = mainTestPath
      , optRootPath = rootPath
      , optOutputPath = outputPath
      , optTarget = target
      , optOptimized = False
      , optBundle = False
      , optCoverage = False
      , optGenerateDerivedInstances = True
      , optInsertInstancePlaholders = True
      }
    []
    mempty
    Driver.Don'tPrune
    (Driver.compilationTask mainTestPath)

  let rf p =
        if "__TestMain__.mad" `List.isSuffixOf` p then
          return ""
        else
          readFile p

  unless (null warnings) $ do
    formattedWarnings <- mapM (Explain.formatWarning rf False) warnings
    putStrLn $ List.intercalate "\n\n\n" formattedWarnings

  if null errors then do
    let jsExePath = computeTargetPath (takeDirectory outputPath) rootPath mainTestPath
    testOutput <- case DistributionSystem.buildOS of
      DistributionSystem.Windows ->
        if target == TLLVM then
          try $ callCommand "\".tests\\runTests\""
        else
          try $ callCommand $ "node \"" <> jsExePath <> "\""

      _ ->
        if target == TLLVM then
          try $ callCommand ".tests/runTests"
        else
          try $ callCommand $ "node " <> jsExePath

    case (testOutput :: Either IOError ()) of
      Left e ->
        error $ ppShow e

      Right _ ->
        return ()
  else do
    formattedErrors <- mapM (Explain.format rf False) errors
    putStrLn $ List.intercalate "\n\n\n" formattedErrors


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
  in  Source emptyArea TargetAll (ListItem (Source emptyArea TargetAll (TupleConstructor [
        Source emptyArea TargetAll (LStr $ "\"" <> testSuitePath <> "\""),
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
  Source emptyArea TargetAll (App (Source emptyArea TargetAll (Var "runAllTestSuites")) [testSuites])


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
