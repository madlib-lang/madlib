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


runTests :: Bool -> String -> Bool -> Target -> IO ()
runTests noCache entrypoint coverage target = case target of
  TNode ->
    runNodeTests entrypoint coverage

  TLLVM ->
    runLLVMTests noCache entrypoint coverage

  _ ->
    undefined


runNodeTests :: String -> Bool -> IO ()
runNodeTests entrypoint coverage = do
  executablePath        <- Executable.getExecutablePath
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


runLLVMTests :: Bool -> String -> Bool -> IO ()
runLLVMTests noCache entrypoint coverage = do
  canonicalEntrypoint       <- canonicalizePath entrypoint
  rootPath                  <- canonicalizePath $ PathUtils.computeRootPath entrypoint
  Just wishModulePath       <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "Wish"
  Just listModulePath       <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "List"
  Just testModulePath       <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "Test"
  sourcesToCompile          <- getFilesToCompile True canonicalEntrypoint
  astTable                  <- buildManyASTTables TLLVM mempty (listModulePath : wishModulePath : sourcesToCompile)
  Just dictionaryModulePath <- resolveAbsoluteSrcPath PathUtils.defaultPathUtils (dropFileName canonicalEntrypoint) "Dictionary"
  let outputPath              = "./.tests/runTests"
      astTableWithTestExports = (addTestExports <$>) <$> astTable
      mainTestPath            =
        if takeExtension canonicalEntrypoint == "" then
          joinPath [canonicalEntrypoint, "__TestMain__.mad"]
        else
          joinPath [takeDirectory canonicalEntrypoint, "__TestMain__.mad"]

  case astTableWithTestExports of
    Right astTable' -> do
      let testSuitePaths          = filter (".spec.mad" `List.isSuffixOf`) $ Map.keys astTable'
          testMainAST             = generateTestMainAST (wishModulePath, listModulePath, testModulePath) testSuitePaths
          fullASTTable            = Map.insert mainTestPath testMainAST { apath = Just mainTestPath } astTable'

      let (canTable, warnings) =
            case astTable of
              Right table ->
                Can.canonicalizeMany dictionaryModulePath TLLVM Can.initialEnv fullASTTable (mainTestPath : sourcesToCompile)

              Left e ->
                error $ ppShow e

      let resolvedASTTable =
            case canTable of
              Right table ->
                runExcept (runStateT (solveManyASTs mempty table (mainTestPath : sourcesToCompile)) InferState { count = 0, errors = [] })

              Left e ->
                error $ ppShow e

      case resolvedASTTable of
        Left err ->
          error $ ppShow err

        Right (solvedTable, InferState { errors }) -> do
          if not (null errors) then do
            formattedErrors <- mapM (Explain.format readFile False) errors
            let fullError = List.intercalate "\n\n\n" formattedErrors
            putStrLn fullError >> Exit.exitFailure
          else do
            let postProcessedTable = tableToCore False solvedTable
            let renamedTable       = Rename.renameTable postProcessedTable
            let reduced            = EtaReduction.reduceTable renamedTable
            let closureConverted   = ClosureConvert.convertTable reduced
            let withTCE            = TCE.resolveTable closureConverted
            LLVM.generateTable noCache outputPath rootPath withTCE mainTestPath

            testOutput <- case DistributionSystem.buildOS of
              DistributionSystem.Windows -> do
                try $ callCommand ".tests\runTests"

              _ -> do
                try $ callCommand ".tests/runTests"

            case (testOutput :: Either IOError ()) of
              Left e ->
                error $ ppShow e

              Right _ ->
                return ()

    Left _ ->
      error "asts could not be parsed"



generateTestSuiteName :: Int -> String
generateTestSuiteName index =
  "__TestSuite" ++ show index ++ "__"

generateTestSuiteImport :: (Int, FilePath) -> Import
generateTestSuiteImport (index, path) =
  let importName = generateTestSuiteName index
  in  Source emptyArea TargetLLVM (DefaultImport (Source emptyArea TargetLLVM importName) path path)


-- fulfill(identity, showResult, parallel(__TestSuite1__.__tests__))
generateRunTestSuiteExp :: Int -> FilePath -> Exp
generateRunTestSuiteExp index testSuitePath =
  let testsAccess = Source emptyArea TargetLLVM (Access (Source emptyArea TargetLLVM (Var $ generateTestSuiteName index)) (Source emptyArea TargetLLVM (Var ".__tests__")))
  in  Source emptyArea TargetLLVM (App (Source emptyArea TargetLLVM (Var "runTestSuite")) [
        Source emptyArea TargetLLVM (LStr $ "\"" <> testSuitePath <> "\""),
        testsAccess
      ])

generateTestSuiteItemExp :: Int -> FilePath -> ListItem
generateTestSuiteItemExp index testSuitePath =
  let testsAccess = Source emptyArea TargetLLVM (Access (Source emptyArea TargetLLVM (Var $ generateTestSuiteName index)) (Source emptyArea TargetLLVM (Var ".__tests__")))
  in  Source emptyArea TargetLLVM (ListItem (Source emptyArea TargetLLVM (TupleConstructor [
        Source emptyArea TargetLLVM (LStr $ "\"" <> testSuitePath <> "\""),
        testsAccess
      ])))

generateTestSuiteListExp :: [ListItem] -> Exp
generateTestSuiteListExp items = Source emptyArea TargetLLVM (ListConstructor items)

generateStaticTestMainImports :: (FilePath, FilePath, FilePath) -> [Import]
generateStaticTestMainImports (wishModulePath, listModulePath, testModulePath) =
  let wishImports = Source emptyArea TargetLLVM (NamedImport [Source emptyArea TargetLLVM "fulfill"] "Wish" wishModulePath)
      listImports = Source emptyArea TargetLLVM (NamedImport [] "List" listModulePath)
      testImports = Source emptyArea TargetLLVM (NamedImport [Source emptyArea TargetLLVM "runAllTestSuites"] "Test" testModulePath)
  in  [wishImports, listImports, testImports]

generateRunTestSuitesExp :: Exp -> Exp
generateRunTestSuitesExp testSuites =
  Source emptyArea TargetLLVM (App (Source emptyArea TargetLLVM (Var "runAllTestSuites")) [testSuites])


generateTestMainAST :: (FilePath, FilePath, FilePath) -> [FilePath] -> AST
generateTestMainAST preludeModulePaths suitePaths =
  let indexedSuitePaths = zip [0..] suitePaths
      imports           = generateTestSuiteImport <$> indexedSuitePaths
      preludeImports    = generateStaticTestMainImports preludeModulePaths
      runTestSuiteExps  = uncurry generateRunTestSuiteExp <$> indexedSuitePaths
      testSuiteItems    = uncurry generateTestSuiteItemExp <$> indexedSuitePaths
      testSuiteList     = generateTestSuiteListExp testSuiteItems
      runAllTestSuites  = generateRunTestSuitesExp testSuiteList
  in  AST
        { aimports    = preludeImports ++ imports
        , aexps       = [runAllTestSuites]
        , atypedecls  = []
        , ainterfaces = []
        , ainstances  = []
        , apath       = Nothing
        }


generateTestAssignment :: Int -> Exp -> (Exp, Maybe String)
generateTestAssignment index exp = case exp of
  Source _ _ (App (Source _ _ (Var "test")) args) ->
    let assignmentName = "__t" <> show index <> "__"
    in  (Source emptyArea TargetLLVM (Assignment assignmentName exp), Just assignmentName)

  _ ->
    (exp, Nothing)


addTestExports :: AST -> AST
addTestExports ast@AST{ apath = Just apath } =
  if ".spec.mad" `List.isSuffixOf` apath then
    let exps           = aexps ast
        assigned       = uncurry generateTestAssignment <$> zip [0..] exps
        exps'          = fst <$> assigned
        namesForExport = Maybe.mapMaybe snd assigned
        testsExport    = Source emptyArea TargetLLVM (Export (Source emptyArea TargetLLVM (Assignment "__tests__" (Source emptyArea TargetLLVM (ListConstructor (
            Source emptyArea TargetLLVM . ListItem . Source emptyArea TargetLLVM . Var <$> namesForExport
          ))))))
    in  ast { aexps = exps' ++ [testsExport] }
  else
    ast
addTestExports _ =
  undefined
