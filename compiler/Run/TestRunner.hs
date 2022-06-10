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
      astTableWithTestExports = (addTestEmptyExports <$>) <$> astTable
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

      (canTable, warnings) <- Can.canonicalizeMany dictionaryModulePath TLLVM Can.initialEnv (mainTestPath : sourcesToCompile)
      -- let (canTable, warnings) =
      --       case astTable of
      --         Right table ->
      --           Can.canonicalizeMany dictionaryModulePath TLLVM Can.initialEnv fullASTTable (mainTestPath : sourcesToCompile)

      --         Left e ->
      --           error $ ppShow e

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
          let tableWithBatchedTests = updateTestExports wishModulePath listModulePath <$> solvedTable
          if not (null errors) then do
            putStrLn $ ppShow errors
            formattedErrors <- mapM (Explain.format readFile False) errors
            let fullError = List.intercalate "\n\n\n" formattedErrors
            putStrLn fullError >> Exit.exitFailure
          else do
            let postProcessedTable = tableToCore False tableWithBatchedTests
            let renamedTable       = Rename.renameTable postProcessedTable
            let reduced            = EtaReduction.reduceTable renamedTable
            let closureConverted   = ClosureConvert.convertTable reduced
            let withTCE            = TCE.resolveTable closureConverted
            LLVM.generateTable noCache outputPath rootPath withTCE mainTestPath

            testOutput <- case DistributionSystem.buildOS of
              DistributionSystem.Windows -> do
                try $ callCommand "\".tests\\runTests\""

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


addTestEmptyExports :: AST -> AST
addTestEmptyExports ast@AST{ apath = Just apath } =
  if ".spec.mad" `List.isSuffixOf` apath then
    let exps             = aexps ast
        -- that export is needed for type checking or else we get an error that the name is not exported
        testsExport      = Source emptyArea TargetLLVM (Export (Source emptyArea TargetLLVM (Assignment "__tests__" (Source emptyArea TargetLLVM (ListConstructor [])))))
    in  ast { aexps = exps ++ [testsExport] }
  else
    ast
addTestEmptyExports _ =
  undefined


data TestAssignment
  = SingleTest String
  | BatchTest String


generateTestAssignment :: Int -> Slv.Exp -> (Slv.Exp, Maybe TestAssignment)
generateTestAssignment index exp = case exp of
  Slv.Typed qt@(_ :=>
    (TApp
      (TCon (TC "List" (Kfun Star Star)) "prelude")
      (TApp
        (TApp (TCon (TC "Wish" wishKind) wishPath) (TCon (TC "String" _) _))
        (TCon (TC "String" _) _))))
    area
    _ ->
      let assignmentName = "__t" <> show index <> "__"
      in (Slv.Typed qt area (Slv.Assignment assignmentName exp), Just (BatchTest assignmentName))

  Slv.Typed qt@(_ :=> TApp (TApp (TCon (TC "Wish" wishKind) wishPath) (TCon (TC "String" _) _)) (TCon (TC "String" _) _)) area _ ->
    let assignmentName = "__t" <> show index <> "__"
    in  (Slv.Typed qt area (Slv.Assignment assignmentName exp), Just (SingleTest assignmentName))

  -- Slv.Typed qt area (Slv.App (Slv.Typed _ _ (Slv.App (Slv.Typed _ _ (Slv.Var "test" _)) _ _)) _ _) ->
  --   let assignmentName = "__t" <> show index <> "__"
  --   in  (Slv.Typed qt area (Slv.Assignment assignmentName exp), Just (SingleTest assignmentName))

  _ ->
    (exp, Nothing)

testType :: FilePath -> Type
testType wishPath =
  TApp (TApp (TCon (TC "Wish" (Kfun Star (Kfun Star Star))) wishPath) tStr) tStr

testListType :: FilePath -> Type
testListType wishPath =
  TApp
    (TCon (TC "List" (Kfun Star Star)) "prelude")
    (testType wishPath)


addTestsToSuite :: FilePath -> Slv.Exp -> [TestAssignment] -> Slv.Exp
addTestsToSuite wishPath currentTests assignments = case assignments of
  [] ->
    currentTests

  (SingleTest name : more) ->
    let testExp =
          Slv.Typed
            ([] :=> tListOf (testListType wishPath))
            emptyArea
            (Slv.ListConstructor
              [Slv.Typed ([] :=> testListType wishPath) emptyArea (Slv.ListItem (Slv.Typed ([] :=> testListType wishPath) emptyArea (Slv.Var name False)))])
        added =
          Slv.Typed
            ([] :=> testListType wishPath)
            emptyArea
            (Slv.App
              (Slv.Typed
                ([] :=> (testListType wishPath `fn` testListType wishPath))
                emptyArea
                (Slv.App
                  (Slv.Typed ([] :=> (testListType wishPath `fn` testListType wishPath `fn` testListType wishPath)) emptyArea (Slv.Var "List.concat" False))
                  currentTests
                  False))
              testExp
              True)
    in  addTestsToSuite wishPath added more

  (BatchTest name : more) ->
    let batchTestExp =
          Slv.Typed
            ([] :=> tListOf (testListType wishPath))
            emptyArea
            (Slv.Var name False)
        added =
          Slv.Typed
            ([] :=> testListType wishPath)
            emptyArea
            (Slv.App
              (Slv.Typed
                ([] :=> (testListType wishPath `fn` testListType wishPath))
                emptyArea
                (Slv.App
                  (Slv.Typed ([] :=> (testListType wishPath `fn` testListType wishPath `fn` testListType wishPath)) emptyArea (Slv.Var "List.concat" False))
                  currentTests
                  False))
              batchTestExp
              True)
    in  addTestsToSuite wishPath added more


listImport :: FilePath -> Slv.Import
listImport listModulePath =
  Slv.Untyped emptyArea (Slv.DefaultImport (Slv.Untyped emptyArea "List") "List" listModulePath)


updateTestExports :: FilePath -> FilePath -> Slv.AST -> Slv.AST
updateTestExports wishPath listPath ast@Slv.AST{ Slv.apath = Just apath, Slv.aexps } =
  if ".spec.mad" `List.isSuffixOf` apath && not (null aexps) then
    let exps              = init aexps
        assigned          = uncurry generateTestAssignment <$> zip [0..] exps
        exps'             = fst <$> assigned
        testAssignments   = Maybe.mapMaybe snd assigned
        initialTests      = Slv.Typed ([] :=> testListType wishPath) emptyArea (Slv.ListConstructor [])
        testsExport       =
          Slv.Typed
            ([] :=> tListOf (testListType wishPath))
            emptyArea
            (Slv.Export
              (Slv.Typed
                ([] :=> tListOf (testListType wishPath))
                emptyArea
                (Slv.Assignment
                  "__tests__"
                  (addTestsToSuite wishPath initialTests testAssignments))))
    in  ast { Slv.aexps = exps' ++ [testsExport], Slv.aimports = listImport listPath : Slv.aimports ast }
  else
    ast
updateTestExports _ _ _ =
  undefined
