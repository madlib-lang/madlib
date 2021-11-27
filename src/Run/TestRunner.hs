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
import AST.Source (AST(atypedecls))


runTests :: String -> Bool -> Target -> IO ()
runTests entrypoint coverage target = case target of
  TNode ->
    runNodeTests entrypoint coverage

  TLLVM ->
    runLLVMTests entrypoint coverage

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


runLLVMTests :: String -> Bool -> IO ()
runLLVMTests entrypoint coverage = do
  canonicalEntrypoint <- canonicalizePath entrypoint
  rootPath            <- canonicalizePath $ PathUtils.computeRootPath entrypoint
  Just wishModulePath <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "Wish"
  Just listModulePath <- PathUtils.resolveAbsoluteSrcPath PathUtils.defaultPathUtils "" "List"
  sourcesToCompile    <- getFilesToCompile True canonicalEntrypoint
  astTable            <- buildManyASTTables TLLVM mempty (listModulePath : wishModulePath : sourcesToCompile)
  let outputPath              = "./build/"
      astTableWithTestExports = (addTestExports <$>) <$> astTable
      mainTestPath            = joinPath [takeDirectory canonicalEntrypoint, "__TestMain__.mad"]

  case astTableWithTestExports of
    Right astTable' -> do
      let testSuitePaths          = filter (".spec.mad" `List.isSuffixOf`) $ Map.keys astTable'
          testMainAST             = generateTestMainAST (wishModulePath, listModulePath) testSuitePaths
          fullASTTable            = Map.insert mainTestPath testMainAST { apath = Just mainTestPath } astTable'
      putStrLn (ppShow fullASTTable)
      putStrLn (ppShow testMainAST)

      let (canTable, warnings) =
            case astTable of
              Right table ->
                Can.canonicalizeMany TLLVM Can.initialEnv fullASTTable (mainTestPath : sourcesToCompile)

              Left e ->
                (Left e, [])

      let resolvedASTTable =
            case canTable of
              Right table ->
                runExcept (runStateT (solveManyASTs mempty table (mainTestPath : sourcesToCompile)) InferState { count = 0, errors = [] })

              Left e ->
                Left e

      

      case resolvedASTTable of
        Left err ->
          error $ ppShow err

        Right (solvedTable, _) -> do
          let renamedTable     = Rename.renameTable solvedTable
          let closureConverted = ClosureConvert.optimizeTable renamedTable
          putStrLn (ppShow closureConverted)
          LLVM.generateTable outputPath rootPath closureConverted mainTestPath


    Left _ ->
      error "asts could not be parsed"



generateTestSuiteName :: Int -> String
generateTestSuiteName index =
  "__TestSuite" ++ show index ++ "__"

generateTestSuiteImport :: (Int, FilePath) -> Import
generateTestSuiteImport (index, path) =
  let importName = generateTestSuiteName index
  in  Source emptyArea (DefaultImport (Source emptyArea importName) path path)


-- fulfill(identity, showResult, parallel(__TestSuite1__.__tests__))
generateRunTestSuiteExp :: Int -> Exp
generateRunTestSuiteExp index =
  let testsAccess = Source emptyArea (Access (Source emptyArea (Var $ generateTestSuiteName index)) (Source emptyArea (Var ".__tests__")))
  in  Source emptyArea (App (Source emptyArea (Var "map")) [
        Source emptyArea (App (Source emptyArea (Var "fulfill")) [
          Source emptyArea (Abs [Source emptyArea "a"] [Source emptyArea (Var "a")]),
          Source emptyArea (Abs [Source emptyArea "a"] [Source emptyArea (Var "a")])
        -- Source emptyArea (Var "showResult"),
        ]),
        testsAccess
      ])
  -- in  Source emptyArea (App (Source emptyArea (Var "fulfill")) [
  --       Source emptyArea (Abs [Source emptyArea "a"] [Source emptyArea (Var "a")]),
  --       Source emptyArea (Abs [Source emptyArea "a"] [Source emptyArea (Var "a")]),
  --       -- Source emptyArea (Var "showResult"),
  --       testsAccess
  --     ])

generateStaticTestMainImports :: (FilePath, FilePath) -> [Import]
generateStaticTestMainImports (wishModulePath, listModulePath) =
  let wishImports = Source emptyArea (NamedImport [Source emptyArea "fulfill"] "Wish" wishModulePath)
      listImports = Source emptyArea (NamedImport [] "List" listModulePath)
  in  [wishImports, listImports]


generateTestMainAST :: (FilePath, FilePath) -> [FilePath] -> AST
generateTestMainAST preludeModulePaths suitePaths =
  let indexedSuitePaths = zip [0..] suitePaths
      imports           = generateTestSuiteImport <$> indexedSuitePaths
      preludeImports    = generateStaticTestMainImports preludeModulePaths
      runTestSuiteExps  = generateRunTestSuiteExp <$> (fst <$> indexedSuitePaths)
  in  AST { aimports = preludeImports ++ imports, aexps = runTestSuiteExps, atypedecls = [], ainterfaces = [], ainstances = [], apath = Nothing }


generateTestAssignment :: Int -> Exp -> (Exp, Maybe String)
generateTestAssignment index exp = case exp of
  Source _ (App (Source _ (Var "test")) args) ->
    let assignmentName = "__t" <> show index <> "__"
    in  (Source emptyArea (Assignment assignmentName exp), Just assignmentName)

  _ ->
    (exp, Nothing)


addTestExports :: AST -> AST
addTestExports ast@AST{ apath = Just apath } =
  if ".spec.mad" `List.isSuffixOf` apath then
    let exps           = aexps ast
        assigned       = uncurry generateTestAssignment <$> zip [0..] exps
        exps'          = fst <$> assigned
        namesForExport = Maybe.mapMaybe snd assigned
        testsExport    = Source emptyArea (Export (Source emptyArea (Assignment "__tests__" (Source emptyArea (ListConstructor (
            Source emptyArea . ListItem . Source emptyArea . Var <$> namesForExport
          ))))))
    in  ast { aexps = exps' ++ [testsExport] }
  else
    ast
