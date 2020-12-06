{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory )
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                )
import           System.Exit
import           Utils.Path              hiding ( PathUtils(..) )
import           Parse.AST

import           Infer.Solve
import           Infer.Infer
import           Options.Applicative
import           Tools.CommandLineFlags
import           Tools.CommandLine
import           Compile
import qualified AST.Solved                    as Slv
import qualified Explain.Format                as Explain
import           Control.Monad                  ( when )
import           System.Process
import           Control.Exception              ( try
                                                , SomeException
                                                )
import           System.Environment             ( setEnv
                                                , getEnv
                                                )
import           System.Environment.Executable  ( getExecutablePath )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Coverage.Coverable             ( collectFromAST
                                                , isFunction
                                                , isLine
                                                , Coverable(..)
                                                )
import           Data.List                      ( isInfixOf
                                                , isPrefixOf
                                                )
import           Data.String.Utils


main :: IO ()
main = run =<< execParser opts

isCoverageEnabled :: IO Bool
isCoverageEnabled = do
  coverageEnv <- try $ getEnv "COVERAGE_MODE"
  case coverageEnv :: Either IOError String of
    Right "on" -> return True
    _          -> return False

run :: Command -> IO ()
run cmd = do
  coverage <- isCoverageEnabled

  case cmd of
    Compile entrypoint outputPath _ verbose debug bundle ->
      runCompilation entrypoint outputPath verbose debug bundle coverage

    Test entrypoint coverage -> runTests entrypoint coverage

    Install                  -> runPackageInstaller

shouldBeCovered :: FilePath -> FilePath -> Bool
shouldBeCovered rootPath path
  | rootPath `isPrefixOf` path && not ("Spec" `isInfixOf` path) = True
  | otherwise = False

runCoverageInitialization :: FilePath -> Slv.Table -> IO ()
runCoverageInitialization rootPath table = do
  let filteredTable =
        M.filterWithKey (\path _ -> shouldBeCovered rootPath path) table
  let coverableFunctions = M.map collectFromAST filteredTable
  let generated = M.mapWithKey generateLCovInfoForAST coverableFunctions
  let lcovInfoContent    = rstrip $ unlines $ M.elems generated

  createDirectoryIfMissing True ".coverage"
  writeFile ".coverage/lcov.info" lcovInfoContent

generateLCovInfoForAST :: FilePath -> [Coverable] -> String
generateLCovInfoForAST astPath coverables =
  let
    functions = filter isFunction coverables
    lines     = filter isLine coverables
    tn        = "TN:"
    sf        = "SF:" <> astPath
    -- sf          = "SF:" <> makeRelativeEx "/Users/a.boeglin/Code/madlib" astPath
    fns =
      rstrip
        $   unlines
        $   (\Function { line, name } -> "FN:" <> show line <> "," <> name)
        <$> functions
    fndas =
      rstrip
        $   unlines
        $   (\Function { line, name } -> "FNDA:0" <> "," <> name)
        <$> functions
    fnf = "FNF:" <> show (length functions)
    fnh = "FNH:0"
    das =
      rstrip
        $   unlines
        $   (\Line { line } -> "DA:" <> show line <> ",0")
        <$> lines
    lf          = "LF:" <> show (length lines)
    lh          = "LH:0"
    endOfRecord = "end_of_record"
  in
    rstrip $ unlines [tn, sf, fns, fndas, fnf, fnh, das, lf, lh, endOfRecord]

runPackageInstaller :: IO ()
runPackageInstaller = do
  executablePath              <- getExecutablePath
  packageInstallerPath        <- try $ getEnv "PKG_INSTALLER_PATH"
  packageInstallerPathChecked <-
    case (packageInstallerPath :: Either IOError String) of
      Left _ -> do
        return $ takeDirectory executablePath <> "/package-installer.js"
      Right p -> return p

  callCommand $ "node " <> packageInstallerPathChecked

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
  testOutput <-
    try $ callCommand $ "node " <> testRunnerPathChecked <> " " <> entrypoint
  case (testOutput :: Either IOError ()) of
    Left  e -> putStrLn $ ppShow e
    Right a -> return ()

runCompilation :: String -> String -> Bool -> Bool -> Bool -> Bool -> IO ()
runCompilation entrypoint outputPath verbose debug bundle coverage = do
  canonicalEntrypoint <- canonicalizePath entrypoint
  astTable            <- buildASTTable canonicalEntrypoint

  rootPath            <- canonicalizePath $ computeRootPath entrypoint

  let entryAST         = astTable >>= flip findAST canonicalEntrypoint
      resolvedASTTable = case (entryAST, astTable) of
        (Right ast, Right table) ->
          runExcept (runStateT (solveTable table ast) Unique { count = 0 })
        (_     , Left e) -> Left e
        (Left e, _     ) -> Left e

  when verbose $ do
    putStrLn $ "OUTPUT: " ++ outputPath
    putStrLn $ "ENTRYPOINT: " ++ canonicalEntrypoint
    putStrLn $ "ROOT PATH: " ++ rootPath
  when debug $ do
    putStrLn $ "PARSED:\n" ++ ppShow astTable
    putStrLn $ "RESOLVED:\n" ++ ppShow resolvedASTTable

  case resolvedASTTable of
    Left err -> do
      hPutStrLn stderr $ ppShow err
      Explain.format readFile err >>= putStrLn >> exitFailure
    Right (table, _) -> do
      when coverage $ do
        runCoverageInitialization rootPath table

      generate rootPath outputPath bundle coverage table

      when bundle $ do
        let entrypointOutputPath = computeTargetPath
              (takeDirectory outputPath <> "/.bundle")
              rootPath
              canonicalEntrypoint

        bundled <- runBundle outputPath entrypointOutputPath
        case bundled of
          Left  e             -> putStrLn e
          Right bundleContent -> do
            _ <- readProcessWithExitCode
              "rm"
              ["-r", takeDirectory outputPath <> "/.bundle"]
              ""
            writeFile outputPath bundleContent

      when debug $ do
        putStrLn "compiled JS:"
        putStrLn
          $   concat
          $   compile
                (CompilationConfig canonicalEntrypoint rootPath outputPath False)
          <$> M.elems table


rollupNotFoundMessage = unlines
  [ "Compilation error:"
  , "Rollup was not found."
  , "You must have rollup installed in order to use the bundling option. Please visit this page in order to install it: https://rollupjs.org/guide/en/#installation"
  ]

runBundle :: FilePath -> FilePath -> IO (Either String String)
runBundle dest entrypointCompiledPath = do
  rollupPath        <- try $ getEnv "ROLLUP_PATH"
  rollupPathChecked <- case (rollupPath :: Either IOError String) of
    Left _ -> do
      r <-
        try (readProcessWithExitCode "rollup" ["--version"] "") :: IO
          (Either SomeException (ExitCode, String, String))
      case r of
        Left  _ -> return $ Left rollupNotFoundMessage
        Right _ -> return $ Right "rollup"
    Right p -> do
      r <-
        try (readProcessWithExitCode p ["--version"] "") :: IO
          (Either SomeException (ExitCode, String, String))
      case r of
        Left _ -> do
          r <-
            try (readProcessWithExitCode "rollup" ["--version"] "") :: IO
              (Either SomeException (ExitCode, String, String))
          case r of
            Left  _ -> return $ Left rollupNotFoundMessage
            Right _ -> return $ Right "rollup"
        Right _ -> return $ Right p


  case rollupPathChecked of
    Right rollup -> do
      (_, stdout, _) <- readProcessWithExitCode
        rollup
        [entrypointCompiledPath, "--format", "umd"]
        ""
      return $ Right stdout
    Left e -> return $ Left e


generate :: FilePath -> FilePath -> Bool -> Bool -> Slv.Table -> IO ()
generate rootPath outputPath bundle coverage table =
  (head <$>)
    <$> mapM (generateAST rootPath outputPath bundle coverage)
    $   M.elems table


generateAST :: FilePath -> FilePath -> Bool -> Bool -> Slv.AST -> IO ()
generateAST rootPath outputPath bundle coverage ast@Slv.AST { Slv.apath = Just path }
  = do
    let computedOutputPath = if bundle
          then computeTargetPath (takeDirectory outputPath <> "/.bundle")
                                 rootPath
                                 path
          else computeTargetPath (takeDirectory outputPath) rootPath path

    createDirectoryIfMissing True $ takeDirectory computedOutputPath
    writeFile computedOutputPath $ compile
      (CompilationConfig rootPath path computedOutputPath coverage)
      ast




