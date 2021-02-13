{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
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

import           Infer.AST
import           Infer.Infer
import           Options.Applicative
import           Tools.CommandLineFlags
import           Tools.CommandLine
import           Compile.Compile
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import qualified AST.Optimized                 as Opt
import           Optimize.Optimize
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
import           Compile.JSInternals
import           Error.Error
import           Explain.Reason
import qualified Canonicalize.Canonicalize     as Can
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import           Target
import           Debug.Trace
import           Text.Show.Pretty


main :: IO ()
main = execParser opts >>= run

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
    Compile{}                -> runCompilation cmd coverage

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
    Left  e -> return ()
    Right a -> return ()

runCompilation :: Command -> Bool -> IO ()
runCompilation opts@(Compile entrypoint outputPath config verbose debug bundle optimized target) coverage
  = do
    canonicalEntrypoint <- canonicalizePath entrypoint
    astTable            <- buildASTTable canonicalEntrypoint
    let canTable = astTable >>= \table -> Can.runCanonicalization
          target
          Can.initialEnv
          table
          canonicalEntrypoint

    rootPath <- canonicalizePath $ computeRootPath entrypoint

    let entryAST         = canTable >>= flip Can.findAST canonicalEntrypoint
        resolvedASTTable = case (entryAST, canTable) of
          (Right ast, Right table) -> do
            runExcept (runStateT (solveTable table ast) Unique { count = 0 })
          (_, Left e) -> Left e
          (Left e, _) -> Left $ InferError (ImportNotFound rootPath) NoReason

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

        let optimizedTable = optimizeTable optimized table

        generate opts { compileInput = canonicalEntrypoint }
                 coverage
                 rootPath
                 optimizedTable

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
        [entrypointCompiledPath, "--format", "umd", "--name", "exe"]
        ""
      return $ Right stdout
    Left e -> return $ Left e


generate :: Command -> Bool -> FilePath -> Opt.Table -> IO ()
generate options coverage rootPath table = do
  let outputPath = compileOutput options
      bundle     = compileBundle options
      optimized  = compileOptimize options
      target     = compileTarget options
  (head <$>) <$> mapM (generateAST options coverage rootPath) $ M.elems table
  writeFile
      (  takeDirectory outputPath
      <> (if bundle then "/.bundle" else "")
      <> "/__internals__.mjs"
      )
    $ generateInternalsModuleContent target optimized coverage


generateAST :: Command -> Bool -> FilePath -> Opt.AST -> IO ()
generateAST options coverage rootPath ast@Opt.AST { Opt.apath = Just path } =
  do
    let entrypointPath     = compileInput options
        outputPath         = compileOutput options
        bundle             = compileBundle options
        optimized          = compileOptimize options
        target             = compileTarget options
        computedOutputPath = if bundle
          then computeTargetPath (takeDirectory outputPath <> "/.bundle")
                                 rootPath
                                 path
          else computeTargetPath (takeDirectory outputPath) rootPath path

    createDirectoryIfMissing True $ takeDirectory computedOutputPath
    writeFile computedOutputPath $ compile
      (CompilationConfig rootPath
                         path
                         entrypointPath
                         computedOutputPath
                         coverage
                         optimized
                         target
      )
      ast
