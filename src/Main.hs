{-# LANGUAGE FlexibleContexts   #-}
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


main :: IO ()
main = run =<< execParser opts

run :: Command -> IO ()
run cmd = do
  case cmd of
    Compile entrypoint outputPath _ verbose debug bundle ->
      runCompilation entrypoint outputPath verbose debug bundle

    Test entrypoint -> runTests entrypoint

    Install         -> runPackageInstaller

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

runTests :: String -> IO ()
runTests entrypoint = do
  executablePath        <- getExecutablePath
  testRunnerPath        <- try $ getEnv "TEST_RUNNER_PATH"
  testRunnerPathChecked <- case (testRunnerPath :: Either IOError String) of
    Left _ -> do
      return $ takeDirectory executablePath <> "/test-runner.js"
    Right p -> return p

  setEnv "MADLIB_PATH" executablePath
  testOutput <-
    try $ callCommand $ "node " <> testRunnerPathChecked <> " " <> entrypoint
  case (testOutput :: Either IOError ()) of
    Left  e -> putStrLn $ ppShow e
    Right a -> return ()

runCompilation :: String -> String -> Bool -> Bool -> Bool -> IO ()
runCompilation entrypoint outputPath verbose debug bundle = do
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
      generate rootPath outputPath bundle table

      when bundle $ do
        let entrypointOutputPath = computeTargetPath
              ((takeDirectory outputPath) <> "/.bundle")
              rootPath
              canonicalEntrypoint

        bundled <- runBundle outputPath entrypointOutputPath
        case bundled of
          Left  e             -> putStrLn e
          Right bundleContent -> do
            _ <- readProcessWithExitCode
              "rm"
              ["-r", (takeDirectory outputPath) <> "/.bundle"]
              ""
            writeFile outputPath bundleContent

      when debug $ do
        putStrLn "compiled JS:"
        putStrLn $ concat $ compile rootPath outputPath <$> M.elems table


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


generate :: FilePath -> FilePath -> Bool -> Slv.Table -> IO ()
generate rootPath outputPath bundle table =
  (head <$>) <$> mapM (generateAST rootPath outputPath bundle) $ M.elems table


generateAST :: FilePath -> FilePath -> Bool -> Slv.AST -> IO ()
generateAST rootPath outputPath bundle ast@Slv.AST { Slv.apath = Just path } =
  do
    let computedOutputPath = if bundle
          then computeTargetPath ((takeDirectory outputPath) <> "/.bundle")
                                 rootPath
                                 path
          else computeTargetPath ((takeDirectory outputPath)) rootPath path

    createDirectoryIfMissing True $ takeDirectory computedOutputPath
    writeFile computedOutputPath $ compile rootPath computedOutputPath ast




