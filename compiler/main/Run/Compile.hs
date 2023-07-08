{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Run.Compile where

import           System.Directory               ( canonicalizePath, doesFileExist )
import           Control.Exception              ( try )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( forever
                                                , when
                                                , unless
                                                )
import           Data.List                     as List
import           Utils.List
import qualified Explain.Format                as Explain
import           Error.Warning
import qualified Utils.PathUtils               as PathUtils
import           Run.CommandLine
import           Run.Options
import qualified Driver
import qualified Rock
import           Driver.Query
import           Error.Error
import           Driver (Prune(Don'tPrune))
import           System.Console.ANSI
import           Rock (Cyclic)
import           System.Exit (exitFailure)
import           Run.OptimizationLevel (OptimizationLevel(O0))
import           System.IO (hPutStr, stderr)


runCompilation :: Command -> IO ()
runCompilation (Compile entrypoint outputPath _ verbose debug bundle optimized target watchMode coverage optLevel)
  = do
    canonicalEntrypoint <- canonicalizePath entrypoint
    canonicalOutputPath <- canonicalizePath outputPath
    rootPath            <- canonicalizePath "./"

    entrypointFound <- doesFileExist canonicalEntrypoint

    unless entrypointFound $ do
      putStrLn $ "'" <> entrypoint <> "' not found, exiting"
      exitFailure

    let optLevel' =
          if debug then
            O0
          else
            optLevel

    let options =
          Options
            { optPathUtils = PathUtils.defaultPathUtils
            , optEntrypoint = canonicalEntrypoint
            , optTarget = target
            , optRootPath = rootPath
            , optOutputPath = canonicalOutputPath
            , optOptimized = optimized
            , optDebug = debug
            , optBundle = bundle
            , optCoverage = coverage
            , optGenerateDerivedInstances = True
            , optInsertInstancePlaholders = True
            , optMustHaveMain = True
            , optOptimizationLevel = optLevel'
            , optParseOnly = False
            }

    when verbose $ do
      putStrLn $ "entrypoint: " <> canonicalEntrypoint
      putStrLn $ "root path: " <> rootPath
      putStrLn $ "output path: " <> outputPath
      putStrLn $ "bundle: " <> show bundle
      putStrLn $ "target: " <> show target


    if watchMode then
      when watchMode $ do
        state <- Driver.initialState
        runCompilationTask True state options [canonicalEntrypoint]
        Driver.watch rootPath (runCompilationTask True state options)
        return ()
    else do
      state <- Driver.initialState
      runCompilationTask False state options [canonicalEntrypoint]


runCompilationTask :: Bool -> Driver.State CompilationError -> Options -> [FilePath] -> IO ()
runCompilationTask watchMode state options invalidatedPaths = do
  when watchMode $ do
    clearScreen
    setCursorPosition 0 0
  Driver.recordAndPrintDuration "Built in " $ do
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
        unless (null warnings) $ do
          formattedWarnings <- mapM (Explain.formatWarning readFile False) $ removeDuplicates warnings
          putStrLn $ List.intercalate "\n" formattedWarnings

        (_, _, _) <- Driver.runIncrementalTask
          state
          options
          invalidatedPaths
          mempty
          Don'tPrune
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
          Don'tPrune
          (Driver.detectCyleTask (optEntrypoint options))

        return (warnings, errors)

    formattedWarnings <- mapM (Explain.formatWarning readFile False) $ removeDuplicates warnings
    formattedErrors   <- mapM (Explain.formatError readFile False) $ removeDuplicates errors

    putStrLn $ List.intercalate "\n" formattedWarnings
    hPutStr stderr $ List.intercalate "\n" formattedErrors
    unless (null errors || watchMode) $ do
      exitFailure

  when watchMode $ putStrLn "\nWatching... (press ctrl-C to quit)"
