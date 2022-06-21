{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Run.Compile where

import           System.Directory               ( canonicalizePath )
import           Control.Exception              ( try )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( forever
                                                , when
                                                , unless
                                                )
import           Data.List                     as List

import qualified Explain.Format                as Explain
import           Error.Warning
import           Coverage.Coverable             ( collectFromAST
                                                , isFunction
                                                , isLine
                                                , Coverable(..)
                                                )
import qualified Utils.PathUtils               as PathUtils
import           Run.CommandLine
import           Run.Options
import qualified Driver
import qualified Rock
import Driver.Query
import Error.Error
import Driver (Prune(Don'tPrune))
import System.Console.ANSI
import Rock (Cyclic)


runCompilation :: Command -> Bool -> IO ()
runCompilation (Compile entrypoint outputPath _ verbose _ bundle optimized target watchMode) coverage
  = do
    canonicalEntrypoint <- canonicalizePath entrypoint
    canonicalOutputPath <- canonicalizePath outputPath
    rootPath            <- canonicalizePath "./"

    let options =
          Options
            { optPathUtils = PathUtils.defaultPathUtils
            , optEntrypoint = canonicalEntrypoint
            , optTarget = target
            , optRootPath = rootPath
            , optOutputPath = canonicalOutputPath
            , optOptimized = optimized
            , optBundle = bundle
            , optCoverage = coverage
            , optGenerateDerivedInstances = True
            , optInsertInstancePlaholders = True
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
        runCompilationTask state options [canonicalEntrypoint]
        putStrLn "\nWatching... (press ctrl-C to quit)"
        Driver.watch rootPath (runCompilationTask state options)
        return ()
    else do
      state <- Driver.initialState
      runCompilationTask state options [canonicalEntrypoint]


runCompilationTask :: Driver.State CompilationError -> Options -> [FilePath] -> IO ()
runCompilationTask state options invalidatedPaths = do
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

    formattedErrors   <- mapM (Explain.format readFile False) errors
    let ppErrors =
          if null errors then
            ""
          else
            List.intercalate "\n\n\n" formattedErrors <> "\n"

    putStr ppWarnings
    putStr ppErrors
  return ()