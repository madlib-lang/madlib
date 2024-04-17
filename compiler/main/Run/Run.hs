{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
module Run.Run where

import qualified Data.Map                      as M
import qualified Data.List                     as List
import           System.FilePath                ( takeBaseName
                                                , takeFileName
                                                , dropExtension
                                                , joinPath
                                                , makeRelative
                                                , dropFileName
                                                )
import           System.Directory               ( canonicalizePath )
import qualified Explain.Format                 as Explain
import           System.Process
import           Data.List                      ( isSuffixOf )
import qualified MadlibDotJson.MadlibDotJson   as MadlibDotJson
import qualified Utils.PathUtils               as PathUtils
import           Run.Target
import           Run.Compile
import           Run.CommandLine
import           Utils.Path (computeTargetPath)
import           Run.OptimizationLevel
import           System.IO (stdout)
import           System.IO.Silently
import           Run.Target
import qualified Driver
import qualified Driver.Query as Query
import           Error.Warning
import           Rock (Cyclic)
import           Error.Error
import           Run.Options
import Data.IORef
import           Control.Monad                  ( forever
                                                , when
                                                , unless
                                                )
import           System.Exit (exitFailure)
import           System.IO (hPutStr, stderr)
import           Driver (Prune(Don'tPrune))
import           Utils.List
import           Control.Exception              ( try )
import           System.Console.ANSI
import GHC.IO (unsafePerformIO)


runRun :: Target -> FilePath -> [String] -> Bool -> IO ()
runRun target input args watchMode = do
  if ".mad" `isSuffixOf` input then do
    runModule target input args watchMode
  else do
    let packagePath              = joinPath ["madlib_modules", input]
    let packageMadlibDotJsonPath = joinPath [packagePath, "madlib.json"]
    parsedMadlibDotJson <- MadlibDotJson.load PathUtils.defaultPathUtils packageMadlibDotJsonPath
    case parsedMadlibDotJson of
      Left e -> putStrLn e

      Right MadlibDotJson.MadlibDotJson { MadlibDotJson.bin = Just bin } -> do
        let exePath = joinPath [packagePath, bin]
        runModule target exePath args watchMode

runFolder :: FilePath
runFolder = "build/"


runModule :: Target -> FilePath -> [String] -> Bool -> IO ()
runModule target input args watchMode = do
  canEntrypoint    <- canonicalizePath input
  canCurrentFolder <- canonicalizePath "./"
  rootPath <- canonicalizePath "./"

  let llvmOutputPath = runFolder <> "run"
  let jsOutputPath = joinPath [runFolder, dropFileName input, (takeBaseName . takeFileName $ input) <> ".mjs"]

  canonicalOutputPath <-
    if target == TNode then
      canonicalizePath runFolder
    else
      canonicalizePath llvmOutputPath

  let options =
        Options
          { optPathUtils = PathUtils.defaultPathUtils
          , optEntrypoint = canEntrypoint
          , optTarget = target
          , optRootPath = rootPath
          , optOutputPath = canonicalOutputPath
          , optOptimized = False
          , optDebug = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          , optMustHaveMain = True
          , optOptimizationLevel = O1
          , optParseOnly = False
          }

  let args' = map (("\"" <>) . (<> "\"")) args
  let runner = \_ ->
        if target == TNode then do
          (_, _, _, handle) <-
            createProcess (proc "node" (jsOutputPath : args'))
          return handle
        else do
          (_, _, _, handle) <-
            createProcess (proc llvmOutputPath args')
          return handle

  state <- Driver.initialState
  runRunTask watchMode state options runner []

  when watchMode $ do
    Driver.watch rootPath (runRunTask watchMode state options runner)
    return ()


runningProcess :: IORef (Maybe ProcessHandle)
{-# NOINLINE runningProcess #-}
runningProcess = unsafePerformIO $ newIORef Nothing


runRunTask :: Bool -> Driver.State CompilationError -> Options -> (() -> IO ProcessHandle) -> [FilePath] -> IO ()
runRunTask watchMode state options runner invalidatedPaths = do
  let silenceIfNeeded effect =
        if watchMode then
          effect
        else
          hSilence [stdout] effect

  (warnings, errors) <- silenceIfNeeded $ do
    result <-
      try $ Driver.runIncrementalTask
        state
        options
        invalidatedPaths
        mempty
        Don'tPrune
        (Driver.typeCheckFileTask $ optEntrypoint options)
        :: IO (Either (Cyclic Query.Query) ((), [CompilationWarning], [CompilationError]))

    case result of
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

  unless (null formattedWarnings && null formattedErrors) $ do
    unless watchMode $ putStrLn $ List.intercalate "\n" formattedWarnings
    hPutStr stderr $ List.intercalate "\n" formattedErrors
  unless (null errors || watchMode) $ do
    exitFailure

  when (null errors) $ do
    runningProcess' <- readIORef runningProcess
    case runningProcess' of
      Just handle ->
        terminateProcess handle

      Nothing ->
        return ()
    handle <- runner ()
    unless watchMode $ do
      waitForProcess handle
      return ()
    writeIORef runningProcess (Just handle)
