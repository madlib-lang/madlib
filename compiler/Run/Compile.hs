{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Run.Compile where

import           GHC.IO                         ( )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory
                                                , joinPath
                                                , splitDirectories, pathSeparator
                                                )
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                , getCurrentDirectory
                                                )
import           System.Exit
import           System.Process
import           Control.Exception              ( try
                                                , SomeException
                                                )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( forever
                                                , when
                                                , unless
                                                )
import qualified Data.Map                      as Map
import           Data.List                     as List
import           Data.String.Utils
import           Text.Show.Pretty

import           Parse.Madlib.AST
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import           Infer.AST
import           Infer.Infer
import           Generate.Javascript           as GenerateJS
import qualified Generate.Json                 as GenerateJson
import           Generate.JSInternals
import qualified Generate.LLVM.LLVM            as LLVM
import qualified Generate.LLVM.ClosureConvert  as ClosureConvert
import qualified Generate.LLVM.Rename          as Rename
import qualified AST.Solved                    as Slv
import qualified AST.Core                      as Core
import qualified Optimize.TCE                  as TCE
import qualified Explain.Format                as Explain
import           Error.Warning
import           Coverage.Coverable             ( collectFromAST
                                                , isFunction
                                                , isLine
                                                , Coverable(..)
                                                )
import qualified MadlibDotJson.MadlibDotJson   as MadlibDotJson
import           MadlibDotJson.MadlibVersion
import           Utils.Path
import qualified Utils.PathUtils               as PathUtils
import           Paths_madlib                   ( version )
import           Run.Utils
import           Run.CommandLine
import           Run.Target
import           Optimize.StripNonJSInterfaces
import           Optimize.ToCore
import qualified Optimize.EtaExpansion as EtaExpansion
import qualified Optimize.EtaReduction as EtaReduction
import           System.FilePath.Posix (dropFileName)
import qualified System.FilePath       as FP
import           Control.Concurrent (threadDelay, forkIO, ThreadId)
import qualified Control.FoldDebounce as Debounce
import           System.FSNotify
import           Run.Options
import qualified Driver
import qualified Rock
import Driver.Query
import Error.Error
import Driver (Prune(Don'tPrune))
import Data.Time.Clock
import System.Console.ANSI
import Rock (Cyclic)


shouldBeCovered :: FilePath -> FilePath -> Bool
shouldBeCovered rootPath path | rootPath `List.isPrefixOf` path && not (".spec.mad" `List.isSuffixOf` path) = True
                              | otherwise = False


runCoverageInitialization :: FilePath -> Slv.Table -> IO ()
runCoverageInitialization rootPath table = do
  let filteredTable      = Map.filterWithKey (\path _ -> shouldBeCovered rootPath path) table
  let coverableFunctions = Map.map collectFromAST filteredTable
  let generated          = Map.mapWithKey generateLCovInfoForAST coverableFunctions
  let lcovInfoContent    = rstrip $ unlines $ Map.elems generated

  createDirectoryIfMissing True ".coverage"
  writeFile ".coverage/lcov.info" lcovInfoContent


generateLCovInfoForAST :: FilePath -> [Coverable] -> String
generateLCovInfoForAST astPath coverables =
  let functions   = filter isFunction coverables
      lines       = filter isLine coverables
      tn          = "TN:"
      sf          = "SF:" <> astPath
      fns         = rstrip $ unlines $ (\Function { line, name } -> "FN:" <> show line <> "," <> name) <$> functions
      fndas       = rstrip $ unlines $ (\Function { name } -> "FNDA:0" <> "," <> name) <$> functions
      fnf         = "FNF:" <> show (length functions)
      fnh         = "FNH:0"
      das         = rstrip $ unlines $ (\Line { line } -> "DA:" <> show line <> ",0") <$> lines
      lf          = "LF:" <> show (length lines)
      lh          = "LH:0"
      endOfRecord = "end_of_record"
  in  rstrip $ unlines [tn, sf, fns, fndas, fnf, fnh, das, lf, lh, endOfRecord]


-- TODO: Just make it print straight?
globalChecks :: IO [CompilationWarning]
globalChecks = do
  parsedMadlibDotJson <- MadlibDotJson.loadCurrentMadlibDotJson

  case parsedMadlibDotJson of
    Left _ -> return []
    Right MadlibDotJson.MadlibDotJson { MadlibDotJson.madlibVersion = Just madlibVersion, MadlibDotJson.name = pkgName }
      -> case checkVersion pkgName madlibVersion version of
        Just warning -> return [warning]
        Nothing      -> return []
    _ -> return []


runCompilation :: Command -> Bool -> IO ()
runCompilation opts@(Compile entrypoint outputPath config verbose debug bundle optimized target json testsOnly watchMode) coverage
  = do
    extraWarnings       <- globalChecks
    canonicalEntrypoint <- canonicalizePath entrypoint
    rootPath            <- canonicalizePath "./"
    -- rootPath            <- canonicalizePath $ computeRootPath entrypoint
    sourcesToCompile    <- getFilesToCompile testsOnly canonicalEntrypoint

    let options =
          Options
            { optPathUtils = PathUtils.defaultPathUtils
            , optEntrypoint = canonicalEntrypoint
            , optTarget = target
            , optRootPath = rootPath
            , optOutputPath = outputPath
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
        watch rootPath (runCompilationTask state options)
        return ()
    else do
      state <- Driver.initialState
      runCompilationTask state options [canonicalEntrypoint]
      -- Driver.compile options (head sourcesToCompile)


recordAndPrintDuration :: String -> IO a -> IO a
recordAndPrintDuration title action = do
  startT       <- getCurrentTime
  actionResult <- action
  endT         <- getCurrentTime
  let diff = diffUTCTime endT startT
  let (ms, _) = properFraction $ diff * 1000
  putStrLn $ title <> show ms <> "ms"
  return actionResult


-- compilationTask :: Options -> Rock.Task Query ()
-- compilationTask options = do
--   -- Rock.fetch $ DetectImportCycle [] (optEntrypoint options)
--   -- return ()

--   -- hasCycle <- Rock.fetch $ DetectImportCycle [] (optEntrypoint options)
--   Rock.fetch $ BuiltTarget (optEntrypoint options)
--   -- hasCycle <- Rock.fetch $ DetectImportCycle [] (optEntrypoint options)
--   -- unless hasCycle $ Rock.fetch $ BuiltTarget (optEntrypoint options)


runCompilationTask :: Driver.State CompilationError -> Options -> [FilePath] -> IO ()
runCompilationTask state options invalidatedPaths = do
  clearScreen
  setCursorPosition 0 0
  recordAndPrintDuration "Built in " $ do
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


watch :: FilePath -> ([FilePath] -> IO ()) -> IO ThreadId
watch root action = do
  withManager $ \mgr -> do
    trigger <-
      Debounce.new
        Debounce.Args
          { Debounce.cb = action
          , Debounce.fold = \l v -> List.nub $ v:l
          , Debounce.init = []
          }
        Debounce.def
          { Debounce.delay = 50000 -- 50ms
          , Debounce.alwaysResetTimer = True
          }

    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      root         -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          f = case e of
                Added f _ _ ->
                  f

                Modified f _ _ ->
                  f

                Removed f _ _ ->
                  f

                Unknown f _ _ ->
                  f

          -- @TODO it would be better to not listen to these folders in the `watchTree` when available
          -- https://github.com/haskell-fswatch/hfsnotify/issues/101
          shouldTrigger = ".mad" `List.isSuffixOf` f

        when shouldTrigger $ Debounce.send trigger f
      )

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
