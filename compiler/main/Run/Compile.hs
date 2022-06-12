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
import           Run.Options
import qualified Driver
import qualified Rock
import Driver.Query
import Error.Error
import Driver (Prune(Don'tPrune))
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
    canonicalOutputPath <- canonicalizePath outputPath
    rootPath            <- canonicalizePath "./"
    -- rootPath            <- canonicalizePath $ computeRootPath entrypoint

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
