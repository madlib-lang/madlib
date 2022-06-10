{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
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
import           Control.Monad                  ( when
                                                , unless
                                                )
import qualified Data.Map                      as M
import           Data.List                      ( isInfixOf
                                                , isSuffixOf
                                                , isPrefixOf
                                                , intercalate
                                                , stripPrefix
                                                )
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
import           Optimize.ToCore
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
import System.FilePath.Posix (dropFileName)
import qualified Driver.Rules as Rules
import Run.Options



shouldBeCovered :: FilePath -> FilePath -> Bool
shouldBeCovered rootPath path | rootPath `isPrefixOf` path && not (".spec.mad" `isSuffixOf` path) = True
                              | otherwise = False


runCoverageInitialization :: FilePath -> Slv.Table -> IO ()
runCoverageInitialization rootPath table = do
  let filteredTable      = M.filterWithKey (\path _ -> shouldBeCovered rootPath path) table
  let coverableFunctions = M.map collectFromAST filteredTable
  let generated          = M.mapWithKey generateLCovInfoForAST coverableFunctions
  let lcovInfoContent    = rstrip $ unlines $ M.elems generated

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
runCompilation opts@(Compile entrypoint outputPath config verbose debug bundle optimized target json testsOnly noCache) coverage
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
            }

    when verbose $ do
      putStrLn $ "entrypoint: " <> canonicalEntrypoint
      putStrLn $ "root path: " <> rootPath
      putStrLn $ "output path: " <> outputPath
      putStrLn $ "bundle: " <> show bundle
      putStrLn $ "target: " <> show target

    Rules.compile options (head sourcesToCompile)


    -- unless (null table) $ do
    --   when verbose $ do
    --     putStrLn $ "OUTPUT: " ++ outputPath
    --     putStrLn $ "ENTRYPOINT: " ++ canonicalEntrypoint
    --     putStrLn $ "ROOT PATH: " ++ rootPath
    --   when debug $ do
    --     putStrLn $ "PARSED:\n" ++ ppShow table
    --     putStrLn $ "RESOLVED:\n" ++ ppShow table

    --   -- case resolvedASTTable of
    --   --   Left err -> do
    --   --     if json
    --   --       then do
    --   --         formattedWarnings <- mapM (\warning -> (warning, ) <$> Explain.formatWarning readFile json warning) warnings
    --   --         formattedErr      <- Explain.format readFile json err
    --   --         putStrLn $ GenerateJson.compileASTTable [(err, formattedErr)] formattedWarnings canonicalEntrypoint mempty
    --   --         -- putStrLn $ GenerateJson.compileASTTable [(err, formattedErr)] formattedWarnings mempty
    --   --       else do
    --   --         unless (null warnings) (putStrLn "\n")
    --   --         Explain.format readFile json err >>= putStrLn >> exitFailure
    --   --   Right (table, inferState) ->
    --   -- let errs      = errors inferState
    --   --     hasErrors = not (null errs)
    --   -- if hasErrors then do
    --   --   unless (null warnings) (putStrLn "\n")
    --   --   formattedErrors <- mapM (Explain.format readFile json) errs
    --   --   let fullError = intercalate "\n\n\n" formattedErrors
    --   --   putStrLn fullError >> exitFailure
    --   -- else do
    --   when coverage $ do
    --     runCoverageInitialization rootPath table

    --   if target == TLLVM then do
    --     let coreTable        = tableToCore False table
    --         renamedTable     = Rename.renameTable coreTable
    --         reduced          = EtaReduction.reduceTable renamedTable
    --         closureConverted = ClosureConvert.convertTable reduced
    --         withTCE          = TCE.resolveTable closureConverted

    --     -- putStrLn (ppShow closureConverted)
    --     -- putStrLn (ppShow withTCE)
    --     LLVM.generateTable noCache outputPath rootPath withTCE canonicalEntrypoint
    --   else do
    --     let coreTable     = tableToCore optimized table
    --         strippedTable = stripTable coreTable
    --         withTCE       = TCE.resolveTable strippedTable
    --     -- putStrLn (ppShow withTCE)
    --     generate opts { compileInput = canonicalEntrypoint } coverage rootPath withTCE sourcesToCompile

    --   when bundle $ do
    --     let entrypointOutputPath =
    --           computeTargetPath (takeDirectory outputPath <> "/.bundle") rootPath canonicalEntrypoint

    --     bundled <- runBundle entrypointOutputPath
    --     case bundled of
    --       Left  e                    -> putStrLn e
    --       Right (bundleContent, err) -> do
    --         _ <- readProcessWithExitCode "rm" ["-r", takeDirectory outputPath <> "/.bundle"] ""
    --         writeFile outputPath bundleContent
    --         unless (null err) $ putStrLn err

