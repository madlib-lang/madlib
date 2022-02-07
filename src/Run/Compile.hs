{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
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
import qualified AST.Optimized                 as Opt
import qualified Optimize.TCE                  as TCE
import           Optimize.Optimize
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
runCompilation opts@(Compile entrypoint outputPath config verbose debug bundle optimized target json testsOnly) coverage
  = do
    extraWarnings       <- globalChecks

    canonicalEntrypoint <- canonicalizePath entrypoint
    sourcesToCompile    <- getFilesToCompile testsOnly canonicalEntrypoint
    astTable            <- buildManyASTTables target mempty sourcesToCompile
    let (canTable, warnings) = case astTable of
          Right table ->
            let (table', warnings) = Can.canonicalizeMany target Can.initialEnv table sourcesToCompile
            in  (table', extraWarnings ++ warnings)
          Left e -> (Left e, [])

    unless json $ do
      formattedWarnings <- mapM (Explain.formatWarning readFile json) warnings
      let fullWarning = intercalate "\n\n\n" formattedWarnings
      unless (null fullWarning) $ putStrLn fullWarning

    rootPath <- canonicalizePath $ computeRootPath entrypoint

    let resolvedASTTable = case canTable of
          Right table -> do
            runExcept (runStateT (solveManyASTs mempty table sourcesToCompile) InferState { count = 0, errors = [] })
          Left e -> Left e

    when verbose $ do
      putStrLn $ "OUTPUT: " ++ outputPath
      putStrLn $ "ENTRYPOINT: " ++ canonicalEntrypoint
      putStrLn $ "ROOT PATH: " ++ rootPath
    when debug $ do
      putStrLn $ "PARSED:\n" ++ ppShow astTable
      putStrLn $ "RESOLVED:\n" ++ ppShow resolvedASTTable

    case resolvedASTTable of
      Left err -> do
        if json
          then do
            formattedWarnings <- mapM (\warning -> (warning, ) <$> Explain.formatWarning readFile json warning) warnings
            formattedErr      <- Explain.format readFile json err
            putStrLn $ GenerateJson.compileASTTable [(err, formattedErr)] formattedWarnings mempty
          else do
            unless (null warnings) (putStrLn "\n")
            Explain.format readFile json err >>= putStrLn >> exitFailure
      Right (table, inferState) ->
        let errs      = errors inferState
            hasErrors = not (null errs)
        in  if json
              then do
                formattedWarnings <- mapM (\warning -> (warning, ) <$> Explain.formatWarning readFile json warning)
                                          warnings
                formattedErrors <- mapM (\err -> (err, ) <$> Explain.format readFile json err) errs
                putStrLn $ GenerateJson.compileASTTable formattedErrors formattedWarnings table
              else if hasErrors
                then do
                  unless (null warnings) (putStrLn "\n")
                  formattedErrors <- mapM (Explain.format readFile json) errs
                  let fullError = intercalate "\n\n\n" formattedErrors
                  putStrLn fullError >> exitFailure
                else do
                  when coverage $ do
                    runCoverageInitialization rootPath table

                  if target == TLLVM then do
                    let renamedTable     = Rename.renameTable table
                    -- -- TODO: only do this in verbose mode?
                    -- putStrLn (ppShow renamedTable)
                    let closureConverted = ClosureConvert.optimizeTable renamedTable
                    -- putStrLn (ppShow closureConverted)
                    LLVM.generateTable outputPath rootPath closureConverted canonicalEntrypoint
                  else do
                    let optimizedTable = optimizeTable optimized table
                        withTCE = TCE.resolve <$> optimizedTable
                    -- putStrLn (ppShow optimizedTable)
                    -- putStrLn (ppShow withTCE)
                    generate opts { compileInput = canonicalEntrypoint } coverage rootPath optimizedTable sourcesToCompile

                  when bundle $ do
                    let entrypointOutputPath =
                          computeTargetPath (takeDirectory outputPath <> "/.bundle") rootPath canonicalEntrypoint

                    bundled <- runBundle entrypointOutputPath
                    case bundled of
                      Left  e                    -> putStrLn e
                      Right (bundleContent, err) -> do
                        _ <- readProcessWithExitCode "rm" ["-r", takeDirectory outputPath <> "/.bundle"] ""
                        writeFile outputPath bundleContent
                        unless (null err) $ putStrLn err


rollupNotFoundMessage = unlines
  [ "Compilation error:"
  , "Rollup was not found."
  , "You must have rollup installed in order to use the bundling option. Please visit this page in order to install it: https://rollupjs.org/guide/en/#installation"
  ]

runBundle :: FilePath -> IO (Either String (String, String))
runBundle entrypointCompiledPath = do
  rollupPath        <- try $ getEnv "ROLLUP_PATH"
  rollupPathChecked <- case (rollupPath :: Either IOError String) of
    Left _ -> do
      r <-
        try (readProcessWithExitCode "rollup" ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left  err -> do
          putStrLn $ ppShow err
          return $ Left rollupNotFoundMessage
        Right _ -> return $ Right "rollup"
    Right p -> do
      r <- try (readProcessWithExitCode p ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left err -> do
          putStrLn $ ppShow err
          r <-
            try (readProcessWithExitCode "rollup" ["--version"] "") :: IO
              (Either SomeException (ExitCode, String, String))
          case r of
            Left  err -> do
              putStrLn $ ppShow err
              return $ Left rollupNotFoundMessage
            Right _ -> return $ Right "rollup"
        Right _ -> return $ Right p

  case rollupPathChecked of
    Right rollup -> do
      r <-
        try
          (readProcessWithExitCode
            rollup
            [ entrypointCompiledPath
            , "--format"
            , "umd"
            , "--name"
            , "exe"
            , "-p"
            , "@rollup/plugin-node-resolve"
            , "--silent"
            ]
            ""
          ) :: IO (Either SomeException (ExitCode, String, String))

      case r of
        Left  e                   -> return $ Left (ppShow e)
        Right (_, stdout, stderr) -> return $ Right (stdout, stderr)
    Left e -> return $ Left e


generate :: Command -> Bool -> FilePath -> Opt.Table -> [FilePath] -> IO ()
generate options@Compile { compileOutput, compileBundle, compileOptimize, compileTarget } coverage rootPath table sourcesToCompile
  = do
    mapM_ (generateAST options coverage rootPath sourcesToCompile) $ M.elems table
    writeFile (takeDirectory compileOutput <> (if compileBundle then "/.bundle" else "") <> (pathSeparator : "__internals__.mjs"))
      $ generateInternalsModuleContent compileTarget compileOptimize coverage


computeInternalsPath :: FilePath -> FilePath -> FilePath
computeInternalsPath rootPath astPath = case stripPrefix rootPath astPath of
  Just s ->
    let dirs = splitDirectories (takeDirectory s)
        minus
          | joinPath ["prelude", "__internal__"] `isInfixOf` astPath = if joinPath ["prelude", "__internal__"] `isInfixOf` rootPath then 0 else 2
          | "madlib_modules" `isInfixOf` astPath && not (rootPath `isPrefixOf` astPath) = -2
          | otherwise = 1
        dirLength = length dirs - minus
    in  joinPath $ ["./"] <> replicate dirLength ".." <> ["__internals__.mjs"]
  Nothing -> "./__internals__.mjs"

generateAST :: Command -> Bool -> FilePath -> [FilePath] -> Opt.AST -> IO ()
generateAST Compile { compileInput, compileOutput, compileBundle, compileOptimize, compileTarget } coverage rootPath sourcesToCompile ast@Opt.AST { Opt.apath = Just path }
  = do
    let internalsPath      = convertWindowsSeparators $ computeInternalsPath rootPath path
        entrypointPath     = if path `elem` sourcesToCompile then path else compileInput
        computedOutputPath = if compileBundle
          then computeTargetPath (takeDirectory compileOutput <> "/.bundle") rootPath path
          else computeTargetPath (takeDirectory compileOutput) rootPath path

    createDirectoryIfMissing True $ takeDirectory computedOutputPath
    writeFile computedOutputPath $ compile
      GenerateJS.initialEnv
      (CompilationConfig rootPath
                         path
                         entrypointPath
                         computedOutputPath
                         coverage
                         compileOptimize
                         compileTarget
                         internalsPath
      )
      ast
