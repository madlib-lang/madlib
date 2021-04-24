{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory
                                                , takeBaseName
                                                , takeExtension
                                                , takeFileName
                                                , dropExtension
                                                , joinPath
                                                , splitDirectories
                                                )
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                , getDirectoryContents
                                                , doesDirectoryExist
                                                , getCurrentDirectory
                                                )
import           System.Exit
import           Utils.Path
import qualified Utils.PathUtils               as PathUtils
import           Parse.Madlib.AST
import qualified Parse.DocString.Grammar       as DocString
import qualified Parse.DocString.DocString     as DocString

import           Infer.AST
import           Infer.Infer
import           Options.Applicative
import           Tools.CommandLineFlags
import           Tools.CommandLine
import           Compile.Javascript            as CompileJS
import qualified Compile.Json                  as CompileJson
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import qualified AST.Optimized                 as Opt
import           Optimize.Optimize
import qualified Explain.Format                as Explain
import           Control.Monad                  ( when
                                                , filterM
                                                , unless
                                                )
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
                                                , isSuffixOf
                                                , isPrefixOf
                                                , intercalate
                                                , stripPrefix
                                                )
import           Data.String.Utils
import           Compile.JSInternals
import           Compile.Documentation
import           Error.Error
import           Error.Context
import qualified Canonicalize.Canonicalize     as Can
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import           Target
import           PackageGenerator
import           Debug.Trace
import           Text.Show.Pretty
import           GHC.IO.Encoding
import qualified MadlibDotJSON
import qualified Utils.URL                     as URL


main :: IO ()
main = do
  setLocaleEncoding utf8
  execParser opts >>= run

isCoverageEnabled :: IO Bool
isCoverageEnabled = do
  coverageEnv <- try $ getEnv "COVERAGE_MODE"
  case coverageEnv :: Either IOError String of
    Right "on" -> return True
    _          -> return False

sanitizeOutputPath :: Command -> Either String FilePath
sanitizeOutputPath Compile { compileOutput, compileBundle } =
  let ext = takeExtension compileOutput
  in
    if compileBundle
      then case ext of
        ".js" -> Right compileOutput
        _ ->
          Left $ "Wrong output. With bundle option the output must have '.js' extension, but '" <> ext <> "' was given!"
      else if last compileOutput == '/' then Right compileOutput else Right $ compileOutput <> "/"


run :: Command -> IO ()
run cmd = do
  coverage <- isCoverageEnabled

  case cmd of
    Compile{} ->
      let sanitizedOutput = sanitizeOutputPath cmd
      in  case sanitizedOutput of
            Right s -> runCompilation cmd { compileOutput = s } coverage
            Left  e -> putStrLn e

    Test entrypoint coverage -> runTests entrypoint coverage

    Install                  -> runPackageInstaller

    New path                 -> runPackageGenerator path

    Doc path                 -> runDocumentationGenerator path

    Run path args            -> runRun path args


runRun :: FilePath -> [String] -> IO ()
runRun input args = do
  if ".mad" `isSuffixOf` input then runSingleModule input args else runPackage input args

runFolder :: FilePath
runFolder = ".run/"

runPackage :: FilePath -> [String] -> IO ()
runPackage package args = do
  currentDir <- getCurrentDirectory
  let madlibDotJsonPath = joinPath [currentDir, "madlib.json"]

  parsedMadlibDotJson <- MadlibDotJSON.load PathUtils.defaultPathUtils madlibDotJsonPath

  case parsedMadlibDotJson of
    Left  e -> putStrLn e

    Right MadlibDotJSON.MadlibDotJSON { MadlibDotJSON.dependencies = maybeDeps } -> case maybeDeps of
      Just dependencies -> case M.lookup package dependencies of
        Just url ->
          let sanitizedPackageUrl      = URL.sanitize url
              packagePath              = joinPath ["madlib_modules", sanitizedPackageUrl]
              packageMadlibDotJsonPath = joinPath [packagePath, "madlib.json"]
          in  do
                parsedMadlibDotJson <- MadlibDotJSON.load PathUtils.defaultPathUtils packageMadlibDotJsonPath
                case parsedMadlibDotJson of
                  Left e -> putStrLn e

                  Right dotJ@MadlibDotJSON.MadlibDotJSON { MadlibDotJSON.bin = Just bin } ->
                    let exePath = joinPath [packagePath, bin]
                    in  do
                          let baseRunFolder  = joinPath [packagePath, runFolder]
                              compileCommand = Compile { compileInput         = exePath
                                                       , compileOutput        = baseRunFolder
                                                       , compileConfig        = "madlib.json"
                                                       , compileVerbose       = False
                                                       , compileDebug         = False
                                                       , compileBundle        = False
                                                       , compileOptimize      = False
                                                       , compileTarget        = TNode
                                                       , compileJson          = False
                                                       , compileTestFilesOnly = False
                                                       }

                          runCompilation compileCommand False
                          let target = joinPath [baseRunFolder, dropExtension (takeFileName bin) <> ".mjs"]
                          callCommand $ "node " <> target <> " " <> unwords args

                  _ -> putStrLn "That package doesn't have any executable!"

        Nothing ->
          putStrLn "Package not found, install it first, or if you did, make sure that you used the right name!"

      Nothing -> putStrLn "It seems that you have no dependency installed at the moment!"

runSingleModule :: FilePath -> [String] -> IO ()
runSingleModule input args = do
  let compileCommand = Compile { compileInput         = input
                               , compileOutput        = runFolder
                               , compileConfig        = "madlib.json"
                               , compileVerbose       = False
                               , compileDebug         = False
                               , compileBundle        = False
                               , compileOptimize      = False
                               , compileTarget        = TNode
                               , compileJson          = False
                               , compileTestFilesOnly = False
                               }

  runCompilation compileCommand False
  let target = runFolder <> (takeBaseName . takeFileName $ input) <> ".mjs"
  callCommand $ "node " <> target <> " " <> unwords args



solveASTsForDoc :: [FilePath] -> IO (Either CompilationError [(Slv.AST, [DocString.DocString])])
solveASTsForDoc []         = return $ Right []
solveASTsForDoc (fp : fps) = do
  canonicalEntrypoint <- canonicalizePath fp
  astTable            <- buildASTTable mempty canonicalEntrypoint
  let (canTable, _) = case astTable of
        Right table -> Can.runCanonicalization TNode Can.initialEnv table canonicalEntrypoint
        Left e -> (Left e, [])

  rootPath <- canonicalizePath $ computeRootPath fp

  let entryAST         = canTable >>= flip Can.findAST canonicalEntrypoint
      resolvedASTTable = case (entryAST, canTable) of
        (Right ast, Right table) -> do
          runExcept (runStateT (solveTable table ast) InferState { count = 0, errors = [] })
        (_     , Left e) -> Left e
        (Left e, _     ) -> Left $ CompilationError (ImportNotFound rootPath) NoContext

  case resolvedASTTable of
    Left  e          -> return $ Left e

    Right (table, _) -> case M.lookup canonicalEntrypoint table of
      Just ast -> do
        fileContent <- readFile fp
        let docStrings = DocString.parse fileContent
        case docStrings of
          Right ds -> do
            next <- solveASTsForDoc fps
            return $ ([(ast, ds)] ++) <$> next
          Left _ -> do
            next <- solveASTsForDoc fps
            return $ ([(ast, [])] ++) <$> next

      Nothing -> solveASTsForDoc fps


runDocumentationGenerator :: FilePath -> IO ()
runDocumentationGenerator fp = do
  let ext = takeExtension fp
  filepaths <- case ext of
    ".mad"     -> return [fp]
    '.' : rest -> putStrLn ("Invalid file extension '" <> ext <> "'") >> return []
    _          -> do
      paths <- getDirectoryContents fp
      let filtered = (\file -> joinPath [fp, file]) <$> filter ((== ".mad") . takeExtension) paths
      return filtered

  asts <- solveASTsForDoc filepaths
  case asts of
    Right asts' -> putStrLn $ generateASTsDoc asts'

    Left  e     -> putStrLn $ ppShow e


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
      fndas       = rstrip $ unlines $ (\Function { line, name } -> "FNDA:0" <> "," <> name) <$> functions
      fnf         = "FNF:" <> show (length functions)
      fnh         = "FNH:0"
      das         = rstrip $ unlines $ (\Line { line } -> "DA:" <> show line <> ",0") <$> lines
      lf          = "LF:" <> show (length lines)
      lh          = "LH:0"
      endOfRecord = "end_of_record"
  in  rstrip $ unlines [tn, sf, fns, fndas, fnf, fnh, das, lf, lh, endOfRecord]

runPackageInstaller :: IO ()
runPackageInstaller = do
  executablePath              <- getExecutablePath
  packageInstallerPath        <- try $ getEnv "PKG_INSTALLER_PATH"
  packageInstallerPathChecked <- case (packageInstallerPath :: Either IOError String) of
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
  testOutput <- try $ callCommand $ "node " <> testRunnerPathChecked <> " " <> entrypoint
  case (testOutput :: Either IOError ()) of
    Left  e -> return ()
    Right a -> return ()

compilationBlackList :: [FilePath]
compilationBlackList = ["madlib_modules", "node_modules"]

getFilesToCompile :: Bool -> FilePath -> IO [FilePath]
getFilesToCompile testsOnly entrypoint = case takeExtension entrypoint of
  ".mad"     -> return [entrypoint]
  '.' : rest -> putStrLn ("Invalid file extension '" <> ('.' : rest) <> "'") >> return []
  _          -> do
    paths <- getDirectoryContents entrypoint
    let fullPaths =
          (\file -> joinPath [entrypoint, file])
            <$> filter (\p -> p /= "." && p /= ".." && not (any (`isSuffixOf` p) compilationBlackList)) paths
    let filtered = if not testsOnly
          then filter ((== ".mad") . takeExtension) fullPaths
          else filter (isSuffixOf ".spec.mad") fullPaths

    subFolders <- filterM doesDirectoryExist fullPaths
    next       <- mapM (getFilesToCompile testsOnly) subFolders
    return $ filtered ++ concat next

runCompilation :: Command -> Bool -> IO ()
runCompilation opts@(Compile entrypoint outputPath config verbose debug bundle optimized target json testsOnly) coverage
  = do
    canonicalEntrypoint <- canonicalizePath entrypoint
    sourcesToCompile    <- getFilesToCompile testsOnly canonicalEntrypoint
    astTable            <- buildManyASTTables mempty sourcesToCompile
    let (canTable, warnings) = case astTable of
          Right table -> Can.canonicalizeMany target Can.initialEnv table sourcesToCompile
          Left e -> (Left e, [])

    unless json $ do
      formattedWarnings <- mapM (Explain.formatWarning readFile json) warnings
      let fullWarning = intercalate "\n\n\n" formattedWarnings
      putStrLn fullWarning

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
            formattedErr <- Explain.format readFile json err
            putStrLn $ CompileJson.compileASTTable [(err, formattedErr)] formattedWarnings mempty
          else do
            unless (null warnings) (putStrLn "\n")
            Explain.format readFile json err >>= putStrLn >> exitFailure
      Right (table, inferState) ->
        let errs      = errors inferState
            hasErrors = not (null errs)
        in  if json then do
              formattedWarnings <- mapM (\warning -> (warning, ) <$> Explain.formatWarning readFile json warning) warnings
              formattedErrors <- mapM (\err -> (err, ) <$> Explain.format readFile json err) errs
              putStrLn $ CompileJson.compileASTTable formattedErrors formattedWarnings table
            else if hasErrors then do
              unless (null warnings) (putStrLn "\n")
              formattedErrors <- mapM (Explain.format readFile json) errs
              let fullError = intercalate "\n\n\n" formattedErrors
              putStrLn fullError >> exitFailure
            else do
              when coverage $ do
                runCoverageInitialization rootPath table

              let optimizedTable = optimizeTable optimized table

              generate opts { compileInput = canonicalEntrypoint } coverage rootPath optimizedTable sourcesToCompile

              when bundle $ do
                let entrypointOutputPath =
                      computeTargetPath (takeDirectory outputPath <> "/.bundle") rootPath canonicalEntrypoint

                bundled <- runBundle outputPath entrypointOutputPath
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

runBundle :: FilePath -> FilePath -> IO (Either String (String, String))
runBundle dest entrypointCompiledPath = do
  rollupPath        <- try $ getEnv "ROLLUP_PATH"
  rollupPathChecked <- case (rollupPath :: Either IOError String) of
    Left _ -> do
      r <-
        try (readProcessWithExitCode "rollup" ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left  _ -> return $ Left rollupNotFoundMessage
        Right _ -> return $ Right "rollup"
    Right p -> do
      r <- try (readProcessWithExitCode p ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
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
          -- Right (_, _, stderr)  -> return $ Left stderr
    Left e -> return $ Left e


generate :: Command -> Bool -> FilePath -> Opt.Table -> [FilePath] -> IO ()
generate options coverage rootPath table sourcesToCompile = do
  let outputPath = compileOutput options
      bundle     = compileBundle options
      optimized  = compileOptimize options
      target     = compileTarget options
  (head <$>) <$> mapM (generateAST options coverage rootPath sourcesToCompile) $ M.elems table
  writeFile (takeDirectory outputPath <> (if bundle then "/.bundle" else "") <> "/__internals__.mjs")
    $ generateInternalsModuleContent target optimized coverage


generateAST :: Command -> Bool -> FilePath -> [FilePath] -> Opt.AST -> IO ()
generateAST options coverage rootPath sourcesToCompile ast@Opt.AST { Opt.apath = Just path } = do
  let internalsPath = case stripPrefix rootPath path of
        Just s ->
          let dirs = splitDirectories (takeDirectory s)
              minus
                | "prelude/__internal__" `isInfixOf` path = if "prelude/__internal__" `isInfixOf` rootPath then 0 else 2
                | "madlib_modules" `isInfixOf` path && not (rootPath `isPrefixOf` path) = -2
                | otherwise = 1
              dirLength = length dirs - minus
          in  joinPath $ ["./"] <> replicate dirLength ".." <> ["__internals__.mjs"]
        Nothing -> "./__internals__.mjs"

  let entrypointPath     = if path `elem` sourcesToCompile then path else compileInput options
      outputPath         = compileOutput options
      bundle             = compileBundle options
      optimized          = compileOptimize options
      target             = compileTarget options
      computedOutputPath = if bundle
        then computeTargetPath (takeDirectory outputPath <> "/.bundle") rootPath path
        else computeTargetPath (takeDirectory outputPath) rootPath path

  createDirectoryIfMissing True $ takeDirectory computedOutputPath
  writeFile computedOutputPath $ compile
    CompileJS.initialEnv
    (CompilationConfig rootPath path entrypointPath computedOutputPath coverage optimized target internalsPath)
    ast
