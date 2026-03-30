{-# LANGUAGE DeriveGeneric #-}

module Fuzz.Engine where

import Control.Monad (filterM, forM, when)
import Data.Aeson (decodeFileStrict', encodeFile)
import Data.List (isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fuzz.Generator
import Fuzz.Types
import Run.Target
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.Process (CreateProcess(..), proc, createProcess, waitForProcess, terminateProcess, StdStream(..))
import System.Timeout (timeout)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.Async (async, wait)


runFuzzCampaign :: RunOptions -> IO Int
runFuzzCampaign opts = do
  Dir.createDirectoryIfMissing True (roSaveArtifactsDir opts)
  putStrLn $ "Running fuzz campaign: seed=" <> show (roSeed opts) <> ", runs=" <> show (roRuns opts)
  results <- forM [0 .. roRuns opts - 1] $ \runIndex -> do
    let seed = roSeed opts + runIndex
    result <- runSingleCase opts runIndex seed
    when (rrClassification result /= Match) $ do
      _ <- saveArtifact opts result
      pure ()
    putStrLn $ "run " <> show runIndex <> ": " <> show (rrClassification result)
    pure result

  let failures = length $ filter (\r -> rrClassification r /= Match) results
  putStrLn $ "Fuzz campaign completed. failures=" <> show failures
  pure failures


runSingleCase :: RunOptions -> Int -> Int -> IO RunResult
runSingleCase opts runIndex seed = do
  let model = generateProgramModel seed (roProfile opts) (roMaxSize opts)
  source <- pure (renderProgram model)
  (llvmOutcome, nodeOutcome) <- runDifferential opts runIndex source
  let initialClass = classifyResult llvmOutcome nodeOutcome
  if initialClass == Match || not (roShrink opts) then
    pure RunResult
      { rrSeed = seed
      , rrRunIndex = runIndex
      , rrProfile = roProfile opts
      , rrSource = source
      , rrReducedSource = Nothing
      , rrLLVM = llvmOutcome
      , rrNode = nodeOutcome
      , rrClassification = initialClass
      , rrShrinkSteps = []
      }
  else do
    let stillFail candidate = do
          let candidateSource = renderProgram candidate
          (llvmO, nodeO) <- runDifferential opts runIndex candidateSource
          pure (classifyResult llvmO nodeO /= Match)
    (reducedModel, history) <- greedyShrink stillFail model
    let reducedSource = renderProgram reducedModel
    (llvmReduced, nodeReduced) <- runDifferential opts runIndex reducedSource
    pure RunResult
      { rrSeed = seed
      , rrRunIndex = runIndex
      , rrProfile = roProfile opts
      , rrSource = source
      , rrReducedSource = Just reducedSource
      , rrLLVM = llvmReduced
      , rrNode = nodeReduced
      , rrClassification = classifyResult llvmReduced nodeReduced
      , rrShrinkSteps = history
      }


saveArtifact :: RunOptions -> RunResult -> IO FilePath
saveArtifact opts result = do
  ts <- round <$> getPOSIXTime
  let dirName = "seed-" <> show (rrSeed result) <> "-run-" <> show (rrRunIndex result) <> "-" <> show ts
      artifactDir = roSaveArtifactsDir opts </> dirName
      sourcePath = artifactDir </> "Entrypoint.mad"
      reducedPath = artifactDir </> "Reduced.mad"
      jsonPath = artifactDir </> "artifact.json"
  Dir.createDirectoryIfMissing True artifactDir
  writeFile sourcePath (rrSource result)
  case rrReducedSource result of
    Just s -> writeFile reducedPath s
    Nothing -> pure ()
  encodeFile jsonPath result
  pure jsonPath


replayArtifact :: RunOptions -> FilePath -> IO Classification
replayArtifact opts artifactPath = do
  decoded <- decodeFileStrict' artifactPath
  case decoded of
    Nothing -> do
      putStrLn $ "Could not parse artifact: " <> artifactPath
      pure BothFailed
    Just artifact -> do
      let source = maybe (rrSource artifact) id (rrReducedSource artifact)
      (llvmOutcome, nodeOutcome) <- runDifferential opts (rrRunIndex artifact) source
      let classification = classifyResult llvmOutcome nodeOutcome
      putStrLn $ "Replay classification: " <> show classification
      pure classification


promoteArtifact :: FilePath -> FilePath -> IO FilePath
promoteArtifact artifactPath testCaseName = do
  decoded <- decodeFileStrict' artifactPath
  case decoded of
    Nothing ->
      fail $ "Could not parse artifact: " <> artifactPath
    Just artifact -> do
      let source = maybe (rrSource artifact) id (rrReducedSource artifact)
          targetDir = "compiler/test/Blackbox/test-cases" </> testCaseName
          expected = chooseOracleOutput artifact
      Dir.createDirectoryIfMissing True targetDir
      writeFile (targetDir </> "Entrypoint.mad") source
      writeFile (targetDir </> "expected-llvm") expected
      writeFile (targetDir </> "expected-js") expected
      writeFile (targetDir </> "FUZZ_PROMOTION.md") $
        unlines
          [ "seed: " <> show (rrSeed artifact)
          , "classification: " <> show (rrClassification artifact)
          , "artifact: " <> artifactPath
          ]
      pure targetDir


chooseOracleOutput :: RunResult -> String
chooseOracleOutput artifact
  | not (null (boStdout (rrNode artifact))) = boStdout (rrNode artifact)
  | otherwise = boStdout (rrLLVM artifact)


classifyResult :: BackendOutcome -> BackendOutcome -> Classification
classifyResult llvmOutcome nodeOutcome
  | not (boCompileOk llvmOutcome) && not (boCompileOk nodeOutcome) = BothFailed
  | not (boCompileOk llvmOutcome) = LLVMFailure
  | not (boCompileOk nodeOutcome) = NodeFailure
  | boTimedOut llvmOutcome && boTimedOut nodeOutcome = BothFailed
  | boTimedOut llvmOutcome = LLVMFailure
  | boTimedOut nodeOutcome = NodeFailure
  | not (boRunOk llvmOutcome) && not (boRunOk nodeOutcome) = BothFailed
  | not (boRunOk llvmOutcome) = LLVMFailure
  | not (boRunOk nodeOutcome) = NodeFailure
  | isAssertFail llvmOutcome || isAssertFail nodeOutcome = AssertionFailure
  | boStdout llvmOutcome /= boStdout nodeOutcome = DifferentialMismatch
  | boExitCode llvmOutcome /= boExitCode nodeOutcome = DifferentialMismatch
  | otherwise = Match


isAssertFail :: BackendOutcome -> Bool
isAssertFail o = "ASSERT_FAIL:" `isInfixOf` boStdout o || "ASSERT_FAIL:" `isInfixOf` boStderr o


runDifferential :: RunOptions -> Int -> String -> IO (BackendOutcome, BackendOutcome)
runDifferential opts runIndex source = do
  llvmOutcome <- runForTarget opts runIndex TLLVM source
  nodeOutcome <- runForTarget opts runIndex TNode source
  pure (llvmOutcome, nodeOutcome)


runForTarget :: RunOptions -> Int -> Target -> String -> IO BackendOutcome
runForTarget opts runIndex target source = do
  ts <- round . (* 1000000) <$> getPOSIXTime
  let backendName = case target of
        TLLVM -> "llvm"
        TNode -> "node"
        TBrowser -> "browser"
      rootDir = roSaveArtifactsDir opts </> ".tmp" </> ("run-" <> show runIndex <> "-" <> backendName <> "-" <> show ts)
      entrypoint = rootDir </> "Entrypoint.mad"
      outputPath = case target of
        TLLVM -> rootDir </> ".tests" </> "run"
        _ -> rootDir </> ".tests" </> "Entrypoint.mjs"
      compileTarget = case target of
        TLLVM -> "llvm"
        _ -> "node"
      compileArgs =
        [ "compile"
        , "-i"
        , entrypoint
        , "-o"
        , outputPath
        , "-t"
        , compileTarget
        , "--O3"
        ]
      runCommand =
        case target of
          TLLVM -> outputPath
          _ -> "node"
      runArgs =
        case target of
          TLLVM -> []
          _ -> [outputPath]

  Dir.createDirectoryIfMissing True rootDir
  writeFile entrypoint source

  compileRes <- runProcessWithTimeout (roTimeoutMs opts) Nothing "madlib" compileArgs
  if rprTimedOut compileRes then
      pure BackendOutcome
        { boBackend = backendName
        , boCompileOk = False
        , boRunOk = False
        , boTimedOut = True
        , boExitCode = Nothing
        , boStdout = rprStdout compileRes
        , boStderr = rprStderr compileRes
        , boCompileOutput = "compile timed out"
        }
  else if rprExitCode compileRes /= ExitSuccess then
    pure BackendOutcome
      { boBackend = backendName
      , boCompileOk = False
      , boRunOk = False
      , boTimedOut = False
      , boExitCode = Just $ exitCodeToInt (rprExitCode compileRes)
      , boStdout = rprStdout compileRes
      , boStderr = rprStderr compileRes
      , boCompileOutput = rprStdout compileRes <> rprStderr compileRes
      }
  else do
    resolvedRunArgs <-
      case target of
        TNode -> do
          nodePath <- resolveNodeRunPath rootDir outputPath
          pure [nodePath]
        _ ->
          pure runArgs
    runRes <- runProcessWithTimeout (roTimeoutMs opts) (roRuntimeMemoryMb opts) runCommand resolvedRunArgs
    pure BackendOutcome
      { boBackend = backendName
      , boCompileOk = True
      , boRunOk = not (rprTimedOut runRes) && rprExitCode runRes == ExitSuccess
      , boTimedOut = rprTimedOut runRes
      , boExitCode = Just $ exitCodeToInt (rprExitCode runRes)
      , boStdout = rprStdout runRes
      , boStderr = rprStderr runRes
      , boCompileOutput = ""
      }


data ProcessResult
  = ProcessResult
    { rprExitCode :: ExitCode
    , rprStdout :: String
    , rprStderr :: String
    , rprTimedOut :: Bool
    }


runProcessWithTimeout :: Int -> Maybe Int -> FilePath -> [String] -> IO ProcessResult
runProcessWithTimeout timeoutMs memoryMb cmd args = do
  let (command, arguments) = case memoryMb of
        Nothing ->
          (cmd, args)
        Just mb ->
          ( "bash"
          , [ "-lc"
            , "ulimit -v "
                <> show (mb * 1024)
                <> "; exec "
                <> shellQuote cmd
                <> " "
                <> unwords (shellQuote <$> args)
            ]
          )
      cp = (proc command arguments) { std_out = CreatePipe, std_err = CreatePipe }
  (_inH, outH, errH, ph) <- createProcess cp
  outReader <- async $ maybe (pure "") (\h -> B8.unpack <$> BS.hGetContents h) outH
  errReader <- async $ maybe (pure "") (\h -> B8.unpack <$> BS.hGetContents h) errH
  waiter <- async (waitForProcess ph)
  done <- timeout (timeoutMs * 1000) (wait waiter)
  case done of
    Nothing -> do
      terminateProcess ph
      _ <- waitForProcess ph
      outContents <- wait outReader
      errContents <- wait errReader
      pure ProcessResult
        { rprExitCode = ExitFailure 124
        , rprStdout = outContents
        , rprStderr = errContents
        , rprTimedOut = True
        }
    Just ec -> do
      outContents <- wait outReader
      errContents <- wait errReader
      pure ProcessResult
        { rprExitCode = ec
        , rprStdout = outContents
        , rprStderr = errContents
        , rprTimedOut = False
        }


shellQuote :: String -> String
shellQuote s =
  "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"


resolveNodeRunPath :: FilePath -> FilePath -> IO FilePath
resolveNodeRunPath rootDir preferred = do
  preferredExists <- Dir.doesFileExist preferred
  if preferredExists then
    pure preferred
  else do
    discovered <- findFileByName rootDir "Entrypoint.mjs"
    pure $ maybe preferred id discovered


findFileByName :: FilePath -> FilePath -> IO (Maybe FilePath)
findFileByName dir targetName = do
  exists <- Dir.doesDirectoryExist dir
  if not exists then
    pure Nothing
  else do
    entries <- Dir.listDirectory dir
    let fullPaths = (\name -> dir </> name) <$> entries
    files <- filterM Dir.doesFileExist fullPaths
    case filter (\p -> FilePath.takeFileName p == targetName) files of
      (p : _) ->
        pure (Just p)
      [] -> do
        dirs <- filterM Dir.doesDirectoryExist fullPaths
        searchDirs dirs
 where
  searchDirs [] = pure Nothing
  searchDirs (d : ds) = do
    found <- findFileByName d targetName
    case found of
      Just p -> pure (Just p)
      Nothing -> searchDirs ds

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
