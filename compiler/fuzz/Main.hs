module Main where

import Options.Applicative
import System.Exit (exitFailure)
import Fuzz.Engine
import Fuzz.Types
import Control.Applicative ((<|>))


data Command
  = RunCampaign RunOptions
  | Replay RunOptions FilePath
  | Promote FilePath String


main :: IO ()
main = do
  command <- execParser parserInfo
  case command of
    RunCampaign opts -> do
      failures <- runFuzzCampaign opts
      if failures > 0 then exitFailure else pure ()
    Replay opts artifactPath -> do
      classification <- replayArtifact opts artifactPath
      if classification == Match then pure () else exitFailure
    Promote artifactPath caseName -> do
      created <- promoteArtifact artifactPath caseName
      putStrLn $ "Created blackbox case: " <> created


parserInfo :: ParserInfo Command
parserInfo =
  info (helper <*> parseCommand) $
    fullDesc
      <> progDesc "Differential fuzzing for madlib backends (LLVM + NodeJS)"


parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "run" (info (RunCampaign <$> parseRunOptions) (progDesc "Run fuzz campaign"))
      <> command "replay" (info parseReplay (progDesc "Replay a saved artifact"))
      <> command "promote" (info parsePromote (progDesc "Promote artifact to blackbox testcase"))
    )


parseReplay :: Parser Command
parseReplay =
  Replay
    <$> parseRunOptions
    <*> strOption
      ( long "artifact"
        <> short 'a'
        <> metavar "ARTIFACT_JSON"
        <> help "Path to a saved artifact.json"
      )


parsePromote :: Parser Command
parsePromote =
  Promote
    <$> strOption
      ( long "artifact"
        <> short 'a'
        <> metavar "ARTIFACT_JSON"
        <> help "Path to a saved artifact.json"
      )
    <*> strOption
      ( long "name"
        <> short 'n'
        <> metavar "TEST_CASE_NAME"
        <> help "New blackbox test-case folder name"
      )


parseRunOptions :: Parser RunOptions
parseRunOptions =
  RunOptions
    <$> option auto
      ( long "seed"
        <> value 1
        <> showDefault
        <> metavar "SEED"
        <> help "Initial seed"
      )
    <*> option auto
      ( long "runs"
        <> value 100
        <> showDefault
        <> metavar "RUNS"
        <> help "How many generated programs to run"
      )
    <*> option parseProfile
      ( long "profile"
        <> value Balanced
        <> showDefault
        <> metavar "PROFILE"
        <> help "Stress profile: balanced | pap-heavy | allocation-heavy | closure-heavy"
      )
    <*> option auto
      ( long "timeout-ms"
        <> value 4000
        <> showDefault
        <> metavar "TIMEOUT_MS"
        <> help "Per-backend compile/run timeout in milliseconds"
      )
    <*> option auto
      ( long "max-size"
        <> value 10
        <> showDefault
        <> metavar "MAX_SIZE"
        <> help "Controls generated input size"
      )
    <*> parseShrinkFlag
    <*> strOption
      ( long "save-artifacts-dir"
        <> value ".fuzz-artifacts"
        <> showDefault
        <> metavar "DIR"
        <> help "Directory where failing artifacts are saved"
      )
    <*> optional
      ( option auto
        ( long "runtime-memory-mb"
          <> metavar "MB"
          <> help "Apply runtime memory cap via ulimit -v"
        )
      )


parseProfile :: ReadM StressProfile
parseProfile = eitherReader $ \s ->
  case s of
    "balanced" -> Right Balanced
    "pap-heavy" -> Right PAPHeavy
    "allocation-heavy" -> Right AllocationHeavy
    "closure-heavy" -> Right ClosureHeavy
    _ ->
      Left "Invalid profile. Use one of: balanced, pap-heavy, allocation-heavy, closure-heavy."


parseShrinkFlag :: Parser Bool
parseShrinkFlag =
  flag'
    True
    ( long "shrink"
      <> help "Enable shrinking for failing programs (default)"
    )
    <|> flag'
      False
      ( long "no-shrink"
        <> help "Disable shrinking for failing programs"
      )
    <|> pure True
