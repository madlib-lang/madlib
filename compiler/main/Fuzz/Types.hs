{-# LANGUAGE DeriveGeneric #-}

module Fuzz.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


data StressProfile
  = Balanced
  | PAPHeavy
  | AllocationHeavy
  | ClosureHeavy
  deriving (Eq, Show, Read, Generic)

instance FromJSON StressProfile
instance ToJSON StressProfile


data ProgramModel
  = ProgramModel
    { pmSeed :: Int
    , pmProfile :: StressProfile
    , pmConsts :: [Int]
    , pmListVals :: [Int]
    , pmUseClosure :: Bool
    , pmUsePap3 :: Bool
    , pmUsePap4 :: Bool
    , pmUseRecursion :: Bool
    , pmUseMap :: Bool
    , pmUseFilter :: Bool
    }
  deriving (Eq, Show, Generic)

instance FromJSON ProgramModel
instance ToJSON ProgramModel


data BackendOutcome
  = BackendOutcome
    { boBackend :: String
    , boCompileOk :: Bool
    , boRunOk :: Bool
    , boTimedOut :: Bool
    , boExitCode :: Maybe Int
    , boStdout :: String
    , boStderr :: String
    , boCompileOutput :: String
    }
  deriving (Eq, Show, Generic)

instance FromJSON BackendOutcome
instance ToJSON BackendOutcome


data Classification
  = Match
  | DifferentialMismatch
  | AssertionFailure
  | LLVMFailure
  | NodeFailure
  | BothFailed
  deriving (Eq, Show, Generic)

instance FromJSON Classification
instance ToJSON Classification


data RunResult
  = RunResult
    { rrSeed :: Int
    , rrRunIndex :: Int
    , rrProfile :: StressProfile
    , rrSource :: String
    , rrReducedSource :: Maybe String
    , rrLLVM :: BackendOutcome
    , rrNode :: BackendOutcome
    , rrClassification :: Classification
    , rrShrinkSteps :: [String]
    }
  deriving (Eq, Show, Generic)

instance FromJSON RunResult
instance ToJSON RunResult


data RunOptions
  = RunOptions
    { roSeed :: Int
    , roRuns :: Int
    , roProfile :: StressProfile
    , roTimeoutMs :: Int
    , roMaxSize :: Int
    , roShrink :: Bool
    , roSaveArtifactsDir :: FilePath
    , roRuntimeMemoryMb :: Maybe Int
    }
  deriving (Eq, Show)
