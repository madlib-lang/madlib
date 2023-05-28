module Run.Options where

import Utils.PathUtils (PathUtils)
import Run.Target (Target)
import Run.OptimizationLevel (OptimizationLevel)


data Options
  = Options
    { optPathUtils :: PathUtils
    , optEntrypoint :: FilePath
    , optRootPath :: FilePath
    , optOutputPath :: FilePath
    , optTarget :: Target
    , optOptimized :: Bool
    , optBundle :: Bool
    , optCoverage :: Bool
    , optGenerateDerivedInstances :: Bool
    , optInsertInstancePlaholders :: Bool
    , optMustHaveMain :: Bool
    , optOptimizationLevel :: OptimizationLevel
    }
