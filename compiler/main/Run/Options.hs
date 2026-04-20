module Run.Options where

import Utils.PathUtils (PathUtils)
import Run.Target (Target)
import Run.OptimizationLevel (OptimizationLevel)
import Run.SourceMapMode (SourceMapMode)
import Run.ErrorFormat (ErrorFormat(..))
import Run.PGOMode (PGOMode(..))


data Options
  = Options
    { optPathUtils :: PathUtils
    , optEntrypoint :: FilePath
    , optRootPath :: FilePath
    , optOutputPath :: FilePath
    , optTarget :: Target
    , optOptimized :: Bool
    , optDebug :: Bool
    , optBundle :: Bool
    , optCoverage :: Bool
    , optGenerateDerivedInstances :: Bool
    , optInsertInstancePlaholders :: Bool
    , optMustHaveMain :: Bool
    , optParseOnly :: Bool
    , optOptimizationLevel :: OptimizationLevel
    , optLspMode :: Bool
    , optEmitLLVM :: Bool
    , optSourceMaps :: SourceMapMode
    , optErrorFormat :: ErrorFormat
    , optPGOMode :: PGOMode
    }
