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
    -- Maximum body-size (in AST nodes) for a top-level definition to be
    -- considered for cross-module inlining at O3.  Defaults to the value
    -- baked into Optimize.Inline (10).  Higher values inline more
    -- aggressively at the cost of code size; see GCC's `-finline-limit`
    -- and GHC's `-funfolding-use-threshold` for prior art.
    , optInlineThreshold :: Maybe Int
    }
