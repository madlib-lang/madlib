module Run.Options where

import Utils.PathUtils (PathUtils)
import Run.Target (Target)


data Options
  = Options
    { optPathUtils :: PathUtils
    , optEntrypoint :: FilePath
    , optRootPath :: FilePath
    , optOutputPath :: FilePath
    , optTarget :: Target
    , optOptimized :: Bool
    }
