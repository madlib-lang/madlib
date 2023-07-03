{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
module Run.Run where

import qualified Data.Map                      as M
import           System.FilePath                ( takeBaseName
                                                , takeFileName
                                                , dropExtension
                                                , joinPath
                                                , makeRelative
                                                , dropFileName
                                                )
import           System.Directory               ( canonicalizePath )
import           System.Process
import           Data.List                      ( isSuffixOf )
import qualified MadlibDotJson.MadlibDotJson   as MadlibDotJson
import qualified Utils.PathUtils               as PathUtils
import           Run.Target
import           Run.Compile
import           Run.CommandLine
import           Utils.Path (computeTargetPath)
import           Run.OptimizationLevel
import           System.IO (stdout)
import           System.IO.Silently
import           Run.Target


runRun :: Target -> FilePath -> [String] -> IO ()
runRun target input args = do
  if ".mad" `isSuffixOf` input then runSingleModule target input args else runRunPackage target input args

runFolder :: FilePath
runFolder = "build/"

runRunPackage :: Target -> FilePath -> [String] -> IO ()
runRunPackage target package args =
  let packagePath              = joinPath ["madlib_modules", package]
      packageMadlibDotJsonPath = joinPath [packagePath, "madlib.json"]
  in  do
        parsedMadlibDotJson <- MadlibDotJson.load PathUtils.defaultPathUtils packageMadlibDotJsonPath
        case parsedMadlibDotJson of
          Left e -> putStrLn e

          Right MadlibDotJson.MadlibDotJson { MadlibDotJson.bin = Just bin } -> do
            let exePath = joinPath [packagePath, bin]
            let baseRunFolder  = joinPath [packagePath, runFolder]
            let llvmOutputPath = baseRunFolder <> "run"
                compileCommand =
                  Compile { compileInput         = exePath
                          , compileOutput        = if target == TNode then baseRunFolder else llvmOutputPath
                          , compileConfig        = "madlib.json"
                          , compileVerbose       = False
                          , compileDebug         = False
                          , compileBundle        = False
                          , compileOptimize      = False
                          , compileTarget        = target
                          , compileWatch         = False
                          , compileCoverage      = False
                          , compileOptimizationLevel = O1
                          }

            hSilence [stdout] $ runCompilation compileCommand
            entrypoint <- canonicalizePath exePath
            rootPath <- canonicalizePath "./"
            outputPath <- canonicalizePath baseRunFolder
            let jsOutputPath = computeTargetPath outputPath rootPath entrypoint
            if target == TNode then
              callCommand $ "node " <> jsOutputPath <> " " <> unwords (map show args)
            else
              callCommand $ llvmOutputPath <> " " <> unwords (map show args)

          _ -> putStrLn "That package doesn't have any executable!"


runSingleModule :: Target -> FilePath -> [String] -> IO ()
runSingleModule target input args = do
  let llvmOutputPath = runFolder <> "run"
  let compileCommand =
        Compile
          { compileInput         = input
          , compileOutput        = if target == TNode then runFolder else llvmOutputPath
          , compileConfig        = "madlib.json"
          , compileVerbose       = False
          , compileDebug         = False
          , compileBundle        = False
          , compileOptimize      = False
          , compileTarget        = target
          , compileWatch         = False
          , compileCoverage      = False
          , compileOptimizationLevel = O1
          }

  hSilence [stdout] $ runCompilation compileCommand

  canEntrypoint    <- canonicalizePath input
  canCurrentFolder <- canonicalizePath "./"

  let fromRoot = makeRelative canCurrentFolder (dropFileName canEntrypoint)

  let jsOutputPath = joinPath [runFolder, fromRoot, (takeBaseName . takeFileName $ input) <> ".mjs"]
  if target == TNode then
    callCommand $ "node " <> jsOutputPath <> " " <> unwords (map (("\"" <>) . (<> "\"")) args)
    -- callCommand $ "node " <> jsOutputPath <> " " <> unwords (map show args)
  else
    callCommand $ llvmOutputPath <> " " <> unwords (map (("\"" <>) . (<> "\"")) args)
    -- callCommand $ llvmOutputPath <> " " <> unwords (map show args)
