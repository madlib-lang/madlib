{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
module Run.Run where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           System.FilePath                ( takeBaseName
                                                , takeFileName
                                                , dropExtension
                                                , joinPath, makeRelative, dropFileName
                                                )
import           System.Directory               ( getCurrentDirectory, canonicalizePath )
import           System.Process
import           Data.List                      ( isSuffixOf )
import qualified MadlibDotJson.MadlibDotJson   as MadlibDotJson
import qualified Utils.URL                     as URL
import qualified Utils.PathUtils               as PathUtils
import           Run.Target
import           Run.GenerateDocumentation
import           Run.Utils
import           Run.GeneratePackage
import           Run.Compile
import           Run.CommandLine
import           Run.PackageInstaller
import           Run.TestRunner
import Run.CommandLine (Command(compileWatch))


runRun :: FilePath -> [String] -> IO ()
runRun input args = do
  if ".mad" `isSuffixOf` input then runSingleModule input args else runRunPackage input args

runFolder :: FilePath
runFolder = ".run/"

runRunPackage :: FilePath -> [String] -> IO ()
runRunPackage package args =
  let packagePath              = joinPath ["madlib_modules", package]
      packageMadlibDotJsonPath = joinPath [packagePath, "madlib.json"]
  in  do
        parsedMadlibDotJson <- MadlibDotJson.load PathUtils.defaultPathUtils packageMadlibDotJsonPath
        case parsedMadlibDotJson of
          Left e -> putStrLn e

          Right dotJ@MadlibDotJson.MadlibDotJson { MadlibDotJson.bin = Just bin } ->
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
                                                , noCache              = False
                                                , compileWatch         = False
                                                }

                  runCompilation compileCommand False
                  let target = joinPath [baseRunFolder, dropExtension (takeFileName bin) <> ".mjs"]
                  callCommand $ "node " <> target <> " " <> unwords args

          _ -> putStrLn "That package doesn't have any executable!"


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
                               , noCache              = False
                               , compileWatch         = False
                               }

  runCompilation compileCommand False

  canEntrypoint    <- canonicalizePath input
  canCurrentFolder <- canonicalizePath "./"

  let fromRoot = makeRelative canCurrentFolder (dropFileName canEntrypoint)

  let target = joinPath [runFolder, fromRoot, (takeBaseName . takeFileName $ input) <> ".mjs"]
  callCommand $ "node " <> target <> " " <> unwords args




