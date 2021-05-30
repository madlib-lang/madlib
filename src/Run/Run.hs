{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
module Run.Run where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           System.FilePath                ( takeBaseName
                                                , takeFileName
                                                , dropExtension
                                                , joinPath
                                                )
import           System.Directory               ( getCurrentDirectory )
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

  parsedMadlibDotJson <- MadlibDotJson.load PathUtils.defaultPathUtils madlibDotJsonPath

  case parsedMadlibDotJson of
    Left  e -> putStrLn e

    Right MadlibDotJson.MadlibDotJson { MadlibDotJson.dependencies = maybeDeps } -> case maybeDeps of
      Just dependencies -> case M.lookup package dependencies of
        Just url ->
          let sanitizedPackageUrl      = URL.sanitize url
              packagePath              = joinPath ["madlib_modules", sanitizedPackageUrl]
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




