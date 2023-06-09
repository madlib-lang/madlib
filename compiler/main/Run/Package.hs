{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Run.Package where

import           System.Directory              ( canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory )
import           System.FilePath               ( takeDirectory, joinPath )
import           Text.Show.Pretty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLChar8
import qualified Data.List                     as List
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder
import           Data.Version
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8    ( unpack )


import qualified MadlibDotJson.MadlibDotJson        as MadlibDotJson
import qualified MadlibDotJson.MadlibVersion        as MadlibVersion
import qualified VersionLock.VersionLock            as VersionLock
import           Error.Error
import           Error.Warning
import qualified AST.Canonical                      as Can
import qualified Canonicalize.AST                   as Can
import qualified Canonicalize.Env                   as Can
import qualified AST.Solved                         as Slv
import qualified Infer.AST                          as Slv
import           Infer.Type
import           Infer.Instantiate
import qualified AST.Source                         as Src
import qualified Parse.Madlib.AST                   as Src
import           Run.Target
import           Run.PackageHash
import           Run.CommandLine
import           Utils.Hash
import           Utils.Version
import           VersionLock.PublicAPI
import           Utils.Tuple
import           Explain.Format
import qualified Utils.PathUtils              as PathUtils
import           Utils.Path
import           System.FilePath.Posix
import qualified Rock
import qualified Driver.Query                 as Query
import           Control.Monad
import qualified Driver
import           Run.Options
import Utils.PathUtils (defaultPathUtils)
import Run.OptimizationLevel


typeCheckTask :: FilePath -> Rock.Task Query.Query Slv.Table
typeCheckTask path = do
  pathsToBuild <- Rock.fetch $ Query.ModulePathsToBuild path
  solvedModules <- forM pathsToBuild $ \pathToBuild -> do
    (ast, _) <- Rock.fetch $ Query.SolvedASTWithEnv pathToBuild
    return (pathToBuild, ast)
  return $ Map.fromList solvedModules


typeCheckMain :: Target -> FilePath -> IO (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheckMain target main = do
  state      <- Driver.initialState
  rootPath   <- canonicalizePath "./"
  outputPath <- canonicalizePath "./build"
  let options =
        Options
          { optPathUtils = defaultPathUtils
          , optEntrypoint = main
          , optRootPath = rootPath
          , optOutputPath = outputPath
          , optTarget = target
          , optOptimized = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = True
          , optInsertInstancePlaholders = True
          , optMustHaveMain = False
          , optOptimizationLevel = O1
          , optDebug = False
          }
  (table, warnings, errors) <- Driver.runIncrementalTask state options [] mempty Driver.Don'tPrune (typeCheckTask main)
  if null errors then
    return (Right table, warnings)
  else
    return (Left errors, warnings)


bumpVersion :: Bool -> APIChange -> Version -> Version
bumpVersion rebuild apiChange version = case (apiChange, version) of
  (Major, Version [_, minor, patch] _) | rebuild && minor == 0 && patch == 0 ->
    version

  (Major, Version [major, _, _] _) ->
    Version [major + 1, 0, 0] []

  (Minor, Version [_, _, patch] _) | rebuild && patch == 0 ->
    version

  (Minor, Version [major, minor, _] _) ->
    Version [major, minor + 1, 0] []

  (Patch, Version [_, _, _] _) | rebuild ->
    version

  (Patch, Version [major, minor, patch] _) ->
    Version [major, minor, patch + 1] []


performBuild :: Bool -> Either VersionLock.ReadError VersionLock.VersionLock -> Maybe Version -> String -> String -> (Slv.AST, Slv.Table) -> (Slv.AST, Slv.Table) -> IO (Either String (Version, VersionLock.VersionLock))
performBuild rebuild eitherVersionLock parsedVersion hashedVersion projectHash (jsAst, jsTable) (llvmAst, llvmTable) =
  case (eitherVersionLock, parsedVersion) of
    -- if there is no version.lock file we generate the initial one with version 0.0.1
    (Left VersionLock.FileNotFound, _) -> do
      let initialVersionHash = hash $ BLChar8.pack "0.0.1"
      let jsApi              = buildAPI jsAst jsTable
      let llvmApi            = buildAPI llvmAst llvmTable
      let  versionLock       = VersionLock.VersionLock
            { VersionLock.versionHash = initialVersionHash
            , VersionLock.buildHash   = projectHash
            , VersionLock.jsApi       = jsApi
            , VersionLock.llvmApi     = llvmApi
            }
      return $ Right (Version [0, 0, 1] [], versionLock)

    (Left VersionLock.ReadError, _) ->
      return $ Left "An error occured while reading the version.lock file"

    (Right VersionLock.VersionLock { VersionLock.versionHash, VersionLock.buildHash, VersionLock.jsApi, VersionLock.llvmApi }, Just version) ->
      if buildHash == projectHash then
        return $ Left "Project has not changed, nothing to do."
      else if hashedVersion /= versionHash then
        return $ Left "It seems that you modified the version in madlib.json manually"
      else do
        let newJSApi        = buildAPI jsAst jsTable
        let newLLVMApi      = buildAPI llvmAst llvmTable
        let nextVersion     = bumpVersion rebuild (computeAPIChange jsApi newJSApi) version
        let nextVersion'    = bumpVersion rebuild (computeAPIChange llvmApi newLLVMApi) version
        let nextVersion''   = max nextVersion nextVersion'
        let nextVersionHash = hash $ BLChar8.pack $ showVersion nextVersion''

        let nextVersionLock = VersionLock.VersionLock
              { VersionLock.versionHash = nextVersionHash
              , VersionLock.buildHash   = projectHash
              , VersionLock.jsApi       = newJSApi
              , VersionLock.llvmApi     = newLLVMApi
              }
        return $ Right (nextVersion, nextVersionLock)


runBuildPackage :: Bool -> IO ()
runBuildPackage rebuild = do
  putStrLn "Building package..."
  madlibDotJson <- MadlibDotJson.loadCurrentMadlibDotJson

  case madlibDotJson of
    Left _ -> putStrLn "The package must have a madlib.json file"

    Right goodMadlibDotJson@MadlibDotJson.MadlibDotJson { MadlibDotJson.main, MadlibDotJson.version = Just version } -> do
      let hashedVersion = hash $ BLChar8.pack version
      let parsedVersion = MadlibVersion.parse version

      canonicalMain        <- canonicalizePath main
      currentDirectoryPath <- getCurrentDirectory
      versionLock          <- VersionLock.loadCurrentVersionLock
      projectHash          <- generatePackageHash currentDirectoryPath
      (typeCheckedJS, _)   <- typeCheckMain TNode canonicalMain
      (typeCheckedLLVM, _) <- typeCheckMain TLLVM canonicalMain

      case (typeCheckedJS, typeCheckedLLVM) of
        (Left _, _) ->
          -- TODO: display errors
          putStrLn "Compilation errors, please fix them before building the package"

        (_, Left _) ->
          -- TODO: display errors
          putStrLn "Compilation errors, please fix them before building the package"

        (Right solvedJSTable, Right solvedLLVMTable) -> do
          let (Just mainJSAST) = Map.lookup canonicalMain solvedJSTable
          let (Just mainLLVMAST) = Map.lookup canonicalMain solvedLLVMTable
          processed <- performBuild rebuild versionLock parsedVersion hashedVersion projectHash (mainJSAST, solvedJSTable) (mainLLVMAST, solvedLLVMTable)
          case processed of
            Left e -> putStrLn e

            Right (version, versionLock) -> do
              VersionLock.save "version.lock" versionLock
              MadlibDotJson.save "madlib.json" goodMadlibDotJson { MadlibDotJson.version = Just $ showVersion version }
              putStrLn $ "Version updated to '" <> showVersion version <> "'. Do not forget to version control the version.lock file!"

      return ()


runPackage :: PackageSubCommand -> Bool -> IO ()
runPackage subCommand rebuild = case subCommand of
  NoPackageSubCommand ->
    runBuildPackage rebuild

  GenerateHash input  ->
    runGeneratePackageHash input
