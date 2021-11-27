{-# LANGUAGE NamedFieldPuns #-}
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

parse :: FilePath -> IO (Either CompilationError Src.Table)
parse = Src.buildASTTable TNode mempty

canonicalize :: Src.Table -> FilePath -> (Either CompilationError Can.Table, [CompilationWarning])
canonicalize srcTable main =
  let (result, warnings) = Can.runCanonicalization mempty TAny Can.initialEnv srcTable main
  in  (fst <$> result, warnings)

typeCheck :: Can.Table -> FilePath -> (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheck canTable path = Slv.solveManyASTs' canTable [path]


typeCheckMain :: FilePath -> IO (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheckMain main = do
  parsed        <- parse main

  case parsed of
    Right srcTable -> case canonicalize srcTable main of
      (Left err, warnings)       -> return (Left [err], warnings)

      (Right canTable, warnings) -> return $ typeCheck canTable main

    Left err    -> return (Left [err], [])


bumpVersion :: Bool -> APIChange -> Version -> Version
bumpVersion rebuild apiChange version = case (apiChange, version) of
  (Major, Version [major, minor, patch] _) | rebuild && minor == 0 && patch == 0 ->
    version

  (Major, Version [major, _, _] _) ->
    Version [major + 1, 0, 0] []

  (Minor, Version [major, minor, patch] _) | rebuild && patch == 0 ->
    version

  (Minor, Version [major, minor, _] _) ->
    Version [major, minor + 1, 0] []

  (Patch, Version [major, minor, patch] _) | rebuild ->
    version

  (Patch, Version [major, minor, patch] _) ->
    Version [major, minor, patch + 1] []


performBuild :: Bool -> Either VersionLock.ReadError VersionLock.VersionLock -> Maybe Version -> String -> String -> Slv.AST -> Slv.Table -> IO (Either String (Version, VersionLock.VersionLock))
performBuild rebuild eitherVersionLock parsedVersion hashedVersion projectHash ast table =
  case (eitherVersionLock, parsedVersion) of
    -- if there is no version.lock file we generate the initial one with version 0.0.1
    (Left VersionLock.FileNotFound, _) -> do
      let initialVersionHash = hash $ BLChar8.pack "0.0.1"
      let api = buildAPI ast table
          versionLock = VersionLock.VersionLock
            { VersionLock.versionHash = initialVersionHash
            , VersionLock.buildHash   = projectHash
            , VersionLock.api         = api
            }
      return $ Right (Version [0, 0, 1] [], versionLock)

    (Left VersionLock.ReadError, _) -> return $ Left "An error occured while reading the version.lock file"

    (Right VersionLock.VersionLock { VersionLock.versionHash, VersionLock.buildHash, VersionLock.api }, Just version) ->
      if buildHash == projectHash then
        return $ Left "Project has not changed, nothing to do."
      else if hashedVersion /= versionHash then
        return $ Left "It seems that you modified the version in madlib.json manually"
      else do
        let currentAPI = buildAPI ast table
        let nextVersion = bumpVersion rebuild (computeAPIChange api currentAPI) version
        let nextVersionHash = hash $ BLChar8.pack $ showVersion nextVersion

        let nextVersionLock = VersionLock.VersionLock
              { VersionLock.versionHash = nextVersionHash
              , VersionLock.buildHash   = projectHash
              , VersionLock.api         = currentAPI
              }
        return $ Right (nextVersion, nextVersionLock)


runBuildPackage :: Bool -> IO ()
runBuildPackage rebuild = do
  putStrLn "Building package..."
  madlibDotJson <- MadlibDotJson.loadCurrentMadlibDotJson

  case madlibDotJson of
    Left _ -> putStrLn "The package must have a madlib.json file"

    Right goodMadlibDotJson@MadlibDotJson.MadlibDotJson { MadlibDotJson.main, MadlibDotJson.version = Just version } -> do
      canonicalMain           <- canonicalizePath main
      currentDirectoryPath    <- getCurrentDirectory
      versionLock             <- VersionLock.loadCurrentVersionLock
      projectHash             <- generatePackageHash currentDirectoryPath
      let hashedVersion = hash $ BLChar8.pack version
      (typeChecked, warnings) <- typeCheckMain canonicalMain

      let parsedVersion = MadlibVersion.parse version

      case typeChecked of
        Left errors ->
          putStrLn "Compilation errors, please fix them before building the package"

        Right solvedTable -> do
          let (Just mainAST) = Map.lookup canonicalMain solvedTable
          processed <- performBuild rebuild versionLock parsedVersion hashedVersion projectHash mainAST solvedTable
          case processed of
            Left e -> putStrLn e

            Right (version, versionLock) -> do
              VersionLock.save "version.lock" versionLock
              MadlibDotJson.save "madlib.json" goodMadlibDotJson { MadlibDotJson.version = Just $ showVersion version }
              putStrLn $ "Version updated to '" <> showVersion version <> "'. Do not forget to version control the version.lock file!"

      return ()


runPackage :: PackageSubCommand -> Bool -> IO ()
runPackage subCommand rebuild = case subCommand of
  NoPackageSubCommand -> runBuildPackage rebuild

  GenerateHash input  -> runGeneratePackageHash input
