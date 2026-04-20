module Run.PackageAdd where

import qualified MadlibDotJson.MadlibDotJson as MadlibDotJson
import           Run.PackageInstaller (runPackageInstaller)
import           System.Directory (getCurrentDirectory)
import           System.FilePath (joinPath)
import           Data.List (isSuffixOf)
import           Utils.PathUtils (defaultPathUtils)


-- | Derive a package name from a URL (last path segment, minus any .git suffix)
nameFromUrl :: String -> String
nameFromUrl url =
  let noTrailingSlash = if "/" `isSuffixOf` url then init url else url
      lastSegment     = reverse . takeWhile (/= '/') . reverse $ noTrailingSlash
      noGitSuffix     = if ".git" `isSuffixOf` lastSegment
                          then take (length lastSegment - 4) lastSegment
                          else lastSegment
  in  noGitSuffix


addPackage :: String -> Maybe String -> IO ()
addPackage url maybeName = do
  currentDir <- getCurrentDirectory
  let madlibDotJsonPath = joinPath [currentDir, "madlib.json"]
  result <- MadlibDotJson.load defaultPathUtils madlibDotJsonPath
  case result of
    Left err ->
      putStrLn $ "Error reading madlib.json: " <> err

    Right madlibJson -> do
      let name       = maybe (nameFromUrl url) id maybeName
          newDep     = MadlibDotJson.Dependency
                         { MadlibDotJson.description = name
                         , MadlibDotJson.url         = url
                         , MadlibDotJson.minVersion  = Nothing
                         , MadlibDotJson.maxVersion  = Nothing
                         }
          existingDeps = maybe [] id (MadlibDotJson.dependencies madlibJson)
          updatedJson  = madlibJson { MadlibDotJson.dependencies = Just (existingDeps ++ [newDep]) }

      MadlibDotJson.save madlibDotJsonPath updatedJson
      putStrLn $ "Added package '" <> name <> "' from " <> url
      putStrLn "Running install..."
      runPackageInstaller


removePackage :: String -> IO ()
removePackage name = do
  currentDir <- getCurrentDirectory
  let madlibDotJsonPath = joinPath [currentDir, "madlib.json"]
  result <- MadlibDotJson.load defaultPathUtils madlibDotJsonPath
  case result of
    Left err ->
      putStrLn $ "Error reading madlib.json: " <> err

    Right madlibJson -> do
      let existingDeps  = maybe [] id (MadlibDotJson.dependencies madlibJson)
          filteredDeps  = filter (\d -> MadlibDotJson.description d /= name) existingDeps
      if length filteredDeps == length existingDeps then
        putStrLn $ "Package '" <> name <> "' not found in madlib.json"
      else do
        let updatedJson = madlibJson { MadlibDotJson.dependencies = Just filteredDeps }
        MadlibDotJson.save madlibDotJsonPath updatedJson
        putStrLn $ "Removed package '" <> name <> "' from madlib.json"
        putStrLn $ "Note: you may want to delete madlib_modules/" <> name <> "/ manually"
