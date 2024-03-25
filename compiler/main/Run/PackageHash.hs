{-# LANGUAGE LambdaCase #-}
module Run.PackageHash where

import           System.Directory              as Dir
import           System.FilePath               ( joinPath )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as Char8
import qualified Data.List                     as List
import           System.Exit
import           System.FilePath.Glob

import           MadlibDotJson.MadlibDotJson    as MadlibDotJson
import           Run.CommandLine
import           Utils.Hash

import           Utils.PathUtils


isIn :: BL.ByteString -> BL.ByteString -> Bool
isIn search inBS
  | search `Char8.isPrefixOf` inBS = True
  | Char8.length search > 0        = isIn (Char8.init search) inBS
  | otherwise                      = False



getFileOrDirContent :: [FilePath] -> FilePath -> IO BL.ByteString
getFileOrDirContent blackList path = do
  canPath <- Dir.canonicalizePath path
  if path `elem` blackList then
    return BL.empty
  else do
    fileExists   <- Dir.doesFileExist path
    folderExists <- Dir.doesDirectoryExist path

    case (fileExists, folderExists) of
      (_, True) -> do
        entries          <- List.sort <$> Dir.listDirectory path
        processedEntries <- mapM (getFileOrDirContent blackList . joinPath . ([path] <>) . return) entries
        return $ BL.concat processedEntries

      (True, _) -> do
        fileContent <- BL.readFile path
        
        if "madlib.json" `List.isInfixOf` canPath then do
          let filteredLines = filter (not . ("\"version\":" `List.isInfixOf`)) (List.lines (Char8.unpack fileContent))
          return $ Char8.pack (List.unlines filteredLines)
        else
          return fileContent

      _         -> return BL.empty


-- consider using this: https://hackage.haskell.org/package/vcs-ignore-0.0.1.0/docs/Data-VCS-Ignore-Repo-Git.html
-- instead of this hardcoded version as it doesn't work well if you have for instance
-- some non versioned lib files or source files that aren't versioned
filesToExclude :: [FilePath]
filesToExclude = [ "madlib_modules"
                 , "node_modules"
                 , ".module_cache"
                 , ".run"
                 , ".docs"
                 , "build"
                 , ".tests"
                 , ".coverage"
                 , "coverage"
                 , "version.lock"
                 , ".git"
                 , ".DS_Store"
                 , "package-lock.json"
                 ]

dotMadlibIgnore :: FilePath
dotMadlibIgnore = ".madlibignore"

dotGitIgnore :: FilePath
dotGitIgnore = ".gitignore"

loadIgnoredPathsFromDotIgnoreFile :: FilePath -> FilePath -> IO [FilePath]
loadIgnoredPathsFromDotIgnoreFile dotIgnoreFileName packageFolder = do
  let dotIgnoreFilePath = joinPath [packageFolder, dotIgnoreFileName]
  fileExists   <- Dir.doesFileExist dotIgnoreFilePath
  if fileExists then do
    dotIgnoreFileContent <- Prelude.readFile dotIgnoreFilePath
    let entries = filter (not . ("#" `List.isPrefixOf`)) $ lines dotIgnoreFileContent
    return entries
  else
    return []

getPackageContent :: FilePath -> IO BL.ByteString
getPackageContent packageFolder = do
  folderExists <- Dir.doesDirectoryExist packageFolder

  if folderExists then do
    filesToExcludeFromDotMadlibIgnore <- loadIgnoredPathsFromDotIgnoreFile dotMadlibIgnore packageFolder
    filesToExcludeFromDotGitIgnore <- loadIgnoredPathsFromDotIgnoreFile dotGitIgnore packageFolder
    let filesToExcludeFromIgnoreFiles = if List.null filesToExcludeFromDotMadlibIgnore then filesToExcludeFromDotGitIgnore else filesToExcludeFromDotMadlibIgnore
    let patterns = compile <$> (filesToExclude <> filesToExcludeFromIgnoreFiles)
    excludedPaths <- concat <$> globDir patterns packageFolder
    excludedPaths' <- mapM Dir.canonicalizePath excludedPaths

    entries          <- List.sort <$> listDirectory packageFolder
    processedEntries <- mapM (getFileOrDirContent excludedPaths' . joinPath . ([packageFolder] <>) . return) entries
    return $ BL.concat processedEntries
  else
    return BL.empty

generatePackageHash :: FilePath -> IO String
generatePackageHash packageFolder = do
  packageContent <- getPackageContent packageFolder

  return $ hash packageContent

runGeneratePackageHash :: FilePath -> IO ()
runGeneratePackageHash packagePath = do
  canPath       <- Dir.canonicalizePath packagePath
  madlibDotJson <- MadlibDotJson.load defaultPathUtils $ joinPath [canPath, "madlib.json"]

  case madlibDotJson of
    Left _  -> putStrLn "No madlib.json found!" >> exitFailure

    Right _ -> do
      hash                 <- generatePackageHash canPath
      putStr hash
