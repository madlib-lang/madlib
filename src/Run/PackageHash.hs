module Run.PackageHash where

import           System.Directory              as Dir
import           System.FilePath               ( joinPath )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.List                     as List
import           System.Exit

import           MadlibDotJson.MadlibDotJson    as MadlibDotJson
import           Run.CommandLine
import           Utils.Hash

import           Utils.PathUtils


getFileOrDirContent :: FilePath -> IO BL.ByteString
getFileOrDirContent path = do
  fileExists   <- Dir.doesFileExist path
  folderExists <- Dir.doesDirectoryExist path

  case (fileExists, folderExists) of
    (_, True) -> do
      entries          <- List.sort <$> Dir.listDirectory path
      processedEntries <- mapM (getFileOrDirContent . joinPath . ([path] <>) . return) entries
      return $ BL.concat processedEntries

    (True, _) -> BL.readFile path

    _         -> return BL.empty


filesToExclude :: [FilePath]
filesToExclude = [ "madlib_modules"
                 , "node_modules"
                 , ".run"
                 , ".docs"
                 , "build"
                 , ".tests"
                 , ".coverage"
                 , "coverage"
                 , "library.json"
                 , ".git"
                 , "package-lock.json"
                 ]

getPackageContent :: FilePath -> IO BL.ByteString
getPackageContent packageFolder = do
  folderExists <- Dir.doesDirectoryExist packageFolder

  if folderExists then do
    entries          <- List.sort . filter (`notElem` filesToExclude) <$> listDirectory packageFolder
    processedEntries <- mapM (getFileOrDirContent . joinPath . ([packageFolder] <>) . return) entries
    return $ BL.concat processedEntries
  else
    return BL.empty

generatePackageHash :: FilePath -> IO String
generatePackageHash packageFolder = do
  packageContent <- getPackageContent packageFolder

  hash packageContent
  -- let hashed  = hashlazy packageContent
  --     hexHash = toLazyByteString . byteStringHex $ hashed

  -- return $ BLChar8.unpack hexHash

runGeneratePackageHash :: FilePath -> IO ()
runGeneratePackageHash packagePath = do
  canPath       <- Dir.canonicalizePath packagePath
  madlibDotJson <- MadlibDotJson.load defaultPathUtils $ joinPath [canPath, "madlib.json"]

  case madlibDotJson of
    Left _  -> putStrLn "No madlib.json found!" >> exitFailure

    Right _ -> do
      hash                 <- generatePackageHash canPath
      putStrLn hash
