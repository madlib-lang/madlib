module Run.PackageHash where

import           System.Directory              ( listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory )
import           System.FilePath               ( joinPath )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLChar8
import qualified Data.List                     as List
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder       ( toLazyByteString, byteStringHex )


getFileOrDirContent :: FilePath -> IO BL.ByteString
getFileOrDirContent path = do
  fileExists   <- doesFileExist path
  folderExists <- doesDirectoryExist path

  case (fileExists, folderExists) of
    (_, True) -> do
      entries          <- List.sort <$> listDirectory path
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
  folderExists <- doesDirectoryExist packageFolder

  if folderExists then do
    entries          <- List.sort . filter (`notElem` filesToExclude) <$> listDirectory packageFolder
    processedEntries <- mapM (getFileOrDirContent . joinPath . ([packageFolder] <>) . return) entries
    return $ BL.concat processedEntries
  else
    return BL.empty

generatePackageHash :: FilePath -> IO String
generatePackageHash packageFolder = do
  packageContent <- getPackageContent packageFolder

  let hashed  = hashlazy packageContent
      hexHash = toLazyByteString . byteStringHex $ hashed

  return $ BLChar8.unpack hexHash

runGeneratePackageHash :: IO ()
runGeneratePackageHash = do
  currentDirectoryPath <- getCurrentDirectory
  hash <- generatePackageHash currentDirectoryPath
  putStrLn hash
