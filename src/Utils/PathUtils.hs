module Utils.PathUtils where

import qualified Data.ByteString.Lazy          as B

data PathUtils
  = PathUtils
      { readFile :: FilePath -> IO String
      , canonicalizePath :: FilePath -> IO FilePath
      , doesFileExist :: FilePath -> IO Bool
      , byteStringReadFile :: FilePath -> IO B.ByteString
      , getExecutablePath :: IO FilePath
      }
