module Utils.PathUtils where

import qualified Data.ByteString.Lazy          as B
import qualified System.Directory              as Dir
import           Prelude                 hiding ( readFile )
import qualified Data.ByteString.Lazy          as B
import qualified System.Environment.Executable as E
import System.IO (openFile, IOMode (ReadMode), hSetEncoding, hGetContents)
import GHC.IO.Encoding (utf8)

data PathUtils
  = PathUtils
      { readFile :: FilePath -> IO String
      , canonicalizePath :: FilePath -> IO FilePath
      , doesFileExist :: FilePath -> IO Bool
      , byteStringReadFile :: FilePath -> IO B.ByteString
      , getExecutablePath :: IO FilePath
      }

rf fileName = do
  inputHandle <- openFile fileName ReadMode
  hSetEncoding inputHandle utf8
  hGetContents inputHandle

defaultPathUtils :: PathUtils
defaultPathUtils = PathUtils { readFile           = rf
                            , canonicalizePath   = Dir.canonicalizePath
                            , doesFileExist      = Dir.doesFileExist
                            , byteStringReadFile = B.readFile
                            , getExecutablePath  = E.getExecutablePath
                            }
