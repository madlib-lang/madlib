module Utils.PathUtils where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as B
import qualified System.Directory              as Dir
import           Prelude                 hiding ( readFile )
import qualified System.Environment.Executable as E
import           System.IO                      ( openFile
                                                , IOMode(ReadMode)
                                                , hSetEncoding
                                                , hGetContents
                                                )
import           GHC.IO.Encoding                ( utf8 )
import           System.FilePath (normalise)

data PathUtils
  = PathUtils
      { readFile :: FilePath -> IO String
      , canonicalizePath :: FilePath -> IO FilePath
      , normalisePath :: FilePath -> FilePath
      , doesFileExist :: FilePath -> IO Bool
      , byteStringReadFile :: FilePath -> IO B.ByteString
      , strictByteStringReadFile :: FilePath -> IO BS.ByteString
      , getExecutablePath :: IO FilePath
      }


rf fileName = do
  inputHandle <- openFile fileName ReadMode
  hSetEncoding inputHandle utf8
  hGetContents inputHandle

defaultPathUtils :: PathUtils
defaultPathUtils = PathUtils { readFile                 = rf
                             , canonicalizePath         = Dir.canonicalizePath
                             , normalisePath            = normalise
                             , doesFileExist            = Dir.doesFileExist
                             , byteStringReadFile       = B.readFile
                             , strictByteStringReadFile = BS.readFile
                             , getExecutablePath        = E.getExecutablePath
                             }
