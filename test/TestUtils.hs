module TestUtils where

import qualified Data.Map                      as M
import           Utils.PathUtils
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Prelude                 hiding ( readFile )
import           GHC.IO.Exception
import           Data.Text                      ( pack
                                                , replace
                                                , unpack
                                                )

type MockFiles = M.Map FilePath String

toByteString :: String -> B.ByteString
toByteString = encodeUtf8 . T.pack

makeReadFile :: MockFiles -> FilePath -> IO String
makeReadFile files path = do
  case M.lookup path files of
    Just f  -> return f
    Nothing -> ioError IOError { ioe_handle      = Nothing
                               , ioe_type        = NoSuchThing
                               , ioe_location    = ""
                               , ioe_description = ""
                               , ioe_errno       = Nothing
                               , ioe_filename    = Just path
                               }

makeByteStringReadFile :: MockFiles -> FilePath -> IO B.ByteString
makeByteStringReadFile files path = toByteString <$> makeReadFile files path


fixPath p = unpack $ replace (pack "/./") (pack "/") (pack p)

defaultPathUtils = PathUtils
  { readFile           = makeReadFile M.empty
  , canonicalizePath   = return . fixPath
  , doesFileExist      = const $ return True
  , byteStringReadFile = makeByteStringReadFile M.empty
  , getExecutablePath  = return "/"
  }
