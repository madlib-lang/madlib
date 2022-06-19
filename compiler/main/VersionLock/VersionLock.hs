{-# LANGUAGE DeriveGeneric #-}
module VersionLock.VersionLock where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor
import           GHC.Generics                   ( Generic )
import qualified Data.Map                       as M
import           Control.Exception              ( try
                                                , SomeException
                                                )
import qualified Data.ByteString.Lazy           as B
import qualified Data.ByteString.Lazy.Char8     as Char8
import           System.Directory               ( getCurrentDirectory )
import           System.FilePath                ( joinPath )

import           Utils.PathUtils
import           VersionLock.PublicAPI

data VersionLock
  = VersionLock
  { versionHash :: String
  , buildHash :: String
  , jsApi :: PublicAPI
  , llvmApi :: PublicAPI
  }
  deriving (Show, Generic)

instance FromJSON VersionLock
instance ToJSON VersionLock

data ReadError
  = ReadError
  | FileNotFound

load :: PathUtils -> FilePath -> IO (Either String VersionLock)
load pathUtils file = do
  content <- try $ byteStringReadFile pathUtils file :: IO (Either SomeException B.ByteString)
  case content of
    Right c -> return $ eitherDecode c :: IO (Either String VersionLock)
    Left  _ -> return $ Left "File not found"


prettyPrint :: VersionLock -> B.ByteString
prettyPrint versionLock =
  encodePretty'
    defConfig { confIndent = Spaces 2 }
    versionLock
  <> Char8.pack "\n"

save :: FilePath -> VersionLock -> IO ()
save filePath = B.writeFile filePath . prettyPrint

loadCurrentVersionLock :: IO (Either ReadError VersionLock)
loadCurrentVersionLock = do
  currentDir <- getCurrentDirectory
  let versionLockPath = joinPath [currentDir, "version.lock"]
  versionLockFound <- doesFileExist defaultPathUtils versionLockPath
  if versionLockFound then
    first (const ReadError) <$> load defaultPathUtils versionLockPath
  else
    return $ Left FileNotFound
