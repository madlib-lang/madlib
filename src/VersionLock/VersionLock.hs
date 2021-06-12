{-# LANGUAGE DeriveGeneric #-}
module VersionLock.VersionLock where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import qualified Data.Map                       as M
import           Control.Exception              ( try
                                                , SomeException
                                                )
import qualified Data.ByteString.Lazy           as B

import           Utils.PathUtils

data VersionLock
  = VersionLock { versionHash :: String
                , buildHash   :: String
                , api         :: M.Map String String
                }
                deriving (Show, Generic)

instance FromJSON VersionLock
instance ToJSON VersionLock


load :: PathUtils -> FilePath -> IO (Either String VersionLock)
load pathUtils file = do
  content <- try $ byteStringReadFile pathUtils file :: IO (Either SomeException B.ByteString)
  case content of
    Right c -> return $ eitherDecode c :: IO (Either String VersionLock)
    Left  _ -> return $ Left "File not found"
