{-# LANGUAGE DeriveGeneric #-}
module MadlibDotJson.MadlibDotJson where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Utils.PathUtils
import qualified Data.Map                      as M
import           Control.Exception              ( try
                                                , SomeException
                                                )
import qualified Data.ByteString.Lazy          as B


data MadlibDotJson
  = MadlibDotJson { main          :: String
                  , bin           :: Maybe String
                  , dependencies  :: Maybe (M.Map String String)
                  , importAliases :: Maybe (M.Map String String)
                  , version       :: Maybe String
                  , name          :: Maybe String
                  , madlibVersion :: Maybe String
                  }
                  deriving (Show, Generic)

instance FromJSON MadlibDotJson
instance ToJSON MadlibDotJson

load :: PathUtils -> FilePath -> IO (Either String MadlibDotJson)
load pathUtils file = do
  content <- try $ byteStringReadFile pathUtils file :: IO (Either SomeException B.ByteString)
  case content of
    Right c -> return $ eitherDecode c :: IO (Either String MadlibDotJson)
    Left  e -> return $ Left "File not found"
