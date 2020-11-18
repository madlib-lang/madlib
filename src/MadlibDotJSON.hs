{-# LANGUAGE DeriveGeneric #-}
module MadlibDotJSON where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Utils.PathUtils

newtype MadlibDotJSON
  = MadlibDotJSON { main :: String } deriving (Show, Generic)

instance FromJSON MadlibDotJSON
instance ToJSON MadlibDotJSON

load :: PathUtils -> FilePath -> IO (Either String MadlibDotJSON)
load pathUtils file = do
  (eitherDecode <$> byteStringReadFile pathUtils file) :: IO
      (Either String MadlibDotJSON)
