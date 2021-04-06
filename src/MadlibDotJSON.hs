{-# LANGUAGE DeriveGeneric #-}
module MadlibDotJSON where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Utils.PathUtils
import qualified Data.Map                      as M

data MadlibDotJSON
  = MadlibDotJSON { main         :: String
                  , bin          :: Maybe String
                  , dependencies :: Maybe (M.Map String String)
                  , version      :: Maybe String
                  , name         :: Maybe String
                  }
                  deriving (Show, Generic)

instance FromJSON MadlibDotJSON
instance ToJSON MadlibDotJSON

load :: PathUtils -> FilePath -> IO (Either String MadlibDotJSON)
load pathUtils file = do
  (eitherDecode <$> byteStringReadFile pathUtils file) :: IO (Either String MadlibDotJSON)
