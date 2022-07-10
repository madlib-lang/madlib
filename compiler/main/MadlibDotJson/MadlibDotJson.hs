{-# LANGUAGE DeriveGeneric #-}
module MadlibDotJson.MadlibDotJson where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           GHC.Generics                   ( Generic )
import           Utils.PathUtils
import qualified Data.Map                      as M
import           Control.Exception              ( try
                                                , SomeException
                                                )
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as Char8
import qualified Data.Text                     as Text
import           System.Directory               ( getCurrentDirectory )
import           System.FilePath                ( joinPath )

data Dependency
  = Dependency
  { description :: String
  , url :: String
  , minVersion :: Maybe String
  , maxVersion :: Maybe String
  }
  deriving (Show, Generic)

data MadlibDotJson
  = MadlibDotJson { main          :: String
                  , bin           :: Maybe String
                  , dependencies  :: Maybe [Dependency]
                  , importAliases :: Maybe (M.Map String String)
                  , version       :: Maybe String
                  , name          :: Maybe String
                  , madlibVersion :: Maybe String
                  , staticLibs   :: Maybe [String]
                  }
                  deriving (Show, Generic)

instance FromJSON MadlibDotJson
instance ToJSON MadlibDotJson where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Dependency
instance ToJSON Dependency where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }


load :: PathUtils -> FilePath -> IO (Either String MadlibDotJson)
load pathUtils file = do
  content <- try $ byteStringReadFile pathUtils file :: IO (Either SomeException B.ByteString)
  case content of
    Right c ->
      return $ eitherDecode c :: IO (Either String MadlibDotJson)

    Left  _ ->
      return $ Left "File not found"


getStaticLibPaths :: PathUtils -> FilePath -> IO [String]
getStaticLibPaths pathUtils path = do
  madlibDotJson <- load pathUtils path

  case madlibDotJson of
    Right MadlibDotJson{ staticLibs = Just flags } ->
      return flags

    _ ->
      return []


prettyPrint :: MadlibDotJson -> B.ByteString
prettyPrint madlibDotJson =
  encodePretty'
    defConfig { confIndent = Spaces 2
              , confCompare = keyOrder $ Text.pack <$> ["name", "version", "madlibVersion", "main", "bin", "importAliases", "dependencies"]
              }
    madlibDotJson
  <> Char8.pack "\n"

save :: FilePath -> MadlibDotJson -> IO ()
save filePath = B.writeFile filePath . prettyPrint

loadCurrentMadlibDotJson :: IO (Either String MadlibDotJson)
loadCurrentMadlibDotJson = do
  currentDir <- getCurrentDirectory
  let madlibDotJsonPath = joinPath [currentDir, "madlib.json"]
  load defaultPathUtils madlibDotJsonPath
