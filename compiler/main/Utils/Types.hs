module Utils.Types where

import           Infer.Type
import qualified Data.Map         as Map
import qualified Data.List        as List
import qualified Utils.Hash       as Hash
import qualified Data.ByteString.Lazy.Char8    as BLChar8


generateHashFromPath :: FilePath -> String
generateHashFromPath =
  Hash.hash . BLChar8.pack
