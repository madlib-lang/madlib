module Utils.Hash where

import qualified Data.ByteString.Lazy          as BL
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder       ( toLazyByteString, byteStringHex )
import qualified Data.ByteString.Lazy.Char8    as BLChar8


hash :: BL.ByteString -> String
hash input =
  let hashed  = hashlazy input
      hexHash = toLazyByteString . byteStringHex $ hashed
  in  BLChar8.unpack hexHash

generateHashFromPath :: FilePath -> String
generateHashFromPath =
  hash . BLChar8.pack
  -- take 5 . hash . BLChar8.pack
