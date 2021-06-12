module Utils.Hash where

import qualified Data.ByteString.Lazy          as BL
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder       ( toLazyByteString, byteStringHex )
import qualified Data.ByteString.Lazy.Char8    as BLChar8


hash :: BL.ByteString -> IO String
hash input = do
  let hashed  = hashlazy input
      hexHash = toLazyByteString . byteStringHex $ hashed
  return $ BLChar8.unpack hexHash
