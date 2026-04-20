{-# LANGUAGE FlexibleContexts #-}
module Generate.LLVM.Helper where

import Control.Monad.State (MonadState, get, put)
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as ByteString
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified LLVM.AST.Operand as Operand
import Data.Bits (xor, shiftR)
import Data.Word (Word64)
import Numeric (showHex)


newMetadataId :: MonadState Int m => m Word
newMetadataId = do
  s <- get
  put (s + 1)
  return $ fromIntegral s


stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack


-- | Per-module string literal deduplication cache.
-- Maps UTF-8 bytes of a string literal to its already-emitted LLVM constant operand.
-- Must be reset at the start of each module compilation via 'resetLitCache'.
{-# NOINLINE globalLitCache #-}
globalLitCache :: IORef (Map.Map ByteString.ByteString Operand.Operand)
globalLitCache = unsafePerformIO (newIORef Map.empty)


-- | Reset the literal cache for a new module compilation unit.
resetLitCache :: IO ()
resetLitCache = writeIORef globalLitCache Map.empty


-- | FNV-1a 64-bit hash of a ByteString.
fnv1a :: ByteString.ByteString -> Word64
fnv1a = ByteString.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) 14695981039346656037


-- | FNV-1a hash rendered as a 16-character lowercase hex string (zero-padded).
fnv1aHex :: ByteString.ByteString -> String
fnv1aHex bs =
  let h   = fnv1a bs
      hex = showHex h ""
  in  replicate (16 - length hex) '0' ++ hex
