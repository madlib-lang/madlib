{-# LANGUAGE FlexibleContexts #-}
module Generate.LLVM.Helper where
import Control.Monad.State (MonadState, get, put)
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.ByteString.Char8 as Char8


newMetadataId :: MonadState Int m => m Word
newMetadataId = do
  s <- get
  put (s + 1)
  return $ fromIntegral s

stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack
