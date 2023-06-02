{-# LANGUAGE FlexibleContexts #-}
module Generate.LLVM.Helper where
import Control.Monad.State (MonadState, get, put)
import Data.Word (Word8)

newMetadataId :: MonadState Int m => m Word8
newMetadataId = do
  s <- get
  put (s + 1)
  return $ fromIntegral s
