{-# LANGUAGE FlexibleInstances #-}
module Utils.PathSpec where

import           Utils.Path
import           Test.Hspec

spec :: Spec
spec = do
  describe "computeRootPath" $ do
    it "should build return the path part before the filename" $ do
      computeRootPath "some/folder/file.mad" `shouldBe` "some/folder/"
