module Compile.JSInternalsSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                )
import           Test.Hspec.Golden              ( Golden(..) )
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import           Compile.JSInternals
import           Target

snapshotTest :: String -> String -> Golden Text
snapshotTest name actualOutput = Golden
  { output        = pack actualOutput
  , encodePretty  = unpack
  , writeToFile   = T.writeFile
  , readFromFile  = T.readFile
  , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
  , directory     = ".snapshots"
  , failFirstTime = False
  }


spec :: Spec
spec = do
  describe "JS Internals" $ do
    it "should build all internal JS functions" $ do
      let actual = generateInternalsModuleContent TNode False False
      snapshotTest "should build all internal JS functions" actual

    it "should include coverage trackers when coverage is True" $ do
      let actual = generateInternalsModuleContent TNode False True
      snapshotTest "should include coverage trackers when coverage is True"
                   actual
