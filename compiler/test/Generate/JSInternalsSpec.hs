module Generate.JSInternalsSpec where

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
import           Generate.JSInternals
import           Run.Target

snapshotTest :: String -> String -> Golden Text
snapshotTest name actualOutput = Golden { output        = pack actualOutput
                                        , encodePretty  = unpack
                                        , writeToFile   = T.writeFile
                                        , readFromFile  = T.readFile
                                        , goldenFile    = ".snapshots/" <> unpack (replace (pack " ") (pack "_") (pack name)) <> "/golden"
                                        , actualFile    = Just $ ".snapshots/" <> unpack (replace (pack " ") (pack "_") (pack name)) <> "/actual"
                                        , failFirstTime = False
                                        }


spec :: Spec
spec = do
  describe "JS Internals" $ do
    it "should build all internal JS functions" $ do
      let actual = generateInternalsModuleContent TNode False False
      snapshotTest "should build all internal JS functions" actual
