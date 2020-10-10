module InferSpec where

import qualified Data.Map                      as M
import qualified Data.Either                   as E
import           Grammar
import           Lexer
import           Infer
import           AST
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import           Test.Hspec.Golden              ( Golden(..) )
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Validate         ( runValidateT )
import           Control.Monad.Reader           ( runReader
                                                , MonadReader(..)
                                                )

snapshotTest :: Show a => String -> a -> Golden Text
snapshotTest name actualOutput = Golden
  { output        = pack $ ppShow actualOutput
  , encodePretty  = ppShow
  , writeToFile   = T.writeFile
  , readFromFile  = T.readFile
  , testName      = unpack $ replace (pack " ") (pack "_") (pack name)
  , directory     = ".snapshots"
  , failFirstTime = False
  }

tester :: String -> Either InferError AST
tester code =
  case buildAST "path" code of
        (Right ast) -> runInfer ast
        _           -> Left $ UnboundVariable ""

spec :: Spec
spec = do
  describe "infer" $ do
    it "should resolve abstractions" $ do
      let code   = "(b, c) => b + c"
          actual = case tester code of
            (Right _) -> True
            _         -> False
      actual `shouldBe` True