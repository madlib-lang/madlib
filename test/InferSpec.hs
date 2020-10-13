module InferSpec where

import           Grammar
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
tester code = case buildAST "path" code of
  (Right ast) -> runInfer (buildInitialEnv ast) ast
  _           -> Left $ UnboundVariable ""

spec :: Spec
spec = do
  describe "infer" $ do
    it "should infer abstractions" $ do
      let code   = "(b, c) => b + c"
          actual = tester code
      snapshotTest "should infer abstractions" actual

    it "should infer assignments" $ do
      let code   = unlines ["fn = (b, c) => b + c", "fn(2, 3)"]
          actual = tester code
      snapshotTest "should infer assignments" actual
    
    it "should infer minus operator" $ do
      let code   = "(b, c) => b - c"
          actual = tester code
      snapshotTest "should infer minus operator" actual
    
    it "should infer multiplication operator" $ do
      let code   = "(b, c) => b * c"
          actual = tester code
      snapshotTest "should infer multiplication operator" actual
    
    it "should infer division operator" $ do
      let code   = "(b, c) => b / c"
          actual = tester code
      snapshotTest "should infer division operator" actual
    
    it "should infer an empty source" $ do
      let code   = ""
          actual = tester code
      snapshotTest "should infer an empty source" actual

    it "should infer adts" $ do
      let code = unlines
            [ "data Result = Success String | Error"
            , "result = Success(\"response\")"
            ]
          actual = tester code
      snapshotTest "should infer adts" actual

    it "should infer adts with type parameters" $ do
      let code = unlines
            [ "data Result a"
            , "  = Success a"
            , "  | Error"
            , "result = Success(True)"
            ]
          actual = tester code
      snapshotTest "should infer adts with type parameters" actual

    it "should infer application of adts" $ do
      let code = unlines
            [ "data Result = Success String | Error"
            , "result1 = Success(\"response\")"
            , "result2 = Error"
            , "((a, b) => a === b)(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer application of adts" actual

    it "should infer adts with type parameters" $ do
      let code = unlines
            [ "data Result a = Success a | Error"
            , "result1 = Success(\"response\")"
            , "result2 = Error"
            , "((a, b) => a === b)(result1, result2)"
            ]
          actual = tester code
      snapshotTest "should infer adt return for abstractions" actual

    -- TODO: Write and implement the same test with ADTs
    -- TODO: Write tests where implementation and definition don't match to force
    -- implementing it as it's currently not implemented.
    it "should resolve abstractions with a type definition" $ do
      let code = unlines
            ["fn :: Num -> Num -> Bool", "fn = (a, b) => a === b", "fn(3, 4)"]
          actual = tester code
      snapshotTest "should resolve abstractions with a type definition" actual

    it "should fail for abstractions with a wrong type definition" $ do
      let code =
            unlines
              [ "fn :: String -> Num -> Bool"
              , "fn = (a, b) => a === b"
              , "fn(3, 4)"
              ]
          actual = tester code
      snapshotTest "should fail for abstractions with a wrong type definition"
                   actual
