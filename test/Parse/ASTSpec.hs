{-# LANGUAGE FlexibleInstances #-}
module Parse.ASTSpec where

import qualified Data.Map                      as M
import           Parse.AST
import           Test.Hspec
import           Error.Error
import           Explain.Reason
import           Utils.PathUtils
import           Prelude                 hiding ( readFile )
import           TestUtils
import           Test.Hspec.Golden              ( Golden(..) )
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import           Text.Show.Pretty               ( ppShow )
import           GHC.IO                         ( unsafePerformIO )


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


spec :: Spec
spec = do
  describe "findAST" $ do
    it "should return a Right AST if the ast exists" $ do
      let source =
            unlines
              [ "fn :: Number -> Number -> Number"
              , "fn = (a, b) => (fn2(a, b) + a)"
              ]

          (Right ast) = buildAST "fixtures/source.mad" source

          expected    = Right ast
          files       = M.fromList [("./fixtures/source.mad", source)]

          rf          = makeReadFile files

          pathUtils   = defaultPathUtils { readFile = rf }

      r <- buildASTTable' pathUtils "" Nothing "fixtures/source.mad"
      let actual = r >>= flip findAST "./fixtures/source.mad"
      actual `shouldBe` expected

    it "should return a Left ImportNotFound if it does not exist" $ do
      let
        source =
          unlines
            [ "fn :: Number -> Number -> Number"
            , "fn = (a, b) => (fn2(a, b) + a)"
            ]

        (Right ast) = buildAST "fixtures/source.mad" source

        expected =
          Left
            (InferError (ImportNotFound "./fixtures/source-not-there.mad")
                        NoReason
            )
        files     = M.fromList [("./fixtures/source.mad", source)]

        rf        = makeReadFile files
        pathUtils = defaultPathUtils { readFile = rf }

      r <- buildASTTable' pathUtils "" Nothing "fixtures/source.mad"
      let actual = r >>= flip findAST "./fixtures/source-not-there.mad"
      actual `shouldBe` expected

  describe "buildASTTable" $ do
    it "should build an AST Table" $ do
      let
        sourceA = unlines
          [ "import { fn2 } from \"./sourceB\""
          , "fn :: Number -> Number -> Number"
          , "fn = (a, b) => (fn2(a, b) + a)"
          ]
        sourceB =
          unlines
            [ "fn2 :: Number -> Number -> Number"
            , "export fn2 = (a, b) => (a + b)"
            ]

        (Right astA) = buildAST "/fixtures/sourceA.mad" sourceA
        (Right astB) = buildAST "/fixtures/sourceB.mad" sourceB


        files        = M.fromList
          [ ("/fixtures/sourceA.mad", sourceA)
          , ("/fixtures/sourceB.mad", sourceB)
          ]

        pathUtils = defaultPathUtils { readFile = makeReadFile files }

        actual    = unsafePerformIO
          $ buildASTTable' pathUtils "" Nothing "/fixtures/sourceA.mad"
      snapshotTest "should build an AST Table" actual

    it "should fail to build an ast table if the source file is not found" $ do
      let
        sourceA = unlines
          [ "import { fn2 } from \"./sourceB\""
          , "fn :: Number -> Number -> Number"
          , "fn = (a, b) => (fn2(a, b) + a)"
          ]

        (Right astA) = buildAST "/fixtures/sourceA.mad" sourceA

        files        = M.fromList [("/fixtures/sourceA.mad", sourceA)]

        rf           = makeReadFile files
        pathUtils    = defaultPathUtils { readFile = rf }

        actual       = unsafePerformIO
          $ buildASTTable' pathUtils "" Nothing "/fixtures/sourceA.mad"
      snapshotTest
        "should fail to build an ast table if the source file is not found"
        actual

    -- TODO: Add tests for other error constructors than ImportNotFound

    -- TODO: test for path resolution ( fixtures not hardcoded )
    it "should figure out the root directory" $ do
      let
        sourceA = unlines
          [ "import { fn2 } from \"./sourceB\""
          , "fn :: Number -> Number -> Number"
          , "fn = (a, b) => (fn2(a, b) + a)"
          ]
        sourceB =
          unlines
            [ "fn2 :: Number -> Number -> Number"
            , "export fn2 = (a, b) => (a + b)"
            ]

        (Right astA) = buildAST "/src/sourceA.mad" sourceA
        (Right astB) = buildAST "/src/sourceB.mad" sourceB

        files        = M.fromList
          [("/src/sourceA.mad", sourceA), ("/src/sourceB.mad", sourceB)]

        rf        = makeReadFile files
        pathUtils = defaultPathUtils { readFile = rf }

        actual    = unsafePerformIO
          $ buildASTTable' pathUtils "" Nothing "/src/sourceA.mad"

      snapshotTest "should figure out the root directory" actual


  describe "buildAST" $ do
    it "should return a GrammarError of the source is not valid" $ do
      let
        source =
          unlines ["fn :: Number -> Number -> Number", "fn : a, b => (a + b)"]
        actual   = buildAST "source.mad" source
        expected = Left $ InferError
          (GrammarError
            "source.mad"
            "Syntax error - line: 2, column: 4\nThe following token is not valid: TokenColon"
          )
          NoReason
      actual `shouldBe` expected

    it "should return a valid AST with boolean expressions" $ do
      let source = unlines
            ["fn :: Boolean -> Boolean -> Boolean", "fn = (a, b) => (a == b)"]
          ast    = buildAST "source.mad" source
          actual = case ast of
            Right _ -> True
            Left  _ -> False

      actual `shouldBe` True

    it "should return a valid AST with True literals" $ do
      let source =
            unlines ["fn :: Boolean -> Boolean", "fn = (a) => (a == True)"]
          ast    = buildAST "source.mad" source
          actual = case ast of
            Right _ -> True
            Left  _ -> False

      actual `shouldBe` True

    it "should return a valid AST with False literals" $ do
      let source =
            unlines ["fn :: Boolean -> Boolean", "fn = (a) => (a == False)"]
          ast    = buildAST "source.mad" source
          actual = case ast of
            Right _ -> True
            Left  _ -> False

      actual `shouldBe` True
