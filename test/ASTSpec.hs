{-# LANGUAGE FlexibleInstances #-}
module ASTSpec where

import qualified Data.Map                      as M
import           AST
import           Test.Hspec
import           Text.Show.Pretty               ( ppShow )
import           GHC.IO.Exception

type MockFiles = M.Map FilePath String

makeReadFile :: MockFiles -> FilePath -> IO String
makeReadFile files path = do
  case M.lookup path files of
    Just f  -> return f
    Nothing -> ioError IOError { ioe_handle      = Nothing
                               , ioe_type        = NoSuchThing
                               , ioe_location    = ""
                               , ioe_description = ""
                               , ioe_errno       = Nothing
                               , ioe_filename    = Just path
                               }

-- TODO: Make tests more dry !
spec :: Spec
spec = do
  describe "findAST" $ do
    it "should return a Right AST if the ast exists" $ do
      let source =
            unlines ["fn :: Num -> Num -> Num", "fn = (a, b) => fn2(a, b) + a"]

          (Right ast) = buildAST "fixtures/source.mad" source

          expected    = Right ast
          files       = M.fromList [("fixtures/source.mad", source)]

          rf          = makeReadFile files

      r <- buildASTTable' rf Nothing "fixtures/" "fixtures/source.mad"
      let actual = r >>= findAST "fixtures/source.mad"
      actual `shouldBe` expected

    it "should return a Left ASTNotFound if it does not exist" $ do
      let source =
            unlines ["fn :: Num -> Num -> Num", "fn = (a, b) => fn2(a, b) + a"]

          (Right ast) = buildAST "fixtures/source.mad" source

          expected    = Left (ASTNotFound "fixtures/source-not-there.mad")
          files       = M.fromList [("fixtures/source.mad", source)]

          rf          = makeReadFile files

      r <- buildASTTable' rf Nothing "fixtures/" "fixtures/source.mad"
      let actual = r >>= findAST "fixtures/source-not-there.mad"
      actual `shouldBe` expected

  describe "buildASTTable" $ do
    it "should build an AST Table" $ do
      let
        sourceA = unlines
          [ "import \"sourceB\""
          , "fn :: Num -> Num -> Num"
          , "fn = (a, b) => fn2(a, b) + a"
          ]
        sourceB = unlines ["fn2 :: Num -> Num -> Num", "fn2 = (a, b) => a + b"]

        (Right astA) = buildAST "fixtures/sourceA.mad" sourceA
        (Right astB) = buildAST "fixtures/sourceB.mad" sourceB

        expected     = Right
          (M.fromList
            [("fixtures/sourceA.mad", astA), ("fixtures/sourceB.mad", astB)]
          )
        files = M.fromList
          [("fixtures/sourceA.mad", sourceA), ("fixtures/sourceB.mad", sourceB)]

        rf = makeReadFile files

      r <- buildASTTable' rf Nothing "fixtures/" "fixtures/sourceA.mad"
      r `shouldBe` expected

    it "should fail if the source file is not found" $ do
      let sourceA = unlines
            [ "import \"sourceB\""
            , "fn :: Num -> Num -> Num"
            , "fn = (a, b) => fn2(a, b) + a"
            ]

          (Right astA) = buildAST "fixtures/sourceA.mad" sourceA

          expected = Left $ ImportNotFound "fixtures/sourceB.mad" (Just astA)
          files        = M.fromList [("fixtures/sourceA.mad", sourceA)]

          rf           = makeReadFile files

      r <- buildASTTable' rf Nothing "fixtures/" "fixtures/sourceA.mad"
      r `shouldBe` expected

    -- TODO: Add tests for other error constructors than ImportNotFound

    -- TODO: test for path resolution ( fixtures not hardcoded )
    it "should figure out the root directory" $ do
      let
        sourceA = unlines
          [ "import \"sourceB\""
          , "fn :: Num -> Num -> Num"
          , "fn = (a, b) => fn2(a, b) + a"
          ]
        sourceB = unlines ["fn2 :: Num -> Num -> Num", "fn2 = (a, b) => a + b"]

        (Right astA) = buildAST "src/sourceA.mad" sourceA
        (Right astB) = buildAST "src/sourceB.mad" sourceB

        expected     = Right
          (M.fromList [("src/sourceA.mad", astA), ("src/sourceB.mad", astB)])
        files = M.fromList
          [("src/sourceA.mad", sourceA), ("src/sourceB.mad", sourceB)]

        rf = makeReadFile files

      r <- buildASTTable' rf Nothing "src/" "src/sourceA.mad"
      r `shouldBe` expected
