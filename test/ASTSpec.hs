{-# LANGUAGE FlexibleInstances #-}
module ASTSpec where

import qualified Data.Map                      as M
import           AST
import           Test.Hspec
import           GHC.IO.Exception
import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Explain.Location
import           AST.Source

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

      r <- buildASTTable' rf "" Nothing "fixtures/" "fixtures/source.mad"
      let actual = r >>= flip findAST "fixtures/source.mad"
      actual `shouldBe` expected

    it "should return a Left ImportNotFound if it does not exist" $ do
      let
        source =
          unlines ["fn :: Num -> Num -> Num", "fn = (a, b) => fn2(a, b) + a"]

        (Right ast) = buildAST "fixtures/source.mad" source

        expected =
          Left
            (InferError (ImportNotFound "fixtures/source-not-there.mad")
                        NoReason
            )
        files = M.fromList [("fixtures/source.mad", source)]

        rf    = makeReadFile files

      r <- buildASTTable' rf "" Nothing "fixtures/" "fixtures/source.mad"
      let actual = r >>= flip findAST "fixtures/source-not-there.mad"
      actual `shouldBe` expected

  describe "buildASTTable" $ do
    it "should build an AST Table" $ do
      let
        sourceA = unlines
          [ "import { fn2 } from \"sourceB\""
          , "fn :: Num -> Num -> Num"
          , "fn = (a, b) => fn2(a, b) + a"
          ]
        sourceB =
          unlines ["fn2 :: Num -> Num -> Num", "export fn2 = (a, b) => a + b"]

        (Right astA) = buildAST "fixtures/sourceA.mad" sourceA
        (Right astB) = buildAST "fixtures/sourceB.mad" sourceB

        expected     = Right
          (M.fromList
            [("fixtures/sourceA.mad", astA), ("fixtures/sourceB.mad", astB)]
          )
        files = M.fromList
          [("fixtures/sourceA.mad", sourceA), ("fixtures/sourceB.mad", sourceB)]

        rf = makeReadFile files

      r <- buildASTTable' rf "" Nothing "fixtures/" "fixtures/sourceA.mad"
      r `shouldBe` expected

    it "should fail if the source file is not found" $ do
      let sourceA = unlines
            [ "import { fn2 } from \"sourceB\""
            , "fn :: Num -> Num -> Num"
            , "fn = (a, b) => fn2(a, b) + a"
            ]

          (Right astA) = buildAST "fixtures/sourceA.mad" sourceA

          expected     = Left $ InferError
            (ImportNotFound "fixtures/sourceB.mad")
            (Reason
              (WrongImport
                (Meta (Infos { nthArg = Nothing, origin = Nothing })
                      (Area (Loc 0 1 1) (Loc 29 1 30))
                      (NamedImport ["fn2"] "sourceB")
                )
              )
              ""
              (Area (Loc 0 1 1) (Loc 29 1 30))
            )
          files = M.fromList [("fixtures/sourceA.mad", sourceA)]

          rf    = makeReadFile files

      r <- buildASTTable' rf "" Nothing "fixtures/" "fixtures/sourceA.mad"
      r `shouldBe` expected

    -- TODO: Add tests for other error constructors than ImportNotFound

    -- TODO: test for path resolution ( fixtures not hardcoded )
    it "should figure out the root directory" $ do
      let
        sourceA = unlines
          [ "import { fn2 } from \"sourceB\""
          , "fn :: Num -> Num -> Num"
          , "fn = (a, b) => fn2(a, b) + a"
          ]
        sourceB =
          unlines ["fn2 :: Num -> Num -> Num", "export fn2 = (a, b) => a + b"]

        (Right astA) = buildAST "src/sourceA.mad" sourceA
        (Right astB) = buildAST "src/sourceB.mad" sourceB

        expected     = Right
          (M.fromList [("src/sourceA.mad", astA), ("src/sourceB.mad", astB)])
        files = M.fromList
          [("src/sourceA.mad", sourceA), ("src/sourceB.mad", sourceB)]

        rf = makeReadFile files

      r <- buildASTTable' rf "" Nothing "src/" "src/sourceA.mad"
      r `shouldBe` expected
  describe "buildAST" $ do
    it "should return a GrammarError if the source is not valid" $ do
      let
        source   = unlines ["fn :: Num -> Num -> Num", "fn : a, b => a + b"]
        actual   = buildAST "source.mad" source
        expected = Left $ InferError
          (GrammarError
            "source.mad"
            "Syntax error - line: 2, column: 4\nThe following token is not valid: TokenColon"
          )
          NoReason
      actual `shouldBe` expected

    it "should return a valid AST with boolean expressions" $ do
      let source =
            unlines ["fn :: Bool -> Bool -> Bool", "fn = (a, b) => a == b"]
          ast    = buildAST "source.mad" source
          actual = case ast of
            Right _ -> True
            Left  _ -> False

      actual `shouldBe` True

    it "should return a valid AST with True literals" $ do
      let source = unlines ["fn :: Bool -> Bool", "fn = (a) => a == True"]
          ast    = buildAST "source.mad" source
          actual = case ast of
            Right _ -> True
            Left  _ -> False

      actual `shouldBe` True

    it "should return a valid AST with False literals" $ do
      let source = unlines ["fn :: Bool -> Bool", "fn = (a) => a == False"]
          ast    = buildAST "source.mad" source
          actual = case ast of
            Right _ -> True
            Left  _ -> False

      actual `shouldBe` True
