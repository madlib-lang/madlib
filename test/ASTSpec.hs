{-# LANGUAGE FlexibleInstances #-}
module ASTSpec where

import qualified Data.Map                      as M
import qualified Data.Either                   as E
import           AST
import           Test.Hspec
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Reader

type MockFiles = M.Map FilePath String

instance FileReader (Reader MockFiles) where
  readFile path = do
    files <- ask
    case M.lookup path files of
      Just f  -> return f
      Nothing -> error $ "File not found for path: " <> path

spec :: Spec
spec = do
  describe "AST" $ do
    it "buildASTTable should build an AST Table" $ do
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
        astTable = M.fromList [("fixtures/sourceA.mad", astA)]
        files    = M.fromList
          [("fixtures/sourceA.mad", sourceA), ("fixtures/sourceB.mad", sourceB)]

      runReader (buildASTTable "fixtures/sourceA.mad") files `shouldBe` expected
