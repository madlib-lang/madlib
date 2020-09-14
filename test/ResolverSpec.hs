module ResolverSpec where

import qualified Data.Map   as M
import           Grammar
import           Resolver
import           Test.Hspec

tester :: String -> Either String AST
tester code =  resolve initialEnv =<< buildAST code
  where initialEnv = Env M.empty M.empty


spec :: Spec
spec = do
  describe "compiler" $ do
    it "should resolve functions that add parameters" $ do
      let code = unlines ["fn :: Num -> Num -> Num"
                         ,"fn = (a, b) => a + b"
                         ]
          actual = case tester code of
            (Right _) -> True
            _         -> False
      actual `shouldBe` True

    it "should resolve function calls" $ do
      let code = unlines ["fn :: Num -> Num -> Num"
                         ,"fn = (a, b) => a + b"
                         , "fn2 :: Num -> Num -> Num"
                         , "fn2 = (a, b) => fn(a, b) + a"
                         ]
          actual = case tester code of
            (Right _) -> True
            _         -> False
      actual `shouldBe` True

    it "should give an error if parameter count does not match the one of the signature" $ do
      let code = unlines ["fn :: Num -> Num -> Num -> Num"
                         ,"fn = (a, b) => a + b"]
          actual = tester code
      actual `shouldBe` Left "Error: () - Parameter count and signature don't match !"
