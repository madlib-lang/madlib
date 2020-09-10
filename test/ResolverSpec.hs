module ResolverSpec where

import qualified Data.Map   as M
import           Grammar
import           Resolver
import           Test.Hspec

tester :: String -> Either String Program
tester code =  resolve initialEnv =<< buildAST code
  where initialEnv = Env M.empty M.empty

spec :: Spec
spec = do
  describe "compiler" $ do
    it "can" $ do
      let code = unlines ["fn :: Num -> Num -> Num"
                         ,"fn = (a, b) => a + b"]
          actual = case tester code of
            (Right _) -> True
            _         -> False

      actual `shouldBe` True
