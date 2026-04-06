module Format.FormatSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldBe
                                                )
import           Run.Format (parseCodeToFormat)
import           Format.Format (astToSource)


-- | Parse and format code, returning the formatted string
formatCode :: String -> IO String
formatCode code = do
  result <- parseCodeToFormat code
  case result of
    Right [(ast, comments)] ->
      return $ astToSource 80 ast comments
    _ ->
      return ""


-- | Format code twice and check idempotency
formatTwice :: String -> IO String
formatTwice code = do
  first <- formatCode code
  formatCode first


spec :: Spec
spec = do
  describe "JS block formatting" $ do
    it "preserves inline JS blocks on a single line" $ do
      let input = "x = #- 42 -#\n"
      result <- formatCode input
      result `shouldBe` "x = #- 42 -#\n"

    it "preserves newline before closing -# in multiline JS blocks" $ do
      let input = "x = #-{\n  const a = 1\n  return a\n}-#\n"
      result <- formatCode input
      -- The closing -# should be on its own line
      last (lines result) `shouldBe` "}-#"

    it "multiline JS block formatting is idempotent" $ do
      let input = "x = #-{\n  const a = 1\n  return a\n}-#\n"
      first <- formatCode input
      second <- formatCode first
      first `shouldBe` second

    it "inline JS block formatting is idempotent" $ do
      let input = "x = #- 42 -#\n"
      first <- formatCode input
      second <- formatCode first
      first `shouldBe` second

    it "multiline JS block with leading newline preserves structure" $ do
      let input = "x = #-\n  const a = 1\n  return a\n-#\n"
      result <- formatCode input
      -- The result should have -# on its own line
      let resultLines = lines result
      last resultLines `shouldBe` "-#"

    it "multiline JS block formatting with leading newline is idempotent" $ do
      let input = "x = #-\n  const a = 1\n  return a\n-#\n"
      first <- formatCode input
      second <- formatCode first
      first `shouldBe` second
