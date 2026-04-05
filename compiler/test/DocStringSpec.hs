module DocStringSpec where

import Test.Hspec
import Parse.Megaparsec.DocString (parse)
import Parse.DocString.DocString


spec :: Spec
spec = describe "DocString parser" $ do
  describe "@param tag" $ do
    it "parses a single @param tag" $ do
      let input = "/** @param x the input\n*/\nexport myFunc = (x) => x\n"
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findParamTags tags `shouldBe` [("x", "the input")]

    it "parses multiple @param tags" $ do
      let input = "/** @param x first param\n * @param y second param\n*/\nexport myFunc = (x, y) => x\n"
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findParamTags tags `shouldBe` [("x", "first param"), ("y", "second param")]

  describe "@returns tag" $ do
    it "parses a @returns tag" $ do
      let input = "/** Does a thing\n * @returns the result\n*/\nexport myFunc = (x) => x\n"
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findReturnsTag tags `shouldBe` Just "the result"

  describe "@deprecated tag" $ do
    it "parses a @deprecated tag" $ do
      let input = "/** Old function\n * @deprecated use newFunc instead\n*/\nexport myFunc = (x) => x\n"
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findDeprecatedTag tags `shouldBe` Just "use newFunc instead"

  describe "combined tags" $ do
    it "parses @param, @returns, and @deprecated together" $ do
      let input = unlines
            [ "/**"
            , " * Does something"
            , " * @param input the input value"
            , " * @returns the output"
            , " * @deprecated use betterFunc instead"
            , " */"
            , "export myFunc = (input) => input"
            ]
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findParamTags tags    `shouldBe` [("input", "the input value")]
          findReturnsTag tags   `shouldBe` Just "the output"
          findDeprecatedTag tags `shouldBe` Just "use betterFunc instead"

  describe "@example and @since still work" $ do
    it "parses @example tag" $ do
      let input = "/** @example myFunc(42)\n*/\nexport myFunc = (x) => x\n"
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findExampleTag tags `shouldBe` Just "myFunc(42)"

    it "parses @since tag" $ do
      let input = "/** @since 1.0.0\n*/\nexport myFunc = (x) => x\n"
      case parse input of
        Left err -> expectationFailure err
        Right ds -> do
          let tags = concatMap getFunctionTags ds
          findSinceTag tags `shouldBe` Just "1.0.0"


getFunctionTags :: DocString -> [DocStringTag]
getFunctionTags ds = case ds of
  FunctionDoc _ _ _ tags -> tags
  _                      -> []
