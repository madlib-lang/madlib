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

    it "preserves raw regex escapes in string literals" $ do
      let expectedLines =
            [ "REGEX = {"
            , "  NON_WORD_AND_SPACES: \"[\\\\W\\\\s]\","
            , "  CAPTURING: {"
            , "    HEADING: \"^(#*)\\\\s*(.*)\","
            , "    MADLIB_FILE: \"\\\\b([\\\\w\\\\/\\\\.]*)\\\\.mad\","
            , "    LIST_ITEM: \" - (\\\\w*)\","
            , "  },"
            , "}"
            ]
          input = unlines
            [ "REGEX = {"
            , "  NON_WORD_AND_SPACES: \"[\\\\W\\\\s]\","
            , "  CAPTURING: {"
            , "    HEADING: \"^(#*)\\\\s*(.*)\","
            , "    MADLIB_FILE: \"\\\\b([\\\\w\\\\/\\\\.]*)\\\\.mad\","
            , "    LIST_ITEM: \" - (\\\\w*)\","
            , "  },"
            , "}"
            ]
      result <- formatCode input
      lines result `shouldBe` expectedLines

    it "does not add a blank line before the closing pipe paren" $ do
      let input = unlines
            [ "f = pipe("
            , "  // comment"
            , ""
            , "  (x) => x + 1,"
            , "  // comment"
            , ")"
            ]
      result <- formatCode input
      lines result `shouldBe`
        [ "f = pipe("
        , "  // comment"
        , ""
        , "  (x) => x + 1,"
        , "  // comment"
        , ")"
        ]

    it "preserves trailing blank lines in pipe comments without growing them" $ do
      let input = unlines
            [ "f = pipe("
            , "  // comment"
            , "  (x) => x + 1,"
            , ""
            , "  // comment"
            , ""
            , ")"
            ]
          expected = unlines
            [ "f = pipe("
            , "  // comment"
            , "  (x) => x + 1,"
            , ""
            , "  // comment"
            , ""
            , ")"
            ]
      result <- formatTwice input
      result `shouldBe` expected

    it "keeps Brekk2 pipe comments tight before the closing paren" $ do
      let input = unlines
            [ "REGEX = {"
            , "  NON_WORD_AND_SPACES: \"[\\\\W\\\\s]\","
            , "  CAPTURING: {"
            , "    HEADING: \"^(#*)\\\\s*(.*)\","
            , "    MADLIB_FILE: \"\\\\b([\\\\w\\\\/\\\\.]*)\\\\.mad\","
            , "    LIST_ITEM: \" - (\\\\w*)\","
            , "  },"
            , "}"
            , ""
            , "x = pipe("
            , "  // COM"
            , "  () => {},"
            , ""
            , "  // COM"
            , ")"
            ]
      result <- formatTwice input
      result `shouldBe` unlines
        [ "REGEX = {"
        , "  NON_WORD_AND_SPACES: \"[\\\\W\\\\s]\","
        , "  CAPTURING: {"
        , "    HEADING: \"^(#*)\\\\s*(.*)\","
        , "    MADLIB_FILE: \"\\\\b([\\\\w\\\\/\\\\.]*)\\\\.mad\","
        , "    LIST_ITEM: \" - (\\\\w*)\","
        , "  },"
        , "}"
        , ""
        , ""
        , "x = pipe("
        , "  // COM"
        , "  () => {},"
        , ""
        , "  // COM"
        , ")"
        ]

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

    it "keeps multiline template strings stable across formatting" $ do
      let input = unlines
            [ "MADLIB_DOT_JSON_MINIMAL = `{"
            , "  \"name\": \"SamplePackage\","
            , "  \"version\": \"0.1.7\","
            , "  \"main\": \"src/Main.mad\""
            , "}`"
            , ""
            , "MADLIB_DOT_JSON_WRONG_VERSION = `{"
            , "  \"name\": \"SamplePackage\","
            , "  \"version\": \"0.1.a\","
            , "  \"main\": \"src/Main.mad\""
            , "}`"
            ]
      result <- formatTwice input
      result `shouldBe` unlines
        [ "MADLIB_DOT_JSON_MINIMAL = `{"
        , "  \"name\": \"SamplePackage\","
        , "  \"version\": \"0.1.7\","
        , "  \"main\": \"src/Main.mad\""
        , "}`"
        , ""
        , ""
        , "MADLIB_DOT_JSON_WRONG_VERSION = `{"
        , "  \"name\": \"SamplePackage\","
        , "  \"version\": \"0.1.a\","
        , "  \"main\": \"src/Main.mad\""
        , "}`"
        ]

    it "preserves multiline block comments that contain line comments" $ do
      let input = unlines
            [ "/*"
            , "report("
            , "  parseFlags,"
            , "  \"parseFlags - strings with quotes\","
            , "  ["
            , "    #[[\"--flag='nice cool hooray'\"], [Flag(\"flag\", \"nice cool hooray\")]],"
            , "    #[[`--info=\"this is a whole sentence\"`], [Flag(\"info\", \"this is a whole sentence\")]],"
            , "  ],"
            , ")"
            , ""
            , "// these cases fail currently"
            , "// /*"
            , "report("
            , "  parseFlags,"
            , "  \"parseFlags - boolean sequence\","
            , "  ["
            , "    #["
            , "      [\"--flag\", \"--flag2\", \"--yet-another-flag\"],"
            , "      [on(\"flag\"), on(\"flag2\"), on(\"yet-another-flag\")],"
            , "    ],"
            , "    #[[\"--flag\", \"--no-dope\"], [on(\"flag\"), off(\"dope\")]],"
            , "  ],"
            , ")"
            , ""
            , "*/"
            ]
      result <- formatTwice input
      result `shouldBe` unlines
        [ ""
        , "/*"
        , "report("
        , "  parseFlags,"
        , "  \"parseFlags - strings with quotes\","
        , "  ["
        , "    #[[\"--flag='nice cool hooray'\"], [Flag(\"flag\", \"nice cool hooray\")]],"
        , "    #[[`--info=\"this is a whole sentence\"`], [Flag(\"info\", \"this is a whole sentence\")]],"
        , "  ],"
        , ")"
        , ""
        , "// these cases fail currently"
        , "// /*"
        , "report("
        , "  parseFlags,"
        , "  \"parseFlags - boolean sequence\","
        , "  ["
        , "    #["
        , "      [\"--flag\", \"--flag2\", \"--yet-another-flag\"],"
        , "      [on(\"flag\"), on(\"flag2\"), on(\"yet-another-flag\")],"
        , "    ],"
        , "    #[[\"--flag\", \"--no-dope\"], [on(\"flag\"), off(\"dope\")]],"
        , "  ],"
        , ")"
        , ""
        , "*/"
        ]
