module MegaparsecSpec where

import Test.Hspec
import Parse.Megaparsec.Madlib (parse)
import System.IO (readFile)
import Prelude hiding (readFile)

preludeDir :: FilePath
preludeDir = "/Users/arnaudboeglin/Code/madlib/prelude/__internal__"

parseFile :: FilePath -> IO ()
parseFile path = do
  code <- readFile path
  case parse code of { Left e -> expectationFailure e; Right _ -> return () }

spec :: Spec
spec = describe "Megaparsec parser" $ do
  it "parses the Brekk fixture" $
    parse brekk `shouldSatisfy` isRight

  it "parses inline a+!b" $
    parse "x = a + !b" `shouldSatisfy` isRight

  it "parses inline a+!b in parens" $
    parse "x = a + (!(b))" `shouldSatisfy` isRight

  it "parses type annotation with multiple composite type args" $
    parse "processSelfPlayerUpdate :: List Polygon -> PlayerData -> List Snapshot -> Barbarian -> List Snapshot\nprocessSelfPlayerUpdate = (x) => x\n" `shouldSatisfy` isRight

  -- Regression: comment on its own line inside if/else-if braced body caused
  -- pIf' to fail (maybeRet was used instead of rets, so the newline after the
  -- comment was not consumed, leaving pExp unable to parse the next expression)
  it "parses comment inside if-else braced body" $
    parse (unlines
      [ "f = (x) => if (x > 0) {"
      , "  1"
      , "} else if (x < 0) {"
      , "  // negative case"
      , "  2"
      , "} else {"
      , "  0"
      , "}"
      ]) `shouldSatisfy` isRight

  it "parses comment inside if-else braced body (with type annotation)" $
    parse (unlines
      [ "f :: Integer -> Integer"
      , "f = (x) => if (x > 0) {"
      , "  1"
      , "} else if (x < 0) {"
      , "  // negative case"
      , "  2"
      , "} else {"
      , "  0"
      , "}"
      ]) `shouldSatisfy` isRight

  -- Regression: comment inside a function call argument list caused the function
  -- body to fail to parse, making the outer typed annotation try backtrack
  it "parses comment inside function call args" $
    parse (unlines
      [ "f :: Float -> {}"
      , "f = (x) => {"
      , "  g("
      , "    // comment"
      , "    x,"
      , "  )"
      , "}"
      ]) `shouldSatisfy` isRight

  -- Regression: type annotation after derive declaration failed
  it "parses type annotation after derive declaration" $
    parse (unlines
      [ "derive Comparable {x, y}"
      , ""
      , ""
      , "f :: Integer -> Integer"
      , "f = (x) => x"
      ]) `shouldSatisfy` isRight

  it "parses type annotation with type-var args after derive" $
    parse (unlines
      [ "derive Comparable {x, y}"
      , ""
      , ""
      , "moveRedLeft :: Dictionary k v -> Dictionary k v"
      , "moveRedLeft = (x) => x"
      ]) `shouldSatisfy` isRight

  it "parses comment inside function arg list then next named typed export" $
    parse (unlines
      [ "drawAttackVfx :: Float -> {}"
      , "export drawAttackVfx = (now) => {"
      , "  if (now > 0.2) do {"
      , "    Model.drawEx("
      , "      barbarian.attack1VfxModel,"
      , "      // { x: 0.9, y: 0.7, z: 0.5 },"
      , "      { x: 0.7, y: 0.7, z: 1 },"
      , "      { r: 0, g: 0, b: 0, a: 255 },"
      , "    )"
      , "  }"
      , "}"
      , ""
      , ""
      , "draw :: Float -> {}"
      , "export draw = (now) => {"
      , "  now"
      , "}"
      ]) `shouldSatisfy` isRight

  it "parses if-do no-else + comment in args + named typed export" $
    parse (unlines
      [ "drawAttackVfx :: Float -> {}"
      , "export drawAttackVfx = (now, camera, barbarian) => {"
      , "  attackDt = now - barbarian.lastAttackTimestamp"
      , "  if (attackDt < 0.7 && attackDt > 0.2) do {"
      , "    Model.drawEx("
      , "      barbarian.attack1VfxModel,"
      , "      // { x: 0.9, y: 0.7, z: 0.5 },"
      , "      { x: 0.7, y: 0.7, z: 1 },"
      , "    )"
      , "  }"
      , "}"
      , ""
      , ""
      , "draw :: Float -> {}"
      , "export draw = (now, barbarian) => {"
      , "  now"
      , "}"
      ]) `shouldSatisfy` isRight

  -- Regression: multiline array index access (e.g. arr[\n  expr\n]) inside a
  -- named typed export caused the body to fail, backtracking the typed annotation
  it "parses multiline array index access" $
    parse (unlines
      [ "draw :: Float -> {}"
      , "export draw = (x) => {"
      , "  y = arr["
      , "  x + 1"
      , "  ]"
      , "}"
      , ""
      , ""
      , "g :: Float -> {}"
      , "export g = (v) => v"
      ]) `shouldSatisfy` isRight

  -- Regression: pattern starting with '_' (e.g. __BUILTINS__.Constructor) was
  -- not tried as a composite pattern because 'isUpperB' returned false for '_'
  it "parses __BUILTINS__-qualified constructor pattern" $
    parse (unlines
      [ "f = (dict) => where(dict) {"
      , "  __BUILTINS__.DictRBNode(c, k) =>"
      , "    c"
      , "}"
      , ""
      , ""
      , "g :: Integer -> Integer"
      , "g = (x) => x"
      ]) `shouldSatisfy` isRight

  it "parses Barbarian.mad" $
    parseFile "/Users/arnaudboeglin/Code/mmorpg/src/client/Barbarian.mad"

  it "parses Pathfinding.mad" $
    parseFile "/Users/arnaudboeglin/Code/mmorpg/src/common/Pathfinding.mad"

  mapM_ (\name -> it ("parses " ++ name) $ parseFile (preludeDir ++ "/" ++ name))
    [ "Wish.mad"
    , "__BUILTINS__.mad"
    , "Alternative.mad"
    , "Applicative.mad"
    , "Array.mad"
    , "Bifunctor.mad"
    , "Byte.mad"
    , "ByteArray.mad"
    , "Char.mad"
    , "Compare.mad"
    , "Control.mad"
    , "Crypto.mad"
    , "Date.mad"
    , "Dictionary.mad"
    , "Directory.mad"
    , "Either.mad"
    , "File.mad"
    , "FilePath.mad"
    , "Float.mad"
    , "Function.mad"
    , "Functor.mad"
    , "Http.mad"
    , "IO.mad"
    , "Integer.mad"
    , "List.mad"
    , "Math.mad"
    , "Maybe.mad"
    , "Monad.mad"
    , "MonadRec.mad"
    , "Network.mad"
    , "Number.mad"
    , "Parse.mad"
    , "PrettyPrint.mad"
    , "Process.mad"
    , "Random.mad"
    , "Scan.mad"
    , "Set.mad"
    , "Short.mad"
    , "Show.mad"
    , "Stream.mad"
    , "String.mad"
    , "Terminal.mad"
    , "Test.mad"
    , "Thread.mad"
    , "Tuple.mad"
    , "Url.mad"
    , "__Coverage__.mad"
    , "__IOError__.mad"
    ]

brekk :: String
brekk = unlines
  [ "import IO from \"IO\""
  , "f = (r) => {"
  , "  return where(r) {"
  , "    { x, sub: { y, ...g } } =>"
  , "      g"
  , "  }"
  , "}"
  , "main = () => {"
  , "  r2 = f({ x: 1, sub: { y: 2, z: 3 } })"
  , "  IO.log(r2)"
  , "}"
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
