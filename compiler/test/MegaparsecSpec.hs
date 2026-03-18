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
