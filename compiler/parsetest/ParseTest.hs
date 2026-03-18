module Main where

import Parse.Megaparsec.Madlib (parse)

main :: IO ()
main = do
  let tests =
        [ ("simple import",  "import IO from \"IO\"\n")
        , ("named import",   "import { foo, bar } from \"Foo\"\n")
        , ("type import",    "import type { Foo } from \"Foo\"\n")
        , ("assignment",     "x = 42\n")
        , ("function",       "f = (x) => x + 1\n")
        ]
  mapM_ run tests
  where
    run (name, code) = case parse code of
      Left e  -> putStrLn $ "FAIL [" ++ name ++ "]: " ++ e
      Right _ -> putStrLn $ "OK   [" ++ name ++ "]"
