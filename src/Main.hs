module Main where

import           Debug.Trace
import           Grammar
import           Lexer

main :: IO ()
main = interact compile

-- TBD
compile :: String -> String
compile x =
  case parse x of
    Left a  -> "\nBAD: " ++ a ++ "\n"
    Right b -> "\nGOOD: " ++ show b ++ "\n"
