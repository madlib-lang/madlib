module Utils.URL where

sanitize :: String -> String
sanitize = (replaceSpecialChar <$>)

replaceSpecialChar :: Char -> Char
replaceSpecialChar c = case c of
  ':' -> '_'
  '/' -> '_'
  '#' -> '_'
  '.' -> '_'
  '?' -> '_'
  '&' -> '_'
  '=' -> '_'
  '-' -> '_'
  _   -> c
