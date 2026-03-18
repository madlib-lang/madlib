{-# LANGUAGE OverloadedStrings #-}
module Parse.Megaparsec.Escape
  ( processEscapes
  , processHexaEscapes
  , isHexaDigit
  ) where

import           Data.Char                      ( chr, isHexDigit, digitToInt )
import           Numeric                        ( readHex )
import qualified Text.ParserCombinators.ReadP   as ReadP
import qualified Data.Char                      as Char

import           Explain.Location               ( Loc(..), Area(..) )


-- | Check if a character is a hex digit
isHexaDigit :: Char -> Bool
isHexaDigit = isHexDigit


-- | Parse characters using Haskell's readLitChar
charParser :: ReadP.ReadS [Char]
charParser = ReadP.readP_to_S $ ReadP.many $ ReadP.readS_to_P Char.readLitChar


-- | Interpret character escape sequences
interpretChars :: [Char] -> [Char]
interpretChars chars =
  let parsed  = charParser chars
      parsed' = fst $ last $ parsed
  in  parsed'


-- | Process hex and unicode escape sequences in a string
-- This is a pure version of the Alex lexer's processHexaEscapes
-- In formatter mode, returns the input unchanged
processHexaEscapes :: Bool -> String -> Either String String
processHexaEscapes isFormatter input
  | isFormatter = Right input
  | otherwise   = processEscapes input


-- | Process escape sequences in a string
processEscapes :: String -> Either String String
processEscapes input = case input of
  -- \u{HHHHHH} - Unicode code point
  '\\':'u':'{':more -> do
    let hexa = takeWhile (/= '}') more
        hexaLength = length hexa
    if hexaLength < 1 || hexaLength > 6 then
      Left $ "BadEscape: invalid unicode escape \\u{" ++ hexa ++ "}"
    else case interpretChars ("\\" ++ show (read ('0':'x':hexa) :: Int)) of
      [] ->
        Left $ "BadEscape: invalid unicode escape \\u{" ++ hexa ++ "}"
      chars -> do
        next <- processEscapes (tail $ dropWhile (/= '}') more)
        Right $ chars ++ next

  -- \uXXXX - 4-digit Unicode escape
  '\\':'u':a1:b1:c1:d1:more -> do
    if any (not . isHexaDigit) [a1, b1, c1, d1] then
      Left $ "BadEscape: invalid unicode escape \\u" ++ [a1, b1, c1, d1]
    else case interpretChars ['\\', 'x', a1, b1, c1, d1] of
      [] ->
        Left $ "BadEscape: invalid unicode escape \\u" ++ [a1, b1, c1, d1]
      chars -> do
        next <- processEscapes more
        Right $ chars ++ next

  -- \u with insufficient digits
  '\\':'u':_ ->
    Left $ "BadEscape: incomplete unicode escape"

  -- \xXX - 2-digit hex escape
  '\\':'x':a1:b1:more -> do
    if any (not . isHexaDigit) [a1, b1] then
      Left $ "BadEscape: invalid hex escape \\x" ++ [a1, b1]
    else case interpretChars ['\\', 'x', a1, b1] of
      [] ->
        Left $ "BadEscape: invalid hex escape \\x" ++ [a1, b1]
      chars -> do
        next <- processEscapes more
        Right $ chars ++ next

  -- \x with insufficient digits
  '\\':'x':_ ->
    Left $ "BadEscape: incomplete hex escape"

  -- All other characters (including \n, \t, \\, \" etc.) are passed through raw.
  -- The code generator is responsible for interpreting these escapes.
  -- This matches the behavior of the original Alex/Happy parser's processHexaEscapes.
  a1:more -> do
    next <- processEscapes more
    Right $ a1 : next

  -- End of string
  [] ->
    Right []
