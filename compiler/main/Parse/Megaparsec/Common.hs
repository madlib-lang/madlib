{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Common
  ( Parser
  , ParserState(..)
  , ParseRecoveryError(..)
  , sc
  , scn
  , lexeme
  , symbol
  , rets
  , maybeRet
  , getLoc
  , withArea
  , pSourceTarget
  , getSourceTarget
  , setSourceTarget
  , getFormatterMode
  , addRecoveryError
  , runMadlibParser
  , runMadlibParserForFormatter
  , runMadlibParserWithState
  , chr8
  , chr8'
  , isAlphaB
  , isDigitB
  , isAlphaNumB
  , isUpperB
  , isLowerB
  , isHexDigitB
  , isIdentB
  ) where

import           Data.Word                      ( Word8 )
import qualified Data.ByteString               as BS
import           Data.Void
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict     ( State, evalState, runState, get, gets, modify', lift )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec                as MP
import qualified Text.Megaparsec.Byte          as C
import qualified Text.Megaparsec.Byte.Lexer    as L

import           AST.Source                     ( SourceTarget(..) )
import           Explain.Location               ( Loc(..), Area(..) )
import           Parse.Megaparsec.Error


-- | Error recorded during recovery parsing
data ParseRecoveryError = ParseRecoveryError
  { preLineStart :: !Int
  , preColStart :: !Int
  , preLineEnd :: !Int
  , preColEnd :: !Int
  , preMessage :: !String
  } deriving (Show)

-- | Parser state tracking source target and formatter mode
-- psLastTokenEnd: the Loc of the end of the last parsed token (before trailing whitespace).
-- Populated only by `lexeme`.
data ParserState = ParserState
  { psSourceTarget :: !SourceTarget
  , psFormatterMode :: !Bool
  , psLastTokenEnd :: !Loc
  , psRecoveryErrors :: ![ParseRecoveryError]
  } deriving (Show)


-- | The main parser type
type Parser = ParsecT CustomError BS.ByteString (State ParserState)


-- | Run a parser
runMadlibParser :: Parser a -> String -> BS.ByteString -> Either (ParseErrorBundle BS.ByteString CustomError) a
runMadlibParser p name input =
  let initialState = ParserState TargetAll False (Loc 0 0 0) []
  in  evalState (runParserT p name input) initialState


-- | Run a parser in formatter mode
runMadlibParserForFormatter :: Parser a -> String -> BS.ByteString -> Either (ParseErrorBundle BS.ByteString CustomError) a
runMadlibParserForFormatter p name input =
  let initialState = ParserState TargetAll True (Loc 0 0 0) []
  in  evalState (runParserT p name input) initialState


-- | Get current source target
{-# INLINE getSourceTarget #-}
getSourceTarget :: Parser SourceTarget
getSourceTarget = lift $ gets psSourceTarget


-- | Set current source target
setSourceTarget :: SourceTarget -> Parser ()
setSourceTarget target = lift $ modify' (\s -> s { psSourceTarget = target })


-- | Get whether we're in formatter mode
getFormatterMode :: Parser Bool
getFormatterMode = lift $ gets psFormatterMode


-- | Add a recovery error to the parser state
addRecoveryError :: ParseRecoveryError -> Parser ()
addRecoveryError err = lift $ modify' (\s -> s { psRecoveryErrors = err : psRecoveryErrors s })


-- | Run a parser and return both result and final state
runMadlibParserWithState :: Parser a -> String -> BS.ByteString -> (Either (ParseErrorBundle BS.ByteString CustomError) a, ParserState)
runMadlibParserWithState p name input =
  let initialState = ParserState TargetAll False (Loc 0 0 0) []
  in  runState (runParserT p name input) initialState


-- | Get current location as a Loc (calls getSourcePos).
{-# INLINE getLoc #-}
getLoc :: Parser Loc
getLoc = do
  !offset <- getOffset
  !pos <- MP.getSourcePos
  return $! Loc offset (unPos $ sourceLine pos) (unPos $ sourceColumn pos)


-- | Run a parser and wrap its result with area information.
-- Captures end position BEFORE trailing whitespace using psLastTokenEnd.
{-# INLINE withArea #-}
withArea :: Parser a -> Parser (Area, a)
withArea p = do
  start  <- getLoc
  result <- p
  end    <- lift $ gets psLastTokenEnd
  return (Area start end, result)


-- | Get the current source target
{-# INLINE pSourceTarget #-}
pSourceTarget :: Parser SourceTarget
pSourceTarget = getSourceTarget


-- Space consumers --

-- | Space consumer that does NOT consume newlines.
-- Fast path: peek first byte to avoid all work when there's no whitespace/comment.
-- In the common case (no trailing whitespace), just one lookAhead + return ().
{-# INLINE sc #-}
sc :: Parser ()
sc = do
  mb <- optional (lookAhead anySingle)
  case mb of
    Nothing -> return ()
    Just b
      | b == 47 ->                        -- '/' : possible comment
          skipMany scOne
      | b == 32 || b == 9 || b == 12 || b == 11 || b == 13 ->
          -- horizontal whitespace: consume then check for more/comments
          void (takeWhile1P Nothing (\b' -> b' == 32 || b' == 9 || b' == 12 || b' == 11 || b' == 13))
          *> skipMany scOne
      | otherwise -> return ()
  where
    {-# INLINE scOne #-}
    scOne :: Parser ()
    scOne = choice
      [ void $ takeWhile1P Nothing (\b -> b == 32 || b == 9 || b == 12 || b == 11 || b == 13)
      , L.skipLineComment "//"
      , L.skipBlockCommentNested "/*" "*/"
      ]


-- | Space consumer that DOES consume newlines
scn :: Parser ()
scn = L.space C.space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")


-- | Wrap a parser to consume trailing whitespace (no newlines).
-- Records end position BEFORE consuming trailing whitespace in psLastTokenEnd.
-- getSourcePos is incremental in megaparsec (scans from last known pos),
-- so this is amortized O(1) per token across the whole file.
{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = do
  result <- p
  !offset <- getOffset
  !pos <- MP.getSourcePos
  lift $! modify' (\s -> s { psLastTokenEnd = Loc offset (unPos $ sourceLine pos) (unPos $ sourceColumn pos) })
  sc
  return result


-- | Parse a symbol and consume trailing whitespace (no newlines)
{-# INLINE symbol #-}
symbol :: BS.ByteString -> Parser BS.ByteString
symbol = L.symbol sc


-- | Parse zero or more newlines (with optional whitespace and comments around them)
{-# INLINE rets #-}
rets :: Parser ()
rets = sc *> skipMany (C.newline *> sc)


-- | Parse zero or one newlines (with optional whitespace).
-- Fast path: peek at first byte — skip whole thing if it's not space, '/', or newline.
{-# INLINE maybeRet #-}
maybeRet :: Parser ()
maybeRet = do
  mb <- optional (lookAhead anySingle)
  case mb of
    Nothing -> return ()
    Just b
      | b == 10 -> void $ C.newline *> sc  -- '\n': consume newline + trailing whitespace
      | b == 32 || b == 9 || b == 12 || b == 11 || b == 13 || b == 47 ->
          -- horizontal whitespace or '/': run full sc, then optional newline
          sc *> (void (C.newline *> sc) <|> pure ())
      | otherwise -> return ()


-- ASCII range helpers --

{-# INLINE chr8 #-}
chr8 :: Char -> Word8
chr8 = fromIntegral . fromEnum

{-# INLINE chr8' #-}
chr8' :: Word8 -> Char
chr8' = toEnum . fromIntegral

{-# INLINE isAlphaB #-}
isAlphaB :: Word8 -> Bool
isAlphaB b = (b >= 65 && b <= 90) || (b >= 97 && b <= 122)

{-# INLINE isDigitB #-}
isDigitB :: Word8 -> Bool
isDigitB b = b >= 48 && b <= 57

{-# INLINE isAlphaNumB #-}
isAlphaNumB :: Word8 -> Bool
isAlphaNumB b = isAlphaB b || isDigitB b

{-# INLINE isUpperB #-}
isUpperB :: Word8 -> Bool
isUpperB b = b >= 65 && b <= 90

{-# INLINE isLowerB #-}
isLowerB :: Word8 -> Bool
isLowerB b = b >= 97 && b <= 122

{-# INLINE isHexDigitB #-}
isHexDigitB :: Word8 -> Bool
isHexDigitB b = isDigitB b || (b >= 65 && b <= 70) || (b >= 97 && b <= 102)

{-# INLINE isIdentB #-}
isIdentB :: Word8 -> Bool
isIdentB b = isAlphaNumB b || b == 95 || b == 39
