{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Common
  ( Parser
  , ParserState(..)
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
  , runMadlibParser
  , runMadlibParserForFormatter
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import           Data.Void
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict     ( State, evalState, get, gets, modify', lift )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec                as MP
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L

import           AST.Source                     ( SourceTarget(..) )
import           Explain.Location               ( Loc(..), Area(..) )
import           Parse.Megaparsec.Error


-- | Parser state tracking source target and formatter mode
data ParserState = ParserState
  { psSourceTarget :: !SourceTarget
  , psFormatterMode :: !Bool
  , psLastTokenEnd :: !Loc
  } deriving (Show)


-- | The main parser type
-- Uses ParsecT with custom errors over Text input, with strict State for mutable parser state
type Parser = ParsecT CustomError Text (State ParserState)


-- | Run a parser
runMadlibParser :: Parser a -> String -> Text -> Either (ParseErrorBundle Text CustomError) a
runMadlibParser p name input =
  let initialState = ParserState TargetAll False (Loc 0 0 0)
  in  evalState (runParserT p name input) initialState


-- | Run a parser in formatter mode
runMadlibParserForFormatter :: Parser a -> String -> Text -> Either (ParseErrorBundle Text CustomError) a
runMadlibParserForFormatter p name input =
  let initialState = ParserState TargetAll True (Loc 0 0 0)
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


-- | Get current location as a Loc
-- NOTE: getSourcePos is O(n) from last checkpoint — call sparingly.
{-# INLINE getLoc #-}
getLoc :: Parser Loc
getLoc = do
  offset <- getOffset
  pos <- MP.getSourcePos
  return $! Loc offset (unPos $ sourceLine pos) (unPos $ sourceColumn pos)


-- | Get only the byte offset as a Loc (line=0, col=0 placeholders)
-- O(1) alternative to getLoc for internal use where line/col not needed
{-# INLINE getLocFast #-}
getLocFast :: Parser Loc
getLocFast = do
  offset <- getOffset
  return $! Loc offset 0 0


-- | Run a parser and wrap its result with area information
-- Captures end position BEFORE trailing whitespace using psLastTokenEnd
{-# INLINE withArea #-}
withArea :: Parser a -> Parser (Area, a)
withArea p = do
  start <- getLoc
  result <- p
  end <- lift $ gets psLastTokenEnd
  return (Area start end, result)


-- | Get the current source target
{-# INLINE pSourceTarget #-}
pSourceTarget :: Parser SourceTarget
pSourceTarget = getSourceTarget


-- Space consumers --

-- | Space consumer that does NOT consume newlines
-- Handles spaces, tabs, and comments
sc :: Parser ()
sc = L.space
  (void $ takeWhile1P Nothing (\c -> c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r'))
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")


-- | Space consumer that DOES consume newlines
scn :: Parser ()
scn = L.space
  C.space1
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")


-- | Wrap a parser to consume trailing whitespace (no newlines)
-- Records the end position BEFORE consuming trailing whitespace in psLastTokenEnd
{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme p = do
  result <- p
  end <- getLoc
  lift $ modify' (\s -> s { psLastTokenEnd = end })
  sc
  return result


-- | Parse a symbol and consume trailing whitespace (no newlines)
symbol :: Text -> Parser Text
symbol = L.symbol sc


-- | Parse zero or more newlines (with optional whitespace and comments around them)
-- This corresponds to the `rets` non-terminal in the Happy grammar
{-# INLINE rets #-}
rets :: Parser ()
rets = sc *> skipMany (C.newline *> sc)


-- | Parse zero or one newlines (with optional whitespace)
-- Corresponds to `maybeRet` in the Happy grammar
{-# INLINE maybeRet #-}
maybeRet :: Parser ()
maybeRet = sc *> void (optional (C.newline *> sc))
