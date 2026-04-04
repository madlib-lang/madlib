{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
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
  -- Kept for any remaining call sites that import these:
  , chr8
  , chr8'
  , isAlphaB
  , isDigitB
  , isAlphaNumB
  , isUpperB
  , isLowerB
  , isHexDigitB
  , isIdentB
  , satisfyTok
  ) where

import           Data.Word                      ( Word8 )
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict     ( State, evalState, runState, get, gets, modify', lift )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec               as MP

import           AST.Source                     ( SourceTarget(..) )
import           Explain.Location               ( Loc(..), Area(..) )
import           Parse.Megaparsec.Error
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )
import           Parse.Lexer.TokenStream        ( TokenStream(..) )


-- | Error recorded during recovery parsing
data ParseRecoveryError = ParseRecoveryError
  { preLineStart :: !Int
  , preColStart  :: !Int
  , preLineEnd   :: !Int
  , preColEnd    :: !Int
  , preMessage   :: !String
  } deriving (Show)

-- | Parser state
data ParserState = ParserState
  { psSourceTarget   :: !SourceTarget
  , psFormatterMode  :: !Bool
  , psLastTokenEnd   :: !Loc    -- end of last consumed token
  , psRecoveryErrors :: ![ParseRecoveryError]
  } deriving (Show)


-- | The main parser type – operates on a token stream
type Parser = ParsecT CustomError TokenStream (State ParserState)


-- | Run a parser on a TokenStream
runMadlibParser :: Parser a -> String -> TokenStream -> Either (ParseErrorBundle TokenStream CustomError) a
runMadlibParser p name input =
  let initialState = ParserState TargetAll False (Loc 0 0 0) []
  in  evalState (runParserT p name input) initialState


-- | Run a parser in formatter mode
runMadlibParserForFormatter :: Parser a -> String -> TokenStream -> Either (ParseErrorBundle TokenStream CustomError) a
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
runMadlibParserWithState :: Parser a -> String -> TokenStream -> (Either (ParseErrorBundle TokenStream CustomError) a, ParserState)
runMadlibParserWithState p name input =
  let initialState = ParserState TargetAll False (Loc 0 0 0) []
  in  runState (runParserT p name input) initialState


-- | Get current location from the next token's start position.
-- At EOF returns the last consumed token's end.
{-# INLINE getLoc #-}
getLoc :: Parser Loc
getLoc = do
  ms <- optional (lookAhead anySingle)
  case ms of
    Just rt -> return (rtStart rt)
    Nothing -> lift $ gets psLastTokenEnd


-- | Run a parser and wrap its result with area information.
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


-- ── Whitespace / newline combinators ──────────────────────────────────────────
-- In the token stream, whitespace has already been stripped by Alex.
-- Newlines appear as TkNewline tokens.

-- | No-op: whitespace already stripped by lexer.
{-# INLINE sc #-}
sc :: Parser ()
sc = return ()

-- | Skip zero or more TkNewline tokens.
{-# INLINE scn #-}
scn :: Parser ()
scn = skipMany newlineTok

-- | "lexeme" wrapper: a no-op in the token-stream world.
-- Kept for API compatibility with call sites that use `lexeme p`.
-- In the token parser, tokens are already "lexemed" by Alex.
-- We still need to update psLastTokenEnd though — this is done inside
-- the token-consuming primitives in Lexeme.hs.
{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = id

-- | symbol: no-op kept for API compat
{-# INLINE symbol #-}
symbol :: a -> Parser a
symbol = return

-- | Skip zero or more newlines
{-# INLINE rets #-}
rets :: Parser ()
rets = scn

-- | Skip at most one newline token
{-# INLINE maybeRet #-}
maybeRet :: Parser ()
maybeRet = void $ optional newlineTok

-- | Consume a TkNewline token, updating psLastTokenEnd
{-# INLINE newlineTok #-}
newlineTok :: Parser ()
newlineTok = void $ satisfyTok (\rt -> rtToken rt == TkNewline)

-- | Consume any single token that satisfies a predicate, updating psLastTokenEnd.
{-# INLINE satisfyTok #-}
satisfyTok :: (RangedToken -> Bool) -> Parser RangedToken
satisfyTok f = do
  rt <- satisfy f
  lift $! modify' (\s -> s { psLastTokenEnd = rtEnd rt })
  return rt


-- ── ASCII range helpers (kept for any remaining import sites) ─────────────────

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
