{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Lexeme
  ( -- Keywords
    pKeyword
  , pImport
  , pExport
  , pFrom
  , pType
  , pAlias
  , pExtern
  , pIf
  , pElse
  , pWhile
  , pWhere
  , pDo
  , pReturn
  , pInterface
  , pInstance
  , pDerive
  , pPipe
    -- Identifiers
  , pName
  , pNameStr
  , pUpperName
  , pLowerName
    -- Literals
  , pNumber
  , pFloat
  , pByte
  , pShort
  , pInt
  , pHexNumber
  , pHexByte
  , pHexShort
  , pHexInt
  , pStringLiteral
  , pCharLiteral
  , pBoolTrue
  , pBoolFalse
  , pJSBlock
    -- Operators and symbols
  , pEq
  , pMutateEq
  , pDoubleColon
  , pColon
  , pComma
  , pDot
  , pSpread
  , pLeftParen
  , pRightParen
  , pLeftCurly
  , pRightCurly
  , pLeftDoubleCurly
  , pLeftSquareBracket
  , pRightSquareBracket
  , pTupleStart
  , pLeftArrow
  , pRightArrow
  , pFatArrow
  , pPipeOp
  , pPipeChar
  , pSemicolon
  , pSharp
  , pDollar
  , pQuestionMark
  , pTypedHole
  -- Arithmetic
  , pPlus
  , pDoublePlus
  , pDash
  , pDashUnary
  , pStar
  , pSlash
  , pPercent
  -- Comparison
  , pDoubleEq
  , pNotEq
  , pLeftChevron
  , pRightChevron
  , pLeftChevronEq
  , pRightChevronEq
  -- Logical
  , pDoubleAmpersand
  , pDoublePipe
  , pExclamationMark
  -- Bitwise
  , pAmpersand
  , pXor
  , pTilde
  , pDoubleLeftChevron
  , pDoubleRightChevron
  , pTripleRightChevron
  -- Misc
  , pAlternativeOp
  , pNewline
  , keywords
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import qualified Data.Set                       as Set
import           Data.Char                      ( isAlphaNum, isAlpha, isDigit, isHexDigit, isUpper )
import           Data.Void
import           Control.Monad                  ( void, when )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec.Char           as C
import qualified Text.Megaparsec.Char.Lexer     as L

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Escape


-- | Set of keywords that cannot be used as identifiers
keywords :: [Text]
keywords =
  [ "if", "else", "while", "where", "do", "return", "pipe"
  , "import", "export", "from", "type", "alias", "extern"
  , "interface", "instance", "derive", "true", "false"
  ]

-- | Fast O(log n) keyword lookup set using Text directly (avoids T.unpack allocation)
keywordSet :: Set.Set Text
keywordSet = Set.fromList keywords


-- | Parse a keyword (must not be followed by alphanumeric or underscore)
pKeyword :: Text -> Parser Text
pKeyword kw = lexeme $ try (C.string kw <* notFollowedBy (C.alphaNumChar <|> C.char '_' <|> C.char '\''))


-- Keywords
pImport, pExport, pFrom, pType, pAlias, pExtern :: Parser Text
pImport    = pKeyword "import"
pExport    = pKeyword "export"
pFrom      = pKeyword "from"
pType      = pKeyword "type"
pAlias     = pKeyword "alias"
pExtern    = pKeyword "extern"

pIf, pElse, pWhile, pWhere, pDo, pReturn :: Parser Text
pIf        = pKeyword "if"
pElse      = pKeyword "else"
pWhile     = pKeyword "while"
pWhere     = pKeyword "where"
pDo        = pKeyword "do"
pReturn    = pKeyword "return"

pInterface, pInstance, pDerive, pPipe :: Parser Text
pInterface = pKeyword "interface"
pInstance  = pKeyword "instance"
pDerive    = pKeyword "derive"
pPipe      = pKeyword "pipe"


-- | Parse an identifier name (starts with alpha or underscore, then alphanumeric, underscore, or ')
pNameStr :: Parser String
pNameStr = lexeme $ try $ do
  c <- C.letterChar <|> C.char '_'
  rest <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '\'')
  let nameT = T.cons c rest
  when (nameT `Set.member` keywordSet) $
    fail $ "keyword " ++ T.unpack nameT ++ " cannot be used as an identifier"
  return $! T.unpack nameT


-- | Parse an identifier name as Text
pName :: Parser Text
pName = lexeme $ try $ do
  c <- C.letterChar <|> C.char '_'
  rest <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '\'')
  let nameT = T.cons c rest
  when (nameT `Set.member` keywordSet) $
    fail $ "keyword " ++ T.unpack nameT ++ " cannot be used as an identifier"
  return nameT


-- | Parse a name that starts with an uppercase letter
pUpperName :: Parser String
pUpperName = try $ do
  name <- pNameStr
  if not (null name) && isUpper (head name)
    then return name
    else fail "expected uppercase identifier"


-- | Parse a name that starts with a lowercase letter or underscore
pLowerName :: Parser String
pLowerName = try $ do
  name <- pNameStr
  if not (null name) && (isAlpha (head name) && not (isUpper (head name)) || head name == '_')
    then return name
    else fail "expected lowercase identifier"


-- Numeric literals --

-- | Parse a decimal number (sequence of digits)
pDecimal :: Parser String
pDecimal = lexeme $ T.unpack <$> takeWhile1P (Just "digit") isDigit

-- | Parse a number literal (decimal, no suffix)
pNumber :: Parser String
pNumber = lexeme $ try $ do
  digits <- T.unpack <$> takeWhile1P (Just "digit") isDigit
  notFollowedBy (C.char '_' <|> C.char '.')
  return digits

-- | Parse a float literal: either decimal.decimal with optional _f, or decimal_f
pFloat :: Parser String
pFloat = lexeme $ try $ do
  whole <- T.unpack <$> takeWhile1P (Just "digit") isDigit
  choice
    [ do
        dot <- C.char '.'
        frac <- T.unpack <$> takeWhile1P (Just "digit") isDigit
        suffix <- optional (C.string "_f")
        return $ whole ++ [dot] ++ frac ++ maybe "" T.unpack suffix
    , do
        void $ C.string "_f"
        return $ whole ++ "_f"
    ]

-- | Parse a byte literal (decimal_b)
pByte :: Parser String
pByte = lexeme $ try $ do
  digits <- T.unpack <$> takeWhile1P (Just "digit") isDigit
  void $ C.string "_b"
  return digits

-- | Parse a short literal (decimal_s)
pShort :: Parser String
pShort = lexeme $ try $ do
  digits <- T.unpack <$> takeWhile1P (Just "digit") isDigit
  void $ C.string "_s"
  return digits

-- | Parse an int literal (decimal_i)
pInt :: Parser String
pInt = lexeme $ try $ do
  digits <- T.unpack <$> takeWhile1P (Just "digit") isDigit
  void $ C.string "_i"
  return digits

-- | Parse a hex number literal
-- try needed: we consume digits then notFollowedBy '_'; need to backtrack if that fails
pHexNumber :: Parser String
pHexNumber = lexeme $ try $ do
  void $ C.string "0x"
  digits <- T.unpack <$> takeWhile1P (Just "hex digit") isHexDigit
  notFollowedBy (C.char '_')
  return $ "0x" ++ digits

-- | Parse a hex byte literal (0x..._b)
-- try needed: ambiguous with pHexNumber (same prefix)
pHexByte :: Parser String
pHexByte = lexeme $ try $ do
  void $ C.string "0x"
  digits <- T.unpack <$> takeWhile1P (Just "hex digit") isHexDigit
  void $ C.string "_b"
  return $ "0x" ++ digits

-- | Parse a hex short literal (0x..._s)
pHexShort :: Parser String
pHexShort = lexeme $ try $ do
  void $ C.string "0x"
  digits <- T.unpack <$> takeWhile1P (Just "hex digit") isHexDigit
  void $ C.string "_s"
  return $ "0x" ++ digits

-- | Parse a hex int literal (0x..._i)
pHexInt :: Parser String
pHexInt = lexeme $ try $ do
  void $ C.string "0x"
  digits <- T.unpack <$> takeWhile1P (Just "hex digit") isHexDigit
  void $ C.string "_i"
  return $ "0x" ++ digits


-- String and char literals --

-- | Parse a double-quoted string literal
-- Collects raw content (preserving backslash escape sequences as-is),
-- then processes all escape sequences via processEscapes.
pStringLiteral :: Parser String
pStringLiteral = lexeme $ do
  void $ C.char '"'
  raw <- concat <$> many rawStringChunk
  void $ C.char '"'
  case processEscapes raw of
    Left err      -> fail err
    Right content -> return content
  where
    rawStringChunk :: Parser String
    rawStringChunk =
      try (do
        void $ C.char '\\'
        c <- anySingle
        case c of
          'u' -> do
            rest <- try (do
                void $ C.char '{'
                hex <- T.unpack <$> takeWhileP (Just "hex digit") isHexDigit
                void $ C.char '}'
                return $ '{' : hex ++ "}")
              <|> (count 4 C.hexDigitChar)
            return $ '\\' : 'u' : rest
          'x' -> do
            hex <- count 2 C.hexDigitChar
            return $ '\\' : 'x' : hex
          _ -> return ['\\', c])
      <|> T.unpack <$> takeWhile1P Nothing (\x -> x /= '"' && x /= '\\')


-- | Parse a character literal
-- Returns the raw string (including escape sequences like "\\n") for the canonicalizer
pCharLiteral :: Parser String
pCharLiteral = lexeme $ do
  void $ C.char '\''
  content <- charLiteralContent
  void $ C.char '\''
  return content
  where
    charLiteralContent :: Parser String
    charLiteralContent = concat <$> many charLiteralChunk
    -- Returns one or two characters: escape sequences return backslash + char
    charLiteralChunk :: Parser String
    charLiteralChunk =
      (\c -> ['\\', c]) <$> (C.char '\\' *> anySingle)
      <|> (:[]) <$> satisfy (\c -> c /= '\'' && c /= '\\')


-- | Parse boolean true
pBoolTrue :: Parser Text
pBoolTrue = pKeyword "true"


-- | Parse boolean false
pBoolFalse :: Parser Text
pBoolFalse = pKeyword "false"


-- | Parse a JS block (#- ... -#)
pJSBlock :: Parser String
pJSBlock = lexeme $ do
  void $ C.string "#-"
  manyTill anySingle (C.string "-#")


-- Operators and symbols --

-- | Parse = (not followed by > or =)
pEq :: Parser ()
pEq = void $ lexeme $ try $ C.char '=' <* notFollowedBy (oneOf ['>', '='])

-- | Parse :=
-- C.string auto-backtracks; no try needed
pMutateEq :: Parser ()
pMutateEq = void $ lexeme $ C.string ":="

-- | Parse ::
-- try needed: "::" consumes ":" before notFollowedBy can check the next ":"
pDoubleColon :: Parser ()
pDoubleColon = void $ lexeme $ try $ C.string "::" <* notFollowedBy (C.char ':')

-- | Parse : (not followed by : or =)
pColon :: Parser ()
pColon = void $ lexeme $ try $ C.char ':' <* notFollowedBy (oneOf [':', '='])

-- | Parse ,
pComma :: Parser ()
pComma = void $ lexeme $ C.char ','

-- | Parse ... (spread operator)
-- C.string auto-backtracks
pSpread :: Parser ()
pSpread = void $ lexeme $ C.string "..."

-- | Parse . (not followed by another .)
pDot :: Parser ()
pDot = void $ lexeme $ try $ C.char '.' <* notFollowedBy (C.char '.')

-- | Parse (
pLeftParen :: Parser ()
pLeftParen = void $ lexeme $ C.char '('

-- | Parse )
pRightParen :: Parser ()
pRightParen = void $ lexeme $ C.char ')'

-- | Parse { (single)
pLeftCurly :: Parser ()
pLeftCurly = void $ lexeme $ try $ C.char '{' <* notFollowedBy (C.char '{')

-- | Parse }
pRightCurly :: Parser ()
pRightCurly = void $ lexeme $ C.char '}'

-- | Parse {{
-- C.string auto-backtracks
pLeftDoubleCurly :: Parser ()
pLeftDoubleCurly = void $ lexeme $ C.string "{{"

-- | Parse [
pLeftSquareBracket :: Parser ()
pLeftSquareBracket = void $ lexeme $ C.char '['

-- | Parse ]
pRightSquareBracket :: Parser ()
pRightSquareBracket = void $ lexeme $ C.char ']'

-- | Parse #[
-- C.string auto-backtracks
pTupleStart :: Parser ()
pTupleStart = void $ lexeme $ C.string "#["

-- | Parse <-
-- C.string auto-backtracks
pLeftArrow :: Parser ()
pLeftArrow = void $ lexeme $ C.string "<-"

-- | Parse ->
-- C.string auto-backtracks
pRightArrow :: Parser ()
pRightArrow = void $ lexeme $ C.string "->"

-- | Parse =>
-- C.string auto-backtracks
pFatArrow :: Parser ()
pFatArrow = void $ lexeme $ C.string "=>"

-- | Parse |> (pipe operator, not followed by >)
-- try needed: "|>" consumes before notFollowedBy check
pPipeOp :: Parser ()
pPipeOp = void $ lexeme $ try $ C.string "|>" <* notFollowedBy (C.char '>')

-- | Parse | (not followed by > or |)
pPipeChar :: Parser ()
pPipeChar = void $ lexeme $ try $ C.char '|' <* notFollowedBy (oneOf ['>', '|'])

-- | Parse ;
pSemicolon :: Parser ()
pSemicolon = void $ lexeme $ C.char ';'

-- | Parse #
pSharp :: Parser ()
pSharp = void $ lexeme $ try $ C.char '#' <* notFollowedBy (oneOf ['-', '['])

-- | Parse $
pDollar :: Parser ()
pDollar = void $ lexeme $ try $ C.char '$' <* notFollowedBy (C.char '{')

-- | Parse ?
pQuestionMark :: Parser ()
pQuestionMark = void $ lexeme $ try $ C.char '?' <* notFollowedBy (C.char '?')

-- | Parse ???
-- C.string auto-backtracks
pTypedHole :: Parser ()
pTypedHole = void $ lexeme $ C.string "???"


-- Arithmetic operators --

-- | Parse + (not followed by +)
pPlus :: Parser ()
pPlus = void $ lexeme $ try $ C.char '+' <* notFollowedBy (C.char '+')

-- | Parse ++
-- C.string auto-backtracks
pDoublePlus :: Parser ()
pDoublePlus = void $ lexeme $ C.string "++"

-- | Parse - as a binary operator (not followed by >)
pDash :: Parser ()
pDash = void $ lexeme $ try $ C.char '-' <* notFollowedBy (C.char '>')

-- | Parse unary minus
pDashUnary :: Parser ()
pDashUnary = void $ lexeme $ try $ C.char '-' <* notFollowedBy (C.char '>')

-- | Parse *
pStar :: Parser ()
pStar = void $ lexeme $ C.char '*'

-- | Parse /
pSlash :: Parser ()
pSlash = void $ lexeme $ try $ C.char '/' <* notFollowedBy (oneOf ['/', '*', '>'])

-- | Parse %
pPercent :: Parser ()
pPercent = void $ lexeme $ C.char '%'


-- Comparison operators --

-- | Parse ==
-- C.string auto-backtracks
pDoubleEq :: Parser ()
pDoubleEq = void $ lexeme $ C.string "=="

-- | Parse !=
-- C.string auto-backtracks
pNotEq :: Parser ()
pNotEq = void $ lexeme $ C.string "!="

-- | Parse < (not followed by - or < or = or | or /)
pLeftChevron :: Parser ()
pLeftChevron = void $ lexeme $ try $ C.char '<' <* notFollowedBy (oneOf ['-', '<', '=', '|', '/'])

-- | Parse > (not followed by > or =)
pRightChevron :: Parser ()
pRightChevron = void $ lexeme $ try $ C.char '>' <* notFollowedBy (oneOf ['>', '='])

-- | Parse <=
-- C.string auto-backtracks
pLeftChevronEq :: Parser ()
pLeftChevronEq = void $ lexeme $ C.string "<="

-- | Parse >=
-- C.string auto-backtracks
pRightChevronEq :: Parser ()
pRightChevronEq = void $ lexeme $ C.string ">="


-- Logical operators --

-- | Parse &&
-- C.string auto-backtracks
pDoubleAmpersand :: Parser ()
pDoubleAmpersand = void $ lexeme $ C.string "&&"

-- | Parse ||
-- C.string auto-backtracks
pDoublePipe :: Parser ()
pDoublePipe = void $ lexeme $ C.string "||"

-- | Parse ! (not followed by =)
pExclamationMark :: Parser ()
pExclamationMark = void $ lexeme $ try $ C.char '!' <* notFollowedBy (C.char '=')


-- Bitwise operators --

-- | Parse & (not followed by &)
pAmpersand :: Parser ()
pAmpersand = void $ lexeme $ try $ C.char '&' <* notFollowedBy (C.char '&')

-- | Parse ^
pXor :: Parser ()
pXor = void $ lexeme $ C.char '^'

-- | Parse ~
pTilde :: Parser ()
pTilde = void $ lexeme $ C.char '~'

-- | Parse <<
-- C.string auto-backtracks
pDoubleLeftChevron :: Parser ()
pDoubleLeftChevron = void $ lexeme $ C.string "<<"

-- | Parse >> (not followed by >)
-- try needed: ">>" consumes before notFollowedBy check
pDoubleRightChevron :: Parser ()
pDoubleRightChevron = void $ lexeme $ try $ C.string ">>" <* notFollowedBy (C.char '>')

-- | Parse >>>
-- C.string auto-backtracks
pTripleRightChevron :: Parser ()
pTripleRightChevron = void $ lexeme $ C.string ">>>"


-- | Parse <|> (alternative operator)
-- C.string auto-backtracks
pAlternativeOp :: Parser ()
pAlternativeOp = void $ lexeme $ C.string "<|>"


-- | Parse a newline character
pNewline :: Parser ()
pNewline = void C.newline
