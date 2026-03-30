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
  , pDoubleQuestionMark
  , pQuestionDot
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

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Set                       as Set
import           Data.Word                      ( Word8 )
import           Data.Char                      ( isUpper, isAlpha )
import           Control.Monad                  ( void, when )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec.Byte          as C

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Escape


-- | Set of keywords that cannot be used as identifiers
keywords :: [BS.ByteString]
keywords =
  [ "if", "else", "while", "where", "do", "return", "pipe"
  , "import", "export", "from", "type", "alias", "extern"
  , "interface", "instance", "derive", "true", "false"
  ]

-- | Fast O(log n) keyword lookup set
keywordSet :: Set.Set BS.ByteString
keywordSet = Set.fromList keywords


-- | Parse a keyword (must not be followed by alphanumeric or underscore)
-- try is needed: C.string consumes before notFollowedBy can check
{-# INLINE pKeyword #-}
pKeyword :: BS.ByteString -> Parser BS.ByteString
pKeyword kw = lexeme $ try (C.string kw <* notFollowedBy (satisfy isIdentB))


-- Keywords
pImport, pExport, pFrom, pType, pAlias, pExtern :: Parser BS.ByteString
pImport    = pKeyword "import"
pExport    = pKeyword "export"
pFrom      = pKeyword "from"
pType      = pKeyword "type"
pAlias     = pKeyword "alias"
pExtern    = pKeyword "extern"

pIf, pElse, pWhile, pWhere, pDo, pReturn :: Parser BS.ByteString
pIf        = pKeyword "if"
pElse      = pKeyword "else"
pWhile     = pKeyword "while"
pWhere     = pKeyword "where"
pDo        = pKeyword "do"
pReturn    = pKeyword "return"

pInterface, pInstance, pDerive, pPipe :: Parser BS.ByteString
pInterface = pKeyword "interface"
pInstance  = pKeyword "instance"
pDerive    = pKeyword "derive"
pPipe      = pKeyword "pipe"


-- | Parse an identifier name (starts with alpha or underscore, then alphanumeric, underscore, or ')
-- try is needed: can fail at keyword-check after consuming identifier chars.
-- Fast path: only bytes that start a keyword need the Set.member check.
-- Keyword first bytes: i e w d r p f t a (all lowercase, no uppercase, no '_')
{-# INLINE pNameStr #-}
pNameStr :: Parser String
pNameStr = lexeme $ try $ do
  b <- satisfy (\b -> isAlphaB b || b == 95)  -- letter or _
  rest <- takeWhileP Nothing isIdentB
  let nameBS = BS.cons b rest
  -- Only do keyword check if first byte is a known keyword-starting byte
  -- and the identifier is short enough to be a keyword (max 9 chars: "interface")
  when (BS.length nameBS <= 9 && mightBeKeyword b) $
    when (nameBS `Set.member` keywordSet) $
      fail $ "keyword " ++ C8.unpack nameBS ++ " cannot be used as an identifier"
  return $! C8.unpack nameBS
  where
    -- First bytes of all keywords (lowercase only; '_' and uppercase can never be keywords)
    {-# INLINE mightBeKeyword #-}
    mightBeKeyword :: Word8 -> Bool
    mightBeKeyword w = w == 105  -- 'i': if, import, instance, interface
                    || w == 101  -- 'e': else, export, extern
                    || w == 119  -- 'w': while, where
                    || w == 100  -- 'd': do, derive
                    || w == 114  -- 'r': return
                    || w == 112  -- 'p': pipe
                    || w == 102  -- 'f': from, false
                    || w == 116  -- 't': type, true
                    || w == 97   -- 'a': alias


-- | Parse an identifier name as String
{-# INLINE pName #-}
pName :: Parser String
pName = pNameStr


-- | Parse a name that starts with an uppercase letter.
-- No try needed: once satisfy isUpperB succeeds, we can never fail.
{-# INLINE pUpperName #-}
pUpperName :: Parser String
pUpperName = lexeme $ do
  b <- satisfy isUpperB
  rest <- takeWhileP Nothing isIdentB
  return $! C8.unpack (BS.cons b rest)


-- | Parse a name that starts with a lowercase letter or underscore
-- try is needed: can fail at keyword-check after consuming identifier chars
{-# INLINE pLowerName #-}
pLowerName :: Parser String
pLowerName = lexeme $ try $ do
  b <- satisfy (\b -> isLowerB b || b == 95)  -- lowercase or _
  rest <- takeWhileP Nothing isIdentB
  let nameBS = BS.cons b rest
  when (nameBS `Set.member` keywordSet) $
    fail $ "keyword " ++ C8.unpack nameBS ++ " cannot be used as an identifier"
  return $! C8.unpack nameBS


-- Numeric literals --

-- | Parse a number literal (decimal, no suffix)
-- try needed: consumes digits before notFollowedBy check
pNumber :: Parser String
pNumber = lexeme $ try $ do
  digits <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
  notFollowedBy (satisfy (\b -> b == 95 || b == 46))  -- _ or .
  return digits

-- | Parse a float literal: either decimal.decimal with optional _f, or decimal_f
-- try needed: both branches consume digits before diverging
pFloat :: Parser String
pFloat = lexeme $ try $ do
  whole <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
  choice
    [ do
        void $ C.char 46  -- '.'
        frac <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
        suffix <- optional (C.string "_f")
        return $ whole ++ "." ++ frac ++ maybe "" C8.unpack suffix
    , do
        void $ C.string "_f"
        return $ whole ++ "_f"
    ]

-- | Parse a byte literal (decimal_b)
-- try needed: consumes digits before suffix check
pByte :: Parser String
pByte = lexeme $ try $ do
  digits <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
  void $ C.string "_b"
  return digits

-- | Parse a short literal (decimal_s)
-- try needed: consumes digits before suffix check
pShort :: Parser String
pShort = lexeme $ try $ do
  digits <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
  void $ C.string "_s"
  return digits

-- | Parse an int literal (decimal_i)
-- try needed: consumes digits before suffix check
pInt :: Parser String
pInt = lexeme $ try $ do
  digits <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
  void $ C.string "_i"
  return digits

-- | Parse a hex number literal
-- try needed: we consume digits then notFollowedBy '_'; need to backtrack if that fails
pHexNumber :: Parser String
pHexNumber = lexeme $ try $ do
  void $ C.string "0x"
  digits <- C8.unpack <$> takeWhile1P (Just "hex digit") isHexDigitB
  notFollowedBy (C.char 95)  -- '_'
  return $ "0x" ++ digits

-- | Parse a hex byte literal (0x..._b)
-- try needed: ambiguous with pHexNumber (same prefix)
pHexByte :: Parser String
pHexByte = lexeme $ try $ do
  void $ C.string "0x"
  digits <- C8.unpack <$> takeWhile1P (Just "hex digit") isHexDigitB
  void $ C.string "_b"
  return $ "0x" ++ digits

-- | Parse a hex short literal (0x..._s)
pHexShort :: Parser String
pHexShort = lexeme $ try $ do
  void $ C.string "0x"
  digits <- C8.unpack <$> takeWhile1P (Just "hex digit") isHexDigitB
  void $ C.string "_s"
  return $ "0x" ++ digits

-- | Parse a hex int literal (0x..._i)
pHexInt :: Parser String
pHexInt = lexeme $ try $ do
  void $ C.string "0x"
  digits <- C8.unpack <$> takeWhile1P (Just "hex digit") isHexDigitB
  void $ C.string "_i"
  return $ "0x" ++ digits


-- String and char literals --

-- | Parse a double-quoted string literal
-- Collects raw content (preserving backslash escape sequences as-is),
-- then processes all escape sequences via processEscapes.
pStringLiteral :: Parser String
pStringLiteral = lexeme $ do
  void $ C.char 34  -- '"'
  raw <- concat <$> many rawStringChunk
  void $ C.char 34  -- '"'
  case processEscapes raw of
    Left err      -> fail err
    Right content -> return content
  where
    rawStringChunk :: Parser String
    rawStringChunk =
      try (do
        void $ C.char 92  -- '\\'
        b <- anySingle
        case chr8' b of
          'u' -> do
            rest <- try (do
                void $ C.char 123  -- '{'
                hex <- C8.unpack <$> takeWhileP (Just "hex digit") isHexDigitB
                void $ C.char 125  -- '}'
                return $ '{' : hex ++ "}")
              <|> (map chr8' <$> count 4 (satisfy isHexDigitB))
            return $ '\\' : 'u' : rest
          'x' -> do
            hex <- map chr8' <$> count 2 (satisfy isHexDigitB)
            return $ '\\' : 'x' : hex
          c -> return ['\\', c])
      <|> (T.unpack . TE.decodeUtf8 <$> takeWhile1P Nothing (\b -> b /= 34 && b /= 92))  -- not " or \


-- | Parse a character literal
-- Returns the raw string (including escape sequences like "\\n") for the canonicalizer
pCharLiteral :: Parser String
pCharLiteral = lexeme $ do
  void $ C.char 39  -- '\''
  content <- charLiteralContent
  void $ C.char 39  -- '\''
  return content
  where
    charLiteralContent :: Parser String
    charLiteralContent = concat <$> many charLiteralChunk
    -- Returns one or two characters: escape sequences return backslash + char
    charLiteralChunk :: Parser String
    charLiteralChunk =
      (\b -> ['\\', chr8' b]) <$> (C.char 92 *> anySingle)  -- '\\'
      <|> (T.unpack . TE.decodeUtf8 <$> takeWhile1P Nothing (\b -> b /= 39 && b /= 92))  -- not ' or \


-- | Parse boolean true
pBoolTrue :: Parser BS.ByteString
pBoolTrue = pKeyword "true"


-- | Parse boolean false
pBoolFalse :: Parser BS.ByteString
pBoolFalse = pKeyword "false"


-- | Parse a JS block (#- ... -#)
pJSBlock :: Parser String
pJSBlock = lexeme $ do
  void $ C.string "#-"
  bs <- manyTill anySingle (C.string "-#")
  return $! T.unpack $ TE.decodeUtf8 $ BS.pack bs


-- Operators and symbols --

-- | Parse = (not followed by > or =)
pEq :: Parser ()
pEq = void $ lexeme $ try $ C.char 61 <* notFollowedBy (oneOf [62, 61 :: Word8])  -- '=' not followed by '>' or '='

-- | Parse :=
-- C.string auto-backtracks; no try needed
pMutateEq :: Parser ()
pMutateEq = void $ lexeme $ C.string ":="

-- | Parse ::
-- try needed: "::" consumes ":" before notFollowedBy can check the next ":"
pDoubleColon :: Parser ()
pDoubleColon = void $ lexeme $ try $ C.string "::" <* notFollowedBy (C.char 58)  -- ':'

-- | Parse : (not followed by : or =)
pColon :: Parser ()
pColon = void $ lexeme $ try $ C.char 58 <* notFollowedBy (oneOf [58, 61 :: Word8])  -- ':' not ':' or '='

-- | Parse ,
pComma :: Parser ()
pComma = void $ lexeme $ C.char 44  -- ','

-- | Parse ... (spread operator)
-- C.string auto-backtracks
pSpread :: Parser ()
pSpread = void $ lexeme $ C.string "..."

-- | Parse . (not followed by another .)
pDot :: Parser ()
pDot = void $ lexeme $ try $ C.char 46 <* notFollowedBy (C.char 46)  -- '.'

-- | Parse (
pLeftParen :: Parser ()
pLeftParen = void $ lexeme $ C.char 40  -- '('

-- | Parse )
pRightParen :: Parser ()
pRightParen = void $ lexeme $ C.char 41  -- ')'

-- | Parse { (single)
pLeftCurly :: Parser ()
pLeftCurly = void $ lexeme $ try $ C.char 123 <* notFollowedBy (C.char 123)  -- '{' not '{{'

-- | Parse }
pRightCurly :: Parser ()
pRightCurly = void $ lexeme $ C.char 125  -- '}'

-- | Parse {{
-- C.string auto-backtracks
pLeftDoubleCurly :: Parser ()
pLeftDoubleCurly = void $ lexeme $ C.string "{{"

-- | Parse [
pLeftSquareBracket :: Parser ()
pLeftSquareBracket = void $ lexeme $ C.char 91  -- '['

-- | Parse ]
pRightSquareBracket :: Parser ()
pRightSquareBracket = void $ lexeme $ C.char 93  -- ']'

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
pPipeOp = void $ lexeme $ try $ C.string "|>" <* notFollowedBy (C.char 62)  -- '>'

-- | Parse | (not followed by > or |)
pPipeChar :: Parser ()
pPipeChar = void $ lexeme $ try $ C.char 124 <* notFollowedBy (oneOf [62, 124 :: Word8])  -- '|' not '>' or '|'

-- | Parse ;
pSemicolon :: Parser ()
pSemicolon = void $ lexeme $ C.char 59  -- ';'

-- | Parse #
pSharp :: Parser ()
pSharp = void $ lexeme $ try $ C.char 35 <* notFollowedBy (oneOf [45, 91 :: Word8])  -- '#' not '-' or '['

-- | Parse $
pDollar :: Parser ()
pDollar = void $ lexeme $ try $ C.char 36 <* notFollowedBy (C.char 123)  -- '$' not '{'

-- | Parse ?
pQuestionMark :: Parser ()
pQuestionMark = void $ lexeme $ try $ C.char 63 <* notFollowedBy (C.char 63)  -- '?' not '?'

-- | Parse ?? (not followed by ?)
pDoubleQuestionMark :: Parser ()
pDoubleQuestionMark = void $ lexeme $ try $ C.string "??" <* notFollowedBy (C.char 63)

-- | Parse ?.
pQuestionDot :: Parser ()
pQuestionDot = void $ lexeme $ C.string "?."

-- | Parse ???
-- C.string auto-backtracks
pTypedHole :: Parser ()
pTypedHole = void $ lexeme $ C.string "???"


-- Arithmetic operators --

-- | Parse + (not followed by +)
pPlus :: Parser ()
pPlus = void $ lexeme $ try $ C.char 43 <* notFollowedBy (C.char 43)  -- '+' not '+'

-- | Parse ++
-- C.string auto-backtracks
pDoublePlus :: Parser ()
pDoublePlus = void $ lexeme $ C.string "++"

-- | Parse - as a binary operator (not followed by >)
pDash :: Parser ()
pDash = void $ lexeme $ try $ C.char 45 <* notFollowedBy (C.char 62)  -- '-' not '>'

-- | Parse unary minus
pDashUnary :: Parser ()
pDashUnary = void $ lexeme $ try $ C.char 45 <* notFollowedBy (C.char 62)  -- '-' not '>'

-- | Parse *
pStar :: Parser ()
pStar = void $ lexeme $ C.char 42  -- '*'

-- | Parse /
pSlash :: Parser ()
pSlash = void $ lexeme $ try $ C.char 47 <* notFollowedBy (oneOf [47, 42, 62 :: Word8])  -- '/' not '/' '*' '>'

-- | Parse %
pPercent :: Parser ()
pPercent = void $ lexeme $ C.char 37  -- '%'


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
pLeftChevron = void $ lexeme $ try $ C.char 60 <* notFollowedBy (oneOf [45, 60, 61, 124, 47 :: Word8])  -- '<'

-- | Parse > (not followed by > or =)
pRightChevron :: Parser ()
pRightChevron = void $ lexeme $ try $ C.char 62 <* notFollowedBy (oneOf [62, 61 :: Word8])  -- '>'

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
pExclamationMark = void $ lexeme $ try $ C.char 33 <* notFollowedBy (C.char 61)  -- '!' not '='


-- Bitwise operators --

-- | Parse & (not followed by &)
pAmpersand :: Parser ()
pAmpersand = void $ lexeme $ try $ C.char 38 <* notFollowedBy (C.char 38)  -- '&' not '&'

-- | Parse ^
pXor :: Parser ()
pXor = void $ lexeme $ C.char 94  -- '^'

-- | Parse ~
pTilde :: Parser ()
pTilde = void $ lexeme $ C.char 126  -- '~'

-- | Parse <<
-- C.string auto-backtracks
pDoubleLeftChevron :: Parser ()
pDoubleLeftChevron = void $ lexeme $ C.string "<<"

-- | Parse >> (not followed by >)
-- try needed: ">>" consumes before notFollowedBy check
pDoubleRightChevron :: Parser ()
pDoubleRightChevron = void $ lexeme $ try $ C.string ">>" <* notFollowedBy (C.char 62)  -- '>'

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
