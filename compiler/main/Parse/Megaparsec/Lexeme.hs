{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
-- | Token-based lexeme parsers for Madlib.
-- Each combinator matches a specific Token constructor in the RangedToken stream.
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
import qualified Data.Set                       as Set
import           Control.Monad                  ( void )
import           Data.List.NonEmpty             ( NonEmpty(..) )

import           Text.Megaparsec                hiding ( State, Token )

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Escape        ( processHexaEscapes )
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )


-- | Set of keywords (kept for any downstream code that checks it)
keywords :: [BS.ByteString]
keywords =
  [ "if", "else", "while", "where", "do", "return", "pipe"
  , "import", "export", "from", "type", "alias", "extern"
  , "interface", "instance", "derive", "true", "false"
  ]


-- ── Low-level primitive ───────────────────────────────────────────────────────

-- | Consume the next token if it matches the given Token constructor.
-- Updates psLastTokenEnd.
{-# INLINE tok #-}
tok :: Token -> Parser ()
tok t = void $ satisfyTok (\rt -> rtToken rt == t)

-- | Consume the next token if the predicate holds, returning the matched RangedToken.
{-# INLINE tokWith #-}
tokWith :: (Token -> Maybe a) -> Parser a
tokWith f = do
  rt <- satisfyTok (\rt -> case f (rtToken rt) of { Just _ -> True; Nothing -> False })
  case f (rtToken rt) of
    Just a  -> return a
    Nothing -> unexpected (Tokens (rt :| []))


-- ── Keywords ─────────────────────────────────────────────────────────────────

-- | Parse a keyword by name (kept for generic callers)
{-# INLINE pKeyword #-}
pKeyword :: BS.ByteString -> Parser BS.ByteString
pKeyword kw = case kw of
  "import"    -> BS.pack [105,109,112,111,114,116] <$ tok TkImport
  "export"    -> BS.pack [101,120,112,111,114,116] <$ tok TkExport
  "from"      -> BS.pack [102,114,111,109]         <$ tok TkFrom
  "type"      -> BS.pack [116,121,112,101]         <$ tok TkType
  "alias"     -> BS.pack [97,108,105,97,115]       <$ tok TkAlias
  "extern"    -> BS.pack [101,120,116,101,114,110] <$ tok TkExtern
  "if"        -> BS.pack [105,102]                 <$ tok TkIf
  "else"      -> BS.pack [101,108,115,101]         <$ tok TkElse
  "while"     -> BS.pack [119,104,105,108,101]     <$ tok TkWhile
  "where"     -> BS.pack [119,104,101,114,101]     <$ tok TkWhere
  "do"        -> BS.pack [100,111]                 <$ tok TkDo
  "return"    -> BS.pack [114,101,116,117,114,110] <$ tok TkReturn
  "pipe"      -> BS.pack [112,105,112,101]         <$ tok TkPipe
  "interface" -> BS.pack [105,110,116,101,114,102,97,99,101] <$ tok TkInterface
  "instance"  -> BS.pack [105,110,115,116,97,110,99,101] <$ tok TkInstance
  "derive"    -> BS.pack [100,101,114,105,118,101] <$ tok TkDerive
  "true"      -> BS.pack [116,114,117,101]         <$ tok TkTrue
  "false"     -> BS.pack [102,97,108,115,101]      <$ tok TkFalse
  _           -> fail $ "unknown keyword: " ++ show kw

pImport, pExport, pFrom, pType, pAlias, pExtern :: Parser BS.ByteString
pImport    = kw "import"    <$ tok TkImport
pExport    = kw "export"    <$ tok TkExport
pFrom      = kw "from"      <$ tok TkFrom
pType      = kw "type"      <$ tok TkType
pAlias     = kw "alias"     <$ tok TkAlias
pExtern    = kw "extern"    <$ tok TkExtern

pIf, pElse, pWhile, pWhere, pDo, pReturn :: Parser BS.ByteString
pIf        = kw "if"        <$ tok TkIf
pElse      = kw "else"      <$ tok TkElse
pWhile     = kw "while"     <$ tok TkWhile
pWhere     = kw "where"     <$ tok TkWhere
pDo        = kw "do"        <$ tok TkDo
pReturn    = kw "return"    <$ tok TkReturn

pInterface, pInstance, pDerive, pPipe :: Parser BS.ByteString
pInterface = kw "interface" <$ tok TkInterface
pInstance  = kw "instance"  <$ tok TkInstance
pDerive    = kw "derive"    <$ tok TkDerive
pPipe      = kw "pipe"      <$ tok TkPipe

-- Helper: convert String keyword to ByteString
{-# INLINE kw #-}
kw :: String -> BS.ByteString
kw = BS.pack . map (fromIntegral . fromEnum)


-- ── Identifiers ───────────────────────────────────────────────────────────────

-- | Parse any identifier (lower or upper case).
-- Also accepts "soft" keywords that are used as function names in practice
-- (when, is, not) since no parser consumes those keyword tokens.
{-# INLINE pName #-}
pName :: Parser String
pName = tokWith $ \t -> case t of
  TkName s     -> Just s
  TkTypeName s -> Just s
  TkWhen       -> Just "when"
  TkIs         -> Just "is"
  TkNot        -> Just "not"
  _            -> Nothing

-- | Parse a lowercase identifier (including soft keywords used as names).
{-# INLINE pNameStr #-}
pNameStr :: Parser String
pNameStr = tokWith $ \t -> case t of
  TkName s -> Just s
  TkWhen   -> Just "when"
  TkIs     -> Just "is"
  TkNot    -> Just "not"
  _        -> Nothing

-- | Parse an upper-case identifier (type/constructor name)
{-# INLINE pUpperName #-}
pUpperName :: Parser String
pUpperName = tokWith $ \t -> case t of
  TkTypeName s -> Just s
  _            -> Nothing

-- | Parse a lower-case identifier
{-# INLINE pLowerName #-}
pLowerName :: Parser String
pLowerName = tokWith $ \t -> case t of
  TkName s -> Just s
  _        -> Nothing


-- ── Numeric literals ─────────────────────────────────────────────────────────

-- | Parse any numeric literal (decimal, no suffix). Returns the string of digits.
pNumber :: Parser String
pNumber = tokWith $ \t -> case t of
  TkInt s -> Just s
  _       -> Nothing

pFloat :: Parser String
pFloat = tokWith $ \t -> case t of
  TkFloat s -> Just s
  _         -> Nothing

pByte :: Parser String
pByte = tokWith $ \t -> case t of
  TkByte s -> Just s
  _        -> Nothing

pShort :: Parser String
pShort = tokWith $ \t -> case t of
  TkShort s -> Just s
  _         -> Nothing

pInt :: Parser String
pInt = tokWith $ \t -> case t of
  TkInt s -> Just s   -- TkInt is used for "decimal" int literals
  _       -> Nothing

pHexNumber :: Parser String
pHexNumber = tokWith $ \t -> case t of
  TkHexNumber s -> Just s
  _             -> Nothing

pHexByte :: Parser String
pHexByte = tokWith $ \t -> case t of
  TkHexByte s -> Just s
  _           -> Nothing

pHexShort :: Parser String
pHexShort = tokWith $ \t -> case t of
  TkHexShort s -> Just s
  _            -> Nothing

pHexInt :: Parser String
pHexInt = tokWith $ \t -> case t of
  TkHexInt s -> Just s
  _          -> Nothing


-- ── String / char literals ────────────────────────────────────────────────────

pStringLiteral :: Parser String
pStringLiteral = do
  isFormatter <- getFormatterMode
  s <- tokWith $ \t -> case t of
    TkString s' -> Just s'
    _           -> Nothing
  case processHexaEscapes isFormatter s of
    Left err  -> fail err
    Right str -> return str

-- NOTE: char literals are returned as-is (raw escape sequences preserved),
-- because Canonicalize.Canonicalize processes char escapes via charParser.
pCharLiteral :: Parser String
pCharLiteral = tokWith $ \t -> case t of
  TkChar s -> Just s
  _        -> Nothing

pBoolTrue :: Parser BS.ByteString
pBoolTrue = kw "true" <$ tok TkTrue

pBoolFalse :: Parser BS.ByteString
pBoolFalse = kw "false" <$ tok TkFalse

pJSBlock :: Parser String
pJSBlock = tokWith $ \t -> case t of
  TkJSBlock s -> Just s
  _           -> Nothing


-- ── Operators and symbols ─────────────────────────────────────────────────────

pEq               :: Parser (); pEq               = tok TkEq
pMutateEq         :: Parser (); pMutateEq         = tok TkMutateEq
pDoubleColon      :: Parser (); pDoubleColon      = tok TkDoubleColon
pColon            :: Parser (); pColon            = tok TkColon
pComma            :: Parser (); pComma            = tok TkComma
pSpread           :: Parser (); pSpread           = tok TkSpread
pDot              :: Parser (); pDot              = tok TkDot
pLeftParen        :: Parser (); pLeftParen        = tok TkLeftParen
pRightParen       :: Parser (); pRightParen       = tok TkRightParen
pLeftCurly        :: Parser (); pLeftCurly        = tok TkLeftCurly
pRightCurly       :: Parser (); pRightCurly       = tok TkRightCurly
pLeftDoubleCurly  :: Parser (); pLeftDoubleCurly  = tok TkLeftDoubleCurly
pLeftSquareBracket  :: Parser (); pLeftSquareBracket  = tok TkLeftSquare
pRightSquareBracket :: Parser (); pRightSquareBracket = tok TkRightSquare
pTupleStart       :: Parser (); pTupleStart       = tok TkTupleStart
pLeftArrow        :: Parser (); pLeftArrow        = tok TkLeftArrow
pRightArrow       :: Parser (); pRightArrow       = tok TkRightArrow
pFatArrow         :: Parser (); pFatArrow         = tok TkFatArrow
pPipeOp           :: Parser (); pPipeOp           = tok TkPipeOp
pPipeChar         :: Parser (); pPipeChar         = tok TkPipeChar
pSemicolon        :: Parser (); pSemicolon        = tok TkSemicolon
pSharp            :: Parser (); pSharp            = tok TkSharp
pDollar           :: Parser (); pDollar           = tok TkDollar
pQuestionMark     :: Parser (); pQuestionMark     = tok TkQuestionMark
pDoubleQuestionMark :: Parser (); pDoubleQuestionMark = tok TkDoubleQuestionMark
pQuestionDot      :: Parser (); pQuestionDot      = tok TkQuestionDot
pTypedHole        :: Parser (); pTypedHole        = tok TkTypedHole

-- Arithmetic
pPlus             :: Parser (); pPlus             = tok TkPlus
pDoublePlus       :: Parser (); pDoublePlus       = tok TkDoublePlus
pDash             :: Parser (); pDash             = tok TkDash
pDashUnary        :: Parser (); pDashUnary        = tok TkDash  -- same token, context is in parser
pStar             :: Parser (); pStar             = tok TkStar
pSlash            :: Parser (); pSlash            = tok TkSlash
pPercent          :: Parser (); pPercent          = tok TkPercent

-- Comparison
pDoubleEq         :: Parser (); pDoubleEq         = tok TkDoubleEq
pNotEq            :: Parser (); pNotEq            = tok TkNotEq
pLeftChevron      :: Parser (); pLeftChevron      = tok TkLeftChevron
pRightChevron     :: Parser (); pRightChevron     = tok TkRightChevron
pLeftChevronEq    :: Parser (); pLeftChevronEq    = tok TkLeftChevronEq
pRightChevronEq   :: Parser (); pRightChevronEq   = tok TkRightChevronEq

-- Logical
pDoubleAmpersand  :: Parser (); pDoubleAmpersand  = tok TkDoubleAmpersand
pDoublePipe       :: Parser (); pDoublePipe       = tok TkDoublePipe
pExclamationMark  :: Parser (); pExclamationMark  = tok TkExclamation

-- Bitwise
pAmpersand        :: Parser (); pAmpersand        = tok TkAmpersand
pXor              :: Parser (); pXor              = tok TkXor
pTilde            :: Parser (); pTilde            = tok TkTilde
pDoubleLeftChevron  :: Parser (); pDoubleLeftChevron  = tok TkDoubleLeftChevron
pDoubleRightChevron :: Parser (); pDoubleRightChevron = tok TkDoubleRightChevron
pTripleRightChevron :: Parser (); pTripleRightChevron = tok TkTripleRightChevron

-- Misc
pAlternativeOp    :: Parser (); pAlternativeOp    = tok TkAlternativeOp

-- | Parse a TkNewline token
pNewline          :: Parser (); pNewline           = tok TkNewline
