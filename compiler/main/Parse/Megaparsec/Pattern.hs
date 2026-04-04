{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Pattern
  ( pPattern
  ) where

import           Data.Char                      ( isUpper )
import           Control.Monad                  ( void )

import           Text.Megaparsec                hiding ( State, Token )

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Lexeme
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )


-- | Parse a pattern (composite or non-composite)
pPattern :: Parser Src.Pattern
pPattern = do
  mrt <- optional (lookAhead anySingle)
  case fmap rtToken mrt of
    Just (TkTypeName _) -> choice [try pCompositePattern, pNonCompositePattern]
    -- TkName can be module prefix (e.g. __BUILTINS__.Constructor) or wildcard "_"
    Just (TkName _)     -> choice [try pCompositePattern, pNonCompositePattern]
    _                   -> pNonCompositePattern


-- | Parse a composite pattern: Constructor(args) or Module.Constructor(args)
-- Succeeds when:
--   - Module.Constructor[(...)] — dot-qualified, lower/upper module prefix
--   - Constructor(args)         — uppercase constructor with argument list
--   - Constructor               — uppercase nullary constructor (no parens)
pCompositePattern :: Parser Src.Pattern
pCompositePattern = try $ do
  (startArea, name1) <- withArea pName
  target <- pSourceTarget
  mDot <- optional $ try $ do
    pDot
    pName
  case mDot of
    Just name2 -> do
      -- Module.Constructor pattern (module can be any identifier)
      let qualName = name1 ++ "." ++ name2
      args <- optional $ try $ do
        pLeftParen
        rets
        as <- pCompositePatternArgs
        _ <- optional pComma
        rets
        pRightParen
        return as
      case args of
        Just as -> do
          (endArea, _) <- withArea (pure ())
          return $ Src.Source (mergeAreas startArea endArea) target (Src.PCon (Src.Source (mergeAreas startArea endArea) target qualName) as)
        Nothing ->
          return $ Src.Source startArea target (Src.PNullaryCon (Src.Source startArea target qualName))
    Nothing -> do
      -- Plain Constructor pattern (must be uppercase)
      if not (null name1) && isUpper (head name1) then do
        args <- optional $ try $ do
          pLeftParen
          rets
          as <- pCompositePatternArgs
          _ <- optional pComma
          rets
          (endArea, _) <- withArea (void pRightParen)
          return (as, endArea)
        case args of
          Just (as, endArea) ->
            return $ Src.Source (mergeAreas startArea endArea) target (Src.PCon (Src.Source startArea target name1) as)
          Nothing ->
            return $ Src.Source startArea target (Src.PNullaryCon (Src.Source startArea target name1))
      else
        fail "expected uppercase constructor name"


-- | Parse composite pattern args separated by commas
pCompositePatternArgs :: Parser [Src.Pattern]
pCompositePatternArgs = do
  first <- pPattern
  rest <- many $ try $ do
    pComma
    rets
    pPattern
  return $ first : rest


-- | Parse a non-composite pattern (no constructor application)
-- Dispatch on the next token type.
pNonCompositePattern :: Parser Src.Pattern
pNonCompositePattern = do
  mrt <- optional (lookAhead anySingle)
  case fmap rtToken mrt of
    Just TkLeftCurly   -> pRecordPattern
    Just TkLeftSquare  -> pListPattern
    Just TkTupleStart  -> pTuplePattern
    Just TkLeftParen   -> pParenthesizedPattern
    Just TkDash        -> try pNegativeNumericPattern <|> pNamePattern
    Just (TkString _)  -> pStrPattern
    Just (TkChar _)    -> pCharPattern
    -- Numeric tokens
    Just (TkInt _)     -> pNumericPattern
    Just (TkFloat _)   -> pNumericPattern
    Just (TkByte _)    -> pNumericPattern
    Just (TkShort _)   -> pNumericPattern
    Just (TkHexNumber _) -> pNumericPattern
    Just (TkHexByte _)   -> pNumericPattern
    Just (TkHexShort _)  -> pNumericPattern
    Just (TkHexInt _)    -> pNumericPattern
    -- Boolean / name
    Just TkTrue        -> pBoolPattern
    Just TkFalse       -> pBoolPattern
    _                  -> choice [pBoolPattern, pNamePattern]


-- | Parse a numeric literal pattern (int, float, hex, byte, short, etc.)
pNumericPattern :: Parser Src.Pattern
pNumericPattern = do
  (area, (target, node)) <- withArea $ do
    t <- pSourceTarget
    n <- parseNum
    return (t, n)
  return $ Src.Source area target node
  where
    parseNum :: Parser Src.Pattern_
    parseNum = choice
      [ Src.PNum  <$> pHexByte
      , Src.PNum  <$> pHexShort
      , Src.PNum  <$> pHexInt
      , Src.PNum  <$> pHexNumber
      , Src.PNum  <$> pByte
      , Src.PNum  <$> pShort
      , Src.PFloat <$> pFloat
      , Src.PNum  <$> pNumber
      ]


-- | Parse a negative numeric pattern (-number or -float)
pNegativeNumericPattern :: Parser Src.Pattern
pNegativeNumericPattern = do
  (area, (target, node)) <- withArea $ do
    t <- pSourceTarget
    pDash
    n <- parseNeg
    return (t, n)
  return $ Src.Source area target node
  where
    parseNeg :: Parser Src.Pattern_
    parseNeg = choice
      [ (\s -> Src.PFloat ('-' : s)) <$> pFloat
      , (\s -> Src.PNum  ('-' : s)) <$> pNumber
      ]


-- | Parse a name/variable/wildcard/nullary constructor pattern
pNamePattern :: Parser Src.Pattern
pNamePattern = do
  (area, name) <- withArea pName
  target <- pSourceTarget
  return $ nameToPattern area target name


-- | Convert a name to the appropriate pattern type
nameToPattern :: Area -> Src.SourceTarget -> String -> Src.Pattern
nameToPattern area target n
  | n == "_"           = Src.Source area target Src.PAny
  | isUpper (head n)   = Src.Source area target (Src.PNullaryCon (Src.Source area target n))
  | otherwise          = Src.Source area target (Src.PVar n)


-- | Parse a string pattern
pStrPattern :: Parser Src.Pattern
pStrPattern = do
  (area, s) <- withArea pStringLiteral
  target <- pSourceTarget
  return $ Src.Source area target (Src.PStr s)


-- | Parse a char pattern
pCharPattern :: Parser Src.Pattern
pCharPattern = do
  (area, c) <- withArea pCharLiteral
  target <- pSourceTarget
  return $ Src.Source area target (Src.PChar c)


-- | Parse a boolean pattern
pBoolPattern :: Parser Src.Pattern
pBoolPattern = do
  (area, b) <- withArea $ choice
    [ "true"  <$ satisfyTok (\rt -> rtToken rt == TkTrue)
    , "false" <$ satisfyTok (\rt -> rtToken rt == TkFalse)
    ]
  target <- pSourceTarget
  return $ Src.Source area target (Src.PBool b)


-- | Parse a record pattern: { field: pat, field, ...rest }
pRecordPattern :: Parser Src.Pattern
pRecordPattern = do
  (startArea, _) <- withArea (void pLeftCurly)
  fields <- pRecordFieldPatterns
  _ <- optional pComma
  (endArea, _) <- withArea (void pRightCurly)
  target <- pSourceTarget
  return $ Src.Source (mergeAreas startArea endArea) target (Src.PRecord fields)


pRecordFieldPatterns :: Parser [Src.PatternField]
pRecordFieldPatterns = do
  first <- pRecordFieldPattern
  rest <- many $ try $ do
    pComma
    pRecordFieldPattern
  return $ first : rest


pRecordFieldPattern :: Parser Src.PatternField
pRecordFieldPattern = choice
  [ -- ...rest pattern
    try $ do
      pSpread
      (nameArea, name) <- withArea pNameStr
      target <- pSourceTarget
      return $ Src.PatternFieldRest (Src.Source nameArea target name)
  , -- name: pattern
    try $ do
      (nameArea, name) <- withArea pNameStr
      target <- pSourceTarget
      pColon
      pat <- pPattern
      return $ Src.PatternField (Src.Source nameArea target name) pat
  , -- shorthand: just name
    do
      (nameArea, name) <- withArea pNameStr
      target <- pSourceTarget
      return $ Src.PatternFieldShorthand (Src.Source nameArea target name)
  ]


-- | Parse a list pattern: [pat, pat, ...rest]
pListPattern :: Parser Src.Pattern
pListPattern = do
  (startArea, _) <- withArea (void pLeftSquareBracket)
  items <- pListItemPatterns
  _ <- optional pComma
  (endArea, _) <- withArea (void pRightSquareBracket)
  target <- pSourceTarget
  return $ Src.Source (mergeAreas startArea endArea) target (Src.PList items)


pListItemPatterns :: Parser [Src.Pattern]
pListItemPatterns = option [] $ do
  first <- pListItemOrSpread
  rest <- many $ try $ do
    pComma
    pListItemOrSpread
  return $ first : rest
  where
    pListItemOrSpread = try pSpreadPattern <|> pPattern


-- | Parse a spread pattern: ...name
pSpreadPattern :: Parser Src.Pattern
pSpreadPattern = do
  (startArea, _) <- withArea (void pSpread)
  (nameArea, name) <- withArea pNameStr
  target <- pSourceTarget
  return $ Src.Source (mergeAreas startArea nameArea) target (Src.PSpread (nameToPattern nameArea target name))


-- | Parse a tuple pattern: #[pat, pat]
pTuplePattern :: Parser Src.Pattern
pTuplePattern = do
  (startArea, _) <- withArea (void pTupleStart)
  rets
  items <- pTupleItemPatterns
  _ <- optional pComma
  rets
  (endArea, _) <- withArea (void pRightSquareBracket)
  target <- pSourceTarget
  return $ Src.Source (mergeAreas startArea endArea) target (Src.PTuple items)


pTupleItemPatterns :: Parser [Src.Pattern]
pTupleItemPatterns = do
  first <- pPattern
  rest <- many $ try $ do
    pComma
    rets
    pPattern
  return $ first : rest


-- | Parse a parenthesized pattern: (pattern)
pParenthesizedPattern :: Parser Src.Pattern
pParenthesizedPattern = do
  pLeftParen
  pat <- pPattern
  pRightParen
  return pat
