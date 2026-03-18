{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Pattern
  ( pPattern
  ) where

import qualified Data.ByteString.Char8         as C8
import           Data.Char                      ( isUpper )
import           Control.Monad                  ( void )

import           Text.Megaparsec                hiding ( State )

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Lexeme


-- | Parse a pattern (composite or non-composite)
pPattern :: Parser Src.Pattern
pPattern = do
  b <- lookAhead anySingle
  -- Only try composite pattern if the first character could be an uppercase name or module
  if isUpperB b
    then choice [try pCompositePattern, pNonCompositePattern]
    else pNonCompositePattern


-- | Parse a composite pattern: Constructor(args) or Module.Constructor(args)
-- Parses the first name once, then dispatches on '.' vs '(' vs end
pCompositePattern :: Parser Src.Pattern
pCompositePattern = try $ do
  (startArea, name1) <- withArea pNameStr
  target <- pSourceTarget
  mDot <- optional $ try $ do
    pDot
    pNameStr
  case mDot of
    Just name2 -> do
      -- Module.Constructor pattern
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
pNonCompositePattern :: Parser Src.Pattern
pNonCompositePattern = choice
  [ pRecordPattern
  , pListPattern
  , pTuplePattern
  , pParenthesizedPattern
  , try pNegativeFloatPattern
  , try pNegativeNumPattern
  , pFloatPattern
  , pNumPattern
  , pStrPattern
  , pCharPattern
  , pBoolPattern
  , pNamePattern
  ]


-- | Parse a name/variable/wildcard/nullary constructor pattern
pNamePattern :: Parser Src.Pattern
pNamePattern = do
  (area, name) <- withArea pNameStr
  target <- pSourceTarget
  return $ nameToPattern area target name


-- | Convert a name to the appropriate pattern type
nameToPattern :: Area -> Src.SourceTarget -> String -> Src.Pattern
nameToPattern area target n
  | n == "_"           = Src.Source area target Src.PAny
  | isUpper (head n)   = Src.Source area target (Src.PNullaryCon (Src.Source area target n))
  | otherwise          = Src.Source area target (Src.PVar n)


-- | Parse a number pattern
pNumPattern :: Parser Src.Pattern
pNumPattern = do
  (area, n) <- withArea pNumber
  target <- pSourceTarget
  return $ Src.Source area target (Src.PNum n)


-- | Parse a negative number pattern
pNegativeNumPattern :: Parser Src.Pattern
pNegativeNumPattern = do
  (area, _) <- withArea pDashUnary
  n <- pNumber
  target <- pSourceTarget
  return $ Src.Source area target (Src.PNum $ '-' : n)


-- | Parse a float pattern
pFloatPattern :: Parser Src.Pattern
pFloatPattern = do
  (area, f) <- withArea pFloat
  target <- pSourceTarget
  return $ Src.Source area target (Src.PFloat f)


-- | Parse a negative float pattern
pNegativeFloatPattern :: Parser Src.Pattern
pNegativeFloatPattern = do
  (area, _) <- withArea pDashUnary
  f <- pFloat
  target <- pSourceTarget
  return $ Src.Source area target (Src.PFloat $ '-' : f)


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
  (area, b) <- withArea (C8.unpack <$> (pBoolTrue <|> pBoolFalse))
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
