{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Typing
  ( pAtomicTyping
  , pTypings
  , pCompositeTyping
  , pConstrainedTyping
  , pConstraint
  , pConstraints
  , pInstanceConstraint
  , pInstanceConstraints
  , pManyTypings
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import qualified Data.Map.Strict                as M
import           Control.Monad                  ( void )

import           Text.Megaparsec                hiding ( State )

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Lexeme


-- | Parse a type annotation that may include constraints
-- constrainedTyping = constraint '=>' typings | '(' constraints ')' '=>' typings | typings
pConstrainedTyping :: Parser Src.Typing
pConstrainedTyping = choice
  [ try $ do
      (area, _) <- withArea (void pLeftParen)
      rets
      cs <- pConstraints
      rets
      pRightParen
      rets
      pFatArrow
      rets
      t <- pTypings
      target <- pSourceTarget
      return $ Src.Source (mergeAreas area (Src.getArea t)) target (Src.TRConstrained cs t)
  , try $ do
      c <- pConstraint
      rets
      pFatArrow
      rets
      t <- pTypings
      return $ Src.Source (mergeAreas (Src.getArea c) (Src.getArea t)) (Src.getSourceTarget c) (Src.TRConstrained [c] t)
  , pTypings
  ]


-- | Parse a list of constraints separated by commas
pConstraints :: Parser [Src.Typing]
pConstraints = do
  first <- pConstraint
  rest <- many (try $ pComma *> rets *> pConstraint)
  return $ first : rest


-- | Parse a single constraint: ClassName typeVar1 typeVar2 ...
-- or ClassName typeVar (typing)
pConstraint :: Parser Src.Typing
pConstraint = do
  (startArea, className) <- withArea pNameStr
  target <- pSourceTarget
  args <- some pConstraintArg
  let endArea = Src.getArea (last args)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TRComp className args)


pConstraintArg :: Parser Src.Typing
pConstraintArg = choice
  [ try $ do
      pLeftParen
      rets
      t <- pTypings
      pRightParen
      return t
  , do
      (area, name) <- withArea pNameStr
      target <- pSourceTarget
      return $ Src.Source area target (Src.TRSingle name)
  ]


-- | Parse instance constraints
-- These use nameC in the original grammar, but we handle them the same way
pInstanceConstraints :: Parser [Src.Typing]
pInstanceConstraints = do
  first <- pInstanceConstraint
  rest <- many (try $ pComma *> maybeRet *> pInstanceConstraint)
  return $ first : rest


-- | Parse a single instance constraint
-- Same structure as a regular constraint
pInstanceConstraint :: Parser Src.Typing
pInstanceConstraint = do
  (startArea, className) <- withArea pNameStr
  target <- pSourceTarget
  args <- some pConstraintArg
  let endArea = Src.getArea (last args)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TRComp className args)


-- | Parse many typings in sequence (for instance declarations: `instance Functor Maybe`)
pManyTypings :: Parser [Src.Typing]
pManyTypings = some pSingleTyping
  where
    pSingleTyping = choice
      [ pAtomicTyping
      , pParenthesizedTyping
      ]

    pParenthesizedTyping = do
      pLeftParen
      rets
      t <- pTypings
      pRightParen
      return t


-- | Parse a full typing (including arrow types)
-- typings = typing '->' typings | compositeTyping '->' typings | compositeTyping | typing
pTypings :: Parser Src.Typing
pTypings = do
  left <- try pCompositeTyping <|> pAtomicTyping
  rest <- optional $ try $ do
    rets
    pRightArrow
    rets
    pTypings
  case rest of
    Nothing -> return left
    Just right -> do
      target <- pSourceTarget
      return $ Src.Source (mergeAreas (Src.getArea left) (Src.getArea right)) target (Src.TRArr left right)


-- | Parse a composite typing (type application): `Maybe a`, `List String`, `M.Map`
pCompositeTyping :: Parser Src.Typing
pCompositeTyping = choice
  [ -- Parenthesized composite: (name args)
    try $ do
      (startArea, _) <- withArea (void pLeftParen)
      name <- pNameStr
      target <- pSourceTarget
      -- Check for qualified name
      qualName <- option name $ try $ do
        pDot
        name2 <- pNameStr
        return $ name ++ "." ++ name2
      rets
      args <- some pCompositeTypingArg
      (endArea, _) <- withArea (void pRightParen)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRComp qualName args)
  , -- name.name args? or name args (at least one arg for plain name)
    try $ do
      (startArea, name1) <- withArea pNameStr
      target <- pSourceTarget
      -- Check for qualified name
      mDot <- optional $ try $ do
        pDot
        name2 <- pNameStr
        return name2
      case mDot of
        Just name2 -> do
          -- Qualified name: may have args or not
          let qualName = name1 ++ "." ++ name2
          args <- many pCompositeTypingArg
          if null args
            then return $ Src.Source startArea target (Src.TRComp qualName [])
            else return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp qualName args)
        Nothing -> do
          -- Simple name: must have at least one arg
          args <- some pCompositeTypingArg
          return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp name1 args)
  ]


-- | Parse a single composite typing argument
pCompositeTypingArg :: Parser Src.Typing
pCompositeTypingArg = choice
  [ try $ do
      -- Parenthesized arrow type: (typing -> typings)
      (startArea, _) <- withArea (void pLeftParen)
      left <- try pCompositeTyping <|> pAtomicTyping
      pRightArrow
      right <- pTypings
      (endArea, _) <- withArea (void pRightParen)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRArr left right)
  , try $ do
      -- Qualified name: name.name
      (startArea, name1) <- withArea pNameStr
      target <- pSourceTarget
      pDot
      (endArea, name2) <- withArea pNameStr
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRSingle $ name1 ++ "." ++ name2)
  , pAtomicTyping
  ]


-- | Parse an atomic (simple) typing
-- typing = name | '{}' | '(' typings ')' | '{' recordTypingArgs '}' | '#[' tupleTypings ']'
pAtomicTyping :: Parser Src.Typing
pAtomicTyping = choice
  [ -- Unit type: {} or {  } (no fields, no extension)
    try $ do
      (startArea, _) <- withArea (void pLeftCurly)
      rets
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRSingle "{}")

  , -- Record type with extension: { ...ext, fields }
    try $ do
      (startArea, _) <- withArea (void pLeftCurly)
      rets
      pSpread
      extName <- pNameStr
      target <- pSourceTarget
      -- May have trailing fields or not
      fields <- option M.empty $ try $ do
        pComma
        rets
        pRecordTypingArgs
      _ <- optional pComma
      rets
      (endArea, _) <- withArea (void pRightCurly)
      let ext = Src.Source (mergeAreas startArea endArea) target (Src.TRSingle extName)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRRecord fields (Just ext))

  , -- Record type: { field :: Type, ... }
    try $ do
      (startArea, _) <- withArea (void pLeftCurly)
      rets
      fields <- pRecordTypingArgs
      _ <- optional pComma
      rets
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRRecord fields Nothing)

  , -- Tuple type: #[Type1, Type2]
    try $ do
      (startArea, _) <- withArea (void pTupleStart)
      items <- pTupleTypings
      _ <- optional pComma
      (endArea, _) <- withArea (void pRightSquareBracket)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRTuple items)

  , -- Parenthesized typing: ( typings )
    try $ do
      pLeftParen
      rets
      t <- pTypings
      pRightParen
      return t

  , -- Simple name
    do
      (area, name) <- withArea pNameStr
      target <- pSourceTarget
      return $ Src.Source area target (Src.TRSingle name)
  ]


-- | Parse record typing args: name :: typings (, name :: typings)*
pRecordTypingArgs :: Parser (M.Map Src.Name (Area, Src.Typing))
pRecordTypingArgs = do
  first <- pRecordTypingField
  rest <- many $ try $ do
    pComma
    rets
    pRecordTypingField
  return $ M.fromList (first : rest)
  where
    pRecordTypingField = do
      (nameArea, name) <- withArea pNameStr
      pDoubleColon
      t <- pTypings
      return (name, (nameArea, t))


-- | Parse tuple typing items
pTupleTypings :: Parser [Src.Typing]
pTupleTypings = do
  first <- pTypings
  rest <- some $ try $ pComma *> pTypings
  return $ first : rest
