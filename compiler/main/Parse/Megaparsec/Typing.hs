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

import qualified Data.Map.Strict                as M
import           Control.Monad                  ( void )

import           Text.Megaparsec                hiding ( State, Token )

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Lexeme
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )


-- Helper: peek at the next token kind (Nothing at EOF)
peekTok :: Parser (Maybe Token)
peekTok = fmap (fmap rtToken) (optional (lookAhead anySingle))


-- | Parse a type annotation that may include constraints
pConstrainedTyping :: Parser Src.Typing
pConstrainedTyping = do
  mt <- peekTok
  case mt of
    Just TkLeftParen -> do
      mResult <- optional $ try $ do
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
      case mResult of
        Just t  -> return t
        Nothing -> pTypings
    _ -> do
      mResult <- optional $ try $ do
        c <- pConstraint
        rets
        pFatArrow
        rets
        t <- pTypings
        return $ Src.Source (mergeAreas (Src.getArea c) (Src.getArea t)) (Src.getSourceTarget c) (Src.TRConstrained [c] t)
      case mResult of
        Just t  -> return t
        Nothing -> pTypings


-- | Parse a list of constraints separated by commas
pConstraints :: Parser [Src.Typing]
pConstraints = do
  first <- pConstraint
  rest <- many (try $ pComma *> rets *> pConstraint)
  return $ first : rest


-- | Parse a single constraint: ClassName typeVar1 typeVar2 ...
pConstraint :: Parser Src.Typing
pConstraint = do
  (startArea, className) <- withArea pName
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
      (area, name) <- withArea pName
      target <- pSourceTarget
      return $ Src.Source area target (Src.TRSingle name)
  ]


-- | Parse instance constraints
pInstanceConstraints :: Parser [Src.Typing]
pInstanceConstraints = do
  first <- pInstanceConstraint
  rest <- many (try $ pComma *> maybeRet *> pInstanceConstraint)
  return $ first : rest


-- | Parse a single instance constraint
pInstanceConstraint :: Parser Src.Typing
pInstanceConstraint = do
  (startArea, className) <- withArea pName
  target <- pSourceTarget
  args <- some pConstraintArg
  let endArea = Src.getArea (last args)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TRComp className args)


-- | Parse many typings in sequence
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
pTypings :: Parser Src.Typing
pTypings = do
  left <- pTypingsLeft
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


-- | Parse the left-hand side of a typing: composite or atomic.
{-# INLINE pTypingsLeft #-}
pTypingsLeft :: Parser Src.Typing
pTypingsLeft = do
  mt <- peekTok
  case mt of
    Just TkLeftCurly  -> pAtomicTyping   -- record or unit
    Just TkTupleStart -> pAtomicTyping   -- tuple type
    Just TkLeftParen  -> try pCompositeTyping <|> pAtomicTyping
    _                 -> pTypingsFromName


-- | Parse a typing that begins with an identifier, parsing the name exactly once.
pTypingsFromName :: Parser Src.Typing
pTypingsFromName = do
  (startArea, name1) <- withArea pName
  target <- pSourceTarget
  mDot <- optional $ try $ do
    pDot
    pName
  case mDot of
    Just name2 -> do
      let qualName = name1 ++ "." ++ name2
      args <- many pCompositeTypingArg
      if null args
        then return $ Src.Source startArea target (Src.TRComp qualName [])
        else return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp qualName args)
    Nothing -> do
      args <- many pCompositeTypingArg
      if null args
        then return $ Src.Source startArea target (Src.TRSingle name1)
        else return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp name1 args)


-- | Parse a composite typing (type application)
pCompositeTyping :: Parser Src.Typing
pCompositeTyping = do
  mt <- peekTok
  case mt of
    Just TkLeftParen -> do
      (startArea, _) <- withArea (void pLeftParen)
      name <- pName
      target <- pSourceTarget
      qualName <- option name $ try $ do
        pDot
        name2 <- pName
        return $ name ++ "." ++ name2
      rets
      args <- some pCompositeTypingArg
      (endArea, _) <- withArea (void pRightParen)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRComp qualName args)
    _ -> do
      (startArea, name1) <- withArea pName
      target <- pSourceTarget
      mDot <- optional $ try $ do
        pDot
        pName
      case mDot of
        Just name2 -> do
          let qualName = name1 ++ "." ++ name2
          args <- many pCompositeTypingArg
          if null args
            then return $ Src.Source startArea target (Src.TRComp qualName [])
            else return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp qualName args)
        Nothing -> do
          args <- some pCompositeTypingArg
          return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp name1 args)


-- | Parse a single composite typing argument
pCompositeTypingArg :: Parser Src.Typing
pCompositeTypingArg = do
  mt <- peekTok
  case mt of
    Just TkLeftParen -> do
      (startArea, _) <- withArea (void pLeftParen)
      rets
      inner <- pTypingsLeft
      mArrow <- optional $ try $ do
        pRightArrow
        pTypings
      case mArrow of
        Just right -> do
          (endArea, _) <- withArea (void pRightParen)
          target <- pSourceTarget
          return $ Src.Source (mergeAreas startArea endArea) target (Src.TRArr inner right)
        Nothing -> do
          pRightParen
          return inner
    Just TkLeftCurly  -> pAtomicTyping
    Just TkTupleStart -> pAtomicTyping
    _ -> do
      (startArea, name1) <- withArea pName
      target <- pSourceTarget
      mDot <- optional $ try $ do
        pDot
        pName
      case mDot of
        Just name2 ->
          return $ Src.Source startArea target (Src.TRSingle $ name1 ++ "." ++ name2)
        Nothing ->
          return $ Src.Source startArea target (Src.TRSingle name1)


-- | Parse an atomic (simple) typing
pAtomicTyping :: Parser Src.Typing
pAtomicTyping = do
  mt <- peekTok
  case mt of
    Just TkLeftCurly  -> try pRecordOrUnitTyping
    Just TkTupleStart -> do
      (startArea, _) <- withArea (void pTupleStart)
      items <- pTupleTypings
      _ <- optional pComma
      (endArea, _) <- withArea (void pRightSquareBracket)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRTuple items)
    Just TkLeftParen  -> try $ do
      pLeftParen
      rets
      t <- pTypings
      pRightParen
      return t
    _ -> do
      (area, name) <- withArea pName
      target <- pSourceTarget
      return $ Src.Source area target (Src.TRSingle name)


-- | Parse record type or unit type (all start with '{')
pRecordOrUnitTyping :: Parser Src.Typing
pRecordOrUnitTyping = do
  (startArea, _) <- withArea (void pLeftCurly)
  rets
  mt <- peekTok
  case mt of
    Just TkRightCurly -> do  -- unit type
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRSingle "{}")
    Just TkSpread -> do   -- spread extension
      pSpread
      extName <- pName
      target <- pSourceTarget
      fields <- option M.empty $ try $ do
        pComma
        rets
        pRecordTypingArgs
      _ <- optional pComma
      rets
      (endArea, _) <- withArea (void pRightCurly)
      let ext = Src.Source (mergeAreas startArea endArea) target (Src.TRSingle extName)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRRecord fields (Just ext))
    _ -> do    -- regular record fields
      fields <- pRecordTypingArgs
      _ <- optional pComma
      rets
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRRecord fields Nothing)


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
