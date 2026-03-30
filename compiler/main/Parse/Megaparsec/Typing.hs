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
pConstrainedTyping = do
  b <- lookAhead anySingle
  case b of
    40 -> do  -- '(' : could be (constraints) => ... or just a parenthesized type
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
      -- Try single constraint => typings, otherwise fall through to typings
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
-- typings = compositeTyping '->' typings | compositeTyping | atomicTyping '->' typings | atomicTyping
-- Optimized: peek ahead to decide composite vs atomic without double-parsing the first name.
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
-- For identifier-starting types, parse the name ONCE then decide if composite or atomic.
{-# INLINE pTypingsLeft #-}
pTypingsLeft :: Parser Src.Typing
pTypingsLeft = do
  b <- lookAhead anySingle
  case b of
    123 -> pAtomicTyping   -- '{': always atomic (record or unit)
    35  -> pAtomicTyping   -- '#': always atomic (tuple type)
    40  -> do              -- '(': parenthesized — could be composite or atomic
      try pCompositeTyping <|> pAtomicTyping
    _   ->                 -- identifier: parse name once, decide composite vs atomic
      pTypingsFromName

-- | Parse a typing that begins with an identifier, parsing the name exactly once.
-- If the name is followed by '.' or an argument, it's composite; otherwise atomic.
pTypingsFromName :: Parser Src.Typing
pTypingsFromName = do
  (startArea, name1) <- withArea pNameStr
  target <- pSourceTarget
  -- Check for dot (qualified name) or more args (composite)
  mDot <- optional $ try $ do
    pDot
    pNameStr
  case mDot of
    Just name2 -> do
      -- Qualified name: name1.name2 — check for args
      let qualName = name1 ++ "." ++ name2
      args <- many pCompositeTypingArg
      if null args
        then return $ Src.Source startArea target (Src.TRComp qualName [])
        else return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp qualName args)
    Nothing -> do
      -- Simple name: check if there are args (composite) or not (atomic)
      args <- many pCompositeTypingArg
      if null args
        then return $ Src.Source startArea target (Src.TRSingle name1)
        else return $ Src.Source (mergeAreas startArea (Src.getArea (last args))) target (Src.TRComp name1 args)


-- | Parse a composite typing (type application): `Maybe a`, `List String`, `M.Map`
-- Optimized: parse name once, then check for dot or args — no double-backtracking.
pCompositeTyping :: Parser Src.Typing
pCompositeTyping = do
  b <- lookAhead anySingle
  case b of
    40 -> do  -- '(' : parenthesized composite
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
    _ -> do
      -- name.name or name with args (or plain qualified name)
      (startArea, name1) <- withArea pNameStr
      target <- pSourceTarget
      -- Check for dot (qualified name)
      mDot <- optional $ try $ do
        pDot
        pNameStr
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


-- | Parse a single composite typing argument
pCompositeTypingArg :: Parser Src.Typing
pCompositeTypingArg = do
  b <- lookAhead anySingle
  case b of
    40 -> do  -- '('
      -- Parse '(' then the inner typing, then check for '->' (arrow) or ')' (plain)
      (startArea, _) <- withArea (void pLeftParen)
      rets
      inner <- pTypingsLeft  -- parse inner typing (handles composite/atomic inside parens)
      -- Check for '->' to detect arrow type
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
    123 -> pAtomicTyping  -- '{': record/unit atomic
    35  -> pAtomicTyping  -- '#': tuple atomic
    _ -> do
      -- Identifier: parse name once, then check if followed by '.' (qualified) or not (atomic)
      (startArea, name1) <- withArea pNameStr
      target <- pSourceTarget
      mDot <- optional $ try $ do
        pDot
        pNameStr
      case mDot of
        Just name2 ->
          return $ Src.Source (mergeAreas startArea startArea) target (Src.TRSingle $ name1 ++ "." ++ name2)
        Nothing ->
          return $ Src.Source startArea target (Src.TRSingle name1)


-- | Parse an atomic (simple) typing
-- typing = name | '{}' | '(' typings ')' | '{' recordTypingArgs '}' | '#[' tupleTypings ']'
pAtomicTyping :: Parser Src.Typing
pAtomicTyping = do
  b <- lookAhead anySingle
  case b of
    123 ->  -- '{'
      try pRecordOrUnitTyping
    35 ->   -- '#'
      -- Tuple type: #[Type1, Type2]
      do
        (startArea, _) <- withArea (void pTupleStart)
        items <- pTupleTypings
        _ <- optional pComma
        (endArea, _) <- withArea (void pRightSquareBracket)
        target <- pSourceTarget
        return $ Src.Source (mergeAreas startArea endArea) target (Src.TRTuple items)
    40 ->   -- '('
      -- Parenthesized typing: ( typings )
      try $ do
        pLeftParen
        rets
        t <- pTypings
        pRightParen
        return t
    _ ->    -- Simple name
      do
        (area, name) <- withArea pNameStr
        target <- pSourceTarget
        return $ Src.Source area target (Src.TRSingle name)


-- | Parse record type or unit type (all start with '{')
pRecordOrUnitTyping :: Parser Src.Typing
pRecordOrUnitTyping = do
  (startArea, _) <- withArea (void pLeftCurly)
  rets
  b <- lookAhead anySingle
  case b of
    125 -> do  -- '}': unit type
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TRSingle "{}")
    46 -> do   -- '.': spread extension { ...ext, fields }
      pSpread
      extName <- pNameStr
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
