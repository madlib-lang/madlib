{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Expression
  ( pExp
  , pBodyExp
  , pMultiExpBody
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Map                       as M
import           Data.Word                      ( Word8 )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( void, when )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec.Byte          as C
import           Control.Monad.Combinators.Expr

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Escape        ( processEscapes )
import           Parse.Megaparsec.Lexeme
import           Parse.Megaparsec.Pattern
import           Parse.Megaparsec.Typing


-- | Parse a body expression (assignment, mutation, typed expression, or plain expression)
-- bodyExp = name '=' exp | name '::' constrainedTyping '\n' name '=' exp | exp ':=' exp | exp
pBodyExp :: Parser Src.Exp
pBodyExp = choice
  [ -- name :: constrainedTyping \n ... (named typed expression OR extern declaration)
    try $ do
      (startArea, name1) <- withArea pNameStr
      target <- pSourceTarget
      pDoubleColon
      typing <- pConstrainedTyping
      rets
      -- Check for extern: [export] name = extern "path"
      -- or typed assignment: name = exp
      choice
        [ -- extern: [export] name = extern "path"
          try $ do
            exported <- optional $ try pExport
            name2 <- pNameStr
            pEq
            pExtern
            (endArea, path) <- withArea pStringLiteral
            let externExpr = Src.Source (mergeAreas startArea endArea) target (Src.Extern typing name2 path)
            case exported of
              Nothing -> return externExpr
              Just _  -> return $ Src.Source (mergeAreas startArea endArea) target (Src.Export externExpr)
        , -- named typed assignment: name = exp
          do
            name2 <- pNameStr
            pEq
            rets
            body <- pExp
            let assignArea = mergeAreas startArea (Src.getArea body)
            return $ Src.Source assignArea target (Src.NamedTypedExp name1 (Src.Source assignArea target (Src.Assignment name2 body)) typing)
        ]
  , -- name = exp (assignment)
    try $ do
      (startArea, name) <- withArea pNameStr
      target <- pSourceTarget
      pEq
      maybeRet
      body <- pExp
      return $ Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Assignment name body)
  , -- exp := exp (mutation)
    try $ do
      lhs <- pExp
      target <- pSourceTarget
      pMutateEq
      maybeRet
      rhs <- pExp
      return $ Src.Source (mergeAreas (Src.getArea lhs) (Src.getArea rhs)) target (Src.Mutate lhs rhs)
  , -- plain expression
    pExp
  ]


-- | Parse a multi-expression body (used in multiline lambdas and do blocks)
-- multiExpBody = 'return' exp | bodyExp rets multiExpBody | empty
pMultiExpBody :: Parser [Src.Exp]
pMultiExpBody = choice
  [ -- return exp
    try $ do
      (startArea, _) <- withArea pReturn
      target <- pSourceTarget
      body <- pExp
      return [Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Return body)]
  , -- empty body produces unit
    try $ do
      lookAhead pRightCurly
      return [Src.Source emptyArea Src.TargetAll Src.LUnit]
  , -- bodyExp followed by more expressions
    do
      e <- pBodyExp
      rets
      rest <- pMultiExpBody
      return $ e : rest
  ]


-- | Parse an expression (includes lowest-precedence ternary)
{-# INLINE pExp #-}
pExp :: Parser Src.Exp
pExp = do
  e <- pExprWithOps
  -- Try ternary: e ? trueExpr : falseExpr (? may be on next line)
  -- Peek first to avoid the overhead of `try` on non-ternary expressions
  mc <- optional (lookAhead anySingle)
  case fmap chr8' mc of
    Just '?' -> do
      pQuestionMark
      maybeRet
      trueExpr <- pExp
      maybeRet
      pColon
      maybeRet
      falseExpr <- pExp
      return $ Src.Source (mergeAreas (Src.getArea e) (Src.getArea falseExpr)) (Src.getSourceTarget e) (Src.Ternary e trueExpr falseExpr)
    Just '\n' -> option e $ try $ do
      maybeRet
      pQuestionMark
      maybeRet
      trueExpr <- pExp
      maybeRet
      pColon
      maybeRet
      falseExpr <- pExp
      return $ Src.Source (mergeAreas (Src.getArea e) (Src.getArea falseExpr)) (Src.getSourceTarget e) (Src.Ternary e trueExpr falseExpr)
    _ -> return e


-- | Expression parser with operator precedence
-- Uses makeExprParser for clean operator handling
pExprWithOps :: Parser Src.Exp
pExprWithOps = makeExprParser pTermWithNewline operatorTable
  where
    -- Consume trailing optional newline after each term so operators can appear on next line
    -- This avoids repeating maybeRet inside every operator in the table
    pTermWithNewline = pTermWithPostfix <* maybeRet


-- | Build a BinOp node from an operator string and its area.
{-# INLINE mkBinOp #-}
mkBinOp :: Area -> Src.SourceTarget -> String -> Src.Exp -> Src.Exp -> Src.Exp
mkBinOp area target opStr l r =
  Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target
    (Src.BinOp l (Src.Source area target (Src.Var opStr)) r)

-- | Operator precedence table for makeExprParser.
-- Uses a single dispatching InfixL per level for fast first-byte dispatch.
-- NOTE: makeExprParser treats FIRST entry as HIGHEST precedence (opposite of docs).
-- This order matches the Happy grammar's %left declarations (highest to lowest).
operatorTable :: [[Operator Parser Src.Exp]]
operatorTable =
  [ -- Highest precedence: * / %
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          42 -> do (area, _) <- withArea pStar;    target <- pSourceTarget; maybeRet; return $ mkBinOp area target "*"
          47 -> do (area, _) <- withArea pSlash;   target <- pSourceTarget; maybeRet; return $ mkBinOp area target "/"
          37 -> do (area, _) <- withArea pPercent; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "%"
          _  -> empty
    ]
  , -- + ++ -
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          43 -> choice
            [ do (area, _) <- withArea pDoublePlus; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "++"
            , do (area, _) <- withArea pPlus;       target <- pSourceTarget; maybeRet; return $ mkBinOp area target "+"
            ]
          45 -> do (area, _) <- withArea pDash; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "-"
          _  -> empty
    ]
  , -- == != > < >= <=
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          61 -> do (area, _) <- withArea pDoubleEq;       target <- pSourceTarget; maybeRet; return $ mkBinOp area target "=="
          33 -> do (area, _) <- withArea pNotEq;          target <- pSourceTarget; maybeRet; return $ mkBinOp area target "!="
          62 -> choice
            [ do (area, _) <- withArea pRightChevronEq;  target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">="
            , do (area, _) <- withArea pRightChevron;    target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">"
            ]
          60 -> choice
            [ do (area, _) <- withArea pLeftChevronEq;   target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<="
            , do (area, _) <- withArea pLeftChevron;     target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<"
            ]
          _  -> empty
    ]
  , -- &&
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          38 -> do (area, _) <- withArea pDoubleAmpersand; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "&&"
          _  -> empty
    ]
  , -- ||
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          124 -> do (area, _) <- withArea pDoublePipe; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "||"
          _   -> empty
    ]
  , -- |>
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          124 -> do (area, _) <- withArea pPipeOp; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "|>"
          _   -> empty
    ]
  , -- <|>
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          60 -> do (area, _) <- withArea pAlternativeOp; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<|>"
          _  -> empty
    ]
  , -- Lowest precedence: bitwise operators | & ^ << >> >>>
    [ InfixL $ try $ do
        b <- lookAhead anySingle
        case b of
          124 -> do (area, _) <- withArea pPipeChar;          target <- pSourceTarget; maybeRet; return $ mkBinOp area target "|"
          38  -> do (area, _) <- withArea pAmpersand;         target <- pSourceTarget; maybeRet; return $ mkBinOp area target "&"
          94  -> do (area, _) <- withArea pXor;               target <- pSourceTarget; maybeRet; return $ mkBinOp area target "^"
          60  -> do (area, _) <- withArea pDoubleLeftChevron; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<<"
          62  -> choice
            [ do (area, _) <- withArea pTripleRightChevron; target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">>>"
            , do (area, _) <- withArea pDoubleRightChevron; target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">>"
            ]
          _   -> empty
    ]
  ]


-- | Parse a term with optional postfix operations (application, access, array access, ternary)
pTermWithPostfix :: Parser Src.Exp
pTermWithPostfix = do
  base <- pTerm
  applyPostfix base
  where
    applyPostfix :: Src.Exp -> Parser Src.Exp
    applyPostfix expr = do
      mc <- optional (lookAhead anySingle)
      case fmap chr8' mc of
        Nothing -> return expr
        Just '(' -> do
          -- Function application or nullary application
          f <- try $ do
            pLeftParen
            choice
              [ try $ do
                  rets
                  args <- pArgsWithPlaceholder
                  rets
                  (endArea, _) <- withArea (void pRightParen)
                  return $ \e -> Src.Source (mergeAreas (Src.getArea e) endArea) (Src.getSourceTarget e) (Src.App e args)
              , do
                  (endArea, _) <- withArea (void pRightParen)
                  return $ \e -> Src.Source (mergeAreas (Src.getArea e) endArea) (Src.getSourceTarget e) (Src.App e [])
              ]
          applyPostfix (f expr)
        Just '.' -> do
          f <- try $ do
            pDot
            (nameArea, name) <- withArea pNameStr
            target <- pSourceTarget
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) (Src.getSourceTarget e) (Src.Access e (Src.Source nameArea target (Src.Var $ '.' : name)))
          applyPostfix (f expr)
        Just '[' -> do
          f <- try $ do
            pLeftSquareBracket
            rets
            idx <- pExp
            rets
            (endArea, _) <- withArea (void pRightSquareBracket)
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) endArea) (Src.getSourceTarget e) (Src.ArrayAccess e idx)
          applyPostfix (f expr)
        Just '\n' -> do
          -- Method chaining: check if next non-whitespace is '.'
          f <- optional $ try $ do
            maybeRet
            pDot
            (nameArea, name) <- withArea pNameStr
            target <- pSourceTarget
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) (Src.getSourceTarget e) (Src.Access e (Src.Source nameArea target (Src.Var $ '.' : name)))
          case f of
            Nothing -> return expr
            Just fn -> applyPostfix (fn expr)
        _ -> return expr


-- | Parse a term (primary expression without operators)
-- Uses lookAhead dispatch to avoid unnecessary backtracking.
pTerm :: Parser Src.Exp
pTerm = do
  b <- lookAhead anySingle
  case chr8' b of
    '?' -> pTypedHoleExpr
    '#' -> choice [pJSBlockExpr, pTupleConstructor, try pHashName]
    '[' -> pListConstructor
    '`' -> pTemplateString
    '(' -> choice [try pAbsOrParenthesized, try pTypedExp]
    '<' -> try pJsxTag
    '.' -> try pDotName
    '!' -> pPrefixExp
    '~' -> pPrefixExp
    '-' -> pPrefixExp
    '{' -> do
        -- Peek 2nd byte: '{{' = dict, '{' alone = record or unit literal
        mb2 <- optional $ lookAhead (anySingle *> anySingle)
        case mb2 of
          Just 123 -> pDict   -- '{{': dictionary literal
          _        -> choice [try pRecord, pLiteral]  -- record or unit {}
    '"' -> pLiteral
    '\'' -> pLiteral
    _ | isDigitB b -> pLiteral
      | b == 105 -> do  -- 'i'
          isIf <- option False (True <$ lookAhead (C.string "if"))
          if isIf then choice [pIf', pNameExpr] else pNameExpr
      | b == 119 -> do  -- 'w'
          isWh <- option False (True <$ lookAhead (C.string "wh"))
          if isWh then choice [pWhile', pWhere', pNameExpr] else pNameExpr
      | b == 100 -> do  -- 'd'
          isDo <- option False (True <$ lookAhead (C.string "do"))
          if isDo then choice [pDo', pNameExpr] else pNameExpr
      | b == 112 -> do  -- 'p'
          isPi <- option False (True <$ lookAhead (C.string "pi"))
          if isPi then choice [pPipe', pNameExpr] else pNameExpr
      | b == 116 || b == 102 -> choice [pLiteral, pNameExpr]  -- 't' or 'f'
      | isLowerB b || b == 95 -> pNameExpr  -- lower or '_'
      | isUpperB b -> pNameExpr
      | otherwise -> pNameExpr


-- | Parse a prefix expression: !, ~, or unary minus applied to a term
pPrefixExp :: Parser Src.Exp
pPrefixExp = do
  b <- lookAhead anySingle
  case b of
    33 -> do  -- '!'
      (area, _) <- withArea pExclamationMark
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "!")) e)
    126 -> do  -- '~'
      (area, _) <- withArea pTilde
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "~")) e)
    _ -> do  -- '-'
      (area, _) <- withArea pDashUnary
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "unary-minus")) e)


-- Literals --

pLiteral :: Parser Src.Exp
pLiteral = do
  b <- lookAhead anySingle
  case chr8' b of
    '{' -> do
      -- Unit literal: {}
      (startArea, _) <- withArea (void pLeftCurly)
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target Src.LUnit
    '"' -> do
      (area, s) <- withArea pStringLiteral
      target <- pSourceTarget
      return $ Src.Source area target (Src.LStr s)
    '\'' -> do
      (area, ch) <- withArea pCharLiteral
      target <- pSourceTarget
      return $ Src.Source area target (Src.LChar ch)
    '0' -> pNumericLiteral
    d | d >= '1' && d <= '9' -> pNumericLiteral
    _ -> do
      -- Booleans (start with 't' or 'f')
      choice
        [ do
            (area, _) <- withArea pBoolTrue
            target <- pSourceTarget
            return $ Src.Source area target (Src.LBool "true")
        , do
            (area, _) <- withArea pBoolFalse
            target <- pSourceTarget
            return $ Src.Source area target (Src.LBool "false")
        ]


-- | Parse all numeric literal variants with a single-pass approach.
-- Dispatches on '0x' prefix for hex, otherwise parses decimal digits once
-- then checks for suffix (_b/_s/_i/_f) or decimal point — eliminating backtracking.
pNumericLiteral :: Parser Src.Exp
pNumericLiteral = do
  (area, (target, node)) <- withArea $ do
    t <- pSourceTarget
    n <- lexeme parseNum
    return (t, n)
  return $ Src.Source area target node
  where
    parseNum :: Parser Src.Exp_
    parseNum = do
      isHex <- option False (True <$ C.string "0x")
      if isHex then parseHex else parseDecimal

    parseHex :: Parser Src.Exp_
    parseHex = do
      digits <- C8.unpack <$> takeWhile1P (Just "hex digit") isHexDigitB
      suffix <- optional $ (C.string "_b" :: Parser C8.ByteString) <|> C.string "_s" <|> C.string "_i"
      let hexStr = "0x" ++ digits
      case fmap C8.unpack suffix of
        Just "_b" -> return $ Src.LByte hexStr
        Just "_s" -> return $ Src.LShort hexStr
        Just "_i" -> return $ Src.LInt hexStr
        _         -> return $ Src.LNum hexStr

    parseDecimal :: Parser Src.Exp_
    parseDecimal = do
      digits <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
      -- Check for float suffix first (before integer suffixes)
      mFloat <- optional $ do
        void (C.char 46 :: Parser Word8)  -- '.'
        frac <- C8.unpack <$> takeWhile1P (Just "digit") isDigitB
        fsuf <- optional (C.string "_f" :: Parser C8.ByteString)
        return $ digits ++ "." ++ frac ++ maybe "" C8.unpack fsuf
      case mFloat of
        Just floatStr -> return $ Src.LFloat floatStr
        Nothing -> do
          suffix <- optional $ (C.string "_b" :: Parser C8.ByteString) <|> C.string "_s" <|> C.string "_i" <|> C.string "_f"
          case fmap C8.unpack suffix of
            Just "_b" -> return $ Src.LByte digits
            Just "_s" -> return $ Src.LShort digits
            Just "_i" -> return $ Src.LInt digits
            Just "_f" -> return $ Src.LFloat (digits ++ "_f")
            _         -> return $ Src.LNum digits


-- | Parse a variable name expression
pNameExpr :: Parser Src.Exp
pNameExpr = do
  (area, name) <- withArea pNameStr
  target <- pSourceTarget
  return $ Src.Source area target (Src.Var name)


-- | Parse #name
pHashName :: Parser Src.Exp
pHashName = do
  (startArea, _) <- withArea (void pSharp)
  (endArea, name) <- withArea pNameStr
  target <- pSourceTarget
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Var $ '#' : name)


-- | Parse .name (accessor shorthand)
pDotName :: Parser Src.Exp
pDotName = do
  (startArea, _) <- withArea (void pDot)
  (endArea, name) <- withArea pNameStr
  target <- pSourceTarget
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Var $ '.' : name)


-- | Parse ???
pTypedHoleExpr :: Parser Src.Exp
pTypedHoleExpr = do
  (area, _) <- withArea pTypedHole
  target <- pSourceTarget
  return $ Src.Source area target Src.TypedHole


-- | Parse a JS block expression
pJSBlockExpr :: Parser Src.Exp
pJSBlockExpr = do
  (area, content) <- withArea pJSBlock
  target <- pSourceTarget
  return $ Src.Source area target (Src.JSExp content)


-- If/Else --

pIf' :: Parser Src.Exp
pIf' = do
  (startArea, _) <- withArea pIf
  target <- pSourceTarget
  pLeftParen
  rets
  cond <- pExp
  maybeRet
  pRightParen
  maybeRet
  -- Try braced body first
  thenExpr <- choice
    [ try $ do
        pLeftCurly
        rets
        body <- pExp
        rets
        void $ withArea (void pRightCurly)
        return body
    , pExp
    ]
  -- Try to parse else clause; returns (bodyExp, endArea)
  elseResult <- optional $ try $ do
    maybeRet
    pElse
    maybeRet
    choice
      [ try $ do
          pLeftCurly
          rets
          body <- pExp
          rets
          (endArea, _) <- withArea (void pRightCurly)
          return (body, endArea)
      , do
          body <- pExp
          return (body, Src.getArea body)
      ]
  let (elseExpr', endArea) = case elseResult of
        Just (body, ea) -> (body, ea)
        Nothing         -> let e = Src.Source (Src.getArea thenExpr) target Src.LUnit
                           in  (e, Src.getArea thenExpr)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.If cond thenExpr elseExpr')


-- While --

pWhile' :: Parser Src.Exp
pWhile' = do
  (startArea, _) <- withArea pWhile
  target <- pSourceTarget
  pLeftParen
  rets
  cond <- pExp
  maybeRet
  pRightParen
  maybeRet
  body <- choice
    [ try $ do
        pLeftCurly
        rets
        e <- pExp
        rets
        (endArea, _) <- withArea (void pRightCurly)
        return (e, endArea)
    , do
        e <- pExp
        return (e, Src.getArea e)
    ]
  let (bodyExpr, endArea) = body
  return $ Src.Source (mergeAreas startArea endArea) target (Src.While cond bodyExpr)


-- Where (pattern matching) --

pWhere' :: Parser Src.Exp
pWhere' = do
  (startArea, _) <- withArea pWhere
  target <- pSourceTarget
  choice
    [ -- where(expr) { branches }
      try $ do
        pLeftParen
        rets
        expr <- pExp
        maybeRet
        pRightParen
        pLeftCurly
        maybeRet
        branches <- pIss
        maybeRet
        (endArea, _) <- withArea (void pRightCurly)
        return $ Src.Source (mergeAreas startArea endArea) target (Src.Where expr branches)
    , -- where { branches } (anonymous)
      do
        pLeftCurly
        rets
        branches <- pIss
        rets
        (endArea, _) <- withArea (void pRightCurly)
        return $ Src.Source (mergeAreas startArea endArea) target (Src.WhereAbs branches)
    ]


-- | Parse pattern match branches
pIss :: Parser [Src.Is]
pIss = do
  first <- pIs
  rest <- many $ try $ do
    rets
    pIs
  return $ first : rest
  where
    pIs = do
      pat <- pPattern
      target <- pSourceTarget
      pFatArrow
      rets
      body <- pExp
      return $ Src.Source (mergeAreas (Src.getArea pat) (Src.getArea body)) target (Src.Is pat body)


-- Do notation --

pDo' :: Parser Src.Exp
pDo' = do
  (startArea, _) <- withArea pDo
  target <- pSourceTarget
  pLeftCurly
  rets
  exprs <- pDoExps
  rets
  (endArea, _) <- withArea (void pRightCurly)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Do exprs)


pDoExps :: Parser [Src.Exp]
pDoExps = choice
  [ -- return exp
    try $ do
      (startArea, _) <- withArea pReturn
      target <- pSourceTarget
      body <- pExp
      return [Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Return body)]
  , -- empty
    try $ do
      lookAhead pRightCurly
      return [Src.Source emptyArea Src.TargetAll Src.LUnit]
  , -- name <- exp ; doExps
    try $ do
      (startArea, name) <- withArea pNameStr
      target <- pSourceTarget
      pLeftArrow
      body <- pExp
      rets
      rest <- pDoExps
      return $ Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.DoAssignment name body) : rest
  , -- bodyExp ; doExps
    do
      e <- pBodyExp
      rets
      rest <- pDoExps
      return $ e : rest
  ]


-- Pipe --

pPipe' :: Parser Src.Exp
pPipe' = do
  (startArea, _) <- withArea pPipe
  target <- pSourceTarget
  pLeftParen
  maybeRet
  exprs <- pArgs
  rets
  (endArea, _) <- withArea (void pRightParen)
  let pipeExpr = buildPipe (mergeAreas startArea endArea) target exprs
  -- Optionally followed by (args) for application
  applied <- optional $ try $ do
    pLeftParen
    args <- pArgsWithPlaceholder
    (appEndArea, _) <- withArea (void pRightParen)
    return (args, appEndArea)
  case applied of
    Nothing -> return pipeExpr
    Just (args, appEndArea) ->
      return $ Src.Source (mergeAreas startArea appEndArea) target (Src.App pipeExpr args)


buildPipe :: Area -> Src.SourceTarget -> [Src.Exp] -> Src.Exp
buildPipe area target exps = Src.Source area target (Src.Pipe exps)


-- Lambda / Parenthesized --

pAbsOrParenthesized :: Parser Src.Exp
pAbsOrParenthesized = do
  -- All alternatives start with '(' - parse it once
  (startArea, _) <- withArea (void pLeftParen)
  target <- pSourceTarget
  -- Peek at what follows to determine which form we have
  b <- lookAhead anySingle
  case chr8' b of
    ')' -> do
      -- () => exp (nullary lambda)
      pRightParen
      pFatArrow
      rets
      pLambdaBody startArea target []
    _ -> do
      rets
      -- Try to parse as lambda params or expression
      mResult <- optional $ try $ do
        params <- pParams
        rets
        void pRightParen
        pFatArrow
        rets
        return params
      case mResult of
        Just params -> do
          -- (params) => exp (multi-param lambda)
          pLambdaBody startArea target params
        Nothing -> do
          -- Either (name) => exp, (name), or (exp)
          inner <- pExp
          rets
          (parenEndArea, _) <- withArea (void pRightParen)
          -- Check if this looks like a single-name lambda
          case inner of
            Src.Source nameArea _ (Src.Var name) -> do
              -- Could be (name) => exp or just (name)
              mLambda <- optional $ try $ do
                pFatArrow
                rets
                return ()
              case mLambda of
                Just _ -> do
                  let param = Src.Source nameArea target name
                  pLambdaBody startArea target [param]
                Nothing ->
                  return $ Src.Source (mergeAreas startArea parenEndArea) target (Src.Parenthesized startArea inner parenEndArea)
            _ ->
              return $ Src.Source (mergeAreas startArea parenEndArea) target (Src.Parenthesized startArea inner parenEndArea)


-- | Parse lambda body after '=>' has been consumed (or we're about to)
pLambdaBody :: Area -> Src.SourceTarget -> [Src.Source Src.Name] -> Parser Src.Exp
pLambdaBody startArea target params = choice
  [ try $ do
      pLeftCurly
      rets
      body <- pMultiExpBody
      rets
      (endArea, _) <- withArea (void pRightCurly)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.AbsWithMultilineBody params body)
  , do
      body <- pBodyExp
      return $ Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Abs params [body])
  ]




-- | Parse lambda parameters (comma-separated names, at least two)
pParams :: Parser [Src.Source Src.Name]
pParams = do
  first <- pParam
  pComma
  rets
  rest <- pParam `sepBy1` (pComma <* rets)
  return $ first : rest
  where
    pParam = do
      (area, name) <- withArea pNameStr
      target <- pSourceTarget
      return $ Src.Source area target name


-- Record --

pRecord :: Parser Src.Exp
pRecord = do
  (startArea, _) <- withArea (void pLeftCurly)
  target <- pSourceTarget
  rets
  choice
    [ -- Unit literal: {} (empty braces = LUnit)
      try $ do
        (endArea, _) <- withArea (void pRightCurly)
        return $ Src.Source (mergeAreas startArea endArea) target Src.LUnit
    , -- Spread record: { ...expr, fields }
      try $ do
        pSpread
        spreadExpr <- pExp
        fields <- option [] $ try $ do
          pComma
          rets
          pRecordFields
        _ <- optional pComma
        rets
        (endArea, _) <- withArea (void pRightCurly)
        let spreadField = Src.Source (mergeAreas startArea (Src.getArea spreadExpr)) target (Src.FieldSpread spreadExpr)
        return $ Src.Source (mergeAreas startArea endArea) target (Src.Record (spreadField : fields))
    , -- Regular record: { field: val, ... }
      do
        fields <- pRecordFields
        _ <- optional pComma
        rets
        (endArea, _) <- withArea (void pRightCurly)
        return $ Src.Source (mergeAreas startArea endArea) target (Src.Record fields)
    ]


pRecordFields :: Parser [Src.Field]
pRecordFields = option [] $ do
  first <- pRecordField
  rest <- many $ try $ do
    rets
    pComma
    rets
    pRecordField
  return $ first : rest
  where
    pRecordField = choice
      [ -- name: exp
        try $ do
          (nameArea, name) <- withArea pNameStr
          target <- pSourceTarget
          pColon
          value <- pExp
          return $ Src.Source (mergeAreas nameArea (Src.getArea value)) target (Src.Field (name, value))
      , -- shorthand: just name
        do
          (nameArea, name) <- withArea pNameStr
          target <- pSourceTarget
          return $ Src.Source nameArea target (Src.FieldShorthand name)
      ]


-- Dictionary --

pDict :: Parser Src.Exp
pDict = do
  (startArea, _) <- withArea (void pLeftDoubleCurly)
  target <- pSourceTarget
  rets
  items <- option [] pDictItems
  _ <- optional pComma
  rets
  pRightCurly
  (endArea, _) <- withArea (void pRightCurly)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Dictionary items)


pDictItems :: Parser [Src.DictItem]
pDictItems = do
  first <- pDictItem
  rest <- many $ try $ do
    rets
    pComma
    rets
    pDictItem
  return $ first : rest
  where
    pDictItem = do
      key <- pExp
      target <- pSourceTarget
      pColon
      value <- pExp
      return $ Src.Source (mergeAreas (Src.getArea key) (Src.getArea value)) target (Src.DictItem key value)


-- List Constructor --

pListConstructor :: Parser Src.Exp
pListConstructor = do
  (startArea, _) <- withArea (void pLeftSquareBracket)
  target <- pSourceTarget
  rets
  items <- pListItems
  _ <- optional pComma
  rets
  (endArea, _) <- withArea (void pRightSquareBracket)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.ListConstructor items)


pListItems :: Parser [Src.ListItem]
pListItems = option [] $ do
  first <- pListItemOrSpread
  rest <- many $ try $ do
    rets
    pComma
    rets
    pListItemOrSpread
  return $ first : rest
  where
    pListItemOrSpread = choice
      [ try $ do
          (startArea, _) <- withArea (void pSpread)
          target <- pSourceTarget
          e <- pExp
          return $ Src.Source (mergeAreas startArea (Src.getArea e)) target (Src.ListSpread e)
      , do
          e <- pExp
          target <- pSourceTarget
          return $ Src.Source (Src.getArea e) target (Src.ListItem e)
      ]


-- Tuple Constructor --

pTupleConstructor :: Parser Src.Exp
pTupleConstructor = do
  (startArea, _) <- withArea (void pTupleStart)
  target <- pSourceTarget
  rets
  items <- pTupleItems
  _ <- optional pComma
  rets
  (endArea, _) <- withArea (void pRightSquareBracket)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TupleConstructor items)


pTupleItems :: Parser [Src.Exp]
pTupleItems = do
  first <- pExp
  rest <- some $ try $ do
    rets
    pComma
    rets
    pExp
  return $ first : rest


-- Template String --

pTemplateString :: Parser Src.Exp
pTemplateString = do
  (startArea, _) <- withArea (void $ lexeme $ C.char 96)  -- '`'
  target <- pSourceTarget
  -- Collect alternating: string-part, expression, string-part, expression, ..., final-string
  parts <- collectParts
  (endArea, endStr) <- withArea pTemplateEnd
  let lastPart = Src.Source endArea target (Src.LStr endStr)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TemplateString (parts ++ [lastPart]))
  where
    -- Collect all parts: each iteration produces a string LStr + an expression
    collectParts :: Parser [Src.Exp]
    collectParts = do
      sections <- many $ try $ do
        strStart <- getLoc
        rawParts <- manyTill pTemplateRawChunk (lookAhead (C.string "${"))
        str <- processEscapesInTemplate (concat rawParts)
        strEnd <- getLoc
        let strArea = Area strStart strEnd
        void $ C.string "${"
        scn
        target' <- pSourceTarget
        expr <- pExp
        scn
        void $ satisfy (== 125)  -- '}'
        let strPart = Src.Source strArea target' (Src.LStr str)
        return [strPart, expr]
      return $ concat sections

    pTemplateEnd :: Parser String
    pTemplateEnd = do
      raw <- manyTill pTemplateRawChunk (satisfy (== 96))  -- '`'
      processEscapesInTemplate (concat raw)

    pTemplateRawChunk :: Parser String
    pTemplateRawChunk = choice
      [ try $ C.string "\\`" >> return ['`']
      , try $ do
          void $ satisfy (== 92)  -- '\\'
          b <- anySingle
          case chr8' b of
            'u' -> do
              rest <- try (do
                  void $ satisfy (== 123)  -- '{'
                  hex <- map chr8' <$> many (satisfy isHexDigitB)
                  void $ satisfy (== 125)  -- '}'
                  return $ '{' : hex ++ "}")
                <|> (map chr8' <$> count 4 (satisfy isHexDigitB))
              return $ '\\' : 'u' : rest
            'x' -> do
              hex <- map chr8' <$> count 2 (satisfy isHexDigitB)
              return $ '\\' : 'x' : hex
            ch -> return ['\\', ch]
      , T.unpack . TE.decodeUtf8 <$> takeWhile1P Nothing (\b -> b /= 96 && b /= 92 && b /= 36)  -- not '`', '\\', or '$'
      , try $ do
          void $ satisfy (== 36)  -- '$' not followed by '{'
          notFollowedBy (satisfy (== 123))
          return "$"
      ]

    processEscapesInTemplate :: String -> Parser String
    processEscapesInTemplate raw = case processEscapes raw of
      Left err      -> fail err
      Right content -> return content


-- Typed Expression --

pTypedExp :: Parser Src.Exp
pTypedExp = do
  void pLeftParen
  target <- pSourceTarget
  inner <- choice
    [ try $ do
        (nameArea, name) <- withArea pNameStr
        pDoubleColon
        t <- pTypings
        return (Src.Source nameArea target (Src.Var name), t)
    , do
        e <- pExp
        pDoubleColon
        t <- pTypings
        return (e, t)
    ]
  pRightParen
  let (expr, typing) = inner
  return $ Src.Source (mergeAreas (Src.getArea expr) (Src.getArea typing)) target (Src.TypedExp expr typing)


-- JSX --

pJsxTag :: Parser Src.Exp
pJsxTag = choice
  [ -- Self-closing: <Name props />
    try $ do
      (startArea, _) <- withArea (void $ lexeme $ C.char 60)  -- '<'
      target <- pSourceTarget
      name <- pNameStr
      props <- pJsxProps
      scn
      void $ lexeme $ C.char 47   -- '/'
      (endArea, _) <- withArea (void $ lexeme $ C.char 62)    -- '>'
      return $ Src.Source (mergeAreas startArea endArea) target (Src.JsxAutoClosedTag name props)
  , -- Opening/closing: <Name props>children</Name>
    try $ do
      (startArea, _) <- withArea (void $ lexeme $ C.char 60)  -- '<'
      target <- pSourceTarget
      name <- pNameStr
      props <- pJsxProps
      scn
      void $ lexeme $ C.char 62   -- '>'
      children <- pJsxChildren
      void $ lexeme $ C.string "</"
      void $ lexeme $ C.string (C8.pack name)
      (endArea, _) <- withArea (void $ lexeme $ C.char 62)    -- '>'
      return $ Src.Source (mergeAreas startArea endArea) target (Src.JsxTag name props children)
  ]


pJsxProps :: Parser [Src.JsxProp]
pJsxProps = many (try (scn *> pJsxProp))
  where
    pJsxProp = choice
      [ -- name="string"
        try $ do
          (nameArea, name) <- withArea pNameStr
          target <- pSourceTarget
          pEq
          (strArea, str) <- withArea pStringLiteral
          return $ Src.Source (mergeAreas nameArea strArea) target (Src.JsxProp name (Src.Source strArea target (Src.LStr str)))
      , -- name={expr}
        try $ do
          (nameArea, name) <- withArea pNameStr
          target <- pSourceTarget
          pEq
          pLeftCurly
          expr <- pExp
          (endArea, _) <- withArea (void pRightCurly)
          return $ Src.Source (mergeAreas nameArea endArea) target (Src.JsxProp name expr)
      , -- name (boolean shorthand)
        do
          (nameArea, name) <- withArea pNameStr
          target <- pSourceTarget
          return $ Src.Source nameArea target (Src.JsxProp name (Src.Source nameArea target (Src.LBool "true")))
      ]


pJsxChildren :: Parser [Src.JsxChild]
pJsxChildren = scn *> many (pJsxChild <* scn)
  where
    pJsxChild = choice
      [ -- {...expr} (must try before {expr} since both start with {)
        try $ do
          pLeftCurly
          scn
          pSpread
          expr <- pExp
          scn
          pRightCurly
          return $ Src.JsxSpreadChild expr
      , -- {expr}
        try $ do
          pLeftCurly
          scn
          expr <- pExp
          scn
          pRightCurly
          return $ Src.JsxExpChild expr
      , -- Nested JSX tag
        try pJsxTag >>= \tag -> return (Src.JsxChild tag)
      , -- Text content (whitespace-only is skipped)
        try $ do
          text <- C8.unpack <$> takeWhile1P Nothing (\b -> b /= 60 && b /= 62 && b /= 123 && b /= 125)
          let trimmed = C8.unpack $ C8.strip $ C8.pack text
          if null trimmed then fail "empty text"
          else return $ Src.JsxChild (Src.Source emptyArea Src.TargetAll (Src.LStr $ "\"" ++ trimmed ++ "\""))
      ]


-- Arguments --

-- | Parse arguments with placeholder ($) support
pArgsWithPlaceholder :: Parser [Src.Exp]
pArgsWithPlaceholder = do
  first <- pArgOrPlaceholder
  rest <- many $ try $ do
    rets
    pComma
    rets
    pArgOrPlaceholder
  _ <- optional pComma
  return $ first : rest
  where
    pArgOrPlaceholder = choice
      [ try $ do
          (area, _) <- withArea pDollar
          target <- pSourceTarget
          return $ Src.Source area target (Src.Var "$")
      , pExp
      ]


-- | Parse regular arguments (no placeholders)
pArgs :: Parser [Src.Exp]
pArgs = do
  first <- pExp
  rest <- many $ try $ do
    rets
    pComma
    rets
    pExp
  _ <- optional pComma
  return $ first : rest
