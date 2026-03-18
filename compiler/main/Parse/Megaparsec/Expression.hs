{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Expression
  ( pExp
  , pBodyExp
  , pMultiExpBody
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import qualified Data.Map                       as M
import           Data.Char                      ( isUpper, isDigit, isAsciiLower, isAsciiUpper )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( void, when )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec.Char           as C
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
  case mc of
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


-- | Operator precedence table for makeExprParser.
-- NOTE: makeExprParser treats FIRST entry as HIGHEST precedence (opposite of docs).
-- This order matches the Happy grammar's %left declarations (highest to lowest).
operatorTable :: [[Operator Parser Src.Exp]]
operatorTable =
  [ -- Highest precedence: * / %
    [ InfixL $ try $ do
        (area, _) <- withArea pStar
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "*")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pSlash
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "/")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pPercent
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "%")) r)
    ]
  , -- + ++ -
    [ InfixL $ try $ do
        (area, _) <- withArea pPlus
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "+")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pDoublePlus
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "++")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pDash
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "-")) r)
    ]
  , -- == != > < >= <=
    [ InfixL $ try $ do
        (area, _) <- withArea pDoubleEq
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "==")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pNotEq
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "!=")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pRightChevron
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var ">")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pLeftChevron
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "<")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pRightChevronEq
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var ">=")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pLeftChevronEq
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "<=")) r)
    ]
  , -- &&
    [ InfixL $ try $ do
        (area, _) <- withArea pDoubleAmpersand
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "&&")) r)
    ]
  , -- ||
    [ InfixL $ try $ do
        (area, _) <- withArea pDoublePipe
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "||")) r)
    ]
  , -- |>
    [ InfixL $ try $ do
        (area, _) <- withArea pPipeOp
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "|>")) r)
    ]
  , -- <|>
    [ InfixL $ try $ do
        (area, _) <- withArea pAlternativeOp
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "<|>")) r)
    ]
  , -- Lowest precedence: bitwise operators (same level as ':' '->' '|' 'else' '=' in Happy)
    [ InfixL $ try $ do
        (area, _) <- withArea pPipeChar
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "|")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pAmpersand
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "&")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pXor
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "^")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pDoubleLeftChevron
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var "<<")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pTripleRightChevron
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var ">>>")) r)
    , InfixL $ try $ do
        (area, _) <- withArea pDoubleRightChevron
        target <- pSourceTarget
        maybeRet
        return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.BinOp l (Src.Source area target (Src.Var ">>")) r)
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
      case mc of
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
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) (Src.getSourceTarget e) (Src.Access e (Src.Source nameArea target (Src.Var $ "." ++ name)))
          applyPostfix (f expr)
        Just '[' -> do
          f <- try $ do
            pLeftSquareBracket
            idx <- pExp
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
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) (Src.getSourceTarget e) (Src.Access e (Src.Source nameArea target (Src.Var $ "." ++ name)))
          case f of
            Nothing -> return expr
            Just fn -> applyPostfix (fn expr)
        _ -> return expr


-- | Parse a term (primary expression without operators)
-- Uses lookAhead dispatch to avoid unnecessary backtracking.
pTerm :: Parser Src.Exp
pTerm = do
  c <- lookAhead anySingle
  case c of
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
    '{' -> choice [try pRecord, try pDict, pLiteral]
    '"' -> pLiteral
    '\'' -> pLiteral
    _ | isDigit c -> pLiteral
      | c == 'i' -> do
          ms <- optional (lookAhead (count 2 anySingle))
          case ms of
            Just ['i','f'] -> choice [pIf', pNameExpr]
            _              -> pNameExpr
      | c == 'w' -> do
          ms <- optional (lookAhead (count 2 anySingle))
          case ms of
            Just ['w','h'] -> choice [pWhile', pWhere', pNameExpr]
            _              -> pNameExpr
      | c == 'd' -> do
          ms <- optional (lookAhead (count 2 anySingle))
          case ms of
            Just ['d','o'] -> choice [pDo', pNameExpr]
            _              -> pNameExpr
      | c == 'p' -> do
          ms <- optional (lookAhead (count 2 anySingle))
          case ms of
            Just ['p','i'] -> choice [pPipe', pNameExpr]
            _              -> pNameExpr
      | c == 't' || c == 'f' -> choice [pLiteral, pNameExpr]
      | isAsciiLower c || c == '_' -> pNameExpr
      | isAsciiUpper c -> pNameExpr
      | otherwise -> pNameExpr


-- | Parse a prefix expression: !, ~, or unary minus applied to a term
pPrefixExp :: Parser Src.Exp
pPrefixExp = choice
  [ do
      (area, _) <- withArea pExclamationMark
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "!")) e)
  , do
      (area, _) <- withArea pTilde
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "~")) e)
  , do
      (area, _) <- withArea pDashUnary
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "unary-minus")) e)
  ]


-- Literals --

pLiteral :: Parser Src.Exp
pLiteral = do
  c <- lookAhead anySingle
  case c of
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


-- | Parse all numeric literal variants, dispatching on prefix
pNumericLiteral :: Parser Src.Exp
pNumericLiteral = choice
  [ try $ do
      (area, s) <- withArea pHexByte
      target <- pSourceTarget
      return $ Src.Source area target (Src.LByte (init . init $ s))
  , try $ do
      (area, s) <- withArea pHexShort
      target <- pSourceTarget
      return $ Src.Source area target (Src.LShort (init . init $ s))
  , try $ do
      (area, s) <- withArea pHexInt
      target <- pSourceTarget
      return $ Src.Source area target (Src.LInt (init . init $ s))
  , try $ do
      (area, s) <- withArea pHexNumber
      target <- pSourceTarget
      return $ Src.Source area target (Src.LNum s)
  , try $ do
      (area, s) <- withArea pByte
      target <- pSourceTarget
      return $ Src.Source area target (Src.LByte s)
  , try $ do
      (area, s) <- withArea pShort
      target <- pSourceTarget
      return $ Src.Source area target (Src.LShort s)
  , try $ do
      (area, s) <- withArea pInt
      target <- pSourceTarget
      return $ Src.Source area target (Src.LInt s)
  , try $ do
      (area, s) <- withArea pFloat
      target <- pSourceTarget
      return $ Src.Source area target (Src.LFloat s)
  , do
      (area, s) <- withArea pNumber
      target <- pSourceTarget
      return $ Src.Source area target (Src.LNum s)
  ]


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
        maybeRet
        body <- pExp
        maybeRet
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
          maybeRet
          body <- pExp
          maybeRet
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
  c <- lookAhead anySingle
  case c of
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
  (startArea, _) <- withArea (void $ lexeme $ C.char '`')
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
        void $ C.char '}'
        let strPart = Src.Source strArea target' (Src.LStr str)
        return [strPart, expr]
      return $ concat sections

    pTemplateEnd :: Parser String
    pTemplateEnd = do
      raw <- manyTill pTemplateRawChunk (C.char '`')
      processEscapesInTemplate (concat raw)

    pTemplateRawChunk :: Parser String
    pTemplateRawChunk = choice
      [ try $ C.string "\\`" >> return ['`']
      , try $ do
          void $ C.char '\\'
          c <- anySingle
          case c of
            'u' -> do
              rest <- try (do
                  void $ C.char '{'
                  hex <- many C.hexDigitChar
                  void $ C.char '}'
                  return $ '{' : hex ++ "}")
                <|> count 4 C.hexDigitChar
              return $ '\\' : 'u' : rest
            'x' -> do
              hex <- count 2 C.hexDigitChar
              return $ '\\' : 'x' : hex
            _ -> return ['\\', c]
      , (:[]) <$> satisfy (\c -> c /= '`' && c /= '\\')
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
      (startArea, _) <- withArea (void $ lexeme $ C.char '<')
      target <- pSourceTarget
      name <- pNameStr
      props <- pJsxProps
      scn
      void $ lexeme $ C.char '/'
      (endArea, _) <- withArea (void $ lexeme $ C.char '>')
      return $ Src.Source (mergeAreas startArea endArea) target (Src.JsxAutoClosedTag name props)
  , -- Opening/closing: <Name props>children</Name>
    try $ do
      (startArea, _) <- withArea (void $ lexeme $ C.char '<')
      target <- pSourceTarget
      name <- pNameStr
      props <- pJsxProps
      scn
      void $ lexeme $ C.char '>'
      children <- pJsxChildren
      void $ lexeme $ C.string "</"
      void $ lexeme $ C.string (T.pack name)
      (endArea, _) <- withArea (void $ lexeme $ C.char '>')
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
          text <- T.unpack <$> takeWhile1P Nothing (\c -> c /= '<' && c /= '>' && c /= '{' && c /= '}')
          let trimmed = T.unpack $ T.strip $ T.pack text
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


