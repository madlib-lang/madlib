{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Expression
  ( pExp
  , pBodyExp
  , pMultiExpBody
  ) where

import           Control.Monad                  ( void, when )

import           Text.Megaparsec                hiding ( State, Token )
import           Control.Monad.Combinators.Expr

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Escape        ( processEscapes )
import           Parse.Megaparsec.Lexeme
import           Parse.Megaparsec.Pattern
import           Parse.Megaparsec.Typing
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )


-- Helper: peek at the next token kind (Nothing at EOF)
peekTok :: Parser (Maybe Token)
peekTok = fmap (fmap rtToken) (optional (lookAhead anySingle))


-- | Parse a body expression (assignment, mutation, typed expression, or plain expression)
-- bodyExp = name '=' exp | name '::' constrainedTyping '\n' name '=' exp | exp ':=' exp | exp
pBodyExp :: Parser Src.Exp
pBodyExp = do
  -- Peek at the next token to determine dispatch
  mt <- peekTok
  case mt of
    -- Could be: name = exp | name :: typing \n name = exp | plain exp
    -- Handles TkName (lowercase), TkTypeName (uppercase/constant), and soft keywords
    Just (TkName _)     -> tryNameAssignment
    Just (TkTypeName _) -> tryNameAssignment
    Just TkWhen         -> tryNameAssignment
    Just TkIs           -> tryNameAssignment
    Just TkNot          -> tryNameAssignment
    _ -> pBodyExpNoName
  where
    tryNameAssignment = do
      mResult <- optional $ try $ do
        (startArea, name1) <- withArea pName
        target <- pSourceTarget
        mt2 <- peekTok
        case mt2 of
          Just TkDoubleColon -> do
            pDoubleColon
            typing <- pConstrainedTyping
            rets
            pNamedTypedBodyExpBody startArea target name1 typing
          Just TkEq -> do
            pEq
            maybeRet
            body <- pExp
            return $ Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Assignment name1 body)
          _ -> empty
      case mResult of
        Just e  -> return e
        Nothing -> pBodyExpNoName

    -- After name :: typing, parse either export extern | export assignment | extern | assignment
    pNamedTypedBodyExpBody startArea target name1 typing = do
      exported <- option False (True <$ lookAhead pExport)
      if exported
        then do
          pExport
          name2 <- pName
          pEq
          isExtern <- option False (True <$ lookAhead pExtern)
          if isExtern
            then do
              pExtern
              (endArea, path) <- withArea pStringLiteral
              let externExpr = Src.Source (mergeAreas startArea endArea) target (Src.Extern typing name2 path)
              return $ Src.Source (mergeAreas startArea endArea) target (Src.Export externExpr)
            else do
              maybeRet
              body <- pExp
              let assignArea = mergeAreas startArea (Src.getArea body)
              return $ Src.Source assignArea target
                (Src.NamedTypedExp name1
                  (Src.Source assignArea target
                    (Src.Export (Src.Source assignArea target (Src.Assignment name2 body))))
                  typing)
        else do
          name2 <- pName
          pEq
          isExtern <- option False (True <$ lookAhead pExtern)
          if isExtern
            then do
              pExtern
              (endArea, path) <- withArea pStringLiteral
              return $ Src.Source (mergeAreas startArea endArea) target (Src.Extern typing name2 path)
            else do
              rets
              body <- pExp
              let assignArea = mergeAreas startArea (Src.getArea body)
              return $ Src.Source assignArea target (Src.NamedTypedExp name1 (Src.Source assignArea target (Src.Assignment name2 body)) typing)

    -- exp := exp or plain expression
    pBodyExpNoName = choice
      [ try $ do
          lhs <- pExp
          target <- pSourceTarget
          pMutateEq
          maybeRet
          rhs <- pExp
          return $ Src.Source (mergeAreas (Src.getArea lhs) (Src.getArea rhs)) target (Src.Mutate lhs rhs)
      , pExp
      ]


-- | Parse a multi-expression body (used in multiline lambdas and do blocks)
pMultiExpBody :: Parser [Src.Exp]
pMultiExpBody = choice
  [ do
      (startArea, _) <- withArea pReturn
      target <- pSourceTarget
      body <- pExp
      return [Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Return body)]
  , do
      lookAhead pRightCurly
      return [Src.Source emptyArea Src.TargetAll Src.LUnit]
  , do
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
  mt <- peekTok
  case mt of
    Just TkQuestionMark -> do
      pQuestionMark
      maybeRet
      trueExpr <- pExp
      maybeRet
      pColon
      maybeRet
      falseExpr <- pExp
      return $ Src.Source (mergeAreas (Src.getArea e) (Src.getArea falseExpr)) (Src.getSourceTarget e) (Src.Ternary e trueExpr falseExpr)
    Just TkNewline -> option e $ try $ do
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
pExprWithOps :: Parser Src.Exp
pExprWithOps = makeExprParser pTermWithNewline operatorTable
  where
    pTermWithNewline = pTermWithPostfix <* maybeRet


-- | Build a BinOp node from an operator string and its area.
{-# INLINE mkBinOp #-}
mkBinOp :: Area -> Src.SourceTarget -> String -> Src.Exp -> Src.Exp -> Src.Exp
mkBinOp area target opStr l r =
  Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target
    (Src.BinOp l (Src.Source area target (Src.Var opStr)) r)


-- | Operator precedence table for makeExprParser.
operatorTable :: [[Operator Parser Src.Exp]]
operatorTable =
  [ -- Highest: * / %
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkStar    -> do (area, _) <- withArea pStar;    target <- pSourceTarget; maybeRet; return $ mkBinOp area target "*"
          Just TkSlash   -> do (area, _) <- withArea pSlash;   target <- pSourceTarget; maybeRet; return $ mkBinOp area target "/"
          Just TkPercent -> do (area, _) <- withArea pPercent; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "%"
          _              -> empty
    ]
  , -- + ++ -
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkDoublePlus -> do (area, _) <- withArea pDoublePlus; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "++"
          Just TkPlus       -> do (area, _) <- withArea pPlus;       target <- pSourceTarget; maybeRet; return $ mkBinOp area target "+"
          Just TkDash       -> do (area, _) <- withArea pDash;       target <- pSourceTarget; maybeRet; return $ mkBinOp area target "-"
          _                 -> empty
    ]
  , -- == != >= > <= <
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkDoubleEq      -> do (area, _) <- withArea pDoubleEq;      target <- pSourceTarget; maybeRet; return $ mkBinOp area target "=="
          Just TkNotEq         -> do (area, _) <- withArea pNotEq;         target <- pSourceTarget; maybeRet; return $ mkBinOp area target "!="
          Just TkRightChevronEq -> do (area, _) <- withArea pRightChevronEq; target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">="
          Just TkRightChevron  -> do (area, _) <- withArea pRightChevron;  target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">"
          Just TkLeftChevronEq -> do (area, _) <- withArea pLeftChevronEq; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<="
          Just TkLeftChevron   -> do (area, _) <- withArea pLeftChevron;   target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<"
          _                    -> empty
    ]
  , -- &&
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkDoubleAmpersand -> do (area, _) <- withArea pDoubleAmpersand; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "&&"
          _                      -> empty
    ]
  , -- ||
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkDoublePipe -> do (area, _) <- withArea pDoublePipe; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "||"
          _                 -> empty
    ]
  , -- ??
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkDoubleQuestionMark -> do
            (_, _) <- withArea pDoubleQuestionMark
            target <- pSourceTarget
            maybeRet
            return $ \l r -> Src.Source (mergeAreas (Src.getArea l) (Src.getArea r)) target (Src.MaybeDefault l r)
          _ -> empty
    ]
  , -- |>
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkPipeOp -> do (area, _) <- withArea pPipeOp; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "|>"
          _             -> empty
    ]
  , -- <|>
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkAlternativeOp -> do (area, _) <- withArea pAlternativeOp; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<|>"
          _                    -> empty
    ]
  , -- Bitwise: | & ^ << >> >>>
    [ InfixL $ try $ do
        mt <- peekTok
        case mt of
          Just TkPipeChar            -> do (area, _) <- withArea pPipeChar;          target <- pSourceTarget; maybeRet; return $ mkBinOp area target "|"
          Just TkAmpersand           -> do (area, _) <- withArea pAmpersand;         target <- pSourceTarget; maybeRet; return $ mkBinOp area target "&"
          Just TkXor                 -> do (area, _) <- withArea pXor;               target <- pSourceTarget; maybeRet; return $ mkBinOp area target "^"
          Just TkDoubleLeftChevron   -> do (area, _) <- withArea pDoubleLeftChevron; target <- pSourceTarget; maybeRet; return $ mkBinOp area target "<<"
          Just TkTripleRightChevron  -> do (area, _) <- withArea pTripleRightChevron; target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">>>"
          Just TkDoubleRightChevron  -> do (area, _) <- withArea pDoubleRightChevron; target <- pSourceTarget; maybeRet; return $ mkBinOp area target ">>"
          _                          -> empty
    ]
  ]


-- | Parse a term with optional postfix operations
pTermWithPostfix :: Parser Src.Exp
pTermWithPostfix = do
  base <- pTerm
  applyPostfix base
  where
    applyPostfix :: Src.Exp -> Parser Src.Exp
    applyPostfix expr = do
      mt <- peekTok
      case mt of
        Just TkLeftParen -> do
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
        Just TkQuestionDot -> do
          mf <- optional $ try $ do
            pQuestionDot
            (nameArea, name) <- withArea pName
            target <- pSourceTarget
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) target
              (Src.OptionalAccess e (Src.Source nameArea target (Src.Var $ '.' : name)))
          case mf of
            Just f  -> applyPostfix (f expr)
            Nothing -> return expr
        Just TkDot -> do
          f <- try $ do
            pDot
            (nameArea, name) <- withArea pName
            target <- pSourceTarget
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) (Src.getSourceTarget e) (Src.Access e (Src.Source nameArea target (Src.Var $ '.' : name)))
          applyPostfix (f expr)
        Just TkLeftSquare -> do
          f <- try $ do
            pLeftSquareBracket
            rets
            idx <- pExp
            rets
            (endArea, _) <- withArea (void pRightSquareBracket)
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) endArea) (Src.getSourceTarget e) (Src.ArrayAccess e idx)
          applyPostfix (f expr)
        Just TkNewline -> do
          -- Method chaining: check if next non-whitespace is '.'
          f <- optional $ try $ do
            maybeRet
            pDot
            (nameArea, name) <- withArea pName
            target <- pSourceTarget
            return $ \e -> Src.Source (mergeAreas (Src.getArea e) nameArea) (Src.getSourceTarget e) (Src.Access e (Src.Source nameArea target (Src.Var $ '.' : name)))
          case f of
            Nothing -> return expr
            Just fn -> applyPostfix (fn expr)
        _ -> return expr


-- | Parse a term (primary expression without operators)
pTerm :: Parser Src.Exp
pTerm = do
  mt <- peekTok
  case mt of
    Just TkTypedHole           -> pTypedHoleExpr
    Just (TkJSBlock _)         -> pJSBlockExpr
    Just TkTupleStart          -> pTupleConstructor
    Just TkSharp               -> choice [pJSBlockExpr, pTupleConstructor, try pHashName]
    Just TkLeftSquare          -> pListConstructor
    Just (TkTemplateStringFull _)  -> pTemplateString
    Just (TkTemplateStringStart _) -> pTemplateString
    Just TkLeftParen           -> choice [try pAbsOrParenthesized, try pTypedExp]
    Just TkLeftChevron         -> try pJsxTag
    Just TkDot                 -> try pDotName
    Just TkExclamation         -> pPrefixExp
    Just TkTilde               -> pPrefixExp
    Just TkDash                -> pPrefixExp
    Just TkLeftDoubleCurly     -> pDict
    Just TkLeftCurly           -> choice [try pRecord, pLiteral]
    Just (TkString _)          -> pLiteral
    Just (TkChar _)            -> pLiteral
    Just (TkInt _)             -> pLiteral
    Just (TkFloat _)           -> pLiteral
    Just (TkByte _)            -> pLiteral
    Just (TkShort _)           -> pLiteral
    Just (TkHexNumber _)       -> pLiteral
    Just (TkHexByte _)         -> pLiteral
    Just (TkHexShort _)        -> pLiteral
    Just (TkHexInt _)          -> pLiteral
    Just TkTrue                -> pLiteral
    Just TkFalse               -> pLiteral
    Just TkIf                  -> pIf'
    Just TkWhile               -> pWhile'
    Just TkWhere               -> pWhere'
    Just TkDo                  -> pDo'
    Just TkPipe                -> pPipe'
    _                          -> pNameExpr


-- | Parse a prefix expression: !, ~, or unary minus applied to a term
pPrefixExp :: Parser Src.Exp
pPrefixExp = do
  mt <- peekTok
  case mt of
    Just TkExclamation -> do
      (area, _) <- withArea pExclamationMark
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "!")) e)
    Just TkTilde -> do
      (area, _) <- withArea pTilde
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "~")) e)
    _ -> do
      (area, _) <- withArea pDashUnary
      target <- pSourceTarget
      e <- pTermWithPostfix
      return $ Src.Source (mergeAreas area (Src.getArea e)) target (Src.UnOp (Src.Source area target (Src.Var "unary-minus")) e)


-- Literals --

pLiteral :: Parser Src.Exp
pLiteral = do
  mt <- peekTok
  case mt of
    Just TkLeftCurly -> do
      -- Unit literal: {}
      (startArea, _) <- withArea (void pLeftCurly)
      (endArea, _) <- withArea (void pRightCurly)
      target <- pSourceTarget
      return $ Src.Source (mergeAreas startArea endArea) target Src.LUnit
    Just (TkString _) -> do
      (area, s) <- withArea pStringLiteral
      target <- pSourceTarget
      return $ Src.Source area target (Src.LStr s)
    Just (TkChar _) -> do
      (area, ch) <- withArea pCharLiteral
      target <- pSourceTarget
      return $ Src.Source area target (Src.LChar ch)
    Just TkTrue -> do
      (area, _) <- withArea pBoolTrue
      target <- pSourceTarget
      return $ Src.Source area target (Src.LBool "true")
    Just TkFalse -> do
      (area, _) <- withArea pBoolFalse
      target <- pSourceTarget
      return $ Src.Source area target (Src.LBool "false")
    _ -> pNumericLiteral


-- | Parse all numeric literal variants using token-based dispatch.
pNumericLiteral :: Parser Src.Exp
pNumericLiteral = do
  (area, (target, node)) <- withArea $ do
    t <- pSourceTarget
    n <- parseNum
    return (t, n)
  return $ Src.Source area target node
  where
    parseNum :: Parser Src.Exp_
    parseNum = choice
      [ Src.LByte  <$> pHexByte
      , Src.LShort <$> pHexShort
      , Src.LInt   <$> pHexInt
      , Src.LNum   <$> pHexNumber
      , Src.LByte  <$> pByte
      , Src.LShort <$> pShort
      , Src.LFloat <$> pFloat
      , Src.LNum   <$> pNumber
      ]


-- | Parse a variable name expression
pNameExpr :: Parser Src.Exp
pNameExpr = do
  (area, name) <- withArea pName
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
  (endArea, name) <- withArea pName
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
  thenExpr <- do
    mt <- peekTok
    case mt of
      Just TkLeftCurly -> do
        pLeftCurly
        rets
        body <- pExp
        rets
        void $ withArea (void pRightCurly)
        return body
      _ -> pExp
  elseResult <- optional $ try $ do
    maybeRet
    pElse
    maybeRet
    mt2 <- peekTok
    case mt2 of
      Just TkLeftCurly -> do
        pLeftCurly
        rets
        body <- pExp
        rets
        (endArea, _) <- withArea (void pRightCurly)
        return (body, endArea)
      _ -> do
        body <- pExp
        return (body, Src.getArea body)
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
  body <- do
    mt <- peekTok
    case mt of
      Just TkLeftCurly -> do
        pLeftCurly
        rets
        e <- pExp
        rets
        (endArea, _) <- withArea (void pRightCurly)
        return (e, endArea)
      _ -> do
        e <- pExp
        return (e, Src.getArea e)
  let (bodyExpr, endArea) = body
  return $ Src.Source (mergeAreas startArea endArea) target (Src.While cond bodyExpr)


-- Where (pattern matching) --

pWhere' :: Parser Src.Exp
pWhere' = do
  (startArea, _) <- withArea pWhere
  target <- pSourceTarget
  mt <- peekTok
  case mt of
    Just TkLeftParen -> do
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
    _ -> do
      pLeftCurly
      rets
      branches <- pIss
      rets
      (endArea, _) <- withArea (void pRightCurly)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.WhereAbs branches)


-- | Parse pattern match branches
pIss :: Parser [Src.Is]
pIss = do
  first <- pIs
  rest <- pIsMore
  return (first : rest)
  where
    pIsMore :: Parser [Src.Is]
    pIsMore = do
      rets
      mt <- peekTok
      case mt of
        Nothing               -> return []
        Just TkRightCurly     -> return []
        _                     -> do
          item <- pIs
          more <- pIsMore
          return (item : more)
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
  [ do
      (startArea, _) <- withArea pReturn
      target <- pSourceTarget
      body <- pExp
      return [Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Return body)]
  , do
      lookAhead pRightCurly
      return [Src.Source emptyArea Src.TargetAll Src.LUnit]
  , -- name <- exp ; doExps  (use try: must backtrack if not an assignment)
    try $ do
      (startArea, name) <- withArea pNameStr
      target <- pSourceTarget
      pLeftArrow
      body <- pExp
      rets
      rest <- pDoExps
      return $ Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.DoAssignment name body) : rest
  , do
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
  (startArea, _) <- withArea (void pLeftParen)
  target <- pSourceTarget
  mt <- peekTok
  case mt of
    Just TkRightParen -> do
      pRightParen
      pFatArrow
      rets
      pLambdaBody startArea target []
    _ -> do
      rets
      mResult <- optional $ try $ do
        params <- pParams
        rets
        void pRightParen
        pFatArrow
        rets
        return params
      case mResult of
        Just params -> pLambdaBody startArea target params
        Nothing -> do
          inner <- pExp
          rets
          (parenEndArea, _) <- withArea (void pRightParen)
          case inner of
            Src.Source nameArea _ (Src.Var name) -> do
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


-- | Parse lambda body after '=>'
pLambdaBody :: Area -> Src.SourceTarget -> [Src.Source Src.Name] -> Parser Src.Exp
pLambdaBody startArea target params = do
  mt <- peekTok
  case mt of
    Just TkLeftCurly -> do
      pLeftCurly
      rets
      body <- pMultiExpBody
      rets
      (endArea, _) <- withArea (void pRightCurly)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.AbsWithMultilineBody params body)
    _ -> do
      body <- pBodyExp
      return $ Src.Source (mergeAreas startArea (Src.getArea body)) target (Src.Abs params [body])


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
  mt <- peekTok
  case mt of
    Just TkRightCurly -> do
      (endArea, _) <- withArea (void pRightCurly)
      return $ Src.Source (mergeAreas startArea endArea) target Src.LUnit
    Just TkSpread -> do
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
    _ -> do
      fields <- pRecordFields
      _ <- optional pComma
      rets
      (endArea, _) <- withArea (void pRightCurly)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.Record fields)


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
    -- Parse name once, then peek at next token to decide field:value vs shorthand.
    pRecordField = do
      (nameArea, name) <- withArea pName
      target <- pSourceTarget
      mt <- peekTok
      case mt of
        Just TkDoubleColon -> return $ Src.Source nameArea target (Src.FieldShorthand name)
        Just TkMutateEq    -> return $ Src.Source nameArea target (Src.FieldShorthand name)
        Just TkColon -> do
          pColon
          value <- pExp
          return $ Src.Source (mergeAreas nameArea (Src.getArea value)) target (Src.Field (name, value))
        _ -> return $ Src.Source nameArea target (Src.FieldShorthand name)


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
-- The Alex lexer pre-processes template strings into:
--   TkTemplateStringFull s      -- `...` with no interpolation
--   TkTemplateStringStart s     -- `...${  (opening segment)
--   TkTemplateStringMid s       -- }...${  (middle segment)
--   TkTemplateStringEnd s       -- }...`   (closing segment)
--   TkTemplateInterpolClose     -- } that closes the interpolated expression

pTemplateString :: Parser Src.Exp
pTemplateString = do
  mt <- peekTok
  case mt of
    Just (TkTemplateStringFull raw) -> do
      (area, _) <- withArea anySingle
      target <- pSourceTarget
      str <- processEscape raw
      return $ Src.Source area target (Src.TemplateString [Src.Source area target (Src.LStr str)])
    Just (TkTemplateStringStart raw) -> do
      (startArea, _) <- withArea anySingle
      target <- pSourceTarget
      str <- processEscape raw
      let strPart = Src.Source startArea target (Src.LStr str)
      parts <- collectInterpolated target startArea
      let endArea = case reverse parts of
            (p:_) -> Src.getArea p
            []    -> startArea
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TemplateString (strPart : parts))
    _ -> fail "expected template string"
  where
    collectInterpolated :: Src.SourceTarget -> Area -> Parser [Src.Exp]
    collectInterpolated tgt prevArea = do
      rets  -- interpolation may start with a newline (e.g. `${  \n  expr\n}`)
      expr <- pExp
      rets
      -- consume the TkTemplateInterpolClose
      _ <- satisfyTok (\rt -> rtToken rt == TkTemplateInterpolClose)
      -- now check for mid or end
      mt2 <- peekTok
      case mt2 of
        Just (TkTemplateStringMid raw) -> do
          (midArea, _) <- withArea anySingle
          str <- processEscape raw
          let strPart = Src.Source midArea tgt (Src.LStr str)
          rest <- collectInterpolated tgt midArea
          return $ expr : strPart : rest
        Just (TkTemplateStringEnd raw) -> do
          (endArea, _) <- withArea anySingle
          str <- processEscape raw
          let strPart = Src.Source endArea tgt (Src.LStr str)
          return [expr, strPart]
        _ -> return [expr]

    processEscape :: String -> Parser String
    processEscape raw = case processEscapes raw of
      Left err -> fail err
      Right s  -> return s


-- Typed Expression --

pTypedExp :: Parser Src.Exp
pTypedExp = do
  void pLeftParen
  target <- pSourceTarget
  inner <- choice
    [ try $ do
        (nameArea, name) <- withArea pName
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
-- JSX uses regular tokens: TkLeftChevron (<), TkRightChevron (>), TkSlash (/)
-- Tag names are identifiers (lower for HTML tags, upper for components)

pJsxTag :: Parser Src.Exp
pJsxTag = choice
  [ -- Self-closing: <Name props />
    try $ do
      (startArea, _) <- withArea (void pLeftChevron)
      target <- pSourceTarget
      name <- pName
      props <- pJsxProps
      scn
      void pSlash
      (endArea, _) <- withArea (void pRightChevron)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.JsxAutoClosedTag name props)
  , -- Opening/closing: <Name props>children</Name>
    try $ do
      (startArea, _) <- withArea (void pLeftChevron)
      target <- pSourceTarget
      name <- pName
      props <- pJsxProps
      scn
      void pRightChevron
      children <- pJsxChildren
      void pLeftChevron
      void pSlash
      -- Match closing tag name
      closeName <- pName
      when (closeName /= name) $ fail $ "mismatched JSX tags: <" ++ name ++ "> vs </" ++ closeName ++ ">"
      (endArea, _) <- withArea (void pRightChevron)
      return $ Src.Source (mergeAreas startArea endArea) target (Src.JsxTag name props children)
  ]


pJsxProps :: Parser [Src.JsxProp]
pJsxProps = many (try (scn *> pJsxProp))
  where
    pJsxProp = choice
      [ -- {...expr} (spread prop) - uses TkLeftCurly directly (not TkLeftDoubleCurly)
        try $ do
          (startArea, _) <- withArea (void pLeftCurly)
          scn
          pSpread
          expr <- pExp
          scn
          (endArea, _) <- withArea (void pRightCurly)
          target <- pSourceTarget
          return $ Src.Source (mergeAreas startArea endArea) target (Src.JsxSpreadProp expr)
      , -- name="string"
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
          void pLeftCurly
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
      [ -- {...expr} (spread child)
        try $ do
          void pLeftCurly
          scn
          pSpread
          expr <- pExp
          scn
          pRightCurly
          return $ Src.JsxSpreadChild expr
      , -- {expr}
        try $ do
          void pLeftCurly
          scn
          expr <- pExp
          scn
          pRightCurly
          return $ Src.JsxExpChild expr
      , -- Nested JSX tag
        try pJsxTag >>= \tag -> return (Src.JsxChild tag)
      -- Note: text content between JSX tags is not handled here since the tokenizer
      -- doesn't produce text tokens between tags. JSX text content would require
      -- special lexer support or a different approach.
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
