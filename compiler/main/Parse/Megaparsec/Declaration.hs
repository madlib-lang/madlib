{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}
module Parse.Megaparsec.Declaration
  ( pAST
  , pASTWithRecovery
  ) where

import qualified Data.Map.Strict                as M
import           Control.Monad                  ( void, unless )
import           Data.Maybe                     ( catMaybes )

import           Text.Megaparsec                hiding ( State )
import qualified Text.Megaparsec.Byte          as C

import qualified Data.ByteString               as BS

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Error          ( CustomError )
import           Parse.Megaparsec.Lexeme
import           Parse.Megaparsec.Typing
import           Parse.Megaparsec.Expression


-- | Parse a full AST (top-level sequence of declarations and expressions)
pAST :: Parser Src.AST
pAST = do
  scn  -- consume leading whitespace including newlines
  items <- many $ do
    item <- pTopLevel
    rets
    return item
  eof
  return $ foldl applyItem emptyAST items
  where
    emptyAST = Src.AST
      { Src.aimports = []
      , Src.aderived = []
      , Src.aexps = []
      , Src.atypedecls = []
      , Src.ainterfaces = []
      , Src.ainstances = []
      , Src.apath = Nothing
      }

    applyItem ast item = case item of
      TopImports imps    -> ast { Src.aimports = Src.aimports ast <> imps }
      TopTypeDecl td     -> ast { Src.atypedecls = Src.atypedecls ast <> [td] }
      TopInterface iface -> ast { Src.ainterfaces = Src.ainterfaces ast <> [iface] }
      TopInstance inst    -> ast { Src.ainstances = Src.ainstances ast <> [inst] }
      TopExp e           -> ast { Src.aexps = Src.aexps ast <> [e] }
      TopDerived d        -> ast { Src.aderived = Src.aderived ast <> [d] }


-- | Parse AST with error recovery at top-level declaration boundaries.
-- When a top-level declaration fails to parse, the error is recorded and
-- parsing skips to the next top-level boundary. Returns a partial AST
-- with recovery errors accumulated in ParserState.
pASTWithRecovery :: Parser Src.AST
pASTWithRecovery = do
  scn
  items <- many $ withRecovery recoverTopLevel (Just <$> (pTopLevel <* rets))
  eof
  return $ foldl applyItem emptyAST (catMaybes items)
  where
    emptyAST = Src.AST
      { Src.aimports = []
      , Src.aderived = []
      , Src.aexps = []
      , Src.atypedecls = []
      , Src.ainterfaces = []
      , Src.ainstances = []
      , Src.apath = Nothing
      }

    applyItem ast item = case item of
      TopImports imps    -> ast { Src.aimports = Src.aimports ast <> imps }
      TopTypeDecl td     -> ast { Src.atypedecls = Src.atypedecls ast <> [td] }
      TopInterface iface -> ast { Src.ainterfaces = Src.ainterfaces ast <> [iface] }
      TopInstance inst    -> ast { Src.ainstances = Src.ainstances ast <> [inst] }
      TopExp e           -> ast { Src.aexps = Src.aexps ast <> [e] }
      TopDerived d        -> ast { Src.aderived = Src.aderived ast <> [d] }

    recoverTopLevel :: ParseError BS.ByteString CustomError -> Parser (Maybe TopLevel)
    recoverTopLevel err = do
      pos <- getLoc
      let errMsg = case err of
            TrivialError _ _ _ -> "Syntax error"
            FancyError _ _     -> "Syntax error"
      addRecoveryError $ ParseRecoveryError
        (getLine' pos) (getCol' pos) (getLine' pos) (getCol' pos) errMsg
      skipToNextTopLevel
      return Nothing

    skipToNextTopLevel :: Parser ()
    skipToNextTopLevel = do
      -- Skip to end of current line
      _ <- takeWhileP Nothing (/= 10) -- skip until newline
      _ <- optional (single 10)       -- consume the newline
      scn                             -- consume blank lines
      atEnd' <- atEnd
      unless atEnd' $ do
        w <- lookAhead $ takeWhileP Nothing isIdentB
        unless (w `elem` topLevelKeywords || BS.null w) skipToNextTopLevel

    topLevelKeywords :: [BS.ByteString]
    topLevelKeywords = ["import", "interface", "instance", "derive", "type", "alias", "export"]

    getLine' (Loc _ l _) = l
    getCol' (Loc _ _ c) = c


-- | Internal type to classify top-level items before folding into AST
data TopLevel
  = TopImports [Src.Import]
  | TopTypeDecl Src.TypeDecl
  | TopInterface Src.Interface
  | TopInstance Src.Instance
  | TopExp Src.Exp
  | TopDerived Src.Derived


-- | Peek at the next identifier-like word without consuming input.
-- Returns the word as a ByteString, or empty if not starting with alpha/underscore.
{-# INLINE peekWord #-}
peekWord :: Parser BS.ByteString
peekWord = lookAhead $ takeWhileP Nothing isIdentB

-- | Peek at a few bytes to check for macro keywords (#iftarget, #elseif, #endif)
{-# INLINE peekMacroWord #-}
peekMacroWord :: Parser BS.ByteString
peekMacroWord = lookAhead $ BS.cons <$> C.char 35 {- '#' -} <*> takeWhileP Nothing isIdentB

-- | Parse a single top-level item, using keyword-dispatch to avoid cascading try/backtrack.
pTopLevel :: Parser TopLevel
pTopLevel = do
  w <- peekWord
  case w of
    "import"    -> TopImports <$> pImportDecls
    "interface" -> TopInterface <$> pInterfaceDecl
    "instance"  -> TopInstance <$> pInstanceDecl
    "derive"    -> TopDerived <$> pDeriveDecl
    "type"      -> TopTypeDecl <$> pTypeDecl
    "alias"     -> TopTypeDecl <$> pTypeDecl
    "export"    -> pExportTopLevel
    "" -> do
      -- Empty word means non-identifier start character; check for macros
      mw <- optional peekMacroWord
      case mw of
        Just "#iftarget" -> TopExp <$> pMacroIfTarget
        Just "#elseif"   -> TopExp <$> pMacroElseIf
        Just "#endif"    -> TopExp <$> pMacroEndIf
        _                -> TopExp <$> pBodyExp
    _ -> TopExp <$> pBodyExp

-- | Dispatch for items starting with 'export' keyword.
-- Peek at the word after 'export' to avoid cascading try.
pExportTopLevel :: Parser TopLevel
pExportTopLevel = do
  -- Peek past 'export' + whitespace to see the next keyword
  nextWord <- lookAhead $ do
    _ <- takeWhileP Nothing isIdentB  -- skip 'export'
    _ <- takeWhileP Nothing (\b -> b == 32 || b == 9) -- skip spaces/tabs
    takeWhileP Nothing isIdentB
  case nextWord of
    "type" -> try (TopTypeDecl <$> pTypeDecl) <|> (TopExp <$> pExportTypeName)
    "alias" -> TopTypeDecl <$> pTypeDecl
    _ -> try (TopExp <$> pExportAssignment) <|> (TopExp <$> pExportName)


-- Import declarations --

pImportDecls :: Parser [Src.Import]
pImportDecls = some pImportDecl


pImportDecl :: Parser Src.Import
pImportDecl = do
  (startArea, _) <- withArea pImport
  target <- pSourceTarget
  -- Dispatch based on first token after 'import'
  w <- lookAhead $ takeWhileP Nothing isIdentB
  case w of
    "type" -> do
      -- import type { names } from "path"
      pType
      pLeftCurly
      rets
      names <- pImportNames
      _ <- optional pComma
      rets
      pRightCurly
      pFrom
      (endArea, path) <- withArea pStringLiteral
      rets
      return $ Src.Source (mergeAreas startArea endArea) target (Src.TypeImport names (sanitizeImportPath path) (sanitizeImportPath path))
    "" -> do
      -- import { names } from "path" (starts with '{')
      pLeftCurly
      rets
      names <- pImportNames
      _ <- optional pComma
      rets
      pRightCurly
      pFrom
      (endArea, path) <- withArea pStringLiteral
      rets
      return $ Src.Source (mergeAreas startArea endArea) target (Src.NamedImport names (sanitizeImportPath path) (sanitizeImportPath path))
    _ -> do
      -- import name from "path"
      (nameArea, name) <- withArea pNameStr
      pFrom
      (endArea, path) <- withArea pStringLiteral
      rets
      return $ Src.Source (mergeAreas startArea endArea) target (Src.DefaultImport (Src.Source nameArea target name) (sanitizeImportPath path) (sanitizeImportPath path))


pImportNames :: Parser [Src.Source Src.Name]
pImportNames = option [] $ do
  first <- pImportName
  rest <- many $ try $ do
    pComma
    rets
    pImportName
  return $ first : rest
  where
    pImportName = do
      (area, name) <- withArea pNameStr
      target <- pSourceTarget
      return $ Src.Source area target name


sanitizeImportPath :: String -> String
sanitizeImportPath = id


-- Interface declaration --

pInterfaceDecl :: Parser Src.Interface
pInterfaceDecl = do
  (startArea, _) <- withArea pInterface
  target <- pSourceTarget
  -- Optional constraints
  (constraints, ifaceName, typeParams) <- choice
    [ -- interface (constraints) => Name params
      try $ do
        pLeftParen
        cs <- pConstraints
        pRightParen
        pFatArrow
        name <- pNameStr
        params <- pNames
        return (cs, name, params)
    , -- interface constraint => Name params
      try $ do
        c <- pConstraint
        pFatArrow
        name <- pNameStr
        params <- pNames
        return ([c], name, params)
    , -- interface Name params
      do
        name <- pNameStr
        params <- pNames
        return ([], name, params)
    ]
  pLeftCurly
  rets
  methods <- pMethodDefs
  rets
  (endArea, _) <- withArea (void pRightCurly)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Interface constraints ifaceName typeParams methods)


pMethodDefs :: Parser (M.Map Src.Name Src.Typing)
pMethodDefs = do
  first <- pMethodDef
  rest <- many $ try $ do
    rets
    pMethodDef
  return $ M.fromList (first : rest)
  where
    pMethodDef = do
      name <- pNameStr
      pDoubleColon
      typing <- pConstrainedTyping
      return (name, typing)


-- Instance declaration --

pInstanceDecl :: Parser Src.Instance
pInstanceDecl = do
  (startArea, _) <- withArea pInstance
  target <- pSourceTarget
  -- Optional constraints
  (constraints, instName, typeArgs) <- choice
    [ -- instance (constraints) => Name typings
      try $ do
        pLeftParen
        maybeRet
        cs <- pInstanceConstraints
        rets
        pRightParen
        pFatArrow
        name <- pNameStr
        args <- pManyTypings
        return (cs, name, args)
    , -- instance constraint => Name typings
      try $ do
        c <- pInstanceConstraint
        pFatArrow
        name <- pNameStr
        args <- pManyTypings
        return ([c], name, args)
    , -- instance Name typings
      do
        name <- pNameStr
        args <- pManyTypings
        return ([], name, args)
    ]
  -- Accept either { or {{ as the instance body opener
  -- (The old Alex lexer converted { to {{ inside instanceHeader state)
  choice [pLeftDoubleCurly, pLeftCurly]
  rets
  methods <- pMethodImpls
  rets
  (endArea, _) <- withArea (void pRightCurly)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Instance constraints instName typeArgs methods)


pMethodImpls :: Parser (M.Map Src.Name Src.Exp)
pMethodImpls = do
  first <- pMethodImpl
  rest <- many $ try $ do
    rets
    pMethodImpl
  return $ M.fromList (first : rest)
  where
    pMethodImpl = do
      (nameArea, name) <- withArea pNameStr
      target <- pSourceTarget
      pEq
      maybeRet
      body <- pExp
      return (name, Src.Source (mergeAreas nameArea (Src.getArea body)) target (Src.Assignment name body))


-- Type declaration --

pTypeDecl :: Parser Src.TypeDecl
pTypeDecl = choice
  [ -- export type Name params = constructors
    try $ do
      (startArea, _) <- withArea pExport
      target <- pSourceTarget
      pType
      name <- pNameStr
      params <- pTypeParams
      rets
      pEq
      constructors <- pAdtConstructors
      return $ Src.Source (mergeAreas startArea (Src.getArea (last constructors))) target
        Src.ADT { Src.adtname = name, Src.adtparams = params, Src.adtconstructors = constructors, Src.adtexported = True }
  , -- type Name params = constructors
    try $ do
      (startArea, _) <- withArea pType
      target <- pSourceTarget
      name <- pNameStr
      params <- pTypeParams
      rets
      pEq
      constructors <- pAdtConstructors
      return $ Src.Source (mergeAreas startArea (Src.getArea (last constructors))) target
        Src.ADT { Src.adtname = name, Src.adtparams = params, Src.adtconstructors = constructors, Src.adtexported = False }
  , -- export alias Name params = typing
    try $ do
      (startArea, _) <- withArea pExport
      target <- pSourceTarget
      pAlias
      name <- pNameStr
      params <- pTypeParams
      rets
      pEq
      t <- pTypings
      return $ Src.Source (mergeAreas startArea (Src.getArea t)) target
        Src.Alias { Src.aliasname = name, Src.aliasparams = params, Src.aliastype = t, Src.aliasexported = True }
  , -- alias Name params = typing
    do
      (startArea, _) <- withArea pAlias
      target <- pSourceTarget
      name <- pNameStr
      params <- pTypeParams
      rets
      pEq
      t <- pTypings
      return $ Src.Source (mergeAreas startArea (Src.getArea t)) target
        Src.Alias { Src.aliasname = name, Src.aliasparams = params, Src.aliastype = t, Src.aliasexported = False }
  ]


pTypeParams :: Parser [Src.Name]
pTypeParams = many pNameStr


pAdtConstructors :: Parser [Src.Constructor]
pAdtConstructors = do
  first <- pAdtConstructor
  rest <- many $ try $ do
    rets
    pPipeChar <|> (rets *> pPipeChar)
    pAdtConstructor
  return $ first : rest


pAdtConstructor :: Parser Src.Constructor
pAdtConstructor = do
  (startArea, name) <- withArea pNameStr
  target <- pSourceTarget
  args <- option [] $ try $ do
    pLeftParen
    rets
    as <- pAdtConstructorArgs
    _ <- optional pComma
    rets
    pRightParen
    return as
  if null args then
    return $ Src.Source startArea target (Src.Constructor name [])
  else do
    (endArea, _) <- withArea (pure ())
    return $ Src.Source (mergeAreas startArea endArea) target (Src.Constructor name args)


pAdtConstructorArgs :: Parser [Src.Typing]
pAdtConstructorArgs = do
  first <- pTypingOrComposite
  rest <- many $ try $ pComma *> rets *> pTypingOrComposite
  return $ first : rest
  where
    pTypingOrComposite = try pTypings <|> pAtomicTyping


-- Derive declaration --

pDeriveDecl :: Parser Src.Derived
pDeriveDecl = do
  (startArea, _) <- withArea pDerive
  target <- pSourceTarget
  name <- pNameStr
  choice
    [ -- derive Name { fields }
      try $ do
        pLeftCurly
        fields <- pDeriveFields
        (endArea, _) <- withArea (void pRightCurly)
        return $ Src.Source (mergeAreas startArea endArea) target (Src.DerivedRecord name (fst fields))
    , -- derive Name Constructor
      do
        (endArea, ctor) <- withArea pNameStr
        return $ Src.Source (mergeAreas startArea endArea) target (Src.DerivedADT name ctor)
    ]


pDeriveFields :: Parser ([String], Area)
pDeriveFields = do
  first <- pNameStr
  rest <- many $ try $ pComma *> pNameStr
  return (first : rest, emptyArea)


-- Export expressions --

-- | export name
pExportName :: Parser Src.Exp
pExportName = do
  (startArea, _) <- withArea pExport
  target <- pSourceTarget
  (endArea, name) <- withArea pNameStr
  return $ Src.Source (mergeAreas startArea endArea) target (Src.NameExport name)


-- | export type name
pExportTypeName :: Parser Src.Exp
pExportTypeName = do
  (startArea, _) <- withArea (pKeyword "export")
  target <- pSourceTarget
  pType
  (endArea, name) <- withArea pNameStr
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TypeExport name)


-- | export name = exp
pExportAssignment :: Parser Src.Exp
pExportAssignment = do
  (startArea, _) <- withArea pExport
  target <- pSourceTarget
  name <- pNameStr
  pEq
  maybeRet
  body <- pExp
  let assignArea = mergeAreas startArea (Src.getArea body)
  return $ Src.Source assignArea target (Src.Export (Src.Source assignArea target (Src.Assignment name body)))


-- Target macros --

pMacroIfTarget :: Parser Src.Exp
pMacroIfTarget = do
  (area, _) <- withArea $ lexeme $ C.string "#iftarget"
  sc
  target <- pTargetName
  setSourceTarget target
  return $ Src.Source area target (Src.IfTarget target)


pMacroElseIf :: Parser Src.Exp
pMacroElseIf = do
  (area, _) <- withArea $ lexeme $ C.string "#elseif"
  sc
  target <- pTargetName
  setSourceTarget target
  return $ Src.Source area target (Src.ElseIfTarget target)


pMacroEndIf :: Parser Src.Exp
pMacroEndIf = do
  (area, _) <- withArea $ lexeme $ C.string "#endif"
  setSourceTarget Src.TargetAll
  return $ Src.Source area Src.TargetAll Src.EndIfTarget


pTargetName :: Parser Src.SourceTarget
pTargetName = choice
  [ Src.TargetLLVM <$ pKeyword "llvm"
  , Src.TargetJS <$ pKeyword "js"
  ]


-- Helper --

pNames :: Parser [Src.Name]
pNames = many pNameStr
