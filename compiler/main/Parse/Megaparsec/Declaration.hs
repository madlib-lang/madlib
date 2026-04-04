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

import           Text.Megaparsec                hiding ( State, Token )

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Error          ( CustomError )
import           Parse.Megaparsec.Lexeme
import           Parse.Megaparsec.Typing
import           Parse.Megaparsec.Expression
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )
import           Parse.Lexer.TokenStream        ( TokenStream )


-- Helper: peek at the next token kind (Nothing at EOF)
peekTok :: Parser (Maybe Token)
peekTok = fmap (fmap rtToken) (optional (lookAhead anySingle))


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

    recoverTopLevel :: ParseError TokenStream CustomError -> Parser (Maybe TopLevel)
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
      -- Skip tokens until we hit a newline, then skip blank lines
      -- until we reach a token that could start a top-level declaration
      _ <- optional $ skipManyTill anySingle (void (satisfyTok (\rt -> rtToken rt == TkNewline)) <|> eof)
      scn   -- skip blank lines (TkNewline tokens)
      atEnd' <- atEnd
      unless atEnd' $ do
        mt <- peekTok
        unless (isTopLevelStart mt) skipToNextTopLevel

    isTopLevelStart :: Maybe Token -> Bool
    isTopLevelStart Nothing           = True   -- EOF
    isTopLevelStart (Just TkImport)   = True
    isTopLevelStart (Just TkInterface) = True
    isTopLevelStart (Just TkInstance) = True
    isTopLevelStart (Just TkDerive)   = True
    isTopLevelStart (Just TkType)     = True
    isTopLevelStart (Just TkAlias)    = True
    isTopLevelStart (Just TkExport)   = True
    isTopLevelStart _                 = False

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


-- | Parse a single top-level item, dispatching on the next token.
pTopLevel :: Parser TopLevel
pTopLevel = do
  mt <- peekTok
  case mt of
    Just TkImport    -> TopImports <$> pImportDecls
    Just TkInterface -> TopInterface <$> pInterfaceDecl
    Just TkInstance  -> TopInstance <$> pInstanceDecl
    Just TkDerive    -> TopDerived <$> pDeriveDecl
    Just TkType      -> TopTypeDecl <$> pTypeDecl
    Just TkAlias     -> TopTypeDecl <$> pTypeDecl
    Just TkExport    -> pExportTopLevel
    Just TkSharp     -> pMacroTopLevel
    _                -> TopExp <$> pBodyExp


-- | Parse macro directives: #iftarget, #elseif, #endif
pMacroTopLevel :: Parser TopLevel
pMacroTopLevel = do
  pSharp
  name <- pNameStr
  case name of
    "iftarget" -> TopExp <$> pMacroIfTargetBody
    "elseif"   -> TopExp <$> pMacroElseIfBody
    "endif"    -> TopExp <$> pMacroEndIfBody
    _          -> TopExp <$> pBodyExp


-- | Dispatch for items starting with 'export' keyword.
pExportTopLevel :: Parser TopLevel
pExportTopLevel = do
  mt2 <- lookAhead $ do
    _ <- anySingle  -- skip TkExport
    fmap (fmap rtToken) (optional anySingle)
  case mt2 of
    Just TkType  -> try (TopTypeDecl <$> pTypeDecl) <|> (TopExp <$> pExportTypeName)
    Just TkAlias -> TopTypeDecl <$> pTypeDecl
    _            -> try (TopExp <$> pExportAssignment) <|> (TopExp <$> pExportName)


-- Import declarations --

pImportDecls :: Parser [Src.Import]
pImportDecls = some pImportDecl


pImportDecl :: Parser Src.Import
pImportDecl = do
  (startArea, _) <- withArea pImport
  target <- pSourceTarget
  -- Dispatch based on first token after 'import'
  mt <- peekTok
  case mt of
    Just TkType -> do
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
    Just TkLeftCurly -> do
      -- import { names } from "path"
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
      -- import name from "path" (name can be upper or lower case)
      (nameArea, name) <- withArea pName
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
      (area, name) <- withArea pName
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
        name <- pName
        params <- pNames
        return (cs, name, params)
    , -- interface constraint => Name params
      try $ do
        c <- pConstraint
        pFatArrow
        name <- pName
        params <- pNames
        return ([c], name, params)
    , -- interface Name params
      do
        name <- pName
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
  rest <- pMethodDefsMore
  return $ M.fromList (first : rest)
  where
    pMethodDefsMore :: Parser [(Src.Name, Src.Typing)]
    pMethodDefsMore = do
      rets
      mt <- peekTok
      case mt of
        Nothing               -> return []
        Just TkRightCurly     -> return []
        _                     -> do
          item <- pMethodDef
          more <- pMethodDefsMore
          return (item : more)
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
        name <- pName
        args <- pManyTypings
        return (cs, name, args)
    , -- instance constraint => Name typings
      try $ do
        c <- pInstanceConstraint
        pFatArrow
        name <- pName
        args <- pManyTypings
        return ([c], name, args)
    , -- instance Name typings
      do
        name <- pName
        args <- pManyTypings
        return ([], name, args)
    ]
  -- Accept either { or {{ as the instance body opener
  choice [pLeftDoubleCurly, pLeftCurly]
  rets
  methods <- pMethodImpls
  rets
  (endArea, _) <- withArea (void pRightCurly)
  return $ Src.Source (mergeAreas startArea endArea) target (Src.Instance constraints instName typeArgs methods)


pMethodImpls :: Parser (M.Map Src.Name Src.Exp)
pMethodImpls = do
  first <- pMethodImpl
  rest <- pMethodImplsMore
  return $ M.fromList (first : rest)
  where
    pMethodImplsMore :: Parser [(Src.Name, Src.Exp)]
    pMethodImplsMore = do
      rets
      mt <- peekTok
      case mt of
        Nothing           -> return []
        Just TkRightCurly -> return []
        _                 -> do
          item <- pMethodImpl
          more <- pMethodImplsMore
          return (item : more)
    pMethodImpl = do
      (nameArea, name) <- withArea pNameStr
      target <- pSourceTarget
      pEq
      maybeRet
      body <- pExp
      return (name, Src.Source (mergeAreas nameArea (Src.getArea body)) target (Src.Assignment name body))


-- Type declaration --

pTypeDecl :: Parser Src.TypeDecl
pTypeDecl = do
  mt <- peekTok
  case mt of
    Just TkExport -> do
      (startArea, _) <- withArea pExport
      target <- pSourceTarget
      mt2 <- peekTok
      case mt2 of
        Just TkType -> do
          pType
          name <- pName
          params <- pTypeParams
          rets
          pEq
          constructors <- pAdtConstructors
          return $ Src.Source (mergeAreas startArea (Src.getArea (last constructors))) target
            Src.ADT { Src.adtname = name, Src.adtparams = params, Src.adtconstructors = constructors, Src.adtexported = True }
        _ -> do  -- alias
          pAlias
          name <- pName
          params <- pTypeParams
          rets
          pEq
          t <- pTypings
          return $ Src.Source (mergeAreas startArea (Src.getArea t)) target
            Src.Alias { Src.aliasname = name, Src.aliasparams = params, Src.aliastype = t, Src.aliasexported = True }
    Just TkType -> do
      (startArea, _) <- withArea pType
      target <- pSourceTarget
      name <- pName
      params <- pTypeParams
      rets
      pEq
      constructors <- pAdtConstructors
      return $ Src.Source (mergeAreas startArea (Src.getArea (last constructors))) target
        Src.ADT { Src.adtname = name, Src.adtparams = params, Src.adtconstructors = constructors, Src.adtexported = False }
    _ -> do  -- alias
      (startArea, _) <- withArea pAlias
      target <- pSourceTarget
      name <- pName
      params <- pTypeParams
      rets
      pEq
      t <- pTypings
      return $ Src.Source (mergeAreas startArea (Src.getArea t)) target
        Src.Alias { Src.aliasname = name, Src.aliasparams = params, Src.aliastype = t, Src.aliasexported = False }


pTypeParams :: Parser [Src.Name]
pTypeParams = many pNameStr


pAdtConstructors :: Parser [Src.Constructor]
pAdtConstructors = do
  first <- pAdtConstructor
  rest <- pAdtConstructorsMore
  return (first : rest)
  where
    pAdtConstructorsMore :: Parser [Src.Constructor]
    pAdtConstructorsMore = do
      rets
      mt <- peekTok
      case mt of
        Just TkPipeChar -> do
          pPipeChar
          item <- pAdtConstructor
          more <- pAdtConstructorsMore
          return (item : more)
        _ -> return []


pAdtConstructor :: Parser Src.Constructor
pAdtConstructor = do
  (startArea, name) <- withArea pName
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
  name <- pName
  choice
    [ -- derive Name { fields }
      try $ do
        pLeftCurly
        fields <- pDeriveFields
        (endArea, _) <- withArea (void pRightCurly)
        return $ Src.Source (mergeAreas startArea endArea) target (Src.DerivedRecord name (fst fields))
    , -- derive Name Constructor
      do
        (endArea, ctor) <- withArea pName
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
  (endArea, name) <- withArea pName
  return $ Src.Source (mergeAreas startArea endArea) target (Src.NameExport name)


-- | export type name
pExportTypeName :: Parser Src.Exp
pExportTypeName = do
  (startArea, _) <- withArea pExport
  target <- pSourceTarget
  pType
  (endArea, name) <- withArea pName
  return $ Src.Source (mergeAreas startArea endArea) target (Src.TypeExport name)


-- | export name = exp
pExportAssignment :: Parser Src.Exp
pExportAssignment = do
  (startArea, _) <- withArea pExport
  target <- pSourceTarget
  name <- pName
  pEq
  maybeRet
  body <- pExp
  let assignArea = mergeAreas startArea (Src.getArea body)
  return $ Src.Source assignArea target (Src.Export (Src.Source assignArea target (Src.Assignment name body)))


-- Target macros (called after '#' has already been consumed) --

pMacroIfTargetBody :: Parser Src.Exp
pMacroIfTargetBody = do
  (area, _) <- withArea $ return ()
  target <- pTargetName
  setSourceTarget target
  return $ Src.Source area target (Src.IfTarget target)


pMacroElseIfBody :: Parser Src.Exp
pMacroElseIfBody = do
  (area, _) <- withArea $ return ()
  target <- pTargetName
  setSourceTarget target
  return $ Src.Source area target (Src.ElseIfTarget target)


pMacroEndIfBody :: Parser Src.Exp
pMacroEndIfBody = do
  (area, _) <- withArea $ return ()
  setSourceTarget Src.TargetAll
  return $ Src.Source area Src.TargetAll Src.EndIfTarget


pTargetName :: Parser Src.SourceTarget
pTargetName = do
  name <- pNameStr
  case name of
    "llvm" -> return Src.TargetLLVM
    "js"   -> return Src.TargetJS
    _      -> fail $ "expected 'llvm' or 'js', got: " ++ name


-- Helper --

pNames :: Parser [Src.Name]
pNames = many pNameStr
