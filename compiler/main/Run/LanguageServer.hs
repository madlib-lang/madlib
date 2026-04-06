{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use forM_" #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run.LanguageServer
  ( runLanguageServer
  , module Run.LanguageServer.State
  , module Run.LanguageServer.Diagnostics
  , module Run.LanguageServer.Hover
  , module Run.LanguageServer.Completion
  ) where

import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import Control.Monad.IO.Class
import qualified Data.Text as T
import Control.Monad.Base
import qualified Rock
import Driver.Rules
import qualified Driver.Query as Query
import qualified Driver
import Error.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error.Context
import Explain.Location
import qualified Explain.Location as Loc
import qualified Explain.Format as Explain
import           Data.List (foldl')
import qualified Data.List as List
import           Control.Monad (forM_, join, forM, unless, void)
import           Data.IORef
import qualified AST.Solved as Slv
import           Explain.Format (prettyPrintQualType, prettyPrintType, kindToStr, prettyPrintTyping, prettyPrintTyping', renderSchemesWithDiff)
import           Control.Applicative ((<|>))
import           Infer.Type (Qual((:=>)), Type (..), kind, Kind (Star), TCon (..), TVar (..), findTypeVarInType, collectVars, buildKind, getQualified, Scheme (Forall), getParamTypes)
import qualified Error.Warning as Warning
import           Error.Warning (CompilationWarning(CompilationWarning))
import qualified Error.Error as Error
import           Data.Time.Clock
import qualified Run.Options as Options
import qualified Utils.PathUtils as PathUtils
import           Run.Target
import qualified Data.Maybe as Maybe
import           Run.Options (Options(optEntrypoint))
import           Control.Monad.Trans.Control
import           Control.Concurrent
import           Data.Foldable (toList)
import           Control.Concurrent.Async
import           Control.Exception (SomeException, try)
import           Rock (Cyclic)
import qualified AST.Canonical         as Can
import qualified Canonicalize.EnvUtils as CanEnv
import Driver.Query (Query(CanonicalizedASTWithEnv))
import qualified AST.Source as Src
import           Parse.DocString.DocString (DocString(..), DocStringTag(..), findParamTags, findReturnsTag, findDeprecatedTag)
import qualified Canonicalize.Typing as Can
import qualified Infer.Typing as Slv
import qualified Canonicalize.Env as CanEnv
import qualified Canonicalize.Interface as Can
import System.FilePath (takeFileName, dropExtension, takeExtension, (</>))
import qualified Data.HashMap.Strict as HashMap
import Run.OptimizationLevel
import Run.SourceMapMode
import           Run.ErrorFormat (ErrorFormat(..))
import           Run.PGOMode (PGOMode(..))
import Data.Maybe (isJust)
import GHC.Base (when)
import Language.LSP.VFS (virtualFileText)
import qualified Infer.Env as SlvEnv
import Data.Char (isAlphaNum, isDigit, isUpper, toLower)
import qualified System.Directory as Dir
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson ((.=), (.:))
import qualified MadlibDotJson.MadlibDotJson as MadlibDotJson

import Run.LanguageServer.State
import Run.LanguageServer.Diagnostics
import Run.LanguageServer.Hover
import Run.LanguageServer.Completion



handlers :: State -> State -> Handlers (LspM ())
handlers state autocompletionState = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      sendNotification SWindowLogMessage (LogMessageParams MtInfo "Madlib server initialized")
      env <- getLspEnv
      liftIO $ void $ forkIO $ backgroundCompileProject state autocompletionState env
  , requestHandler STextDocumentHover $ \req responder ->
      recordAndPrintDuration "hover" $ do
        let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos@(Position line col) _workDone) = req
        maybeHoverInfo <- getHoverInformation state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
        case maybeHoverInfo of
          Just info -> do
            let ms    = HoverContents $ MarkupContent MkMarkdown (T.pack info)
                range = Range pos pos
                rsp   = Hover ms (Just range)
            responder (Right $ Just rsp)

          Nothing ->
            responder (Right Nothing)
  , requestHandler STextDocumentDefinition $ \(RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) (Position line col) _ _)) responder ->
      recordAndPrintDuration "definition" $ do
        links <- getDefinitionLinks state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
        case links of
          [] ->
            responder $ Right (InR $ InL $ List [])

          [loc] ->
            responder $ Right (InL loc)

          locs ->
            responder $ Right (InR $ InL $ List locs)
  , requestHandler STextDocumentCompletion $ \(RequestMessage _ _ _ (CompletionParams (TextDocumentIdentifier uri) (Position line col) _ _ _)) responder ->
      recordAndPrintDuration "completion" $ do
        file <- getVirtualFile (toNormalizedUri uri)
        let fileContent = T.unpack . virtualFileText <$> file
        case fileContent of
          Just content -> do
            suggestions <- getAutocompletionSuggestions autocompletionState (Loc 0 (line + 1) (col + 1)) (uriToPath uri) content
            let completionItems = map (\(s, typing, kind) -> CompletionItem (T.pack s) (Just kind) Nothing (Just $ T.pack typing) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) suggestions
            responder $ Right $ InR (CompletionList True (List completionItems))

          Nothing ->
            responder $ Right $ InR (CompletionList True (List []))

  , notificationHandler STextDocumentDidOpen $ \(NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) ->
      recordAndPrintDuration "file open" $ generateDiagnostics False state autocompletionState uri mempty
  , notificationHandler STextDocumentDidSave $ \(NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _)) ->
      recordAndPrintDuration "file save" $ generateDiagnostics False state autocompletionState uri mempty
  , notificationHandler STextDocumentDidChange $ \(NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) (List changes))) -> do
      let (TextDocumentContentChangeEvent _ _ docContent) = last changes
      let fileUpdates = Map.singleton (uriToPath uri) (T.unpack docContent)
      currentTime <- liftIO getCurrentTime
      liftIO $ writeIORef (_debounceRef state) (Just currentTime)
      env <- getLspEnv
      liftIO $ void $ forkIO $ do
        threadDelay 100000  -- 100ms debounce
        latest <- readIORef (_debounceRef state)
        case latest of
          Just t | t == currentTime ->
            runLspT env $ recordAndPrintDuration "file change" $
              generateDiagnostics True state autocompletionState uri fileUpdates
          _ -> return ()
  , requestHandler STextDocumentDocumentSymbol $ \(RequestMessage _ _ _ (DocumentSymbolParams _ _ (TextDocumentIdentifier uri))) responder ->
      recordAndPrintDuration "documentSymbol" $ do
        symbols <- getDocumentSymbols state (uriToPath uri)
        responder $ Right $ InL (List symbols)
  , requestHandler STextDocumentCodeAction $ \(RequestMessage _ _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _ (CodeActionContext (List diags) _))) responder ->
      recordAndPrintDuration "codeAction" $ do
        let removeImportActions = Maybe.mapMaybe (diagnosticToCodeAction uri) diags
        file <- getVirtualFile (toNormalizedUri uri)
        let fileContent = maybe "" (T.unpack . virtualFileText) file
        addImportActions <- getAddImportActions state (uriToPath uri) uri diags fileContent
        responder $ Right $ List (map InR (removeImportActions ++ addImportActions))
  , requestHandler STextDocumentSignatureHelp $ \(RequestMessage _ _ _ (SignatureHelpParams (TextDocumentIdentifier uri) (Position line col) _ _)) responder ->
      recordAndPrintDuration "signatureHelp" $ do
        help <- getSignatureHelp state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
        responder $ Right help
  , requestHandler STextDocumentReferences $ \(RequestMessage _ _ _ (ReferenceParams (TextDocumentIdentifier uri) (Position line col) _ _ _)) responder ->
      recordAndPrintDuration "references" $ do
        refs <- findReferences state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
        responder $ Right (List refs)
  , requestHandler STextDocumentRename $ \(RequestMessage _ _ _ (RenameParams (TextDocumentIdentifier uri) (Position line col) _ newName)) responder ->
      recordAndPrintDuration "rename" $ do
        edits <- renameSymbol state (Loc 0 (line + 1) (col + 1)) (uriToPath uri) (T.unpack newName)
        responder $ Right $ WorkspaceEdit (Just edits) Nothing Nothing
  , requestHandler SWorkspaceSymbol $ \(RequestMessage _ _ _ (WorkspaceSymbolParams _ _ query)) responder ->
      recordAndPrintDuration "workspaceSymbol" $ do
        symbols <- getWorkspaceSymbols state (T.unpack query)
        responder $ Right (List symbols)
  , requestHandler STextDocumentFoldingRange $ \(RequestMessage _ _ _ (FoldingRangeParams _ _ (TextDocumentIdentifier uri))) responder ->
      recordAndPrintDuration "foldingRange" $ do
        ranges <- getFoldingRanges state (uriToPath uri)
        responder $ Right (List ranges)
  , requestHandler (SCustomMethod "textDocument/inlayHint") $ \req responder ->
      recordAndPrintDuration "inlayHint" $ do
        let RequestMessage _ _ _ reqParams = req
        case Aeson.parseMaybe parseInlayHintParams reqParams of
          Just (uri, startLine, endLine) -> do
            hints <- getInlayHints state (uriToPath uri) startLine endLine
            responder $ Right $ Aeson.toJSON hints
          Nothing ->
            responder $ Right $ Aeson.toJSON ([] :: [Aeson.Value])

  -- Feature 5: Semantic tokens (textDocument/semanticTokens/full)
  , requestHandler (SCustomMethod "textDocument/semanticTokens/full") $ \req responder ->
      recordAndPrintDuration "semanticTokens" $ do
        let RequestMessage _ _ _ reqParams = req
        case Aeson.parseMaybe parseSemanticTokensParams reqParams of
          Just uri -> do
            tokens <- getSemanticTokens state (uriToPath uri)
            responder $ Right $ Aeson.toJSON tokens
          Nothing ->
            responder $ Right $ Aeson.object ["data" .= ([] :: [Int])]

  -- Feature 9: Call hierarchy
  , requestHandler (SCustomMethod "textDocument/prepareCallHierarchy") $ \req responder ->
      recordAndPrintDuration "prepareCallHierarchy" $ do
        let RequestMessage _ _ _ reqParams = req
        case Aeson.parseMaybe parsePositionParams reqParams of
          Just (uri, line, col) -> do
            items <- prepareCallHierarchy state (uriToPath uri) line col
            responder $ Right $ Aeson.toJSON items
          Nothing ->
            responder $ Right $ Aeson.toJSON ([] :: [Aeson.Value])

  , requestHandler (SCustomMethod "callHierarchy/incomingCalls") $ \req responder ->
      recordAndPrintDuration "callHierarchyIncoming" $ do
        let RequestMessage _ _ _ reqParams = req
        case Aeson.parseMaybe parseCallHierarchyItemParam reqParams of
          Just (itemName, itemUri) -> do
            calls <- getIncomingCalls state itemName itemUri
            responder $ Right $ Aeson.toJSON calls
          Nothing ->
            responder $ Right $ Aeson.toJSON ([] :: [Aeson.Value])

  , requestHandler (SCustomMethod "callHierarchy/outgoingCalls") $ \req responder ->
      recordAndPrintDuration "callHierarchyOutgoing" $ do
        let RequestMessage _ _ _ reqParams = req
        case Aeson.parseMaybe parseCallHierarchyItemParam reqParams of
          Just (itemName, itemUri) -> do
            calls <- getOutgoingCalls state itemName itemUri
            responder $ Right $ Aeson.toJSON calls
          Nothing ->
            responder $ Right $ Aeson.toJSON ([] :: [Aeson.Value])
  ]


-- Definition Links --

getDefinitionLinks :: State -> Loc -> FilePath -> LspM () [Location]
getDefinitionLinks state loc path = do
  jsOptions <- buildOptions TNode
  jsResult <- liftIO $ safeRunTask state jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (definitionLocationTask loc path)
  let jsLocations = case jsResult of
        Just (Just (p, area), _, _) ->
          [Location (pathToUri p) (areaToRange area)]
        _ ->
          []
  if not (null jsLocations) then
    return jsLocations
  else do
    llvmOptions <- buildOptions TLLVM
    llvmResult <- liftIO $ safeRunTask state llvmOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (definitionLocationTask loc path)
    case llvmResult of
      Just (Just (p, area), _, _) ->
        return [Location (pathToUri p) (areaToRange area)]
      _ ->
        return []


definitionLocationTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe (FilePath, Area))
definitionLocationTask loc path = do
  -- hasCycle <- Rock.fetch $ Query.DetectImportCycle [] path
  -- if hasCycle then
  --   return Nothing
  -- else do
    (typedAst, _)  <- Rock.fetch $ Query.SolvedASTWithEnv path
    (canAst, _, _) <- Rock.fetch $ Query.CanonicalizedASTWithEnv path
    srcAst         <- Rock.fetch $ Query.ParsedAST path

    let foundNode = findNodeInAst loc srcAst typedAst
    case findNameInNode foundNode of
      Just name -> do
        maybeExp         <- Rock.fetch $ Query.ForeignExp path name
        maybeConstructor <- Rock.fetch $ Query.ForeignConstructor path name
        maybeTypeDecl    <- Rock.fetch $ Query.ForeignTypeDeclaration path name
        case Slv.getArea <$> maybeExp <|> Slv.getArea <$> maybeConstructor <|> Slv.getArea <$> maybeTypeDecl of
          Just area ->
            return $ Just (path, area)

          _ -> do
            case findForeignAstForName name (Can.aimports canAst) of
              Just fp -> do
                let name' =
                      if '.' `elem` name then
                        case dropWhile (/= '.') name of
                          '.' : name ->
                            name

                          or ->
                            or
                      else
                        name

                maybeExp'         <- Rock.fetch $ Query.ForeignExp fp name'
                maybeConstructor' <- Rock.fetch $ Query.ForeignConstructor fp name'
                maybeTypeDecl'    <- Rock.fetch $ Query.ForeignTypeDeclaration fp name'
                case Slv.getArea <$> maybeExp' <|> Slv.getArea <$> maybeConstructor' <|> Slv.getArea <$> maybeTypeDecl' of
                  Just area' ->
                    return $ Just (fp, area')

                  _ ->
                    return Nothing

              Nothing ->
                return Nothing

      _ ->
        case foundNode of
          Just (NamedImportNode _ importName importPath) -> do
            maybeExp'         <- Rock.fetch $ Query.ForeignExp importPath importName
            maybeConstructor' <- Rock.fetch $ Query.ForeignConstructor importPath importName
            case Slv.getArea <$> maybeExp' <|> Slv.getArea <$> maybeConstructor' of
              Just area' ->
                return $ Just (importPath, area')

              _ ->
                return Nothing

          Just (TypeImportNode _ importName importPath) -> do
            maybeTypeDecl'    <- Rock.fetch $ Query.ForeignTypeDeclaration importPath importName
            case Slv.getArea <$> maybeTypeDecl' of
              Just area' ->
                return $ Just (importPath, area')

              _ ->
                return Nothing

          Just (DefaultImportNode area filepath) ->
            return $ Just (filepath, area)

          _ ->
            return Nothing


-- Document Symbols --

getDocumentSymbols :: State -> FilePath -> LspM () [DocumentSymbol]
getDocumentSymbols state path = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (documentSymbolsTask path)
  case result of
    Just (symbols, _, _) -> return symbols
    Nothing              -> return []


documentSymbolsTask :: FilePath -> Rock.Task Query.Query [DocumentSymbol]
documentSymbolsTask path = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  let expSymbols = Maybe.mapMaybe expToDocSymbol (Slv.aexps typedAst)
  let typeSymbols = map typeDeclToDocSymbol (Slv.atypedecls typedAst)
  let ifaceSymbols = map ifaceToDocSymbol (Slv.ainterfaces typedAst)
  return $ expSymbols ++ typeSymbols ++ ifaceSymbols


expToDocSymbol :: Slv.Exp -> Maybe DocumentSymbol
expToDocSymbol exp = case exp of
  Slv.Typed qt area (Slv.Assignment name _) | not (isSyntheticName name) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _) | not (isSyntheticName name) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _))) | not (isSyntheticName name) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _) | not (isSyntheticName name) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.Extern _ name _) | not (isSyntheticName name) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.Export (Slv.Typed _ _ (Slv.Extern _ name _))) | not (isSyntheticName name) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  _ -> Nothing


typeDeclToDocSymbol :: Slv.TypeDecl -> DocumentSymbol
typeDeclToDocSymbol td = case td of
  Slv.Untyped area (Slv.ADT { Slv.adtname = name, Slv.adtconstructors = ctors }) ->
    let children = map ctorToDocSymbol ctors
    in  mkDocSymbolWithChildren (T.pack name) Nothing SkEnum area area (Just $ List children)

  Slv.Typed _ area (Slv.ADT { Slv.adtname = name, Slv.adtconstructors = ctors }) ->
    let children = map ctorToDocSymbol ctors
    in  mkDocSymbolWithChildren (T.pack name) Nothing SkEnum area area (Just $ List children)

  Slv.Untyped area (Slv.Alias { Slv.aliasname = name }) ->
    mkDocSymbol (T.pack name) Nothing SkClass area area

  Slv.Typed _ area (Slv.Alias { Slv.aliasname = name }) ->
    mkDocSymbol (T.pack name) Nothing SkClass area area


ctorToDocSymbol :: Slv.Constructor -> DocumentSymbol
ctorToDocSymbol ctor =
  let name = Slv.getConstructorName ctor
      area = Slv.getArea ctor
  in  mkDocSymbol (T.pack name) Nothing SkEnumMember area area


ifaceToDocSymbol :: Slv.Interface -> DocumentSymbol
ifaceToDocSymbol iface = case iface of
  Slv.Untyped area (Slv.Interface name _ _ _ _) ->
    mkDocSymbol (T.pack name) Nothing SkInterface area area

  Slv.Typed _ area (Slv.Interface name _ _ _ _) ->
    mkDocSymbol (T.pack name) Nothing SkInterface area area


mkDocSymbol :: T.Text -> Maybe T.Text -> SymbolKind -> Area -> Area -> DocumentSymbol
mkDocSymbol name detail symbolKind range selRange =
  DocumentSymbol name detail symbolKind Nothing Nothing (areaToRange range) (areaToRange selRange) Nothing

mkDocSymbolWithChildren :: T.Text -> Maybe T.Text -> SymbolKind -> Area -> Area -> Maybe (List DocumentSymbol) -> DocumentSymbol
mkDocSymbolWithChildren name detail symbolKind range selRange children =
  DocumentSymbol name detail symbolKind Nothing Nothing (areaToRange range) (areaToRange selRange) children


-- Workspace Symbols --

getWorkspaceSymbols :: State -> String -> LspM () [SymbolInformation]
getWorkspaceSymbols state query = do
  bgDone <- liftIO $ readIORef (_backgroundDone state)
  if not bgDone || null query then return []
  else do
    allPaths <- liftIO $ Set.toList <$> readIORef (_allModulePaths state)
    options <- buildOptions TNode
    results <- forM allPaths $ \modPath -> do
      result <- liftIO $ safeRunTask state options { optEntrypoint = modPath }
                  Driver.Don'tPrune mempty mempty (workspaceSymbolsTask query modPath)
      case result of
        Just (syms, _, _) -> return syms
        Nothing           -> return []
    return $ concat results


workspaceSymbolsTask :: String -> FilePath -> Rock.Task Query.Query [SymbolInformation]
workspaceSymbolsTask query modPath = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
  let queryLower = map toLower query
      matchesQuery name = queryLower `List.isInfixOf` map toLower name
      fileUri = Uri (T.pack $ "file://" ++ modPath)
  let expSyms = Maybe.mapMaybe (expToSymbolInfo fileUri matchesQuery) (Slv.aexps typedAst)
  let typeSyms = Maybe.mapMaybe (typeDeclToSymbolInfo fileUri matchesQuery) (Slv.atypedecls typedAst)
  return $ expSyms ++ typeSyms


expToSymbolInfo :: Uri -> (String -> Bool) -> Slv.Exp -> Maybe SymbolInformation
expToSymbolInfo fileUri matchesQuery exp = case exp of
  Slv.Typed _ area (Slv.Assignment name _) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _))) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.Extern _ name _) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  Slv.Typed _ area (Slv.Export (Slv.Typed _ _ (Slv.Extern _ name _))) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkFunction area fileUri
  _ -> Nothing


typeDeclToSymbolInfo :: Uri -> (String -> Bool) -> Slv.TypeDecl -> Maybe SymbolInformation
typeDeclToSymbolInfo fileUri matchesQuery td = case td of
  Slv.Untyped area (Slv.ADT { Slv.adtname = name }) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkEnum area fileUri
  Slv.Typed _ area (Slv.ADT { Slv.adtname = name }) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkEnum area fileUri
  Slv.Untyped area (Slv.Alias { Slv.aliasname = name }) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkClass area fileUri
  Slv.Typed _ area (Slv.Alias { Slv.aliasname = name }) | matchesQuery name ->
    Just $ mkSymInfo (T.pack name) SkClass area fileUri
  _ -> Nothing


mkSymInfo :: T.Text -> SymbolKind -> Area -> Uri -> SymbolInformation
mkSymInfo name symbolKind area fileUri =
  SymbolInformation name symbolKind Nothing Nothing (Location fileUri (areaToRange area)) Nothing


-- Inlay Hints --

inlayHintToJSON :: Int -> Int -> String -> Aeson.Value
inlayHintToJSON line char label =
  Aeson.object
    [ "position" .= Aeson.object ["line" .= line, "character" .= char]
    , "label"    .= label
    , "kind"     .= (1 :: Int)  -- InlayHintKind.Type = 1
    , "paddingLeft" .= True
    ]


parseInlayHintParams :: Aeson.Value -> Aeson.Parser (Uri, Int, Int)
parseInlayHintParams = Aeson.withObject "InlayHintParams" $ \o -> do
  td    <- o .: "textDocument"
  uri   <- td .: "uri"
  range <- o .: "range"
  start <- range .: "start"
  end'  <- range .: "end"
  startLine <- start .: "line"
  endLine   <- end' .: "line"
  return (Uri uri, startLine, endLine)


collectInlayHints :: Int -> Int -> [Slv.Exp] -> [Aeson.Value]
collectInlayHints startLine endLine = concatMap (collectFromExp True)
  where
    inRange area =
      let line = Loc.getLine (getStartLoc area) - 1
      in  line >= startLine && line <= endLine

    collectFromExp topLevel exp = case exp of
      -- Skip explicitly typed bindings
      Slv.Typed _ _ (Slv.TypedExp _ _ _) ->
        []

      -- Top-level or let-binding assignment (inferred type)
      Slv.Typed qt area (Slv.Assignment name body)
        | inRange area && not (isSyntheticName name) ->
          let line0 = Loc.getLine (getStartLoc area) - 1
          in  inlayHintToJSON line0 0 (sanitizeName name <> " :: " <> prettyQt topLevel qt)
              : collectFromBody body

      -- Exported assignment (inferred type)
      Slv.Typed _ _ (Slv.Export (Slv.Typed qt' innerArea (Slv.Assignment name body)))
        | inRange innerArea && not (isSyntheticName name) ->
          let line0 = Loc.getLine (getStartLoc innerArea) - 1
          in  inlayHintToJSON line0 0 (sanitizeName name <> " :: " <> prettyQt topLevel qt')
              : collectFromBody body

      -- Exported with annotation — skip
      Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.TypedExp _ _ _))) ->
        []

      _ ->
        []

    collectFromBody exp = case exp of
      Slv.Typed _ _ (Slv.Abs _ body) ->
        concatMap (collectFromExp False) body
      Slv.Typed _ _ (Slv.Do body) ->
        concatMap (collectFromExp False) body
      _ ->
        []


getInlayHints :: State -> FilePath -> Int -> Int -> LspM () [Aeson.Value]
getInlayHints state path startLine endLine = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = path }
              Driver.Don'tPrune mempty mempty (inlayHintTask startLine endLine path)
  case result of
    Just (hints, _, _) -> return hints
    Nothing            -> return []


inlayHintTask :: Int -> Int -> FilePath -> Rock.Task Query.Query [Aeson.Value]
inlayHintTask startLine endLine path = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  return $ collectInlayHints startLine endLine (Slv.aexps typedAst)


-- Semantic Tokens (Feature 5) --

-- Token type indices (must match legend advertised to client)
tokenTypeFunction, tokenTypeVariable, tokenTypeEnumMember, tokenTypeType :: Int
tokenTypeFunction   = 0
tokenTypeVariable   = 1
tokenTypeEnumMember = 2
tokenTypeType       = 3

parseSemanticTokensParams :: Aeson.Value -> Aeson.Parser Uri
parseSemanticTokensParams = Aeson.withObject "SemanticTokensParams" $ \o -> do
  td  <- o .: "textDocument"
  uri <- td .: "uri"
  return (Uri uri)

-- | Encode a list of (line, col, len, tokenType, modifiers) as LSP delta-encoded data
encodeSemanticTokens :: [(Int, Int, Int, Int, Int)] -> [Int]
encodeSemanticTokens tokens = go 0 0 (List.sortOn (\(l,c,_,_,_) -> (l,c)) tokens)
  where
    go _ _ [] = []
    go prevLine prevChar ((line, char, len, ttype, mods) : rest) =
      let deltaLine = line - prevLine
          deltaChar = if deltaLine == 0 then char - prevChar else char
      in  deltaLine : deltaChar : len : ttype : mods : go line char rest

collectSemanticTokens :: [Slv.Exp] -> [(Int, Int, Int, Int, Int)]
collectSemanticTokens = concatMap collectFromExp
  where
    collectFromExp exp = case exp of
      Slv.Typed _ area (Slv.Assignment name body)
        | not (isSyntheticName name) ->
          let line = Loc.getLine (getStartLoc area) - 1
              col  = Loc.getCol (getStartLoc area) - 1
              len  = length (sanitizeName name)
          in  (line, col, len, tokenTypeFunction, 0) : collectFromBody body

      Slv.Typed _ _ (Slv.Export inner) ->
        collectFromExp inner

      Slv.Typed _ _ (Slv.TypedExp inner _ _) ->
        collectFromExp inner

      _ ->
        []

    collectFromBody body = case body of
      Slv.Typed _ _ (Slv.Abs _ exps) ->
        concatMap collectFromExp exps
      Slv.Typed _ _ (Slv.Do exps) ->
        concatMap collectFromExp exps
      _ ->
        []

semanticTokensTask :: FilePath -> Rock.Task Query.Query [Int]
semanticTokensTask path = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  let raw = collectSemanticTokens (Slv.aexps typedAst)
  return $ encodeSemanticTokens raw

getSemanticTokens :: State -> FilePath -> LspM () Aeson.Value
getSemanticTokens state path = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = path }
              Driver.Don'tPrune mempty mempty (semanticTokensTask path)
  case result of
    Just (tokenData, _, _) ->
      return $ Aeson.object ["data" .= tokenData]
    Nothing ->
      return $ Aeson.object ["data" .= ([] :: [Int])]


-- Call Hierarchy (Feature 9) --

parsePositionParams :: Aeson.Value -> Aeson.Parser (Uri, Int, Int)
parsePositionParams = Aeson.withObject "CallHierarchyPrepareParams" $ \o -> do
  td  <- o .: "textDocument"
  uri <- td .: "uri"
  pos <- o .: "position"
  line <- pos .: "line"
  col  <- pos .: "character"
  return (Uri uri, line, col)

parseCallHierarchyItemParam :: Aeson.Value -> Aeson.Parser (String, String)
parseCallHierarchyItemParam = Aeson.withObject "CallHierarchyParams" $ \o -> do
  item <- o .: "item"
  name <- item .: "name"
  uri  <- item .: "uri"
  return (name, uri)

callHierarchyItem :: String -> String -> Int -> Int -> Aeson.Value
callHierarchyItem name uri line col =
  Aeson.object
    [ "name"           .= name
    , "kind"           .= (12 :: Int)  -- SymbolKind.Function = 12
    , "uri"            .= uri
    , "range"          .= Aeson.object
        [ "start" .= Aeson.object ["line" .= line, "character" .= col]
        , "end"   .= Aeson.object ["line" .= line, "character" .= (col + length name)]
        ]
    , "selectionRange" .= Aeson.object
        [ "start" .= Aeson.object ["line" .= line, "character" .= col]
        , "end"   .= Aeson.object ["line" .= line, "character" .= (col + length name)]
        ]
    ]

-- | Find the top-level function at the given position and return a CallHierarchyItem
prepareCallHierarchy :: State -> FilePath -> Int -> Int -> LspM () [Aeson.Value]
prepareCallHierarchy state path line col = do
  options <- buildOptions TNode
  let uriStr = T.unpack $ getUri $ pathToUri path
  result <- liftIO $ safeRunTask state options { optEntrypoint = path }
              Driver.Don'tPrune mempty mempty $ do
    (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
    let loc = Loc 0 (line + 1) (col + 1)
    return $ Maybe.mapMaybe (findTopLevelFnAt loc uriStr) (Slv.aexps typedAst)
  case result of
    Just (items, _, _) -> return items
    Nothing            -> return []

findTopLevelFnAt :: Loc -> String -> Slv.Exp -> Maybe Aeson.Value
findTopLevelFnAt loc uriStr exp = case exp of
  Slv.Typed _ area (Slv.Assignment name _)
    | not (isSyntheticName name) && areaContainsLoc area loc ->
      let l = Loc.getLine (getStartLoc area) - 1
          c = Loc.getCol (getStartLoc area) - 1
      in  Just $ callHierarchyItem (sanitizeName name) uriStr l c

  Slv.Typed _ _ (Slv.Export inner) ->
    findTopLevelFnAt loc uriStr inner

  Slv.Typed _ _ (Slv.TypedExp inner _ _) ->
    findTopLevelFnAt loc uriStr inner

  _ ->
    Nothing

areaContainsLoc :: Area -> Loc -> Bool
areaContainsLoc (Area (Loc _ sl sc) (Loc _ el ec)) (Loc _ line col) =
  (line > sl || (line == sl && col >= sc)) &&
  (line < el || (line == el && col <= ec))

-- | Find all top-level functions that call the given function name
getIncomingCalls :: State -> String -> String -> LspM () [Aeson.Value]
getIncomingCalls state targetName _targetUri = do
  options <- buildOptions TNode
  allPaths <- liftIO $ Set.toList <$> readIORef (_allModulePaths state)
  results <- forM allPaths $ \modPath -> do
    let uriStr = T.unpack $ getUri $ pathToUri modPath
    result <- liftIO $ safeRunTask state options { optEntrypoint = modPath }
                Driver.Don'tPrune mempty mempty $ do
      (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
      return $ findCallersOf targetName uriStr (Slv.aexps typedAst)
    case result of
      Just (calls, _, _) -> return calls
      Nothing            -> return []
  return $ concat results

findCallersOf :: String -> String -> [Slv.Exp] -> [Aeson.Value]
findCallersOf targetName uriStr exps = Maybe.mapMaybe check exps
  where
    check exp = case exp of
      Slv.Typed _ area (Slv.Assignment name body)
        | not (isSyntheticName name) && expCallsName targetName body ->
          let l = Loc.getLine (getStartLoc area) - 1
              c = Loc.getCol (getStartLoc area) - 1
              item = callHierarchyItem (sanitizeName name) uriStr l c
          in  Just $ Aeson.object
                [ "from"       .= item
                , "fromRanges" .= ([] :: [Aeson.Value])
                ]
      Slv.Typed _ _ (Slv.Export inner) -> check inner
      Slv.Typed _ _ (Slv.TypedExp inner _ _) -> check inner
      _ -> Nothing

expCallsName :: String -> Slv.Exp -> Bool
expCallsName name exp = case exp of
  Slv.Typed _ _ (Slv.Var n _) -> n == name
  Slv.Typed _ _ (Slv.App f arg _) -> expCallsName name f || expCallsName name arg
  Slv.Typed _ _ (Slv.Abs _ body) -> any (expCallsName name) body
  Slv.Typed _ _ (Slv.Do body) -> any (expCallsName name) body
  Slv.Typed _ _ (Slv.Assignment _ body) -> expCallsName name body
  Slv.Typed _ _ (Slv.Export inner) -> expCallsName name inner
  Slv.Typed _ _ (Slv.TypedExp inner _ _) -> expCallsName name inner
  Slv.Typed _ _ (Slv.If c t f) -> expCallsName name c || expCallsName name t || expCallsName name f
  Slv.Typed _ _ (Slv.Where body _) -> expCallsName name body
  _ -> False

-- | Find all top-level functions called from the given function body
getOutgoingCalls :: State -> String -> String -> LspM () [Aeson.Value]
getOutgoingCalls state callerName callerUri = do
  options <- buildOptions TNode
  let path = uriToPath (Uri (T.pack callerUri))
  result <- liftIO $ safeRunTask state options { optEntrypoint = path }
              Driver.Don'tPrune mempty mempty $ do
    (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
    let callerBody = Maybe.mapMaybe (findFnBody callerName) (Slv.aexps typedAst)
    let uriStr = callerUri
    return $ case callerBody of
      body : _ -> collectCallees uriStr body
      []       -> []
  case result of
    Just (calls, _, _) -> return calls
    Nothing            -> return []

findFnBody :: String -> Slv.Exp -> Maybe Slv.Exp
findFnBody name exp = case exp of
  Slv.Typed _ _ (Slv.Assignment n body) | n == name -> Just body
  Slv.Typed _ _ (Slv.Export inner) -> findFnBody name inner
  Slv.Typed _ _ (Slv.TypedExp inner _ _) -> findFnBody name inner
  _ -> Nothing

collectCallees :: String -> Slv.Exp -> [Aeson.Value]
collectCallees uriStr exp = Map.elems $ go Map.empty exp
  where
    go seen e = case e of
      Slv.Typed _ area (Slv.Var n False)
        | not (isSyntheticName n) && not (Map.member n seen) ->
          let l    = Loc.getLine (getStartLoc area) - 1
              c    = Loc.getCol (getStartLoc area) - 1
              item = Aeson.object
                       [ "to"         .= callHierarchyItem (sanitizeName n) uriStr l c
                       , "fromRanges" .= ([] :: [Aeson.Value])
                       ]
          in  Map.insert n item seen
      Slv.Typed _ _ (Slv.App f arg _) -> go (go seen f) arg
      Slv.Typed _ _ (Slv.Abs _ body) -> foldl go seen body
      Slv.Typed _ _ (Slv.Do body) -> foldl go seen body
      Slv.Typed _ _ (Slv.If c t f) -> go (go (go seen c) t) f
      Slv.Typed _ _ (Slv.Where body _) -> go seen body
      _ -> seen


-- Folding Ranges --

getFoldingRanges :: State -> FilePath -> LspM () [FoldingRange]
getFoldingRanges state path = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (foldingRangesTask path)
  case result of
    Just (ranges, _, _) -> return ranges
    Nothing             -> return []


foldingRangesTask :: FilePath -> Rock.Task Query.Query [FoldingRange]
foldingRangesTask path = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  let expRanges = Maybe.mapMaybe expToFoldingRange (Slv.aexps typedAst)
  let typeRanges = Maybe.mapMaybe typeDeclToFoldingRange (Slv.atypedecls typedAst)
  let ifaceRanges = Maybe.mapMaybe ifaceToFoldingRange (Slv.ainterfaces typedAst)
  let instanceRanges = Maybe.mapMaybe instanceToFoldingRange (Slv.ainstances typedAst)
  -- Import block folding
  let importRange = importsToFoldingRange (Slv.aimports typedAst)
  return $ Maybe.maybeToList importRange ++ expRanges ++ typeRanges ++ ifaceRanges ++ instanceRanges


areaToFoldingRange :: FoldingRangeKind -> Area -> Maybe FoldingRange
areaToFoldingRange kind (Area (Loc _ startLine startCol) (Loc _ endLine endCol)) =
  if endLine > startLine
  then Just $ FoldingRange (startLine - 1) (Just (startCol - 1)) (endLine - 1) (Just (endCol - 1)) (Just kind)
  else Nothing


expToFoldingRange :: Slv.Exp -> Maybe FoldingRange
expToFoldingRange exp = case exp of
  Slv.Typed _ area (Slv.Assignment _ _)  -> areaToFoldingRange FoldingRangeRegion area
  Slv.Typed _ area (Slv.TypedExp _ _ _)  -> areaToFoldingRange FoldingRangeRegion area
  Slv.Typed _ area (Slv.Export _)        -> areaToFoldingRange FoldingRangeRegion area
  Slv.Typed _ area (Slv.Extern _ _ _)    -> areaToFoldingRange FoldingRangeRegion area
  _ -> Nothing


typeDeclToFoldingRange :: Slv.TypeDecl -> Maybe FoldingRange
typeDeclToFoldingRange td = case td of
  Slv.Untyped area _ -> areaToFoldingRange FoldingRangeRegion area
  Slv.Typed _ area _ -> areaToFoldingRange FoldingRangeRegion area


ifaceToFoldingRange :: Slv.Interface -> Maybe FoldingRange
ifaceToFoldingRange iface = case iface of
  Slv.Untyped area _ -> areaToFoldingRange FoldingRangeRegion area
  Slv.Typed _ area _ -> areaToFoldingRange FoldingRangeRegion area


instanceToFoldingRange :: Slv.Instance -> Maybe FoldingRange
instanceToFoldingRange inst = case inst of
  Slv.Untyped area _ -> areaToFoldingRange FoldingRangeRegion area
  Slv.Typed _ area _ -> areaToFoldingRange FoldingRangeRegion area


importsToFoldingRange :: [Slv.Import] -> Maybe FoldingRange
importsToFoldingRange [] = Nothing
importsToFoldingRange imports =
  let areas = map Slv.getArea imports
      lines' = [l | Area (Loc _ l _) _ <- areas, l > 0] ++ [l | Area _ (Loc _ l _) <- areas, l > 0]
  in  case lines' of
        [] -> Nothing
        _  ->
          let startLine = max 0 (minimum lines' - 1)
              endLine = max 0 (maximum [l | Area _ (Loc _ l _) <- areas, l > 0] - 1)
          in  if endLine > startLine
              then Just $ FoldingRange startLine Nothing endLine Nothing (Just FoldingRangeImports)
              else Nothing


-- Code Actions --

diagnosticToCodeAction :: Uri -> Diagnostic -> Maybe CodeAction
diagnosticToCodeAction uri diag@(Diagnostic range _ _ _ msg tags _) =
  case tags of
    Just (List [DtUnnecessary])
      | "Unused import" `T.isPrefixOf` msg ->
        let Range (Position startLine _) _ = range
            editRange = Range (Position startLine 0) (Position (startLine + 1) 0)
            edit = TextEdit editRange ""
            wsEdit = WorkspaceEdit (Just $ HashMap.singleton uri (List [edit])) Nothing Nothing
        in  Just $ CodeAction "Remove unused import" (Just CodeActionQuickFix)
              (Just $ List [diag]) Nothing Nothing (Just wsEdit) Nothing Nothing
    _ ->
      -- Code actions not based on tags
      if "Incomplete pattern" `T.isPrefixOf` msg
      then
        case extractMissingPatterns msg of
          [] -> Nothing
          patterns ->
            let Range _ (Position endLine _) = range
                insertPos = Position endLine 0
                insertRange = Range insertPos insertPos
                branches = T.concat ["\n  " <> T.pack p <> " =>\n    ???\n" | p <- patterns]
                edit = TextEdit insertRange branches
                wsEdit = WorkspaceEdit (Just $ HashMap.singleton uri (List [edit])) Nothing Nothing
            in  Just $ CodeAction "Add missing pattern branches" (Just CodeActionQuickFix)
                  (Just $ List [diag]) Nothing Nothing (Just wsEdit) Nothing Nothing
      else if "Redundant pattern" `T.isPrefixOf` msg
      then
        let Range (Position startLine _) (Position endLine _) = range
            editRange = Range (Position startLine 0) (Position (endLine + 1) 0)
            edit = TextEdit editRange ""
            wsEdit = WorkspaceEdit (Just $ HashMap.singleton uri (List [edit])) Nothing Nothing
        in  Just $ CodeAction "Remove redundant pattern" (Just CodeActionQuickFix)
              (Just $ List [diag]) Nothing Nothing (Just wsEdit) Nothing Nothing
      else Nothing


-- | Extract missing pattern names from an "Incomplete pattern" diagnostic message.
-- Message format: "Incomplete pattern\n\nExamples of missing patterns:\n  - Constructor1\n  - Constructor2\n\nNote: ..."
extractMissingPatterns :: T.Text -> [String]
extractMissingPatterns msg =
  let ls = T.lines msg
      patternLines = filter (\l -> "  - " `T.isPrefixOf` l) ls
  in  map (T.unpack . T.drop 4) patternLines


-- | Extract the unbound variable name from a diagnostic message.
-- Message format: "Unbound variable\n\nThe variable 'name' has not been declared\n\n..."
extractUnboundName :: T.Text -> Maybe String
extractUnboundName msg
  | "Unbound variable" `T.isPrefixOf` msg =
    let parts = T.splitOn "'" msg
    in  if length parts >= 2 then Just (T.unpack (parts !! 1)) else Nothing
  | otherwise = Nothing


-- | Extract the unknown type name from a diagnostic message.
-- Message format: "Unknown type\n\nThe type 'TypeName' was not found\n\n..."
extractUnknownType :: T.Text -> Maybe String
extractUnknownType msg
  | "Unknown type" `T.isPrefixOf` msg =
    let parts = T.splitOn "'" msg
    in  if length parts >= 2 then Just (T.unpack (parts !! 1)) else Nothing
  | otherwise = Nothing


-- | Extract the member name from a namespace access at a diagnostic position.
-- Given source text and a diagnostic for unbound variable "A", looks at the text
-- right after "A" to find ".memberName" (e.g., "A.parse" -> Just "parse").
extractMemberFromSource :: String -> Diagnostic -> String -> Maybe String
extractMemberFromSource fileContent (Diagnostic (Range (Position line col) _) _ _ _ _ _ _) name =
  let fileLines = lines fileContent
  in  if line < 0 || fromIntegral line >= length fileLines then Nothing
      else
        let ln = fileLines !! fromIntegral line
            afterName = drop (fromIntegral col + length name) ln
        in  case afterName of
              ('.':rest) ->
                let member = takeWhile (\c -> isAlphaNum c || c == '_') rest
                in  if null member then Nothing else Just member
              _ -> Nothing


-- | Get "Add missing import" code actions for unbound variable and unknown type diagnostics.
getAddImportActions :: State -> FilePath -> Uri -> [Diagnostic] -> String -> LspM () [CodeAction]
getAddImportActions state path _uri diags fileContent = do
  let unboundDiags = [(d, name) | d@(Diagnostic _ _ _ _ msg _ _) <- diags, Just name <- [extractUnboundName msg]]
  let unknownTypeDiags = [(d, name) | d@(Diagnostic _ _ _ _ msg _ _) <- diags, Just name <- [extractUnknownType msg]]
  -- For capitalized unbound vars, try to extract the member name from source (e.g., A.parse -> "parse")
  let unboundWithMembers = map (\(d, name) ->
        if not (null name) && isUpper (head name)
        then (d, name, extractMemberFromSource fileContent d name)
        else (d, name, Nothing)
        ) unboundDiags
  if null unboundDiags && null unknownTypeDiags then return []
  else do
    options <- buildOptions TNode
    bgDone <- liftIO $ readIORef (_backgroundDone state)
    extraPaths <- if bgDone
      then liftIO $ Set.toList <$> readIORef (_allModulePaths state)
      else return []
    results <- liftIO $ safeRunTask state options { optEntrypoint = path }
                 Driver.Don'tPrune mempty mempty (addImportActionsTask path unboundWithMembers unknownTypeDiags extraPaths)
    case results of
      Just (actions, _, _) -> return actions
      Nothing              -> return []


-- | Rock task that searches all compiled modules for exports matching unbound names and unknown types.
-- unboundDiags contains (diagnostic, name, maybeMemberName) where maybeMemberName is extracted from source
-- e.g., for "A.parse(...)", name="A" and maybeMemberName=Just "parse"
addImportActionsTask :: FilePath -> [(Diagnostic, String, Maybe String)] -> [(Diagnostic, String)] -> [FilePath] -> Rock.Task Query.Query [CodeAction]
addImportActionsTask path unboundDiags unknownTypeDiags extraPaths = do
  localModulePaths <- Rock.fetch $ Query.ModulePathsToBuild path
  let modulePaths = List.nub $ localModulePaths ++ extraPaths

  -- Build a global mapping: absolute path -> relative import path
  -- by collecting all imports from all modules
  -- Also build a mapping: default import alias -> [(absPath, relPath)]
  -- so we can suggest imports for aliases like "import L from 'List'" when L.find is used
  allImportInfo <- fmap concat $ forM modulePaths $ \modPath -> do
    (modAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
    return [(imp, Slv.getImportAbsolutePath imp, Slv.getImportPath imp) | imp <- Slv.aimports modAst]

  let absToRelPath = Map.fromList [(absP, relP) | (_, absP, relP) <- allImportInfo]

  -- Build alias -> [(absPath, relPath)] from DefaultImport entries
  let defaultImportAliases = Map.fromListWith (++) $ Maybe.mapMaybe
        (\(imp, absP, relP) -> case imp of
          Slv.Untyped _ (Slv.DefaultImport (Slv.Untyped _ alias) _ _) -> Just (alias, [(absP, relP)])
          Slv.Typed _ _ (Slv.DefaultImport (Slv.Untyped _ alias) _ _) -> Just (alias, [(absP, relP)])
          Slv.Untyped _ (Slv.DefaultImport (Slv.Typed _ _ alias) _ _) -> Just (alias, [(absP, relP)])
          Slv.Typed _ _ (Slv.DefaultImport (Slv.Typed _ _ alias) _ _) -> Just (alias, [(absP, relP)])
          _ -> Nothing
        ) allImportInfo

  -- For each module, get its exports and constructor names
  moduleExports <- fmap concat $ forM modulePaths $ \modPath ->
    if modPath == path then return []
    else do
      (modAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
      let exports = Map.keys $ Slv.extractExportedExps modAst
      let exportedADTs = Slv.extractExportedADTs modAst
      let constructorNames = concatMap getConstructorNames exportedADTs
      return [(name, modPath) | name <- exports ++ constructorNames]

  -- For each module, get its exported type names
  moduleTypeExports <- fmap concat $ forM modulePaths $ \modPath ->
    if modPath == path then return []
    else do
      (modAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
      let exportedADTs = Slv.extractExportedADTs modAst
      let getADTName (Slv.Untyped _ adt) = Slv.adtname adt
          getADTName (Slv.Typed _ _ adt) = Slv.adtname adt
      let typeNames = map (\td -> (getADTName td, modPath)) exportedADTs
      let exportedAliases = Slv.extractExportedAliases modAst
      let getAliasName (Slv.Untyped _ alias) = Slv.aliasname alias
          getAliasName (Slv.Typed _ _ alias) = Slv.aliasname alias
      let aliasNames = map (\td -> (getAliasName td, modPath)) exportedAliases
      return $ typeNames ++ aliasNames

  let insertRange = Range (Position 0 0) (Position 0 0)

  -- Helper to make a named import code action
  let makeNamedImportAction diag name modPath =
        case Map.lookup modPath absToRelPath of
          Just relPath ->
            let importText = "import { " <> T.pack name <> " } from \"" <> T.pack relPath <> "\"\n"
                edit = TextEdit insertRange importText
                wsEdit = WorkspaceEdit (Just $ HashMap.singleton (pathToUri path) (List [edit])) Nothing Nothing
                title = "Add import { " <> T.pack name <> " } from \"" <> T.pack relPath <> "\""
            in  [CodeAction title (Just CodeActionQuickFix) (Just $ List [diag]) Nothing Nothing (Just wsEdit) Nothing Nothing]
          Nothing -> []

  -- Helper to make a type import code action (import type { ... } from "...")
  let makeTypeImportAction diag name modPath =
        case Map.lookup modPath absToRelPath of
          Just relPath ->
            let importText = "import type { " <> T.pack name <> " } from \"" <> T.pack relPath <> "\"\n"
                edit = TextEdit insertRange importText
                wsEdit = WorkspaceEdit (Just $ HashMap.singleton (pathToUri path) (List [edit])) Nothing Nothing
                title = "Add import type { " <> T.pack name <> " } from \"" <> T.pack relPath <> "\""
            in  [CodeAction title (Just CodeActionQuickFix) (Just $ List [diag]) Nothing Nothing (Just wsEdit) Nothing Nothing]
          Nothing -> []

  -- Helper to find default import suggestions for a namespace alias.
  -- Looks up the alias in defaultImportAliases (from existing imports across the project),
  -- and also checks if any module basename matches the alias.
  -- This handles both "import List from 'List'" and "import L from 'List'" patterns.
  let findDefaultImportsForAlias alias =
        let fromExistingAliases = case Map.lookup alias defaultImportAliases of
              Just entries -> List.nub [(absP, relP) | (absP, relP) <- entries, absP /= path]
              Nothing      -> []
            fromBasename = [(modPath, relP) | modPath <- modulePaths
                          , modPath /= path
                          , dropExtension (takeFileName modPath) == alias
                          , let relP = Map.findWithDefault (dropExtension (takeFileName modPath)) modPath absToRelPath]
        in  List.nubBy (\(a, _) (b, _) -> a == b) (fromExistingAliases ++ fromBasename)

  -- Helper to make a default import code action
  let makeDefaultImportAction diag alias relPath =
        let importText = "import " <> T.pack alias <> " from \"" <> T.pack relPath <> "\"\n"
            edit = TextEdit insertRange importText
            wsEdit = WorkspaceEdit (Just $ HashMap.singleton (pathToUri path) (List [edit])) Nothing Nothing
            title = "Add import " <> T.pack alias <> " from \"" <> T.pack relPath <> "\""
        in  CodeAction title (Just CodeActionQuickFix) (Just $ List [diag]) Nothing Nothing (Just wsEdit) Nothing Nothing

  -- Generate code actions for unbound variables
  let unboundActions = concatMap (\(diag, name, maybeMember) ->
        if not (null name) && isUpper (head name)
        then
          -- Capitalized name: likely a namespace (e.g., "A" from "A.parse(...)").
          -- First try alias mappings and basename matching
          let aliasMatches = findDefaultImportsForAlias name
              aliasActions = map (\(_, relPath) -> makeDefaultImportAction diag name relPath) aliasMatches
              -- Also check if it's an exported constructor or value
              exportMatches = [modPath | (n, modPath) <- moduleExports, n == name]
              exportActions = concatMap (makeNamedImportAction diag name) exportMatches
          in  if not (null aliasActions) || not (null exportActions)
              then aliasActions ++ exportActions
              else
                -- No alias/basename match found. If we have a member name (e.g., "parse" from "A.parse"),
                -- search all modules for that export and suggest default import under this alias.
                case maybeMember of
                  Just member ->
                    let memberMatches = List.nub [modPath | (n, modPath) <- moduleExports, n == member]
                    in  concatMap (\modPath ->
                          case Map.lookup modPath absToRelPath of
                            Just relPath -> [makeDefaultImportAction diag name relPath]
                            Nothing ->
                              let relPath = dropExtension (takeFileName modPath)
                              in  [makeDefaultImportAction diag name relPath]
                        ) memberMatches
                  Nothing -> []
        else
          -- Lowercase name: regular named import
          let matching = [modPath | (n, modPath) <- moduleExports, n == name]
          in  concatMap (makeNamedImportAction diag name) matching
        ) unboundDiags

  -- Generate code actions for unknown types
  let typeActions = concatMap (\(diag, typeName) ->
        if '.' `elem` typeName
        then
          -- Qualified type (e.g., "A.SomeType"): suggest default import for namespace
          let namespace = takeWhile (/= '.') typeName
              memberType = drop 1 $ dropWhile (/= '.') typeName
              -- First try alias mappings and basename matching
              aliasMatches = findDefaultImportsForAlias namespace
              aliasActions = map (\(_, relPath) -> makeDefaultImportAction diag namespace relPath) aliasMatches
          in  if not (null aliasActions) then aliasActions
              else
                -- No alias match: search all modules for the member type name
                let typeMatches = List.nub [modPath | (n, modPath) <- moduleTypeExports, n == memberType]
                in  concatMap (\modPath ->
                      case Map.lookup modPath absToRelPath of
                        Just relPath -> [makeDefaultImportAction diag namespace relPath]
                        Nothing ->
                          let relPath = dropExtension (takeFileName modPath)
                          in  [makeDefaultImportAction diag namespace relPath]
                    ) typeMatches
        else
          -- Simple type name: search exported types and suggest type import
          let matching = [modPath | (n, modPath) <- moduleTypeExports, n == typeName]
          in  concatMap (makeTypeImportAction diag typeName) matching
        ) unknownTypeDiags

  return $ unboundActions ++ typeActions
  where
    getConstructorNames :: Slv.TypeDecl -> [String]
    getConstructorNames (Slv.Untyped _ (Slv.ADT { Slv.adtconstructors = ctors, Slv.adtexported = True })) =
      map getCtorName ctors
    getConstructorNames (Slv.Typed _ _ (Slv.ADT { Slv.adtconstructors = ctors, Slv.adtexported = True })) =
      map getCtorName ctors
    getConstructorNames _ = []

    getCtorName :: Slv.Constructor -> String
    getCtorName (Slv.Typed _ _ (Slv.Constructor name _ _)) = name
    getCtorName (Slv.Untyped _ (Slv.Constructor name _ _)) = name


-- Signature Help --

getSignatureHelp :: State -> Loc -> FilePath -> LspM () SignatureHelp
getSignatureHelp state loc path = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (signatureHelpTask loc path)
  case result of
    Just (Just help, _, _) -> return help
    _                      -> return $ SignatureHelp (List []) Nothing Nothing


signatureHelpTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe SignatureHelp)
signatureHelpTask loc path = do
  srcAst        <- Rock.fetch $ Query.ParsedAST path
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  -- Find the enclosing App node — the cursor is inside the argument of a function call
  let result = findEnclosingApp loc (Slv.aexps typedAst ++ Slv.getAllMethods typedAst)
  case result of
    Just (name, qt) ->
      let typeStr = prettyQt False qt
          paramTypes = getParamTypes (getQualified qt)
          params = map (\t -> ParameterInformation (ParameterLabelString $ T.pack $ prettyPrintType False t) Nothing) paramTypes
          sigInfo = SignatureInformation (T.pack $ name <> " :: " <> typeStr) Nothing (Just $ List params) (Just 0)
      in  return $ Just $ SignatureHelp (List [sigInfo]) (Just 0) (Just 0)
    Nothing ->
      -- Fall back: if cursor is directly on a function name, show its signature
      let maybeNode = findNodeInAst loc srcAst typedAst
      in  case maybeNode of
            Just (ExpNode _ (Slv.Typed qt _ (Slv.Var name _))) | not (null (getParamTypes (getQualified qt))) ->
              let typeStr = prettyQt False qt
                  paramTypes = getParamTypes (getQualified qt)
                  params = map (\t -> ParameterInformation (ParameterLabelString $ T.pack $ prettyPrintType False t) Nothing) paramTypes
                  sigInfo = SignatureInformation (T.pack $ name <> " :: " <> typeStr) Nothing (Just $ List params) (Just 0)
              in  return $ Just $ SignatureHelp (List [sigInfo]) (Just 0) (Just 0)
            _ -> return Nothing


-- | Find the outermost App node that contains the cursor in one of its arguments,
-- and return the function name and type at the call site.
findEnclosingApp :: Loc -> [Slv.Exp] -> Maybe (String, Qual Type)
findEnclosingApp loc = go
  where
    go [] = Nothing
    go (e:es) = findInExp e <|> go es

    findInExp :: Slv.Exp -> Maybe (String, Qual Type)
    findInExp e = case e of
      Slv.Typed _ area (Slv.App fn arg _) | isInRange loc area ->
        -- Recurse into arg first (deeper nesting wins)
        findInExp arg
        -- Then check if cursor is in the argument position
        <|> (if isInRange loc (Slv.getArea arg) then extractFnInfo fn else Nothing)
        -- Then recurse into fn (for curried applications)
        <|> findInExp fn
      Slv.Typed _ area (Slv.Assignment _ body) | isInRange loc area ->
        findInExp body
      Slv.Typed _ area (Slv.Export inner) | isInRange loc area ->
        findInExp inner
      Slv.Typed _ area (Slv.TypedExp inner _ _) | isInRange loc area ->
        findInExp inner
      Slv.Typed _ area (Slv.Abs _ body) | isInRange loc area ->
        go body
      Slv.Typed _ area (Slv.If cond thenE elseE) | isInRange loc area ->
        findInExp cond <|> findInExp thenE <|> findInExp elseE
      Slv.Typed _ area (Slv.Where inner iss) | isInRange loc area ->
        findInExp inner <|> foldl' (\acc is -> acc <|> findInIs is) Nothing iss
      Slv.Typed _ area (Slv.Do exps) | isInRange loc area ->
        go exps
      _ -> Nothing

    findInIs :: Slv.Is -> Maybe (String, Qual Type)
    findInIs (Slv.Typed _ _ (Slv.Is _ body)) = findInExp body
    findInIs _ = Nothing

    extractFnInfo :: Slv.Exp -> Maybe (String, Qual Type)
    extractFnInfo fn = case fn of
      Slv.Typed qt _ (Slv.Var name _)       -> Just (name, qt)
      Slv.Typed _ _ (Slv.App inner _ _)     -> extractFnInfo inner
      _                                       -> Nothing


-- Find References --

findReferences :: State -> Loc -> FilePath -> LspM () [Location]
findReferences state loc path = do
  options <- buildOptions TNode
  bgDone <- liftIO $ readIORef (_backgroundDone state)
  extraPaths <- if bgDone
    then liftIO $ Set.toList <$> readIORef (_allModulePaths state)
    else return []
  result <- liftIO $ safeRunTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (findReferencesTask loc path extraPaths)
  case result of
    Just (refs, _, _) -> return refs
    Nothing           -> return []


findReferencesTask :: Loc -> FilePath -> [FilePath] -> Rock.Task Query.Query [Location]
findReferencesTask loc path extraPaths = do
  srcAst        <- Rock.fetch $ Query.ParsedAST path
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  let maybeNode = findNodeInAst loc srcAst typedAst
  case maybeNode >>= nodeToName of
    Just name -> do
      -- Search current module
      let localAreas = collectAllOccurrences name typedAst
      let localRefs = map (\area -> Location (pathToUri path) (areaToRange area)) localAreas

      -- Search all other compiled modules for cross-module references
      -- In other modules, the name may be namespace-qualified (e.g., "MMOMath.degToRad")
      let nameSuffix = "." ++ name
      localModulePaths <- Rock.fetch $ Query.ModulePathsToBuild path
      let modulePaths = List.nub $ localModulePaths ++ extraPaths
      crossRefs <- fmap concat $ forM modulePaths $ \modPath ->
        if modPath == path then return []
        else do
          (modAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv modPath
          -- Search for both exact name and namespace-qualified name (suffix match)
          let areas = collectAllOccurrencesWithSuffix name nameSuffix modAst
          return $ map (\area -> Location (pathToUri modPath) (areaToRange area)) areas

      return $ localRefs ++ crossRefs
    Nothing -> return []


-- Rename Symbol --

renameSymbol :: State -> Loc -> FilePath -> String -> LspM () WorkspaceEditMap
renameSymbol state loc path newName = do
  options <- buildOptions TNode
  result <- liftIO $ safeRunTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (renameSymbolTask loc path newName)
  case result of
    Just (edits, _, _) -> return edits
    Nothing            -> return HashMap.empty


renameSymbolTask :: Loc -> FilePath -> String -> Rock.Task Query.Query WorkspaceEditMap
renameSymbolTask loc path newName = do
  srcAst        <- Rock.fetch $ Query.ParsedAST path
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  let maybeNode = findNodeInAst loc srcAst typedAst
  case maybeNode >>= nodeToName of
    Just name ->
      let areas = collectAllOccurrences name typedAst
          edits = map (\area -> TextEdit (areaToRange area) (T.pack newName)) areas
      in  return $ HashMap.singleton (pathToUri path) (List edits)
    Nothing -> return HashMap.empty


-- | Extract the name from a Node, if it has one
nodeToName :: Node -> Maybe String
nodeToName node = case node of
  ExpNode _ (Slv.Typed _ _ (Slv.Assignment name _))     -> Just name
  ExpNode _ (Slv.Typed _ _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _)) -> Just name
  ExpNode _ (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) -> Just name
  ExpNode _ (Slv.Typed _ _ (Slv.Var name _))            -> Just name
  ExpNode _ (Slv.Typed _ _ (Slv.Extern _ name _))       -> Just name
  NameNode _ (Slv.Typed _ _ name)                        -> Just name
  PatternNode (Slv.Typed _ _ (Slv.PVar name))           -> Just name
  _                                                       -> Nothing


-- | Collect all areas where a given name occurs in the solved AST
collectAllOccurrences :: String -> Slv.AST -> [Area]
collectAllOccurrences name = collectAllOccurrencesBy (== name)


-- | Collect all areas where a name matches exactly or as a namespace-qualified suffix.
-- For example, matching "degToRad" will also match "MMOMath.degToRad".
collectAllOccurrencesWithSuffix :: String -> String -> Slv.AST -> [Area]
collectAllOccurrencesWithSuffix name nameSuffix =
  collectAllOccurrencesBy (\n -> n == name || nameSuffix `List.isSuffixOf` n)


collectAllOccurrencesBy :: (String -> Bool) -> Slv.AST -> [Area]
collectAllOccurrencesBy matches ast =
  concatMap (collectInExp matches) (Slv.aexps ast ++ Slv.getAllMethods ast)


-- | Compute an area covering only the name portion at the start of a larger area.
nameOnlyArea :: Area -> String -> Area
nameOnlyArea (Area (Loc a l c) _) name =
    Area (Loc a l c) (Loc (a + length name) l (c + length name))


collectInExp :: (String -> Bool) -> Slv.Exp -> [Area]
collectInExp matches exp = case exp of
  Slv.Typed _ area (Slv.Var n _) | matches n -> [area]
  Slv.Typed _ area (Slv.Assignment n body) ->
    (if matches n then [nameOnlyArea area n] else []) ++ collectInExp matches body
  Slv.Typed _ _ (Slv.App fn arg _) ->
    collectInExp matches fn ++ collectInExp matches arg
  Slv.Typed _ _ (Slv.Abs param body) ->
    (if matches (Slv.getValue param) then [Slv.getArea param] else [])
    ++ concatMap (collectInExp matches) body
  Slv.Typed _ _ (Slv.If cond thenE elseE) ->
    collectInExp matches cond ++ collectInExp matches thenE ++ collectInExp matches elseE
  Slv.Typed _ _ (Slv.Where exp iss) ->
    collectInExp matches exp ++ concatMap (collectInIs matches) iss
  Slv.Typed _ _ (Slv.Export inner) ->
    collectInExp matches inner
  Slv.Typed _ _ (Slv.TypedExp inner _ _) ->
    collectInExp matches inner
  Slv.Typed _ _ (Slv.ListConstructor items) ->
    concatMap (\item -> case item of
      Slv.Typed _ _ (Slv.ListItem e)   -> collectInExp matches e
      Slv.Typed _ _ (Slv.ListSpread e) -> collectInExp matches e
      _ -> []) items
  Slv.Typed _ _ (Slv.TupleConstructor items) ->
    concatMap (collectInExp matches) items
  Slv.Typed _ _ (Slv.Record fields) ->
    concatMap (\f -> case f of
      Slv.Typed _ _ (Slv.Field (_, e))   -> collectInExp matches e
      Slv.Typed _ _ (Slv.FieldSpread e)  -> collectInExp matches e
      _ -> []) fields
  Slv.Typed _ _ (Slv.Access e _) ->
    collectInExp matches e
  Slv.Typed _ _ (Slv.ArrayAccess e idx) ->
    collectInExp matches e ++ collectInExp matches idx
  Slv.Typed _ _ (Slv.Do exps) ->
    concatMap (collectInExp matches) exps
  Slv.Typed _ _ (Slv.While cond body) ->
    collectInExp matches cond ++ collectInExp matches body
  Slv.Typed _ _ (Slv.Mutate target val) ->
    collectInExp matches target ++ collectInExp matches val
  Slv.Typed _ _ (Slv.TemplateString parts) ->
    concatMap (collectInExp matches) parts
  Slv.Typed _ area (Slv.NameExport n) | matches n -> [area]
  Slv.Typed _ area (Slv.Extern _ n _) | matches n -> [area]
  _ -> []


collectInIs :: (String -> Bool) -> Slv.Is -> [Area]
collectInIs matches is = case is of
  Slv.Typed _ _ (Slv.Is pat exp) ->
    collectInPattern matches pat ++ collectInExp matches exp
  _ -> []


collectInPattern :: (String -> Bool) -> Slv.Pattern -> [Area]
collectInPattern matches pat = case pat of
  Slv.Typed _ area (Slv.PVar n) | matches n -> [area]
  Slv.Typed _ _ (Slv.PCon _ pats) ->
    concatMap (collectInPattern matches) pats
  Slv.Typed _ _ (Slv.PRecord m _) ->
    concatMap (collectInPattern matches) (Map.elems m)
  Slv.Typed _ _ (Slv.PList pats) ->
    concatMap (collectInPattern matches) pats
  Slv.Typed _ _ (Slv.PTuple pats) ->
    concatMap (collectInPattern matches) pats
  Slv.Typed _ _ (Slv.PSpread inner) ->
    collectInPattern matches inner
  _ -> []


-- Background Compilation --

-- | Recursively find all .mad files under a directory
discoverProjectModules :: FilePath -> IO [FilePath]
discoverProjectModules rootPath = do
  let srcDir = rootPath </> "src"
  exists <- Dir.doesDirectoryExist srcDir
  if exists then findMadFiles srcDir else return []
  where
    findMadFiles dir = do
      entries <- Dir.listDirectory dir
      fmap concat $ forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- Dir.doesDirectoryExist path
        if isDir then findMadFiles path
        else if takeExtension path == ".mad" then do
          absPath <- Dir.canonicalizePath path
          return [absPath]
        else return []


-- | Read madlib.json to find the project's main entrypoint
getProjectEntrypoint :: FilePath -> IO (Maybe FilePath)
getProjectEntrypoint rootPath = do
  let madlibJsonPath = rootPath </> "madlib.json"
  exists <- Dir.doesFileExist madlibJsonPath
  if exists then do
    content <- BSL.readFile madlibJsonPath
    case Aeson.decode content of
      Just (mdj :: MadlibDotJson.MadlibDotJson) -> do
        let mainPath = rootPath </> MadlibDotJson.main mdj
        mainExists <- Dir.doesFileExist mainPath
        if mainExists then do
          absPath <- Dir.canonicalizePath mainPath
          return (Just absPath)
        else return Nothing
      Nothing -> return Nothing
  else return Nothing


-- | Compile all project modules in the background to warm the Rock cache
backgroundCompileProject :: State -> State -> LanguageContextEnv () -> IO ()
backgroundCompileProject state autocompletionState env = do
  rootPath <- runLspT env $ Maybe.fromMaybe "./" <$> getRootPath

  -- Discover all .mad files in src/
  projectModules <- discoverProjectModules rootPath

  -- Get main entrypoint - compile it first for maximum transitive coverage
  maybeMain <- getProjectEntrypoint rootPath
  let orderedModules = case maybeMain of
        Just main -> main : filter (/= main) projectModules
        Nothing   -> projectModules

  -- Compile each module (both targets), yielding lock between batches
  compiledPaths <- newIORef Set.empty

  forM_ (chunksOf 5 orderedModules) $ \batch -> do
    forM_ batch $ \modPath -> do
      forM_ [TNode, TLLVM] $ \target -> do
        options <- runLspT env $ buildOptions target
        result <- try $ runTask state options { optEntrypoint = modPath }
                    Driver.Don'tPrune mempty mempty (typeCheckFileTask modPath)
                  :: IO (Either SomeException (Bool, [CompilationWarning], [CompilationError]))
        case result of
          Right _ -> atomicModifyIORef' compiledPaths (\s -> (Set.insert modPath s, ()))
          Left _  -> return ()

  -- Build the full set of known module paths (including transitive deps and prelude)
  compiled <- readIORef compiledPaths
  allPaths <- newIORef compiled
  forM_ (Set.toList compiled) $ \modPath -> do
    options <- runLspT env $ buildOptions TNode
    result <- try $ runTask state options { optEntrypoint = modPath }
                Driver.Don'tPrune mempty mempty (Rock.fetch $ Query.ModulePathsToBuild modPath)
              :: IO (Either SomeException ([FilePath], [CompilationWarning], [CompilationError]))
    case result of
      Right (paths, _, _) -> atomicModifyIORef' allPaths (\s -> (Set.union s (Set.fromList paths), ()))
      Left _ -> return ()

  finalPaths <- readIORef allPaths
  atomicWriteIORef (_allModulePaths state) finalPaths
  atomicWriteIORef (_backgroundDone state) True

  -- Copy to autocompletion state
  copyStateTo (_jsDriverState state) (_jsDriverState autocompletionState)
  copyStateTo (_llvmDriverState state) (_llvmDriverState autocompletionState)

  runLspT env $ sendNotification SWindowLogMessage
    (LogMessageParams MtInfo $ T.pack $ "Background compilation complete: " ++ show (Set.size finalPaths) ++ " modules")


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t


-- Server Entry Point --

runLanguageServer :: IO ()
runLanguageServer = do
  jsDriverState <- Driver.initialState
  llvmDriverState <- Driver.initialState
  openFiles <- newIORef mempty
  debounceRef <- newIORef Nothing
  stateLock <- newMVar ()
  allModulePaths <- newIORef Set.empty
  backgroundDone <- newIORef False
  interfaceSnapshots <- newIORef Map.empty
  let state = State jsDriverState llvmDriverState openFiles mempty debounceRef stateLock allModulePaths backgroundDone interfaceSnapshots

  autocompletionJsDriverState <- Driver.initialState
  autocompletionLlvmDriverState <- Driver.initialState
  autocompletionOpenFiles <- newIORef mempty
  autocompletionDebounceRef <- newIORef Nothing
  autocompletionStateLock <- newMVar ()
  autocompletionAllModulePaths <- newIORef Set.empty
  autocompletionBackgroundDone <- newIORef False
  autocompletionInterfaceSnapshots <- newIORef Map.empty
  let autocompletionState = State autocompletionJsDriverState autocompletionLlvmDriverState autocompletionOpenFiles mempty autocompletionDebounceRef autocompletionStateLock autocompletionAllModulePaths autocompletionBackgroundDone autocompletionInterfaceSnapshots
  runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers state autocompletionState
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions { textDocumentSync = Just textDocumentSyncOptions }
    }
  return ()


-- Concurrency Utilities --

pooledForConcurrently_ ::
  (Foldable t, MonadBaseControl IO m) =>
  t a ->
  (a -> m b) ->
  m ()
pooledForConcurrently_ as f =
  liftBaseWith $ \runInIO ->
    pooledForConcurrentlyIO_ as (runInIO . f)

pooledForConcurrentlyIO_ ::
  Foldable t =>
  t a ->
  (a -> IO b) ->
  IO ()
pooledForConcurrentlyIO_ as f = do
  todoRef <- newIORef $ toList as
  processCount <- getNumCapabilities
  let go =
        join $
          atomicModifyIORef' todoRef $ \todo ->
            case todo of
              [] ->
                (todo, pure ())
              (a : todo') ->
                ( todo'
                , do
                    _ <- f a
                    go
                )
  replicateConcurrently_ (max 8 processCount) go


pooledForConcurrentlyIO ::
  Traversable t =>
  t a ->
  (a -> IO b) ->
  IO (t b)
pooledForConcurrentlyIO as f = do
  jobs <- forM as $ \a -> do
    ref <- newIORef $ error "pooledForConcurrently not done"
    pure (a, ref)
  todoRef <- newIORef $ toList jobs
  processCount <- getNumCapabilities
  let go =
        join $
          atomicModifyIORef' todoRef $ \todo ->
            case todo of
              [] ->
                (todo, pure ())
              ((a, ref) : todo') ->
                ( todo'
                , do
                    result <- f a
                    atomicWriteIORef ref result
                    go
                )
  replicateConcurrently_ (max 8 processCount) go
  forM jobs $ \(_, ref) ->
    readIORef ref

pooledForConcurrently ::
  (Traversable t, MonadBaseControl IO m, StM m b ~ b) =>
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrently as f =
  liftBaseWith $ \runInIO ->
    pooledForConcurrentlyIO as (runInIO . f)
