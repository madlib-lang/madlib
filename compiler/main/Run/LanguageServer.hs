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
module Run.LanguageServer where

import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import Control.Monad.IO.Class
import qualified Data.Text as T
import Control.Monad.Base
import qualified Rock
import Driver.Rules
import qualified Driver.Query as Query
import Text.Show.Pretty (ppShow)
import qualified Driver
import Error.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error.Context
import Explain.Location
import qualified Explain.Location as Loc
import Language.LSP.Diagnostics
import qualified Explain.Format as Explain
import           Data.List (group, foldl')
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
import qualified Canonicalize.Typing as Can
import qualified Infer.Typing as Slv
import qualified Canonicalize.Env as CanEnv
import qualified Canonicalize.Interface as Can
import System.FilePath (takeFileName, dropExtension, takeExtension, (</>))
import qualified Data.HashMap.Strict as HashMap
import Run.OptimizationLevel
import Data.Maybe (isJust)
import GHC.Base (when)
import Language.LSP.VFS (virtualFileText)
import qualified Infer.Env as SlvEnv
import Data.Char (isAlphaNum, isDigit, isUpper, toLower)
import qualified System.Directory as Dir
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import qualified MadlibDotJson.MadlibDotJson as MadlibDotJson



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
  ]


data AutocompletionKind
  = AutocompletingName String
  | AutocompletingRecordAccess String String
  | AutocompletingNamespaceAccess String
  deriving(Eq, Show)


computeAutocompletionKind :: String -> AutocompletionKind
computeAutocompletionKind reversedCurrentLine =
  let consumeChars chars = case chars of
        c : cs | isAlphaNum c ->
          c : consumeChars cs

        _ ->
          []
      readName chars =
        let _name = reverse $ consumeChars chars
            _rest = drop (length _name) chars
        in  (dropWhile isDigit _name, _rest)
      (name, rest) = readName reversedCurrentLine
  in  case rest of
        '.' : afterDot ->
          let (recordName, rest') = readName afterDot
          in  case rest' of
                _ | not (null recordName) && isUpper (head recordName) ->
                  AutocompletingNamespaceAccess recordName


                _ ->
                  AutocompletingRecordAccess recordName name

        _ ->
          AutocompletingName name



buildOptions :: Target -> LspM () Options.Options
buildOptions target = do
  maybeRootPath <- getRootPath
  let rootPath = Maybe.fromMaybe "./" maybeRootPath
  return
    Options.Options
      { Options.optEntrypoint = ""
      , Options.optTarget = target
      , Options.optRootPath = rootPath
      , Options.optOutputPath = ""
      , Options.optOptimized = False
      , Options.optPathUtils = PathUtils.defaultPathUtils
      , Options.optBundle = False
      , Options.optCoverage = False
      , Options.optGenerateDerivedInstances = False
      , Options.optInsertInstancePlaholders = False
      , Options.optMustHaveMain = False
      , Options.optParseOnly = False
      , Options.optOptimizationLevel = O1
      , Options.optLspMode = True
      , Options.optEmitLLVM = False
      , Options.optDebug = False
      }


recordAndPrintDuration :: T.Text -> LspM a b -> LspM a b
recordAndPrintDuration title action = do
  startT       <- liftIO getCurrentTime
  actionResult <- action
  endT         <- liftIO getCurrentTime
  let diff = diffUTCTime endT startT
  let (ms, _) = properFraction $ diff * 1000
  sendNotification SWindowLogMessage $ LogMessageParams MtInfo (title <> " - duration: " <> T.pack (show ms <> "ms"))
  return actionResult


isInRange :: Loc -> Area -> Bool
isInRange (Loc _ l c) (Area (Loc _ lstart cstart) (Loc _ lend cend)) =
  (l >= lstart && l <= lend)
  && (not (l == lstart && c < cstart) && not (l == lend && c > cend))


isLocAfter :: Loc -> Area -> Bool
isLocAfter (Loc _ l _) (Area _ (Loc _ lend _)) =
  l > lend

isLocAfterStart :: Loc -> Area -> Bool
isLocAfterStart (Loc _ l _) (Area (Loc _ lstart _) _) =
  l > lstart


prettyQt :: Bool -> Qual Type -> String
prettyQt topLevel qt@(_ :=> t)
  | qt == failedQt = "_"
  | topLevel       = let (r, _) = renderSchemesWithDiff False (Forall [] qt) (Forall [] qt) in r
  | otherwise      = let (r, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t)) in r


prettyScheme :: Scheme -> String
prettyScheme sc =
  let (r, _) = renderSchemesWithDiff False sc sc in r


data Node
  = ExpNode Bool Slv.Exp
  | NameNode Bool (Slv.Solved String)
  | PatternNode Slv.Pattern
  | TypingNode (Maybe Slv.Typing) Slv.Typing
  | RecordFieldAnnotation Area String Slv.Typing
  | ADTNode String Area Kind
  | DefaultImportNode Area FilePath
  | NamedImportNode Area String FilePath
  | TypeImportNode Area String FilePath
  deriving(Eq, Show)


getNodeLine :: Node -> Int
getNodeLine n = case n of
  ExpNode _ (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  ExpNode _ (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  NameNode _ (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  NameNode _ (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  PatternNode (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  PatternNode (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  TypingNode _ (Slv.Typed _ (Area (Loc _ l _) _) _) ->
    l

  TypingNode _ (Slv.Untyped (Area (Loc _ l _) _) _) ->
    l

  ADTNode _ (Area (Loc _ l _) _) _ ->
    l

  RecordFieldAnnotation (Area (Loc _ l _) _) _ _ ->
    l

  DefaultImportNode (Area (Loc _ l _) _) _ ->
    l

  NamedImportNode (Area (Loc _ l _) _) _ _ ->
    l

  TypeImportNode (Area (Loc _ l _) _) _ _ ->
    l



findNodeAtLocInListItem :: Loc -> Slv.ListItem -> Maybe Node
findNodeAtLocInListItem loc li = case li of
  Slv.Typed _ _ (Slv.ListItem exp) ->
    findNodeAtLoc False loc exp

  Slv.Typed _ _ (Slv.ListSpread exp) ->
    findNodeAtLoc False loc exp

  _ ->
    Nothing


findNodeAtLocInField :: Loc -> Slv.Field -> Maybe Node
findNodeAtLocInField loc field = case field of
  Slv.Typed _ _ (Slv.Field (_, exp)) ->
    findNodeAtLoc False loc exp

  Slv.Typed _ _ (Slv.FieldSpread exp) ->
    findNodeAtLoc False loc exp

  _ ->
    Nothing


findNodeAtLocInPattern :: Loc -> Slv.Pattern -> Maybe Node
findNodeAtLocInPattern _ (Slv.Untyped _ _) = Nothing
findNodeAtLocInPattern loc input@(Slv.Typed _ area pat) =
  if isInRange loc area then
    let deeper =
          case pat of
            Slv.PCon _ pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PList pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PTuple pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PRecord pats _ ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PSpread pat ->
              findNodeAtLocInPattern loc pat

            _ ->
              Nothing
    in  case deeper of
          Nothing ->
            Just $ PatternNode input

          Just _ ->
            deeper
  else
    Nothing


findNodeAtLocInIs :: Loc -> Slv.Is -> Maybe Node
findNodeAtLocInIs _ (Slv.Untyped _ _) = Nothing
findNodeAtLocInIs loc (Slv.Typed _ _ (Slv.Is pat exp)) =
  findNodeAtLocInPattern loc pat <|> findNodeAtLoc False loc exp


findNodeInRecordFieldTypeAnnotation :: Loc -> (String, (Area, Slv.Typing)) -> Maybe Node
findNodeInRecordFieldTypeAnnotation loc (fieldName, (area, typing)) =
  if isInRange loc area then
    Just $ RecordFieldAnnotation area fieldName typing
  else
    Nothing


findNodeInTypeAnnotation :: Loc -> Maybe Slv.Typing -> Slv.Typing -> Maybe Node
findNodeInTypeAnnotation loc maybeRoot typing =
  if isInRange loc (Slv.getArea typing) then
    let deeper =
          case Slv.getValue typing of
            Slv.TRComp _ typings ->
              foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc (Just typing) <$> typings

            Slv.TRArr l r ->
              findNodeInTypeAnnotation loc Nothing l <|> findNodeInTypeAnnotation loc Nothing r

            Slv.TRRecord fields _ ->
              foldl' (<|>) Nothing (findNodeInRecordFieldTypeAnnotation loc <$> Map.toList fields)
              <|> foldl' (<|>) Nothing (findNodeInTypeAnnotation loc Nothing <$> Map.elems (snd <$> fields))

            Slv.TRTuple items ->
              foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc Nothing <$> items

            Slv.TRConstrained constraints subTyping ->
              foldl' (<|>) Nothing (findNodeInTypeAnnotation loc Nothing <$> constraints) <|> findNodeInTypeAnnotation loc Nothing subTyping

            _ ->
              Nothing
    in  deeper <|> Just (TypingNode maybeRoot typing)
  else
    Nothing


findNodeAtLoc :: Bool -> Loc -> Slv.Exp -> Maybe Node
findNodeAtLoc topLevel loc input@(Slv.Typed qt area exp) =
  if isInRange loc area then
    let deeper =
          case exp of
            Slv.App fn arg _ ->
              findNodeAtLoc False loc arg <|> findNodeAtLoc False loc fn

            Slv.Abs param@(Slv.Typed _ paramArea _) body ->
              if isInRange loc paramArea then
                Just $ NameNode False param
              else
                foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> body

            Slv.Do body ->
              foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> body

            Slv.Assignment _ exp ->
              findNodeAtLoc False loc exp

            Slv.Mutate lhs exp ->
              findNodeAtLoc False loc lhs <|> findNodeAtLoc False loc exp

            Slv.Access rec field ->
              -- Nothing
              findNodeAtLoc False loc rec <|> findNodeAtLoc False loc field

            Slv.ArrayAccess arr index ->
              findNodeAtLoc False loc arr <|> findNodeAtLoc False loc index

            Slv.If cond truthy falsy ->
              findNodeAtLoc False loc cond
              <|> findNodeAtLoc False loc truthy
              <|> findNodeAtLoc False loc falsy

            Slv.While cond body ->
              findNodeAtLoc False loc cond
              <|> findNodeAtLoc False loc body

            Slv.Export exp' ->
              findNodeAtLoc True loc exp'

            Slv.TypedExp exp' typing _ ->
              findNodeInTypeAnnotation loc Nothing typing
              <|> findNodeAtLoc False loc exp'

            Slv.Var name _ ->
              Just $ NameNode topLevel (Slv.Typed qt area name)

            Slv.Extern _ name _ ->
              Just $ NameNode topLevel (Slv.Typed qt area name)

            Slv.TemplateString exps ->
              foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> exps

            Slv.TupleConstructor items ->
              foldl' (<|>) Nothing $ findNodeAtLoc False loc <$> items

            Slv.ListConstructor items ->
              foldl' (<|>) Nothing $ findNodeAtLocInListItem loc <$> items

            Slv.Record fields ->
              foldl' (<|>) Nothing $ findNodeAtLocInField loc <$> fields

            Slv.Where exp iss ->
              findNodeAtLoc False loc exp
              <|> foldl' (<|>) Nothing (findNodeAtLocInIs loc <$> iss)

            _ ->
              Nothing
    in  case deeper of
          Nothing ->
            Just $ ExpNode topLevel input

          Just _ ->
            deeper
  else
    Nothing
findNodeAtLoc _ _ (Slv.Untyped _ _) =
  Nothing


findNodeInAst :: Loc -> Src.AST -> Slv.AST -> Maybe Node
findNodeInAst loc srcAst slvAst =
  findNodeInExps loc (Slv.aexps slvAst ++ Slv.getAllMethods slvAst)
  <|> findNodeInTypeDeclarations loc (Slv.atypedecls slvAst)
  <|> findNodeInInstanceHeaders loc (Src.ainstances srcAst)
  <|> findNodeInImports loc (Src.aimports srcAst)


findNodeInImports :: Loc -> [Src.Import] -> Maybe Node
findNodeInImports loc imports =
  foldl' (<|>) Nothing $ findNodeInImport loc <$> imports


findNodeInImport :: Loc -> Src.Import -> Maybe Node
findNodeInImport loc imp =
  if isInRange loc $ Src.getArea imp then
    case imp of
      Src.Source impArea _ (Src.DefaultImport _ _ filepath) ->
        Just $ DefaultImportNode impArea filepath

      Src.Source impArea _ (Src.NamedImport names _ filepath) ->
        foldl' (<|>) Nothing (findNamedImportNode loc filepath NamedImportNode <$> names)
        <|> Just (DefaultImportNode impArea filepath)

      Src.Source impArea _ (Src.TypeImport typeNames _ filepath) ->
        foldl' (<|>) Nothing (findNamedImportNode loc filepath TypeImportNode <$> typeNames)
        <|> Just (DefaultImportNode impArea filepath)
  else
    Nothing

findNamedImportNode :: Loc -> FilePath -> (Area -> String -> FilePath -> Node) -> Src.Source Src.Name -> Maybe Node
findNamedImportNode loc importPath ctor (Src.Source area _ n) =
  if isInRange loc area then
    Just $ ctor area n importPath
  else
    Nothing


findNodeInTypeDeclarations :: Loc -> [Slv.TypeDecl] -> Maybe Node
findNodeInTypeDeclarations loc typeDecls = case typeDecls of
  typeDeclaration : next ->
    findNodeInTypeDeclaration loc typeDeclaration
    <|> findNodeInTypeDeclarations loc next

  [] ->
    Nothing


findNodeInConstructor :: Loc -> Slv.Constructor -> Maybe Node
findNodeInConstructor loc constructor =
  if isInRange loc (Slv.getArea constructor) then
    foldl' (<|>) Nothing (findNodeInTypeAnnotation loc Nothing <$> Slv.getConstructorTypings constructor)
    <|> Just (NameNode False (Slv.Typed ([] :=> Slv.getConstructorType constructor) (Slv.getArea constructor) $ Slv.getConstructorName constructor))
  else
    Nothing


findNodeInTypeDeclaration :: Loc -> Slv.TypeDecl -> Maybe Node
findNodeInTypeDeclaration loc typeDeclaration = case typeDeclaration of
  Slv.Untyped area Slv.ADT { Slv.adtname, Slv.adtconstructors, Slv.adtparams } ->
    -- TODO: look in constructors as well and make it a prio as it should be deeper in the AST if found
    if isInRange loc area then
      foldl' (<|>) Nothing (findNodeInConstructor loc <$> adtconstructors)
      <|> Just (ADTNode adtname area (buildKind $ length adtparams))
    else
      Nothing

  Slv.Untyped area Slv.Alias { Slv.aliasname, Slv.aliasparams, Slv.aliastype } ->
    -- TODO: look in typing as well and make it a prio as it should be deeper in the AST if found
    if isInRange loc area then
      findNodeInTypeAnnotation loc Nothing aliastype
      <|> Just (ADTNode aliasname area (buildKind $ length aliasparams))
    else
      Nothing

  _ ->
    Nothing


findNodeInExps :: Loc -> [Slv.Exp] -> Maybe Node
findNodeInExps loc exps = case exps of
  exp : next ->
    findNodeAtLoc True loc exp <|> findNodeInExps loc next

  [] ->
    Nothing


findNodeInInstanceHeaders :: Loc -> [Src.Instance] -> Maybe Node
findNodeInInstanceHeaders loc instances =
  let srcTypings = concatMap (\i -> Src.getInstanceConstraintTypings i ++ Src.getInstanceTypings i) instances
      -- srcTypings = findNodeInExps loc $ Slv.aexps ast ++ Slv.getAllMethods ast
      canTypings = Can.canonicalizeTyping' <$> srcTypings
      slvTypings = Slv.updateTyping <$> canTypings
  in  foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc Nothing <$> slvTypings


retrieveKind :: Type -> Kind
retrieveKind t = case t of
  TCon (TC _ k) _ _ ->
    k

  TApp l _ ->
    retrieveKind l

  TVar (TV _ k) ->
    k

  _ ->
    kind t


failedQt :: Qual Type
failedQt = [] :=> TVar (TV (-1) Star)


sanitizeName :: String -> String
sanitizeName s = case s of
  "_P_" ->
    "pipe"

  '_':'_':'$':'P':'H':_ ->
    "$"

  or ->
    or


internalNames :: [String]
internalNames =
  [ "_P_" ]


nodeToHoverInfo :: Rock.MonadFetch Query.Query m => FilePath -> Node -> m String
nodeToHoverInfo modulePath node = do
  (_, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
  nodeInfo <- case node of
    ExpNode topLevel (Slv.Typed qt _ (Slv.Assignment name _)) ->
      return $ sanitizeName name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _)) ->
      return $ sanitizeName name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _)) ->
      return $ sanitizeName name <> " :: " <> prettyQt topLevel qt

    NameNode topLevel (Slv.Typed qt _ name) -> do
      let prefix =
            if name `elem` internalNames then
              ""
            else
              sanitizeName name <> " :: "
      return $ prefix <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ _) ->
      return $ prettyQt topLevel qt

    PatternNode (Slv.Typed qt _ (Slv.PVar name)) ->
      return $ name <> " :: " <> prettyQt False qt

    PatternNode (Slv.Typed qt _ _) ->
      return $ prettyQt False qt

    ADTNode name _ k ->
      return $ name <> " :: " <> kindToStr k

    RecordFieldAnnotation _ name typing ->
      return $ name <> " :: " <> prettyPrintTyping' False typing

    -- TODO: make dry with case for TRSingle
    TypingNode _ (Slv.Untyped _ (Slv.TRComp name ts)) -> do
      maybeType <- CanEnv.lookupADT' canEnv name
      case maybeType of
        Just t ->
          return $ name <> " :: " <> kindToStr (retrieveKind t)

        Nothing ->
          return $ name <> " :: " <> List.intercalate " -> " (replicate (length ts + 1) "*")

    -- TypingNode maybeRootTyping typing@(Slv.Untyped _ (Slv.TRSingle name)) -> do
    --   case maybeRootTyping of
    --     Just (Slv.Untyped _ (Slv.TRComp rootTypingName typings)) -> do
    --       maybeInterface <- Rock.fetch $ Query.CanonicalizedInterface modulePath rootTypingName
    --       return $ "root: " <> ppShow maybeRootTyping <> "\ntyping: " <> ppShow typing <> "\nmaybe interface: " <> ppShow maybeInterface
    TypingNode maybeRootTyping typing@(Slv.Untyped _ (Slv.TRSingle name)) -> do
      maybeKind <- case maybeRootTyping of
        Just (Slv.Untyped _ (Slv.TRComp rootTypingName typings)) -> do
          -- first we try to find a var in an adt
          maybeRootType <- CanEnv.lookupADT' canEnv rootTypingName
          case maybeRootType of
            Just t -> do
              let allVars = collectVars t
              case findTRSingleIndex typings typing of
                Just i ->
                  if i < length allVars then
                    return $ Just $ kind $ allVars !! i
                  else
                    return Nothing
                _ ->
                  return Nothing

            _ -> do
              -- otherwise we try to find a var in an interface declaration
              -- maybeInterface <- Rock.fetch $ Query.CanonicalizedInterface modulePath rootTypingName
              maybeInterface <- Can.lookupInterface' canEnv rootTypingName
              return $ maybeInterface >>= \(CanEnv.Interface vars _ _) -> do
                case findTRSingleIndex typings typing of
                  Just i ->
                    if i < length vars then
                      Just $ kind $ vars !! i
                    else
                      Nothing

                  _ ->
                    Nothing
            where
              findTRSingleIndex :: [Slv.Typing] -> Slv.Typing -> Maybe Int
              findTRSingleIndex trCompVars trSingle = case trCompVars of
                (trc : _) | trc == trSingle ->
                  Just 0

                (_ : next) ->
                  (+ 1) <$> findTRSingleIndex next trSingle

                _ ->
                  Nothing

        _ ->
          return Nothing

      case maybeKind of
        Just k ->
          return $ name <> " :: " <> kindToStr k

        Nothing -> do
          maybeType <- CanEnv.lookupADT' canEnv name
          case maybeType of
            Just t ->
              return $ name <> " :: " <> kindToStr (retrieveKind t)

            Nothing ->
              return $ name <> " :: *"

    TypingNode _ (Slv.Untyped _ (Slv.TRArr _ _)) ->
      return "(->) :: * -> * -> *"

    TypingNode _ _ ->
      return "*"

    -- TODO: add docstring for the module here
    DefaultImportNode _ filepath ->
      return $ dropExtension (takeFileName filepath)

    NamedImportNode _ name filepath -> do
      maybeExp <- Rock.fetch $ Query.ForeignExp filepath name
      case maybeExp of
        Just exp ->
          let qt = Slv.getQualType exp
          in  return $ name <> " :: " <> prettyQt False qt

        Nothing ->
          return name

    TypeImportNode _ name _ -> do
      maybeType <- CanEnv.lookupADT' canEnv name
      case maybeType of
        Just t ->
          return $ name <> " :: " <> kindToStr (retrieveKind t)

        Nothing ->
          return name

    _ ->
      return ""

  return $
    "```madlib\n"
    <> nodeInfo
    <> "\n"
    <> "```\n\n"
    <> "*Defined in '"
    <> modulePath
    <> "' at line "
    <> show (getNodeLine node)
    <> "*"


getAutocompletionSuggestions :: State -> Loc -> FilePath -> String -> LspM () [(String, String, CompletionItemKind)]
getAutocompletionSuggestions autocompletionState loc path moduleContent = do
  -- Use single target for completion — types are target-independent for the vast majority of code
  jsOptions <- buildOptions TNode
  jsResult <- liftIO $ safeRunTask autocompletionState jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (completionSuggestionsTask loc path moduleContent)
  return $ case jsResult of
    Just (suggestions, _, _) -> suggestions
    Nothing                  -> []


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


getHoverInformation :: State -> Loc -> FilePath -> LspM () (Maybe String)
getHoverInformation state loc path = do
  jsOptions <- buildOptions TNode
  jsResult <- liftIO $ safeRunTask state jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
  case jsResult of
    Just (Just info, _, _) -> return (Just info)
    _ -> do
      llvmOptions <- buildOptions TLLVM
      llvmResult <- liftIO $ safeRunTask state llvmOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
      return $ case llvmResult of
        Just (r, _, _) -> r
        Nothing        -> Nothing


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
  Slv.Typed qt area (Slv.Assignment name _) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _))) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.Extern _ name _) ->
    Just $ mkDocSymbol (T.pack name) (Just $ T.pack $ prettyQt True qt) SkFunction area area

  Slv.Typed qt area (Slv.Export (Slv.Typed _ _ (Slv.Extern _ name _))) ->
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


pathToUri :: FilePath -> Uri
pathToUri path =
  Uri $ T.pack ("file://" <> path)


uriToPath :: Uri -> FilePath
uriToPath uri =
  let unpacked = T.unpack $ getUri uri
  in  if take 7 unpacked == "file://" then
        drop 7 unpacked
      else
        unpacked


typeCheckFileTask :: FilePath -> Rock.Task Query.Query Bool
typeCheckFileTask path = do
  parsedAst <- Rock.fetch $ Query.ParsedAST path
  Rock.fetch $ Query.SolvedASTWithEnv path
  return $ isJust (Src.apath parsedAst)


runTypeCheckIO :: Bool -> State -> FilePath -> Map.Map FilePath String -> Options.Options -> IO (Bool, [CompilationWarning], [CompilationError])
runTypeCheckIO invalidatePath state path fileUpdates options = do
  let changedFiles =
        if invalidatePath then
          [path]
        else
          []
  result <- try $
    runTask
      state
      options { optEntrypoint = path }
      Driver.Don'tPrune
      changedFiles
      fileUpdates
      (typeCheckFileTask path)
      :: IO (Either (Cyclic Query.Query) (Bool, [CompilationWarning], [CompilationError]))

  case result of
    Left _ -> do
      (_, warnings, errors) <- runTask
        state
        options { optEntrypoint = path }
        Driver.Don'tPrune
        changedFiles
        fileUpdates
        (Driver.detectCyleTask path)

      return (False, warnings, errors)

    Right (couldParse, warnings, errors) ->
      return (couldParse, warnings, errors)


runTypeCheck :: Bool -> State -> Target -> FilePath -> Map.Map FilePath String -> LspM () (Bool, [CompilationWarning], [CompilationError])
runTypeCheck invalidatePath state target path fileUpdates = do
  options <- buildOptions target
  liftIO $ runTypeCheckIO invalidatePath state path fileUpdates options


sendDiagnosticsForWarningsAndErrors :: [CompilationWarning] -> [CompilationError] -> LspM () ()
sendDiagnosticsForWarningsAndErrors warnings errors = do
  let errsByModule = groupErrsByModule errors
  let warnsByModule = groupWarnsByModule warnings

  errorDiagnostics <- liftIO $ mapM
    (\(p, errs) -> do
      diagnostics <- mapM errorToDiagnostic errs
      return (p, diagnostics)
    )
    $ Map.toList errsByModule

  warningDiagnostics <- liftIO $ mapM
    (\(p, warnings) -> do
      diagnostics <- mapM warningToDiagnostic warnings
      return (p, diagnostics)
    )
    $ Map.toList warnsByModule

  flushDiagnosticsBySource 100 (Just "Madlib")

  let allDiagnostics =
        Map.toList $ Map.unionWith
          (<>)
          (Map.fromList errorDiagnostics)
          (Map.fromList warningDiagnostics)

  forM_ allDiagnostics $ \(modulePath, diagnostics) -> do
    let moduleUri = pathToUri modulePath
    let diagnosticsBySource = partitionBySource diagnostics
    publishDiagnostics 100 (toNormalizedUri moduleUri) Nothing diagnosticsBySource


generateDiagnostics :: Bool -> State -> State -> Uri -> Map.Map FilePath String -> LspM () ()
generateDiagnostics invalidatePath state autocompletionState uri fileUpdates = do
  let path = uriToPath uri
  (_couldParseJs, jsWarnings, jsErrors)     <- runTypeCheck invalidatePath state TNode path fileUpdates
  (_couldParseLlvm, llvmWarnings, llvmErrors) <- runTypeCheck invalidatePath state TLLVM path fileUpdates
  let allWarnings = jsWarnings `List.union` llvmWarnings
  let allErrors   = jsErrors `List.union` llvmErrors

  when (null jsErrors) $ do
    liftIO $ copyStateTo (_jsDriverState state) (_jsDriverState autocompletionState)

  when (null llvmErrors) $ do
    liftIO $ copyStateTo (_llvmDriverState state) (_llvmDriverState autocompletionState)

  sendDiagnosticsForWarningsAndErrors allWarnings allErrors


uriOfError :: CompilationError -> Uri
uriOfError err = case Error.getContext err of
  Context path _ ->
    Uri $ T.pack ("file://" <> path)
  NoContext ->
    Uri $ T.pack "file://"


uriOfWarning :: CompilationWarning -> Uri
uriOfWarning warning = case Warning.getContext warning of
  Context path _ ->
    Uri $ T.pack ("file://" <> path)
  NoContext ->
    Uri $ T.pack "file://"


areErrorsFromSameModule :: CompilationError -> CompilationError -> Bool
areErrorsFromSameModule a b = case (a, b) of
  (CompilationError _ (Context pathA _), CompilationError _ (Context pathB _)) ->
    pathA == pathB

  _ ->
    False


isErrorFromModule :: FilePath -> CompilationError -> Bool
isErrorFromModule path err = case err of
  CompilationError _ (Context ctxPath _) ->
    ctxPath == path

  _ ->
    False


errorsForModule :: [CompilationError] -> FilePath -> [CompilationError]
errorsForModule errs path =
  filter (isErrorFromModule path) errs


areWarningsFromSameModule :: CompilationWarning -> CompilationWarning -> Bool
areWarningsFromSameModule a b = case (a, b) of
  (CompilationWarning _ (Context pathA _), CompilationWarning _ (Context pathB _)) ->
    pathA == pathB

  _ ->
    False


isWarningFromModule :: FilePath -> CompilationWarning -> Bool
isWarningFromModule path err = case err of
  CompilationWarning _ (Context ctxPath _) ->
    ctxPath == path

  _ ->
    False


warningsForModule :: [CompilationWarning] -> FilePath -> [CompilationWarning]
warningsForModule errs path =
  filter (isWarningFromModule path) errs


groupErrsByModule :: [CompilationError] -> Map.Map FilePath [CompilationError]
groupErrsByModule errs =
  let errs' = filter ((/= NoContext) . Error.getContext) errs
  in  Map.fromListWith (<>) $ map (\e -> (Error.getPath e, [e])) errs'


groupWarnsByModule :: [CompilationWarning] -> Map.Map FilePath [CompilationWarning]
groupWarnsByModule warnings =
  let warnings' = filter ((/= NoContext) . Warning.getContext) warnings
  in  Map.fromListWith (<>) $ map (\w -> (Warning.getPath w, [w])) warnings'



warningToDiagnostic :: CompilationWarning -> IO Diagnostic
warningToDiagnostic warning = do
  formattedWarning <- Explain.simpleFormatWarning True warning
  case warning of
    CompilationWarning _ (Context _ area) ->
      return $ Diagnostic
        (areaToRange area)        -- _range
        (Just DsWarning)            -- _severity
        Nothing                   -- _code
        (Just "Madlib")           -- _source
        (T.pack formattedWarning)   -- _message
        (if Warning.isUnusedWarning warning then Just $ List [DtUnnecessary] else Nothing)                   -- _tags
        Nothing                   -- _relatedInformation

    _ ->
      return $ Diagnostic
        noRange
        (Just DsWarning)
        Nothing
        (Just "Madlib")
        (T.pack formattedWarning)
        Nothing
        Nothing


errorToDiagnostic :: CompilationError -> IO Diagnostic
errorToDiagnostic err = do
  formattedError <- Explain.simpleFormatError True err
  case err of
    CompilationError _ (Context _ area) ->
      return $ Diagnostic
        (areaToRange area)        -- _range
        (Just DsError)            -- _severity
        Nothing                   -- _code
        (Just "Madlib")           -- _source
        (T.pack formattedError)   -- _message
        Nothing                   -- _tags
        Nothing                   -- _relatedInformation

    _ ->
      return $ Diagnostic
        noRange
        (Just DsError)
        Nothing
        (Just "Madlib")
        (T.pack formattedError)
        Nothing
        Nothing

noRange :: Range
noRange =
  Range (Position 0 0) (Position 0 0)

areaToRange :: Area -> Range
areaToRange area =
  Range
    (Position (Loc.getLine (Loc.getStartLoc area) - 1) (Loc.getCol (Loc.getStartLoc area) - 1))
    (Position (Loc.getLine (Loc.getEndLoc area) - 1) (Loc.getCol (Loc.getEndLoc area) - 1))


data State = State
  { _jsDriverState :: Driver.State CompilationError
  , _llvmDriverState :: Driver.State CompilationError
  , _openFiles :: IORef (Map.Map FilePath String)
  , _changedFiles :: Set.Set FilePath
  , _debounceRef :: IORef (Maybe UTCTime)
  , _stateLock :: MVar ()
  , _allModulePaths :: IORef (Set.Set FilePath)
  , _backgroundDone :: IORef Bool
  }


textDocumentSyncOptions :: TextDocumentSyncOptions
textDocumentSyncOptions =
  TextDocumentSyncOptions
    (Just True)       -- _openClose
    (Just TdSyncFull) -- _change
    Nothing           -- _willSave
    Nothing           -- _willSaveWaitUntil
    (Just $ InL True) -- _save


copyStateTo :: Driver.State err -> Driver.State err -> IO ()
copyStateTo from to = do
  _startedVar <- readIORef (Driver._startedVar from)
  _hashesVar <- readIORef (Driver._hashesVar from)
  _reverseDependenciesVar <- readIORef (Driver._reverseDependenciesVar from)
  _tracesVar <- readIORef (Driver._tracesVar from)
  _errorsVar <- readIORef (Driver._errorsVar from)
  _warningsVar <- readIORef (Driver._warningsVar from)

  atomicWriteIORef (Driver._startedVar to) _startedVar
  atomicWriteIORef (Driver._hashesVar to) _hashesVar
  atomicWriteIORef (Driver._reverseDependenciesVar to) _reverseDependenciesVar
  atomicWriteIORef (Driver._tracesVar to) _tracesVar
  atomicWriteIORef (Driver._errorsVar to) _errorsVar
  atomicWriteIORef (Driver._warningsVar to) _warningsVar

  return ()


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


runLanguageServer :: IO ()
runLanguageServer = do
  jsDriverState <- Driver.initialState
  llvmDriverState <- Driver.initialState
  openFiles <- newIORef mempty
  debounceRef <- newIORef Nothing
  stateLock <- newMVar ()
  allModulePaths <- newIORef Set.empty
  backgroundDone <- newIORef False
  let state = State jsDriverState llvmDriverState openFiles mempty debounceRef stateLock allModulePaths backgroundDone

  autocompletionJsDriverState <- Driver.initialState
  autocompletionLlvmDriverState <- Driver.initialState
  autocompletionOpenFiles <- newIORef mempty
  autocompletionDebounceRef <- newIORef Nothing
  autocompletionStateLock <- newMVar ()
  autocompletionAllModulePaths <- newIORef Set.empty
  autocompletionBackgroundDone <- newIORef False
  let autocompletionState = State autocompletionJsDriverState autocompletionLlvmDriverState autocompletionOpenFiles mempty autocompletionDebounceRef autocompletionStateLock autocompletionAllModulePaths autocompletionBackgroundDone
  runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers state autocompletionState
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions { textDocumentSync = Just textDocumentSyncOptions }
    }
  return ()


hoverInfoTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe String)
hoverInfoTask loc path = do
  -- hasCycle <- Rock.fetch $ Query.DetectImportCycle [] path
  -- if hasCycle then
  --   return Nothing
  -- else do
    srcAst        <- Rock.fetch $ Query.ParsedAST path
    (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
    mapM (nodeToHoverInfo path) (findNodeInAst loc srcAst typedAst)


findForeignAstForName :: String -> [Can.Import] -> Maybe FilePath
findForeignAstForName name imports = case imports of
  [] ->
    Nothing

  (Can.Canonical _ (Can.NamedImport names _ path) : _) | name `elem` (Can.getCanonicalContent <$> names) ->
    Just path

  (Can.Canonical _ (Can.DefaultImport namespace _ path) : _) | takeWhile (/= '.') name == Can.getCanonicalContent namespace ->
    Just path

  (Can.Canonical _ (Can.TypeImport names _ path) : _) | name `elem` (Can.getCanonicalContent <$> names) ->
    Just path

  (_ : next) ->
    findForeignAstForName name next


findNameInNode :: Maybe Node -> Maybe String
findNameInNode node = case node of
  Just (NameNode _ (Slv.Typed _ _ name)) ->
    Just name

  Just (PatternNode (Slv.Typed _ _ (Slv.PVar name))) ->
    Just name

  Just (PatternNode (Slv.Typed _ _ (Slv.PCon name _))) ->
    Just name

  Just (TypingNode _ (Slv.Untyped _ (Slv.TRComp name _))) ->
    return name

  Just (TypingNode _ (Slv.Untyped _ (Slv.TRSingle name))) ->
    return name

  _ ->
    Nothing


findTopLevelExp :: Loc -> [Slv.Exp] -> Maybe Slv.Exp
findTopLevelExp loc exps = case exps of
  e@(Slv.Typed _ area _) : next ->
    if isInRange loc area then
      Just e
    else
      findTopLevelExp loc next

  _ : next ->
    findTopLevelExp loc next

  [] ->
    Nothing


getLocalNames :: Loc -> Slv.Exp -> [(String, Type)]
getLocalNames loc exp = case exp of
  Slv.Typed _ area (Slv.Assignment _ e) | not (isLocAfter loc area) ->
    getLocalNames loc e

  Slv.Typed (_ :=> t) _ (Slv.Assignment n e) ->
    (n, t) : getLocalNames loc e

  Slv.Typed _ area (Slv.Abs _ body) | not (isLocAfterStart loc area) ->
    body >>= getLocalNames loc

  Slv.Typed (_ :=> t) _ (Slv.Abs param body) ->
    let paramType = case getParamTypes t of
          (pt : _) -> pt
          []       -> TVar (TV (-1) Star)
    in  (Slv.getValue param, paramType) : (body >>= getLocalNames loc)

  Slv.Typed _ _ (Slv.Export e) ->
    getLocalNames loc e

  Slv.Typed _ _ (Slv.TypedExp e _ _) ->
    getLocalNames loc e

  _ ->
    []


completionSuggestionsTask :: Loc -> FilePath -> String -> Rock.Task Query.Query [(String, String, CompletionItemKind)]
completionSuggestionsTask loc@(Loc _ line col) modulePath moduleContent = do
  (typedAst, env)  <- Rock.fetch $ Query.SolvedASTWithEnv modulePath

  let contentLines = lines moduleContent
  let foundLine = if line - 1 >= 0 && line - 1 < length contentLines
                  then take (col - 1) (contentLines !! (line - 1))
                  else ""
  let autocompletionKind = computeAutocompletionKind (reverse foundLine)

  let topLevelExp = findTopLevelExp loc (Slv.aexps typedAst)
  let localNames = map (\(n, t) -> (n, Forall [] ([] :=> t))) $ Maybe.fromMaybe [] $ (getLocalNames loc) <$> topLevelExp
  let namesInEnv = Map.toList $ SlvEnv.envVars env
  let methodsInEnv = Map.toList $ SlvEnv.envMethods env
  case autocompletionKind of
    AutocompletingNamespaceAccess namespace ->
      return
        $ map (\(n, qt) -> (if '.' `List.elem` n then List.tail $ dropWhile (/= '.') n else n, prettyScheme qt, CiFunction))
        $ filter (List.isPrefixOf namespace . fst) namesInEnv

    AutocompletingName _ ->
      return
        $ map (\(n, qt) -> (n, prettyScheme qt, CiFunction))
        $ localNames ++ namesInEnv ++ methodsInEnv

    AutocompletingRecordAccess recordName fieldName -> do
      let updatedLoc = Loc 0 line (col - length fieldName - 1)
      case findNodeInExps updatedLoc (Slv.aexps typedAst ++ Slv.getAllMethods typedAst) of
        Just (ExpNode _ (Slv.Typed (_ :=> TRecord fields _ extraFields) _ _)) ->
          return
            $ map (\(n, t) -> (n, prettyScheme (Forall [] ([] :=> t)), CiField))
            $ Map.toList $ fields <> extraFields

        Just (NameNode _ (Slv.Typed (_ :=> TApp _ (TRecord fields _ extraFields)) _ _)) ->
          return
            $ map (\(n, t) -> (n, prettyScheme (Forall [] ([] :=> t)), CiField))
            $ Map.toList $ fields <> extraFields

        _ ->
          case List.find (\(n, _) -> n == recordName) localNames of
            Just (_, Forall _ (_ :=> TRecord fields _ extraFields)) ->
              return
                $ map (\(n, t) -> (n, prettyScheme (Forall [] ([] :=> t)), CiField))
                $ Map.toList
                $ fields <> extraFields

            _ ->
              return $ map (\(n, qt) -> (n, prettyScheme qt, CiField)) $ localNames ++ namesInEnv ++ methodsInEnv


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


safeRunTask :: State -> Options.Options -> Driver.Prune -> [FilePath] -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (Maybe (a, [CompilationWarning], [CompilationError]))
safeRunTask state options prune invalidatedPaths fileUpdates task = do
  result <- try $ runTask state options prune invalidatedPaths fileUpdates task
  case result of
    Right r  -> return (Just r)
    Left (_ :: SomeException) ->
      return Nothing


runTask :: State -> Options.Options -> Driver.Prune -> [FilePath] -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (a, [CompilationWarning], [CompilationError])
runTask state options prune invalidatedPaths fileUpdates task = do
  withMVar (_stateLock state) $ \_ -> do
    let driverState =
          if Options.optTarget options == TLLVM then
            _llvmDriverState state
          else
            _jsDriverState state
    Driver.runIncrementalTask
      driverState
      options
      invalidatedPaths
      fileUpdates
      prune
      task


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
