{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
module Run.LanguageServer where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import Control.Monad.Base
import qualified Rock
import Driver.Rules
import qualified Driver.Query as Query
import Text.Show.Pretty (ppShow)
import LLVM.Internal.FFI.Type (x86FP80TypeInContext)
import qualified Driver
import Error.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error.Context
import Explain.Location
import qualified Explain.Location as Loc
import Language.LSP.Diagnostics
import qualified Explain.Format as Explain
import Data.List (group, foldl')
import qualified Data.List as List
import Control.Monad (forM_)
import Data.IORef
import qualified AST.Solved as Slv
import Explain.Format (prettyPrintQualType, prettyPrintType)
import Control.Applicative ((<|>))
import Infer.Type (Qual((:=>)), Type)
import Error.Warning
import qualified Error.Warning as Warning
import Error.Warning (CompilationWarning(CompilationWarning))
import qualified Error.Error as Error
import Data.Time.Clock


handlers :: State -> Handlers (LspM ())
handlers state = mconcat
  [ notificationHandler SInitialized $ \_not ->
      sendNotification SWindowLogMessage (LogMessageParams MtInfo "Madlib server initialized")
  , requestHandler STextDocumentHover $ \req responder -> do
      p <- getRootPath
      let path = case p of
            Just x ->
              x

            _ ->
              "no path"
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
          Position _l _c = pos
      sendNotification SWindowLogMessage (LogMessageParams MtInfo (T.pack $ ppShow uri))
      maybeHoverInfo <- liftIO $ getHoverInformation state (Loc 0 (_l + 1) (_c + 1)) (uriToPath uri)
      case maybeHoverInfo of
        Just info -> do
          let ms    = HoverContents $ MarkupContent MkMarkdown (T.pack info)
              range = Range pos pos
              rsp   = Hover ms (Just range)
          responder (Right $ Just rsp)

        Nothing ->
          return ()

  , notificationHandler STextDocumentDidOpen $ \(NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) ->
      generateDiagnostics state uri mempty
  , notificationHandler STextDocumentDidSave $ \(NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _)) ->
      generateDiagnostics state uri mempty
  , notificationHandler STextDocumentDidChange $ \(NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) (List changes))) -> do
      startT <- liftIO getCurrentTime
      let (TextDocumentContentChangeEvent _ _ docContent) = last changes
      generateDiagnostics state uri (Map.singleton (uriToPath uri) (T.unpack docContent))
      endT <- liftIO getCurrentTime
      let diff = diffUTCTime endT startT
      -- undefined
      sendNotification SWindowLogMessage $ LogMessageParams MtInfo ("change duration: " <> T.pack (ppShow diff))
      -- sendNotification SWindowLogMessage $ LogMessageParams MtInfo ("error count: " <> T.pack (ppShow $ last changes))
  ]



isInRange :: Loc -> Area -> Bool
isInRange (Loc _ l c) (Area (Loc _ lstart cstart) (Loc _ lend cend)) =
  (l >= lstart && l <= lend)
  && (not (l == lstart && c < cstart) && not (l == lend && c > cend))



prettyQt :: Bool -> Qual Type -> String
prettyQt topLevel qt@(_ :=> t) =
  if topLevel then
    prettyPrintQualType True qt
  else
    prettyPrintType True t


data Node
  = ExpNode Bool Slv.Exp
  | NameNode Bool (Slv.Solved String)
  | PatternNode Slv.Pattern


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
findNodeAtLocInPattern loc input@(Slv.Untyped _ _) = Nothing
findNodeAtLocInPattern loc input@(Slv.Typed qt area pat) =
  if isInRange loc area then
    let deeper =
          case pat of
            Slv.PCon name pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PList pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PTuple pats ->
              foldl' (<|>) Nothing $ findNodeAtLocInPattern loc <$> pats

            Slv.PRecord pats ->
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
findNodeAtLocInIs loc (Slv.Untyped _ _) = Nothing
findNodeAtLocInIs loc (Slv.Typed _ _ (Slv.Is pat exp)) =
  findNodeAtLocInPattern loc pat <|> findNodeAtLoc False loc exp


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

            Slv.Assignment name exp ->
              findNodeAtLoc False loc exp

            Slv.Placeholder _ exp' ->
              findNodeAtLoc False loc exp'

            Slv.If cond truthy falsy ->
              findNodeAtLoc False loc cond
              <|> findNodeAtLoc False loc truthy
              <|> findNodeAtLoc False loc falsy

            Slv.Export exp' ->
              findNodeAtLoc False loc exp'

            Slv.TypedExp exp' _ _ ->
              findNodeAtLoc False loc exp'

            Slv.Var name _ ->
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
          Nothing | Slv.isPlaceholderExp input ->
            Nothing

          Nothing ->
            Just $ ExpNode topLevel input

          Just _ ->
            deeper
  else
    Nothing
findNodeAtLoc topLevel loc (Slv.Untyped _ _) =
  Nothing




findNodeForLocInExps :: Loc -> [Slv.Exp] -> Maybe Node
findNodeForLocInExps loc exps = case exps of
  exp : next ->
    findNodeAtLoc True loc exp <|> findNodeForLocInExps loc next

  [] ->
    Nothing


nodeToHoverInfo :: FilePath -> Node -> String
nodeToHoverInfo modulePath node =
  let typeInfo = case node of
        ExpNode topLevel (Slv.Typed qt _ (Slv.Assignment name _)) ->
          name <> " :: " <> prettyQt topLevel qt

        ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _)) ->
          name <> " :: " <> prettyQt topLevel qt

        ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _)) ->
          name <> " :: " <> prettyQt topLevel qt

        NameNode topLevel (Slv.Typed qt _ name) ->
          name <> " :: " <> prettyQt topLevel qt

        ExpNode topLevel (Slv.Typed qt _ _) ->
          prettyQt topLevel qt

        PatternNode (Slv.Typed qt _ (Slv.PVar name)) ->
          name <> " :: " <> prettyQt False qt

        PatternNode (Slv.Typed qt _ _) ->
          prettyQt False qt

        _ ->
          ""
  in  "```madlib\n"
      <> typeInfo
      <> "\n"
      <> "```\n\n"
      <> "*Defined in "
      <> modulePath
      <> " at line "
      <> show (getNodeLine node)
      <> "*"



hoverInfoTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe String)
hoverInfoTask loc path = do
  (typedAst, _) <- Rock.fetch $ Query.SolvedASTWithEnv path
  return $ nodeToHoverInfo path <$> findNodeForLocInExps loc (Slv.aexps typedAst)




getHoverInformation :: State -> Loc -> FilePath -> IO (Maybe String)
getHoverInformation state loc path = do
  (result, _, _) <- runTask state Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
  return result


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

generateDiagnostics :: State -> Uri -> Map.Map FilePath String -> LspM () ()
generateDiagnostics state uri fileUpdates = do
  let path = uriToPath uri
  -- retrieve current errors first, so that we can update diagnostics of files
  -- that went from errors > 0 to 0 and remove them completely.
  (_, warnings, errs) <- liftIO $ runTask state Driver.Don'tPrune [path] fileUpdates (typeCheckFile path)
  let errsByModule = groupErrsByModule errs
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

  flushDiagnosticsBySource 20 (Just "Madlib")

  let allDiagnostics =
        Map.toList $ Map.unionWith
          (<>)
          (Map.fromList errorDiagnostics)
          (Map.fromList warningDiagnostics)

  forM_ allDiagnostics $ \(modulePath, diagnostics) -> do
    let moduleUri = pathToUri modulePath
    let diagnosticsBySource = partitionBySource diagnostics
    publishDiagnostics 20 (toNormalizedUri moduleUri) Nothing diagnosticsBySource


uriOfError :: CompilationError -> Uri
uriOfError err = case Error.getContext err of
  Context path _ _ ->
    Uri $ T.pack ("file://" <> path)


uriOfWarning :: CompilationWarning -> Uri
uriOfWarning warning = case Warning.getContext warning of
  Context path _ _ ->
    Uri $ T.pack ("file://" <> path)


areErrorsFromSameModule :: CompilationError -> CompilationError -> Bool
areErrorsFromSameModule a b = case (a, b) of
  (CompilationError _ (Context pathA _ _), CompilationError _ (Context pathB _ _)) ->
    pathA == pathB

  _ ->
    False


isErrorFromModule :: FilePath -> CompilationError -> Bool
isErrorFromModule path err = case err of
  CompilationError _ (Context ctxPath _ _) ->
    ctxPath == path

  _ ->
    False


errorsForModule :: [CompilationError] -> FilePath -> [CompilationError]
errorsForModule errs path =
  filter (isErrorFromModule path) errs


areWarningsFromSameModule :: CompilationWarning -> CompilationWarning -> Bool
areWarningsFromSameModule a b = case (a, b) of
  (CompilationWarning _ (Context pathA _ _), CompilationWarning _ (Context pathB _ _)) ->
    pathA == pathB

  _ ->
    False


isWarningFromModule :: FilePath -> CompilationWarning -> Bool
isWarningFromModule path err = case err of
  CompilationWarning _ (Context ctxPath _ _) ->
    ctxPath == path

  _ ->
    False


warningsForModule :: [CompilationWarning] -> FilePath -> [CompilationWarning]
warningsForModule errs path =
  filter (isWarningFromModule path) errs


groupErrsByModule :: [CompilationError] -> Map.Map FilePath [CompilationError]
groupErrsByModule errs =
  let errs' = filter ((/= NoContext) . Error.getContext) errs
  in  Map.fromList $ (\errs -> (Error.getPath $ head errs, errs)) <$> List.groupBy areErrorsFromSameModule errs'


groupWarnsByModule :: [CompilationWarning] -> Map.Map FilePath [CompilationWarning]
groupWarnsByModule warnings =
  let warnings' = filter ((/= NoContext) . Warning.getContext) warnings
  in  Map.fromList $ (\warnings -> (Warning.getPath $ head warnings, warnings)) <$> List.groupBy areWarningsFromSameModule warnings'



warningToDiagnostic :: CompilationWarning -> IO Diagnostic
warningToDiagnostic warning = do
  formattedWarning <- Explain.formatWarning readFile True warning
  case warning of
    CompilationWarning _ (Context astPath area _) ->
      return $ Diagnostic
        (areaToRange area)        -- _range
        (Just DsWarning)            -- _severity
        Nothing                   -- _code
        (Just "Madlib")           -- _source
        (T.pack formattedWarning)   -- _message
        (if Warning.isUnusedImport warning then Just $ List [DtUnnecessary] else Nothing)                   -- _tags
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
  formattedError <- Explain.format readFile True err
  case err of
    CompilationError _ (Context astPath area _) ->
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
  { _driverState :: Driver.State CompilationError
  , _openFiles :: IORef (Map.Map FilePath String)
  , _changedFiles :: Set.Set FilePath
  }


textDocumentSyncOptions :: TextDocumentSyncOptions
textDocumentSyncOptions =
  TextDocumentSyncOptions
    (Just True)       -- _openClose
    (Just TdSyncFull) -- _change
    Nothing           -- _willSave
    Nothing           -- _willSaveWaitUntil
    (Just $ InL True) -- _save


runLanguageServer :: IO Int
runLanguageServer = do
  driverState <- Driver.initialState
  openFiles <- newIORef mempty
  let state = State driverState openFiles mempty
  runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers state
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions { textDocumentSync = Just textDocumentSyncOptions }
    }


typeCheckFile :: FilePath -> Rock.Task Query.Query ()
typeCheckFile path = do
  Rock.fetch $ Query.SolvedASTWithEnv path
  return ()


runTask :: State -> Driver.Prune -> [FilePath] -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (a, [CompilationWarning], [CompilationError])
runTask state prune invalidatedPaths fileUpdates task =
  Driver.runIncrementalTask
  (_driverState state)
  invalidatedPaths
  fileUpdates
  prune
  task
