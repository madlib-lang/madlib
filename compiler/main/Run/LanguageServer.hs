{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use forM_" #-}
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
import           Data.List (group, foldl')
import qualified Data.List as List
import           Control.Monad (forM_, join, forM, unless)
import           Data.IORef
import qualified AST.Solved as Slv
import           Explain.Format (prettyPrintQualType, prettyPrintType, kindToStr)
import           Control.Applicative ((<|>))
import           Infer.Type (Qual((:=>)), Type (..), kind, Kind (Star), TCon (..), TVar (..), findTypeVarInType, collectVars)
import           Error.Warning
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
import           Control.Exception (try)
import           Rock (Cyclic)
import qualified AST.Canonical         as Can
import qualified Canonicalize.EnvUtils as CanEnv
import Driver.Query (Query(CanonicalizedASTWithEnv))
import qualified AST.Source as Src
import qualified Canonicalize.Typing as Can
import qualified Infer.Typing as Slv
import qualified Canonicalize.Env as CanEnv
import qualified Canonicalize.Interface as Can



handlers :: State -> Handlers (LspM ())
handlers state = mconcat
  [ notificationHandler SInitialized $ \_not ->
      sendNotification SWindowLogMessage (LogMessageParams MtInfo "Madlib server initialized")
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos@(Position line col) _workDone) = req
      maybeHoverInfo <- getHoverInformation state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
      case maybeHoverInfo of
        Just info -> do
          let ms    = HoverContents $ MarkupContent MkMarkdown (T.pack info)
              range = Range pos pos
              rsp   = Hover ms (Just range)
          responder (Right $ Just rsp)

        Nothing ->
          return ()
  , requestHandler STextDocumentDefinition $ \req@(RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) (Position line col) _ _)) responder -> do
      recordAndPrintDuration "definition" $ do
        maybeLink <- getDefinitionLink state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
        case maybeLink of
          Just loc ->
            responder $ Right (InL loc)

          Nothing ->
            return ()
  -- , requestHandler STextDocumentDocumentLink $ \req responder -> do
  --     sendNotification SWindowLogMessage $ LogMessageParams MtInfo (T.pack $ ppShow req)
  -- , requestHandler STextDocumentImplementation $ \_ responder -> do
  --     sendNotification SWindowLogMessage $ LogMessageParams MtInfo "tdi"
  , notificationHandler STextDocumentDidOpen $ \(NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) -> do
      recordAndPrintDuration "file open" $ generateDiagnostics state uri mempty
  , notificationHandler STextDocumentDidSave $ \(NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _)) ->
      recordAndPrintDuration "file save" $ generateDiagnostics state uri mempty
  , notificationHandler STextDocumentDidChange $ \(NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) (List changes))) -> do
      recordAndPrintDuration "file change" $ do
        let (TextDocumentContentChangeEvent _ _ docContent) = last changes
        generateDiagnostics state uri (Map.singleton (uriToPath uri) (T.unpack docContent))
  ]


buildOptions :: LspM () Options.Options
buildOptions = do
  maybeRootPath <- getRootPath
  let rootPath = Maybe.fromMaybe "./" maybeRootPath
  return
    Options.Options
      { Options.optEntrypoint = ""
      , Options.optTarget = TNode
      , Options.optRootPath = rootPath
      , Options.optOutputPath = ""
      , Options.optOptimized = False
      , Options.optPathUtils = PathUtils.defaultPathUtils
      , Options.optBundle = False
      , Options.optCoverage = False
      , Options.optGenerateDerivedInstances = False
      , Options.optInsertInstancePlaholders = False
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
  | TypingNode (Maybe Slv.Typing) Slv.Typing
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
              foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc Nothing <$> Map.elems fields

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

            Slv.TypedExp exp' typing _ ->
              findNodeInTypeAnnotation loc Nothing typing
              <|> findNodeAtLoc False loc exp'

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


-- TODO: Add search in type annotations
findNodeInAst :: Loc -> Src.AST -> Slv.AST -> Maybe Node
findNodeInAst loc srcAst slvAst =
  findNodeInExps loc (Slv.aexps slvAst ++ Slv.getAllMethods slvAst)
  <|> findNodeInInstanceHeaders loc (Src.ainstances srcAst)


findNodeInExps :: Loc -> [Slv.Exp] -> Maybe Node
findNodeInExps loc exps = case exps of
  exp : next ->
    findNodeAtLoc True loc exp <|> findNodeInExps loc next

  [] ->
    Nothing


findNodeInInstanceHeaders :: Loc -> [Src.Instance] -> Maybe Node
findNodeInInstanceHeaders loc instances =
  let srcTypings = concatMap (\i -> Src.getInstanceTypings i ++ Src.getInstanceConstraintTypings i) instances
      -- srcTypings = findNodeInExps loc $ Slv.aexps ast ++ Slv.getAllMethods ast
      canTypings = Can.canonicalizeTyping' <$> srcTypings
      slvTypings = Slv.updateTyping <$> canTypings
  in  foldl' (<|>) Nothing $ findNodeInTypeAnnotation loc Nothing <$> slvTypings


retrieveKind :: Type -> Kind
retrieveKind t = case t of
  TCon (TC _ k) _ ->
    k

  TApp l _ ->
    retrieveKind l

  TVar (TV _ k) ->
    k

  _ ->
    kind t


nodeToHoverInfo :: Rock.MonadFetch Query.Query m => FilePath -> Node -> m String
nodeToHoverInfo modulePath node = do
  (_, canEnv, _) <- Rock.fetch $ CanonicalizedASTWithEnv modulePath
  typeInfo <- case node of
    ExpNode topLevel (Slv.Typed qt _ (Slv.Assignment name _)) ->
      return $ name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Assignment name _)) _ _)) ->
      return $ name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ (Slv.TypedExp (Slv.Typed _ _ (Slv.Export (Slv.Typed _ _ (Slv.Assignment name _)))) _ _)) ->
      return $ name <> " :: " <> prettyQt topLevel qt

    NameNode topLevel (Slv.Typed qt _ name) ->
      return $ name <> " :: " <> prettyQt topLevel qt

    ExpNode topLevel (Slv.Typed qt _ _) ->
      return $ prettyQt topLevel qt

    PatternNode (Slv.Typed qt _ (Slv.PVar name)) ->
      return $ name <> " :: " <> prettyQt False qt

    PatternNode (Slv.Typed qt _ _) ->
      return $ prettyQt False qt

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
              return $ maybeInterface >>= \(CanEnv.Interface vars _) -> do
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

    _ ->
      return ""
  return $
    "```madlib\n"
    <> typeInfo
    <> "\n"
    <> "```\n\n"
    <> "*Defined in '"
    <> modulePath
    <> "' at line "
    <> show (getNodeLine node)
    <> "*"


getDefinitionLink :: State -> Loc -> FilePath -> LspM () (Maybe Location)
getDefinitionLink state loc path = do
  options <- buildOptions
  (result, _, _) <- liftIO $ runTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (definitionLocationTask loc path)
  case result of
    Nothing ->
      return Nothing

    Just (path, area) ->
      return $ Just $ Location (pathToUri path) (areaToRange area)


getHoverInformation :: State -> Loc -> FilePath -> LspM () (Maybe String)
getHoverInformation state loc path = do
  options <- buildOptions
  (result, _, _) <- liftIO $ runTask state options { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
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
  options <- buildOptions
  let path = uriToPath uri
  (warnings, errs) <- liftIO $ do
    result <- try $
      runTask
        state
        options { optEntrypoint = path }
        Driver.Don'tPrune
        [path]
        fileUpdates
        (Driver.typeCheckFileTask path)
        :: IO (Either (Cyclic Query.Query) ((), [CompilationWarning], [CompilationError]))

    case result of
      Left _ -> do
        (_, warnings, errors) <- runTask
          state
          options { optEntrypoint = path }
          Driver.Don'tPrune
          [path]
          fileUpdates
          (Driver.detectCyleTask path)
        return (warnings, errors)

      Right (_, warnings, errors) ->
        return (warnings, errors)

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


hoverInfoTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe String)
hoverInfoTask loc path = do
  hasCycle <- Rock.fetch $ Query.DetectImportCycle [] path
  if hasCycle then
    return Nothing
  else do
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


definitionLocationTask :: Loc -> FilePath -> Rock.Task Query.Query (Maybe (FilePath, Area))
definitionLocationTask loc path = do
  hasCycle <- Rock.fetch $ Query.DetectImportCycle [] path
  if hasCycle then
    return Nothing
  else do
    (typedAst, _)  <- Rock.fetch $ Query.SolvedASTWithEnv path
    (canAst, _, _) <- Rock.fetch $ Query.CanonicalizedASTWithEnv path
    srcAst         <- Rock.fetch $ Query.ParsedAST path
    case findNameInNode $ findNodeInAst loc srcAst typedAst of
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
        return Nothing


runTask :: State -> Options.Options -> Driver.Prune -> [FilePath] -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (a, [CompilationWarning], [CompilationError])
runTask state options prune invalidatedPaths fileUpdates task =
  Driver.runIncrementalTask
    (_driverState state)
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
