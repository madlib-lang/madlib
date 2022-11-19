{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use forM_" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use list comprehension" #-}
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
import           Explain.Format (prettyPrintQualType, prettyPrintType, kindToStr, prettyPrintTyping, prettyPrintTyping', renderSchemesWithDiff)
import           Control.Applicative ((<|>))
import           Infer.Type (Qual((:=>)), Type (..), kind, Kind (Star), TCon (..), TVar (..), findTypeVarInType, collectVars, buildKind, getQualified, Scheme (Forall))
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
import System.FilePath (takeFileName, dropExtension)



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
  , requestHandler STextDocumentDefinition $ \(RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) (Position line col) _ _)) responder -> do
      recordAndPrintDuration "definition" $ do
        links <- getDefinitionLinks state (Loc 0 (line + 1) (col + 1)) (uriToPath uri)
        case links of
          [] ->
            return ()

          [loc] ->
            responder $ Right (InL loc)

          locs ->
            responder $ Right (InR $ InL $ List locs)
  -- , requestHandler STextDocumentCompletion $ \(RequestMessage _ _ _ completionParams) responder -> do
  --     sendNotification SWindowLogMessage $ LogMessageParams MtInfo (T.pack $ ppShow completionParams)

  -- , requestHandler STextDocumentDocumentLink $ \req responder -> do
  --     sendNotification SWindowLogMessage $ LogMessageParams MtInfo (T.pack $ ppShow req)
  -- , requestHandler STextDocumentImplementation $ \_ responder -> do
  --     sendNotification SWindowLogMessage $ LogMessageParams MtInfo "tdi"
  , notificationHandler STextDocumentDidOpen $ \(NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) -> do
      recordAndPrintDuration "file open" $ generateDiagnostics False state uri mempty
  , notificationHandler STextDocumentDidSave $ \(NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _)) ->
      recordAndPrintDuration "file save" $ generateDiagnostics False state uri mempty
  , notificationHandler STextDocumentDidChange $ \(NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) (List changes))) -> do
      recordAndPrintDuration "file change" $ do
        let (TextDocumentContentChangeEvent _ _ docContent) = last changes
        generateDiagnostics True state uri (Map.singleton (uriToPath uri) (T.unpack docContent))
  ]


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
prettyQt topLevel qt@(_ :=> t)
  | qt == failedQt = "_"
  | topLevel       = let (r, _) = renderSchemesWithDiff False (Forall [] qt) (Forall [] qt) in r
  | otherwise      = let (r, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t)) in r


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

            Slv.Placeholder _ exp' ->
              findNodeAtLoc False loc exp'

            Slv.If cond truthy falsy ->
              findNodeAtLoc False loc cond
              <|> findNodeAtLoc False loc truthy
              <|> findNodeAtLoc False loc falsy

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
          Nothing | Slv.isPlaceholderExp input ->
            Nothing

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
      Src.Source _ _ (Src.DefaultImport _ _ filepath) ->
        Just $ DefaultImportNode (Area (Loc 1 1 1) (Loc 1 100000 1)) filepath

      Src.Source _ _ (Src.NamedImport names _ filepath) ->
        foldl' (<|>) Nothing (findNamedImportNode loc filepath NamedImportNode <$> names)
        <|> Just (DefaultImportNode (Area (Loc 1 1 1) (Loc 1 100000 1)) filepath)

      Src.Source _ _ (Src.TypeImport typeNames _ filepath) ->
        foldl' (<|>) Nothing (findNamedImportNode loc filepath TypeImportNode <$> typeNames)
        <|> Just (DefaultImportNode (Area (Loc 1 1 1) (Loc 1 100000 1)) filepath)
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
  TCon (TC _ k) _ ->
    k

  TApp l _ ->
    retrieveKind l

  TVar (TV _ k) ->
    k

  _ ->
    kind t


failedQt :: Qual Type
failedQt = [] :=> TVar (TV "-" Star)


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


getDefinitionLinks :: State -> Loc -> FilePath -> LspM () [Location]
getDefinitionLinks state loc path = do
  jsOptions        <- buildOptions TNode
  (jsResult, _, _) <- liftIO $ runTask state jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (definitionLocationTask loc path)
  let jsLocations = case jsResult of
        Nothing ->
          []

        Just (path, area) ->
          [Location (pathToUri path) (areaToRange area)]

  llvmOptions        <- buildOptions TLLVM
  (llvmResult, _, _) <- liftIO $ runTask state llvmOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (definitionLocationTask loc path)
  case llvmResult of
    Nothing ->
      return jsLocations

    Just (path, area) ->
      return $ [Location (pathToUri path) (areaToRange area)] `List.union` jsLocations


getHoverInformation :: State -> Loc -> FilePath -> LspM () (Maybe String)
getHoverInformation state loc path = do
  jsOptions <- buildOptions TNode
  (jsResult, _, _) <- liftIO $ runTask state jsOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
  if Maybe.isNothing jsResult then do
    llvmOptions <- buildOptions TLLVM
    (llvmResult, _, _) <- liftIO $ runTask state llvmOptions { optEntrypoint = path } Driver.Don'tPrune mempty mempty (hoverInfoTask loc path)
    return llvmResult
  else
    return jsResult


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


runTypeCheck :: Bool -> State -> Target -> FilePath -> Map.Map FilePath String -> LspM () ([CompilationWarning], [CompilationError])
runTypeCheck invalidatePath state target path fileUpdates = do
  let changedFiles =
        if invalidatePath then
          [path]
        else
          []
  options <- buildOptions target
  liftIO $ do
    result <- try $
      runTask
        state
        options { optEntrypoint = path }
        Driver.Don'tPrune
        changedFiles
        fileUpdates
        (Driver.typeCheckFileTask path)
        :: IO (Either (Cyclic Query.Query) ((), [CompilationWarning], [CompilationError]))

    case result of
      Left _ -> do
        (_, warnings, errors) <- runTask
          state
          options { optEntrypoint = path }
          Driver.Don'tPrune
          changedFiles
          fileUpdates
          (Driver.detectCyleTask path)
        return (warnings, errors)

      Right (_, warnings, errors) ->
        return (warnings, errors)


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


generateDiagnostics :: Bool -> State -> Uri -> Map.Map FilePath String -> LspM () ()
generateDiagnostics invalidatePath state uri fileUpdates = do
  let path = uriToPath uri
  (jsWarnings, jsErrors)     <- runTypeCheck invalidatePath state TNode path fileUpdates
  (llvmWarnings, llvmErrors) <- runTypeCheck invalidatePath state TLLVM path fileUpdates
  let allWarnings = jsWarnings `List.union` llvmWarnings
  let allErrors   = jsErrors `List.union` llvmErrors

  sendDiagnosticsForWarningsAndErrors allWarnings allErrors


uriOfError :: CompilationError -> Uri
uriOfError err = case Error.getContext err of
  Context path _ ->
    Uri $ T.pack ("file://" <> path)


uriOfWarning :: CompilationWarning -> Uri
uriOfWarning warning = case Warning.getContext warning of
  Context path _ ->
    Uri $ T.pack ("file://" <> path)


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
  in  Map.fromList $ (\errs -> (Error.getPath $ head errs, errs)) <$> List.groupBy areErrorsFromSameModule errs'


groupWarnsByModule :: [CompilationWarning] -> Map.Map FilePath [CompilationWarning]
groupWarnsByModule warnings =
  let warnings' = filter ((/= NoContext) . Warning.getContext) warnings
  in  Map.fromList $ (\warnings -> (Warning.getPath $ head warnings, warnings)) <$> List.groupBy areWarningsFromSameModule warnings'



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
  }


textDocumentSyncOptions :: TextDocumentSyncOptions
textDocumentSyncOptions =
  TextDocumentSyncOptions
    (Just True)       -- _openClose
    (Just TdSyncFull) -- _change
    Nothing           -- _willSave
    Nothing           -- _willSaveWaitUntil
    (Just $ InL True) -- _save


runLanguageServer :: IO ()
runLanguageServer = do
  jsDriverState <- Driver.initialState
  llvmDriverState <- Driver.initialState
  openFiles <- newIORef mempty
  let state = State jsDriverState llvmDriverState openFiles mempty
  runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers state
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions { textDocumentSync = Just textDocumentSyncOptions }
    }
  return ()


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


runTask :: State -> Options.Options -> Driver.Prune -> [FilePath] -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (a, [CompilationWarning], [CompilationError])
runTask state options prune invalidatedPaths fileUpdates task = do
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
