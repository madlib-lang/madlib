{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE NamedFieldPuns #-}
module Run.LanguageServer where

-- import qualified Language.Haskell.LSP.Control           as LSP
-- import qualified Language.Haskell.LSP.Core              as LSP
-- import qualified Language.Haskell.LSP.Types             as LSPTypes
-- import           Data.Default (def)
-- import qualified Data.Text as Text
-- import qualified Language.Haskell.LSP.Types as LSP
-- import qualified Language.Haskell.LSP.Messages as LSP
-- import           Data.HashSet (HashSet)
-- import qualified Data.HashSet as HashSet
-- import           Data.HashMap.Lazy (HashMap)
-- import qualified Data.HashMap.Lazy as HashMap
-- import GHC.Conc
-- import Control.Concurrent.STM
-- import Control.Monad
-- import Control.Applicative
-- import Control.Concurrent


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
import Data.List (group)
import qualified Data.List as List
import Control.Monad (forM_)


handlers :: State -> Handlers (LspM ())
handlers state = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      -- let params = ShowMessageRequestParams MtInfo (T.pack "Turn on code lenses?")
      --       (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      -- _ <- sendRequest SWindowShowMessageRequest params $ \res ->
      --   case res of
      --     Right (Just (MessageActionItem "Turn on")) -> do
      --       let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

      --       _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
      --         let cmd = Command "Say hello" "lsp-hello-command" Nothing
      --             rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
      --         responder (Right rsp)
      --       pure ()
      --     Right _ ->
      --       sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
      --     Left err ->
      --       sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
      sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Initialized")
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      p <- getRootPath
      let path = case p of
            Just x ->
              x

            _ ->
              "no path"
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "madlib" "hover"
          range = Range pos pos
      responder (Right $ Just rsp)
  , notificationHandler STextDocumentDidOpen $ \(NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) -> do
      generateDiagnostics state uri

  , notificationHandler STextDocumentDidSave $ \(NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _)) -> do
      generateDiagnostics state uri
  ]


-- publishDiagnostics :: MonadLsp config m => Int -> NormalizedUri -> TextDocumentVersion -> DiagnosticsBySource -> m ()

uriToPath :: Uri -> FilePath
uriToPath uri =
  let unpacked = T.unpack $ getUri uri
  in  if take 7 unpacked == "file://" then
        drop 7 unpacked
      else
        unpacked

generateDiagnostics :: State -> Uri -> LspM () ()
generateDiagnostics state uri = do
  let path = uriToPath uri
  -- retrieve current errors first, so that we can update diagnostics of files
  -- that went from errors > 0 to 0 and remove them completely.
  (_, errs) <- liftIO $ runTask state Driver.Don'tPrune [path] (typeCheckFile path)
  let errsByModule = groupErrsByModule errs


  forM_ errsByModule $ \errs' -> do
    let moduleUri = uriOfError $ head errs'
    diagnostics <- liftIO $ mapM errorToDiagnostic errs'
    let diagnosticsBySource = partitionBySource diagnostics
    -- sendNotification SWindowLogMessage $ LogMessageParams MtInfo ("errs: " <> T.pack (ppShow errs))
    -- flushDiagnosticsBySource 20 (Just "Madlib")
    publishDiagnostics 20 (toNormalizedUri moduleUri) Nothing diagnosticsBySource


uriOfError :: CompilationError -> Uri
uriOfError err = case getContext err of
  Context path _ _ ->
    Uri $ T.pack ("file://" <> path)


areErrorsFromSameModule :: CompilationError -> CompilationError -> Bool
areErrorsFromSameModule a b = case (a, b) of
  (CompilationError _ (Context pathA _ _), CompilationError _ (Context pathB _ _)) ->
    pathA == pathB

  _ ->
    False


groupErrsByModule :: [CompilationError] -> [[CompilationError]]
groupErrsByModule errs =
  let errs' = filter ((/= NoContext) . getContext) errs
  in  List.groupBy areErrorsFromSameModule errs'



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
  , _openFiles :: Map.Map FilePath String
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
  let state = State driverState mempty mempty
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


runTask :: State -> Driver.Prune -> [FilePath] -> Rock.Task Query.Query a -> IO (a, [CompilationError])
runTask state prune invalidatedPaths task = do
  -- filePaths <- Rock.fetch $ Query.ModulePathsToBuild path
  -- files <- mapM
  --       (\path -> do
  --           file <- Rock.fetch $ Query.File path
  --           return (path, file)
  --       )
  --       filePaths
  Driver.runIncrementalTask
    (_driverState state)
    invalidatedPaths
    -- (_changedFiles state)
    prune
    task
