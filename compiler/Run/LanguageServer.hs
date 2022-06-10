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
      -- x <- liftIO $ Rock.fetch $ Query.ModulePathsToBuild "entrypoint"
      p <- getRootPath
      let path = case p of
            Just x ->
              x

            _ ->
              "no path"
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "madlib" ("hover in: " <> T.pack path <> T.pack (ppShow _doc))
          range = Range pos pos
      -- liftIO $ putStrLn (ppShow _doc)
      responder (Right $ Just rsp)
  , notificationHandler STextDocumentDidOpen $ \(NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) -> do
      sendNotification SWindowLogMessage $ LogMessageParams MtInfo ("open: " <> getUri uri)

  ]


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


runTask :: State -> Driver.Prune -> Rock.Task Query.Query a -> IO (a, [CompilationError])
runTask state prune task = do
  -- let prettyError :: Error.Hydrated -> Task Query (Error.Hydrated, Doc ann)
  --     prettyError err = do
  --       (heading, body) <- Error.Hydrated.headingAndBody $ Error.Hydrated._error err
  --       pure (err, heading <> Doc.line <> body)

      -- files =
      --   fmap Rope.toText (_openFiles state) <> _diskFiles state

  Driver.runIncrementalTask
    (_driverState state)
    (_changedFiles state)
    -- (HashSet.fromList $ _sourceDirectories state)
    -- files
    -- prettyError
    prune
    task
