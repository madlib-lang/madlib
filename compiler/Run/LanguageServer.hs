{-# LANGUAGE OverloadedStrings #-}
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

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageRequestParams MtInfo (T.pack "Turn on code lenses?")
            (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      _ <- sendRequest SWindowShowMessageRequest params $ \res ->
        case res of
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)
              
            _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      responder (Right $ Just rsp)
  ]

runLanguageServer :: IO Int
runLanguageServer = runServer $ ServerDefinition
  { defaultConfig = ()
  , onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }


-- data State = State
--   { _lspFuncs :: !(LSP.LspFuncs ())
-- --   , _driverState :: !(Driver.State (Error.Hydrated, Doc Void))
--   , _receiveMessage :: !(STM LSP.FromClientMessage)
--   , _diskChangeSignalled :: !(STM ())
-- --   , _diskFileStateVar :: !(MVar (HashSet FilePath, [FileSystem.Directory], HashMap FilePath Text))
-- --   , _sourceDirectories :: [FileSystem.Directory]
--   , _diskFiles :: HashMap FilePath Text.Text
-- --   , _openFiles :: HashMap FilePath Rope
--   , _changedFiles :: HashSet FilePath
--   }

-- -- initialState = State

-- -- initCallbacks :: LSP.InitializeCallbacks ()
-- -- initCallbacks =
  


-- messagePump :: State -> IO ()
-- messagePump state = do
--   sendNotification state $ Text.pack ("messagePump changed files: " <> show (_changedFiles state))
--   join $
--     atomically $
--       onMessage state <$> _receiveMessage state
--         <|> onDiskChange state <$ _diskChangeSignalled state
--         <|> onOutOfDate state <$ guard (not $ HashSet.null $ _changedFiles state)


-- onDiskChange :: State -> IO ()
-- onDiskChange state = do
-- --   (changedFiles, sourceDirectories, diskFiles) <- modifyMVar (_diskFileStateVar state) $ \(changedFiles, sourceDirectories, diskFiles) ->
-- --     pure ((mempty, sourceDirectories, diskFiles), (changedFiles, sourceDirectories, diskFiles))
--   messagePump state
--     -- state
--     --   { _diskFiles = diskFiles
--     --   , _changedFiles = changedFiles <> _changedFiles state
--     --   }

-- onOutOfDate :: State -> IO ()
-- onOutOfDate state = do
-- --   checkAllAndPublishDiagnostics state
--   messagePump
--     state
--       { _changedFiles = mempty
--       }

-- -- onMessage :: LSP.FromClientMessage -> a
-- onMessage :: State -> LSP.FromClientMessage -> IO ()
-- onMessage state message = case message of
--   LSP.ReqHover req -> do
--     sendNotification state $ Text.pack $ "messagePump: HoverRequest: " <> show req
--     let rsp = Just $ LSPTypes.Hover ms Nothing
--         ms = LSPTypes.HoverContents $ LSPTypes.markedUpContent (Text.pack "lsp-demo-simple-server") (Text.pack "Hello world")
--     LSP.sendFunc (_lspFuncs state) $ LSP.RspHover $ LSP.makeResponseMessage req rsp
--     messagePump state


-- handlers :: (LSP.FromClientMessage -> IO ()) -> LSP.Handlers
-- handlers sendMessage =
--   def { LSP.hoverHandler = Just $ sendMessage . LSP.ReqHover }

-- -- hoverHandler :: LSP.Handler LSPTypes.HoverRequest
-- -- hoverHandler = 


-- -- hoverHandler :: LSP.Handler LSPTypes.HoverRequest
-- -- hoverHandler config req = do
-- --   let rsp = LSPTypes.Hover ms Nothing
-- --       ms = LSPTypes.HoverContents $ LSPTypes.markedUpContent (Text.pack "lsp-demo-simple-server") (Text.pack "Hello world")
-- --   return ()
--     --   range = Range pos pos
-- --   return (Right $ Just rsp)
-- --   return ()


-- options :: LSP.Options
-- options = def


-- runLanguageServer :: IO Int
-- runLanguageServer = do
--   messageQueue <- newTQueueIO
--   signalChangeVar <- newEmptyTMVarIO
--   LSP.run
--     LSP.InitializeCallbacks
--       { LSP.onInitialConfiguration = \_ -> Right ()
--       , LSP.onConfigurationChange = \_ -> Right ()
--       , LSP.onStartup = \lspFuncs -> do
--           forkIO $ messagePump
--             State { _lspFuncs = lspFuncs
--                   , _receiveMessage = readTQueue messageQueue
--                   , _diskChangeSignalled = takeTMVar signalChangeVar
--                   }
--           return Nothing
--       }
--     (handlers $ atomically . writeTQueue messageQueue)
--     options
--     Nothing


-- sendNotification :: State -> Text.Text -> IO ()
-- sendNotification state s =
--   LSP.sendFunc (_lspFuncs state) $
--     LSP.NotLogMessage $
--       LSP.NotificationMessage (Text.pack "2.0") LSP.WindowLogMessage $
--         LSP.LogMessageParams LSP.MtInfo s

-- publishDiagnostics :: State -> LSP.Uri -> [LSP.Diagnostic] -> IO ()
-- publishDiagnostics state uri diagnostics =
--   LSP.sendFunc (_lspFuncs state) $
--     LSP.NotPublishDiagnostics $
--       LSP.NotificationMessage (Text.pack "2.0") LSP.TextDocumentPublishDiagnostics $
--         LSP.PublishDiagnosticsParams uri (LSP.List diagnostics)

-- diagnosticSource :: LSP.DiagnosticSource
-- diagnosticSource = (Text.pack "madlib")
