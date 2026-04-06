{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run.LanguageServer.State
  ( State(..)
  , textDocumentSyncOptions
  , pathToUri
  , uriToPath
  , areaToRange
  , noRange
  , buildOptions
  , runTask
  , safeRunTask
  , typeCheckFileTask
  , runTypeCheckIO
  , runTypeCheck
  , copyStateTo
  , recordAndPrintDuration
  ) where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Rock
import qualified Driver
import qualified Driver.Query as Query
import qualified Data.Map as Map
import qualified Data.Set as Set
import Explain.Location (Area(..))
import qualified Explain.Location as Loc
import           Data.IORef
import qualified AST.Source as Src
import           Data.Time.Clock
import qualified Run.Options as Options
import qualified Utils.PathUtils as PathUtils
import           Run.Target
import qualified Data.Maybe as Maybe
import           Run.Options (Options(optEntrypoint))
import           Control.Concurrent
import           Control.Exception (SomeException, try)
import           Rock (Cyclic)
import           Error.Error (CompilationError(..))
import qualified Error.Warning as Warning
import           Error.Warning (CompilationWarning(..))
import Run.OptimizationLevel
import Run.SourceMapMode
import           Run.ErrorFormat (ErrorFormat(..))
import           Run.PGOMode (PGOMode(..))
import Data.Maybe (isJust)
import qualified Infer.Env as SlvEnv


data State = State
  { _jsDriverState :: Driver.State CompilationError
  , _llvmDriverState :: Driver.State CompilationError
  , _openFiles :: IORef (Map.Map FilePath String)
  , _changedFiles :: Set.Set FilePath
  , _debounceRef :: IORef (Maybe UTCTime)
  , _stateLock :: MVar ()
  , _allModulePaths :: IORef (Set.Set FilePath)
  , _backgroundDone :: IORef Bool
  , _interfaceSnapshots :: IORef (Map.Map FilePath (SlvEnv.Vars, SlvEnv.Methods))
  }


textDocumentSyncOptions :: TextDocumentSyncOptions
textDocumentSyncOptions =
  TextDocumentSyncOptions
    (Just True)       -- _openClose
    (Just TdSyncFull) -- _change
    Nothing           -- _willSave
    Nothing           -- _willSaveWaitUntil
    (Just $ InL True) -- _save


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


areaToRange :: Area -> Range
areaToRange area =
  Range
    (Position (max 0 (Loc.getLine (Loc.getStartLoc area) - 1)) (max 0 (Loc.getCol (Loc.getStartLoc area) - 1)))
    (Position (max 0 (Loc.getLine (Loc.getEndLoc area) - 1)) (max 0 (Loc.getCol (Loc.getEndLoc area) - 1)))


noRange :: Range
noRange =
  Range (Position 0 0) (Position 0 0)


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
      , Options.optSourceMaps = NoSourceMap
      , Options.optDebug = False
      , Options.optErrorFormat = TextFormat
      , Options.optPGOMode = NoPGO
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


typeCheckFileTask :: FilePath -> Rock.Task Query.Query Bool
typeCheckFileTask path = do
  parsedAst <- Rock.fetch $ Query.ParsedAST path
  Rock.fetch $ Query.SolvedASTWithEnv path
  return $ isJust (Src.apath parsedAst)


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


safeRunTask :: State -> Options.Options -> Driver.Prune -> [FilePath] -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (Maybe (a, [CompilationWarning], [CompilationError]))
safeRunTask state options prune invalidatedPaths fileUpdates task = do
  result <- try $ runTask state options prune invalidatedPaths fileUpdates task
  case result of
    Right r  -> return (Just r)
    Left (_ :: SomeException) ->
      return Nothing


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
