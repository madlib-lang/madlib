{-# LANGUAGE OverloadedStrings #-}
module Run.LanguageServer.Diagnostics
  ( generateDiagnostics
  , sendDiagnosticsForWarningsAndErrors
  , errorToDiagnostic
  , warningToDiagnostic
  , groupErrsByModule
  , groupWarnsByModule
  , isErrorFromModule
  , areErrorsFromSameModule
  , isWarningFromModule
  , areWarningsFromSameModule
  , errorsForModule
  , warningsForModule
  , uriOfError
  , uriOfWarning
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
import Error.Error
import Error.Context
import qualified Explain.Format as Explain
import qualified Data.List as List
import           Control.Monad (forM_, forM)
import           Data.IORef
import qualified Error.Warning as Warning
import           Error.Warning (CompilationWarning(CompilationWarning))
import qualified Error.Error as Error
import           Run.Target
import           Language.LSP.Diagnostics
import qualified Infer.Env as SlvEnv
import GHC.Base (when)
import           Infer.Type (isFunctionType)

import Run.LanguageServer.State
import Run.Options (optEntrypoint)
import qualified Run.Options as Options


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
  -- Update interface snapshot after successful type-check.
  -- This snapshot is used to detect whether the exported interface has changed,
  -- so we can skip cascading re-checks when it hasn't.
  interfaceChanged <- if null jsErrors && invalidatePath
    then do
      options <- buildOptions TNode
      maybeResult <- liftIO $ safeRunTask state options { optEntrypoint = path } Driver.Don'tPrune mempty fileUpdates $ do
        (_, env) <- Rock.fetch $ Query.SolvedASTWithEnv path
        return (SlvEnv.envVars env, SlvEnv.envMethods env)
      case maybeResult of
        Just ((newVars, newMethods), _, _) -> do
          oldSnapshot <- liftIO $ Map.lookup path <$> readIORef (_interfaceSnapshots state)
          liftIO $ modifyIORef' (_interfaceSnapshots state) (Map.insert path (newVars, newMethods))
          return $ case oldSnapshot of
            Just (oldVars, oldMethods) -> oldVars /= newVars || oldMethods /= newMethods
            Nothing                    -> True  -- No previous snapshot, assume changed
        _ -> return True
    else return False

  -- When a file changes and its exported interface changed, re-check all known
  -- module paths so that diagnostics for dependent modules get updated.
  -- Rock's cache has already been invalidated for reverse dependencies, so
  -- re-running typeCheckFileTask for them will recompute as needed; modules
  -- whose cache is still valid will be a fast no-op.
  (depWarnings, depErrors) <- if invalidatePath && interfaceChanged
    then do
      bgDone <- liftIO $ readIORef (_backgroundDone state)
      if bgDone
        then do
          allPaths <- liftIO $ Set.toList <$> readIORef (_allModulePaths state)
          let otherPaths = filter (/= path) allPaths
          depResults <- forM otherPaths $ \depPath -> do
            (_, w, e) <- runTypeCheck False state TNode depPath mempty
            return (w, e)
          let (ws, es) = unzip depResults
          return (concat ws, concat es)
        else return ([], [])
    else return ([], [])

  let allWarnings = jsWarnings `List.union` llvmWarnings `List.union` depWarnings
  let allErrors   = jsErrors `List.union` llvmErrors `List.union` depErrors

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
  formattedWarning <- Explain.simpleFormatWarningWithHints True warning
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
  formattedError <- Explain.simpleFormatErrorWithHints True err
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
