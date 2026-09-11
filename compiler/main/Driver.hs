{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Driver where


import           Control.Monad.Trans.Control
import           Control.Monad.Identity
import           Data.Constraint.Extras           (has')
import           Data.Dependent.HashMap           (DHashMap)
import qualified Data.Dependent.HashMap           as DHashMap
import           Data.Dependent.Sum               (DSum ((:=>)))
import           Data.Some                        (Some(Some))
import           Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet                     as HashSet
import           Data.IORef.Lifted
import qualified Data.Text.IO                     as Text
import           Driver.Query (Query)
import qualified Driver.Query                     as Query
import           Rock
import qualified Driver.Rules as Rules
import           Data.Functor.Const
import           Data.Text (Text)
import qualified Data.Text                        as Text
import           Control.Exception
import           Error.Error                      (CompilationError(CompilationError))
import qualified Utils.Path as Path
import           Data.Hashable                    (Hashable (hash))
import           Run.Options
import           Error.Warning                    (CompilationWarning(CompilationWarning))
import           GHC.IO.Handle.FD (stderr)
import           Control.Arrow (first)
import qualified Data.Set                             as Set
import qualified Data.List                            as List
import           Text.Show.Pretty (ppShow)
import qualified Data.Map                             as Map
import           Run.Target                           (Target(TNode, TLLVM))
import qualified Data.ByteString                      as BS
import qualified Data.Text.Encoding                   as TE
import           Utils.PathUtils                      (defaultPathUtils)
import qualified Utils.PathUtils                      as PathUtils
import           Control.Concurrent                   (MVar)
import qualified Explain.Format                       as Explain
import           Control.Monad.IO.Class               (liftIO)
import           Driver.Rules                         (rules)
import           Control.Concurrent                   (threadDelay, forkIO, ThreadId)
import qualified Control.FoldDebounce                 as Debounce
import           System.FSNotify
import           Data.Time.Clock
import qualified AST.Source as Src
import Data.Maybe (isJust)
import           System.Environment               (lookupEnv)
import qualified Infer.MonomorphizationState      as Mono



data State err = State
  { _startedVar :: !(IORef (DHashMap Query MemoEntry))
  , _hashesVar :: !(IORef (DHashMap Query (Const Int)))
  , _reverseDependenciesVar :: !(IORef (ReverseDependencies Query))
  -- The current forward edges let us replace, rather than accumulate, reverse
  -- edges whenever a query is recomputed.  Rock's stock tracker is additive.
  , _forwardDependenciesVar :: !(IORef (HashMap (Some Query) (HashSet (Some Query))) )
  , _monomorphizationSnapshotVar :: !(IORef MonomorphizationSnapshot)
  , _tracesVar :: !(IORef (Traces Query (Const Int)))
  , _errorsVar :: !(IORef (DHashMap Query (Const [err])))
  , _warningsVar :: !(IORef (DHashMap Query (Const [CompilationWarning])))
  }


initialState :: IO (State err)
initialState = do
  startedVar             <- newIORef mempty
  hashesVar              <- newIORef mempty
  reverseDependenciesVar <- newIORef mempty
  forwardDependenciesVar <- newIORef mempty
  monomorphizationSnapshotVar <- newIORef emptyMonomorphizationSnapshot
  tracesVar              <- newIORef mempty
  errorsVar              <- newIORef mempty
  warningsVar            <- newIORef mempty
  return
    State
      { _startedVar             = startedVar
      , _hashesVar              = hashesVar
      , _reverseDependenciesVar = reverseDependenciesVar
      , _forwardDependenciesVar = forwardDependenciesVar
      , _monomorphizationSnapshotVar = monomorphizationSnapshotVar
      , _tracesVar              = tracesVar
      , _errorsVar              = errorsVar
      , _warningsVar            = warningsVar
      }


setQueryResult :: IORef (DHashMap Query MemoEntry) -> Query a -> a -> IO ()
setQueryResult ref query value = do
  started <- readIORef ref
  let inserted = DHashMap.insert query (Done value) started
  atomicWriteIORef ref inserted


resetState :: State err -> IO ()
resetState state = do
  atomicWriteIORef (_startedVar state) mempty
  atomicWriteIORef (_hashesVar state) mempty
  atomicWriteIORef (_reverseDependenciesVar state) mempty
  atomicWriteIORef (_forwardDependenciesVar state) mempty
  atomicWriteIORef (_monomorphizationSnapshotVar state) emptyMonomorphizationSnapshot
  atomicWriteIORef (_tracesVar state) mempty
  atomicWriteIORef (_errorsVar state) mempty
  atomicWriteIORef (_warningsVar state) mempty


data Prune
  = Don'tPrune
  | Prune


runIncrementalTask ::
  State CompilationError ->
  Options ->
  [FilePath] ->
  Map.Map FilePath String ->
  Prune ->
  Task Query a ->
  IO (a, [CompilationWarning], [CompilationError])
runIncrementalTask state options changedFiles fileUpdates prune task = handleException $ do
  enabledMetrics <- (== Just "1") <$> lookupEnv "MADLIB_ROCK_METRICS"
  metricsVar <- newIORef Map.empty
  reverseDependencies <- readIORef $ _reverseDependenciesVar state
  started             <- readIORef $ _startedVar state
  hashes              <- readIORef $ _hashesVar state

  -- A program-wide monomorphization traversal is a coordination query, not a
  -- useful invalidation boundary.  Keep its outgoing edges while walking the
  -- source change; after it recomputes we invalidate only module projections
  -- whose generated specialization/import data changed.
  let monoKey = Some Query.MonomorphizedProgram
      monoParents = HashMap.lookupDefault mempty monoKey reverseDependencies
      reverseForInvalidation = HashMap.delete monoKey reverseDependencies
      (keysToInvalidate, reverseDependenciesWithoutMono) =
        List.foldl'
          ( \(keysToInvalidate_, reverseDependencies_) file ->
              let (inv1, rd1) = reachableReverseDependencies (Query.File file) reverseDependencies_
                  (inv2, rd2) = reachableReverseDependencies (Query.FileBS file) rd1
              in  first (<> keysToInvalidate_ <> inv2) (inv1, rd2)
          )
          (mempty, reverseForInvalidation)
          changedFiles
      reverseDependencies' = HashMap.insert monoKey monoParents reverseDependenciesWithoutMono
  let started' = DHashMap.difference started keysToInvalidate
      hashes'  = DHashMap.difference hashes keysToInvalidate

  atomicWriteIORef (_startedVar state) started'
  atomicWriteIORef (_hashesVar state) hashes'
  atomicWriteIORef (_reverseDependenciesVar state) reverseDependencies'

  threadDepsVar <- newIORef mempty
  let readSourceFile_ file
        | Just text <- Map.lookup file fileUpdates =
          return text
        | otherwise =
          readFile file `catch` \(_ :: IOException) -> pure mempty

      readSourceFileBS_ file
        | Just text <- Map.lookup file fileUpdates =
          return $! TE.encodeUtf8 (Text.pack text)
        | otherwise =
          BS.readFile file `catch` \(_ :: IOException) -> pure mempty

      traceFetch_ ::
        GenRules (Writer TaskKind Query) Query ->
        GenRules (Writer TaskKind Query) Query
      traceFetch_ = id
      -- traceFetch_ =
      --   traceFetch
      --     (\(Writer key) -> modifyMVar_ printVar $ \n -> do
      --       putText $ fold (replicate n "| ") <> "fetching " <> show key
      --       return $ n + 1)
      --     (\_ _ -> modifyMVar_ printVar $ \n -> do
      --       putText $ fold (replicate (n - 1) "| ") <> "*"
      --       return $ n - 1)

      writeErrorsAndWarnings :: Writer TaskKind Query a -> ([CompilationWarning], [CompilationError]) -> Task Query ()
      writeErrorsAndWarnings (Writer key) (warns, errs) = do
        atomicModifyIORef' (_warningsVar state) $
          (,()) . if null warns then DHashMap.delete key else DHashMap.insert key (Const warns)
        atomicModifyIORef' (_errorsVar state) $
          (,()) . if null errs then DHashMap.delete key else DHashMap.insert key (Const errs)
        return ()

      rules :: Rules Query
      rules =
        memoiseWithCycleDetection (_startedVar state) threadDepsVar $
          trackCurrentReverseDependencies
            (_reverseDependenciesVar state)
            (_forwardDependenciesVar state) $
            verifyTraces
              (_tracesVar state)
              ( \query value -> do
                  hashed <- readIORef $ _hashesVar state
                  case DHashMap.lookup query hashed of
                    Just h ->
                      pure h
                    Nothing -> do
                      let h =
                            Const $ has' @Hashable @Identity query $ hash $ Identity value
                      atomicModifyIORef' (_hashesVar state) $
                        (,()) . DHashMap.insert query h
                      pure h
              )
              $ traceFetch_
              $ writer writeErrorsAndWarnings
              $ profileRules state metricsVar
              $ Rules.rules options
                    { optPathUtils = (optPathUtils options)
                        { PathUtils.readFile = readSourceFile_
                        , PathUtils.strictByteStringReadFile = readSourceFileBS_
                        } }

  result    <- Rock.runTask rules task
  printMetrics enabledMetrics metricsVar (length $ DHashMap.toList keysToInvalidate) (_reverseDependenciesVar state)
  started   <- readIORef $ _startedVar state
  errorsMap <- case prune of
    Don'tPrune ->
      readIORef $ _errorsVar state

    Prune -> do
      atomicModifyIORef' (_tracesVar state) $
        (,()) . DHashMap.intersectionWithKey (\_ _ t -> t) started
      atomicModifyIORef' (_errorsVar state) $ \errors -> do
        let errors' = DHashMap.intersectionWithKey (\_ _ e -> e) started errors
        (errors', errors')

  warningsMap <- case prune of
    Don'tPrune ->
      readIORef $ _warningsVar state

    Prune -> do
      atomicModifyIORef' (_tracesVar state) $
        (,()) . DHashMap.intersectionWithKey (\_ _ t -> t) started
      atomicModifyIORef' (_warningsVar state) $ \warnings -> do
        let warnings' = DHashMap.intersectionWithKey (\_ _ e -> e) started warnings
        (warnings', warnings')

  let errors = do
        (_ :=> Const errs) <- DHashMap.toList errorsMap
        errs
  let warnings = do
        (_ :=> Const warns) <- DHashMap.toList warningsMap
        warns

  atomicModifyIORef' (_errorsVar state) $ \errMap ->
    let filtered = DHashMap.filterWithKey
                    (\k _ -> case k of
                      Query.DetectImportCycle _ _ ->
                        False

                      _ ->
                        True
                    )
                    errMap
    in  (filtered, filtered)

  pure (result, warnings, errors)
  where
    handleException m =
      m `catch` \e -> do
        -- TODO: test if we can just restore the initial state here instead
        resetState state
        throw (e :: SomeException)


ignoreTaskKind :: Rock.GenRules (Rock.Writer Rock.TaskKind f) f -> Rock.Rules f
ignoreTaskKind rs key = fst <$> rs (Rock.Writer key)


-- | Rock's 'trackReverseDependencies' only ever adds edges.  That is safe but
-- makes the graph increasingly conservative after imports are removed.  Keep
-- the current forward edge set for each query so a recomputation atomically
-- replaces its old reverse edges with the observed ones.
trackCurrentReverseDependencies
  :: IORef (ReverseDependencies Query)
  -> IORef (HashMap (Some Query) (HashSet (Some Query)))
  -> Rock.Rules Query
  -> Rock.Rules Query
trackCurrentReverseDependencies reverseVar forwardVar rules key = do
  (result, deps) <- Rock.track (\_ _ -> Const ()) (rules key)
  let parent = Some key
      children = HashSet.fromList
        [ Some child
        | child :=> Const () <- DHashMap.toList deps
        ]
  oldChildren <- atomicModifyIORef' forwardVar $ \forward ->
    let old = HashMap.lookupDefault mempty parent forward
        forward' =
          if HashSet.null children
            then HashMap.delete parent forward
            else HashMap.insert parent children forward
    in (forward', old)
  atomicModifyIORef' reverseVar $ \reverseDeps ->
    let withoutOld = HashSet.foldl'
          (\acc child ->
            HashMap.update
              (\parents ->
                let parents' = HashSet.delete parent parents
                in if HashSet.null parents' then Nothing else Just parents'
              )
              child
              acc
          )
          reverseDeps
          oldChildren
        withCurrent = HashSet.foldl'
          (\acc child -> HashMap.insertWith (<>) child (HashSet.singleton parent) acc)
          withoutOld
          children
    in (withCurrent, ())
  return result


type QueryMetrics = Map.Map String (Int, NominalDiffTime)


-- | The monomorphizer is currently stateful, but its consumers are modular.
-- These fingerprints bridge that gap until specialization itself becomes a
-- pure per-module query: only a changed module projection is evicted.
data MonomorphizationSnapshot = MonomorphizationSnapshot
  { monoModuleFingerprints :: !(Map.Map FilePath Int)
  , monoMethodsFingerprint :: !Int
  }


emptyMonomorphizationSnapshot :: MonomorphizationSnapshot
emptyMonomorphizationSnapshot = MonomorphizationSnapshot mempty 0


captureMonomorphizationSnapshot :: IO MonomorphizationSnapshot
captureMonomorphizationSnapshot = do
  byModule <- readIORef Mono.monomorphizationStateByModule
  imports <- readIORef Mono.monomorphizationImports
  methods <- readIORef Mono.monomorphicMethods
  let paths = Set.toList $ Set.union (Map.keysSet byModule) (Map.keysSet imports)
      fingerprints = Map.fromList
        [ (path, hash (show (Map.lookup path byModule), show (Map.lookup path imports)))
        | path <- paths
        ]
  return $ MonomorphizationSnapshot fingerprints (hash $ show methods)


invalidateQueryClosure :: State err -> [Some Query] -> IO ()
invalidateQueryClosure state roots = do
  reverseDeps <- readIORef (_reverseDependenciesVar state)
  started <- readIORef (_startedVar state)
  hashes <- readIORef (_hashesVar state)
  let (keys, reverseDeps') = List.foldl'
        (\(invalidated, graph) (Some query) ->
          let (more, graph') = reachableReverseDependencies query graph
          in (invalidated <> more, graph')
        )
        (mempty, reverseDeps)
        roots
  atomicWriteIORef (_startedVar state) $ DHashMap.difference started keys
  atomicWriteIORef (_hashesVar state) $ DHashMap.difference hashes keys
  atomicWriteIORef (_reverseDependenciesVar state) reverseDeps'


refreshMonomorphizationModules :: State err -> IO ()
refreshMonomorphizationModules state = do
  current <- captureMonomorphizationSnapshot
  previous <- atomicModifyIORef' (_monomorphizationSnapshotVar state) (\old -> (current, old))
  let methodsChanged = monoMethodsFingerprint previous /= monoMethodsFingerprint current
      oldFingerprints = monoModuleFingerprints previous
      newFingerprints = monoModuleFingerprints current
      changedPaths
        | methodsChanged = Map.keys newFingerprints
        | otherwise =
            [ path
            | path <- Set.toList $ Set.union (Map.keysSet oldFingerprints) (Map.keysSet newFingerprints)
            , Map.lookup path oldFingerprints /= Map.lookup path newFingerprints
            ]
  invalidateQueryClosure state (Some . Query.MonomorphizedAST <$> changedPaths)


profileRules
  :: State err
  -> IORef QueryMetrics
  -> Rock.GenRules (Rock.Writer ([CompilationWarning], [CompilationError]) (Rock.Writer Rock.TaskKind Query)) Query
  -> Rock.GenRules (Rock.Writer ([CompilationWarning], [CompilationError]) (Rock.Writer Rock.TaskKind Query)) Query
profileRules state metrics rules key@(Rock.Writer (Rock.Writer query)) = do
  started <- liftIO getCurrentTime
  result <- rules key
  finished <- liftIO getCurrentTime
  liftIO $ atomicModifyIORef' metrics $ \entries ->
    let name = queryTag query
        (count, elapsed) = Map.findWithDefault (0, 0) name entries
    in (Map.insert name (count + 1, elapsed + diffUTCTime finished started) entries, ())
  case query of
    Query.MonomorphizedProgram -> liftIO $ refreshMonomorphizationModules state
    _ -> return ()
  return result


queryTag :: Query a -> String
queryTag = filter (/= '(') . takeWhile (/= ' ') . drop 5 . show . Some


printMetrics :: Bool -> IORef QueryMetrics -> Int -> IORef (ReverseDependencies Query) -> IO ()
printMetrics enabled metrics invalidated reverseVar = when enabled $ do
  entries <- readIORef metrics
  reverseDeps <- readIORef reverseVar
  let sorted = List.sortOn (negate . realToFrac . snd . snd) (Map.toList entries)
      topFanout = take 5 $ List.sortOn (negate . HashSet.size . snd) (HashMap.toList reverseDeps)
  putStrLn $ "Rock metrics: invalidated=" <> show invalidated
  forM_ (take 12 sorted) $ \(name, (count, elapsed)) ->
    putStrLn $ "  " <> name <> ": " <> show count <> " run(s), " <> show (round (elapsed * 1000) :: Integer) <> "ms"
  unless (null topFanout) $ do
    putStrLn "  highest reverse fan-out:"
    forM_ topFanout $ \(Some query, parents) ->
      putStrLn $ "    " <> queryTag query <> ": " <> show (HashSet.size parents)


typeCheckFileTask :: FilePath -> Rock.Task Query.Query ()
typeCheckFileTask path = do
  Rock.fetch $ Query.SolvedASTWithEnv path
  return ()


compilationTask :: FilePath -> Rock.Task Query ()
compilationTask path = do
  Rock.fetch $ Query.BuiltTarget path


detectCyleTask :: FilePath -> Rock.Task Query ()
detectCyleTask path = do
  Rock.fetch $ Query.DetectImportCycle [] path
  return ()


-- Measure time

recordAndPrintDuration :: String -> IO a -> IO a
recordAndPrintDuration title action = do
  startT       <- getCurrentTime
  actionResult <- action
  endT         <- getCurrentTime
  let diff = diffUTCTime endT startT
  let (ms, _) = properFraction $ diff * 1000
  putStrLn $ title <> show ms <> "ms"
  return actionResult

-- Watch

watch :: FilePath -> ([FilePath] -> IO ()) -> IO ThreadId
watch root action =
  forkIO $ withManager $ \mgr -> do
    trigger <-
      Debounce.new
        Debounce.Args
          { Debounce.cb = action
          , Debounce.fold = \l v -> List.nub $ v:l
          , Debounce.init = []
          }
        Debounce.def
          { Debounce.delay = 50000 -- 50ms
          , Debounce.alwaysResetTimer = True
          }

    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      root         -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          f = case e of
                Added f _ _ ->
                  f

                Modified f _ _ ->
                  f

                Removed f _ _ ->
                  f

                Unknown f _ _ ->
                  f

          -- TODO: it would be better to not listen to these folders in the `watchTree` when available
          -- https://github.com/haskell-fswatch/hfsnotify/issues/101
          shouldTrigger = ".mad" `List.isSuffixOf` f

        when shouldTrigger $ Debounce.send trigger f
      )

    -- sleep forever (until interrupted via killThread)
    forever $ threadDelay 1000000
