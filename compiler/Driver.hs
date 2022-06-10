{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Driver where


import Control.Monad.Trans.Control
import Control.Monad.Identity
import Data.Constraint.Extras (has')
import Data.Dependent.HashMap (DHashMap)
import qualified Data.Dependent.HashMap as DHashMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef.Lifted
import qualified Data.Text.IO as Text
import Driver.Query (Query)
import qualified Driver.Query as Query
import Rock
import qualified Driver.Rules as Rules
import Data.Functor.Const
import Data.Text (Text)
import qualified Data.Text as Text
import           Control.Exception
import Error.Error (CompilationError(CompilationError))
import qualified Utils.Path as Path
import Data.Hashable (Hashable (hash))
import Run.Options
import Error.Warning (CompilationWarning(CompilationWarning))
import GHC.IO.Handle.FD (stderr)
import Control.Arrow (first)
import qualified Data.Set as Set
import qualified Data.List as List
import Text.Show.Pretty (ppShow)
import qualified Data.Map as Map
import Run.Target (Target(TNode, TLLVM))
import Utils.PathUtils (defaultPathUtils)
import qualified Utils.PathUtils as PathUtils




data State err = State
  { _startedVar :: !(IORef (DHashMap Query MemoEntry))
  , _hashesVar :: !(IORef (DHashMap Query (Const Int)))
  , _reverseDependenciesVar :: !(IORef (ReverseDependencies Query))
  , _tracesVar :: !(IORef (Traces Query (Const Int)))
  , _errorsVar :: !(IORef (DHashMap Query (Const [err])))
  , _warningsVar :: !(IORef (DHashMap Query (Const [CompilationWarning])))
  }


initialState :: IO (State err)
initialState = do
  startedVar <- newIORef mempty
  hashesVar <- newIORef mempty
  reverseDependenciesVar <- newIORef mempty
  tracesVar <- newIORef mempty
  errorsVar <- newIORef mempty
  warningsVar <- newIORef mempty
  return
    State
      { _startedVar = startedVar
      , _hashesVar = hashesVar
      , _reverseDependenciesVar = reverseDependenciesVar
      , _tracesVar = tracesVar
      , _errorsVar = errorsVar
      , _warningsVar = warningsVar
      }

data Prune
  = Don'tPrune
  | Prune


runIncrementalTask ::
  State CompilationError ->
  [FilePath] ->
  Map.Map FilePath String ->
  Prune ->
  Task Query a ->
  IO (a, [CompilationError])
runIncrementalTask state changedFiles fileUpdates prune task =
  handleEx $ do
    reverseDependencies <- readIORef $ _reverseDependenciesVar state
    started <- readIORef $ _startedVar state
    hashes <- readIORef $ _hashesVar state

    let (keysToInvalidate, reverseDependencies') =
          List.foldl'
            ( \(keysToInvalidate_, reverseDependencies_) file ->
                first (<> keysToInvalidate_) $ reachableReverseDependencies (Query.File file) reverseDependencies_
            )
            (mempty, reverseDependencies)
            changedFiles
    let started' = DHashMap.difference started keysToInvalidate
        hashes' = DHashMap.difference hashes keysToInvalidate

    atomicWriteIORef (_startedVar state) started'
    atomicWriteIORef (_hashesVar state) hashes'
    atomicWriteIORef (_reverseDependenciesVar state) reverseDependencies'

    threadDepsVar <- newIORef mempty
    let readSourceFile_ file
          | Just text <- Map.lookup file fileUpdates =
            return text
          | otherwise =
            readFile file `catch` \(_ :: IOException) -> pure mempty

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
        writeErrorsAndWarnings (Writer key) (_, errs) = do
        --   errs' <- mapM (prettyError <=< Error.Hydrated.fromError) errs
          atomicModifyIORef' (_errorsVar state) $
            (,()) . if null errs then DHashMap.delete key else DHashMap.insert key (Const errs)
          return ()
        rules :: Rules Query
        rules =
          memoiseWithCycleDetection (_startedVar state) threadDepsVar $
            trackReverseDependencies (_reverseDependenciesVar state) $
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
                $ Rules.rules Options { optEntrypoint = head changedFiles, optTarget = TNode, optRootPath = "/Users/arnaudboeglin/Code/madlib/", optOutputPath = "", optOptimized = False, optPathUtils = PathUtils.defaultPathUtils { PathUtils.readFile = readSourceFile_ } }

    result <- Rock.runTask rules task
    started <- readIORef $ _startedVar state
    errorsMap <- case prune of
      Don'tPrune ->
        readIORef $ _errorsVar state
      Prune -> do
        atomicModifyIORef' (_tracesVar state) $
          (,()) . DHashMap.intersectionWithKey (\_ _ t -> t) started
        atomicModifyIORef' (_errorsVar state) $ \errors -> do
          let errors' = DHashMap.intersectionWithKey (\_ _ e -> e) started errors
          (errors', errors')

    let errors = do
          (_ :=> Const errs) <- DHashMap.toList errorsMap
          errs
    pure (result, errors)
  where
    handleEx m =
      m `catch` \e -> do
        Text.hPutStrLn stderr $ Text.pack ("exception! " <> show (e :: SomeException))
        error $ show e