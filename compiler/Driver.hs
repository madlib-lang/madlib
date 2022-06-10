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




data State err = State
  { _startedVar :: !(IORef (DHashMap Query MemoEntry))
  , _hashesVar :: !(IORef (DHashMap Query (Const Int)))
  , _reverseDependenciesVar :: !(IORef (ReverseDependencies Query))
  , _tracesVar :: !(IORef (Traces Query (Const Int)))
  , _errorsVar :: !(IORef (DHashMap Query (Const [err])))
  }


initialState :: IO (State err)
initialState = do
  startedVar <- newIORef mempty
  hashesVar <- newIORef mempty
  reverseDependenciesVar <- newIORef mempty
  tracesVar <- newIORef mempty
  errorsVar <- newIORef mempty
  return
    State
      { _startedVar = startedVar
      , _hashesVar = hashesVar
      , _reverseDependenciesVar = reverseDependenciesVar
      , _tracesVar = tracesVar
      , _errorsVar = errorsVar
      }

data Prune
  = Don'tPrune
  | Prune

-- runIncrementalTask ::
--   State err ->
--   HashSet FilePath ->
--   HashSet FilePath ->
--   HashMap FilePath String ->
--   (CompilationError -> Task Query err) ->
--   Prune ->
--   Task Query a ->
--   IO (a, [err])
-- runIncrementalTask state changedFiles sourceDirectories files prettyError prune task =
runIncrementalTask ::
  State CompilationError ->
  Set.Set FilePath ->
  Prune ->
  Task Query a ->
  IO (a, [CompilationError])
runIncrementalTask state changedFiles prune task =
  handleEx $ do
    do
      reverseDependencies <- readIORef $ _reverseDependenciesVar state
      started <- readIORef $ _startedVar state
      hashes <- readIORef $ _hashesVar state

      -- TODO: Add query to read all files
      case DHashMap.lookup (Query.ModulePathsToBuild "") started of
        Just (Done inputFiles) -> do
          -- TODO find a nicer way to do this
          let builtinFile = Path.computeLLVMTargetPath "" "" "builtin/Builtin.vix"
          -- if inputFiles /= files then do
          --   atomicWriteIORef (_reverseDependenciesVar state) mempty
          --   atomicWriteIORef (_startedVar state) mempty
          --   atomicWriteIORef (_hashesVar state) mempty
          -- else do
          do
            changedFiles' <- flip filterM (Set.toList changedFiles) $ \file ->
              pure $ case DHashMap.lookup (Query.File file) started of
                -- Just (Done text) ->
                --   Just text /= HashMap.lookup file files
                _ ->
                  True
            -- Text.hPutStrLn stderr $ "Driver changed files " <> show changedFiles'
            let (keysToInvalidate, reverseDependencies') =
                  List.foldl'
                    ( \(keysToInvalidate_, reverseDependencies_) file ->
                        first (<> keysToInvalidate_) $ reachableReverseDependencies (Query.File file) reverseDependencies_
                    )
                    (mempty, reverseDependencies)
                    changedFiles'
            let started' = DHashMap.difference started keysToInvalidate

                hashes' = DHashMap.difference hashes keysToInvalidate

            atomicWriteIORef (_startedVar state) started'
            atomicWriteIORef (_hashesVar state) hashes'
            atomicWriteIORef (_reverseDependenciesVar state) reverseDependencies'

        _ -> do
          atomicWriteIORef (_reverseDependenciesVar state) mempty
          atomicWriteIORef (_startedVar state) mempty
          atomicWriteIORef (_hashesVar state) mempty

    threadDepsVar <- newIORef mempty
    let readSourceFile_ file
          -- | Just text <- HashMap.lookup file files =
          --   return text
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
        writeErrors :: Writer TaskKind Query a -> ([CompilationWarning], [CompilationError]) -> Task Query ()
        writeErrors (Writer key) (_, errs) = do
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
                $ traceFetch_ $
                  -- writer writeErrors $
                  writer (\_ _ -> pure ()) $
                    Rules.rules Options{}

    -- result <- Rock.runMemoisedTask (_startedVar state) rules task
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
