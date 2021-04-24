{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.CanonicalM where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import           Error.Warning
import qualified Data.Set                   as S


data CanonicalState = CanonicalState { warnings :: [CompilationWarning], namesAccessed :: S.Set String, accumulatedJS :: String }

type CanonicalM a = forall m . (MonadError CompilationError m, MonadState CanonicalState m) => m a

pushWarning :: CompilationWarning -> CanonicalM ()
pushWarning warning = do
  s <- get
  put s { warnings = warnings s <> [warning] }

pushJS :: String -> CanonicalM ()
pushJS js = do
  s <- get
  put s { accumulatedJS = accumulatedJS s <> js }

getJS :: CanonicalM String
getJS = gets accumulatedJS

resetJS :: CanonicalM ()
resetJS = do
  s <- get
  put s { accumulatedJS = "" }

pushNameAccess :: String -> CanonicalM ()
pushNameAccess name = do
  s <- get
  put s { namesAccessed = namesAccessed s <> S.singleton name }

resetNameAccesses :: CanonicalM ()
resetNameAccesses = do
  s <- get
  put s { namesAccessed = S.empty }

getAllNameAccesses :: CanonicalM (S.Set String)
getAllNameAccesses = gets namesAccessed
