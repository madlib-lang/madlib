{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.CanonicalM where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import           Error.Warning
import qualified Data.Set                      as S


data Accessed
  = NameAccessed String
  | TypeAccessed String
  deriving(Eq, Show, Ord)

data CanonicalState = CanonicalState { warnings :: [CompilationWarning], namesAccessed :: S.Set Accessed, accumulatedJS :: String }

type CanonicalM a = forall m . (MonadError CompilationError m, MonadState CanonicalState m) => m a

isNameAccess :: Accessed -> Bool
isNameAccess a = case a of
  NameAccessed _ -> True
  TypeAccessed _ -> False

isTypeAccess :: Accessed -> Bool
isTypeAccess a = case a of
  NameAccessed _ -> False
  TypeAccessed _ -> True

getAccessName :: Accessed -> String
getAccessName a = case a of
  NameAccessed n -> n
  TypeAccessed n -> n

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
  put s { namesAccessed = namesAccessed s <> S.singleton (NameAccessed name) }

pushTypeAccess :: String -> CanonicalM ()
pushTypeAccess name = do
  s <- get
  put s { namesAccessed = namesAccessed s <> S.singleton (TypeAccessed name) }

resetNameAccesses :: CanonicalM ()
resetNameAccesses = do
  s <- get
  put s { namesAccessed = S.empty }

getAllAccesses :: CanonicalM (S.Set Accessed)
getAllAccesses = gets namesAccessed

getAllNameAccesses :: CanonicalM (S.Set String)
getAllNameAccesses = gets (S.map getAccessName . S.filter isNameAccess . namesAccessed)

getAllTypeAccesses :: CanonicalM (S.Set String)
getAllTypeAccesses = gets (S.map getAccessName . S.filter isTypeAccess . namesAccessed)
