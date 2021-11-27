{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.CanonicalM where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import           Error.Warning
import qualified Data.Set                      as Set
import           AST.Canonical
import Infer.Type


data Accessed
  = NameAccessed String
  | TypeAccessed String
  deriving(Eq, Show, Ord)


-- List of typedecl found in the AST for which we need to derive
-- an instance of Eq
newtype ToDerive
  = TypeDeclToDerive TypeDecl
  deriving(Eq, Show, Ord)

-- List of types for which an instance of Eq has been defined.
-- This will mostly be useful for records since for a record such as:
-- { x :: Integer, y :: Integer } we would generate the general Eq instance:
-- instance (Eq a, Eq b) => Eq { x :: a, y :: b } { ... }
-- so that it can work for all variations of that record, but also we should
-- not redefine that instance later on if that record is present in another module.
newtype Derived
  = Derived Type

data CanonicalState
  = CanonicalState
      { warnings :: [CompilationWarning]
      , namesAccessed :: Set.Set Accessed
      , accumulatedJS :: String
      , typesToDerive :: Set.Set ToDerive
      , derivedTypes :: Set.Set Derived
      }

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
  put s { namesAccessed = namesAccessed s <> Set.singleton (NameAccessed name) }

pushTypeAccess :: String -> CanonicalM ()
pushTypeAccess name = do
  s <- get
  put s { namesAccessed = namesAccessed s <> Set.singleton (TypeAccessed name) }

resetNameAccesses :: CanonicalM ()
resetNameAccesses = do
  s <- get
  put s { namesAccessed = Set.empty }

getAllAccesses :: CanonicalM (Set.Set Accessed)
getAllAccesses = gets namesAccessed

getAllNameAccesses :: CanonicalM (Set.Set String)
getAllNameAccesses = gets (Set.map getAccessName . Set.filter isNameAccess . namesAccessed)

getAllTypeAccesses :: CanonicalM (Set.Set String)
getAllTypeAccesses = gets (Set.map getAccessName . Set.filter isTypeAccess . namesAccessed)


pushTypeDeclToDerive :: TypeDecl -> CanonicalM ()
pushTypeDeclToDerive td = do
  s <- get
  put s { typesToDerive = typesToDerive s <> Set.singleton (TypeDeclToDerive td) }

getTypeDeclarationsToDerive :: CanonicalM (Set.Set ToDerive)
getTypeDeclarationsToDerive =
  gets typesToDerive

resetToDerive :: CanonicalM ()
resetToDerive = do
  s <- get
  put s { typesToDerive = Set.empty }
