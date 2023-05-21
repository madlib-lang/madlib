{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Canonicalize.CanonicalM where

import           Control.Monad.Except
import           Control.Monad.State
import           Error.Error
import           Error.Warning
import qualified Data.Set                      as Set
import           AST.Canonical
import           Canonicalize.InstanceToDerive
import           Canonicalize.Coverable
import qualified Data.List as List
import qualified Driver.Query as Query
import qualified Rock


data Accessed
  = NameAccessed String
  | TypeAccessed String
  deriving(Eq, Show, Ord)

data Declared
  = Declared Int String
  deriving(Eq, Show, Ord)



data CanonicalState
  = CanonicalState
      { warnings :: [CompilationWarning]
      , namesAccessed :: Set.Set Accessed
      , namesDeclared :: Set.Set Declared
      , accumulatedJS :: String
      , typesToDerive :: [InstanceToDerive]
      -- List of InstanceToDerive for which an instance of Eq has been defined.
      -- This will mostly be useful for records since for a record such as:
      -- { x :: Integer, y :: Integer } we would generate the general Eq instance:
      -- instance (Eq a, Eq b) => Eq { x :: a, y :: b } { ... }
      -- so that it can work for all variations of that record, but also we should
      -- not redefine that instance later on if that record is present in another module.
      , derivedTypes :: Set.Set InstanceToDerive
      , placeholderIndex :: Int
      , parameterIndex :: Int
      , coverableInfo :: [Coverable]
      , linesTracked :: [Int]
      , anonymousFunctionIndex :: Int
      , nextBlockIndex :: Int
      }

type CanonicalM a = forall m . (MonadIO m, Rock.MonadFetch Query.Query m, MonadError CompilationError m, MonadState CanonicalState m) => m a

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

newBlock :: CanonicalM Int
newBlock = do
  s <- get
  let index = nextBlockIndex s
  put s { nextBlockIndex = index + 1 }
  return index


generateAnonymousFunctionName :: CanonicalM String
generateAnonymousFunctionName = do
  s <- get
  let index = anonymousFunctionIndex s
  put s { anonymousFunctionIndex = index + 1 }
  return $ "Anonymous__" <> show index

pushCoverable :: Coverable -> CanonicalM ()
pushCoverable cov = do
  s <- get
  let linesTracked' =
        case cov of
          Line { cline } ->
            cline : linesTracked s

          _ ->
            linesTracked s
  put s { coverableInfo = cov : coverableInfo s, linesTracked = linesTracked' }

isLineTracked :: Int -> CanonicalM Bool
isLineTracked line = do
  tracked <- gets linesTracked
  return $ line `elem` tracked

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

pushNameDeclaration :: Int -> String -> CanonicalM ()
pushNameDeclaration position name = do
  s <- get
  put s { namesDeclared = namesDeclared s <> Set.singleton (Declared position name) }

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

resetNamesDeclared :: CanonicalM ()
resetNamesDeclared = do
  s <- get
  put s { namesDeclared = Set.empty }

setDeclaredNames :: Set.Set Declared -> CanonicalM ()
setDeclaredNames declared = do
  s <- get
  put s { namesDeclared = declared }

setAccesses :: Set.Set Accessed -> CanonicalM ()
setAccesses accesses = do
  s <- get
  put s { namesAccessed = accesses }

getAllDeclaredNames :: CanonicalM (Set.Set Declared)
getAllDeclaredNames = gets namesDeclared

getAllAccesses :: CanonicalM (Set.Set Accessed)
getAllAccesses = gets namesAccessed

getAllNameAccesses :: CanonicalM (Set.Set String)
getAllNameAccesses = gets (Set.map getAccessName . Set.filter isNameAccess . namesAccessed)

getAllTypeAccesses :: CanonicalM (Set.Set String)
getAllTypeAccesses = gets (Set.map getAccessName . Set.filter isTypeAccess . namesAccessed)


pushTypeDeclToDerive :: TypeDecl -> CanonicalM ()
pushTypeDeclToDerive td = do
  s <- get
  put s { typesToDerive = typesToDerive s <> [TypeDeclToDerive td] }

pushRecordToDerive :: [String] -> CanonicalM ()
pushRecordToDerive fieldNames = do
  s <- get
  put s { typesToDerive = typesToDerive s <> [RecordToDerive (Set.fromList fieldNames)] }

getTypeDeclarationsToDerive :: CanonicalM [InstanceToDerive]
getTypeDeclarationsToDerive =
  gets typesToDerive

resetToDerive :: CanonicalM ()
resetToDerive = do
  s <- get
  put s { typesToDerive = [] }

addDerivedTypes :: Set.Set InstanceToDerive -> CanonicalM ()
addDerivedTypes derived = do
  s <- get
  put s { derivedTypes = derivedTypes s <> derived }

getDerivedTypes :: CanonicalM (Set.Set InstanceToDerive)
getDerivedTypes =
  gets derivedTypes


generatePlaceholderIndex :: CanonicalM Int
generatePlaceholderIndex = do
  s <- get
  let index = placeholderIndex s + 1
  put s { placeholderIndex = index }
  return index

generateParameterIndex :: CanonicalM Int
generateParameterIndex = do
  s <- get
  let index = parameterIndex s + 1
  put s { parameterIndex = index }
  return index
