{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Infer.Env where

import qualified Data.Set               as Set
import qualified Data.Map               as Map
import           Infer.Type
import           Data.Hashable
import           GHC.Generics hiding(Constructor)

data Interface
  = Interface [TVar] [Pred] [Instance]
  deriving(Eq, Show, Generic, Hashable)

data Instance
  = Instance (Qual Pred) Vars
  deriving(Eq, Show, Generic, Hashable)


type Vars = Map.Map String Scheme
type Interfaces = Map.Map Id Interface
type Methods = Map.Map String Scheme
type TypeDecls = Map.Map String Type


data ImportType
  = NamespaceImport
  | TypeImport
  | NameImport
  deriving(Eq, Show, Generic, Hashable)


data ImportInfo
  = ImportInfo
    { iiModulePath :: FilePath
    , iiType :: ImportType
    , iiName :: String
    }
    deriving(Eq, Show, Generic, Hashable)

data Env
  = Env
    { envVars :: Vars
    , envInterfaces :: Interfaces
    , envConstructors :: Set.Set String
    , envMethods :: Methods
    , envCurrentPath :: FilePath
    , envInBody :: Bool
    , envDeferBodyAmbiguity :: Bool
    , envNamesInScope :: Vars
    -- TODO: remove and use envImportInfo instead
    , envNamespacesInScope :: Set.Set String
    , envImportInfo :: [ImportInfo]
    , envPlaceholdersInScope :: [Pred]
    , envPlaceholdersToDelete :: Map.Map String [Int]
    -- ^ key is the name of the binding and the list of int is the placeholder indices to remove
    , envPatternBoundNames :: Set.Set String
    -- ^ names bound by pattern matching, cannot be mutated with ':='
    , envFreeTVars :: !(Set.Set TVar)
    -- ^ Cached union of free type variables across `envVars`. Maintained
    -- as an over-approximation so the Substitutable Env apply can
    -- short-circuit when the substitution's domain doesn't overlap. When
    -- entries are added, their `ftv` is unioned in; entries that get
    -- removed or refined leave stale entries that are safe (just
    -- prevent the fast path from firing in some cases).
    , envOpenVarNames :: !(Set.Set Id)
    -- ^ Names of `envVars` entries whose schemes have non-empty free type
    -- variables (i.e., parameters or pre-generalization let-bindings).
    -- The Substitutable Env apply walks only these on the slow path,
    -- skipping the typically-large set of closed (TGen) schemes from
    -- imports and post-generalization bindings. Maintained by
    -- `extendVars` / `mergeVars` / etc.
    }
    deriving(Eq, Show, Generic, Hashable)
