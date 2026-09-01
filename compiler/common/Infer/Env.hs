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
    -- ^ Exact cached union of free type variables across envVars. This is
    -- maintained together with envOpenVarNames; use the constructors in
    -- Infer.EnvUtils instead of updating envVars directly.
    , envOpenVarNames :: !(Set.Set Id)
    -- ^ Names of `envVars` entries whose schemes have non-empty free type
    -- variables (i.e., parameters or pre-generalization let-bindings).
    -- The Substitutable Env apply walks only these on the slow path,
    -- skipping the typically-large set of closed (TGen) schemes from
    -- imports and post-generalization bindings. Maintained by
    -- setVars / extendVars / mergeVars.
    , envBuiltinsModulePath :: FilePath
    -- ^ Absolute path to the builtins module. Used for runtime Type values
    -- synthesized by `typeof` so they share the same origin as the source
    -- level `Type` declaration.
    }
    deriving(Eq, Show, Generic, Hashable)
