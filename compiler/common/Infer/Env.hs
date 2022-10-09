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
    -- TODO: remove and use envImportInfo instead
    , envNamespacesInScope :: Set.Set String
    , envImportInfo :: [ImportInfo]
    , envPlaceholdersInScope :: [Pred]
    , envPlaceholdersToDelete :: Map.Map String [Int]
    -- ^ key is the name of the binding and the list of int is the placeholder indices to remove
    }
    deriving(Eq, Show, Generic, Hashable)
