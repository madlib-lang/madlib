module Infer.Env where

import qualified Data.Set               as Set
import qualified Data.Map               as Map
import           Infer.Type
import           Error.Backtrace

data Interface = Interface [TVar] [Pred] [Instance] deriving(Eq, Show)

data Instance = Instance (Qual Pred) Vars deriving(Eq, Show)


type Vars = Map.Map String Scheme
type Interfaces = Map.Map Id Interface
type Methods = Map.Map String Scheme
type TypeDecls = Map.Map String Type


data ImportType
  = NamespaceImport
  | TypeImport
  | NameImport
  deriving(Eq, Show)


data ImportInfo
  = ImportInfo
    { iiModulePath :: FilePath
    , iiType :: ImportType
    , iiName :: String
    }
    deriving(Eq, Show)

data Env
  = Env
    { envVars         :: Vars
    , envInterfaces   :: Interfaces
    , envConstructors :: Set.Set String
    , envMethods      :: Methods
    , envCurrentPath  :: FilePath
    , envBacktrace    :: Backtrace
    -- TODO: remove and use envImportInfo instead
    , envNamespacesInScope :: Set.Set String
    , envImportInfo :: [ImportInfo]
    }
    deriving(Eq, Show)
