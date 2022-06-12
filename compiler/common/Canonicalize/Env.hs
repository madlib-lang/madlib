{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Canonicalize.Env where

import           Infer.Type
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Data.Hashable
import           GHC.Generics hiding(Constructor)



data Interface
  = Interface [TVar] [Pred]
  deriving(Eq, Show, Generic, Hashable)

type TypeDecls = M.Map String Type
type Interfaces = M.Map String Interface

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
    { envImportInfo  :: [ImportInfo]
    , envTypeDecls   :: TypeDecls
    , envInterfaces  :: Interfaces
    , envCurrentPath :: FilePath
    , envFromDictionaryListName :: String
    }
    deriving(Eq, Show, Generic, Hashable)


initialEnv :: Env
initialEnv = Env { envTypeDecls = M.fromList [("List", tList), ("Dictionary", tDictionary), ("Array", tArray), ("ByteArray", tByteArray)]
                 , envInterfaces = M.fromList [("Eq", Interface [TV "a" Star] []), ("Inspect", Interface [TV "a" Star] [])]
                 , envCurrentPath = ""
                 , envFromDictionaryListName = ""
                 , envImportInfo = []
                 }

initialWithPath :: FilePath -> Env
initialWithPath astPath = initialEnv { envCurrentPath = astPath }
