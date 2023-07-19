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
import qualified AST.Canonical                 as Can



data Interface
  = Interface [TVar] [Pred] [String]
  deriving(Eq, Show, Generic, Hashable)

data ConstructorInfo
  = ConstructorInfo String Int
  deriving(Eq, Show, Generic, Hashable)

type TypeDecls = M.Map String Type
type ConstructorInfos = M.Map String [ConstructorInfo]
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


isTypeImport :: ImportInfo -> Bool
isTypeImport ii = case iiType ii of
  TypeImport ->
    True

  _ ->
    False


data Env
  = Env
    { envImportInfo :: [ImportInfo]
    , envTypeDecls :: TypeDecls
    , envConstructorInfos :: ConstructorInfos
    , envInterfaces :: Interfaces
    , envCurrentPath :: FilePath
    , envFromDictionaryListName :: String
    , envIsMainModule :: Bool
    , envExpPosition :: Int
    -- ^ used for unused var checks, if we reassign from a later place then it's fine
    }
    deriving(Eq, Show, Generic, Hashable)


initialEnv :: Env
initialEnv = Env { envTypeDecls = M.fromList [("List", tList), ("Dictionary", tDictionary), ("Array", tArray), ("ByteArray", tByteArray)]
                 , envConstructorInfos = M.empty
                 , envInterfaces =
                    M.fromList
                      [ ("Eq", Interface [TV "a" Star] [] ["=="])
                      , ("Comparable", Interface [TV "a" Star] [IsIn "Eq" [TVar $ TV "a" Star] Nothing] ["compare"])
                      , ("Show", Interface [TV "a" Star] [] ["show"])
                      ]
                 , envCurrentPath = ""
                 , envFromDictionaryListName = ""
                 , envImportInfo = []
                 , envIsMainModule = False
                 , envExpPosition = 0
                 }
