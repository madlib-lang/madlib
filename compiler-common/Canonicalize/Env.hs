{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.Env where

import           Infer.Type
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import qualified Data.Map                      as M



data Interface = Interface [TVar] [Pred] deriving(Eq, Show)

type TypeDecls = M.Map String Type
type Interfaces = M.Map String Interface

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
      { envImportInfo  :: [ImportInfo]
      , envTypeDecls   :: TypeDecls
      , envInterfaces  :: Interfaces
      , envCurrentPath :: FilePath
      , envFromDictionaryListName :: String
      }
      deriving(Eq, Show)





-- lookupADT :: Env -> String -> CanonicalM Type
-- lookupADT env name = case M.lookup name (envTypeDecls env) of
--   Just found ->
--     return found

--   Nothing    ->
--     throwError $ CompilationError (UnknownType name) NoContext



initialEnv :: Env
initialEnv = Env { envTypeDecls = M.fromList [("List", tList), ("Dictionary", tDictionary), ("Array", tArray), ("ByteArray", tByteArray)]
                 , envInterfaces = M.fromList [("Eq", Interface [TV "a" Star] []), ("Inspect", Interface [TV "a" Star] [])]
                 , envCurrentPath = ""
                 , envFromDictionaryListName = ""
                 , envImportInfo = []
                 }

initialWithPath :: FilePath -> Env
initialWithPath astPath = initialEnv { envCurrentPath = astPath }
