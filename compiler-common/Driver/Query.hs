{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Driver.Query where

import qualified Rock
import qualified AST.Source                 as Src
import qualified AST.Canonical              as Can
import qualified Canonicalize.Env           as CanEnv
import           Canonicalize.InstanceToDerive
import qualified AST.Solved                 as Slv
import qualified Infer.Env                  as SlvEnv
import qualified AST.Core                   as Core
import           Error.Error (CompilationError(CompilationError))
import           Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import           Data.GADT.Show.TH (deriveGShow)
import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.Some
import           Data.Hashable
import           Infer.Type
import           Generate.LLVM.SymbolTable
import qualified Generate.LLVM.Env          as LLVM
import           Parse.DocString.DocString

data Query a where
  ModulePathsToBuild :: FilePath -> Query [FilePath]
  DictionaryModuleAbsolutePath :: Query FilePath

  -- Parsing
  DetectImportCycle :: FilePath -> Query Bool
  File :: FilePath -> Query String
  ParsedAST :: FilePath -> Query Src.AST

  -- Documentation
  DocStrings :: FilePath -> Query [DocString]

  -- Canonicalization
  CanonicalizedASTWithEnv :: FilePath -> Query (Can.AST, CanEnv.Env, [InstanceToDerive])
  CanonicalizedInterface :: FilePath -> String -> Query (Maybe CanEnv.Interface)
  ForeignADTType :: FilePath -> String -> Query (Maybe Type)

  -- Type checking
  SolvedASTWithEnv :: FilePath -> Query (Slv.AST, SlvEnv.Env)
  SolvedInterface :: FilePath -> String -> Query (Maybe SlvEnv.Interface)
  ForeignScheme :: FilePath -> String -> Query (Maybe Scheme)

  -- Core
  CoreAST :: FilePath -> Query Core.AST

  -- LLVM
  SymbolTableWithEnv :: FilePath -> Query (SymbolTable, LLVM.Env)
  BuiltInSymbolTableWithEnv :: Query (SymbolTable, LLVM.Env)

  -- JS
  BuiltJSModule :: FilePath -> Query ()

  BuiltTarget :: FilePath -> Query ()

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query
deriveArgDict ''Query

instance Hashable (Query a) where
  hashWithSalt salt query = case query of
    ModulePathsToBuild path ->
      hashWithSalt salt (path, 0 :: Int)

    DictionaryModuleAbsolutePath ->
      hashWithSalt salt ("DictionaryModuleAbsolutePath", 1 :: Int)


    DetectImportCycle path ->
      hashWithSalt salt (path, 2 :: Int)

    File path ->
      hashWithSalt salt (path, 3 :: Int)

    ParsedAST path ->
      hashWithSalt salt (path, 4 :: Int)

    DocStrings path ->
      hashWithSalt salt (path, 5 :: Int)

    CanonicalizedASTWithEnv path ->
      hashWithSalt salt (path, 6 :: Int)

    CanonicalizedInterface _ name ->
      hashWithSalt salt (name, 7 :: Int)

    ForeignADTType modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 8 :: Int)

    SolvedASTWithEnv path ->
      hashWithSalt salt (path, 9 :: Int)

    SolvedInterface _ name ->
      hashWithSalt salt (name, 10 :: Int)

    ForeignScheme modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 11 :: Int)

    CoreAST path ->
      hashWithSalt salt (path, 12 :: Int)

    SymbolTableWithEnv path ->
      hashWithSalt salt (path, 13 :: Int)

    BuiltInSymbolTableWithEnv ->
      hashWithSalt salt ("BuiltInSymbolTableWithEnv", 14 :: Int)

    BuiltJSModule path ->
      hashWithSalt salt (path, 15 :: Int)

    BuiltTarget path ->
      hashWithSalt salt (path, 16 :: Int)


instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query
