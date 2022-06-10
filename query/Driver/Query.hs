{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Driver.Query where

import qualified Rock
import qualified AST.Source                 as Src
import qualified AST.Canonical              as Can
import qualified Canonicalize.Env           as CanEnv
import qualified AST.Solved                 as Slv
import qualified Infer.Env                  as SlvEnv
import qualified AST.Core                   as Core
-- import           Parse.Madlib.AST
import           Error.Error (CompilationError(CompilationError))
import           Data.GADT.Compare.TH (deriveGEq)
import           Data.Some
import           Data.Hashable
import           Infer.Type
import           Generate.LLVM.SymbolTable
import qualified Generate.LLVM.Env          as LLVM

data Query a where
  -- Parsing
  File :: FilePath -> Query String
  ParsedAST :: FilePath -> Query Src.AST

  -- Canonicalization
  CanonicalizedASTWithEnv :: FilePath -> Query (Can.AST, CanEnv.Env)
  CanonicalizedInterface :: FilePath -> String -> Query CanEnv.Interface
  ForeignADTType :: FilePath -> String -> Query (Maybe Type)

  -- Type checking
  SolvedASTWithEnv :: FilePath -> Query (Slv.AST, SlvEnv.Env)
  SolvedTable :: [FilePath] -> Query Slv.Table
  SolvedInterface :: FilePath -> String -> Query SlvEnv.Interface
  ForeignScheme :: FilePath -> String -> Query (Maybe Scheme)

  -- Core
  CoreAST :: FilePath -> Query Core.AST

  -- LLVM
  SymbolTableWithEnv :: FilePath -> Query (SymbolTable, LLVM.Env)
  BuiltInSymbolTableWithEnv :: Query (SymbolTable, LLVM.Env)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt query = case query of
    File path ->
      hashWithSalt salt (path, 0 :: Int)

    ParsedAST path ->
      hashWithSalt salt (path, 1 :: Int)

    CanonicalizedASTWithEnv path ->
      hashWithSalt salt (path, 2 :: Int)

    CanonicalizedInterface _ name ->
      hashWithSalt salt (name, 3 :: Int)

    ForeignADTType modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 4 :: Int)

    SolvedASTWithEnv path ->
      hashWithSalt salt (path, 5 :: Int)

    SolvedTable modulePaths ->
      hashWithSalt salt (show modulePaths, 6 :: Int)

    SolvedInterface _ name ->
      hashWithSalt salt (name, 7 :: Int)

    ForeignScheme modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 8 :: Int)

    CoreAST path ->
      hashWithSalt salt (path, 9 :: Int)

    SymbolTableWithEnv path ->
      hashWithSalt salt (path, 10 :: Int)

    BuiltInSymbolTableWithEnv ->
      hashWithSalt salt ("BuiltInSymbolTableWithEnv", 11 :: Int)


instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query
