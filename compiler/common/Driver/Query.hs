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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as BSU
import           Infer.Type
import           Generate.LLVM.SymbolTable
import qualified Generate.LLVM.Env          as LLVM
import           Parse.DocString.DocString
import qualified LLVM.AST                        as AST hiding (function)
import           Infer.MonomorphizationState


data Query a where
  ModulePathsToBuild :: FilePath -> Query [FilePath]
  AbsolutePreludePath :: FilePath -> Query FilePath
  DictionaryModuleAbsolutePath :: Query FilePath

  -- Parsing
  DetectImportCycle :: [FilePath] -> FilePath -> Query Bool
  File :: FilePath -> Query String
  ParsedAST :: FilePath -> Query Src.AST

  -- Documentation
  DocStrings :: FilePath -> Query [DocString]

  -- Canonicalization
  CanonicalizedASTWithEnv :: FilePath -> Query (Can.AST, CanEnv.Env, [InstanceToDerive])
  CanonicalizedInterface :: FilePath -> String -> Query (Maybe CanEnv.Interface)
  ForeignADTType :: FilePath -> String -> Query (Maybe Type)
  ForeignConstructorInfos :: FilePath -> String -> Query (Maybe [CanEnv.ConstructorInfo])

  -- Type checking
  SolvedASTWithEnv :: FilePath -> Query (Slv.AST, SlvEnv.Env)
  AllSolvedASTsWithEnvs :: Query (Map.Map FilePath (Slv.AST, SlvEnv.Env))
  SolvedInterface :: FilePath -> String -> Query (Maybe SlvEnv.Interface)
  ForeignScheme :: FilePath -> String -> Query (Maybe Scheme)
  ForeignExp :: FilePath -> String -> Query (Maybe Slv.Exp)
  ForeignMethod :: FilePath -> String -> Type -> Query (Maybe Slv.Exp)
  SolvedMethodNode :: String -> Type -> Query (Maybe (Slv.Exp, FilePath))
  DefinesInterfaceForMethod :: FilePath -> String -> Query Bool
  ForeignConstructor :: FilePath -> String -> Query (Maybe Slv.Constructor)
  ForeignTypeDeclaration :: FilePath -> String -> Query (Maybe Slv.TypeDecl)

  -- Monomorphization
  MonomorphizedProgram :: Query
    ( Map.Map FunctionId MonomorphizationRequest
    , Map.Map FilePath (Map.Map FilePath (Set.Set (String, Type, ImportType)))
    , Set.Set String
    )
  MonomorphizedAST :: FilePath -> Query Slv.AST

  -- Core
  CoreAST :: FilePath -> Query Core.AST

  -- LLVM
  BuiltObjectFile :: FilePath -> Query (SymbolTable, LLVM.Env, ByteString.ByteString)

  -- JS
  GeneratedJSModule :: FilePath -> Query String
  BuiltJSModule :: FilePath -> Query String

  BuiltTarget :: FilePath -> Query ()

  -- Misc
  StaticLibPathsToLink :: FilePath -> Query [FilePath]
  EnvVar :: String -> Query (Maybe String)

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query
deriveArgDict ''Query

instance Hashable (Query a) where
  hashWithSalt salt query = case query of
    ModulePathsToBuild path ->
      hashWithSalt salt (path, 0 :: Int)

    AbsolutePreludePath moduleName ->
      hashWithSalt salt (moduleName, 1 :: Int)

    DictionaryModuleAbsolutePath ->
      hashWithSalt salt (2 :: Int)

    DetectImportCycle importChain path ->
      hashWithSalt salt (importChain, path, 3 :: Int)

    File path ->
      hashWithSalt salt (path, 4 :: Int)

    ParsedAST path ->
      hashWithSalt salt (path, 5 :: Int)

    DocStrings path ->
      hashWithSalt salt (path, 6 :: Int)

    CanonicalizedASTWithEnv path ->
      hashWithSalt salt (path, 7 :: Int)

    CanonicalizedInterface path name ->
      hashWithSalt salt (path, name, 8 :: Int)

    ForeignADTType modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 9 :: Int)

    ForeignConstructorInfos modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 10 :: Int)

    SolvedASTWithEnv path ->
      hashWithSalt salt (path, 11 :: Int)

    AllSolvedASTsWithEnvs ->
      hashWithSalt salt (101 :: Int)

    SolvedInterface path name ->
      hashWithSalt salt (path, name, 12 :: Int)

    ForeignScheme modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 13 :: Int)

    ForeignExp modulePath expName ->
      hashWithSalt salt (modulePath <> "." <> expName, 14 :: Int)

    ForeignMethod modulePath methodName methodType ->
      hashWithSalt salt (modulePath, methodName, methodType, 15 :: Int)

    SolvedMethodNode methodName methodType ->
      hashWithSalt salt (methodName, methodType, 16 :: Int)

    DefinesInterfaceForMethod modulePath methodName ->
      hashWithSalt salt (modulePath, methodName, 17 :: Int)

    ForeignConstructor modulePath constructorName ->
      hashWithSalt salt (modulePath <> "." <> constructorName, 18 :: Int)

    ForeignTypeDeclaration modulePath typeName ->
      hashWithSalt salt (modulePath <> "." <> typeName, 19 :: Int)

    MonomorphizedProgram ->
      hashWithSalt salt (21 :: Int)

    MonomorphizedAST path ->
      hashWithSalt salt (path, 22 :: Int)

    CoreAST path ->
      hashWithSalt salt (path, 23 :: Int)

    BuiltObjectFile path ->
      hashWithSalt salt (path, 24 :: Int)

    GeneratedJSModule path ->
      hashWithSalt salt (path, 25 :: Int)

    BuiltJSModule path ->
      hashWithSalt salt (path, 26 :: Int)

    BuiltTarget path ->
      hashWithSalt salt (path, 27 :: Int)

    StaticLibPathsToLink path ->
      hashWithSalt salt (path, 28 :: Int)

    EnvVar name ->
      hashWithSalt salt (name, 29 :: Int)


instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query
