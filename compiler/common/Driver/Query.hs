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
import qualified Data.HashMap.Strict as HM
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
  FileBS :: FilePath -> Query ByteString.ByteString
  ParsedAST :: FilePath -> Query Src.AST

  -- Documentation
  DocStrings :: FilePath -> Query [DocString]

  -- Canonicalization
  CanonicalizedASTWithEnv :: FilePath -> Query (Can.AST, CanEnv.Env, [InstanceToDerive])
  CanonicalizedInterface :: FilePath -> String -> Query (Maybe CanEnv.Interface)
  ForeignCanTypeDeclaration :: FilePath -> String -> Query (Maybe Can.TypeDecl)
  ForeignADTType :: FilePath -> String -> Query (Maybe Type)
  ForeignConstructorInfos :: FilePath -> String -> Query (Maybe [CanEnv.ConstructorInfo])

  -- Type checking
  SolvedASTWithEnv :: FilePath -> Query (Slv.AST, SlvEnv.Env)
  AllSolvedASTsWithEnvs :: Query (Map.Map FilePath (Slv.AST, SlvEnv.Env))
  SolvedInterface :: FilePath -> String -> Query (Maybe SlvEnv.Interface)
  ForeignScheme :: FilePath -> String -> Query (Maybe Scheme)
  ForeignFunctionScheme :: FilePath -> String -> Query (Maybe Scheme)
  ForeignExp :: FilePath -> String -> Query (Maybe Slv.Exp)
  ResolvedExp :: FilePath -> String -> Query (Maybe (Slv.Exp, FilePath))
  ForeignMethod :: FilePath -> String -> Type -> Query (Maybe Slv.Exp)
  SolvedMethodNode :: String -> Type -> Query (Maybe (Slv.Exp, FilePath))
  DefinesInterfaceForMethod :: FilePath -> String -> Query Bool
  ForeignConstructor :: FilePath -> String -> Query (Maybe Slv.Constructor)
  ForeignExportedConstructor :: FilePath -> String -> Query (Maybe Slv.Constructor)
  ForeignTypeDeclaration :: FilePath -> String -> Query (Maybe Slv.TypeDecl)

  -- Monomorphization
  MonomorphizedProgram :: Query
    ( HM.HashMap FunctionId MonomorphizationRequest
    , Map.Map FilePath (Map.Map FilePath (Set.Set (String, Type, ImportType)))
    , Set.Set String
    )
  MonomorphizedAST :: FilePath -> Query Slv.AST

  -- Core
  FoldedCoreAST :: FilePath -> Query Core.AST
  CoreAST :: FilePath -> Query Core.AST
  PropagatedAST :: FilePath -> Query Core.AST
  ForeignCoreExp :: FilePath -> String -> Query (Maybe Core.Exp)
  FunctionEscapeSummaries :: FilePath -> Query Core.FunctionSummaries
  InlineCandidates :: FilePath -> Query Core.InlineCandidates

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

    FileBS path ->
      hashWithSalt salt (path, 40 :: Int)

    ParsedAST path ->
      hashWithSalt salt (path, 5 :: Int)

    DocStrings path ->
      hashWithSalt salt (path, 6 :: Int)

    CanonicalizedASTWithEnv path ->
      hashWithSalt salt (path, 7 :: Int)

    CanonicalizedInterface path name ->
      hashWithSalt salt (path, name, 8 :: Int)

    ForeignCanTypeDeclaration path typeName ->
      hashWithSalt salt (path, typeName, 9 :: Int)

    ForeignADTType modulePath typeName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (10 :: Int)) modulePath) typeName

    ForeignConstructorInfos modulePath typeName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (11 :: Int)) modulePath) typeName

    SolvedASTWithEnv path ->
      hashWithSalt salt (path, 12 :: Int)

    AllSolvedASTsWithEnvs ->
      hashWithSalt salt (13 :: Int)

    SolvedInterface path name ->
      hashWithSalt salt (path, name, 14 :: Int)

    ForeignScheme modulePath typeName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (15 :: Int)) modulePath) typeName

    ForeignFunctionScheme modulePath typeName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (16 :: Int)) modulePath) typeName

    ForeignExp modulePath expName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (17 :: Int)) modulePath) expName

    ResolvedExp modulePath expName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (38 :: Int)) modulePath) expName

    ForeignMethod modulePath methodName methodType ->
      hashWithSalt salt (modulePath, methodName, methodType, 18 :: Int)

    SolvedMethodNode methodName methodType ->
      hashWithSalt salt (methodName, methodType, 19 :: Int)

    DefinesInterfaceForMethod modulePath methodName ->
      hashWithSalt salt (modulePath, methodName, 20 :: Int)

    ForeignConstructor modulePath constructorName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (21 :: Int)) modulePath) constructorName

    ForeignExportedConstructor modulePath constructorName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (22 :: Int)) modulePath) constructorName

    ForeignTypeDeclaration modulePath typeName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (23 :: Int)) modulePath) typeName

    MonomorphizedProgram ->
      hashWithSalt salt (24 :: Int)

    MonomorphizedAST path ->
      hashWithSalt salt (path, 25 :: Int)

    FoldedCoreAST path ->
      hashWithSalt salt (path, 36 :: Int)

    CoreAST path ->
      hashWithSalt salt (path, 26 :: Int)

    PropagatedAST path ->
      hashWithSalt salt (path, 27 :: Int)

    ForeignCoreExp modulePath expName ->
      hashWithSalt (hashWithSalt (hashWithSalt salt (28 :: Int)) modulePath) expName

    FunctionEscapeSummaries path ->
      hashWithSalt salt (path, 35 :: Int)

    InlineCandidates path ->
      hashWithSalt salt (path, 37 :: Int)

    BuiltObjectFile path ->
      hashWithSalt salt (path, 29 :: Int)

    GeneratedJSModule path ->
      hashWithSalt salt (path, 30 :: Int)

    BuiltJSModule path ->
      hashWithSalt salt (path, 31 :: Int)

    BuiltTarget path ->
      hashWithSalt salt (path, 32 :: Int)

    StaticLibPathsToLink path ->
      hashWithSalt salt (path, 33 :: Int)

    EnvVar name ->
      hashWithSalt salt (name, 34 :: Int)


instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query
