{-# LANGUAGE NamedFieldPuns #-}
module Run.Package where

import           System.Directory              ( canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory )
import           System.FilePath               ( takeDirectory, joinPath )
import           Text.Show.Pretty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLChar8
import qualified Data.List                     as List
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder
import           Data.Version


import           MadlibDotJson.MadlibDotJson
import qualified MadlibDotJson.MadlibVersion        as MadlibVersion
import           VersionLock.VersionLock
import           Error.Error
import           Error.Warning
import qualified AST.Canonical                      as Can
import qualified Canonicalize.AST                   as Can
import qualified Canonicalize.Env                   as Can
import qualified AST.Solved                         as Slv
import qualified Infer.AST                          as Slv
import           Infer.Type
import           Infer.Instantiate
import qualified AST.Source                         as Src
import qualified Parse.Madlib.AST                   as Src
import           Run.Target
import           Run.PackageHash
import           Run.CommandLine
import           Utils.Hash
import           Utils.Version
import           Utils.Tuple
import           Explain.Format

parse :: FilePath -> IO (Either CompilationError Src.Table)
parse = Src.buildASTTable mempty

canonicalize :: Src.Table -> FilePath -> (Either CompilationError Can.Table, [CompilationWarning])
canonicalize srcTable main =
  let (result, warnings) = Can.runCanonicalization mempty TAny Can.initialEnv srcTable main
  in  (fst <$> result, warnings)

typeCheck :: Can.Table -> FilePath -> (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheck canTable path = Slv.solveManyASTs' canTable [path]


typeCheckMain :: FilePath -> IO (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheckMain main = do
  parsed        <- parse main

  case parsed of
    Right srcTable -> case canonicalize srcTable main of
      (Left err, warnings)       -> return (Left [err], warnings)

      (Right canTable, warnings) -> return $ typeCheck canTable main

    Left err    -> return (Left [err], [])


data PublicAPI = PublicAPI
  { apiNames      :: Map.Map String String
  , apiInterfaces :: Map.Map String (Map.Map String String)
  , apiInstances  :: [String]
  , apiTypes      :: Map.Map String [String]
  , apiAliases    :: Map.Map String String
  }
  deriving(Eq, Show, Ord)

addInterface :: Slv.Interface -> PublicAPI -> PublicAPI
addInterface (Slv.Untyped _ (Slv.Interface name supers vars _ methodTypings)) api =
  let key     = name <> " " <> unwords (getTVarId <$> vars)
      key'   =
        if not (null supers) then
          lst (predsToStr False mempty supers) <> " => " <> key
        else
          key
      methods = prettyPrintConstructorTyping' False <$> methodTypings
  in  api { apiInterfaces = Map.insert key' methods $ apiInterfaces api }

addInstance :: Slv.Instance -> PublicAPI -> PublicAPI
addInstance (Slv.Untyped _ (Slv.Instance _ supers pred _)) api =
  let decl  = lst $ predToStr False mempty pred
      decl' =
        if not (null supers) then
          lst (predsToStr False mempty supers) <> " => " <> decl
        else
          decl
  in  api { apiInstances = apiInstances api <> [decl'] }

addADT :: Slv.TypeDecl -> PublicAPI -> PublicAPI
addADT (Slv.Untyped _ (Slv.ADT name params ctors _ _)) api =
  let key  = name
      key' =
        if not (null params) then
          key <> " " <> unwords params
        else
          key
      ctors' = (\(Slv.Untyped _ (Slv.Constructor name ts _)) -> unwords $ name : (prettyPrintConstructorTyping <$> ts)) <$> ctors
  in  api { apiTypes = Map.insert key' ctors' $ apiTypes api }

addAlias :: Slv.TypeDecl -> PublicAPI -> PublicAPI
addAlias (Slv.Untyped _ (Slv.Alias name params typing _)) api =
  let key  = name
      key' =
        if not (null params) then
          key <> " " <> unwords params
        else
          key
      aliased = prettyPrintConstructorTyping typing
  in  api { apiAliases = Map.insert key' aliased $ apiAliases api }


buildAPI :: Slv.AST -> Slv.Table -> PublicAPI
buildAPI ast table =
  let exports           = Slv.extractExportedExps ast
      packageTypes      = Slv.extractExportedADTs ast
      packageAliases    = Slv.extractExportedAliases ast
      packageASTs       = Map.filterWithKey (\path _ -> not ("node_modules" `List.isInfixOf` path) && not ("prelude/__internal__" `List.isInfixOf` path)) table
      packageInterfaces = Map.elems packageASTs >>= Slv.ainterfaces
      packageInstances  = Map.elems packageASTs >>= Slv.ainstances

      apiWithNames      = PublicAPI
        { apiNames      = prettyPrintQualType True . Slv.getQualType <$> exports
        , apiInterfaces = mempty
        , apiInstances  = mempty
        , apiTypes      = mempty
        , apiAliases    = mempty
        }
      apiWithInterfaces = foldr addInterface apiWithNames packageInterfaces
      apiWithInstances  = foldr addInstance apiWithInterfaces packageInstances
      apiWithADTs       = foldr addADT apiWithInstances packageTypes
      apiWithAliases    = foldr addAlias apiWithADTs packageAliases
  in  apiWithAliases


performBuild :: Either String VersionLock -> Maybe Version -> String -> String -> Slv.AST -> Slv.Table -> Either String (String, VersionLock)
performBuild eitherVersionLock parsedVersion hashedVersion projectHash ast table =
  case eitherVersionLock of
    -- Left e -> Left "Error with the version.lock file"
    Left _ -> do
      let api = buildAPI ast table
      Left $ ppShow api

    Right VersionLock { versionHash, buildHash, api } ->
      if buildHash == projectHash then
        Left "Project has not changed, nothing to do."
      else if hashedVersion /= versionHash then
        Left "It seems that you modified the version in madlib.json manually"
      else do
        undefined




runBuildPackage :: IO ()
runBuildPackage = do
  putStrLn "Build package"
  madlibDotJson <- loadCurrentMadlibDotJson

  case madlibDotJson of
    Left _ -> putStrLn "The package must have a madlib.json file"

    Right MadlibDotJson { main, version = Just version } -> do
      canonicalMain           <- canonicalizePath main
      currentDirectoryPath    <- getCurrentDirectory
      versionLock             <- loadCurrentVersionLock
      projectHash             <- generatePackageHash currentDirectoryPath
      hashedVersion           <- hash $ BLChar8.pack version
      (typeChecked, warnings) <- typeCheckMain canonicalMain

      let parsedVersion = MadlibVersion.parse version

      case typeChecked of
        Left errors ->
          putStrLn "Compilation errors, please fix them before building the package"

        Right solvedTable -> do
          let (Just mainAST) = Map.lookup canonicalMain solvedTable
              processed      = performBuild versionLock parsedVersion hashedVersion projectHash mainAST solvedTable
          case processed of
            Left e -> putStrLn e

            Right _ -> putStrLn "Built"

      return ()


runPackage :: PackageSubCommand -> IO ()
runPackage subCommand = case subCommand of
  NoPackageSubCommand -> runBuildPackage

  GenerateHash input  -> runGeneratePackageHash input
