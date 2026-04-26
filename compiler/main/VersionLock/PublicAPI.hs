{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module VersionLock.PublicAPI where

import           Data.Aeson
import           GHC.Generics                  ( Generic )
import           System.Directory              ( canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory )
import           System.FilePath               ( takeDirectory, joinPath, pathSeparator )
import           Text.Show.Pretty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLChar8
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder
import           Data.Version
import           Explain.Format


import           MadlibDotJson.MadlibDotJson
import qualified MadlibDotJson.MadlibVersion        as MadlibVersion
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

data PublicAPI = PublicAPI
  { apiNames      :: Map.Map String String
  , apiInterfaces :: Map.Map String (Map.Map String String)
  , apiInstances  :: [String]
  , apiTypes      :: Map.Map String [String]
  , apiAliases    :: Map.Map String String
  }
  deriving(Eq, Show, Ord, Generic)

data APIChange
  = Major
  | Minor
  | Patch

instance FromJSON PublicAPI
instance ToJSON PublicAPI

intersect :: PublicAPI -> PublicAPI -> PublicAPI
intersect previousAPI currentAPI = PublicAPI
  { apiNames      = Map.intersection (apiNames currentAPI) (apiNames previousAPI)
  , apiInterfaces = Map.intersection (apiInterfaces currentAPI) (apiInterfaces previousAPI)
  , apiInstances  = apiInstances currentAPI `List.intersect` apiInstances previousAPI
  , apiTypes      = Map.intersection (apiTypes currentAPI) (apiTypes previousAPI)
  , apiAliases    = Map.intersection (apiAliases currentAPI) (apiAliases previousAPI)
  }

computeAPIChange :: PublicAPI -> PublicAPI -> APIChange
computeAPIChange previousAPI currentAPI
  | previousAPI == currentAPI                       = Patch
  | intersect previousAPI currentAPI == previousAPI = Minor
  | otherwise                                       = Major


addInterface :: Slv.Interface -> PublicAPI -> PublicAPI
addInterface (Slv.Untyped _ (Slv.Interface name supers vars _ methodTypings)) api =
  let key     = name <> " " <> unwords (renderTVar . getTVarId <$> vars)
      key'   =
        if not (null supers) then
          lst (predsToStr False mempty supers) <> " => " <> key
        else
          key
      methods = prettyPrintTyping' False <$> methodTypings
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

addADT :: String -> Slv.TypeDecl -> PublicAPI -> PublicAPI
addADT prefix (Slv.Untyped _ (Slv.ADT name params ctors _ _)) api =
  let key  = name
      key' =
        if not (null params) then
          key <> " " <> unwords params
        else
          key
      ctors' = (\(Slv.Untyped _ (Slv.Constructor name ts _)) -> unwords $ name : (prettyPrintTyping <$> ts)) <$> ctors
  in  api { apiTypes = Map.insert (prefix <> key') ctors' $ apiTypes api }

addAlias :: String -> Slv.TypeDecl -> PublicAPI -> PublicAPI
addAlias prefix (Slv.Untyped _ (Slv.Alias name params typing _)) api =
  let key  = name
      key' =
        if not (null params) then
          key <> " " <> unwords params
        else
          key
      aliased = prettyPrintTyping typing
  in  api { apiAliases = Map.insert (prefix <> key') aliased $ apiAliases api }


-- | Compute the package-relative path of a module file inside the package,
-- normalised to use forward slashes so it's portable across platforms.
relativeModulePath :: FilePath -> FilePath -> FilePath
relativeModulePath packageRoot modulePath =
  let normaliseSeps = map (\c -> if c == '\\' then '/' else c)
      normalisedRoot = normaliseSeps packageRoot
      normalisedMod  = normaliseSeps modulePath
      withTrailingSep = normalisedRoot <> "/"
  in  if withTrailingSep `List.isPrefixOf` normalisedMod then
        drop (length withTrailingSep) normalisedMod
      else
        normalisedMod


-- | The key prefix used to namespace an export by the module it lives in.
-- The main module is the one identified by `mainPath`; its exports are kept
-- with un-prefixed keys to remain stable as the canonical entry-point API.
moduleKeyPrefix :: FilePath -> FilePath -> FilePath -> String
moduleKeyPrefix packageRoot mainPath modulePath
  | modulePath == mainPath = ""
  | otherwise              = relativeModulePath packageRoot modulePath <> "#"


-- | Build the public API of the package by aggregating exports, types,
-- aliases, interfaces and instances from every source module of the package
-- (i.e. the main entry-point AND every reachable sub-module that lives inside
-- the package source tree). Modules from `madlib_modules` (transitive deps)
-- and the prelude are excluded, since they're not part of *this* package's
-- public API surface.
--
-- The main module's exports keep flat keys (e.g. "eval"); sub-module exports
-- are namespaced by the module's package-relative path, e.g.
-- "src/Math/Basic.mad#eval", so that consumers using `from "pkg/Math/Basic"`
-- have their direct dependency tracked for version-bump purposes.
buildAPI :: FilePath -> Slv.AST -> Slv.Table -> PublicAPI
buildAPI packageRoot mainAst table =
  let mainPath          = Maybe.fromMaybe "" (Slv.apath mainAst)
      -- The package's own transitive deps live under `<packageRoot>/madlib_modules/`.
      -- Exclude *those* (and the prelude) but keep modules that just happen to be
      -- in a `madlib_modules/` further up the tree (e.g. when a package is itself
      -- being type-checked from inside another project's madlib_modules — the
      -- canonical fixture layout for tests).
      depsPrefix        = packageRoot <> [pathSeparator] <> "madlib_modules" <> [pathSeparator]
      packageASTs       = Map.filterWithKey (\path _ -> not (depsPrefix `List.isPrefixOf` path) && not (("prelude" <> (pathSeparator : "__internal__")) `List.isInfixOf` path)) table
      packageInterfaces = Map.elems packageASTs >>= Slv.ainterfaces
      packageInstances  = filter ((\name -> name /= "Eq" && name /= "Show") . Slv.getInstanceName) $ Map.elems packageASTs >>= Slv.ainstances

      perModuleEntries  = do
        (modPath, ast) <- Map.toList packageASTs
        let prefix     = moduleKeyPrefix packageRoot mainPath modPath
        return (prefix, ast)

      apiNamesMap       = Map.fromList $ do
        (prefix, ast) <- perModuleEntries
        (name, expr)  <- Map.toList (Slv.extractExportedExps ast)
        return (prefix <> name, prettyPrintQualType (Slv.getQualType expr))

      emptyAPI          = PublicAPI
        { apiNames      = apiNamesMap
        , apiInterfaces = mempty
        , apiInstances  = mempty
        , apiTypes      = mempty
        , apiAliases    = mempty
        }
      apiWithInterfaces = foldr addInterface emptyAPI packageInterfaces
      apiWithInstances  = foldr addInstance apiWithInterfaces packageInstances
      apiWithADTs       = foldr (\(prefix, td) -> addADT prefix td) apiWithInstances $ do
        (prefix, ast) <- perModuleEntries
        td            <- Slv.extractExportedADTs ast
        return (prefix, td)
      apiWithAliases    = foldr (\(prefix, td) -> addAlias prefix td) apiWithADTs $ do
        (prefix, ast) <- perModuleEntries
        td            <- Slv.extractExportedAliases ast
        return (prefix, td)
  in  apiWithAliases
