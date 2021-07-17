{-# LANGUAGE DeriveGeneric #-}
module VersionLock.PublicAPI where

import           Data.Aeson
import           GHC.Generics                  ( Generic )
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
      packageASTs       = Map.filterWithKey (\path _ -> not ("madlib_modules" `List.isInfixOf` path) && not ("prelude/__internal__" `List.isInfixOf` path)) table
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
