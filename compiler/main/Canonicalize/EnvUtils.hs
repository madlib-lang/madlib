{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Canonicalize.EnvUtils where

import qualified Data.Map               as Map
import           Canonicalize.Env
import           Canonicalize.CanonicalM
import qualified Rock
import qualified Driver.Query           as Query
import qualified Data.List              as List
import           Infer.Type
import           Control.Monad.Except
import           Error.Error
import           Error.Context


addADT :: Env -> String -> Type -> Env
addADT env name adt =
  let adts         = envTypeDecls env
      withAddition = Map.insert name adt adts
  in  env { envTypeDecls = withAddition }


addConstructorInfos :: Env -> String -> [ConstructorInfo] -> Env
addConstructorInfos env typeName infos =
  let constructorInfos = envConstructorInfos env
      withInfos = Map.insert typeName infos constructorInfos
  in  env { envConstructorInfos = withInfos }


isTypeNameInImport :: String -> ImportInfo -> Bool
isTypeNameInImport typeName ImportInfo { iiType, iiName }
  | iiType == TypeImport && iiName == typeName = True
  | iiType == NamespaceImport && iiName == takeWhile (/= '.') typeName = True
  | otherwise = False


lookupConstructorInfos :: Env -> String -> CanonicalM (Maybe [ConstructorInfo])
lookupConstructorInfos env name = do
  case List.find (isTypeNameInImport name) $ envImportInfo env of
    Just (ImportInfo path TypeImport typeName) ->
      Rock.fetch $ Query.ForeignConstructorInfos path typeName

    Just (ImportInfo path NamespaceImport _) ->
      if '.' `elem` name then
        Rock.fetch $ Query.ForeignConstructorInfos path (tail $ dropWhile (/= '.') name)
      else
        Rock.fetch $ Query.ForeignConstructorInfos path name

    _ ->
      return $ Map.lookup name (envConstructorInfos env)

  -- case maybeType of
  --   Just t ->
  --     return t

  --   Nothing ->
  --     throwError $ CompilationError (UnknownType name) NoContext


lookupADT :: Env -> String -> CanonicalM Type
lookupADT env name = do
  maybeType <- case List.find (isTypeNameInImport name) $ envImportInfo env of
    Just (ImportInfo path TypeImport typeName) ->
      Rock.fetch $ Query.ForeignADTType path typeName

    Just (ImportInfo path NamespaceImport _) ->
      if '.' `elem` name then
        Rock.fetch $ Query.ForeignADTType path (tail $ dropWhile (/= '.') name)
      else
        Rock.fetch $ Query.ForeignADTType path name

    _ ->
      return $ Map.lookup name (envTypeDecls env)

  case maybeType of
    Just t ->
      return t

    Nothing ->
      throwError $ CompilationError (UnknownType name) NoContext


lookupADT' :: Rock.MonadFetch Query.Query m => Env -> String -> m (Maybe Type)
lookupADT' env name = do
  case List.find (isTypeNameInImport name) $ envImportInfo env of
    Just (ImportInfo path TypeImport typeName) ->
      Rock.fetch $ Query.ForeignADTType path typeName

    Just (ImportInfo path NamespaceImport _) ->
      if '.' `elem` name then
        Rock.fetch $ Query.ForeignADTType path (tail $ dropWhile (/= '.') name)
      else
        Rock.fetch $ Query.ForeignADTType path name

    _ ->
      return $ Map.lookup name (envTypeDecls env)
