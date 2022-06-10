{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Canonicalize.EnvUtil where

import qualified Data.Map as Map
import           Canonicalize.Env
import           Canonicalize.CanonicalM
import qualified Rock
import qualified Driver.Query as Query
import qualified Data.List    as List
import Infer.Type
import Control.Monad.Except
import Error.Error
import Error.Context

addADT :: Env -> String -> Type -> Env
addADT env name adt =
  let adts         = envTypeDecls env
      withAddition = Map.insert name adt adts
  in  env { envTypeDecls = withAddition }


isTypeNameInImport :: String -> ImportInfo -> Bool
isTypeNameInImport typeName ImportInfo { iiType, iiName }
  | iiType == TypeImport && iiName == typeName = True
  | iiType == NamespaceImport && iiName == takeWhile (/= '.') typeName = True
  | otherwise = False


lookupADT :: Env -> String -> CanonicalM Type
lookupADT env name = case List.find (isTypeNameInImport name) $ envImportInfo env of
  Just (ImportInfo path TypeImport typeName) -> do
    Rock.fetch $ Query.ForeignType path typeName

  Just (ImportInfo path NamespaceImport ns) ->
    Rock.fetch $ Query.ForeignType path (tail $ dropWhile (/= '.') name)

  Nothing ->
    case Map.lookup name (envTypeDecls env) of
      Just found ->
        return found

      Nothing    ->
        throwError $ CompilationError (UnknownType name) NoContext

