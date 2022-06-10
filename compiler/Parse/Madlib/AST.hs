{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Parse.Madlib.AST where

import qualified Data.Map                      as M
import           Control.Exception              ( IOException
                                                , try
                                                )

import           Parse.Madlib.Grammar           ( parse )
import           AST.Source
import           Utils.Path                     ( resolveAbsoluteSrcPath )
import           Utils.PathUtils
import           Error.Error
import           Error.Context

import           Control.Monad.Except
import           System.FilePath                ( dropFileName, takeExtension, normalise )
import qualified Prelude                       as P
import           Prelude                       hiding ( readFile )
import qualified System.Environment.Executable as E
import           Data.Maybe
import           Explain.Location
import Data.List
import           Parse.Macro
import           Run.Target
import Debug.Trace
import Text.Show.Pretty
import Parse.Madlib.TargetMacro
import Parse.Madlib.Dictionary
import Run.Options



isJsonImport :: Import -> Bool
isJsonImport imp = case imp of
  (Source _ _ (DefaultImport _ _ absPath)) ->
    takeExtension absPath == ".json"

  _ ->
    False


processJsonImports :: Options -> AST -> IO AST
processJsonImports options ast@AST{ aimports } = do
  let (jsonImports, madImports) = partition isJsonImport aimports
  jsonAssignments <- generateJsonAssignments (optPathUtils options) jsonImports
  return ast { aimports = madImports, aexps = jsonAssignments ++ aexps ast }


escapeJSONString :: String -> String
escapeJSONString s = case s of
  '\n':r -> "\\n" ++ escapeJSONString r
  '\t':r -> "\\t" ++ escapeJSONString r
  '\\':r -> "\\\\" ++ escapeJSONString r
  '\"':r -> "\\\"" ++ escapeJSONString r
  '\'':r -> "\\\'" ++ escapeJSONString r
  x:r    -> x : escapeJSONString r
  _      -> ""


generateJsonAssignments :: PathUtils -> [Import] -> IO [Exp]
generateJsonAssignments pathUtils [] = return []
generateJsonAssignments pathUtils ((Source area sourceTarget (DefaultImport (Source _ _ name) _ absPath)):imps) = do
  next <- generateJsonAssignments pathUtils imps
  jsonContent <- readFile pathUtils absPath
  let var = Source area sourceTarget (LStr $ "\"" <> escapeJSONString jsonContent <> "\"")
  let assignment = Source area sourceTarget (Assignment name var)

  return $ assignment : next


buildAST :: Options -> FilePath -> String -> IO (Either CompilationError AST)
buildAST options path code = case parse code of
  Right ast -> do
    let astWithPath = setPath ast path
    let astWithProcessedMacros = resolveMacros (optTarget options) astWithPath
    astWithDictImport          <- addDictionaryImportIfNeeded (optPathUtils options) (dropFileName path) astWithProcessedMacros
    astWithAbsoluteImportPaths <- computeAbsoluteImportPaths (optRootPath options) astWithDictImport
    astWithJsonAssignments     <- processJsonImports options astWithAbsoluteImportPaths
    return $ Right astWithJsonAssignments

  Left e -> do
    let split = lines e
        line  = (read $ head split) :: Int
        col   = (read $ split !! 1) :: Int
        text  = unlines (tail . tail $ split)
    return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line col) (Loc 0 line (col + 1))) [])


setPath :: AST -> FilePath -> AST
setPath ast path = ast { apath = Just path }


computeAbsoluteImportPath :: FilePath -> Import -> IO Import
computeAbsoluteImportPath rootPath (Source area target imp) = case imp of
  NamedImport names rel _ -> do
    (Just abs) <- resolveAbsoluteSrcPath defaultPathUtils rootPath rel
    return $ Source area target $ NamedImport names rel abs

  TypeImport names rel _ -> do
    (Just abs) <- resolveAbsoluteSrcPath defaultPathUtils rootPath rel
    return $ Source area target $ TypeImport names rel abs

  DefaultImport namespace rel _ -> do
    (Just abs) <- resolveAbsoluteSrcPath defaultPathUtils rootPath rel
    return $ Source area target $ DefaultImport namespace rel abs

  ImportAll rel _ -> do
    (Just abs) <- resolveAbsoluteSrcPath defaultPathUtils rootPath rel
    return $ Source area target $ ImportAll rel abs


computeAbsoluteImportPaths :: FilePath -> AST -> IO AST
computeAbsoluteImportPaths rootPath ast@AST{ aimports, apath = Just path } = do
  updatedImports <- mapM (computeAbsoluteImportPath (dropFileName path)) aimports
  return ast { aimports = updatedImports }
