{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parse.Madlib.AST where

import qualified Data.Map                         as M
import           Control.Exception                ( IOException
                                                  , try, SomeException (SomeException)
                                                  )
import           Parse.Madlib.Grammar             ( parse )
import           AST.Source 
import           Utils.Path                       ( resolveAbsoluteSrcPath )
import           Utils.PathUtils
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import           System.FilePath                  ( dropFileName, takeExtension, normalise )
import qualified Prelude                          as P
import           Prelude                          hiding ( readFile )
import qualified System.Environment.Executable    as E
import           Explain.Location
import           Data.List
import           Parse.Madlib.TargetMacro
import           Parse.Madlib.Dictionary
import           Run.Options
import           Text.Read (readMaybe)
import           Text.Show.Pretty (ppShow)



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
generateJsonAssignments _ [] = return []
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
    let builtinsImport = Source emptyArea TargetAll $ NamedImport [] "__BUILTINS__" "__BUILTINS__"
    let astWithBuiltinsImport =
          if "__BUILTINS__.mad" `isSuffixOf` path then
            astWithDictImport
          else
            astWithDictImport { aimports = builtinsImport : aimports astWithDictImport }
    astWithAbsoluteImportPaths <- computeAbsoluteImportPathsForAST (optPathUtils options) (optRootPath options) astWithBuiltinsImport
    case astWithAbsoluteImportPaths of
      Right astWithAbsoluteImportPaths' -> do
        astWithJsonAssignments     <- processJsonImports options astWithAbsoluteImportPaths'
        return $ Right astWithJsonAssignments

      Left _ ->
        return astWithAbsoluteImportPaths

  Left e -> do
    let split = lines e
        line  = (readMaybe $ head split) :: Maybe Int
        col   = (readMaybe $ split !! 1) :: Maybe Int
        text  = if length split < 2 then
                  "Syntax error"
                else
                  unlines (tail . tail $ split)

    case (line, col) of
      (Just line', Just col') ->
        return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' col') (Loc 0 line' (col' + 1))))

      (Just line', Nothing) ->
        return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' 0) (Loc 0 (line' + 1) 0)))

      _ -> do
        -- then we try to parse this: "lexical error at line 2, column 11"
        let split' = words e
        let line = (readMaybe $ init $ split' !! 4) :: Maybe Int
            col  = (readMaybe $ split' !! 6) :: Maybe Int
            text = "Syntax error"

        case (line, col) of
          (Just line', Just col') ->
            return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' col') (Loc 0 line' (col' + 1))))

          (Just line', Nothing) ->
            return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' 0) (Loc 0 (line' + 1) 0)))

          _ ->
            return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 1 1) (Loc 1 1 1)))


setPath :: AST -> FilePath -> AST
setPath ast path = ast { apath = Just path }


computeAbsoluteImportPath :: PathUtils -> FilePath -> Import -> IO (Maybe Import)
computeAbsoluteImportPath pathUtils rootPath (Source area target imp) = case imp of
  NamedImport names rel _ -> do
    abs <- resolveAbsoluteSrcPath pathUtils rootPath rel
    return $ Source area target . NamedImport names rel <$> abs

  TypeImport names rel _ -> do
    abs <- resolveAbsoluteSrcPath pathUtils rootPath rel
    return $ Source area target . TypeImport names rel <$> abs

  DefaultImport namespace rel _ -> do
    abs <- resolveAbsoluteSrcPath pathUtils rootPath rel
    return $ Source area target . DefaultImport namespace rel <$> abs


computeAbsoluteImportPaths :: PathUtils -> FilePath -> FilePath -> [Import] -> IO (Either CompilationError [Import])
computeAbsoluteImportPaths pathUtils astPath rootPath imps = case imps of
  imp : next -> do
    imp' <- computeAbsoluteImportPath pathUtils (dropFileName astPath) imp
    case imp' of
      Nothing ->
        return
          $ Left
          $ CompilationError
              (ImportNotFound $ snd $ getImportPath imp)
              (Context astPath (getArea imp))

      Just good -> do
        next' <- computeAbsoluteImportPaths pathUtils astPath rootPath next
        return $ (good :) <$> next'

  [] ->
    return $ Right []


computeAbsoluteImportPathsForAST :: PathUtils -> FilePath -> AST -> IO (Either CompilationError AST)
computeAbsoluteImportPathsForAST pathUtils rootPath ast@AST{ aimports, apath = Just path } = do
  updatedImports <- computeAbsoluteImportPaths pathUtils path rootPath aimports
  return $ (\updated -> ast { aimports = updated }) <$> updatedImports
computeAbsoluteImportPathsForAST _ _ ast = error $ "ast has no path\n\n" <> ppShow ast
