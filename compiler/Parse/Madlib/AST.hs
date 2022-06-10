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



-- buildASTTable' :: Target -> Table -> PathUtils -> FilePath -> Maybe Import -> [FilePath] -> FilePath -> IO (Either CompilationError Table)
-- buildASTTable' target previousTable pathUtils parentPath imp previousPaths srcPath
--   | srcPath `elem` previousPaths = return $ Left $ CompilationError (ImportCycle (previousPaths ++ [srcPath])) NoContext
--   | otherwise = do
--     let parentDir = dropFileName parentPath
--     resolveAbsoluteSrcPath pathUtils parentDir srcPath >>= \case
--       Nothing -> do
--         let ctx = case imp of
--               Just imp' -> Context parentPath (getArea imp') []
--               Nothing   -> NoContext
--         return $ Left $ CompilationError (ImportNotFound srcPath) ctx
--       Just absoluteSrcPath -> do
--         code <- try $ readFile pathUtils absoluteSrcPath :: IO (Either IOException String)

--         let ctx = case imp of
--               Just imp' -> Context parentPath (getArea imp') []
--               Nothing   -> NoContext

--         let source = case code of
--               Right a -> Right a
--               Left  _ -> Left $ CompilationError (ImportNotFound absoluteSrcPath) ctx

--         ast <- case source of
--           Left  e    ->
--             return $ Left $ CompilationError (ImportNotFound absoluteSrcPath) ctx

--           Right code -> do
--             ast' <- buildAST pathUtils srcPath code
--             return $ resolveMacros target <$> ast'

--         ast' <- case ast of
--           Right a -> do
--             a' <- addDictionaryImportIfNeeded pathUtils (dropFileName srcPath) a
--             return (Right a')

--           Left e ->
--             return (Left e)


--         getImportsWithAbsolutePaths pathUtils (dropFileName srcPath) ast' >>= \case
--           Left  x               -> return $ Left x
--           Right completeImports -> do
--             let (jsonImports, madImports) = partition (\case
--                                                         (Source _ _ (DefaultImport _ _ absPath)) -> takeExtension absPath == ".json"
--                                                         _ -> False
--                                                       ) completeImports
--             jsonAssignments <- generateJsonAssignments pathUtils jsonImports

--             let generatedTable =
--                   uncurry M.singleton . (absoluteSrcPath, ) . (\ast' -> ast' { aimports = madImports, aexps = jsonAssignments ++ aexps ast' }) <$> ast

--             childTables <- foldM
--               (\table imp' ->
--                 case table of
--                   Left e       -> return $ Left e
--                   Right table' -> buildChildTable target pathUtils previousPaths srcPath (previousTable <> table') imp'
--               )
--               (Right M.empty)
--               madImports

--             return $ liftM2 M.union generatedTable childTables


-- buildChildTable :: Target -> PathUtils -> [FilePath] -> FilePath -> Table -> Import -> IO (Either CompilationError Table)
-- buildChildTable target pathUtils previousPaths srcPath table imp = do
--   let absPath = getImportAbsolutePath imp
--   builtImport <- case M.lookup absPath table of
--     Just ast -> return $ Right $ M.singleton absPath ast
--     Nothing  -> buildASTTable' target
--                                table
--                                pathUtils
--                                srcPath
--                                (Just imp)
--                                (previousPaths ++ [srcPath])
--                                (getImportAbsolutePath imp)
--   case builtImport of
--     Right x -> return $ Right (M.union table x)
--     Left  e -> return $ Left e


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


-- getImportsWithAbsolutePaths :: PathUtils -> FilePath -> Either CompilationError AST -> IO (Either CompilationError [Import])
-- getImportsWithAbsolutePaths pathUtils ctxPath ast =
--   let astPath = case ast of
--         Right x ->
--           fromMaybe "" $ apath x

--         Left _ ->
--           ""

--   in  case ast of
--         Left  x ->
--           return $ Left x

--         Right ast' ->
--           sequence <$> mapM (updatePath astPath) (aimports ast')

--  where
--   updatePath :: FilePath -> Import -> IO (Either CompilationError Import)
--   updatePath path imp = do
--     let importPath = (snd . getImportPath) imp
--     absolutePath <- resolveAbsoluteSrcPath pathUtils ctxPath importPath
--     case absolutePath of
--       Nothing -> do
--         return $ Left $ CompilationError (ImportNotFound importPath) (Context path (getArea imp) [])

--       Just abs -> do
--         return $ Right (setImportAbsolutePath imp abs)



setImportAbsolutePath :: Import -> FilePath -> Import
setImportAbsolutePath imp fp = case imp of
  Source a sourceTarget (NamedImport   s p _) ->
    Source a sourceTarget (NamedImport s p fp)

  Source a sourceTarget (TypeImport   s p _)  ->
    Source a sourceTarget (TypeImport s p fp)

  Source a sourceTarget (DefaultImport s p _) ->
    Source a sourceTarget (DefaultImport s p fp)

  Source a sourceTarget (ImportAll p _)       ->
    Source a sourceTarget (ImportAll p fp)



findAST :: Table -> FilePath -> Either CompilationError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ CompilationError (ImportNotFound path) NoContext


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
