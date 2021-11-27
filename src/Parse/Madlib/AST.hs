{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
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
import           System.FilePath                ( dropFileName, takeExtension )
import qualified Prelude                       as P
import           Prelude                       hiding ( readFile )
import qualified System.Environment.Executable as E
import           Data.Maybe
import           Explain.Location
import Data.List



buildManyASTTables :: Table -> [FilePath] -> IO (Either CompilationError Table)
buildManyASTTables currentTable fps = case fps of
  [] -> return $ return mempty
  fp:fps' -> do
    current <- buildASTTable currentTable fp
    next    <- case current of
      Left e  -> return $ Left e
      Right t -> buildManyASTTables (currentTable <> t) fps'
    return $ current >>= (\current' -> next >>= (\next' -> return $ current' <> next'))


buildASTTable :: Table -> FilePath -> IO (Either CompilationError Table)
buildASTTable table path = do
  buildASTTable' table defaultPathUtils path Nothing [] path


buildASTTable' :: Table -> PathUtils -> FilePath -> Maybe Import -> [FilePath] -> FilePath -> IO (Either CompilationError Table)
buildASTTable' previousTable pathUtils parentPath imp previousPaths srcPath
  | srcPath `elem` previousPaths = return $ Left $ CompilationError (ImportCycle (previousPaths ++ [srcPath])) NoContext
  | otherwise = do
    let parentDir = dropFileName parentPath
    resolveAbsoluteSrcPath pathUtils parentDir srcPath >>= \case
      Nothing -> do
        let ctx = case imp of
              Just imp' -> Context parentPath (getArea imp') []
              Nothing   -> NoContext
        return $ Left $ CompilationError (ImportNotFound srcPath) ctx
      Just absoluteSrcPath -> do
        code <- try $ readFile pathUtils absoluteSrcPath :: IO (Either IOException String)

        let ctx = case imp of
              Just imp' -> Context parentPath (getArea imp') []
              Nothing   -> NoContext

        let source = case code of
              Right a -> Right a
              Left  _ -> Left $ CompilationError (ImportNotFound absoluteSrcPath) ctx

        ast <- case source of
          Left  e    -> return $ Left $ CompilationError (ImportNotFound absoluteSrcPath) ctx
          Right code -> return $ buildAST srcPath code

        getImportsWithAbsolutePaths pathUtils (dropFileName srcPath) ast >>= \case
          Left  x               -> return $ Left x
          Right completeImports -> do
            let (jsonImports, madImports) = partition (\case
                                                        (Source _ (DefaultImport _ _ absPath)) -> takeExtension absPath == ".json"
                                                        _ -> False
                                                      ) completeImports
            jsonAssignments <- generateJsonAssignments pathUtils jsonImports

            let generatedTable =
                  uncurry M.singleton . (absoluteSrcPath, ) . (\ast' -> ast' { aimports = madImports, aexps = jsonAssignments ++ aexps ast' }) <$> ast

            childTables <- foldM
              (\table imp'@(Source area _) ->
                case table of
                  Left e       -> return $ Left e
                  Right table' -> buildChildTable pathUtils previousPaths srcPath (previousTable <> table') imp'
              )
              (Right M.empty)
              madImports

            return $ liftM2 M.union generatedTable childTables


buildChildTable :: PathUtils -> [FilePath] -> FilePath -> Table -> Import -> IO (Either CompilationError Table)
buildChildTable pathUtils previousPaths srcPath table imp = do
  let absPath = getImportAbsolutePath imp
  builtImport <- case M.lookup absPath table of
    Just ast -> return $ Right $ M.singleton absPath ast
    Nothing  -> buildASTTable' table
                               pathUtils
                               srcPath
                               (Just imp)
                               (previousPaths ++ [srcPath])
                               (getImportAbsolutePath imp)
  case builtImport of
    Right x -> return $ Right (M.union table x)
    Left  e -> return $ Left e


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
generateJsonAssignments pathUtils ((Source area (DefaultImport (Source _ name) _ absPath)):imps) = do
  next <- generateJsonAssignments pathUtils imps
  jsonContent <- readFile pathUtils absPath
  let var = Source area (LStr $ "\"" <> escapeJSONString jsonContent <> "\"")
  let assignment = Source area (Assignment name var)

  return $ assignment : next


getImportsWithAbsolutePaths :: PathUtils -> FilePath -> Either CompilationError AST -> IO (Either CompilationError [Import])
getImportsWithAbsolutePaths pathUtils ctxPath ast =
  let astPath = case ast of
        Right x -> fromMaybe "" $ apath x
        Left  _ -> ""
  in  case ast of
        Left  x    -> return $ Left x
        Right ast' -> sequence <$> mapM (updatePath astPath) (aimports ast')
 where
  updatePath :: FilePath -> Import -> IO (Either CompilationError Import)
  updatePath path imp = do
    let importPath = (snd . getImportPath) imp
    absolutePath <- resolveAbsoluteSrcPath pathUtils ctxPath importPath
    case absolutePath of
      Nothing  -> return $ Left $ CompilationError (ImportNotFound importPath) (Context path (getArea imp) [])
      Just abs -> return $ Right (setImportAbsolutePath imp abs)


setImportAbsolutePath :: Import -> FilePath -> Import
setImportAbsolutePath imp fp = case imp of
  Source a (NamedImport   s p _) -> Source a (NamedImport s p fp)
  Source a (TypeImport   s p _)  -> Source a (TypeImport s p fp)
  Source a (DefaultImport s p _) -> Source a (DefaultImport s p fp)
  Source a (ImportAll p _)       -> Source a (ImportAll p fp)


findAST :: Table -> FilePath -> Either CompilationError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ CompilationError (ImportNotFound path) NoContext


buildAST :: FilePath -> String -> Either CompilationError AST
buildAST path code = case parse code of
  Right ast -> setPath ast path
  Left e ->
    let split = lines e
        line  = (read $ head split) :: Int
        col   = (read $ split !! 1) :: Int
        text  = unlines (tail . tail $ split)
    in  Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line col) (Loc 0 line (col + 1))) [])


setPath :: AST -> FilePath -> Either e AST
setPath ast path = Right ast { apath = Just path }
