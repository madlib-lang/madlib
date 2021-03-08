{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
module Parse.AST where

import qualified Data.Map                      as M
import           Control.Exception              ( IOException
                                                , try
                                                )

import           Parse.Grammar                  ( parse )
import           AST.Source
import           Utils.Path                     ( resolveAbsoluteSrcPath )
import           Utils.PathUtils
import           Explain.Meta
import           Error.Error
import qualified System.Directory              as Dir
import           Control.Monad.Except
import           System.FilePath                ( dropFileName )
import qualified Prelude                       as P
import           Prelude                 hiding ( readFile )
import qualified Data.ByteString.Lazy          as B
import qualified System.Environment.Executable as E
import           Data.Maybe
import           Explain.Location




-- TODO: Write an integration test with real files ?
-- Move that to Main and rename buildASTTable' to buildASTTable
-- Then use the scoped name in Main in order to partially apply it.
buildASTTable :: FilePath -> IO (Either InferError Table)
buildASTTable path = do
  let pathUtils = PathUtils { readFile           = P.readFile
                            , canonicalizePath   = Dir.canonicalizePath
                            , doesFileExist      = Dir.doesFileExist
                            , byteStringReadFile = B.readFile
                            , getExecutablePath  = E.getExecutablePath
                            }
  buildASTTable' pathUtils path Nothing [] path


buildASTTable' :: PathUtils -> FilePath -> Maybe Import -> [FilePath] -> FilePath -> IO (Either InferError Table)
buildASTTable' pathUtils parentPath imp previousPaths srcPath
  | srcPath `elem` previousPaths = return $ Left $ InferError (ImportCycle (previousPaths ++ [srcPath])) NoContext
  | otherwise = do
    let parentDir = dropFileName parentPath
    resolveAbsoluteSrcPath pathUtils parentDir srcPath >>= \case
      Nothing -> do
        let ctx = case imp of
              Just imp' -> Context parentPath (getArea imp') []
              Nothing   -> NoContext
        return $ Left $ InferError (ImportNotFound srcPath) ctx
      Just absoluteSrcPath -> do
        code <- try $ readFile pathUtils absoluteSrcPath :: IO (Either IOException String)

        let ctx = case imp of
              Just imp' -> Context parentPath (getArea imp') []
              Nothing   -> NoContext

        let source = case code of
              Right a -> Right a
              Left  _ -> Left $ InferError (ImportNotFound absoluteSrcPath) ctx

        ast <- case source of
          Left  e    -> return $ Left $ InferError (ImportNotFound absoluteSrcPath) ctx
          Right code -> return $ buildAST srcPath code

        getImportsWithAbsolutePaths pathUtils (dropFileName srcPath) ast >>= \case
          Left  x               -> return $ Left x
          Right completeImports -> do
            let generatedTable =
                  uncurry M.singleton . (absoluteSrcPath, ) . (\ast' -> ast' { aimports = completeImports }) <$> ast

            childTables <- mapM
              (\imp'@(Source _ area _) -> do
                builtImport <- buildASTTable' pathUtils
                                              srcPath
                                              (Just imp')
                                              (previousPaths ++ [srcPath])
                                              (getImportAbsolutePath imp')
                case builtImport of
                  Right x                -> return $ Right x
                  Left  (InferError e _) -> return $ Left $ InferError e (Context srcPath area [])
              )
              completeImports

            return $ foldr (liftM2 M.union) generatedTable childTables


getImportsWithAbsolutePaths :: PathUtils -> FilePath -> Either InferError AST -> IO (Either InferError [Import])
getImportsWithAbsolutePaths pathUtils ctxPath ast =
  let astPath = case ast of
        Right x -> fromMaybe "" $ apath x
        Left  _ -> ""
  in  case ast of
        Left  x    -> return $ Left x
        Right ast' -> sequence <$> mapM (updatePath astPath) (aimports ast')
 where
  updatePath :: FilePath -> Import -> IO (Either InferError Import)
  updatePath path imp = do
    let importPath = (snd . getImportPath) imp
    absolutePath <- resolveAbsoluteSrcPath pathUtils ctxPath importPath
    case absolutePath of
      Nothing  -> return $ Left $ InferError (ImportNotFound importPath) (Context path (getArea imp) [])
      Just abs -> return $ Right (setImportAbsolutePath imp abs)


setImportAbsolutePath :: Import -> FilePath -> Import
setImportAbsolutePath imp fp = case imp of
  Source i a (NamedImport   s p _) -> Source i a (NamedImport s p fp)
  Source i a (DefaultImport s p _) -> Source i a (DefaultImport s p fp)


findAST :: Table -> FilePath -> Either InferError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ InferError (ImportNotFound path) NoContext


buildAST :: FilePath -> String -> Either InferError AST
buildAST path code = case parse code of
  Right ast -> setPath ast path
  Left e ->
    let split = lines e
        line  = (read $ head split) :: Int
        col   = (read $ split !! 1) :: Int
        text  = unlines (tail . tail $ split)
    in  Left $ InferError (GrammarError path text) (Context path (Area (Loc 0 line col) (Loc 0 line (col + 1))) [])

setPath :: AST -> FilePath -> Either e AST
setPath ast path = Right ast { apath = Just path }
