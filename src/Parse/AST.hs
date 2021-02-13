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
import           Explain.Reason
import qualified System.Directory              as Dir
import           Control.Monad.Except
import           System.FilePath                ( dropFileName )
import qualified Prelude                       as P
import           Prelude                 hiding ( readFile )
import qualified Data.ByteString.Lazy          as B
import qualified System.Environment.Executable as E




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


buildASTTable'
  :: PathUtils
  -> FilePath
  -> Maybe Import
  -> [FilePath]
  -> FilePath
  -> IO (Either InferError Table)
buildASTTable' pathUtils parentPath imp previousPaths srcPath
  | srcPath `elem` previousPaths = return $ Left $ InferError
    (ImportCycle (previousPaths ++ [srcPath]))
    NoReason
  | otherwise = do
    let parentDir = dropFileName parentPath
    absoluteSrcPath <- resolveAbsoluteSrcPath pathUtils parentDir srcPath

    code            <-
      try $ readFile pathUtils absoluteSrcPath :: IO (Either IOException String)

    let reason = case imp of
          Just imp' -> Reason (WrongImport imp') parentPath (getArea imp')
          Nothing   -> NoReason

    let source = case code of
          Right a -> Right a
          Left  _ -> Left $ InferError (ImportNotFound absoluteSrcPath) reason

    ast <- case source of
      Left  e    -> return $ Left e
      Right code -> return $ buildAST srcPath code

    completeImports <- getImportsWithAbsolutePaths pathUtils
                                                   (dropFileName srcPath)
                                                   ast

    let generatedTable =
          uncurry M.singleton
            .   (absoluteSrcPath, )
            .   (\ast' -> ast' { aimports = completeImports })
            <$> ast

    childTables <- mapM
      (\imp' -> buildASTTable' pathUtils
                               srcPath
                               (Just imp')
                               (previousPaths ++ [srcPath])
                               (getImportAbsolutePath imp')
      )
      completeImports

    return $ foldr (liftM2 M.union) generatedTable childTables


getImportsWithAbsolutePaths
  :: PathUtils -> FilePath -> Either e AST -> IO [Import]
getImportsWithAbsolutePaths pathUtils ctxPath ast = case ast of
  Left  _    -> return []
  Right ast' -> mapM updatePath (aimports ast')
 where
  updatePath :: Import -> IO Import
  updatePath imp = do
    let importPath = (snd . getImportPath) imp
    absolutePath <- resolveAbsoluteSrcPath pathUtils ctxPath importPath
    return (setImportAbsolutePath imp absolutePath)


setImportAbsolutePath :: Import -> FilePath -> Import
setImportAbsolutePath imp fp = case imp of
  Source i a (NamedImport   s p _) -> Source i a (NamedImport s p fp)
  Source i a (DefaultImport s p _) -> Source i a (DefaultImport s p fp)


findAST :: Table -> FilePath -> Either InferError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ InferError (ImportNotFound path) NoReason


buildAST :: FilePath -> String -> Either InferError AST
buildAST path code = case parse code of
  Right ast -> setPath ast path
  Left  e   -> Left $ InferError (GrammarError path e) NoReason

setPath :: AST -> FilePath -> Either e AST
setPath ast path = Right ast { apath = Just path }
