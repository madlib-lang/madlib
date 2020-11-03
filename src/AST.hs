{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
module AST where

import qualified Data.Map                      as M
import           Control.Monad                  ( liftM2 )
import           Data.Either                    ( fromRight )
import           Control.Exception              ( IOException
                                                , try
                                                )
import           Data.Either.Combinators        ( mapLeft )

import           Parse.Grammar                  ( parse )
import           AST.Source
import           Path                           ( computeRootPath )
import Explain.Meta
import Error.Error
import Explain.Reason



-- newtype ASTError = ASTNotFound FilePath
--               deriving(Eq, Show)

-- TODO: Write an integration test with real files ?
-- Move that to Main and rename buildASTTable' to buildASTTable
-- Then use the scoped name in Main in order to partially apply it.
buildASTTable :: FilePath -> IO (Either InferError Table)
buildASTTable path =
  let rootPath = computeRootPath path
  in  buildASTTable' readFile path Nothing rootPath path


buildASTTable'
  :: (FilePath -> IO String)
  -> FilePath
  -> Maybe Import
  -> FilePath
  -> FilePath
  -> IO (Either InferError Table)
buildASTTable' rf parentPath imp rootPath entrypoint = do
  s <- try $ rf entrypoint :: IO (Either IOException String)
  let reason = case imp of
        Just imp' -> Reason (WrongImport imp') parentPath (getArea imp')
        Nothing   -> NoReason

      source = either (const $ Left (InferError (ImportNotFound entrypoint "-") reason)) Right s
      ast            = source >>= buildAST entrypoint
      importPaths    = importPathsFromAST rootPath ast
      generatedTable = uncurry M.singleton . (entrypoint, ) <$> ast
  childTables <- mapM (buildImport rf parentPath rootPath) importPaths
  return $ foldr (liftM2 M.union) generatedTable childTables


buildImport :: (FilePath -> IO String) -> FilePath -> FilePath -> (Import, FilePath) -> IO (Either InferError Table)
buildImport rf parentPath rootPath (imp, fp) = buildASTTable' rf parentPath (Just imp) rootPath fp

importPathsFromAST :: FilePath -> Either e AST -> [(Import, FilePath)]
importPathsFromAST rootPath ast = fromRight
  []
  ((mapSnd ((rootPath ++) . (++ ".mad")) . getImportPath <$>) . aimports <$> ast)

getImportPath :: Import -> (Import, FilePath)
getImportPath imp@(Meta _ _ (NamedImport   _ p)) = (imp, p)
getImportPath imp@(Meta _ _ (DefaultImport _ p)) = (imp, p)


mapSnd f (a, b) = (a, f b)



findAST :: Table -> FilePath -> Either InferError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ InferError (ImportNotFound path "") NoReason


buildAST :: FilePath -> String -> Either InferError AST
buildAST path code = mapLeft (\message -> InferError (GrammarError path message) NoReason) $ parse code >>= setPath
 where
  setPath :: AST -> Either e AST
  setPath a = return a { apath = Just path }
