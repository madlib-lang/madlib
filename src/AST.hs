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
  in  buildASTTable' readFile rootPath path


buildASTTable'
  :: (FilePath -> IO String)
  -> FilePath
  -> FilePath
  -> IO (Either InferError Table)
buildASTTable' rf rootPath entrypoint = do
  s <- try $ rf entrypoint :: IO (Either IOException String)
  let source = either (const $ Left (InferError (ImportNotFound entrypoint "-") NoReason)) Right s
      ast            = source >>= buildAST entrypoint
      importPaths    = importPathsFromAST rootPath ast
      generatedTable = uncurry M.singleton . (entrypoint, ) <$> ast
  childTables <- mapM (buildASTTable' rf rootPath) importPaths
  return $ foldr (liftM2 M.union) generatedTable childTables


importPathsFromAST :: FilePath -> Either e AST -> [FilePath]
importPathsFromAST rootPath ast = fromRight
  []
  (((rootPath ++) . (++ ".mad") . getImportPath <$>) . aimports <$> ast)

getImportPath :: Import -> FilePath
getImportPath (Meta _ _ (NamedImport   _ p)) = p
getImportPath (Meta _ _ (DefaultImport _ p)) = p


findAST :: Table -> FilePath -> Either InferError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ InferError (ImportNotFound path "") NoReason


buildAST :: FilePath -> String -> Either InferError AST
buildAST path code = mapLeft (\message -> InferError (GrammarError path message) NoReason) $ parse code >>= setPath
 where
  setPath :: AST -> Either e AST
  setPath a = return a { apath = Just path }
