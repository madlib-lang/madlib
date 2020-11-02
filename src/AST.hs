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



data ASTError = ImportNotFound FilePath (Maybe AST)
              | GrammarError FilePath String
              | ASTNotFound FilePath
              deriving(Eq, Show)

-- TODO: Write an integration test with real files ?
-- Move that to Main and rename buildASTTable' to buildASTTable
-- Then use the scoped name in Main in order to partially apply it.
buildASTTable :: FilePath -> IO (Either ASTError Table)
buildASTTable path =
  let rootPath = computeRootPath path
  in  buildASTTable' readFile Nothing rootPath path


buildASTTable'
  :: (FilePath -> IO String)
  -> Maybe AST
  -> FilePath
  -> FilePath
  -> IO (Either ASTError Table)
buildASTTable' rf parent rootPath entrypoint = do
  s <- try $ rf entrypoint :: IO (Either IOException String)
  let source = either (const $ Left $ ImportNotFound entrypoint parent) Right s
      ast            = source >>= buildAST entrypoint
      importPaths    = importPathsFromAST rootPath ast
      generatedTable = uncurry M.singleton . (entrypoint, ) <$> ast
      nextParent     = either (const Nothing) Just ast
  childTables <- mapM (buildASTTable' rf nextParent rootPath) importPaths
  return $ foldr (liftM2 M.union) generatedTable childTables


importPathsFromAST :: FilePath -> Either e AST -> [FilePath]
importPathsFromAST rootPath ast = fromRight
  []
  (((rootPath ++) . (++ ".mad") . getImportPath <$>) . aimports <$> ast)

getImportPath :: Import -> FilePath
getImportPath (NamedImport   _ p) = p
getImportPath (DefaultImport _ p) = p


findAST :: Table -> FilePath -> Either ASTError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ ASTNotFound path


buildAST :: FilePath -> String -> Either ASTError AST
buildAST path code = mapLeft (GrammarError path) $ parse code >>= setPath
 where
  setPath :: AST -> Either e AST
  setPath a = return a { apath = Just path }
