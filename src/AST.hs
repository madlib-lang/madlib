{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
module AST
  ( ASTTable(..)
  , ASTError(..)
  , buildAST
  , buildASTTable
  , buildASTTable'
  , findAST
  )
where

import qualified Data.Map                      as M
import           Control.Monad                  ( liftM2 )
import           Data.Either                    ( fromRight )
import           Control.Exception              ( IOException
                                                , try
                                                )
import           Data.Either.Combinators        ( mapLeft )

import           Parse.Grammar                  ( parse )
import           AST.AST
import           Path                           ( computeRootPath )


type ASTTable = M.Map FilePath AST

data ASTError = ImportNotFound FilePath (Maybe AST)
              | GrammarError FilePath String
              | ASTNotFound FilePath
              deriving(Eq, Show)

-- TODO: Write an integration test with real files ?
buildASTTable :: FilePath -> IO (Either ASTError ASTTable)
buildASTTable path =
  let rootPath = computeRootPath path
  in  buildASTTable' readFile Nothing rootPath path


buildASTTable'
  :: (FilePath -> IO String)
  -> Maybe AST
  -> FilePath
  -> FilePath
  -> IO (Either ASTError ASTTable)
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
importPathsFromAST rootPath ast =
  fromRight [] (((rootPath ++) . (++ ".mad") . ipath <$>) . aimports <$> ast)


findAST :: ASTTable -> FilePath -> Either ASTError AST
findAST table path = case M.lookup path table of
  Just x  -> return x
  Nothing -> Left $ ASTNotFound path


buildAST :: FilePath -> String -> Either ASTError AST
buildAST path code = mapLeft (GrammarError path) $ parse code >>= setPath
 where
  setPath :: AST -> Either e AST
  setPath a = return a { apath = Just path }
