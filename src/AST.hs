{-# LANGUAGE TupleSections   #-}
module AST
  ( ASTTable(..)
  , FileReader
  , readFile
  , buildAST
  , buildASTTable
  )
where

import           Grammar
import           Prelude                 hiding ( readFile )
import qualified Prelude                       as P
import qualified Data.Map                      as M
import           Control.Monad                  ( liftM2
                                                , liftM3
                                                )
import           Data.Either                    ( fromRight )

type ASTTable = M.Map FilePath AST

class Monad m => FileReader m where
  readFile :: FilePath -> m String

instance FileReader IO where
  readFile = P.readFile

buildASTTable :: FileReader m => FilePath -> m (Either String ASTTable)
buildASTTable entrypoint = do
  source <- readFile entrypoint
  let ast            = buildAST entrypoint source
      imports        = aimports <$> ast
      importPathsE   = (("fixtures/" ++) . (++ ".mad") . ipath <$>) <$> imports
      importPaths    = fromRight [] importPathsE
      generatedTable = uncurry M.singleton . (entrypoint, ) <$> ast
  childTables <- mapM buildASTTable importPaths
  return $ foldl (liftM2 M.union) generatedTable childTables

buildAST :: Path -> String -> Either String AST
buildAST path code = parse code >>= (\a -> return a { apath = Just path })
