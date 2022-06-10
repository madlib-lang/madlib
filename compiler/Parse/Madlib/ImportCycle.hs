{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Parse.Madlib.ImportCycle where

import           AST.Source
import qualified Rock
import qualified Driver.Query                           as Query
import           Error.Error
import Error.Context
import Control.Monad


detectCycle :: [FilePath] -> FilePath -> Rock.Task Query.Query (Maybe CompilationError)
detectCycle found path = do
  AST { aimports, apath } <- Rock.fetch $ Query.ParsedAST path
  case apath of
    Just astPath ->
      foldM (processImport astPath (found ++ [astPath])) Nothing aimports

    _ ->
      undefined


processImport :: FilePath -> [FilePath] -> Maybe CompilationError -> Import -> Rock.Task Query.Query (Maybe CompilationError)
processImport originAstPath importChain err imp = do
  let importPath = getImportAbsolutePath imp
  let importArea = getArea imp
  case err of
    Just err' ->
      return $ Just err'

    Nothing ->
      if importPath `elem` importChain then
        return $ Just $ CompilationError (ImportCycle $ importChain ++ [importPath]) (Context originAstPath importArea [])
      else
        detectCycle importChain importPath


