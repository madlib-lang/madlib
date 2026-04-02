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
import Control.Monad.IO.Class (liftIO)


detectCycle :: [FilePath] -> FilePath -> Rock.Task Query.Query (Maybe CompilationError)
detectCycle found path = detectCycleStep False found path

detectCycleStep :: Bool -> [FilePath] -> FilePath -> Rock.Task Query.Query (Maybe CompilationError)
detectCycleStep finalStep found path = do
  AST { aimports, apath } <- Rock.fetch $ Query.ParsedAST path
  case apath of
    Just astPath ->
      foldM (processImport finalStep astPath (found ++ [astPath])) Nothing aimports

    _ ->
      return Nothing


processImport :: Bool -> FilePath -> [FilePath] -> Maybe CompilationError -> Import -> Rock.Task Query.Query (Maybe CompilationError)
processImport finalStep originAstPath importChain err imp = do
  let importPath = getImportAbsolutePath imp
  let importArea = getArea imp
  case err of
    Nothing ->
      if importPath `elem` importChain then
        if finalStep then
          -- Second time we see a cycle: report here. Use importChain which already
          -- contains originAstPath (appended in detectCycleStep), giving the full path.
          return $ Just $ CompilationError (ImportCycle importChain) (Context originAstPath importArea)
        else
          -- First time: recurse once more into the repeated node so the error
          -- is reported from within it (gives the full cycle representation).
          -- Append importPath to chain so the next pass sees it.
          detectCycleStep True importChain importPath
      else
        detectCycleStep finalStep importChain importPath

    Just err' ->
      return $ Just err'



