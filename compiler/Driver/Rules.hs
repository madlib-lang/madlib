{-# language GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Driver.Rules where

import qualified Rock
import qualified AST.Source                 as Src
import qualified AST.Canonical              as Can
import qualified AST.Solved                 as Slv
import           Error.Error (CompilationError(CompilationError))
import           Data.IORef
import           Parse.Madlib.AST
import           Control.Monad.IO.Class
import           Driver.Query
import           Run.Target
import           Parse.Madlib.TargetMacro





rules :: Rock.GenRules (Rock.Writer Rock.TaskKind Query) Query
rules (Rock.Writer query) = case query of
  File path -> do
    input $ liftIO $ readFile path

  ParsedAST rootPath path -> nonInput $ do
    source <- Rock.fetch $ File path
    case buildAST path source of
      Right ast -> do
        ast' <- liftIO $ computeAbsoluteImportPaths rootPath ast
        return ast'

      Left err ->
        return Src.AST {}

  CanonicalizedAST path -> nonInput $ do
    sourceAst <- Rock.fetch $ ParsedAST "" path

    undefined

  _ ->
    undefined



input :: (Functor f) => f a -> f (a, Rock.TaskKind)
input = fmap (, Rock.Input)


nonInput :: (Functor f) => f a -> f (a, Rock.TaskKind)
nonInput = fmap (, Rock.NonInput)


ignoreTaskKind :: Rock.GenRules (Rock.Writer Rock.TaskKind f) f -> Rock.Rules f
ignoreTaskKind rs key = fst <$> rs (Rock.Writer key)


runQuery :: Query a -> IO a
runQuery query = do
  memoVar <- newIORef mempty
  let task = Rock.fetch query
  Rock.runTask (Rock.memoise memoVar (ignoreTaskKind rules)) task



-- TODO: move target resolution
parse :: Target -> FilePath -> FilePath -> IO Src.AST
parse target rootPath path = do
  ast <- runQuery $ ParsedAST rootPath path
  return $ resolveMacros target ast
