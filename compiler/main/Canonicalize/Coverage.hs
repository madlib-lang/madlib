{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Canonicalize.Coverage where

import Canonicalize.CanonicalM
import AST.Canonical
import Canonicalize.Env (ImportType(NamespaceImport))
import qualified Rock
import Driver.Query
import Explain.Location
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Text.Show.Pretty
import Run.Options
import Canonicalize.Coverable


addTrackers :: Options -> AST -> CanonicalM AST
addTrackers _ AST{ apath = Nothing } = undefined
addTrackers options ast@AST{ apath = Just path } = do
  let isPrelude = "prelude/__internal__" `isInfixOf` path
  coverageModulePath <- Rock.fetch $ AbsolutePreludePath "Coverage"
  processModulePath  <- Rock.fetch $ AbsolutePreludePath "Process"

  -- We skip it for prelude and the coverage module itself
  if coverageModulePath /= path && not isPrelude then do
    updatedImports  <- addImport coverageModuleName "Coverage" coverageModulePath $ aimports ast
    updatedImports' <- addImport processModuleName "Process" processModulePath updatedImports
    updatedExps     <- mapM (addTrackersToExp options path) $ aexps ast
    return ast { aexps = updatedExps, aimports = updatedImports' }
  else
    return ast


coverageModuleName :: String
coverageModuleName = "__Coverage__"

processModuleName :: String
processModuleName = "__Process__"


addImport :: String -> String -> String -> [Import] -> CanonicalM [Import]
addImport name relPath absPath imports = do
  return (Canonical emptyArea (DefaultImport (Canonical emptyArea name) relPath absPath) : imports)

reporterRef :: Exp
reporterRef = Canonical emptyArea (Access (Canonical emptyArea (Var coverageModuleName)) (Canonical emptyArea (Var ".Reporter")))

onExitRef :: Exp
onExitRef = Canonical emptyArea (Access (Canonical emptyArea (Var processModuleName)) (Canonical emptyArea (Var ".onExit")))

lineTrackerRef :: Exp
lineTrackerRef = Canonical emptyArea (Access reporterRef (Canonical emptyArea (Var ".lineTracker")))

writeLcovFileRef :: Exp
writeLcovFileRef = Canonical emptyArea (Access reporterRef (Canonical emptyArea (Var ".writeLcovFile")))

addLineTracker :: FilePath -> Exp -> CanonicalM Exp
addLineTracker astPath exp = do
  let area = getArea exp
  let line = getLineFromStart area
  isAlreadyTracked <- isLineTracked line
  if isAlreadyTracked then
    return exp
  else do
    pushCoverable (Line { cline = line, castpath = astPath })
    return $ Canonical area (App (Canonical area (App (Canonical area (App lineTrackerRef exp False)) (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) True)


reportSetupRef :: Exp
reportSetupRef =
  Canonical emptyArea (App onExitRef (Canonical emptyArea (Abs (Canonical emptyArea "_") [Canonical emptyArea (App writeLcovFileRef (Canonical emptyArea LUnit) True)])) True)


addTrackersToExp :: Options -> FilePath -> Exp -> CanonicalM Exp
addTrackersToExp options astPath exp = case exp of
  Canonical area (Assignment "main" (Canonical absArea (Abs p body))) | optEntrypoint options == astPath -> do
    -- TODO: fetch all coverable info and generate Madlib exps for them
    body' <- mapM (addTrackersToExp options astPath) body
    return $ Canonical area (Assignment "main" (Canonical absArea (Abs p (reportSetupRef : body'))))

  Canonical _ (LNum _) ->
    addLineTracker astPath exp

  Canonical area (Abs p body) -> do
    body' <- mapM (addTrackersToExp options astPath) body
    return $ Canonical area (Abs p body')

  Canonical area (TypedExp e ty sc) -> do
    e' <- addTrackersToExp options astPath e
    return $ Canonical area (TypedExp e' ty sc)

  Canonical area (Assignment n e) -> do
    e' <- addTrackersToExp options astPath e
    return $ Canonical area (Assignment n e')

  or ->
    return or
