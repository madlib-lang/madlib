{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.State



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
    -- TODO: only do this for main module
    injectedMain    <- mapM (injectMain options path) updatedExps
    trackers        <- generateTrackerFunctions
    return ast { aexps = trackers ++ injectedMain, aimports = updatedImports' }
  else
    return ast


generateTrackerFunctions :: CanonicalM [Exp]
generateTrackerFunctions = do
  coverableInfo <- gets coverableInfo
  forM coverableInfo $ \(Line line astPath) -> do
    return $ Canonical emptyArea (Assignment (makeLineTrackerFunctionName line) (Canonical emptyArea (App (Canonical emptyArea (App lineTrackerRef (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) True)))


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

reportSetupRef :: Exp
reportSetupRef =
  Canonical emptyArea (App onExitRef (Canonical emptyArea (Abs (Canonical emptyArea "_") [Canonical emptyArea (App writeLcovFileRef (Canonical emptyArea LUnit) True)])) True)

registerCoverableLineRef :: Exp
registerCoverableLineRef = Canonical emptyArea (Access reporterRef (Canonical emptyArea (Var ".registerCoverableLine")))

makeLineTrackerFunctionName :: Int -> String
makeLineTrackerFunctionName line =
  "__trackLine_" <> show line <> "__"

addLineTracker :: FilePath -> Exp -> CanonicalM Exp
addLineTracker astPath exp = do
  let area = getArea exp
  let line = getLineFromStart area
  isAlreadyTracked <- isLineTracked line
  if isAlreadyTracked || line == 0 then
    return exp
  else do
    pushCoverable (Line { cline = line, castpath = astPath })
    return $ Canonical area (App (Canonical emptyArea (Var $ makeLineTrackerFunctionName line)) exp True)

addLineTrackerForLine :: FilePath -> Exp -> Int -> CanonicalM Exp
addLineTrackerForLine astPath exp line = do
  let area = getArea exp
  isAlreadyTracked <- isLineTracked line
  if isAlreadyTracked || line == 0 then
    return exp
  else do
    pushCoverable (Line { cline = line, castpath = astPath })
    return $ Canonical area (App (Canonical emptyArea (Var $ makeLineTrackerFunctionName line)) exp True)


-- buildRegisterCoverableExps :: Coverable -> Exp
-- buildRegisterCoverableExps coverable = case coverable of
--   Line { cline, castpath } ->
--     Canonical emptyArea (App (Canonical emptyArea (App registerCoverableLineRef (Canonical emptyArea (LNum (show cline))) False)) (Canonical emptyArea (LStr castpath)) True)

--   _ ->
--     undefined


-- TODO: possible move back to addTrackers function
injectMain :: Options -> FilePath -> Exp -> CanonicalM Exp
injectMain options astPath exp = case exp of
  Canonical area (Assignment "main" (Canonical absArea (Abs p body))) | optEntrypoint options == astPath -> do
    return $ Canonical area (Assignment "main" (Canonical absArea (Abs p (reportSetupRef : body))))

  Canonical area (Abs p body) -> do
    body' <- mapM (injectMain options astPath) body
    return $ Canonical area (Abs p body')

  Canonical area (TypedExp e ty sc) -> do
    e' <- injectMain options astPath e
    return $ Canonical area (TypedExp e' ty sc)

  Canonical area (Assignment n e) -> do
    e' <- injectMain options astPath e
    return $ Canonical area (Assignment n e')

  Canonical area (Export e) -> do
    e' <- injectMain options astPath e
    return $ Canonical area (Export e')

  or ->
    return or


addTrackersToExp :: Options -> FilePath -> Exp -> CanonicalM Exp
addTrackersToExp options astPath exp = case exp of
  Canonical _ (LNum _) ->
    addLineTracker astPath exp

  Canonical _ (LFloat _) ->
    addLineTracker astPath exp

  Canonical _ (LStr _) ->
    addLineTracker astPath exp

  Canonical _ (LBool _) ->
    addLineTracker astPath exp

  Canonical _ (LChar _) ->
    addLineTracker astPath exp

  Canonical _ LUnit ->
    addLineTracker astPath exp

  Canonical _ (Var _) ->
    addLineTracker astPath exp

  Canonical area (Record fields) -> do
    fields' <- mapM (addTrackersToField options astPath) fields
    return $ Canonical area (Record fields')

  Canonical area (Abs p body) -> do
    body' <- mapM (addTrackersToExp options astPath) body
    return $ Canonical area (Abs p body')

  Canonical area (Do exps) -> do
    exps' <- mapM (addTrackersToExp options astPath) exps
    return $ Canonical area (Do exps')

  Canonical area (App f arg final) -> do
    arg' <- addTrackersToExp options astPath arg
    f'   <- addTrackersToExp options astPath f
    addLineTracker astPath $ Canonical area (App f' arg' final)

  Canonical area (TypedExp e ty sc) -> do
    e' <- addTrackersToExp options astPath e
    return $ Canonical area (TypedExp e' ty sc)

  Canonical area (Assignment n e) -> do
    e' <- addTrackersToExp options astPath e
    return $ Canonical area (Assignment n e')

  Canonical area (Export e) -> do
    e' <- addTrackersToExp options astPath e
    return $ Canonical area (Export e')

  Canonical area (Where e iss) -> do
    e'   <- addTrackersToExp options astPath e
    iss' <- mapM (addTrackersToIs options astPath) iss
    return $ Canonical area (Where e' iss')

  Canonical area (If cond truthy falsy) -> do
    cond'   <- addTrackersToExp options astPath cond
    truthy' <- addTrackersToExp options astPath truthy
    falsy'  <- addTrackersToExp options astPath falsy
    return $ Canonical area (If cond' truthy' falsy')

  Canonical area (Access record field) -> do
    let fieldLine = getLineFromStart (getArea field)
    record'  <- addTrackersToExp options astPath record
    record'' <- addLineTrackerForLine astPath record' fieldLine
    return $ Canonical area (Access record'' field)

  or ->
    return or


addTrackersToField :: Options -> FilePath -> Field -> CanonicalM Field
addTrackersToField options astPath field = case field of
  Canonical area (Field (name, exp)) -> do
    exp' <- addTrackersToExp options astPath exp
    return $ Canonical area (Field (name, exp'))

  Canonical area (FieldSpread exp) -> do
    exp' <- addTrackersToExp options astPath exp
    return $ Canonical area (FieldSpread exp')

addTrackersToIs :: Options -> FilePath -> Is -> CanonicalM Is
addTrackersToIs options astPath is = case is of
  Canonical area (Is pat exp) -> do
    exp' <- addTrackersToExp options astPath exp
    return $ Canonical area (Is pat exp')
