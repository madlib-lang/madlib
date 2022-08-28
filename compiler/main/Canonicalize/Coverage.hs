{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Canonicalize.Coverage where

import Canonicalize.CanonicalM
import AST.Canonical
import Canonicalize.Env (ImportType(NamespaceImport))
import qualified Rock
import Driver.Query
import Explain.Location
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Run.Options
import Canonicalize.Coverable
import Control.Monad.State



addTrackers :: Options -> AST -> CanonicalM AST
addTrackers _ AST{ apath = Nothing } = undefined
addTrackers options ast@AST{ apath = Just path } = do
  let isPrelude = "prelude/__internal__" `isInfixOf` path
  let isPackage = "madlib_modules" `isInfixOf` path
  coverageModulePath <- Rock.fetch $ AbsolutePreludePath "Coverage"
  processModulePath  <- Rock.fetch $ AbsolutePreludePath "Process"

  -- We skip it for prelude and the coverage module itself
  if coverageModulePath /= path && not isPrelude && not isPackage then do
    updatedImports   <- addImport coverageModuleName "Coverage" coverageModulePath $ aimports ast
    updatedImports'  <- addImport processModuleName "Process" processModulePath updatedImports
    updatedExps      <- mapM (addTrackersToExp options path) $ aexps ast
    updatedInstances <- forM (ainstances ast) $ \(Canonical area (Instance n ps p methods)) -> do
      methods' <- mapM (addTrackersToExp options path) methods
      return $ Canonical area (Instance n ps p methods')
    trackers         <- generateTrackerFunctions
    return ast { aexps = trackers ++ updatedExps, ainstances = updatedInstances, aimports = updatedImports' }
  else
    return ast


generateTrackerFunctions :: CanonicalM [Exp]
generateTrackerFunctions = do
  coverableInfo <- gets coverableInfo
  forM coverableInfo $ \case
    (Line line astPath) -> do
      return $ Canonical emptyArea (Assignment (makeLineTrackerName line) (Canonical emptyArea (App (Canonical emptyArea (App lineTrackerRef (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) True)))

    (Function line name astPath) -> do
      return $ Canonical emptyArea (Assignment (makeFunctionTrackerName line name) (Canonical emptyArea (App (Canonical emptyArea (App (Canonical emptyArea (App functionTrackerRef (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) False)) (Canonical emptyArea (LStr name)) True)))

    _ ->
      -- Not implemented
      undefined


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

functionTrackerRef :: Exp
functionTrackerRef = Canonical emptyArea (Access reporterRef (Canonical emptyArea (Var ".functionTracker")))


generateReportRef :: Exp
generateReportRef = Canonical emptyArea (Access reporterRef (Canonical emptyArea (Var ".generateReport")))


reportSetupRef :: Exp
reportSetupRef =
  Canonical emptyArea (App onExitRef (Canonical emptyArea (Abs (Canonical emptyArea "_") [Canonical emptyArea (App generateReportRef (Canonical emptyArea LUnit) True)])) True)


makeLineTrackerName :: Int -> String
makeLineTrackerName line =
  "__lineTracker_" <> show line <> "__"


makeFunctionTrackerName :: Int -> String -> String
makeFunctionTrackerName line name =
  "__functionTracker_" <> show line <> "_" <> name <> "__"


makeFunctionTracker :: FilePath -> Int -> String -> CanonicalM Exp
makeFunctionTracker astPath functionLine functionName = do
  pushCoverable (Function { cname = functionName, cline = functionLine, castpath = astPath })
  return $ Canonical emptyArea (App (Canonical emptyArea (Access (Canonical emptyArea (Var $ makeFunctionTrackerName functionLine functionName)) (Canonical emptyArea (Var ".increment")))) (Canonical emptyArea LUnit) True)


addLineTracker :: FilePath -> Exp -> CanonicalM Exp
addLineTracker astPath exp = do
  let area = getArea exp
  let line = getLineFromStart area
  isAlreadyTracked <- isLineTracked line
  if isAlreadyTracked || line == 0 then
    return exp
  else do
    pushCoverable (Line { cline = line, castpath = astPath })
    return $ Canonical area (Do [Canonical emptyArea (App (Canonical emptyArea (Access (Canonical emptyArea (Var $ makeLineTrackerName line)) (Canonical emptyArea (Var ".increment")))) (Canonical emptyArea LUnit) True), exp])


addLineTrackerForLine :: FilePath -> Int -> Exp -> CanonicalM Exp
addLineTrackerForLine astPath line exp = do
  let area = getArea exp
  isAlreadyTracked <- isLineTracked line
  if isAlreadyTracked || line == 0 then
    return exp
  else do
    pushCoverable (Line { cline = line, castpath = astPath })
    return $ Canonical area (Do [Canonical emptyArea (App (Canonical emptyArea (Access (Canonical emptyArea (Var $ makeLineTrackerName line)) (Canonical emptyArea (Var ".increment")))) (Canonical emptyArea LUnit) True), exp])


addTrackerToBody :: FilePath -> Int -> String -> [Exp] -> CanonicalM [Exp]
addTrackerToBody astPath line name body = case body of
  [Canonical area (Abs p body')] -> do
    body'' <- addTrackerToBody astPath line name body'
    return [Canonical area (Abs p body'')]

  body' -> do
    tracker <- makeFunctionTracker astPath line name
    return $ tracker : body'



addTrackersToExp :: Options -> FilePath -> Exp -> CanonicalM Exp
addTrackersToExp options astPath exp = case exp of
  Canonical area (Assignment "main" (Canonical absArea (Abs p body))) | optEntrypoint options == astPath -> do
    let line = getLineFromStart (getArea exp)
    tracker <- makeFunctionTracker astPath line "main"
    body'   <- mapM (addTrackersToExp options astPath) body
    return $ Canonical area (Assignment "main" (Canonical absArea (Abs p (tracker : reportSetupRef : body'))))

  Canonical area (Assignment fnName (Canonical absArea (Abs p body))) -> do
    let line = getLineFromStart (getArea exp)
    body'  <- mapM (addTrackersToExp options astPath) body
    body'' <-
      if line == 0 then
        return body'
      else do
        addTrackerToBody astPath line fnName body'

    return $ Canonical area (Assignment fnName (Canonical absArea (Abs p body'')))

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
    let line = getLineFromStart (getArea exp)
    anonymousFunctionName <- generateAnonymousFunctionName
    body'                 <- mapM (addTrackersToExp options astPath) body
    body''                <- addTrackerToBody astPath line anonymousFunctionName body'
    return $ Canonical area (Abs p body'')

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
    addLineTrackerForLine astPath fieldLine $ Canonical area (Access record field)

  Canonical area (TemplateString exps) -> do
    exps' <- mapM (addTrackersToExp options astPath) exps
    addLineTracker astPath $ Canonical area (TemplateString exps')

  Canonical area (TupleConstructor exps) -> do
    exps' <- mapM (addTrackersToExp options astPath) exps
    return $ Canonical area (TupleConstructor exps')

  Canonical area (ListConstructor lis) -> do
    lis' <- mapM (addTrackersToListItem options astPath) lis
    return $ Canonical area (ListConstructor lis')

  Canonical area (JSExp code) -> do
    addLineTracker astPath $ Canonical area (JSExp code)

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


addTrackersToListItem :: Options -> FilePath -> ListItem -> CanonicalM ListItem
addTrackersToListItem options astPath li = case li of
  Canonical area (ListItem exp) -> do
    exp' <- addTrackersToExp options astPath exp
    return $ Canonical area (ListItem exp')

  Canonical area (ListSpread exp) -> do
    exp' <- addTrackersToExp options astPath exp
    return $ Canonical area (ListSpread exp')


addTrackersToIs :: Options -> FilePath -> Is -> CanonicalM Is
addTrackersToIs options astPath is = case is of
  Canonical area (Is pat exp) -> do
    let patLine = getLineFromStart (getArea pat)
    exp' <- addTrackersToExp options astPath exp
    exp'' <- addLineTrackerForLine astPath patLine exp'
    return $ Canonical area (Is pat exp'')
