{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Canonicalize.Coverage where

import Canonicalize.CanonicalM
import AST.Canonical
import qualified Rock
import Driver.Query
import Explain.Location
import Data.List (isInfixOf, isSuffixOf)
import Run.Options
import Canonicalize.Coverable
import Control.Monad.State


trackingModuleName :: String
trackingModuleName = "__CoverageTracking__"


coverageModuleName :: String
coverageModuleName = "__Coverage__"


processModuleName :: String
processModuleName = "__Process__"


addTrackers :: Options -> AST -> CanonicalM AST
addTrackers _ ast@AST{ apath = Nothing } = return ast
addTrackers options ast@AST{ apath = Just path } = do
  let isPackage    = "madlib_modules" `isInfixOf` path
  let isTest       = ".spec.mad" `isSuffixOf` path
  let isTestRunner = "/Test.mad" `isSuffixOf` path
  let isBuiltins   = "/__BUILTINS__.mad" `isSuffixOf` path
  let isTestMain   = "/__TestMain__.mad" `isSuffixOf` path
  trackingModulePath <- Rock.fetch $ AbsolutePreludePath "__CoverageTracking__"
  coverageModulePath <- Rock.fetch $ AbsolutePreludePath "__Coverage__"
  processModulePath  <- Rock.fetch $ AbsolutePreludePath "Process"

  -- __CoverageTracking__ has zero imports, so any module can import it without cycles.
  let isCoverageModule = path == trackingModulePath || path == coverageModulePath
  let willInstrument = not isCoverageModule && not isPackage && not isTest && not isTestRunner && not isBuiltins && not isTestMain

  let isEntrypoint = optEntrypoint options == path

  if willInstrument then do
    -- All instrumented modules import __CoverageTracking__ for tracking
    updatedImports <- addImport trackingModuleName "__CoverageTracking__" trackingModulePath $ aimports ast

    -- The entry point also imports __Coverage__ (for report generation) and Process (for onExit)
    updatedImports' <-
      if isEntrypoint then do
        i1 <- addImport coverageModuleName "__Coverage__" coverageModulePath updatedImports
        addImport processModuleName "Process" processModulePath i1
      else
        return updatedImports

    updatedExps      <- mapM (addTrackersToExp options path) $ aexps ast
    updatedInstances <- forM (ainstances ast) $ \(Canonical area (Instance n ps p methods)) -> do
      if n == "Eq" || n == "Show" then
        return $ Canonical area (Instance n ps p methods)
      else do
        methods' <- mapM (addTrackersToExp options path) methods
        return $ Canonical area (Instance n ps p methods')
    trackers         <- generateTrackerFunctions
    return ast { aexps = trackers ++ updatedExps, ainstances = updatedInstances, aimports = updatedImports' }

  -- Entry point that's not instrumented (e.g. __TestMain__) still needs report setup
  else if isEntrypoint then do
    updatedImports <- addImport coverageModuleName "__Coverage__" coverageModulePath $ aimports ast
    updatedImports' <- addImport processModuleName "Process" processModulePath updatedImports
    let updatedExps = addReportSetupToMain options path $ aexps ast
    return ast { aexps = updatedExps, aimports = updatedImports' }

  else
    return ast


generateTrackerFunctions :: CanonicalM [Exp]
generateTrackerFunctions = do
  coverableInfo <- gets coverableInfo
  forM coverableInfo $ \case
    Line line astPath ->
      return $ Canonical emptyArea (Assignment (makeLineTrackerName line) (Canonical emptyArea (Access (Canonical emptyArea (App (Canonical emptyArea (App lineTrackerRef (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) True)) (Canonical emptyArea (Var ".increment")))))

    Function line name astPath ->
      return $ Canonical emptyArea (Assignment (makeFunctionTrackerName line name) (Canonical emptyArea (Access (Canonical emptyArea (App (Canonical emptyArea (App (Canonical emptyArea (App functionTrackerRef (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) False)) (Canonical emptyArea (LStr name)) True)) (Canonical emptyArea (Var ".increment")))))

    Branch line blockIndex branchIndex astPath ->
      return $ Canonical emptyArea (Assignment (makeBranchTrackerName line blockIndex branchIndex) (Canonical emptyArea (Access (Canonical emptyArea (App (Canonical emptyArea (App (Canonical emptyArea (App (Canonical emptyArea (App branchTrackerRef (Canonical emptyArea (LStr astPath)) False)) (Canonical emptyArea (LNum (show line))) False)) (Canonical emptyArea (LNum (show blockIndex))) False)) (Canonical emptyArea (LNum (show branchIndex))) True)) (Canonical emptyArea (Var ".increment")))))


addImport :: String -> String -> String -> [Import] -> CanonicalM [Import]
addImport name relPath absPath imports = do
  return (Canonical emptyArea (DefaultImport (Canonical emptyArea name) relPath absPath) : imports)


onExitRef :: Exp
onExitRef = Canonical emptyArea (Access (Canonical emptyArea (Var processModuleName)) (Canonical emptyArea (Var ".onExit")))


-- Tracking refs point to __CoverageTracking__.Tracking (zero-import module)
trackingRef :: Exp
trackingRef = Canonical emptyArea (Access (Canonical emptyArea (Var trackingModuleName)) (Canonical emptyArea (Var ".Tracking")))


lineTrackerRef :: Exp
lineTrackerRef = Canonical emptyArea (Access trackingRef (Canonical emptyArea (Var ".trackLine")))


functionTrackerRef :: Exp
functionTrackerRef = Canonical emptyArea (Access trackingRef (Canonical emptyArea (Var ".trackFunction")))


branchTrackerRef :: Exp
branchTrackerRef = Canonical emptyArea (Access trackingRef (Canonical emptyArea (Var ".trackBranch")))


-- Report generation ref points to __Coverage__ (only imported in entry point)
generateReportRef :: Exp
generateReportRef = Canonical emptyArea (Access (Canonical emptyArea (Var coverageModuleName)) (Canonical emptyArea (Var ".generateReport")))


reportSetupRef :: Exp
reportSetupRef =
  Canonical emptyArea (App onExitRef (Canonical emptyArea (Abs (Canonical emptyArea "_") [Canonical emptyArea (App generateReportRef (Canonical emptyArea LUnit) True)])) True)


-- | Add report setup to the main function without full instrumentation.
-- Used for entry points like __TestMain__ that shouldn't be instrumented.
addReportSetupToMain :: Options -> FilePath -> [Exp] -> [Exp]
addReportSetupToMain options path = map go
  where
    go (Canonical area (TypedExp (Canonical assignArea (Assignment "main" (Canonical absArea (Abs p body)))) ty sc)) | optEntrypoint options == path =
      Canonical area (TypedExp (Canonical assignArea (Assignment "main" (Canonical absArea (Abs p (reportSetupRef : body))))) ty sc)
    go (Canonical area (Assignment "main" (Canonical absArea (Abs p body)))) | optEntrypoint options == path =
      Canonical area (Assignment "main" (Canonical absArea (Abs p (reportSetupRef : body))))
    go e = e


makeLineTrackerName :: Int -> String
makeLineTrackerName line =
  "__lineTracker_" <> show line <> "__"


makeFunctionTrackerName :: Int -> String -> String
makeFunctionTrackerName line name =
  "__functionTracker_" <> show line <> "_" <> name <> "__"


makeBranchTrackerName :: Int -> Int -> Int -> String
makeBranchTrackerName line blockIndex branchIndex =
  "__branchTracker_" <> show line <> "_" <> show blockIndex <> "_" <> show branchIndex <> "__"


makeFunctionTracker :: FilePath -> Int -> String -> CanonicalM Exp
makeFunctionTracker astPath functionLine functionName = do
  pushCoverable (Function { cname = functionName, cline = functionLine, castpath = astPath })
  return $ Canonical emptyArea (App (Canonical emptyArea (Var $ makeFunctionTrackerName functionLine functionName)) (Canonical emptyArea LUnit) True)


-- | Create a line tracker call expression for insertion as a standalone statement.
makeLineTrackerCall :: FilePath -> Int -> CanonicalM (Maybe Exp)
makeLineTrackerCall astPath line = do
  isAlreadyTracked <- isLineTracked line
  if isAlreadyTracked || line == 0 then
    return Nothing
  else do
    pushCoverable (Line { cline = line, castpath = astPath })
    return $ Just $ Canonical emptyArea (App (Canonical emptyArea (Var $ makeLineTrackerName line)) (Canonical emptyArea LUnit) True)


-- | Add line tracker calls as standalone statements before each statement in a body.
-- This avoids wrapping expressions in Do blocks (which breaks codegen).
addLineTrackersToBody :: FilePath -> [Exp] -> CanonicalM [Exp]
addLineTrackersToBody _ [] = return []
addLineTrackersToBody astPath (stmt : rest) = do
  let line = getLineFromStart (getArea stmt)
  maybeTracker <- makeLineTrackerCall astPath line
  rest' <- addLineTrackersToBody astPath rest
  case maybeTracker of
    Just tracker -> return $ tracker : stmt : rest'
    Nothing      -> return $ stmt : rest'


addFunctionTrackerToBody :: FilePath -> Int -> String -> [Exp] -> CanonicalM [Exp]
addFunctionTrackerToBody astPath line name body = case body of
  [Canonical _ (JSExp _)] -> do
    return body

  [Canonical area (Abs p body')] -> do
    body'' <- addFunctionTrackerToBody astPath line name body'
    return [Canonical area (Abs p body'')]

  body' -> do
    tracker <- makeFunctionTracker astPath line name
    return $ tracker : body'


addBranchTracker :: FilePath -> Int -> Int -> Int -> Exp -> CanonicalM Exp
addBranchTracker astPath line blockIndex branchIndex exp = do
  pushCoverable Branch { cline = line, cblocknumber = blockIndex, cbranchnumber = branchIndex, castpath = astPath }
  let trackerCall = Canonical emptyArea (App (Canonical emptyArea (Var $ makeBranchTrackerName line blockIndex branchIndex)) (Canonical emptyArea LUnit) True)
  let area = getArea exp
  return $ Canonical area (Do [trackerCall, exp])


updateBody :: [Exp] -> ([Exp] -> CanonicalM [Exp]) -> CanonicalM [Exp]
updateBody body f = case body of
  [Canonical area (Abs p body')] -> do
    body'' <- updateBody body' f
    return [Canonical area (Abs p body'')]

  body' -> do
    f body'



addTrackersToExp :: Options -> FilePath -> Exp -> CanonicalM Exp
addTrackersToExp options astPath exp = case exp of
  Canonical area (Assignment "main" (Canonical absArea (Abs p body))) | optEntrypoint options == astPath -> do
    let line = getLineFromStart (getArea exp)
    tracker <- makeFunctionTracker astPath line "main"
    body'   <- mapM (addTrackersToExp options astPath) body
    body''  <- addLineTrackersToBody astPath body'
    return $ Canonical area (Assignment "main" (Canonical absArea (Abs p (tracker : reportSetupRef : body''))))

  Canonical area (Assignment fnName (Canonical absArea (Abs p body))) -> do
    body'' <- updateBody body $ \body' -> do
      let line = getLineFromStart (getArea exp)
      body''  <- mapM (addTrackersToExp options astPath) body'
      body''' <- addLineTrackersToBody astPath body''
      if line == 0 then
        return body'''
      else do
        addFunctionTrackerToBody astPath line fnName body'''

    return $ Canonical area (Assignment fnName (Canonical absArea (Abs p body'')))

  -- Leaf expressions: no tracking here. Line coverage is handled by
  -- addLineTrackersToBody which inserts tracker calls as statements in function bodies.
  Canonical _ (LNum _) -> return exp
  Canonical _ (LFloat _) -> return exp
  Canonical _ (LStr _) -> return exp
  Canonical _ (LBool _) -> return exp
  Canonical _ (LChar _) -> return exp
  Canonical _ LUnit -> return exp
  Canonical _ (Var _) -> return exp

  Canonical area (Record fields) -> do
    fields' <- mapM (addTrackersToField options astPath) fields
    return $ Canonical area (Record fields')

  Canonical area (Abs p body) -> do
    body'' <- updateBody body $ \body' -> do
      let line = getLineFromStart area
      body''  <- mapM (addTrackersToExp options astPath) body'
      body''' <- addLineTrackersToBody astPath body''
      if line == 0 then
        return body'''
      else do
        anonName <- generateAnonymousFunctionName
        addFunctionTrackerToBody astPath line anonName body'''
    return $ Canonical area (Abs p body'')

  Canonical area (Do exps) -> do
    exps' <- mapM (addTrackersToExp options astPath) exps
    exps'' <- addLineTrackersToBody astPath exps'
    return $ Canonical area (Do exps'')

  Canonical area (App (Canonical _ (App (Canonical _ (Var fnName)) _ _)) _ _)
    | fnName == "&&" || fnName == "||" -> do
      blockIndex <- newBlock
      let line = getLineFromStart area
      (_, exp') <- addTrackersToBooleanExpression options astPath line blockIndex 0 exp
      return exp'

  Canonical area (App f arg final) -> do
    arg' <- addTrackersToExp options astPath arg
    f'   <- addTrackersToExp options astPath f
    return $ Canonical area (App f' arg' final)

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

    blockIndex <- newBlock
    let line = getLineFromStart area

    iss' <- mapM (\(is, branchIndex) -> addTrackersToIs options astPath line blockIndex branchIndex is) (zip iss [0..])
    return $ Canonical area (Where e' iss')

  Canonical area (If cond truthy falsy) -> do
    cond'   <- addTrackersToExp options astPath cond
    truthy' <- addTrackersToExp options astPath truthy
    falsy'  <- addTrackersToExp options astPath falsy

    blockIndex <- newBlock
    let line = getLineFromStart area
    truthy'' <- addBranchTracker astPath line blockIndex 0 truthy'
    falsy''  <- addBranchTracker astPath line blockIndex 1 falsy'

    return $ Canonical area (If cond' truthy'' falsy'')

  Canonical area (While cond body) -> do
    cond' <- addTrackersToExp options astPath cond
    body' <- addTrackersToExp options astPath body
    return $ Canonical area (While cond' body')

  Canonical area (Access record field) -> do
    return $ Canonical area (Access record field)

  Canonical area (TemplateString exps) -> do
    exps' <- mapM (addTrackersToExp options astPath) exps
    return $ Canonical area (TemplateString exps')

  Canonical area (TupleConstructor exps) -> do
    exps' <- mapM (addTrackersToExp options astPath) exps
    return $ Canonical area (TupleConstructor exps')

  Canonical area (ListConstructor lis) -> do
    lis' <- mapM (addTrackersToListItem options astPath) lis
    return $ Canonical area (ListConstructor lis')

  -- Canonical area (JSExp code) -> do
  --   addLineTracker astPath $ Canonical area (JSExp code)

  or ->
    return or


addTrackersToBooleanExpression :: Options -> FilePath -> Int -> Int -> Int -> Exp -> CanonicalM (Int, Exp)
addTrackersToBooleanExpression options astPath line blockIndex branchIndex exp = case exp of
  Canonical area (App (Canonical innerArea (App (Canonical varArea (Var fnName)) leftArg leftFinal)) rightArg rightFinal) | fnName == "&&" || fnName == "||" -> do
    (lastBranchIndex, leftArg')   <- addTrackersToBooleanExpression options astPath line blockIndex branchIndex leftArg
    (lastBranchIndex', rightArg') <- addTrackersToBooleanExpression options astPath line blockIndex (lastBranchIndex + 1) rightArg
    return (lastBranchIndex', Canonical area (App (Canonical innerArea (App (Canonical varArea (Var fnName)) leftArg' leftFinal)) rightArg' rightFinal))

  _ -> do
    exp' <- addTrackersToExp options astPath exp
    exp'' <- addBranchTracker astPath line blockIndex branchIndex exp'
    return (branchIndex, exp'')


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


addTrackersToIs :: Options -> FilePath -> Int -> Int -> Int -> Is -> CanonicalM Is
addTrackersToIs options astPath line blockIndex branchIndex is = case is of
  Canonical area (Is pat exp) -> do
    exp'   <- addTrackersToExp options astPath exp
    exp''  <- addBranchTracker astPath line blockIndex branchIndex exp'
    return $ Canonical area (Is pat exp'')
