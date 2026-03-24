{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Infer.Scope where

import           Infer.Env
import           AST.Solved
import           Infer.Infer
import           Infer.Type
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Maybe
import           Control.Monad
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import           Explain.Location
import qualified Data.List as List
import Infer.Unify


-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Names currently in scope.
newtype InScope = InScope (S.Set String)
  deriving (Semigroup, Monoid)

-- | A collected variable access: (name, the expression that contains it).
newtype Accesses = Accesses [(String, Exp)]
  deriving (Semigroup, Monoid)

-- | Maps each top-level name to the list of names it depends on.
-- Example: fn = (x) => x + var1 + var2  →  "fn" -> [("var1",…), ("var2",…)]
newtype Dependencies = Dependencies (M.Map String [(String, Exp)])

-- Helpers

memberScope :: String -> InScope -> Bool
memberScope n (InScope s) = S.member n s

insertScope :: String -> InScope -> InScope
insertScope n (InScope s) = InScope (S.insert n s)

fromListScope :: [String] -> InScope
fromListScope = InScope . S.fromList

setScope :: S.Set String -> InScope
setScope = InScope

unAccesses :: Accesses -> [(String, Exp)]
unAccesses (Accesses xs) = xs

singleAccess :: String -> Exp -> Accesses
singleAccess n e = Accesses [(n, e)]

emptyDeps :: Dependencies
emptyDeps = Dependencies M.empty

lookupDeps :: String -> Dependencies -> Maybe [(String, Exp)]
lookupDeps n (Dependencies m) = M.lookup n m

insertDeps :: String -> [(String, Exp)] -> Dependencies -> Dependencies
insertDeps n xs (Dependencies m) = Dependencies (M.insert n xs m)


-- ---------------------------------------------------------------------------
-- Collection context (invariant across a single top-level expression's pass)
-- ---------------------------------------------------------------------------

data CollectCtx = CollectCtx
  { ctxEnv                 :: Env
  , ctxTopLevelAssignments :: S.Set String
    -- ^ All top-level names in the module (for shadow detection).
  , ctxCurrentTopLevel     :: Maybe String
    -- ^ Name of the top-level expression currently being collected from.
    --   Its own name is excluded from shadow-detection.
  }

-- | Traversal state — changes at each level of recursion.
data CollectState = CollectState
  { csFoundNames  :: [String]
    -- ^ Names that have crossed an Abs boundary, making their recursive use legal.
  , csNameToFind  :: Maybe String
    -- ^ The name we are watching for illegal self-access (set when entering an Assignment).
  , csGlobalScope :: InScope
    -- ^ Names visible at the global (top-level) module scope.
  , csLocalScope  :: InScope
    -- ^ Names bound in local (lambda/do) scope.
  }


-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

checkAST :: Env -> AST -> Infer ()
checkAST env ast = do
  errs <- getErrors
  unless (null errs) $ return ()
  when (null errs) $ do
    let defaultImportNames  = getDefaultImportNames ast
        initialScope        = fromListScope (M.keys $ envVars env)
                           <> fromListScope (M.keys $ envMethods env)
                           <> fromListScope defaultImportNames
        exps                = aexps ast
        methods             = concat $ getInstanceMethods <$> ainstances ast
        topLevelAssignments = getAllTopLevelAssignments ast
        expsFromGlobalScope = getAllExpsFromGlobalScope ast
    checkExps env ast expsFromGlobalScope topLevelAssignments initialScope emptyDeps (methods ++ exps)
    checkConstructors env (fromListScope defaultImportNames) (atypedecls ast)
    mapM_ (verifyMutations env mempty) exps


-- ---------------------------------------------------------------------------
-- Constructor shadowing check
-- ---------------------------------------------------------------------------

-- | We need this extra check for constructors as type checking only
-- verifies what is in the env, but namespaced names are always of the form
-- Namespace.name and will thus always get through.
checkConstructors :: Env -> InScope -> [TypeDecl] -> Infer ()
checkConstructors env scope tds = do
  let adts             = filter isADT tds
      constructors     = concat $ mapMaybe getADTConstructors adts
      constructorNames = (\(Untyped area (Constructor name _ _)) -> (name, area)) <$> constructors
  mapM_
    (\(constructorName, area) ->
      when
        (constructorName `memberScope` scope)
        (pushError $ CompilationError (NameAlreadyDefined constructorName) (Context (envCurrentPath env) area))
    )
    constructorNames


-- ---------------------------------------------------------------------------
-- Top-level expression processing loop
-- ---------------------------------------------------------------------------

checkExps :: Env -> AST -> [(String, Exp)] -> S.Set String -> InScope -> Dependencies -> [Exp] -> Infer ()
checkExps _   _   _       _                   _           _    []       = return ()
checkExps env ast globals topLevelAssignments globalScope deps (e : es) = do
  let globalScope' = extendScopeFromExp globalScope e
  collectedAccesses <- runCollect env topLevelAssignments (getExpName e) globalScope' e

  catchError (verifyScope env globals (unAccesses collectedAccesses) globalScope' deps e) pushError

  let shouldBeTypedOrAbove =
        if isMethod env e then
          []
        else
          let typedNames = fromListScope [ n | exp <- aexps ast, Just n <- [getExpName exp], isTypedExp exp ]
          in  List.filter
                (\(name, exp) ->
                  not (name `memberScope` globalScope') && not (name `memberScope` typedNames) && not (isTypeOrNameExport exp)
                )
                (unAccesses collectedAccesses)

  generateShouldBeTypedOrAboveErrors env shouldBeTypedOrAbove

  let deps' = extendDependencies collectedAccesses deps e
  checkExps env ast globals topLevelAssignments globalScope' deps' es


generateShouldBeTypedOrAboveErrors :: Env -> [(String, Exp)] -> Infer ()
generateShouldBeTypedOrAboveErrors env = foldM_
  (\_ (name, exp) -> pushError
    $ CompilationError (ShouldBeTypedOrAbove name) (Context (envCurrentPath env) (getArea exp))
  )
  ()


-- ---------------------------------------------------------------------------
-- Scope verification
-- ---------------------------------------------------------------------------

verifyScope :: Env -> [(String, Exp)] -> [(String, Exp)] -> InScope -> Dependencies -> Exp -> Infer ()
verifyScope env globals globalAccesses globalScope deps exp =
  if shouldSkip env exp then
    return ()
  else
    foldM_ (verifyOneAccess env globals S.empty globalScope deps exp) () globalAccesses


-- | Verify a single accessed name.
-- Branch 1: already verified or expression has Abs → skip.
-- Branch 2: name is in globalScope → it's a forward reference; check transitively.
-- Branch 3: otherwise → report NotInScope.
verifyOneAccess
  :: Env -> [(String, Exp)] -> S.Set String -> InScope -> Dependencies -> Exp
  -> () -> (String, Exp)
  -> Infer ()
verifyOneAccess env globals verified globalScope deps originExp@(Typed _ originArea _) _ (nameToVerify, spotExp@(Typed qt (Area loc _) _))
  | nameToVerify `S.member` verified = return ()
  | hasAbs spotExp                    = return ()
  | nameToVerify `memberScope` globalScope =
      case lookupDeps nameToVerify deps of
        Just names ->
          case List.find (\(n, _) -> n == nameToVerify) globals of
            Just (_, Typed foreignQt _ _) -> do
              sameType <- catchError (unify (getQualified foreignQt) (getQualified qt) >> return True) (\_ -> return False)
              when sameType $
                foldM_ (verifyOneAccess env globals (verified <> S.singleton nameToVerify) globalScope deps originExp) () names
            _ -> return ()
        Nothing -> return ()
  | otherwise =
      ifNotAlreadyReported env nameToVerify $
        throwError $ CompilationError (NotInScope nameToVerify loc) (Context (envCurrentPath env) originArea)
verifyOneAccess _ _ _ _ _ _ _ _ = return ()


-- | Only report error if it hasn't already been reported as UnboundVariable.
ifNotAlreadyReported :: Env -> String -> Infer () -> Infer ()
ifNotAlreadyReported env name action = do
  currentErrors <- getErrors
  let isReported = any
        (\case
          CompilationError (UnboundVariable n _) (Context fp _) -> n == name && envCurrentPath env == fp
          _ -> False
        )
        currentErrors
  unless isReported action


-- ---------------------------------------------------------------------------
-- Dependency and scope utilities
-- ---------------------------------------------------------------------------

extendScopeFromExp :: InScope -> Exp -> InScope
extendScopeFromExp inScope exp = case getExpName exp of
  Just name -> insertScope name inScope
  Nothing   -> inScope

extendDependencies :: Accesses -> Dependencies -> Exp -> Dependencies
extendDependencies (Accesses accesses) deps exp = case getExpName exp of
  Just name -> insertDeps name accesses deps
  Nothing   -> deps


-- ---------------------------------------------------------------------------
-- shouldSkip and helpers
-- ---------------------------------------------------------------------------

-- | Unwrap TypedExp and Export wrappers to get to the core expression.
peelToCore :: Exp -> Exp
peelToCore e = case e of
  Typed _ _ (TypedExp inner _ _) -> peelToCore inner
  Typed _ _ (Export inner)       -> peelToCore inner
  _                              -> e

-- | True if this top-level expression should be skipped from scope checking.
shouldSkip :: Env -> Exp -> Bool
shouldSkip env e = isMethod env e || case peelToCore e of
  Untyped _ _                    -> True
  Typed _ _ (Assignment _ rhs)   -> hasAbs rhs
  _                              -> False

hasAbs :: Exp -> Bool
hasAbs e = case e of
  Typed _ _ (Abs _ _) -> True
  _                   -> False

isMethod :: Env -> Exp -> Bool
isMethod _ (Untyped _ _)   = False
isMethod env (Typed _ _ e) = case e of
  Var n _        -> M.member n (envMethods env)
  Assignment n _ -> M.member n (envMethods env)
  _              -> False

isMethodName :: Env -> String -> Bool
isMethodName env name = M.member name (envMethods env)


-- ---------------------------------------------------------------------------
-- Helpers used by checkAST / older utility functions
-- ---------------------------------------------------------------------------

findAssignmentByName :: String -> [Exp] -> Maybe Exp
findAssignmentByName _    []       = Nothing
findAssignmentByName name (e : es) = case getExpName e of
  Just found -> if name == found then Just e else findAssignmentByName name es
  Nothing    -> findAssignmentByName name es

getAllTopLevelAssignments :: AST -> S.Set String
getAllTopLevelAssignments ast =
  S.fromList $ mapMaybe getExpName (aexps ast)


-- ---------------------------------------------------------------------------
-- Collection entry point
-- ---------------------------------------------------------------------------

-- | Build the context and initial state, then run 'collect'.
runCollect :: Env -> S.Set String -> Maybe String -> InScope -> Exp -> Infer Accesses
runCollect env topLevel currentName globalScope topExp =
  collect ctx initialState topExp
  where
    ctx = CollectCtx
      { ctxEnv                 = env
      , ctxTopLevelAssignments = topLevel
      , ctxCurrentTopLevel     = currentName
      }
    initialState = CollectState
      { csFoundNames  = []
      , csNameToFind  = Nothing
      , csGlobalScope = globalScope
      , csLocalScope  = mempty
      }


-- ---------------------------------------------------------------------------
-- Concern: shadow detection
-- ---------------------------------------------------------------------------

checkShadow :: CollectCtx -> Area -> String -> Infer ()
checkShadow ctx area name =
  when
    (Just name /= ctxCurrentTopLevel ctx && name `S.member` ctxTopLevelAssignments ctx)
    (pushError $ CompilationError (NameAlreadyDefined name) (Context (envCurrentPath $ ctxEnv ctx) area))


-- ---------------------------------------------------------------------------
-- Concern: recursion detection
-- ---------------------------------------------------------------------------

checkRecursiveAccess :: CollectCtx -> CollectState -> Area -> String -> Infer ()
checkRecursiveAccess ctx cs area name =
  case csNameToFind cs of
    Just n ->
      when
        (n == name && notElem n (csFoundNames cs) && not (isMethodName (ctxEnv ctx) name))
        (throwError $ CompilationError (RecursiveVarAccess name) (Context (envCurrentPath $ ctxEnv ctx) area))
    Nothing -> return ()


-- ---------------------------------------------------------------------------
-- collect: thin dispatcher
-- ---------------------------------------------------------------------------

{-|
  'collect' walks the AST and gathers all variable accesses.
  It also fires two side-effect checks as it goes:
    - 'checkShadow' when an inner name would shadow a top-level name.
    - 'checkRecursiveAccess' when a Var is reached that might be an illegal
      self-reference.

  The invariant context (env, top-level assignments, current name) lives in
  'CollectCtx'; the per-call traversal state lives in 'CollectState'.
-}
collect :: CollectCtx -> CollectState -> Exp -> Infer Accesses
collect ctx cs solvedExp = case solvedExp of
  Typed _ area (Var name _)                              -> collectVar ctx cs area name solvedExp
  Typed _ area (Assignment name rhs)                     -> collectAssignment ctx cs area name rhs
  Typed _ area (Mutate (Typed _ _ (Var name _)) rhs)     -> collectMutateVar ctx cs area name rhs
  Typed _ _    (Mutate lhs rhs)                          -> (<>) <$> collect ctx cs lhs <*> collect ctx cs rhs
  Typed _ _    (App fn arg _)                            -> (<>) <$> collect ctx cs fn  <*> collect ctx cs arg
  Typed _ _    (If c t f)                                -> mconcat <$> mapM (collect ctx cs) [c, t, f]
  Typed _ _    (While cond body)                         -> (<>) <$> collect ctx cs cond <*> collect ctx cs body
  Typed _ _    (Abs (Typed _ _ name) body)               -> collectAbs ctx cs name body solvedExp
  Typed _ _    (Do body)                                 -> collectBody ctx cs body
  Typed _ _    (TypedExp inner _ _)                      -> collect ctx cs inner
  Typed _ _    (Export inner)                            -> collect ctx cs inner
  Typed _ _    (Access record _)                         -> collect ctx cs record
  Typed _ _    (TemplateString exps)                     -> mconcat <$> mapM (collect ctx cs) exps
  Typed _ _    (TupleConstructor exps)                   -> mconcat <$> mapM (collect ctx cs) exps
  Typed _ _    (Where exp iss)                           -> collectWhere ctx cs exp iss
  Typed _ _    (ListConstructor items)                   -> mconcat <$> mapM (collectListItem ctx cs) items
  Typed _ _    (Record fields)                           -> mconcat <$> mapM (collectField ctx cs) fields
  Typed _ area (Extern _ _ name)                         -> collectExtern ctx area name
  Typed _ _    (NameExport name)                         -> collectNameExport ctx cs name solvedExp
  Untyped _    (TypeExport name)                         -> collectTypeExport ctx cs name solvedExp
  _                                                      -> return mempty


-- ---------------------------------------------------------------------------
-- Per-node collect helpers
-- ---------------------------------------------------------------------------

collectVar :: CollectCtx -> CollectState -> Area -> String -> Exp -> Infer Accesses
collectVar ctx cs area name self
  | name == "_"      = throwError $ CompilationError IllegalSkipAccess (Context (envCurrentPath $ ctxEnv ctx) area)
  | isPropAccess name = return mempty
  | otherwise        = do
      checkRecursiveAccess ctx cs area name
      if name `memberScope` csLocalScope cs
        then return mempty
        else return (singleAccess name self)
  where
    isPropAccess ('.':_) = True
    isPropAccess _       = False

collectAssignment :: CollectCtx -> CollectState -> Area -> String -> Exp -> Infer Accesses
collectAssignment ctx cs area name rhs = do
  checkShadow ctx area name
  collect ctx cs { csNameToFind = Just name } rhs

collectMutateVar :: CollectCtx -> CollectState -> Area -> String -> Exp -> Infer Accesses
collectMutateVar ctx cs area name rhs = do
  checkShadow ctx area name
  collect ctx cs { csNameToFind = Just name } rhs

-- | Collect from an Abs (lambda). Adds the parameter to localScope and
-- foundNames, clears nameToFind (crossing the lambda boundary makes any
-- prior nameToFind legal).
-- All accesses collected from the body are remapped to point to the Abs
-- expression itself (preserving the original behaviour).
collectAbs :: CollectCtx -> CollectState -> String -> [Exp] -> Exp -> Infer Accesses
collectAbs ctx cs paramName body absExp = do
  let foundNames' = extendFoundNames (csNameToFind cs) (csFoundNames cs)
      cs' = cs
        { csFoundNames  = paramName : foundNames'
        , csNameToFind  = Nothing
        , csLocalScope  = insertScope paramName (csLocalScope cs)
        }
      -- The parameter name also enters topLevelAssignments so that inner
      -- bindings that shadow it are caught.
      ctx' = ctx { ctxTopLevelAssignments = S.insert paramName (ctxTopLevelAssignments ctx) }
  accesses <- collectBody ctx' cs' body
  -- Remap all accesses to the Abs expression (original behaviour).
  return $ Accesses [ (n, absExp) | (n, _) <- unAccesses accesses ]

-- | Extend foundNames when crossing an Abs boundary.
extendFoundNames :: Maybe String -> [String] -> [String]
extendFoundNames (Just "_") found = found
extendFoundNames (Just n)   found = n : found
extendFoundNames Nothing    found = found

-- | Traverse a body (list of expressions) such as a do-block or lambda body.
-- Each assignment extends the local scope for subsequent expressions.
collectBody :: CollectCtx -> CollectState -> [Exp] -> Infer Accesses
collectBody _   _  []     = return mempty
collectBody ctx cs (e:es) = do
  let cs' = cs { csLocalScope = extendScopeFromExp (csLocalScope cs) e }
  accesses <- collect ctx cs' e
  let cs'' = cs'
        { csFoundNames = case getExpName e of
            Just n  -> n : csFoundNames cs'
            Nothing -> csFoundNames cs'
        }
  rest <- collectBody ctx cs'' es
  return (accesses <> rest)

collectWhere :: CollectCtx -> CollectState -> Exp -> [Is] -> Infer Accesses
collectWhere ctx cs exp iss = do
  expAccesses <- collect ctx cs exp
  issAccesses <- mconcat <$> mapM (collectIs ctx cs) iss
  return (expAccesses <> issAccesses)

collectIs :: CollectCtx -> CollectState -> Is -> Infer Accesses
collectIs ctx cs (Typed _ _ (Is pat body)) =
  collect ctx cs { csLocalScope = csLocalScope cs <> buildPatternScope pat } body
collectIs _   _  _                         = return mempty

collectListItem :: CollectCtx -> CollectState -> ListItem -> Infer Accesses
collectListItem ctx cs (Typed _ _ li) = case li of
  ListItem   e -> collect ctx cs e
  ListSpread e -> collect ctx cs e
collectListItem _   _  _              = return mempty

collectField :: CollectCtx -> CollectState -> Field -> Infer Accesses
collectField ctx cs (Typed _ _ f) = case f of
  Field (_, e)  -> collect ctx cs e
  FieldSpread e -> collect ctx cs e
collectField _   _  _             = return mempty

collectExtern :: CollectCtx -> Area -> String -> Infer Accesses
collectExtern ctx area name = do
  checkShadow ctx area name
  return mempty

collectNameExport :: CollectCtx -> CollectState -> String -> Exp -> Infer Accesses
collectNameExport _ctx cs name self
  | name `memberScope` csGlobalScope cs = return mempty
  | otherwise                            = return (singleAccess name self)

collectTypeExport :: CollectCtx -> CollectState -> String -> Exp -> Infer Accesses
collectTypeExport _ctx cs name self
  | name `memberScope` csGlobalScope cs = return mempty
  | otherwise                            = return (singleAccess name self)


-- ---------------------------------------------------------------------------
-- Pattern scope building
-- ---------------------------------------------------------------------------

buildPatternScope :: Pattern -> InScope
buildPatternScope (Typed _ _ pat) = case pat of
  PVar name            -> InScope (S.singleton name)
  PCon _ pats          -> mconcat (buildPatternScope <$> pats)
  PRecord fieldPats restName ->
    mconcat (buildPatternScope <$> M.elems fieldPats)
    <> maybe mempty (\n -> InScope (S.singleton n)) restName
  PList  pats          -> mconcat (buildPatternScope <$> pats)
  PTuple pats          -> mconcat (buildPatternScope <$> pats)
  PSpread p            -> buildPatternScope p
  _                    -> mempty
buildPatternScope _ = mempty


-- ---------------------------------------------------------------------------
-- Mutation verification
-- ---------------------------------------------------------------------------

verifyMutations :: Env -> M.Map String Bool -> Exp -> Infer ()
verifyMutations env scope exp = case exp of
  Typed _ area (Assignment n e) -> do
    let inScope = M.lookup n scope
    when ((isAbs e && isJust inScope) || inScope == Just True) $
      throwError (CompilationError (MutatingFunction n) (Context (envCurrentPath env) area))
    verifyMutations env (M.insert n (isAbs e) scope) e

  Typed _ area (Mutate (Typed _ _ (Var n _)) e) -> do
    let inScope = M.lookup n scope
    when ((isAbs e && isJust inScope) || inScope == Just True) $
      throwError (CompilationError (MutatingFunction n) (Context (envCurrentPath env) area))
    verifyMutations env (M.insert n (isAbs e) scope) e

  Typed _ _ (App f e _) -> do
    verifyMutations env scope f
    verifyMutations env scope e

  Typed _ _ (Abs _ es) ->
    verifyMutationsInBody env scope es

  Typed _ _ (Do es) ->
    verifyMutationsInBody env scope es

  Typed _ _ (While _ e) ->
    verifyMutations env scope e

  Typed _ _ (Access rec field) -> do
    verifyMutations env scope rec
    verifyMutations env scope field

  Typed _ _ (TemplateString es) ->
    mapM_ (verifyMutations env scope) es

  Typed _ _ (TupleConstructor es) ->
    mapM_ (verifyMutations env scope) es

  Typed _ _ (ListConstructor items) ->
    mapM_ (verifyMutations env scope . getListItemExp) items

  Typed _ _ (Record fields) ->
    mapM_ (verifyMutations env scope . getFieldExp) fields

  Typed _ _ (Where e iss) -> do
    verifyMutations env scope e
    mapM_ (verifyMutations env scope . getIsExpression) iss

  Typed _ _ (If cond truthy falsy) -> do
    verifyMutations env scope cond
    verifyMutations env scope truthy
    verifyMutations env scope falsy

  Typed _ _ (Export e) ->
    verifyMutations env scope e

  Typed _ _ (TypedExp e _ _) ->
    verifyMutations env scope e

  _ ->
    return ()


verifyMutationsInBody :: Env -> M.Map String Bool -> [Exp] -> Infer ()
verifyMutationsInBody env scope exps = case exps of
  e@(Typed _ _ (Assignment n _)) : es -> do
    let nextScope = M.insert n (isAbs e) scope
    verifyMutations env scope e
    verifyMutationsInBody env nextScope es

  e : es -> do
    verifyMutations env scope e
    verifyMutationsInBody env scope es

  [] ->
    return ()
