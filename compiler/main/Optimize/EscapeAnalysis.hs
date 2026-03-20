{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Optimize.EscapeAnalysis
  ( analyzeAST
  , buildFunctionSummaries
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import           AST.Core


-- ParamEscape and FunctionSummaries are defined in AST.Core
-- to allow cross-module use via Driver.Query


-- | Run escape analysis on the entire AST.
-- Takes external (cross-module) summaries and merges them with local summaries
-- to more precisely determine which allocations can be stack-allocated.
analyzeAST :: FunctionSummaries -> AST -> AST
analyzeAST externalSummaries ast =
  let localSummaries = buildFunctionSummaries (aexps ast)
      -- Merge: local summaries take precedence over external ones
      summaries = M.union localSummaries externalSummaries
  in  ast { aexps = analyzeTopLevel summaries <$> aexps ast }


-- | Build inter-procedural function summaries by analyzing each top-level
-- function to determine which of its parameters escape.
-- Iterates to a fixed point to handle mutual recursion.
buildFunctionSummaries :: [Exp] -> FunctionSummaries
buildFunctionSummaries topExps =
  let -- Extract all top-level function definitions: (name, params, body)
      funDefs = concatMap extractFunDef topExps
      -- Initialize all parameters as DoesNotEscape
      initial = M.fromList [(name, replicate (length params) DoesNotEscape) | (name, params, _) <- funDefs]
      -- Iterate to fixed point
      iterate' summaries =
        let summaries' = foldl (analyzeFunDef funDefs) summaries funDefs
        in  if summaries' == summaries then summaries else iterate' summaries'
  in  iterate' initial


-- | Extract function definitions from a top-level expression.
-- Unwraps Export and Assignment wrappers to find Definition nodes.
extractFunDef :: Exp -> [(String, [Core Name], [Exp])]
extractFunDef exp = case exp of
  Typed _ _ _ (Export e) ->
    extractFunDef e

  Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) (Typed _ _ _ (Definition params body))) ->
    [(name, params, body)]

  _ -> []


-- | Analyze a single function definition and update the summaries.
-- A parameter escapes if it (or a variable derived from it) appears in:
--   - The return position (last expression of the body)
--   - An argument to a call where the callee's summary says that position escapes
--   - A reference store
--   - A closure capture
analyzeFunDef :: [(String, [Core Name], [Exp])] -> FunctionSummaries -> (String, [Core Name], [Exp]) -> FunctionSummaries
analyzeFunDef _allDefs summaries (name, params, body) =
  let paramNames = map getParamName params
      -- Find all names that escape in this function body using current summaries
      escapingNames = findEscapingNamesForSummary summaries body
      -- Check which parameters escape
      paramEscapes = [ if pName `S.member` escapingNames then Escapes else DoesNotEscape
                     | pName <- paramNames
                     ]
  in  M.insert name paramEscapes summaries


-- | Like findEscapingNames but for building summaries.
-- The key difference: all names in the return position escape (they leave the function).
-- We also need to transitively close through assignments.
findEscapingNamesForSummary :: FunctionSummaries -> [Exp] -> S.Set String
findEscapingNamesForSummary summaries body =
  let -- The last expression is the return value — names there escape the function
      returnEscaping = case body of
        [] -> S.empty
        _  -> collectVarNames (last body)
      -- Names that escape through calls, stores, closures
      callEscaping = S.unions (map (collectEscapingFromExp summaries) body)
      directlyEscaping = S.union returnEscaping callEscaping
      -- Transitively close
      allEscaping = transitiveClose body directlyEscaping
  in  allEscaping


-- | Analyze a top-level expression.
analyzeTopLevel :: FunctionSummaries -> Exp -> Exp
analyzeTopLevel summaries exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (analyzeExp summaries rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (analyzeTopLevel summaries e))

  _ ->
    analyzeExp summaries exp


-- | Analyze an expression, marking non-escaping allocations as StackAllocatable.
analyzeExp :: FunctionSummaries -> Exp -> Exp
analyzeExp summaries exp = case exp of
  Typed qt area metadata (Definition params body) ->
    let body' = analyzeBody summaries body
    in  Typed qt area metadata (Definition params body')

  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (analyzeExp summaries rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (analyzeExp summaries e))

  _ -> exp


-- | Analyze a function body (list of expressions in a Do block).
-- The last expression is the return value — allocations there always escape.
-- For intermediate assignments, check if the bound name escapes.
analyzeBody :: FunctionSummaries -> [Exp] -> [Exp]
analyzeBody summaries body =
  let -- Collect the set of names that escape in this scope
      escapingNames = findEscapingNames summaries body
      -- Process each expression, marking non-escaping allocations
      body' = markAllocations escapingNames <$> body
      -- Recurse into sub-expressions
  in  recurseIntoBody summaries body'


-- | Recurse into body expressions to analyze nested definitions.
recurseIntoBody :: FunctionSummaries -> [Exp] -> [Exp]
recurseIntoBody summaries = map (recurseExp summaries)


-- | Recurse into an expression to find and analyze nested definitions.
recurseExp :: FunctionSummaries -> Exp -> Exp
recurseExp summaries exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (recurseExp summaries rhs))

  Typed qt area metadata (Definition params body) ->
    let body' = analyzeBody summaries body
    in  Typed qt area metadata (Definition params body')

  Typed qt area metadata (If cond thenBranch elseBranch) ->
    Typed qt area metadata (If (recurseExp summaries cond) (recurseExp summaries thenBranch) (recurseExp summaries elseBranch))

  Typed qt area metadata (Where scrutinee branches) ->
    let branches' = map (\(Typed qt' area' md (Is pat body)) ->
                          Typed qt' area' md (Is pat (recurseExp summaries body))) branches
    in  Typed qt area metadata (Where (recurseExp summaries scrutinee) branches')

  Typed qt area metadata (Do exps) ->
    let exps' = analyzeBody summaries exps
    in  Typed qt area metadata (Do exps')

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (recurseExp summaries fn) (map (recurseExp summaries) args))

  _ -> exp


-- | Find all names that escape in a body (list of expressions).
-- A name escapes if:
--   1. It appears in the return position (last expression)
--   2. It is passed as an argument to a function call (unless the callee's summary says it doesn't escape)
--   3. It is stored in a reference (ReferenceStore metadata)
--   4. It is assigned to another variable that escapes
--   5. It is used as a field in a constructor/tuple/record that escapes
findEscapingNames :: FunctionSummaries -> [Exp] -> S.Set String
findEscapingNames summaries body =
  let -- The last expression is always in return position — any names used there escape
      returnEscaping = case body of
        [] -> S.empty
        _  -> collectVarNames (last body)
      -- Collect names that escape through function calls and other uses
      callEscaping = S.unions (map (collectEscapingFromExp summaries) body)
      -- All directly escaping names
      directlyEscaping = S.union returnEscaping callEscaping
      -- Transitively close: if x = #[y, z] and x escapes, then y and z escape too
      allEscaping = transitiveClose body directlyEscaping
  in  allEscaping


-- | Transitively close the escaping set:
-- If x escapes and x = Constructor(a, b), then a and b also escape.
-- If x escapes and x = #[a, b], then a and b also escape.
transitiveClose :: [Exp] -> S.Set String -> S.Set String
transitiveClose body escaping =
  let expanded = foldl expandAssignment escaping body
  in  if expanded == escaping
      then escaping
      else transitiveClose body expanded


-- | If an assignment's LHS escapes, all var names in its RHS also escape.
expandAssignment :: S.Set String -> Exp -> S.Set String
expandAssignment escaping exp = case exp of
  Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) rhs) ->
    if name `S.member` escaping
    then S.union escaping (collectVarNames rhs)
    else escaping

  _ -> escaping


-- | Collect names that escape through function calls, stores, etc.
-- Does NOT include the return position (handled separately).
-- Uses function summaries to avoid conservatively marking all call arguments as escaping.
collectEscapingFromExp :: FunctionSummaries -> Exp -> S.Set String
collectEscapingFromExp summaries exp = case exp of
  -- Function calls: use summaries to determine which arguments escape
  Typed _ _ _ (Call fn args) ->
    let fnSummary = case fn of
          Typed _ _ _ (Var name _) -> M.lookup name summaries
          _                        -> Nothing
    in  case fnSummary of
          Just paramEscapes ->
            -- Only mark arguments as escaping where the callee's summary says so
            S.unions [ collectVarNames arg
                     | (arg, escapeInfo) <- zip args paramEscapes
                     , escapeInfo == Escapes
                     ]
          Nothing ->
            -- Unknown function: conservative fallback — all arguments escape
            S.unions (map collectVarNames args)

  -- Reference stores: the stored value escapes
  Typed _ _ metadata (Assignment _ rhs) | isReferenceStore metadata ->
    collectVarNames rhs

  -- Where expressions: scrutinee is consumed, doesn't escape
  -- But the branch bodies might have escaping expressions
  Typed _ _ _ (Where _scrutinee branches) ->
    S.unions (map (\(Typed _ _ _ (Is _ body)) -> collectEscapingFromExp summaries body) branches)

  -- If expressions: both branches might have escaping expressions
  Typed _ _ _ (If _ thenBranch elseBranch) ->
    S.union (collectEscapingFromExp summaries thenBranch) (collectEscapingFromExp summaries elseBranch)

  -- Do blocks: recurse
  Typed _ _ _ (Do exps) ->
    S.unions (map (collectEscapingFromExp summaries) exps)

  -- List constructors: items in list literals don't escape the list itself
  -- (the list allocation is what matters)

  _ -> S.empty


-- | Collect all variable names referenced in an expression (shallowly).
-- Used to determine what "escapes" when an expression is in escaping position.
collectVarNames :: Exp -> S.Set String
collectVarNames exp = case exp of
  Typed _ _ _ (Var name _) ->
    S.singleton name

  Typed _ _ _ (Call fn args) ->
    S.union (collectVarNames fn) (S.unions (map collectVarNames args))

  Typed _ _ _ (TupleConstructor exps) ->
    S.unions (map collectVarNames exps)

  Typed _ _ _ (ListConstructor items) ->
    S.unions (map collectListItem items)

  Typed _ _ _ (Record fields) ->
    S.unions (map collectField fields)

  Typed _ _ _ (If cond thenBranch elseBranch) ->
    S.unions [collectVarNames cond, collectVarNames thenBranch, collectVarNames elseBranch]

  Typed _ _ _ (Where scrutinee branches) ->
    S.union (collectVarNames scrutinee)
            (S.unions (map (\(Typed _ _ _ (Is _ body)) -> collectVarNames body) branches))

  Typed _ _ _ (Do exps) ->
    case exps of
      [] -> S.empty
      _  -> collectVarNames (last exps)

  Typed _ _ _ (Access e _) ->
    collectVarNames e

  Typed _ _ _ (Assignment _ rhs) ->
    collectVarNames rhs

  Typed _ _ _ (Definition _ _) ->
    -- A definition captures free variables — those escape
    collectFreeVarsFromDefinition exp

  _ -> S.empty


-- | Collect free variable names from a definition (lambda/closure).
-- Any variable from the outer scope that appears free in the definition escapes
-- because it will be captured in the closure environment.
collectFreeVarsFromDefinition :: Exp -> S.Set String
collectFreeVarsFromDefinition exp = case exp of
  Typed _ _ _ (Definition params body) ->
    let paramNames = S.fromList (map getParamName params)
        bodyVars   = S.unions (map collectAllVarRefs body)
    in  S.difference bodyVars paramNames

  _ -> S.empty


-- | Get the parameter name from a Core Name wrapper.
getParamName :: Core Name -> String
getParamName (Typed _ _ _ name) = name
getParamName (Untyped _ _ name) = name


-- | Collect ALL variable references in an expression (deep).
collectAllVarRefs :: Exp -> S.Set String
collectAllVarRefs exp = case exp of
  Typed _ _ _ (Var name _) -> S.singleton name
  Typed _ _ _ (Call fn args) -> S.union (collectAllVarRefs fn) (S.unions (map collectAllVarRefs args))
  Typed _ _ _ (TupleConstructor exps) -> S.unions (map collectAllVarRefs exps)
  Typed _ _ _ (ListConstructor items) -> S.unions (map collectAllVarRefsListItem items)
  Typed _ _ _ (Record fields) -> S.unions (map collectAllVarRefsField fields)
  Typed _ _ _ (If c t e) -> S.unions [collectAllVarRefs c, collectAllVarRefs t, collectAllVarRefs e]
  Typed _ _ _ (Where s bs) -> S.union (collectAllVarRefs s) (S.unions (map collectAllVarRefsBranch bs))
  Typed _ _ _ (Do exps) -> S.unions (map collectAllVarRefs exps)
  Typed _ _ _ (Assignment _ rhs) -> collectAllVarRefs rhs
  Typed _ _ _ (Definition params body) ->
    let paramNames = S.fromList (map getParamName params)
    in  S.difference (S.unions (map collectAllVarRefs body)) paramNames
  Typed _ _ _ (Access e _) -> collectAllVarRefs e
  Typed _ _ _ (ArrayAccess e1 e2) -> S.union (collectAllVarRefs e1) (collectAllVarRefs e2)
  Typed _ _ _ (Export e) -> collectAllVarRefs e
  _ -> S.empty

collectAllVarRefsListItem :: ListItem -> S.Set String
collectAllVarRefsListItem item = case item of
  Typed _ _ _ (ListItem e) -> collectAllVarRefs e
  Typed _ _ _ (ListSpread e) -> collectAllVarRefs e
  _ -> S.empty

collectAllVarRefsBranch :: Is -> S.Set String
collectAllVarRefsBranch branch = case branch of
  Typed _ _ _ (Is _pat body) -> collectAllVarRefs body
  _ -> S.empty

collectAllVarRefsField :: Field -> S.Set String
collectAllVarRefsField field = case field of
  Typed _ _ _ (Field (_, e)) -> collectAllVarRefs e
  Typed _ _ _ (FieldSpread e) -> collectAllVarRefs e
  _ -> S.empty


-- | Collect var names from a list item.
collectListItem :: ListItem -> S.Set String
collectListItem item = case item of
  Typed _ _ _ (ListItem e) -> collectVarNames e
  Typed _ _ _ (ListSpread e) -> collectVarNames e
  _ -> S.empty


-- | Collect var names from a record field.
collectField :: Field -> S.Set String
collectField field = case field of
  Typed _ _ _ (Field (_, e)) -> collectVarNames e
  Typed _ _ _ (FieldSpread e) -> collectVarNames e
  _ -> S.empty


-- | Mark allocations as StackAllocatable if the bound name does not escape.
-- Only applies to Assignment expressions where the RHS is an allocation.
markAllocations :: S.Set String -> Exp -> Exp
markAllocations escapingNames exp = case exp of
  Typed qt area metadata (Assignment (Typed lhsQt lhsArea lhsMetadata (Var name isConstructor)) rhs) ->
    if name `S.notMember` escapingNames && isAllocation rhs
    then
      let rhs' = addStackAllocatable rhs
      in  Typed qt area metadata (Assignment (Typed lhsQt lhsArea lhsMetadata (Var name isConstructor)) rhs')
    else
      exp

  _ -> exp


-- | Check if an expression is an allocation (constructor call, tuple, record).
isAllocation :: Exp -> Bool
isAllocation exp = case exp of
  -- Constructor call: Var with isConstructor=True applied to args
  Typed _ _ _ (Call (Typed _ _ _ (Var _ True)) _) -> True
  -- Zero-arity constructor reference
  Typed _ _ _ (Var _ True) -> True
  -- Tuple allocation
  Typed _ _ _ (TupleConstructor _) -> True
  -- Record allocation
  Typed _ _ _ (Record _) -> True
  _ -> False


-- | Add StackAllocatable metadata to an allocation expression.
addStackAllocatable :: Exp -> Exp
addStackAllocatable exp = case exp of
  Typed qt area metadata e ->
    Typed qt area (StackAllocatable : metadata) e

  _ -> exp
