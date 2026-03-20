{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Optimize.EscapeAnalysis where

import qualified Data.Set as S
import           AST.Core


-- | Run escape analysis on the entire AST.
-- Marks allocations (constructors, tuples, records) as StackAllocatable
-- when they provably do not escape their defining scope.
analyzeAST :: AST -> AST
analyzeAST ast =
  ast { aexps = analyzeTopLevel <$> aexps ast }


-- | Analyze a top-level expression.
analyzeTopLevel :: Exp -> Exp
analyzeTopLevel exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (analyzeExp rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (analyzeTopLevel e))

  _ ->
    analyzeExp exp


-- | Analyze an expression, marking non-escaping allocations as StackAllocatable.
analyzeExp :: Exp -> Exp
analyzeExp exp = case exp of
  Typed qt area metadata (Definition params body) ->
    let body' = analyzeBody body
    in  Typed qt area metadata (Definition params body')

  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (analyzeExp rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (analyzeExp e))

  _ -> exp


-- | Analyze a function body (list of expressions in a Do block).
-- The last expression is the return value — allocations there always escape.
-- For intermediate assignments, check if the bound name escapes.
analyzeBody :: [Exp] -> [Exp]
analyzeBody body =
  let -- Collect the set of names that escape in this scope
      escapingNames = findEscapingNames body
      -- Process each expression, marking non-escaping allocations
      body' = markAllocations escapingNames <$> body
      -- Recurse into sub-expressions
  in  recurseIntoBody body'


-- | Recurse into body expressions to analyze nested definitions.
recurseIntoBody :: [Exp] -> [Exp]
recurseIntoBody = map recurseExp


-- | Recurse into an expression to find and analyze nested definitions.
recurseExp :: Exp -> Exp
recurseExp exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (recurseExp rhs))

  Typed qt area metadata (Definition params body) ->
    let body' = analyzeBody body
    in  Typed qt area metadata (Definition params body')

  Typed qt area metadata (If cond thenBranch elseBranch) ->
    Typed qt area metadata (If (recurseExp cond) (recurseExp thenBranch) (recurseExp elseBranch))

  Typed qt area metadata (Where scrutinee branches) ->
    let branches' = map (\(Typed qt' area' md (Is pat body)) ->
                          Typed qt' area' md (Is pat (recurseExp body))) branches
    in  Typed qt area metadata (Where (recurseExp scrutinee) branches')

  Typed qt area metadata (Do exps) ->
    let exps' = analyzeBody exps
    in  Typed qt area metadata (Do exps')

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (recurseExp fn) (map recurseExp args))

  _ -> exp


-- | Find all names that escape in a body (list of expressions).
-- A name escapes if:
--   1. It appears in the return position (last expression)
--   2. It is passed as an argument to a function call
--   3. It is stored in a reference (ReferenceStore metadata)
--   4. It is assigned to another variable that escapes
--   5. It is used as a field in a constructor/tuple/record that escapes
findEscapingNames :: [Exp] -> S.Set String
findEscapingNames body =
  let -- The last expression is always in return position — any names used there escape
      returnEscaping = case body of
        [] -> S.empty
        _  -> collectVarNames (last body)
      -- Collect names that escape through function calls and other uses
      callEscaping = S.unions (map collectEscapingFromExp body)
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
collectEscapingFromExp :: Exp -> S.Set String
collectEscapingFromExp exp = case exp of
  -- Function calls: all arguments escape (conservative — we don't do inter-procedural analysis)
  Typed _ _ _ (Call _fn args) ->
    S.unions (map collectVarNames args)

  -- Reference stores: the stored value escapes
  Typed _ _ metadata (Assignment _ rhs) | isReferenceStore metadata ->
    collectVarNames rhs

  -- Where expressions: scrutinee is consumed, doesn't escape
  -- But the branch bodies might have escaping expressions
  Typed _ _ _ (Where _scrutinee branches) ->
    S.unions (map (\(Typed _ _ _ (Is _ body)) -> collectEscapingFromExp body) branches)

  -- If expressions: both branches might have escaping expressions
  Typed _ _ _ (If _ thenBranch elseBranch) ->
    S.union (collectEscapingFromExp thenBranch) (collectEscapingFromExp elseBranch)

  -- Do blocks: recurse
  Typed _ _ _ (Do exps) ->
    S.unions (map collectEscapingFromExp exps)

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
