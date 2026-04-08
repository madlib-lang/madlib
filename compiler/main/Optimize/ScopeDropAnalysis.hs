{-# LANGUAGE LambdaCase #-}
{-|
Module      : Optimize.ScopeDropAnalysis
Description : Annotates bindings that need rc_dec at scope exit (Perceus RC).

= Background

In the Perceus ownership model, every heap-allocated value has exactly one owner.
When a binding goes out of scope without being returned, its refcount must be
decremented.  This pass walks each function body and attaches @ScopeDrop@ metadata
to @Assignment@ nodes whose bound value:

  * is heap-managed (not an atomic/primitive type like Integer, Float, Char, etc.)
  * is NOT the function body's return value
  * is NOT a @ReferenceAllocation@ (mutable ref-cells are handled separately via
    their own drop mechanism — their contained value changes over time)

= Scoping Rules

  * Function body (@Definition@): treat as a fresh scope
  * @Do@ block: treat as a nested scope, recurse with @annotateBody@
  * @Where@ branches: each branch body is its own scope
  * @While@ loop body: recurse but do NOT mark outer-scope ref-cells as dropped here
    (they're dropped by the enclosing function scope)
  * @If@ branches: each branch body is treated independently

= Output

@ScopeDrop@ metadata on @Assignment@ nodes tells the LLVM codegen to register
the bound value for @rc_dec@ at the end of the enclosing scope.
-}
module Optimize.ScopeDropAnalysis
  ( annotateAST
  ) where

import           AST.Core
import qualified Infer.Type as IT
import qualified Data.List  as List
import qualified Data.Set   as Set


-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

annotateAST :: AST -> AST
annotateAST ast = ast { aexps = map annotateTopLevel (aexps ast) }


annotateTopLevel :: Exp -> Exp
annotateTopLevel exp = case exp of
  -- Top-level function definition: annotate the body as a fresh scope.
  Typed qt area metadata (Assignment lhs (Typed qt2 area2 meta2 (Definition params body))) ->
    Typed qt area metadata
      (Assignment lhs (Typed qt2 area2 meta2 (Definition params (annotateBody body))))

  -- Exported top-level function definition
  Typed qt area metadata (Export (Typed qt2 area2 meta2 (Assignment lhs (Typed qt3 area3 meta3 (Definition params body))))) ->
    Typed qt area metadata
      (Export (Typed qt2 area2 meta2
        (Assignment lhs (Typed qt3 area3 meta3 (Definition params (annotateBody body))))))

  _ -> exp


-- ---------------------------------------------------------------------------
-- Body annotation
-- ---------------------------------------------------------------------------

-- | Annotate a function/branch body (list of expressions).
-- The last expression's value is the return value — bindings that ARE the
-- return value must not be marked ScopeDrop.
annotateBody :: [Exp] -> [Exp]
annotateBody [] = []
annotateBody exps =
  let returnNames = collectReturnNames (last exps)
  in  map (annotateExp returnNames) exps


-- | Collect ALL variable names referenced (transitively) from the return
-- expression.  Any binding that flows directly into the returned value must
-- not be dropped, because the return value now owns those references.
--
-- We recurse through Records, Tuples, Lists, Do blocks, and Var nodes.
-- We do NOT recurse into nested Definitions (lambdas) — those are fresh
-- scopes and their vars are not the caller's responsibility.
collectReturnNames :: Exp -> Set.Set String
collectReturnNames exp = case exp of
  Typed _ _ _ (Var name _) ->
    Set.singleton name

  Typed _ _ _ (Do exps) -> case exps of
    [] -> Set.empty
    _  -> collectReturnNames (last exps)

  Typed _ _ _ (Record fields) ->
    Set.unions $ map collectFromField fields

  Typed _ _ _ (TupleConstructor exps) ->
    Set.unions $ map collectReturnNames exps

  Typed _ _ _ (ListConstructor items) ->
    Set.unions $ map collectFromListItem items

  Typed _ _ _ (Call fn args) ->
    -- Function calls: the result depends on all arguments
    Set.unions $ collectReturnNames fn : map collectReturnNames args

  Typed _ _ _ (If _ truthy falsy) ->
    -- Either branch can be returned
    Set.union (collectReturnNames truthy) (collectReturnNames falsy)

  Typed _ _ _ (Where _ branches) ->
    Set.unions $ map collectFromBranch branches

  _ -> Set.empty

  where
    collectFromField field = case field of
      Typed _ _ _ (Field (_, e))  -> collectReturnNames e
      Typed _ _ _ (FieldSpread e) -> collectReturnNames e
      _                           -> Set.empty

    collectFromListItem item = case item of
      Typed _ _ _ (ListItem e)   -> collectReturnNames e
      Typed _ _ _ (ListSpread e) -> collectReturnNames e
      _                          -> Set.empty

    collectFromBranch is = case is of
      Typed _ _ _ (Is _ body) -> collectReturnNames body
      _                       -> Set.empty


-- ---------------------------------------------------------------------------
-- Expression annotation
-- ---------------------------------------------------------------------------

-- | Annotate a single expression.  @returnNames@ is the set of variable names
-- that the enclosing scope returns (directly or as part of a composite value).
-- Those bindings must not be dropped.
annotateExp :: Set.Set String -> Exp -> Exp
annotateExp returnNames exp = case exp of
  -- Plain assignment: potentially mark as ScopeDrop
  Typed qt area metadata (Assignment lhs@(Typed _ _ _ (Var name _)) rhs) ->
    let shouldDrop =
             not (Set.member name returnNames)    -- not part of the return value
          && not (isReferenceAllocation metadata)  -- ref cells handled separately
          && not (isAtomicMadlibType (getType rhs))  -- only heap-managed values
        metadata' = if shouldDrop then ScopeDrop : metadata else metadata
        -- Recurse into the RHS to annotate nested definitions/lambdas
        rhs'      = annotateNestedDefs rhs
    in  Typed qt area metadata' (Assignment lhs rhs')

  -- Do block: treat as a nested scope
  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (annotateBody exps))

  -- Where: annotate scrutinee and each branch as its own scope
  Typed qt area metadata (Where scrutinee branches) ->
    Typed qt area metadata
      (Where (annotateNestedDefs scrutinee)
             (map annotateBranch branches))

  -- If: annotate both branches (each is its own scope relative to the if-result)
  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata
      (If (annotateNestedDefs cond)
          (annotateNestedDefs truthy)
          (annotateNestedDefs falsy))

  -- While: recurse into body but don't change outer returnNames
  Typed qt area metadata (While cond body) ->
    Typed qt area metadata
      (While (annotateNestedDefs cond)
             (annotateNestedDefs body))

  -- Anything else: just recurse into sub-expressions for nested definitions
  _ -> annotateNestedDefs exp


-- | Annotate a Where branch — each branch body is a fresh scope.
annotateBranch :: Is -> Is
annotateBranch is = case is of
  Typed qt area metadata (Is pat body) ->
    Typed qt area metadata (Is pat (annotateNestedDefs body))
  _ -> is


-- | Recurse into sub-expressions to find and annotate nested @Definition@s
-- (lambdas, local functions) — each is a fresh scope.
-- Does NOT change the current returnName context.
annotateNestedDefs :: Exp -> Exp
annotateNestedDefs exp = case exp of
  Typed qt area metadata (Definition params body) ->
    -- Fresh scope for a nested lambda/function
    Typed qt area metadata (Definition params (annotateBody body))

  Typed qt area metadata (Assignment lhs rhs) ->
    -- Don't mark this assignment (not in a body context here), but recurse
    Typed qt area metadata (Assignment lhs (annotateNestedDefs rhs))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (annotateBody exps))

  Typed qt area metadata (Where scrutinee branches) ->
    Typed qt area metadata
      (Where (annotateNestedDefs scrutinee)
             (map annotateBranch branches))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata
      (If (annotateNestedDefs cond)
          (annotateNestedDefs truthy)
          (annotateNestedDefs falsy))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata
      (While (annotateNestedDefs cond)
             (annotateNestedDefs body))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata
      (Call (annotateNestedDefs fn) (map annotateNestedDefs args))

  Typed qt area metadata (TupleConstructor exps) ->
    Typed qt area metadata (TupleConstructor (map annotateNestedDefs exps))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (map (mapListItem annotateNestedDefs) items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (map (mapRecordField annotateNestedDefs) fields))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (annotateNestedDefs rec) field)

  Typed qt area metadata (ArrayAccess arr idx) ->
    Typed qt area metadata (ArrayAccess (annotateNestedDefs arr) (annotateNestedDefs idx))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (annotateNestedDefs e))

  _ -> exp


-- ---------------------------------------------------------------------------
-- Type helpers (duplicated from Generate.LLVM.Builtins to avoid circular dep)
-- ---------------------------------------------------------------------------

-- | Returns True for Madlib primitive types that are encoded as inttoptr
-- values (NOT actual heap pointers).  These must NEVER receive rc_inc/rc_dec.
isAtomicMadlibType :: IT.Type -> Bool
isAtomicMadlibType t = case t of
  IT.TCon (IT.TC "Integer" _) _ _ -> True
  IT.TCon (IT.TC "Float"   _) _ _ -> True
  IT.TCon (IT.TC "Boolean" _) _ _ -> True
  IT.TCon (IT.TC "Byte"    _) _ _ -> True
  IT.TCon (IT.TC "Short"   _) _ _ -> True
  IT.TCon (IT.TC "Char"    _) _ _ -> True
  IT.TCon (IT.TC "Unit"    _) _ _ -> True
  _                               -> False
