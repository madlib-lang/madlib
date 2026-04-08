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
-- Additionally, variables consumed by calls (passed as non-OwnedArg arguments)
-- have ownership transferred to the callee and must NOT be ScopeDrop'd.
annotateBody :: [Exp] -> [Exp]
annotateBody [] = []
annotateBody exps =
  let returnNames   = collectReturnNames (last exps)
      consumedNames = collectConsumedNames exps
      protectedNames = Set.union returnNames consumedNames
  in  map (annotateExp protectedNames) exps


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

  -- Field access: the receiver is live (e.g. hitCounter.read() keeps hitCounter alive)
  Typed _ _ _ (Access rec _) ->
    collectReturnNames rec

  -- Array access: both the array and the index are live
  Typed _ _ _ (ArrayAccess arr idx) ->
    Set.union (collectReturnNames arr) (collectReturnNames idx)

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


-- | Collect names of variables that are consumed by function calls in the
-- body (passed as arguments WITHOUT OwnedArg metadata).  These variables have
-- their ownership transferred to the callee and must NOT be ScopeDrop'd —
-- the callee is now responsible for freeing them.
--
-- This is critical for in-place TRMC (e.g. map reusing the input list) where
-- the callee returns the same pointer as the input.  Without this guard,
-- both the input binding and the result binding would be ScopeDrop'd, causing
-- a double-free.
collectConsumedNames :: [Exp] -> Set.Set String
collectConsumedNames = Set.unions . map consumedInExp
  where
    consumedInExp exp = case exp of
      Typed _ _ _ (Assignment _ rhs) ->
        consumedInExp rhs

      Typed _ _ _ (Call fn args) ->
        let consumedArgs = Set.unions $ map consumedVar args
            recurse      = Set.unions $ consumedInExp fn : map consumedInExp args
        in  Set.union consumedArgs recurse

      Typed _ _ _ (Do exps) ->
        collectConsumedNames exps

      Typed _ _ _ (Where scrut branches) ->
        -- The scrutinee is consumed by the match (pattern-matched on and then
        -- discarded). Mark it as consumed so pattern vars that are sub-fields of
        -- the scrutinee are NOT separately PatternVarDrop'd — the scrutinee (outer
        -- container) still holds those references until it is itself dropped.
        Set.unions [ consumedVar scrut
                   , consumedInExp scrut
                   , Set.unions $ map consumedInBranch branches
                   ]

      Typed _ _ _ (If cond t f) ->
        Set.unions [consumedInExp cond, consumedInExp t, consumedInExp f]

      -- Variables stored into tuple/list/record constructors transfer ownership
      -- into the container — they must NOT be ScopeDrop'd independently.
      Typed _ _ _ (TupleConstructor exps) ->
        Set.unions $ map consumedVar exps

      Typed _ _ _ (ListConstructor items) ->
        Set.unions $ map consumedFromListItem items

      Typed _ _ _ (Record fields) ->
        Set.unions $ map consumedFromField fields

      _ -> Set.empty

    -- A direct Var argument that does NOT carry OwnedArg is consumed by the
    -- callee — ownership transfers, so the binding must not be ScopeDrop'd.
    -- Unwrap Do blocks (from coverage instrumentation) to find the inner Var.
    consumedVar arg = case unwrapDo arg of
      Typed _ _ metadata (Var name _)
        | not (isOwnedArg metadata) -> Set.singleton name
      _ -> Set.empty

    -- Unwrap a single-layer Do block to its last expression (used by coverage
    -- instrumentation which wraps expressions in Do [tracking..., expr]).
    unwrapDo exp = case exp of
      Typed _ _ _ (Do exps) | not (null exps) -> last exps
      _ -> exp

    consumedFromListItem item = case item of
      Typed _ _ _ (ListItem e)   -> consumedVar e
      Typed _ _ _ (ListSpread e) -> consumedVar e
      _                          -> Set.empty

    consumedFromField field = case field of
      Typed _ _ _ (Field (_, e))  -> consumedVar e
      Typed _ _ _ (FieldSpread e) -> consumedVar e
      _                           -> Set.empty

    consumedInBranch is = case is of
      Typed _ _ _ (Is _ body) -> consumedInExp body
      _                       -> Set.empty


-- ---------------------------------------------------------------------------
-- Expression annotation
-- ---------------------------------------------------------------------------

-- | Annotate a single expression.  @returnNames@ is the set of variable names
-- that the enclosing scope returns (directly or as part of a composite value).
-- Those bindings must not be dropped.
annotateExp :: Set.Set String -> Exp -> Exp
annotateExp returnNames exp = case exp of
  -- Plain assignment: potentially mark as ScopeDrop or RefCellDrop
  Typed qt area metadata (Assignment lhs@(Typed _ _ _ (Var name _)) rhs) ->
    let isProtected  = Set.member name returnNames
        -- Recurse into the RHS to annotate nested definitions/lambdas
        rhs'         = annotateNestedDefs rhs
    in  if isProtected
          then Typed qt area metadata (Assignment lhs rhs')
          else if isReferenceAllocation metadata
            -- Ref cells: annotate with RefCellDrop so codegen drops the stored value
            -- and the outer cell box at scope exit.
            then Typed qt area (RefCellDrop : metadata) (Assignment lhs rhs')
            else if not (isAtomicMadlibType (getType rhs))
              then Typed qt area (ScopeDrop : metadata) (Assignment lhs rhs')
              else Typed qt area metadata (Assignment lhs rhs')

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


-- | Collect variable names bound by a pattern that are candidates for PatternVarDrop.
--
-- We only collect from PCon (ADT constructor) patterns because those values are
-- typically uniquely owned at the match site.  PList, PTuple, and PRecord patterns
-- match values that are often still owned by an outer container (e.g. a list
-- holding tuples) — dropping their fields while the container still points to
-- them causes use-after-free.  The rc_is_unique guard in emitPatternVarDrops
-- provides a runtime safety net for PCon cases that are shared.
collectPatternVarNames :: Pattern -> Set.Set String
collectPatternVarNames pat = case pat of
  Typed _ _ _ (PVar name)      -> Set.singleton name
  Typed _ _ _ (PCon _ subPats) -> Set.unions $ map collectPatternVarNames subPats
  -- PList, PTuple, PRecord: elements/fields are borrowed from the container.
  -- Dropping them here while the container still holds them causes UAF.
  Typed _ _ _ (PList _)        -> Set.empty
  Typed _ _ _ (PTuple _)       -> Set.empty
  Typed _ _ _ (PRecord _ _)    -> Set.empty
  Typed _ _ _ (PSpread inner)  -> collectPatternVarNames inner
  _                            -> Set.empty


-- | Collect consumed names from a single expression (adapts collectConsumedNames
-- which operates on [Exp]).
collectConsumedNamesInExp :: Exp -> Set.Set String
collectConsumedNamesInExp exp = collectConsumedNames [exp]


-- | Annotate a Where branch:
--  1. Recurse into the body for nested definitions.
--  2. Compute which pattern-bound vars need rc_dec (not returned, not consumed)
--     and attach PatternVarDrop metadata to the Is node.
annotateBranch :: Is -> Is
annotateBranch is = case is of
  Typed qt area metadata (Is pat body) ->
    let body'         = annotateNestedDefs body
        -- Names bound by the pattern that are heap-managed
        patNames      = collectPatternVarNames pat
        -- Names that flow into the return value (must NOT be dropped)
        returnNames   = collectReturnNames body
        -- Names consumed by calls (ownership transferred to callee, must NOT be dropped)
        consumedNames = collectConsumedNamesInExp body
        protectedNames = Set.union returnNames consumedNames
        -- Names to drop = pattern vars - protected - atomic types
        -- We can't check isAtomicMadlibType here (no type info on pattern vars).
        -- Use the type of the pattern sub-expression instead — check it at the
        -- codegen level using the Madlib type from the symbol table.
        -- Here we just record ALL non-protected pattern var names; the codegen
        -- guards with isAtomicType / pointer-type check at emit time.
        dropNames     = Set.toList $ Set.difference patNames protectedNames
        metadata'     = if null dropNames
                          then metadata
                          else PatternVarDrop dropNames : metadata
    in  Typed qt area metadata' (Is pat body')
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
