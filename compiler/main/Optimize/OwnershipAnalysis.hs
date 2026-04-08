{-# LANGUAGE LambdaCase #-}
{-|
Module      : Optimize.OwnershipAnalysis
Description : Perceus ownership analysis — annotates call arguments that need rc_inc.

= Background

In the Perceus ownership model, each heap value has exactly one owner.
When a value is passed to a function call, ownership transfers to the callee.
If the caller still needs the value after the call, it must @rc_inc@ before
passing it.  This pass annotates call arguments that are NOT the last use
of a variable, so the LLVM codegen knows to emit @rc_inc@.

= What this pass does

For every expression in a Do block (sequential execution), it computes
which variables are "live after" (used in subsequent expressions).
When a call argument is a variable that is live after the call, the
argument is annotated with @OwnedArg@ metadata (meaning rc_inc is needed).

= Output

The pass adds @OwnedArg@ metadata to call argument expressions that need
rc_inc.  Arguments without this metadata are "consumed" (last use) and
don't need rc_inc.
-}
module Optimize.OwnershipAnalysis
  ( annotateAST
  ) where

import           AST.Core
import qualified Data.Set    as Set
import qualified Data.List   as List


-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

annotateAST :: AST -> AST
annotateAST ast = ast { aexps = map annotateTopLevel (aexps ast) }


annotateTopLevel :: Exp -> Exp
annotateTopLevel exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (annotateExp Set.empty rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (annotateTopLevel e))

  _ -> annotateExp Set.empty exp


-- ---------------------------------------------------------------------------
-- Expression traversal
-- ---------------------------------------------------------------------------

-- | Annotate an expression.  @liveAfter@ is the set of variable names that
-- are used after this expression in the enclosing sequence.
annotateExp :: Set.Set String -> Exp -> Exp
annotateExp liveAfter exp = case exp of
  Typed qt area metadata (Definition params body) ->
    -- Inside a function body, start fresh liveness analysis
    Typed qt area metadata (Definition params (annotateBody body))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (annotateDoBlock liveAfter exps))

  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (annotateExp liveAfter rhs))

  Typed qt area metadata (Call fn args) ->
    -- This is the critical case: annotate args that need rc_inc
    let fn'   = annotateExp liveAfter fn
        -- For each arg, check if the variable is live after this call.
        -- If so, tag it with OwnedArg metadata (meaning rc_inc needed).
        args' = map (annotateCallArg liveAfter) args
    in  Typed qt area metadata (Call fn' args')

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata
      (If (annotateExp liveAfter cond)
          (annotateExp liveAfter truthy)
          (annotateExp liveAfter falsy))

  Typed qt area metadata (Where scrutinee branches) ->
    Typed qt area metadata
      (Where (annotateExp liveAfter scrutinee)
             (map (annotateBranch liveAfter) branches))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (annotateExp liveAfter e))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (map (annotateExp liveAfter) items))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (map (mapListItem (annotateExp liveAfter)) items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (map (mapRecordField (annotateExp liveAfter)) fields))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (annotateExp liveAfter rec) field)

  Typed qt area metadata (ArrayAccess arr idx) ->
    Typed qt area metadata (ArrayAccess (annotateExp liveAfter arr) (annotateExp liveAfter idx))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (annotateExp liveAfter cond) (annotateExp liveAfter body))

  _ -> exp


-- | Annotate a function body (list of expressions).
annotateBody :: [Exp] -> [Exp]
annotateBody = annotateDoBlock Set.empty


-- | Annotate a Do block with liveness information flowing backwards.
-- For each expression in the list, compute what's live in the REST of the list.
annotateDoBlock :: Set.Set String -> [Exp] -> [Exp]
annotateDoBlock _ [] = []
annotateDoBlock outerLive [e] =
  -- Last expression: only outer liveness matters
  [annotateExp outerLive e]
annotateDoBlock outerLive (e : rest) =
  let -- Compute free variables in the rest of the block + outer liveness
      restFV   = Set.unions (map freeVarsExp rest) `Set.union` outerLive
      -- Annotate this expression with "what's live after it"
      e'       = annotateExp restFV e
      -- Continue with the rest
      rest'    = annotateDoBlock outerLive rest
  in  e' : rest'


-- | Annotate a call argument.  If the argument is a variable that is
-- in the @liveAfter@ set, tag it with OwnedArg (meaning rc_inc needed).
annotateCallArg :: Set.Set String -> Exp -> Exp
annotateCallArg liveAfter arg = case arg of
  Typed qt area metadata (Var name _)
    | name `Set.member` liveAfter ->
      -- This variable is used after this call — needs rc_inc
      Typed qt area (OwnedArg : metadata) (Var name False)
  _ ->
    -- Not a simple variable, or last use — no annotation needed
    annotateExp liveAfter arg


annotateBranch :: Set.Set String -> Is -> Is
annotateBranch liveAfter is = case is of
  Typed qt area metadata (Is pat body) ->
    Typed qt area metadata (Is pat (annotateExp liveAfter body))
  _ -> is


-- ---------------------------------------------------------------------------
-- Free variable computation (simplified — just collect Var names)
-- ---------------------------------------------------------------------------

freeVarsExp :: Exp -> Set.Set String
freeVarsExp exp = case exp of
  Typed _ _ _ (Var n _)          -> Set.singleton n
  Typed _ _ _ (Assignment _ rhs) -> freeVarsExp rhs
  Typed _ _ _ (Call fn args)     -> Set.unions (freeVarsExp fn : map freeVarsExp args)
  Typed _ _ _ (Do exps)          -> Set.unions (map freeVarsExp exps)
  Typed _ _ _ (If c t f)         -> Set.unions [freeVarsExp c, freeVarsExp t, freeVarsExp f]
  Typed _ _ _ (Where s bs)       -> Set.union (freeVarsExp s) (Set.unions (map freeVarsIs bs))
  Typed _ _ _ (Definition _ body)-> Set.unions (map freeVarsExp body)
  Typed _ _ _ (TupleConstructor es) -> Set.unions (map freeVarsExp es)
  Typed _ _ _ (ListConstructor items) -> Set.unions (map (freeVarsExp . getListItemExp) items)
  Typed _ _ _ (Record fields)    -> Set.unions (map (freeVarsExp . getFieldExp) fields)
  Typed _ _ _ (Access rec _)     -> freeVarsExp rec
  Typed _ _ _ (ArrayAccess a i)  -> Set.union (freeVarsExp a) (freeVarsExp i)
  Typed _ _ _ (While c b)        -> Set.union (freeVarsExp c) (freeVarsExp b)
  Typed _ _ _ (Export e)         -> freeVarsExp e
  _                              -> Set.empty


freeVarsIs :: Is -> Set.Set String
freeVarsIs is = case is of
  Typed _ _ _ (Is _ body) -> freeVarsExp body
  _                       -> Set.empty
