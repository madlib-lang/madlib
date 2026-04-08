{-# LANGUAGE LambdaCase #-}
{-|
Module      : Optimize.ReuseAnalysis
Description : FBIP reuse analysis — annotates constructor calls that can reuse
              the memory of a uniquely-owned pattern-match scrutinee.

= Background

Functional But In-Place (FBIP), as described in the Perceus paper, turns
functional tree/list transformations into in-place updates when the old value
is uniquely owned.  The key insight: if a @where@ branch deconstructs a
constructor and the branch body builds a new constructor of the *same ADT*,
and the scrutinee is not used again after the match, the new constructor can be
allocated by calling @rc_reuse(scrutinee, new_size)@ instead of
@rc_alloc(new_size)@.  @rc_reuse@ returns the same pointer when the refcount
is 1, giving zero-allocation in-place update.

= What this pass does

For every @Where(scrutinee, branches)@ expression where:

1. The scrutinee is a simple variable reference (@Var name@).
2. At least one branch has a @PCon@ pattern *from the same ADT as the scrutinee*.
3. The branch body contains a constructor call to that same ADT.
4. The scrutinee variable does not appear free in the branch body (it is fully
   consumed by the pattern).

…the pass attaches a @ReuseCandidate scrutinee_name@ metadata tag to the
outermost constructor call expression in the branch body.

The LLVM codegen then calls @rc_reuse(scrutinee_ptr, sizeof(new_ctor))@
instead of @rc_alloc(sizeof(new_ctor))@, achieving in-place mutation when
the scrutinee is uniquely owned.

= Safety

- The annotation is purely advisory: @rc_reuse@ falls back to a fresh
  allocation whenever the object is shared (refcount > 1).  So even if the
  analysis is too aggressive, the worst case is an unnecessary @rc_reuse@
  call (which behaves identically to @rc_alloc@).
- The annotation is only added when the scrutinee variable is provably not
  free in the branch body (it is consumed by the pattern binding).
-}
module Optimize.ReuseAnalysis
  ( annotateAST
  ) where

import           AST.Core
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.List   as List
import qualified Infer.Type  as IT


-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

-- | Annotate the AST with ReuseCandidate metadata on eligible constructor
-- calls inside pattern-match branches.
annotateAST :: AST -> AST
annotateAST ast = ast { aexps = map annotateExp (aexps ast) }


-- ---------------------------------------------------------------------------
-- Expression traversal
-- ---------------------------------------------------------------------------

annotateExp :: Exp -> Exp
annotateExp exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (annotateExp rhs))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (annotateExp e))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (map annotateExp body))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (map annotateExp exps))

  Typed qt area metadata (If c t f) ->
    Typed qt area metadata (If (annotateExp c) (annotateExp t) (annotateExp f))

  -- The main case: pattern match.
  Typed qt area metadata (Where scrutinee branches) ->
    let scrutinee' = annotateExp scrutinee
        scrutineeName = getVarName scrutinee'
        scrType       = getExprADTType scrutinee'
        branches'     = map (annotateBranch scrutineeName scrType) branches
    in  Typed qt area metadata (Where scrutinee' branches')

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (annotateExp fn) (map annotateExp args))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (map annotateExp items))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (map (mapListItem annotateExp) items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (map (mapRecordField annotateExp) fields))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (annotateExp rec) field)

  Typed qt area metadata (ArrayAccess arr idx) ->
    Typed qt area metadata (ArrayAccess (annotateExp arr) (annotateExp idx))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (annotateExp cond) (annotateExp body))

  _ -> exp


-- | Annotate a single Is branch.
annotateBranch :: Maybe String   -- ^ Scrutinee variable name (Nothing = complex expr)
               -> Maybe IT.Type  -- ^ ADT type of the scrutinee (Nothing = not an ADT)
               -> Is
               -> Is
annotateBranch Nothing   _       is = is  -- scrutinee not a simple Var, skip
annotateBranch _         Nothing is = is  -- scrutinee not an ADT, skip
annotateBranch (Just scrVar) (Just scrType) is = case is of
  Typed qt area metadata (Is pat body) ->
    -- Only attempt reuse when the pattern is a PCon (constructor deconstruction)
    -- from the same ADT as the scrutinee.
    case getPatternADTType pat of
      Just patType | sameADT scrType patType ->
        -- Check that the scrutinee variable does not appear free in the body
        -- (it is fully consumed by the pattern binding).
        if isFreeIn scrVar body
          then is  -- scrutinee escapes, skip
          else
            let body' = annotateBodyForReuse scrVar body
            in  Typed qt area metadata (Is pat body')
      _ ->
        is  -- pattern is not a matching PCon, skip
  _ -> is


-- | Walk the branch body and attach ReuseCandidate to the *outermost*
-- (first encountered top-level, or in a Do body) constructor call of the
-- matching ADT.
annotateBodyForReuse :: String -> Exp -> Exp
annotateBodyForReuse scrVar body = case body of
  -- Do block: annotate the first eligible constructor call we find
  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (annotateDoExpsForReuse scrVar exps))

  -- Direct constructor call: annotate it
  Typed qt area metadata (Call fn@(Typed _ _ _ (Var _ True)) args) ->
    addReuseMetadata scrVar body

  -- Assignment where RHS is a constructor call
  Typed qt area metadata (Assignment lhs rhs@(Typed _ _ _ (Call (Typed _ _ _ (Var _ True)) _))) ->
    Typed qt area metadata (Assignment lhs (addReuseMetadata scrVar rhs))

  -- Record construction (treated as a constructor)
  Typed qt area metadata (Record _) ->
    addReuseMetadata scrVar body

  -- Also recurse into sub-expressions (if/where inside a branch body)
  _ -> annotateExp body


annotateDoExpsForReuse :: String -> [Exp] -> [Exp]
annotateDoExpsForReuse scrVar [] = []
annotateDoExpsForReuse scrVar (e : rest) =
  case e of
    -- If this statement is a constructor call, annotate and stop
    Typed qt area metadata (Call (Typed _ _ _ (Var _ True)) _) ->
      addReuseMetadata scrVar e : rest

    -- Assignment with constructor RHS
    Typed qt area metadata (Assignment lhs rhs@(Typed _ _ _ (Call (Typed _ _ _ (Var _ True)) _))) ->
      Typed qt area metadata (Assignment lhs (addReuseMetadata scrVar rhs)) : rest

    -- Record literal
    Typed qt area metadata (Record _) ->
      addReuseMetadata scrVar e : rest

    -- Not a constructor call here — keep looking
    _ ->
      e : annotateDoExpsForReuse scrVar rest


-- | Prepend a ReuseCandidate metadata tag to an expression.
addReuseMetadata :: String -> Exp -> Exp
addReuseMetadata scrVar exp = case exp of
  Typed qt area metadata inner ->
    -- Don't add duplicate ReuseCandidate annotations
    if isReuseCandidate metadata
      then exp
      else Typed qt area (ReuseCandidate scrVar : metadata) inner
  _ -> exp


-- ---------------------------------------------------------------------------
-- Helper: is a variable free in an expression?
-- ---------------------------------------------------------------------------

-- | Returns True if the variable @name@ appears free (unbound) in @exp@.
-- "Free" means it is referenced but not re-bound in a sub-pattern.
isFreeIn :: String -> Exp -> Bool
isFreeIn name exp = Set.member name (freeVars exp)

freeVars :: Exp -> Set.Set String
freeVars exp = case exp of
  Typed _ _ _ (Var n _) ->
    Set.singleton n

  Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) rhs) ->
    -- The variable n is being bound, not free.  The RHS may have free vars
    -- but n itself is in the "binder" position.
    Set.delete n (freeVars rhs)

  Typed _ _ _ (Assignment lhs rhs) ->
    Set.union (freeVars lhs) (freeVars rhs)

  Typed _ _ _ (Call fn args) ->
    Set.unions (freeVars fn : map freeVars args)

  Typed _ _ _ (Do exps) ->
    freeVarsDoBlock exps

  Typed _ _ _ (If c t f) ->
    Set.unions [freeVars c, freeVars t, freeVars f]

  Typed _ _ _ (Where scrut branches) ->
    Set.union (freeVars scrut) (Set.unions (map freeVarsIs branches))

  Typed _ _ _ (Definition params body) ->
    let paramNames = Set.fromList [ n | Typed _ _ _ n <- params ]
    in  Set.difference (Set.unions (map freeVars body)) paramNames

  Typed _ _ _ (TupleConstructor exps) ->
    Set.unions (map freeVars exps)

  Typed _ _ _ (ListConstructor items) ->
    Set.unions (map (freeVars . getListItemExp) items)

  Typed _ _ _ (Record fields) ->
    Set.unions (map (freeVars . getFieldExp) fields)

  Typed _ _ _ (Access rec _) ->
    freeVars rec

  Typed _ _ _ (ArrayAccess arr idx) ->
    Set.union (freeVars arr) (freeVars idx)

  Typed _ _ _ (While cond body) ->
    Set.union (freeVars cond) (freeVars body)

  Typed _ _ _ (Export e) ->
    freeVars e

  _ ->
    Set.empty


freeVarsDoBlock :: [Exp] -> Set.Set String
freeVarsDoBlock [] = Set.empty
freeVarsDoBlock (e : rest) = case e of
  -- An assignment binds the name in subsequent expressions
  Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) rhs) ->
    Set.union (freeVars rhs) (Set.delete n (freeVarsDoBlock rest))
  _ ->
    Set.union (freeVars e) (freeVarsDoBlock rest)


freeVarsIs :: Is -> Set.Set String
freeVarsIs is = case is of
  Typed _ _ _ (Is pat body) ->
    Set.difference (freeVars body) (patternBoundVars pat)
  _ -> Set.empty

patternBoundVars :: Pattern -> Set.Set String
patternBoundVars pat = case pat of
  Typed _ _ _ (PVar n)         -> Set.singleton n
  Typed _ _ _ PAny             -> Set.empty
  Typed _ _ _ PNum{}           -> Set.empty
  Typed _ _ _ PBool{}          -> Set.empty
  Typed _ _ _ PStr{}           -> Set.empty
  Typed _ _ _ PChar{}          -> Set.empty
  Typed _ _ _ (PCon _ pats)    -> Set.unions (map patternBoundVars pats)
  Typed _ _ _ (PTuple pats)    -> Set.unions (map patternBoundVars pats)
  Typed _ _ _ (PList pats)     -> Set.unions (map patternBoundVars pats)
  Typed _ _ _ (PSpread p)      -> patternBoundVars p
  Typed _ _ _ (PRecord fps mn) ->
    Set.union
      (Set.unions (map patternBoundVars (Map.elems fps)))
      (maybe Set.empty Set.singleton mn)
  _                            -> Set.empty


-- ---------------------------------------------------------------------------
-- Helper: extract variable name from a scrutinee expression
-- ---------------------------------------------------------------------------

getVarName :: Exp -> Maybe String
getVarName (Typed _ _ _ (Var n _)) = Just n
getVarName _                       = Nothing


-- ---------------------------------------------------------------------------
-- Helper: extract the ADT type from an expression / pattern
-- ---------------------------------------------------------------------------

-- | Return the monomorphized ADT type of an expression, if it is an ADT.
-- We only care about TApp-based or TCon-based types (not primitives).
getExprADTType :: Exp -> Maybe IT.Type
getExprADTType exp = case exp of
  Typed (qt IT.:=> t) _ _ _ -> if isADTType t then Just t else Nothing
  _                         -> Nothing

-- | Return the ADT type that a PCon pattern matches, if applicable.
getPatternADTType :: Pattern -> Maybe IT.Type
getPatternADTType pat = case pat of
  Typed (qt IT.:=> t) _ _ (PCon _ _) -> if isADTType t then Just t else Nothing
  _                                   -> Nothing

-- | True if a type is an ADT (not a primitive, not a function).
isADTType :: IT.Type -> Bool
isADTType t = case t of
  IT.TCon (IT.TC name _) _ _
    | name `elem` ["Integer", "Float", "Boolean", "Byte", "Short", "Char", "Unit", "String"] -> False
    | otherwise -> True
  IT.TApp{}     -> True   -- parameterised ADT
  _             -> False

-- | True if two types refer to the same ADT constructor (ignoring type params).
sameADT :: IT.Type -> IT.Type -> Bool
sameADT a b = adtName a == adtName b

adtName :: IT.Type -> Maybe String
adtName t = case t of
  IT.TCon (IT.TC name _) _ _ -> Just name
  IT.TApp f _                -> adtName f
  _                          -> Nothing


