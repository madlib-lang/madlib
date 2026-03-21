{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Optimize.Inline
  ( inlineAST
  , findInlineCandidates
  , findCrossModuleInlineCandidates
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import           AST.Core


-- | Maximum expression size for a function to be considered for inlining
inlineThreshold :: Int
inlineThreshold = 10


-- | Run inlining on the entire AST with cross-module candidates.
inlineAST :: InlineCandidates -> AST -> AST
inlineAST externalCandidates ast =
  let localCandidates = findInlineCandidates (aexps ast)
      candidates = M.union localCandidates externalCandidates
  in  if M.null candidates then ast
      else ast { aexps = inlineInTopLevel candidates <$> aexps ast }


-- | Find all top-level functions eligible for inlining.
-- A function is eligible if:
--   1. It has a small body (size <= threshold)
--   2. It is not recursive (does not reference itself)
--   3. It has no mutation metadata (no ReferenceAllocation/ReferenceStore)
findInlineCandidates :: [Exp] -> InlineCandidates
findInlineCandidates exps = M.fromList $ concatMap extractCandidate exps
  where
    extractCandidate exp = case exp of
      Typed _ _ _ (Export e) ->
        extractCandidate e

      Typed qt _ _ (Assignment (Typed _ _ _ (Var name _)) (Typed _ _ _ (Definition params body))) ->
        let paramNames = getValue <$> params
            bodySize = sum (exprSize <$> body)
            isSelfRecursive = name `S.member` collectVarNames body
            hasMutations = any (any isMutationMetadata . getMetadata) body
        in  if bodySize <= inlineThreshold && not isSelfRecursive && not hasMutations
            then [(name, InlineCandidate { icParams = paramNames, icBody = body })]
            else []

      _ -> []

    isMutationMetadata :: Metadata -> Bool
    isMutationMetadata m = case m of
      ReferenceAllocation -> True
      ReferenceStore      -> True
      MutatingFunctionRef -> True
      _                   -> False


-- | Find inline candidates safe for cross-module inlining.
-- Includes functions whose body only references their own parameters
-- and other inline candidates from the same module (transitive inlining).
findCrossModuleInlineCandidates :: [Exp] -> InlineCandidates
findCrossModuleInlineCandidates exps =
  let allCandidates = findInlineCandidates exps
      candidateNames = M.keysSet allCandidates
  in  M.filter (isSafe candidateNames) allCandidates
  where
    isSafe candidateNames candidate =
      let paramSet = S.fromList (icParams candidate)
          freeVars = collectVarNames (icBody candidate)
          -- Only consider module-level references (prefixed with _) as problematic.
          -- Operators and built-ins (like +, *, -, /, ==, etc.) are always available.
          moduleFreeVars = S.filter isModuleName freeVars
      in  moduleFreeVars `S.isSubsetOf` S.union paramSet candidateNames

    isModuleName name = case name of
      ('_':_) -> True
      _       -> False


-- | Compute the size of an expression (number of nodes)
exprSize :: Exp -> Int
exprSize (Typed _ _ _ e) = case e of
  Literal _             -> 1
  Var _ _               -> 1
  Call fn args           -> 1 + exprSize fn + sum (exprSize <$> args)
  Definition _ body      -> 1 + sum (exprSize <$> body)
  Assignment lhs rhs     -> 1 + exprSize lhs + exprSize rhs
  If c t f               -> 1 + exprSize c + exprSize t + exprSize f
  Where e iss            -> 1 + exprSize e + sum (isSize <$> iss)
  Do exps                -> sum (exprSize <$> exps)
  Access a b             -> 1 + exprSize a + exprSize b
  ArrayAccess a b        -> 1 + exprSize a + exprSize b
  ListConstructor items  -> 1 + sum (listItemSize <$> items)
  TupleConstructor exps  -> 1 + sum (exprSize <$> exps)
  Record fields          -> 1 + sum (fieldSize <$> fields)
  Export e               -> exprSize e
  NameExport _           -> 1
  Extern _ _ _           -> 1
  While c b              -> 1 + exprSize c + exprSize b
  JSExp _                -> 1
  TypedHole              -> 1
  where
    isSize (Typed _ _ _ (Is _ e)) = 1 + exprSize e
    listItemSize (Typed _ _ _ (ListItem e))   = exprSize e
    listItemSize (Typed _ _ _ (ListSpread e)) = exprSize e
    fieldSize (Typed _ _ _ (Field (_, e)))  = 1 + exprSize e
    fieldSize (Typed _ _ _ (FieldSpread e)) = exprSize e
exprSize (Untyped _ _ e) = case e of
  Literal _ -> 1
  Var _ _   -> 1
  _         -> 1


-- | Collect all variable names referenced in expressions
collectVarNames :: [Exp] -> S.Set Name
collectVarNames = foldl (\acc e -> S.union acc (collectVarsInExp e)) S.empty

collectVarsInExp :: Exp -> S.Set Name
collectVarsInExp (Typed _ _ _ e) = collectVarsInExp_ e
collectVarsInExp (Untyped _ _ e) = collectVarsInExp_ e

collectVarsInExp_ :: Exp_ -> S.Set Name
collectVarsInExp_ e = case e of
  Var name _           -> S.singleton name
  Call fn args          -> S.union (collectVarsInExp fn) (collectVarNames args)
  Definition _ body     -> collectVarNames body
  Assignment lhs rhs    -> S.union (collectVarsInExp lhs) (collectVarsInExp rhs)
  If c t f              -> S.unions [collectVarsInExp c, collectVarsInExp t, collectVarsInExp f]
  Where exp iss         -> S.union (collectVarsInExp exp) (S.unions (isVars <$> iss))
  Do exps               -> collectVarNames exps
  Access a b            -> S.union (collectVarsInExp a) (collectVarsInExp b)
  ArrayAccess a b       -> S.union (collectVarsInExp a) (collectVarsInExp b)
  ListConstructor items -> S.unions (listItemVars <$> items)
  TupleConstructor exps -> collectVarNames exps
  Record fields         -> S.unions (fieldVars <$> fields)
  Export e              -> collectVarsInExp e
  _                     -> S.empty
  where
    isVars (Typed _ _ _ (Is _ e))             = collectVarsInExp e
    listItemVars (Typed _ _ _ (ListItem e))   = collectVarsInExp e
    listItemVars (Typed _ _ _ (ListSpread e)) = collectVarsInExp e
    fieldVars (Typed _ _ _ (Field (_, e)))    = collectVarsInExp e
    fieldVars (Typed _ _ _ (FieldSpread e))   = collectVarsInExp e


-- | Inline calls in top-level expressions, also removing definitions that are inlined
inlineInTopLevel :: InlineCandidates -> Exp -> Exp
inlineInTopLevel candidates exp = case exp of
  Typed qt area meta (Export e) ->
    Typed qt area meta (Export (inlineInTopLevel candidates e))

  -- Don't inline the definition of the candidate itself - it's kept since it may
  -- be exported or referenced from other modules
  Typed qt area meta (Assignment lhs e) ->
    Typed qt area meta (Assignment lhs (inlineInExp candidates e))

  _ ->
    inlineInExp candidates exp


-- | Inline eligible calls within an expression
inlineInExp :: InlineCandidates -> Exp -> Exp
inlineInExp candidates exp@(Typed qt area meta e) = case e of
  -- Call to a known inline candidate with matching arity
  Call (Typed _ _ _ (Var fnName _)) args
    | Just candidate <- M.lookup fnName candidates
    , L.length args == L.length (icParams candidate) ->
      let inlinedArgs = inlineInExp candidates <$> args
          substitution = M.fromList $ L.zip (icParams candidate) inlinedArgs
          inlinedBody = substituteInExps substitution (icBody candidate)
          -- Recursively inline in the result to handle transitive inlining
          -- (e.g., compose3 -> addOne(double(square(x))) -> inline addOne, double, square)
          result = case inlinedBody of
            [singleExp] -> singleExp
            multipleExps -> Typed qt area meta (Do multipleExps)
      in  inlineInExp candidates result

  -- Recurse into subexpressions
  Call fn args ->
    Typed qt area meta (Call (inlineInExp candidates fn) (inlineInExp candidates <$> args))

  Definition params body ->
    Typed qt area meta (Definition params (inlineInExp candidates <$> body))

  Assignment lhs rhs ->
    Typed qt area meta (Assignment (inlineInExp candidates lhs) (inlineInExp candidates rhs))

  If c t f ->
    Typed qt area meta (If (inlineInExp candidates c) (inlineInExp candidates t) (inlineInExp candidates f))

  Where e iss ->
    Typed qt area meta (Where (inlineInExp candidates e) (inlineIs candidates <$> iss))

  Do exps ->
    Typed qt area meta (Do (inlineInExp candidates <$> exps))

  Access a b ->
    Typed qt area meta (Access (inlineInExp candidates a) (inlineInExp candidates b))

  ArrayAccess a b ->
    Typed qt area meta (ArrayAccess (inlineInExp candidates a) (inlineInExp candidates b))

  ListConstructor items ->
    Typed qt area meta (ListConstructor (inlineListItem candidates <$> items))

  TupleConstructor exps ->
    Typed qt area meta (TupleConstructor (inlineInExp candidates <$> exps))

  Record fields ->
    Typed qt area meta (Record (inlineField candidates <$> fields))

  Export e ->
    Typed qt area meta (Export (inlineInExp candidates e))

  While c b ->
    Typed qt area meta (While (inlineInExp candidates c) (inlineInExp candidates b))

  _ -> exp

inlineInExp candidates exp@(Untyped _ _ _) = exp


inlineIs :: InlineCandidates -> Is -> Is
inlineIs candidates (Typed qt area meta (Is pat e)) =
  Typed qt area meta (Is pat (inlineInExp candidates e))

inlineListItem :: InlineCandidates -> ListItem -> ListItem
inlineListItem candidates (Typed qt area meta li) = case li of
  ListItem e   -> Typed qt area meta (ListItem (inlineInExp candidates e))
  ListSpread e -> Typed qt area meta (ListSpread (inlineInExp candidates e))

inlineField :: InlineCandidates -> Field -> Field
inlineField candidates (Typed qt area meta f) = case f of
  Field (name, e)  -> Typed qt area meta (Field (name, inlineInExp candidates e))
  FieldSpread e    -> Typed qt area meta (FieldSpread (inlineInExp candidates e))


-- | Substitute parameter names with argument expressions in a list of body expressions
substituteInExps :: M.Map Name Exp -> [Exp] -> [Exp]
substituteInExps subst = map (substituteInExp subst)

substituteInExp :: M.Map Name Exp -> Exp -> Exp
substituteInExp subst exp@(Typed qt area meta e) = case e of
  Var name _ | Just replacement <- M.lookup name subst ->
    replacement

  Call fn args ->
    Typed qt area meta (Call (substituteInExp subst fn) (substituteInExp subst <$> args))

  Definition params body ->
    -- Shadow substitution for params
    let paramNames = S.fromList (getValue <$> params)
        subst' = M.filterWithKey (\k _ -> k `S.notMember` paramNames) subst
    in  Typed qt area meta (Definition params (substituteInExp subst' <$> body))

  Assignment lhs rhs ->
    Typed qt area meta (Assignment (substituteInExp subst lhs) (substituteInExp subst rhs))

  If c t f ->
    Typed qt area meta (If (substituteInExp subst c) (substituteInExp subst t) (substituteInExp subst f))

  Where e iss ->
    Typed qt area meta (Where (substituteInExp subst e) (substIs subst <$> iss))

  Do exps ->
    Typed qt area meta (Do (substituteInExp subst <$> exps))

  Access a b ->
    Typed qt area meta (Access (substituteInExp subst a) (substituteInExp subst b))

  ArrayAccess a b ->
    Typed qt area meta (ArrayAccess (substituteInExp subst a) (substituteInExp subst b))

  ListConstructor items ->
    Typed qt area meta (ListConstructor (substListItem subst <$> items))

  TupleConstructor exps ->
    Typed qt area meta (TupleConstructor (substituteInExp subst <$> exps))

  Record fields ->
    Typed qt area meta (Record (substField subst <$> fields))

  Export e ->
    Typed qt area meta (Export (substituteInExp subst e))

  While c b ->
    Typed qt area meta (While (substituteInExp subst c) (substituteInExp subst b))

  _ -> exp

substituteInExp _ exp@(Untyped _ _ _) = exp


substIs :: M.Map Name Exp -> Is -> Is
substIs subst (Typed qt area meta (Is pat e)) =
  -- Shadow pattern-bound variables
  let patVars = collectPatternVars pat
      subst' = M.filterWithKey (\k _ -> k `S.notMember` patVars) subst
  in  Typed qt area meta (Is pat (substituteInExp subst' e))

substListItem :: M.Map Name Exp -> ListItem -> ListItem
substListItem subst (Typed qt area meta li) = case li of
  ListItem e   -> Typed qt area meta (ListItem (substituteInExp subst e))
  ListSpread e -> Typed qt area meta (ListSpread (substituteInExp subst e))

substField :: M.Map Name Exp -> Field -> Field
substField subst (Typed qt area meta f) = case f of
  Field (name, e)  -> Typed qt area meta (Field (name, substituteInExp subst e))
  FieldSpread e    -> Typed qt area meta (FieldSpread (substituteInExp subst e))


-- | Collect variable names bound by a pattern
collectPatternVars :: Pattern -> S.Set Name
collectPatternVars (Typed _ _ _ p) = case p of
  PVar name        -> S.singleton name
  PAny             -> S.empty
  PCon _ pats      -> S.unions (collectPatternVars <$> pats)
  PRecord fields _ -> S.unions (collectPatternVars <$> M.elems fields)
  PList pats       -> S.unions (collectPatternVars <$> pats)
  PTuple pats      -> S.unions (collectPatternVars <$> pats)
  PSpread p        -> collectPatternVars p
  _                -> S.empty
collectPatternVars (Untyped _ _ _) = S.empty
