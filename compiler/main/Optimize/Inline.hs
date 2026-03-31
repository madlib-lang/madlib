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

      Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) (Typed _ _ _ (Definition params body))) ->
        let paramNames = getValue <$> params
            bodySize = sum (exprSize <$> body)
            isSelfRecursive = name `S.member` collectVarNames body
            hasMutations = any containsMutations body
        in  if bodySize <= inlineThreshold && not isSelfRecursive && not hasMutations
            then [(name, InlineCandidate { icParams = paramNames, icBody = body })]
            else []

      _ -> []


-- | Deeply check whether an expression contains any mutation metadata.
-- Top-level check is insufficient: mutations inside If/Do/Where branches
-- would be missed if we only inspect the outer expression's metadata.
containsMutations :: Exp -> Bool
containsMutations exp = case exp of
  Typed _ _ metadata e ->
    any isMutation metadata || containsMutations_ e
  Untyped _ _ e ->
    containsMutations_ e
  where
    isMutation m = case m of
      ReferenceAllocation -> True
      ReferenceStore      -> True
      MutatingFunctionRef -> True
      _                   -> False

containsMutations_ :: Exp_ -> Bool
containsMutations_ e = case e of
  Call fn args          -> containsMutations fn || any containsMutations args
  Definition _ body     -> any containsMutations body
  Assignment lhs rhs    -> containsMutations lhs || containsMutations rhs
  If c t f              -> any containsMutations [c, t, f]
  Where exp' iss        -> containsMutations exp' || any containsMutationsIs iss
  Do exps               -> any containsMutations exps
  Access a b            -> containsMutations a || containsMutations b
  ArrayAccess a b       -> containsMutations a || containsMutations b
  ListConstructor items -> any containsMutationsListItem items
  TupleConstructor exps -> any containsMutations exps
  Record fields         -> any containsMutationsField fields
  While c b             -> containsMutations c || containsMutations b
  Export e              -> containsMutations e
  _                     -> False

containsMutationsIs :: Is -> Bool
containsMutationsIs (Typed _ _ _ (Is _ body)) = containsMutations body
containsMutationsIs _                          = False

containsMutationsListItem :: ListItem -> Bool
containsMutationsListItem (Typed _ _ _ (ListItem e))   = containsMutations e
containsMutationsListItem (Typed _ _ _ (ListSpread e)) = containsMutations e
containsMutationsListItem _                             = False

containsMutationsField :: Field -> Bool
containsMutationsField (Typed _ _ _ (Field (_, e)))  = containsMutations e
containsMutationsField (Typed _ _ _ (FieldSpread e)) = containsMutations e
containsMutationsField _                             = False


-- | Find inline candidates safe for cross-module inlining.
-- Includes functions whose body only references their own parameters
-- and other safe inline candidates from the same module (transitive inlining).
-- Iterates to a fixed point: if candidate A references candidate B,
-- and B is unsafe, then A must also be excluded.
findCrossModuleInlineCandidates :: [Exp] -> InlineCandidates
findCrossModuleInlineCandidates exps =
  let allCandidates = findInlineCandidates exps
  in  iterateUntilStable allCandidates
  where
    iterateUntilStable candidates =
      let filtered = M.filter (isSafe candidates) candidates
      in  if M.size filtered == M.size candidates
          then filtered
          else iterateUntilStable filtered

    isSafe allCandidates candidate =
      let paramSet = S.fromList (icParams candidate)
          freeVars = collectVarNames (icBody candidate)
          -- Only consider module-level references (prefixed with _) as problematic.
          -- Operators and built-ins (like +, *, -, /, ==, etc.) are always available.
          moduleFreeVars = S.filter isModuleName freeVars
          -- References to other candidates are safe ONLY if they appear exclusively
          -- in direct-call position with matching arity (transitive inlining resolves them).
          -- References used as values (e.g., passed to a HOF) would remain dangling.
          directCallNames = collectDirectCallNames allCandidates (icBody candidate)
          nonDirectRefs = moduleFreeVars `S.difference` paramSet `S.difference` directCallNames
      in  S.null nonDirectRefs

    isModuleName name = case name of
      ('_':_) -> True
      _       -> False


-- | Collect names that appear ONLY in direct-call position with matching arity
-- to known candidates. If a name appears both as a direct call and as a value,
-- it is excluded (the value reference would remain dangling after inlining).
collectDirectCallNames :: InlineCandidates -> [Exp] -> S.Set Name
collectDirectCallNames candidates body =
  let (callNames, valueNames) = foldl (\acc e -> collectCallsAndValues candidates acc e) (S.empty, S.empty) body
  in  callNames `S.difference` valueNames

collectCallsAndValues :: InlineCandidates -> (S.Set Name, S.Set Name) -> Exp -> (S.Set Name, S.Set Name)
collectCallsAndValues candidates acc (Typed _ _ _ e) = collectCallsAndValues_ candidates acc e
collectCallsAndValues candidates acc (Untyped _ _ e) = collectCallsAndValues_ candidates acc e

collectCallsAndValues_ :: InlineCandidates -> (S.Set Name, S.Set Name) -> Exp_ -> (S.Set Name, S.Set Name)
collectCallsAndValues_ candidates (calls, values) e = case e of
  -- Direct call to a candidate with matching arity → safe
  Call (Typed _ _ _ (Var fnName _)) args
    | Just candidate <- M.lookup fnName candidates
    , L.length args == L.length (icParams candidate) ->
      let (calls', values') = foldl (collectCallsAndValues candidates) (calls, values) args
      in  (S.insert fnName calls', values')

  -- Any other Var reference → unsafe (used as value)
  Var name _           -> (calls, S.insert name values)

  -- Recurse into subexpressions
  Call fn args          -> foldl (collectCallsAndValues candidates) (collectCallsAndValues candidates (calls, values) fn) args
  Definition _ body     -> foldl (collectCallsAndValues candidates) (calls, values) body
  Assignment lhs rhs    -> collectCallsAndValues candidates (collectCallsAndValues candidates (calls, values) lhs) rhs
  If c t f              -> foldl (collectCallsAndValues candidates) (calls, values) [c, t, f]
  Where exp iss         -> let acc' = collectCallsAndValues candidates (calls, values) exp
                           in  foldl (\a (Typed _ _ _ (Is _ ie)) -> collectCallsAndValues candidates a ie) acc' iss
  Do exps               -> foldl (collectCallsAndValues candidates) (calls, values) exps
  Access a b            -> collectCallsAndValues candidates (collectCallsAndValues candidates (calls, values) a) b
  ArrayAccess a b       -> collectCallsAndValues candidates (collectCallsAndValues candidates (calls, values) a) b
  ListConstructor items -> foldl (\a item -> case item of
                              Typed _ _ _ (ListItem ie) -> collectCallsAndValues candidates a ie
                              Typed _ _ _ (ListSpread ie) -> collectCallsAndValues candidates a ie
                              _ -> a) (calls, values) items
  TupleConstructor exps -> foldl (collectCallsAndValues candidates) (calls, values) exps
  Record fields         -> foldl (\a f -> case f of
                              Typed _ _ _ (Field (_, fe)) -> collectCallsAndValues candidates a fe
                              Typed _ _ _ (FieldSpread fe) -> collectCallsAndValues candidates a fe
                              _ -> a) (calls, values) fields
  Export ie             -> collectCallsAndValues candidates (calls, values) ie
  While c b             -> collectCallsAndValues candidates (collectCallsAndValues candidates (calls, values) c) b
  _                     -> (calls, values)


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
  While c b             -> S.union (collectVarsInExp c) (collectVarsInExp b)
  _                     -> S.empty
  where
    isVars (Typed _ _ _ (Is pat e))           = S.union (collectPatternConstructorNames pat) (collectVarsInExp e)
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
    , L.length args == L.length (icParams candidate)
    , all isTriviallyPureExp args ->
      let inlinedArgs = inlineInExp candidates <$> args
          substitution = M.fromList $ L.zip (icParams candidate) inlinedArgs
          freeArgVars  = foldl (\acc e -> S.union acc (collectVarsInExp e)) S.empty inlinedArgs
          safeBody     = avoidCapture freeArgVars (icBody candidate)
          inlinedBody  = substituteInExps substitution safeBody
          -- Recursively inline in the result to handle transitive inlining
          -- (e.g., compose3 -> addOne(double(square(x))) -> inline addOne, double, square)
          result = case inlinedBody of
            [singleExp] -> singleExp
            multipleExps -> Typed qt area meta (Do multipleExps)
      in  inlineInExp (M.delete fnName candidates) result

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


-- | Rename a local binding name throughout a list of expressions.
-- Used to avoid variable capture during inlining: if the body locally binds
-- a name that also appears free in a substituted argument, the local binding
-- must be renamed so that subsequent substitutions don't accidentally resolve
-- the argument variable to the local binding.
renameInExps :: Name -> Name -> [Exp] -> [Exp]
renameInExps oldName newName = map (renameInExp oldName newName)

renameInExp :: Name -> Name -> Exp -> Exp
renameInExp oldName newName exp@(Typed qt area meta e) = case e of
  Var n isCtor | n == oldName ->
    Typed qt area meta (Var newName isCtor)

  Assignment (Typed lhsQt lhsArea lhsMeta (Var n isCtor)) rhs
    | n == oldName ->
      -- Rename both the LHS binding and in the RHS
      let rhs' = renameInExp oldName newName rhs
      in  Typed qt area meta (Assignment (Typed lhsQt lhsArea lhsMeta (Var newName isCtor)) rhs')

  Assignment lhs rhs ->
    Typed qt area meta (Assignment (renameInExp oldName newName lhs) (renameInExp oldName newName rhs))

  Definition params body ->
    -- Don't rename inside nested lambdas that rebind oldName as a parameter
    if any (\p -> getValue p == oldName) params then
      exp
    else
      Typed qt area meta (Definition params (renameInExps oldName newName body))

  Call fn args ->
    Typed qt area meta (Call (renameInExp oldName newName fn) (renameInExp oldName newName <$> args))

  If c t f ->
    Typed qt area meta (If (renameInExp oldName newName c) (renameInExp oldName newName t) (renameInExp oldName newName f))

  Where e iss ->
    Typed qt area meta (Where (renameInExp oldName newName e)
      (map (\(Typed qt' area' meta' (Is pat isBody)) ->
        -- Don't rename inside branches that bind oldName in their pattern
        if oldName `S.member` collectPatternVars pat then
          Typed qt' area' meta' (Is pat isBody)
        else
          Typed qt' area' meta' (Is pat (renameInExp oldName newName isBody))
      ) iss))

  Do exps ->
    Typed qt area meta (Do (renameInExps oldName newName exps))

  Access a b ->
    Typed qt area meta (Access (renameInExp oldName newName a) (renameInExp oldName newName b))

  ArrayAccess a b ->
    Typed qt area meta (ArrayAccess (renameInExp oldName newName a) (renameInExp oldName newName b))

  ListConstructor items ->
    Typed qt area meta (ListConstructor (map (mapListItem (renameInExp oldName newName)) items))

  TupleConstructor exps ->
    Typed qt area meta (TupleConstructor (renameInExp oldName newName <$> exps))

  Record fields ->
    Typed qt area meta (Record (map (mapRecordField (renameInExp oldName newName)) fields))

  While c b ->
    Typed qt area meta (While (renameInExp oldName newName c) (renameInExp oldName newName b))

  Export e ->
    Typed qt area meta (Export (renameInExp oldName newName e))

  _ -> exp
renameInExp _ _ exp = exp


-- | Collect top-level let-binding names from a sequential body (not inside nested lambdas).
collectTopLevelBindingNames :: [Exp] -> S.Set Name
collectTopLevelBindingNames = foldl go S.empty
  where
    go acc (Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) _)) = S.insert n acc
    go acc _                                                     = acc


-- | Before substituting into a body, rename any local bindings that clash with
-- free variables in the argument expressions. This avoids variable capture where
-- a substituted Var "z" would accidentally resolve to an inner binding named "z".
avoidCapture :: S.Set Name -> [Exp] -> [Exp]
avoidCapture freeArgVars body =
  let localBindings  = collectTopLevelBindingNames body
      clashingNames  = S.intersection localBindings freeArgVars
  in  S.foldl renameClash body clashingNames
  where
    renameClash b n = renameInExps n ("$" <> n <> "_inline") b


-- | Substitute parameter names with argument expressions in a list of body expressions.
-- Processes sequentially so that assignments shadow substitutions for subsequent expressions.
substituteInExps :: M.Map Name Exp -> [Exp] -> [Exp]
substituteInExps subst exps = case exps of
  [] -> []
  (exp : rest) ->
    let (exp', subst') = substituteInExpWithShadow subst exp
    in  exp' : substituteInExps subst' rest


-- | Substitute in an expression and return the updated substitution map
-- (with any newly-bound names removed for subsequent expressions in the same scope).
substituteInExpWithShadow :: M.Map Name Exp -> Exp -> (Exp, M.Map Name Exp)
substituteInExpWithShadow subst exp@(Typed qt area meta e) = case e of
  Assignment (Typed lhsQt lhsArea lhsMeta (Var name isCtor)) rhs ->
    -- Substitute only in the RHS, not in the LHS variable name.
    -- Then shadow this name so subsequent expressions don't substitute it.
    let rhs' = substituteInExp subst rhs
        subst' = M.delete name subst
    in  (Typed qt area meta (Assignment (Typed lhsQt lhsArea lhsMeta (Var name isCtor)) rhs'), subst')

  _ -> (substituteInExp subst exp, subst)

substituteInExpWithShadow subst exp = (exp, subst)


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
    in  Typed qt area meta (Definition params (substituteInExps subst' body))

  Assignment lhs rhs ->
    Typed qt area meta (Assignment lhs (substituteInExp subst rhs))

  If c t f ->
    Typed qt area meta (If (substituteInExp subst c) (substituteInExp subst t) (substituteInExp subst f))

  Where e iss ->
    Typed qt area meta (Where (substituteInExp subst e) (substIs subst <$> iss))

  Do exps ->
    Typed qt area meta (Do (substituteInExps subst exps))

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
  PRecord fields restName -> maybe id S.insert restName $ S.unions (collectPatternVars <$> M.elems fields)
  PList pats       -> S.unions (collectPatternVars <$> pats)
  PTuple pats      -> S.unions (collectPatternVars <$> pats)
  PSpread p        -> collectPatternVars p
  _                -> S.empty
collectPatternVars (Untyped _ _ _) = S.empty


-- | Collect constructor names referenced in a pattern (for cross-module safety checks)
collectPatternConstructorNames :: Pattern -> S.Set Name
collectPatternConstructorNames (Typed _ _ _ p) = case p of
  PCon name pats -> S.insert name (S.unions (collectPatternConstructorNames <$> pats))
  PRecord fields _ -> S.unions (collectPatternConstructorNames <$> M.elems fields)
  PList pats       -> S.unions (collectPatternConstructorNames <$> pats)
  PTuple pats      -> S.unions (collectPatternConstructorNames <$> pats)
  PSpread p        -> collectPatternConstructorNames p
  _                -> S.empty
collectPatternConstructorNames (Untyped _ _ _) = S.empty


-- Keep inlining conservative: substituting effectful arguments into function
-- bodies can duplicate, drop, or reorder effects relative to call-by-value
-- evaluation. We only inline when call arguments are trivially pure.
isTriviallyPureExp :: Exp -> Bool
isTriviallyPureExp exp = case exp of
  Typed _ _ _ (Literal _) ->
    True

  Typed _ _ _ (Var _ _) ->
    True

  Typed _ _ _ (TupleConstructor items) ->
    all isTriviallyPureExp items

  Typed _ _ _ (ListConstructor items) ->
    all (isTriviallyPureExp . getListItemExp) items

  Typed _ _ _ (Record fields) ->
    all (isTriviallyPureExp . getFieldExp) fields

  _ ->
    False
