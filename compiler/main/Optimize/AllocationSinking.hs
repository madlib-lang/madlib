{-# LANGUAGE LambdaCase #-}
{-|
Module      : Optimize.AllocationSinking
Description : O3 optimization that removes short-lived record allocations.

This pass is meant to reduce runtime allocations (and therefore GC pressure) in
very local patterns where a record is created and immediately consumed by a
single field access.

It runs in the LLVM O3 pipeline (after simplification and before closure
conversion) and only rewrites proven-safe cases:

* The bound record value is used exactly once.
* The use happens in the immediately following expression.
* The use is a direct field access on that record.
* No risky control flow or mutation-like structure is involved.
* The record does not escape to later expressions.

When all checks pass, the pass substitutes the selected field value directly,
avoiding allocation of the temporary record.
-}
module Optimize.AllocationSinking
  ( sinkAST
  ) where

import           AST.Core
import qualified Data.Map as Map
import qualified Data.Set as Set

sinkAST :: AST -> AST
sinkAST ast = ast { aexps = map sinkExp (aexps ast) }

sinkExp :: Exp -> Exp
sinkExp exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    Typed qt area metadata (Assignment lhs (sinkExp rhs))
  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (sinkExp e))
  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (sinkDoBody body))
  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (sinkDoBody exps))
  Typed qt area metadata (If c t f) ->
    Typed qt area metadata (If (sinkExp c) (sinkExp t) (sinkExp f))
  Typed qt area metadata (Where scrutinee branches) ->
    let branches' = map mapBranch branches
        mapBranch (Typed qt' area' md (Is pat body)) =
          Typed qt' area' md (Is pat (sinkExp body))
        mapBranch b = b
    in Typed qt area metadata (Where (sinkExp scrutinee) branches')
  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (sinkExp fn) (map sinkExp args))
  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (map sinkExp items))
  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (map (mapListItem sinkExp) items))
  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (map (mapRecordField sinkExp) fields))
  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (sinkExp rec) (sinkExp field))
  Typed qt area metadata (ArrayAccess arr idx) ->
    Typed qt area metadata (ArrayAccess (sinkExp arr) (sinkExp idx))
  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (sinkExp cond) (sinkExp body))
  _ -> exp

sinkDoBody :: [Exp] -> [Exp]
sinkDoBody exps = go [] (map sinkExp exps)
  where
    go acc [] = reverse acc
    go acc (e:rest) =
      case trySink e rest of
        Just (replacement, rest') -> go acc (replacement : rest')
        Nothing                   -> go (e:acc) rest

trySink :: Exp -> [Exp] -> Maybe (Exp, [Exp])
trySink e rest = do
  (recordName, fields) <- boundRecord e
  -- We only sink very local single-use patterns in the next expression.
  next <- case rest of
    []     -> Nothing
    (x:xs) -> Just (x, xs)
  let (nextExp, tailExps) = next
  if not (isSafeImmediateUse recordName nextExp) then
    Nothing
  else do
    replacement <- rewriteRecordAccess recordName fields nextExp
    let usedInTail = any (usesVar recordName) tailExps
    if usedInTail || usesVar recordName replacement then
      Nothing
    else
      Just (replacement, tailExps)

isSafeImmediateUse :: String -> Exp -> Bool
isSafeImmediateUse name exp =
  countVarUses name exp == 1
  && not (hasRiskyControlFlow exp)

boundRecord :: Exp -> Maybe (String, Map.Map String Exp)
boundRecord exp = case exp of
  Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) (Typed _ _ _ (Record fields))) ->
    case recordFieldMap fields of
      Just m  -> Just (name, m)
      Nothing -> Nothing
  _ -> Nothing

recordFieldMap :: [Field] -> Maybe (Map.Map String Exp)
recordFieldMap fields = foldr addField (Just Map.empty) fields
  where
    addField _ Nothing = Nothing
    addField field (Just acc) = case field of
      Typed _ _ _ (Field (name, value))
        | isSimpleValue value -> Just (Map.insert name value acc)
      -- Spreads and non-simple expressions are skipped for safety.
      _ -> Nothing

isSimpleValue :: Exp -> Bool
isSimpleValue exp = case exp of
  Typed _ _ _ (Var _ _)     -> True
  Typed _ _ _ (Literal _)   -> True
  Typed _ _ _ (TupleConstructor xs) -> all isSimpleValue xs
  _                         -> False

rewriteRecordAccess :: String -> Map.Map String Exp -> Exp -> Maybe Exp
rewriteRecordAccess name fields exp = case exp of
  Typed qt area metadata (Assignment lhs rhs) ->
    fmap (Typed qt area metadata . Assignment lhs) (rewriteRecordAccess name fields rhs)
  Typed qt area metadata (Access (Typed _ _ _ (Var varName _)) (Typed _ _ _ (Var ('.':fieldName) _)))
    | varName == name ->
      Map.lookup fieldName fields
  _ -> Nothing

countVarUses :: String -> Exp -> Int
countVarUses needle exp = case exp of
  Typed _ _ _ (Var name _) -> if name == needle then 1 else 0
  Typed _ _ _ (Call fn args) -> countVarUses needle fn + sum (map (countVarUses needle) args)
  Typed _ _ _ (Access rec field) -> countVarUses needle rec + countVarUses needle field
  Typed _ _ _ (ArrayAccess arr idx) -> countVarUses needle arr + countVarUses needle idx
  Typed _ _ _ (Assignment _ rhs) -> countVarUses needle rhs
  Typed _ _ _ (Definition _ body) -> sum (map (countVarUses needle) body)
  Typed _ _ _ (Do exps) -> sum (map (countVarUses needle) exps)
  Typed _ _ _ (If c t f) -> countVarUses needle c + countVarUses needle t + countVarUses needle f
  Typed _ _ _ (Where s branches) -> countVarUses needle s + sum (map (countVarUsesBranch needle) branches)
  Typed _ _ _ (TupleConstructor exps) -> sum (map (countVarUses needle) exps)
  Typed _ _ _ (ListConstructor items) -> sum (map (countVarUsesListItem needle) items)
  Typed _ _ _ (Record fields) -> sum (map (countVarUsesField needle) fields)
  Typed _ _ _ (While c b) -> countVarUses needle c + countVarUses needle b
  Typed _ _ _ (Export e) -> countVarUses needle e
  _ -> 0

countVarUsesBranch :: String -> Is -> Int
countVarUsesBranch needle is = case is of
  Typed _ _ _ (Is _ body) -> countVarUses needle body
  _ -> 0

countVarUsesListItem :: String -> ListItem -> Int
countVarUsesListItem needle li = case li of
  Typed _ _ _ (ListItem e)   -> countVarUses needle e
  Typed _ _ _ (ListSpread e) -> countVarUses needle e
  _ -> 0

countVarUsesField :: String -> Field -> Int
countVarUsesField needle field = case field of
  Typed _ _ _ (Field (_, e))  -> countVarUses needle e
  Typed _ _ _ (FieldSpread e) -> countVarUses needle e
  _ -> 0

hasRiskyControlFlow :: Exp -> Bool
hasRiskyControlFlow exp = case exp of
  Typed _ _ _ (Definition _ _) -> True
  Typed _ _ _ (Do _)           -> True
  Typed _ _ _ (If _ _ _)       -> True
  Typed _ _ _ (Where _ _)      -> True
  Typed _ _ _ (While _ _)      -> True
  _ -> False

usesVar :: String -> Exp -> Bool
usesVar needle exp = not (Set.null (collectVars exp `Set.intersection` Set.singleton needle))

collectVars :: Exp -> Set.Set String
collectVars exp = case exp of
  Typed _ _ _ (Var name _) -> Set.singleton name
  Typed _ _ _ (Call fn args) -> Set.unions (collectVars fn : map collectVars args)
  Typed _ _ _ (Access rec field) -> Set.union (collectVars rec) (collectVars field)
  Typed _ _ _ (ArrayAccess arr idx) -> Set.union (collectVars arr) (collectVars idx)
  Typed _ _ _ (Assignment _ rhs) -> collectVars rhs
  Typed _ _ _ (Definition _ body) -> Set.unions (map collectVars body)
  Typed _ _ _ (Do exps) -> Set.unions (map collectVars exps)
  Typed _ _ _ (If c t f) -> Set.unions [collectVars c, collectVars t, collectVars f]
  Typed _ _ _ (Where s branches) -> Set.union (collectVars s) (Set.unions (map collectVarsBranch branches))
  Typed _ _ _ (TupleConstructor exps) -> Set.unions (map collectVars exps)
  Typed _ _ _ (ListConstructor items) -> Set.unions (map collectVarsListItem items)
  Typed _ _ _ (Record fields) -> Set.unions (map collectVarsField fields)
  Typed _ _ _ (While c b) -> Set.union (collectVars c) (collectVars b)
  Typed _ _ _ (Export e) -> collectVars e
  _ -> Set.empty

collectVarsBranch :: Is -> Set.Set String
collectVarsBranch is = case is of
  Typed _ _ _ (Is _ body) -> collectVars body
  _ -> Set.empty

collectVarsListItem :: ListItem -> Set.Set String
collectVarsListItem li = case li of
  Typed _ _ _ (ListItem e)   -> collectVars e
  Typed _ _ _ (ListSpread e) -> collectVars e
  _ -> Set.empty

collectVarsField :: Field -> Set.Set String
collectVarsField field = case field of
  Typed _ _ _ (Field (_, e))  -> collectVars e
  Typed _ _ _ (FieldSpread e) -> collectVars e
  _ -> Set.empty
