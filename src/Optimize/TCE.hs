module Optimize.TCE where

import           AST.Core
import qualified Data.Maybe          as Maybe

newtype Env
  = Env { envCurrentName :: Maybe String }



getAppName :: Exp -> Maybe String
getAppName exp = case exp of
  Typed _ _ _ (Var n) ->
    Just n

  Typed _ _ _ (Call fn _) ->
    getAppName fn

  Typed _ _ _ (Placeholder _ e) ->
    getAppName e

  _ ->
    Nothing



resolve :: AST -> AST
resolve ast =
  let resolved = markDefinition (Env Nothing) <$> aexps ast
  in  ast { aexps = resolved }


markDefinition :: Env -> Exp -> Exp
markDefinition env exp = case exp of
  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (markDefinition env e))

  Typed qt area metadata (Assignment fnName abs) ->
    Typed qt area metadata (Assignment fnName (markDefinition env { envCurrentName = Just fnName } abs))

  Typed qt area metadata (Definition params body) | Maybe.isJust (envCurrentName env) ->
    let Just fnName = envCurrentName env
    in  if isCandidate fnName params body then
          Typed qt area (TCODefinition : metadata) (Definition params (markTRCCalls BasicRecursion fnName . markDefinition env <$> body))
        else
          Typed qt area metadata (Definition params (markDefinition env <$> body))

  Typed qt area metadata (Placeholder ref exp) ->
    Typed qt area metadata (Placeholder ref (markDefinition env exp))

  _ ->
    exp


markIs :: RecursionKind -> String -> Is -> Is
markIs recKind fnName is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (markTRCCalls recKind fnName exp))

  _ ->
    is

-- looks for tail recursive calls and marks them
markTRCCalls :: RecursionKind -> String -> Exp -> Exp
markTRCCalls recKind fnName exp = case exp of
  Typed qt area metadata (Call fn args) ->
    if Just fnName == getAppName exp then
      Typed qt area (TailRecursiveCall recKind : metadata) (Call fn args)
    else
      exp

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (markTRCCalls recKind fnName cond) (markTRCCalls recKind fnName truthy) (markTRCCalls recKind fnName falsy))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where exp (markIs recKind fnName <$> iss))

  -- TODO: Probably we need to either mark the whole thing, or just the last expression of it
  -- Typed qt area metadata (Do exps) ->
  --   Typed qt area metadata (Do (markTRCCalls recKind fnName <$> exps))

  Typed qt area metadata (Placeholder info exp) ->
    Typed qt area metadata (Placeholder info (markTRCCalls recKind fnName exp))

  Typed qt area metadata (ListConstructor [Typed qtLi areaLi metadataLi (ListItem li), Typed qtSpread areaSpread metadataSpread (ListSpread spread)]) ->
    -- we probably need to mark this node as being ListRecursion
    Typed qt area metadata (ListConstructor [
      Typed qtLi areaLi metadataLi (ListItem li),
      Typed qtSpread areaSpread metadataSpread (ListSpread (markTRCCalls ListRecursion fnName spread))
    ])

  Typed qt area metadata e ->
    Typed qt area (LeafNode : metadata) e

  _ ->
    exp


areIssCandidate :: String -> [String] -> [Is] -> Bool
areIssCandidate fnName params iss = case iss of
  [] ->
    True

  [Typed _ _ _ (Is _ isExp)] ->
    containsRecursion True fnName isExp || isCandidate fnName params [isExp]

  (Typed _ _ _ (Is _ isExp) : next) ->
    containsRecursion True fnName isExp
      || isCandidate fnName params [isExp]
      || areIssCandidate fnName params next

  _ ->
    False


isCandidate :: String -> [String] -> [Exp] -> Bool
isCandidate fnName params exps = case exps of
  [] ->
    True

  [lastExp] -> case lastExp of
    Typed _ _ _ Call {} ->
      containsRecursion True fnName lastExp

    Typed _ _ _ (If cond truthy falsy) ->
      not (containsRecursion False fnName cond)
      && (
           containsRecursion True fnName truthy
           || containsRecursion True fnName falsy
           || isCandidate fnName params [truthy]
           || isCandidate fnName params [falsy]
         )

    Typed _ _ _ (Where exp iss) ->
      let isExpValid = not (containsRecursion False fnName exp)
      in  isExpValid && areIssCandidate fnName params iss

    Typed _ _ _ (ListConstructor [Typed _ _ _ (ListItem li), Typed _ _ _ (ListSpread spread)]) ->
      not (containsRecursion False fnName li) && containsRecursion True fnName spread

    _ ->
      False

  (exp : next) ->
    not (containsRecursion False fnName exp) && isCandidate fnName params next


containsRecursion :: Bool -> String  -> Exp -> Bool
containsRecursion direct fnName exp = case exp of
  Typed _ _ _ Call {} ->
    Just fnName == getAppName exp

  Typed _ _ _ (Access rec accessor) ->
    not direct && (containsRecursion direct fnName rec || containsRecursion direct fnName accessor)

  Typed _ _ _ (Definition _ exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ _ (Assignment _ exp) ->
    not direct && containsRecursion direct fnName exp

  Typed _ _ _ (ListConstructor exps) ->
    not direct && any (containsRecursion direct fnName) (getListItemExp <$> exps)

  Typed _ _ _ (TupleConstructor exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ _ (Record fields) ->
    not direct && any (containsRecursion direct fnName) (getFieldExp <$> fields)

  Typed _ _ _ (If cond truthy falsy) ->
    not direct &&
      (
        containsRecursion direct fnName cond
        || containsRecursion direct fnName truthy
        || containsRecursion direct fnName falsy
      )

  Typed _ _ _ (Do exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ _ (Where exp iss) ->
    not direct &&
      (
        containsRecursion direct fnName exp
        || any (containsRecursion direct fnName) (getIsExpression <$> iss)
      )

  Typed _ _ _ (Placeholder _ exp) ->
    not direct && containsRecursion direct fnName exp

  _ ->
    False
