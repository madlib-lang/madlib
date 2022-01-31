module Optimize.TCE where

import AST.Optimized

newtype Env
  = Env { envCurrentFunction :: Maybe String }


collectAbsParams :: Exp -> [String]
collectAbsParams exp = case exp of
  Optimized _ _ (Abs param [bodyExp]) ->
    let nextParams = collectAbsParams bodyExp
    in  param : nextParams

  Optimized _ _ (Abs param bodyExp) ->
    [param]

  _ ->
    []


getAppName :: Exp -> Maybe String
getAppName exp = case exp of
  Optimized _ _ (App (Optimized _ _ (Var n)) _ _) ->
    Just n

  Optimized _ _ (App fn _ _) ->
    getAppName fn

  _ ->
    Nothing



resolve :: AST -> AST
resolve ast =
  let resolved = markDefinition (Env Nothing) <$> aexps ast
  in  ast { aexps = resolved }


markDefinition :: Env -> Exp -> Exp
markDefinition env exp = case exp of
  Optimized assType assArea (Assignment fnName abs@(Optimized absType absArea (Abs param body))) ->
    let params   = collectAbsParams abs
        bodyExps = getAbsBody body
    in  if isCandidate fnName params bodyExps then
          Optimized assType assArea (Assignment fnName (Optimized absType absArea (TCEDefinition params (markDefinition env <$> bodyExps))))
        else
          Optimized assType assArea (Assignment fnName (Optimized absType absArea (Abs param (markDefinition env <$> body))))

  _ ->
    exp


getAbsBody :: [Exp] -> [Exp]
getAbsBody exps = case exps of
  [Optimized _ _ (Abs _ bodyExps)] ->
    getAbsBody bodyExps

  _ ->
    exps


areIssCandidate :: String -> [String] -> [Is] -> Bool
areIssCandidate fnName params iss = case iss of
  [] ->
    True

  [Optimized _ _ (Is _ isExp)] ->
    containsRecursion True fnName isExp || isCandidate fnName params [isExp]

  (Optimized _ _ (Is _ isExp) : next) ->
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
    Optimized _ _ App {} ->
      containsRecursion True fnName lastExp

    Optimized _ _ (If cond truthy falsy) ->
      not (containsRecursion False fnName cond)
      && (
           containsRecursion True fnName truthy
           || containsRecursion True fnName falsy
           || isCandidate fnName params [truthy]
           || isCandidate fnName params [falsy]
         )

    Optimized _ _ (Where exp iss) ->
      let isExpValid = not (containsRecursion False fnName exp)
      in  isExpValid && areIssCandidate fnName params iss

    _ ->
      False

  (exp : next) ->
    not (containsRecursion False fnName exp) && isCandidate fnName params next



containsRecursion :: Bool -> String  -> Exp -> Bool
containsRecursion direct fnName exp = case exp of
  Optimized _ _ App {} ->
    Just fnName == getAppName exp

  Optimized _ _ (TemplateString exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Optimized _ _ (Access rec accessor) ->
    not direct && (containsRecursion direct fnName rec || containsRecursion direct fnName accessor)

  Optimized _ _ (Abs _ exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Optimized _ _ (Assignment _ exp) ->
    not direct && containsRecursion direct fnName exp

  Optimized _ _ (TypedExp exp _) ->
    not direct && containsRecursion direct fnName exp

  Optimized _ _ (ListConstructor exps) ->
    not direct && any (containsRecursion direct fnName) (getListItemExp <$> exps)

  Optimized _ _ (TupleConstructor exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Optimized _ _ (Record fields) ->
    not direct && any (containsRecursion direct fnName) (getFieldExp <$> fields)

  Optimized _ _ (If cond truthy falsy) ->
    not direct &&
      (
        containsRecursion direct fnName cond
        || containsRecursion direct fnName truthy
        || containsRecursion direct fnName falsy
      )

  Optimized _ _ (Do exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Optimized _ _ (Where exp iss) ->
    not direct &&
      (
        containsRecursion direct fnName exp
        || any (containsRecursion direct fnName) (getIsExpression <$> iss)
      )

  Optimized _ _ (Placeholder _ exp) ->
    not direct && containsRecursion direct fnName exp

  _ ->
    False
