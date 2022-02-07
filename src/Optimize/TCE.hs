module Optimize.TCE where

import AST.PostProcessed

newtype Env
  = Env { envCurrentFunction :: Maybe String }



getAppName :: Exp -> Maybe String
getAppName exp = case exp of
  Typed _ _ (Call (Typed _ _ (Var n)) _) ->
    Just n

  Typed _ _ (Call fn _) ->
    getAppName fn

  _ ->
    Nothing



resolve :: AST -> AST
resolve ast =
  let resolved = markDefinition (Env Nothing) <$> aexps ast
  in  ast { aexps = resolved }


markDefinition :: Env -> Exp -> Exp
markDefinition env exp = case exp of
  Typed assType assArea (Assignment fnName abs@(Typed absType absArea (Definition params body))) ->
    if isCandidate fnName params body then
      Typed assType assArea (Assignment fnName (Typed absType absArea (TCEDefinition params (markDefinition env <$> body))))
    else
      Typed assType assArea (Assignment fnName (Typed absType absArea (Definition params (markDefinition env <$> body))))

  _ ->
    exp


getAbsBody :: [Exp] -> [Exp]
getAbsBody exps = case exps of
  [Typed _ _ (Definition _ bodyExps)] ->
    getAbsBody bodyExps

  _ ->
    exps


areIssCandidate :: String -> [String] -> [Is] -> Bool
areIssCandidate fnName params iss = case iss of
  [] ->
    True

  [Typed _ _ (Is _ isExp)] ->
    containsRecursion True fnName isExp || isCandidate fnName params [isExp]

  (Typed _ _ (Is _ isExp) : next) ->
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
    Typed _ _ Call {} ->
      containsRecursion True fnName lastExp

    Typed _ _ (If cond truthy falsy) ->
      not (containsRecursion False fnName cond)
      && (
           containsRecursion True fnName truthy
           || containsRecursion True fnName falsy
           || isCandidate fnName params [truthy]
           || isCandidate fnName params [falsy]
         )

    Typed _ _ (Where exp iss) ->
      let isExpValid = not (containsRecursion False fnName exp)
      in  isExpValid && areIssCandidate fnName params iss

    _ ->
      False

  (exp : next) ->
    not (containsRecursion False fnName exp) && isCandidate fnName params next



containsRecursion :: Bool -> String  -> Exp -> Bool
containsRecursion direct fnName exp = case exp of
  Typed _ _ Call {} ->
    Just fnName == getAppName exp

  Typed _ _ (TemplateString exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ (Access rec accessor) ->
    not direct && (containsRecursion direct fnName rec || containsRecursion direct fnName accessor)

  Typed _ _ (Definition _ exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ (Assignment _ exp) ->
    not direct && containsRecursion direct fnName exp

  Typed _ _ (TypedExp exp _) ->
    not direct && containsRecursion direct fnName exp

  Typed _ _ (ListConstructor exps) ->
    not direct && any (containsRecursion direct fnName) (getListItemExp <$> exps)

  Typed _ _ (TupleConstructor exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ (Record fields) ->
    not direct && any (containsRecursion direct fnName) (getFieldExp <$> fields)

  Typed _ _ (If cond truthy falsy) ->
    not direct &&
      (
        containsRecursion direct fnName cond
        || containsRecursion direct fnName truthy
        || containsRecursion direct fnName falsy
      )

  Typed _ _ (Do exps) ->
    not direct && any (containsRecursion direct fnName) exps

  Typed _ _ (Where exp iss) ->
    not direct &&
      (
        containsRecursion direct fnName exp
        || any (containsRecursion direct fnName) (getIsExpression <$> iss)
      )

  Typed _ _ (Placeholder _ exp) ->
    not direct && containsRecursion direct fnName exp

  _ ->
    False
