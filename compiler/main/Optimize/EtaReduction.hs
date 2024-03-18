module Optimize.EtaReduction where
import AST.Core
import qualified Data.Bifunctor as Bifunctor


reduceAST :: AST -> AST
reduceAST ast =
  let reducedExps = reduce blacklist <$> aexps ast
  in  ast { aexps = reducedExps }


reduceIs :: Is -> Is
reduceIs is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (reduce blacklist exp))

  _ ->
    is


usesParam :: String -> Exp -> Bool
usesParam name exp = case exp of
  Typed _ _ _ (Var n _) ->
    name == n

  Typed _ _ _ (Definition _ body) ->
    any (usesParam name) body

  Typed _ _ _ (Call fn args) ->
    usesParam name fn || any (usesParam name) args

  Typed _ _ _ (Access rec _) ->
    usesParam name rec

  Typed _ _ _ (ListConstructor items) ->
    any (usesParam name) (getListItemExp <$> items)

  Typed _ _ _ (TupleConstructor items) ->
    any (usesParam name) items

  Typed _ _ _ (Record fields) ->
    any (usesParam name) (getFieldExp <$> fields)

  Typed _ _ _ (If cond truthy falsy) ->
    usesParam name cond
    || usesParam name truthy
    || usesParam name falsy

  Typed _ _ _ (While cond body) ->
    usesParam name cond || usesParam name body

  Typed _ _ _ (Do exps) ->
    any (usesParam name) exps

  _ ->
    False


reduceDefinitionParams :: Exp -> [Core String] -> [Exp] -> ([String], [Exp])
reduceDefinitionParams fn params args = case (reverse params, reverse args) of
  (param : nextParams, arg : nextArgs) ->
    case arg of
      Typed _ _ _ (Var name _)
        | name == getValue param
        && not (or (usesParam (getValue param) <$> nextArgs))
        && not (usesParam (getValue param) fn) ->
        reduceDefinitionParams fn (reverse nextParams) (reverse nextArgs)

      _ ->
        (getValue <$> params, args)

  _ ->
    (getValue <$> params, args)


blacklist :: [String]
blacklist = ["!", "unary-minus", "++", "+", "-", "*", "/", "%", "==", "!=", "&&", "||", "|", "&", "^", "~", "<<", ">>", ">>>", "<", ">", ">=", "<=", "|>"]


isInBlacklist :: [String] -> Exp -> Bool
isInBlacklist bl fn = case fn of
  Typed _ _ _ (Var name _) ->
    name `elem` bl

  _ ->
    False


reduce :: [String] -> Exp -> Exp
reduce blacklist exp = case exp of
  Typed qt area metadata (Assignment lhs@(Typed _ _ _ (Var name _)) e) ->
    Typed qt area metadata (Assignment lhs (reduce (name : blacklist) e))

  Typed qt area metadata (Assignment lhs e) ->
    Typed qt area metadata (Assignment lhs (reduce blacklist e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (reduce blacklist e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call fn (reduce blacklist <$> args))

  Typed qt area metadata (Definition params body) ->
    case body of
      [Typed _ _ _ (Call fn@(Typed _ _ _ (Var _ _)) args)] | not (isInBlacklist blacklist fn) ->
        let (paramsLeft, argsLeft) = reduceDefinitionParams fn params args
        in  if null paramsLeft && null argsLeft then
              updateQualType qt fn
            else
              Typed qt area metadata (Definition params (reduce blacklist <$> body))

      _ ->
        Typed qt area metadata (Definition params (reduce blacklist <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem (reduce blacklist) <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (reduce blacklist <$> items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField (reduce blacklist) <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (reduce blacklist cond) (reduce blacklist truthy) (reduce blacklist falsy))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (reduce blacklist cond) (reduce blacklist body))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (reduce blacklist <$> exps))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (reduce blacklist exp) (reduceIs <$> iss))

  _ ->
    exp
