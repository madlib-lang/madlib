module Optimize.CalledLambda where
import AST.Core

{-
Replaces expressions of the form
  ((x) => x + 1)(5)
with
  5 + 1

It is not very common, but that is mainly what pipe(..)(..) generates when called directly
and creates an extra function.
-}

replaceVarInIsWith :: Name -> Exp -> Is -> Is
replaceVarInIsWith n replaceWith is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (replaceVarWith n replaceWith exp))

  _ ->
    is


replaceVarWith :: Name -> Exp -> Exp -> Exp
replaceVarWith n replaceWith exp = case exp of
  Typed qt area metadata (Assignment name e) ->
    Typed qt area metadata (Assignment name (replaceVarWith n replaceWith e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (replaceVarWith n replaceWith e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call fn (replaceVarWith n replaceWith <$> args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (replaceVarWith n replaceWith <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem (replaceVarWith n replaceWith) <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (replaceVarWith n replaceWith <$> items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField (replaceVarWith n replaceWith) <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (replaceVarWith n replaceWith cond) (replaceVarWith n replaceWith truthy) (replaceVarWith n replaceWith falsy))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (replaceVarWith n replaceWith rec) (replaceVarWith n replaceWith field))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (replaceVarWith n replaceWith <$> exps))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (replaceVarWith n replaceWith exp) (replaceVarInIsWith n replaceWith <$> iss))

  Typed _ _ _ (Var name _) | name == n ->
    replaceWith

  _ ->
    exp


reduceIs :: Is -> Is
reduceIs is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (reduce exp))

  _ ->
    is


reduce :: Exp -> Exp
reduce exp = case exp of
  Typed _ _ _ (Call (Typed _ _ _ (Definition [param] [body])) [arg]) ->
    replaceVarWith (getValue param) arg body

  Typed qt area metadata (Assignment name e) ->
    Typed qt area metadata (Assignment name (reduce e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (reduce e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call fn (reduce <$> args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (reduce <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem reduce <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (reduce <$> items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField reduce <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (reduce cond) (reduce truthy) (reduce falsy))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (reduce <$> exps))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (reduce exp) (reduceIs <$> iss))

  _ ->
    exp


reduceAST :: AST -> AST
reduceAST ast =
  let reducedExps = reduce <$> aexps ast
  in  ast { aexps = reducedExps }
