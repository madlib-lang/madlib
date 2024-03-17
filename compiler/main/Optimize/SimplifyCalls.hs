{-# LANGUAGE LambdaCase #-}
module Optimize.SimplifyCalls where
import AST.Core
import qualified Data.List as List

{-
Replaces expressions of the form
  ((x) => x + 1)(5)
with
  5 + 1

It is not very common, but that is mainly what pipe(..)(..) generates when called directly
and creates an extra function.

Because it seems to somehow alter program behavior when generalized, it's currently
limited to pipes, where shorthand, and placeholder args ( $ ).
-}

replaceVarInIsWith :: String -> Exp -> Is -> Is
replaceVarInIsWith n replaceWith is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (replaceVarWith n replaceWith exp))

  _ ->
    is


replaceVarWith :: String -> Exp -> Exp -> Exp
replaceVarWith n replaceWith exp = case exp of
  Typed qt area metadata (Assignment name e) ->
    Typed qt area metadata (Assignment name (replaceVarWith n replaceWith e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (replaceVarWith n replaceWith e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (replaceVarWith n replaceWith fn) (replaceVarWith n replaceWith <$> args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (replaceVarWith n replaceWith <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem (replaceVarWith n replaceWith) <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (replaceVarWith n replaceWith <$> items))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (replaceVarWith n replaceWith rec) (replaceVarWith n replaceWith field))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (mapRecordField (replaceVarWith n replaceWith) <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (replaceVarWith n replaceWith cond) (replaceVarWith n replaceWith truthy) (replaceVarWith n replaceWith falsy))

  Typed qt area metadata (While cond body) ->
    Typed qt area metadata (While (replaceVarWith n replaceWith cond) (replaceVarWith n replaceWith body))

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


isEligible :: Exp -> Bool
isEligible exp = case exp of
  Typed _ _ _ (Assignment _ e) ->
    isEligible e

  Typed _ _ _ (Export e) ->
    isEligible e

  Typed _ _ _ (Call fn args) ->
    isEligible fn || all isEligible args

  Typed _ _ _ (Definition _ body) ->
    all isEligible body

  Typed _ _ _ (ListConstructor items) ->
    all
      (\case
        Typed _ _ _ (ListItem e) ->
          isEligible e

        Typed _ _ _ (ListSpread e) ->
          isEligible e

        _ ->
          True
      )
      items

  Typed _ _ _ (TupleConstructor items) ->
    all isEligible items

  Typed _ _ _ (Access rec field) ->
    isEligible rec || isEligible field

  Typed _ _ _ (Record fields) ->
    all
      (\case
        Typed _ _ _ (Field (_, e)) ->
          isEligible e

        Typed _ _ _ (FieldSpread e) ->
          isEligible e

        _ ->
          True
      )
      fields

  Typed _ _ _ (If cond truthy falsy) ->
    isEligible cond || isEligible truthy || isEligible falsy

  Typed _ _ _ (While cond body) ->
    isEligible cond || isEligible body

  Typed _ _ _ (Do exps) ->
    all isEligible exps

  Typed _ _ _ (Where exp iss) ->
    isEligible exp ||
    all
      (\(Typed _ _ _ (Is _ e)) ->
        isEligible e
      )
      iss

  Typed _ _ _ (JSExp _) ->
    False

  _ ->
    True


occurencesOf :: String -> Exp -> Int
occurencesOf name exp = case exp of
  Typed _ _ _ (Assignment _ e) ->
    occurencesOf name e

  Typed _ _ _ (Export e) ->
    occurencesOf name e

  Typed _ _ _ (Call fn args) ->
    occurencesOf name fn + foldr ((+) . occurencesOf name) 0 args

  Typed _ _ _ (Definition _ body) ->
    foldr ((+) . occurencesOf name) 0 body

  Typed _ _ _ (ListConstructor items) ->
    foldr
      ((+) . \case
        Typed _ _ _ (ListItem e) ->
          occurencesOf name e

        Typed _ _ _ (ListSpread e) ->
          occurencesOf name e

        _ ->
          0
      )
      0
      items

  Typed _ _ _ (TupleConstructor items) ->
    foldr ((+) . occurencesOf name) 0 items

  Typed _ _ _ (Record fields) ->
    foldr
      ((+) . \case
        Typed _ _ _ (Field (_, e)) ->
          occurencesOf name e

        Typed _ _ _ (FieldSpread e) ->
          occurencesOf name e

        _ ->
          0
      )
      0
      fields

  Typed _ _ _ (Access rec field) ->
    occurencesOf name rec + occurencesOf name field

  Typed _ _ _ (If cond truthy falsy) ->
    occurencesOf name cond + occurencesOf name truthy + occurencesOf name falsy

  Typed _ _ _ (While cond body) ->
    occurencesOf name cond + occurencesOf name body

  Typed _ _ _ (Do exps) ->
    foldr ((+) . occurencesOf name) 0 exps

  Typed _ _ _ (Where exp iss) ->
    occurencesOf name exp +
    foldr
      ((+) . \(Typed _ _ _ (Is _ e)) ->
        occurencesOf name e
      )
      0
      iss

  Typed _ _ _ (Var n _) ->
    if n == name then
      1
    else
      0

  Typed _ _ _ (JSExp _) ->
    0

  _ ->
    0


isLiteralOrVar :: Exp -> Bool
isLiteralOrVar exp = case exp of
  Typed _ _ _ (Literal _) ->
    True

  Typed _ _ _ (Var _ _) ->
    True

  _ ->
    False


reduce :: Exp -> Exp
reduce exp = case exp of
  Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param@(Typed _ _ _ pName)] [body])) [arg@(Typed _ _ _ (Var aName _))])
    | "__$PH" `List.isPrefixOf` pName ||
      "__P__" `List.isPrefixOf` pName ||
      "__M__" `List.isPrefixOf` pName ||
      "__W__" `List.isPrefixOf` pName ||
      "__$PH" `List.isPrefixOf` aName ||
      "__P__" `List.isPrefixOf` aName ||
      "__M__" `List.isPrefixOf` aName ||
      "__W__" `List.isPrefixOf` aName ->
        if isEligible body && (isLiteralOrVar arg || occurencesOf pName body == 1) then
          reduce $ replaceVarWith (getValue param) (reduce arg) (reduce body)
        else
          Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param] [reduce body])) [reduce arg])

  Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param@(Typed _ _ _ pName)] [body])) [arg])
    | "__$PH" `List.isPrefixOf` pName || "__P__" `List.isPrefixOf` pName || "__M__" `List.isPrefixOf` pName || "__W__" `List.isPrefixOf` pName ->
      if isEligible body && (isLiteralOrVar arg || occurencesOf pName body == 1) then
        reduce $ replaceVarWith (getValue param) (reduce arg) (reduce body)
      else
        Typed qt area metadata (Call (Typed qt' area' metadata' (Definition [param] [reduce body])) [reduce arg])

  Typed qt area metadata (Assignment name e) ->
    Typed qt area metadata (Assignment name (reduce e))

  Typed qt area metadata (Export e) ->
    Typed qt area metadata (Export (reduce e))

  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (reduce fn) (reduce <$> args))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (reduce <$> body))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (mapListItem reduce <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (reduce <$> items))

  Typed qt area metadata (Access rec field) ->
    Typed qt area metadata (Access (reduce rec) (reduce field))

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
