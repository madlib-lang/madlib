{-# LANGUAGE LambdaCase #-}
module Canonicalize.Rewrite where
import AST.Canonical
import Explain.Location (emptyArea, increaseStartColumns)
import qualified Data.Map as Map

{-
Rewrite AST for optimization purposes.

Currently only transforms:
  map(f2, map(f1, functor))
to:
  map((x) => f2(f1(x)), functor)
-}


rewriteInstance :: Instance -> Instance
rewriteInstance inst = case inst of
  Canonical area (Instance n ps p methods) ->
    Canonical area (Instance n ps p (Map.map (rewrite 0) methods))


rewriteAST :: AST -> AST
rewriteAST ast =
  ast
    { aexps = map (rewrite 0) (aexps ast)
    , ainstances = map rewriteInstance (ainstances ast)
    }


rewrite :: Int -> Exp -> Exp
rewrite index exp = case exp of
  Canonical area (App (Canonical _ (App (Canonical area2 (Var "map")) f True)) (Canonical _ (App (Canonical _ (App (Canonical _ (Var "map")) f' True)) arg _)) isFinal) ->
    let paramName = "__M__" ++ show index
        mergedAbs =
          Canonical (increaseStartColumns 3 area) (Abs (Canonical emptyArea paramName) [
            Canonical (increaseStartColumns 3 area) (App f (Canonical (increaseStartColumns 3 area) (App f' (Canonical emptyArea (Var paramName)) True)) True)
          ])
    in  rewrite (index + 1) $
          Canonical area (App (Canonical area2 (App (Canonical area2 (Var "map")) mergedAbs True)) arg isFinal)

  Canonical area (Record fields) ->
    let fields' =
          map
            (\case
              Canonical area (Field (n, e)) ->
                Canonical area (Field (n, rewrite index e))

              Canonical area (FieldSpread e) ->
                Canonical area (FieldSpread (rewrite index e))
            )
            fields
    in  Canonical area (Record fields')

  Canonical area (Abs p body) ->
    Canonical area (Abs p (map (rewrite index) body))

  Canonical area (Do exps) ->
    Canonical area (Do (map (rewrite index) exps))

  Canonical area (App f arg final) ->
    Canonical area (App (rewrite index f) (rewrite index arg) final)

  Canonical area (TypedExp e ty sc) ->
    Canonical area (TypedExp (rewrite index e) ty sc)

  Canonical area (Assignment n e) ->
    Canonical area (Assignment n (rewrite index e))

  Canonical area (Export e) ->
    Canonical area (Export (rewrite index e))

  Canonical area (Where e iss) ->
    let iss' =
          map
            (\(Canonical area (Is pat e)) ->
                Canonical area (Is pat (rewrite index e))
            )
            iss
    in  Canonical area (Where (rewrite index e) iss')

  Canonical area (If cond truthy falsy) ->
    Canonical area (If (rewrite index cond) (rewrite index truthy) (rewrite index falsy))

  Canonical area (While cond body) ->
    Canonical area (While (rewrite index cond) (rewrite index body))

  Canonical area (Access record field) ->
    Canonical area (Access (rewrite index record) field)

  Canonical area (TemplateString exps) ->
    Canonical area (TemplateString (map (rewrite index) exps))

  Canonical area (TupleConstructor exps) ->
    Canonical area (TupleConstructor (map (rewrite index) exps))

  Canonical area (ListConstructor items) ->
    let items' =
          map
            (\case
              Canonical area (ListItem e) ->
                Canonical area (ListItem (rewrite index e))

              Canonical area (ListSpread e) ->
                Canonical area (ListSpread (rewrite index e))
            )
            items
    in  Canonical area (ListConstructor items')

  or ->
    or
