module Optimize.StripNonJSInterfaces where

import           AST.Core
import qualified Data.Bifunctor as Bifunctor


stripTable :: Table -> Table
stripTable table = stripAST <$> table

stripAST :: AST -> AST
stripAST ast =
  let strippedExps = strip <$> aexps ast
      strippedInstances =
        (
          \(Untyped area metadata (Instance name ps n methods)) ->
             let strippedMethods = Bifunctor.first strip <$> methods
             in  Untyped area metadata (Instance name ps n strippedMethods)
        ) <$> ainstances ast
  in  ast { aexps = strippedExps, ainstances = strippedInstances }

strip :: Exp -> Exp
strip exp = case exp of
  Typed qt area metadata (Call fn args) ->
    Typed qt area metadata (Call (strip fn) (strip <$> args))

  Typed qt area metadata (Access name field) ->
    Typed qt area metadata (Access (strip name) (strip field))

  Typed qt area metadata (Definition params body) ->
    Typed qt area metadata (Definition params (strip <$> body))

  Typed qt area metadata (Assignment name exp) ->
    Typed qt area metadata (Assignment name (strip exp))

  Typed qt area metadata (Export exp) ->
    Typed qt area metadata (Export (strip exp))

  Typed qt area metadata (ListConstructor items) ->
    Typed qt area metadata (ListConstructor (stripListItem <$> items))

  Typed qt area metadata (TupleConstructor items) ->
    Typed qt area metadata (TupleConstructor (strip <$> items))

  Typed qt area metadata (Record fields) ->
    Typed qt area metadata (Record (stripField <$> fields))

  Typed qt area metadata (If cond truthy falsy) ->
    Typed qt area metadata (If (strip cond) (strip truthy) (strip falsy))

  Typed qt area metadata (Do exps) ->
    Typed qt area metadata (Do (strip <$> exps))

  Typed qt area metadata (Where exp iss) ->
    Typed qt area metadata (Where (strip exp) (stripIs <$> iss))

  Typed _ _ _ (Placeholder (ClassRef "Number" _ _ _, _) exp) ->
    strip exp

  Typed _ _ _ (Placeholder (MethodRef "Number" _ _, _) exp) ->
    strip exp

  Typed _ _ _ (Placeholder (ClassRef "Eq" _ _ _, _) exp) ->
    strip exp

  Typed _ _ _ (Placeholder (MethodRef "Eq" _ _, _) exp) ->
    strip exp

  Typed qt area metadata (Placeholder ref exp) ->
    Typed qt area metadata (Placeholder ref (strip exp))

  _ ->
    exp

stripIs :: Is -> Is
stripIs is = case is of
  Typed qt area metadata (Is pat exp) ->
    Typed qt area metadata (Is pat (strip exp))

  _ ->
    undefined

stripListItem :: ListItem -> ListItem
stripListItem li = case li of
  Typed qt area metadata (ListItem exp) ->
    Typed qt area metadata (ListItem (strip exp))

  Typed qt area metadata (ListSpread exp) ->
    Typed qt area metadata (ListSpread (strip exp))

  _ ->
    undefined

stripField :: Field -> Field
stripField li = case li of
  Typed qt area metadata (Field (name, exp)) ->
    Typed qt area metadata (Field (name, strip exp))

  Typed qt area metadata (FieldSpread exp) ->
    Typed qt area metadata (FieldSpread (strip exp))

  _ ->
    undefined
