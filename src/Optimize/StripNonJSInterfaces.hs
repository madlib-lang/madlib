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
          \(Untyped area (Instance name ps n methods)) ->
             let strippedMethods = Bifunctor.first strip <$> methods
             in  Untyped area (Instance name ps n strippedMethods)
        ) <$> ainstances ast
  in  ast { aexps = strippedExps, ainstances = strippedInstances }

strip :: Exp -> Exp
strip exp = case exp of
  Typed qt area (Call callType fn args) ->
    Typed qt area (Call callType (strip fn) (strip <$> args))

  Typed qt area (Access name field) ->
    Typed qt area (Access (strip name) (strip field))

  Typed qt area (Definition defType params body) ->
    Typed qt area (Definition defType params (strip <$> body))

  Typed qt area (Assignment name exp) ->
    Typed qt area (Assignment name (strip exp))

  Typed qt area (Export exp) ->
    Typed qt area (Export (strip exp))

  Typed qt area (TypedExp exp sc) ->
    Typed qt area (TypedExp (strip exp) sc)

  Typed qt area (ListConstructor items) ->
    Typed qt area (ListConstructor (stripListItem <$> items))

  Typed qt area (TupleConstructor items) ->
    Typed qt area (TupleConstructor (strip <$> items))

  Typed qt area (Record fields) ->
    Typed qt area (Record (stripField <$> fields))

  Typed qt area (If cond truthy falsy) ->
    Typed qt area (If (strip cond) (strip truthy) (strip falsy))

  Typed qt area (Do exps) ->
    Typed qt area (Do (strip <$> exps))

  Typed qt area (Where exp iss) ->
    Typed qt area (Where (strip exp) (stripIs <$> iss))

  Typed qt area (Placeholder (ClassRef "Number" _ _ _, ts) exp) ->
    strip exp

  Typed qt area (Placeholder (MethodRef "Number" _ _, _) exp) ->
    strip exp

  Typed qt area (Placeholder (ClassRef "Eq" _ _ _, _) exp) ->
    strip exp

  Typed qt area (Placeholder (MethodRef "Eq" _ _, _) exp) ->
    strip exp

  Typed qt area (Placeholder ref exp) ->
    Typed qt area (Placeholder ref (strip exp))

  _ ->
    exp

stripIs :: Is -> Is
stripIs is = case is of
  Typed qt area (Is pat exp) ->
    Typed qt area (Is pat (strip exp))

  _ ->
    undefined

stripListItem :: ListItem -> ListItem
stripListItem li = case li of
  Typed qt area (ListItem exp) ->
    Typed qt area (ListItem (strip exp))

  Typed qt area (ListSpread exp) ->
    Typed qt area (ListSpread (strip exp))

  _ ->
    undefined

stripField :: Field -> Field
stripField li = case li of
  Typed qt area (Field (name, exp)) ->
    Typed qt area (Field (name, strip exp))

  Typed qt area (FieldSpread exp) ->
    Typed qt area (FieldSpread (strip exp))

  _ ->
    undefined

-- Placeholder (ClassRef "Number" _ _ _, ts) exp ->
--   compile env config exp

-- Placeholder (MethodRef "Number" _ _, ts) exp ->
--   compile env config exp

-- Placeholder (ClassRef "Eq" _ _ _, ts) exp ->
--   compile env config exp

-- Placeholder (MethodRef "Eq" _ _, ts) exp ->
--   compile env config exp