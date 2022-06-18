{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Coverage.Coverable where

import           AST.Solved
import           Explain.Location
import qualified Data.Set                      as S
import           Infer.Type

data Coverable
  = Function { line :: Int, name :: String }
  | Line { line :: Int }
  deriving (Ord, Eq, Show)

collectFromAST :: AST -> [Coverable]
collectFromAST AST { aexps, ainstances } =
  let expCoverables    = S.toList $ S.fromList $ concat $ collect <$> aexps
      methods          = filter (isFunctionType . getType) $ concat $ getInstanceMethods <$> ainstances
      methodCoverables = S.toList $ S.fromList $ concat $ collect <$> methods
  in  expCoverables ++ methodCoverables

class Collectable a where
  collect :: a -> [Coverable]

instance Collectable Exp where
  collect (Untyped _ _) = []
  collect (Typed (_ :=> t) (Area (Loc _ l _) _) exp) = case exp of
    Assignment name (Typed _ (Area (Loc _ line _) _) (Abs _ body)) ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)

    Export (Typed _ _ (Assignment name (Typed _ (Area (Loc _ line _) _) (Abs _ body)))) ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)

    TypedExp (Typed _ _ (Assignment name (Typed _ (Area (Loc _ line _) _) (Abs _ body)))) _ _ ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)

    TypedExp (Typed _ _ (Export (Typed _ _ (Assignment name (Typed _ (Area (Loc _ line _) _) (Abs _ body)))))) _ _ ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)

    Assignment name e ->
      if isFunctionType t then
        [Function { line = l, name = name }, Line { line = l }] <> collect e
      else
        [] <> collect e

    Export (Typed _ _ (Assignment name e)) ->
      if isFunctionType t then
        [Function { line = l, name = name }, Line { line = l }] <> collect e
      else
        [] <> collect e

    TypedExp (Typed _ (Area (Loc _ l' _) _) (Assignment name e)) _ _ ->
      if isFunctionType t then
        [Function { line = l', name = name }, Line { line = l' }] <> collect e
      else
        [] <> collect e

    TypedExp (Typed _ (Area (Loc _ l' _) _) (Export (Typed _ _ (Assignment name e)))) _ _ ->
      if isFunctionType t then
        [Function { line = l', name = name }, Line { line = l' }] <> collect e
      else
        [] <> collect e

    App fn arg _ ->
      collect fn <> collect arg

    Abs _ body ->
      concat (collect <$> body)

    Access rec field ->
      collect rec <> collect field

    Export e ->
      collect e

    TypedExp e _ _ ->
      collect e

    TupleConstructor es ->
      concat $ collect <$> es

    If cond good bad ->
      collect cond <> collect good <> collect bad

    Where e iss ->
      [Line { line = l }] <> collect e <> concat (collect <$> iss)

    Placeholder _ e ->
      collect e

    JSExp _ ->
      []

    Var _ _ ->
      [Line { line = l }]

    Record fields ->
      concat $ collect <$> fields

    ListConstructor items ->
      concat $ collect <$> items

    _ ->
      []


isOperator :: String -> Bool
isOperator n = "|>" == n || "==" == n || ">=" == n || "<=" == n || "&&" == n || "||" == n || "!=" == n

instance Collectable Is where
  collect (Typed _ (Area (Loc _ l _) _) (Is _ e)) = [Line { line = l }] <> collect e
  collect _ = []

instance Collectable Field where
  collect (Typed _ _ (Field (_, exp))) = collect exp
  collect (Typed _ _ (FieldSpread exp))   = collect exp
  collect _                                = []

instance Collectable ListItem where
  collect (Typed _ _ (ListItem exp))   = collect exp
  collect (Typed _ _ (ListSpread exp)) = collect exp
  collect _                             = []

isFunction :: Coverable -> Bool
isFunction Function{} = True
isFunction _          = False

isLine :: Coverable -> Bool
isLine Line{} = True
isLine _      = False
