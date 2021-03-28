{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Coverage.Coverable where

import           AST.Solved
import           Explain.Location
import qualified Data.Set                      as S
import Infer.Type

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
  collect (Solved t (Area (Loc _ l _) _) exp) = case exp of    
    Assignment name (Solved _ (Area (Loc _ line _) _) (Abs _ body)) ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)

    Export (Solved _ _ (Assignment name (Solved _ (Area (Loc _ line _) _) (Abs _ body)))) ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)
    
    TypedExp (Solved _ _ (Assignment name (Solved _ (Area (Loc _ line _) _) (Abs _ body)))) _ ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)
    
    TypedExp (Solved _ _ (Export (Solved _ _ (Assignment name (Solved _ (Area (Loc _ line _) _) (Abs _ body)))))) _ ->
      [Function { line = line, name = name }, Line { line = line }] <> concat (collect <$> body)

    Assignment name e ->
      if isFunctionType t then
        [Function { line = l, name = name }, Line { line = l }] <> collect e
      else
        [Line { line = l }] <> collect e
    Export (Solved _ _ (Assignment name e)) ->
      if isFunctionType t then
        [Function { line = l, name = name }, Line { line = l }] <> collect e
      else
        [Line { line = l }] <> collect e
    TypedExp (Solved _ (Area (Loc _ l' _) _) (Assignment name e)) _ ->
      if isFunctionType t then
        [Function { line = l', name = name }, Line { line = l' }] <> collect e
      else
        [Line { line = l' }] <> collect e
    TypedExp (Solved _ (Area (Loc _ l' _) _) (Export (Solved _ _ (Assignment name e)))) _ ->
      if isFunctionType t then
        [Function { line = l', name = name }, Line { line = l' }] <> collect e
      else
        [Line { line = l' }] <> collect e

    App fn arg _          -> collect fn <> collect arg
    Abs         _   body  -> concat (collect <$> body)
    FieldAccess rec field -> collect rec <> collect field
    -- Assignment  _   e     -> collect e
    Export e              -> collect e
    TypedExp e _          -> collect e
    TupleConstructor es   -> concat $ collect <$> es
    If cond good bad      -> collect cond <> collect good <> collect bad
    Where e iss           -> [Line { line = l }] <> collect e <> concat (collect <$> iss)
    Placeholder _ e       -> collect e
    JSExp _               -> []
    Var   n               -> [Line { line = l }]
    _                     -> []

isOperator :: String -> Bool
isOperator n = "|>" == n || "==" == n || ">=" == n || "<=" == n || "&&" == n || "||" == n || "!=" == n

instance Collectable Is where
  collect (Solved _ (Area (Loc _ l _) _) (Is _ e)) = [Line { line = l }] <> collect e


isFunction :: Coverable -> Bool
isFunction Function{} = True
isFunction _          = False

isLine :: Coverable -> Bool
isLine Line{} = True
isLine _      = False
