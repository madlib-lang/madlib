{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Optimize.SortExpressions where
import AST.Core
import qualified Data.Maybe as Maybe
import Data.Graph


sortASTExpressions :: AST -> AST
sortASTExpressions ast =
  let exps = aexps ast
      expDeps = buildDependenciesForAllExps exps
      sortedExps = concat $ stronglyConnComp expDeps >>= flattenSCC
  in ast { aexps = sortedExps }


buildDependenciesForAllExps :: [Exp] -> [([Exp], String, [String])]
buildDependenciesForAllExps exps =
  let allLocalNames = Maybe.mapMaybe getExpName exps
  in  buildDependencies allLocalNames [] exps


buildDependencies :: [String] -> [Exp] -> [Exp] -> [([Exp], String, [String])]
buildDependencies localNames cachedExps exps = case exps of
  e : es ->
    case getExpName e of
      Just n ->
        let deps = buildDependencies' localNames n e
        in  (cachedExps ++ [e], n, deps) : buildDependencies localNames [] es

      Nothing ->
        buildDependencies localNames (cachedExps ++ [e]) es

  [] ->
    []


buildDependencies' :: [String] -> String -> Exp -> [String]
buildDependencies' localNames expName exp = case exp of
  Typed _ _ _ (Definition _ body) ->
    body >>= buildDependencies' localNames expName

  Typed _ _ _ (Do exps) ->
    exps >>= buildDependencies' localNames expName

  Typed _ _ _ (Assignment _ e) ->
    buildDependencies' localNames expName e

  Typed _ _ _ (Export e) ->
    buildDependencies' localNames expName e

  Typed _ _ _ (Var n _) ->
    if n `elem` localNames then
      [n]
    else
      []

  Typed _ _ _ (Where e iss) ->
    buildDependencies' localNames expName e
    ++ (iss >>= (buildDependencies' localNames expName . getIsExpression))

  Typed _ _ _ (If cond truthy falsy) ->
    buildDependencies' localNames expName cond
    ++ buildDependencies' localNames expName truthy
    ++ buildDependencies' localNames expName falsy

  Typed _ _ _ (Call fn args) ->
    buildDependencies' localNames expName fn
    ++ (args >>= buildDependencies' localNames expName)

  Typed _ _ _ (Record fields) ->
    concatMap
      (\case
        Typed _ _ _ (Field (_, exp)) ->
          buildDependencies' localNames expName exp

        Typed _ _ _ (FieldSpread exp) ->
          buildDependencies' localNames expName exp

        _ ->
          []
      )
      fields

  Typed _ _ _ (Access rec _) ->
    buildDependencies' localNames expName rec

  Typed _ _ _ (ListConstructor items) ->
    concatMap
      (\case
        Typed _ _ _ (ListItem exp) ->
          buildDependencies' localNames expName exp

        Typed _ _ _ (ListSpread exp) ->
          buildDependencies' localNames expName exp

        _ ->
          []
      )
      items

  Typed _ _ _ (TupleConstructor items) ->
    items >>= buildDependencies' localNames expName

  _ ->
    []

