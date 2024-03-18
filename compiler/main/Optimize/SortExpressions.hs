{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Optimize.SortExpressions where
import AST.Core
import qualified Data.Maybe as Maybe
import Data.Graph
import Debug.Trace
import Text.Show.Pretty (ppShow)
import qualified Data.List as List


-- In case of CyclicSSC we try to keep the order from the areas
-- as those should only happen within the same files and mainly not move
-- because cyclic imports aren't allowed anyways.
flattenAndFixCyclic :: SCC [Exp] -> [[Exp]]
flattenAndFixCyclic ssc = case ssc of
  AcyclicSCC v ->
    [v]

  CyclicSCC vs ->
    List.sortBy (\a b -> compare (getArea $ List.head a) (getArea $ List.head b)) vs


sortASTExpressions :: AST -> AST
sortASTExpressions ast =
  let exps = aexps ast
      expDeps = buildDependenciesForAllExps exps
      sortedExps = concat $ stronglyConnComp expDeps >>= flattenAndFixCyclic
  in ast { aexps = sortedExps }


-- Used for the REPL
keepLastMainExpAndDeps :: AST -> AST
keepLastMainExpAndDeps ast =
  if apath ast == Just "__REPL__.mad" then
    let Typed qt area metadata (Assignment n (Typed qt' area' metadata' (Definition params body))) =
          last $ aexps ast
    in  if length body > 1 then
          let allLocalNames = Maybe.mapMaybe getExpName body
              deps = buildDependenciesForMain 0 allLocalNames [] (init body)
              (graph, findNode, findVertex) = graphFromEdges deps
              (_, key, _) = last deps
              Just lastVertex = findVertex key
              reachedVertices = reachable graph lastVertex
              reachedNodes = map findNode reachedVertices
              newBody = concat $ map (\(exps, _, _) -> exps) reachedNodes
              newBody' = filter (`elem` newBody) body
              newBodyWithReturn = newBody' ++ [last body]
              newMainFunction = Typed qt area metadata (Assignment n (Typed qt' area' metadata' (Definition params newBodyWithReturn)))
          in  ast { aexps = init (aexps ast) ++ [newMainFunction] }
        else
          ast
  else
    ast


buildDependenciesForAllExps :: [Exp] -> [([Exp], String, [String])]
buildDependenciesForAllExps exps =
  let allLocalNames = Maybe.mapMaybe getExpName exps
  in  buildDependencies allLocalNames [] exps


buildDependenciesForMain :: Int -> [String] -> [Exp] -> [Exp] -> [([Exp], String, [String])]
buildDependenciesForMain expIndex localNames cachedExps exps = case exps of
  e : es ->
    case getExpName e of
      Just n ->
        let deps = buildDependencies' localNames n e
        in  (cachedExps ++ [e], n, deps) : buildDependenciesForMain (expIndex + 1) localNames [] es

      Nothing ->
        let expName = "exp__" <> show expIndex
            deps = buildDependencies' localNames expName e
        in  (cachedExps ++ [e], expName, deps) : buildDependenciesForMain (expIndex + 1) localNames [] es
    -- case getExpName e of
    --   Just n ->
    --     let deps = buildDependencies' localNames n e
    --     in  (cachedExps ++ [e], n, deps) : buildDependencies localNames [] es

    --   Nothing ->
    --     buildDependencies localNames (cachedExps ++ [e]) es

  [] ->
    []


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

  Typed _ _ _ (While cond body) ->
    buildDependencies' localNames expName cond
    ++ buildDependencies' localNames expName body

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

  Typed _ _ _ (ArrayAccess arr index) ->
    buildDependencies' localNames expName arr ++ buildDependencies' localNames expName index

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

