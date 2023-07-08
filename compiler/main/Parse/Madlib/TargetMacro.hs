module Parse.Madlib.TargetMacro where

import AST.Source
import Run.Target

resolveMacros :: Target -> AST -> AST
resolveMacros target ast =
  ast
    { aimports    = filter (matchTarget target) (aimports ast)
    , aexps       = filter (\exp -> matchTarget target exp && not (isMacroExp exp)) (aexps ast)
    , atypedecls  = filter (matchTarget target) (atypedecls ast)
    , ainterfaces = filter (matchTarget target) (ainterfaces ast)
    , ainstances  = filter (matchTarget target) (ainstances ast)
    , aderived    = filter (matchTarget target) (aderived ast)
    }


matchTarget :: Target -> Source a -> Bool
matchTarget target (Source _ sourceTarget _) = case sourceTarget of
  _ | target == TAny ->
    True

  TargetLLVM | target == TLLVM ->
    True

  TargetJS | target == TNode || target == TBrowser ->
    True

  TargetAll ->
    True

  _ ->
    False
