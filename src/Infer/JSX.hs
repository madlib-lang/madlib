{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.JSX where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Substitute
import           Infer.Unify

type InferFunction = Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)

inferJSXExpChild :: InferFunction -> Env -> Type -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferJSXExpChild infer env ty (Can.Canonical area (Can.JSXExpChild exp')) = do
  (s1, ps, t, e) <- infer env exp'
  s2 <- unify ty t
  let s = s1 `compose` s2
  return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListItem e)
