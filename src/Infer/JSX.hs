{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Infer.JSX where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Explain.Location
import           Data.List                      ( (\\)
                                                , union
                                                , partition
                                                , foldl'
                                                )
import           Infer.Infer
import           Infer.Type
import           Infer.Env
import           Infer.Substitute
import           Infer.Unify


type InferFunction = Env -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.Exp)

buildTextListChildExp :: Area -> Can.Exp -> Can.Exp
buildTextListChildExp area exp = Can.Canonical area $ Can.App
  (Can.Canonical
    area
    (Can.App (Can.Canonical area (Can.JSExp "((f) => (xs) => xs.map(f))")) (Can.Canonical area (Can.Var "text")) False)
  )
  exp
  True

inferTextListChild
  :: InferFunction -> Env -> Type -> Type -> Can.Exp -> Area -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferTextListChild infer env ty t exp area = do
  let exp'' = buildTextListChildExp area exp
  (s1, ps, t, e) <- infer env exp''
  s2             <- unify t $ tListOf ty
  let s = s1 `compose` s2
  return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListSpread e)

inferJSXExpChild :: InferFunction -> Env -> Type -> Can.Exp -> Infer (Substitution, [Pred], Type, Slv.ListItem)
inferJSXExpChild infer env ty (Can.Canonical area (Can.JSXExpChild exp')) = do
  (s1, ps, t, e) <- infer env exp'

  case t of
    (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") (TCon (TC "String" Star) "prelude")) ->
      inferTextListChild infer env ty t exp' area

    (TApp (TVar (TV _ (Kfun Star Star))) (TCon (TC "String" Star) "prelude")) ->
      inferTextListChild infer env ty t exp' area

    (TApp (TCon (TC "List" (Kfun Star Star)) "prelude") t') -> do
      s2 <- unify t (tListOf ty)
      let s = s1 `compose` s2
      return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListSpread e)

    (TApp (TVar (TV _ (Kfun Star Star))) t') -> do
      s2 <- unify t (tListOf ty)
      let s = s1 `compose` s2
      return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListSpread e)

    TCon (TC "String" Star) "prelude" -> do
      let exp'' = Can.Canonical area $ Can.App (Can.Canonical area (Can.Var "text")) exp' True
      (s1, ps, t, e) <- infer env exp''
      s2             <- unify t ty
      let s = s1 `compose` s2

      return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListItem e)

    t'@(TVar _) -> do
      let exp'' = Can.Canonical area $ Can.App (Can.Canonical area (Can.Var "__tmp_jsx_children__")) exp' True
      (s1, ps, t, e') <- infer (extendVars env ("__tmp_jsx_children__", Forall [] $ [] :=> (t' `fn` ty))) exp''
      s2              <- unify t ty
      let s = s1 `compose` s2

      return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListItem e')

    _ -> do
      s2 <- unify t ty
      let s = s1 `compose` s2
      return (s, ps, apply s ty, Slv.Solved (apply s ps :=> apply s ty) area $ Slv.ListItem e)
