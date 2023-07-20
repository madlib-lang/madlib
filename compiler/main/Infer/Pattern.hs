{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Pattern where

import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Type
import           Infer.Infer
import           Infer.Instantiate
import           Infer.Scheme
import           Infer.Unify
import           Infer.EnvUtils
import           Infer.Env
import           Infer.Substitute
import qualified Utils.Tuple                   as T
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.List
import           Error.Context
import           Error.Error
import           Control.Monad.Except
import           Data.Foldable
import qualified Data.Maybe as Maybe
import Explain.Location (Area)


inferPatterns :: Env -> [Can.Pattern] -> Infer ([Slv.Pattern], [Pred], Vars, [Type])
inferPatterns env pats = do
  psasts <- mapM (inferPattern env) pats
  let ps   = concat [ ps' | (_, ps', _, _) <- psasts ]
      as   = foldr M.union M.empty [ vars | (_, _, vars, _) <- psasts ]
      ts   = [ t | (_, _, _, t) <- psasts ]
      pats = [ pats' | (pats', _, _, _) <- psasts ]
  return (pats, ps, as, ts)

inferPattern :: Env -> Can.Pattern -> Infer (Slv.Pattern, [Pred], Vars, Type)
inferPattern env p@(Can.Canonical area pat) = case pat of
  Can.PNum n -> do
    numberType <- newTVar Star
    return (Slv.Typed ([IsIn "Number" [numberType] Nothing] :=> numberType) area (Slv.PNum n), [IsIn "Number" [numberType] Nothing], M.empty, numberType)

  Can.PBool b ->
    return (Slv.Typed ([] :=> tBool) area (Slv.PBool b), [], M.empty, tBool)

  Can.PStr s ->
    return (Slv.Typed ([] :=> tStr) area (Slv.PStr s), [], M.empty, tStr)

  Can.PChar s ->
    return (Slv.Typed ([] :=> tChar) area (Slv.PChar s), [], M.empty, tChar)

  Can.PVar i -> do
    v <- newTVar Star
    when (Maybe.isJust $ M.lookup i (envVars env)) $
      throwError $ CompilationError (NameAlreadyDefined i) (Context (envCurrentPath env) area)
    return (Slv.Typed ([] :=> v) area (Slv.PVar i), [], M.singleton i (toScheme v), v)

  Can.PAny -> do
    v <- newTVar Star
    return (Slv.Typed ([] :=> v) area Slv.PAny, [], M.empty, v)

  Can.PTuple pats -> do
    ti <- mapM (inferPattern env) pats
    let ts     = (\(_, _, _, a) -> a) <$> ti
    let ps     = foldr (<>) [] ((\(_, a, _, _) -> a) <$> ti)
    let vars   = foldr (<>) M.empty ((\(_, _, a, _) -> a) <$> ti)
    let pats'  = (\(a, _, _, _) -> a) <$> ti

    let tupleT = getTupleCtor (length ts)
    let t      = foldl' TApp tupleT ts

    return (Slv.Typed ([] :=> t) area (Slv.PTuple pats'), ps, vars, t)

  Can.PList pats -> do
    tv <- newTVar Star

    (pats, ps, vars, t) <- foldlM
      (\(pats, ps, vars, t) pat -> do
        (pat', ps', vars', t') <- inferPListItem env t pat
        s                      <- contextualUnify env pat t t'
        return (pats ++ [pat'], ps ++ ps', M.map (apply s) vars <> M.map (apply s) vars', apply s t)
      )
      ([], [], mempty, tv)
      pats

    return (Slv.Typed ([] :=> tListOf t) area (Slv.PList pats), ps, vars, tListOf t)

   where
    inferPListItem :: Env -> Type -> Can.Pattern -> Infer (Slv.Pattern, [Pred], Vars, Type)
    inferPListItem env listType pat@(Can.Canonical spreadArea p) = case p of
      Can.PSpread (Can.Canonical varArea Can.PAny) -> do
        let t' = tListOf listType
        return (Slv.Typed ([] :=> t') spreadArea (Slv.PSpread (Slv.Typed ([] :=> t') varArea Slv.PAny)), [], M.empty, listType)

      Can.PSpread (Can.Canonical varArea (Can.PVar i)) -> do
        let t' = tListOf listType
        return (Slv.Typed ([] :=> t') spreadArea (Slv.PSpread (Slv.Typed ([] :=> t') varArea (Slv.PVar i))), [], M.singleton i (toScheme t'), listType)

      Can.PSpread (Can.Canonical varArea (Can.PNum n)) -> do
        let t' = tListOf listType
        let itemType = listItemType t'
        return (Slv.Typed ([IsIn "Number" [itemType] Nothing] :=> t') spreadArea (Slv.PSpread (Slv.Typed ([IsIn "Number" [itemType] Nothing] :=> t') varArea (Slv.PNum n))), [IsIn "Number" [itemType] Nothing], M.empty, listType)

      _ -> inferPattern env pat

  Can.PRecord pats -> do
    fields <- mapM (inferFieldPattern env) pats
    tv <- newTVar Star
    let ts     = (\(_, _, _, a) -> a) <$> fields
    let ps     = foldr (<>) [] ((\(_, a, _, _) -> a) <$> fields)
    let vars   = foldr (<>) M.empty ((\(_, _, a, _) -> a) <$> fields)
    let pats'  = (\(a, _, _, _) -> a) <$> fields

    let t = TRecord ts (Just tv) mempty

    return (Slv.Typed ([] :=> t) area (Slv.PRecord pats'), ps, vars, t)

   where
    inferFieldPattern :: Env -> Can.Pattern -> Infer (Slv.Pattern, [Pred], Vars, Type)
    inferFieldPattern env pat@(Can.Canonical spreadArea p) = case p of
      Can.PSpread (Can.Canonical varArea Can.PAny) -> do
        tv <- newTVar Star
        return (Slv.Typed ([] :=> tv) spreadArea (Slv.PSpread (Slv.Typed ([] :=> tv) varArea Slv.PAny)), [], M.empty, tv)

      Can.PSpread (Can.Canonical varArea (Can.PVar i)) -> do
        tv <- newTVar Star
        return (Slv.Typed ([] :=> tv) spreadArea (Slv.PSpread (Slv.Typed ([] :=> tv) varArea (Slv.PVar i))), [], M.singleton i (toScheme tv), tv)

      _ -> inferPattern env pat

  Can.PCon n pats -> do
    (pats', ps, vars, ts) <- inferPatterns env pats
    tv                    <- newTVar Star
    sc                    <- catchError
      (lookupVar env n)
      (\(CompilationError e _) -> throwError $ CompilationError e (Context (envCurrentPath env) area))

    verifyConstructor env area n

    (ps' :=> t) <- instantiate sc
    s           <- contextualUnify env p t (foldr fn tv ts)

    let t = apply s tv

    return (Slv.Typed ([] :=> t) area (Slv.PCon n pats'), ps <> ps', M.map (apply s) vars, t)

  _ -> undefined


verifyConstructor :: Env -> Area -> String -> Infer ()
verifyConstructor env area name = do
  unless (isConstructor env name || name == "Dictionary") $ do
    throwError $ CompilationError (NotAConstructor name) (Context (envCurrentPath env) area)
