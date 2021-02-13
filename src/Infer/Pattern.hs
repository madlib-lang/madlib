{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Pattern where

import qualified AST.Canonical                 as Can
import           Infer.Type
import           Infer.Infer
import           Infer.Instantiate
import           Infer.Scheme
import           Infer.Unify
import           Infer.Env
import           Infer.Substitute
import qualified Utils.Tuple                   as T
import qualified Data.Map                      as M


inferPatterns :: Env -> [Can.Pattern] -> Infer ([Pred], Vars, [Type])
inferPatterns env pats = do
  psasts <- mapM (inferPattern env) pats
  let ps = concat [ ps' | (ps', _, _) <- psasts ]
      as = foldr M.union M.empty [ vars | (_, vars, _) <- psasts ]
      ts = [ t | (_, _, t) <- psasts ]
  return (ps, as, ts)

inferPattern :: Env -> Can.Pattern -> Infer ([Pred], Vars, Type)
inferPattern env (Can.Canonical _ pat) = case pat of
  Can.PNum  _ -> return ([], M.empty, tNumber)
  Can.PBool _ -> return ([], M.empty, tBool)
  Can.PStr  _ -> return ([], M.empty, tStr)

  Can.PCon  n -> return ([], M.empty, TCon $ TC n Star)

  Can.PVar  i -> do
    v    <- newTVar Star
    env' <- safeExtendVars env (i, toScheme v)
    return ([], M.singleton i (toScheme v), v)

  Can.PAny -> do
    v <- newTVar Star
    return ([], M.empty, v)

  Can.PTuple pats -> do
    ti <- mapM (inferPattern env) pats
    let ts     = T.lst <$> ti
    let ps     = foldr (<>) [] (T.beg <$> ti)
    let vars   = foldr (<>) M.empty (T.mid <$> ti)

    let tupleT = getTupleCtor (length ts)
    let t      = foldl TApp tupleT ts

    return (ps, vars, t)

  Can.PList pats -> do
    li <- mapM (inferPListItem env) pats
    tv <- newTVar Star
    let ts   = if null li then [tv] else T.lst <$> li
    let ps   = foldr (<>) [] (T.beg <$> li)
    let vars = foldr (<>) M.empty (T.mid <$> li)

    s <- unifyElems (mergeVars env vars) ts

    return
      ( ps
      , M.map (apply s) vars
      , TApp (TCon (TC "List" $ Kfun Star Star)) (apply s (head ts))
      )

   where
    inferPListItem :: Env -> Can.Pattern -> Infer ([Pred], Vars, Type)
    inferPListItem env pat@(Can.Canonical _ p) = case p of
      Can.PSpread (Can.Canonical _ (Can.PVar i)) -> do
        tv <- newTVar Star
        let t' = TApp (TCon (TC "List" $ Kfun Star Star)) tv
        return ([], M.singleton i (toScheme t'), tv)
      _ -> inferPattern env pat

  Can.PRecord pats -> do
    li <- mapM (inferFieldPattern env) pats
    let vars = foldr (<>) M.empty $ T.mid . snd <$> M.toList li
    let ps   = foldr (<>) [] $ T.beg . snd <$> M.toList li
    let ts   = T.lst . snd <$> M.toList li

    return (ps, vars, TRecord (M.map T.lst li) True)

   where
    inferFieldPattern :: Env -> Can.Pattern -> Infer ([Pred], Vars, Type)
    inferFieldPattern env pat@(Can.Canonical _ p) = case p of
      Can.PSpread (Can.Canonical _ (Can.PVar i)) -> do
        tv <- newTVar Star
        return ([], M.singleton i (toScheme tv), tv)

      _ -> inferPattern env pat

  Can.PCtor n pats -> do
    (ps, vars, ts) <- inferPatterns env pats
    tv             <- newTVar Star
    sc             <- lookupVar env n
    (ps' :=> t)    <- instantiate sc
    s              <- unify t (foldr fn tv ts)

    return (ps <> ps', vars, apply s tv)
