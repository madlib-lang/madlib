{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.Typing where


import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Canonicalize.CanonicalM
import           Canonicalize.Env
import           Infer.Type
import           Infer.Scheme
import           Infer.Substitute
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Char


canonicalizeTyping :: Src.Typing -> CanonicalM Can.Typing
canonicalizeTyping (Src.Source _ area t) = case t of
  Src.TRSingle name       -> return $ Can.Canonical area (Can.TRSingle name)

  Src.TRComp name typings -> do
    typings' <- mapM canonicalizeTyping typings
    return $ Can.Canonical area (Can.TRComp name typings')

  Src.TRArr left right -> do
    left'  <- canonicalizeTyping left
    right' <- canonicalizeTyping right
    return $ Can.Canonical area (Can.TRArr left' right')

  Src.TRRecord fields -> do
    fields' <- mapM canonicalizeTyping fields
    return $ Can.Canonical area (Can.TRRecord fields')

  Src.TRTuple typings -> do
    typings' <- mapM canonicalizeTyping typings
    return $ Can.Canonical area (Can.TRTuple typings')

  Src.TRConstrained constraints typing -> do
    constraints' <- mapM canonicalizeTyping constraints
    typing'      <- canonicalizeTyping typing
    return $ Can.Canonical area (Can.TRConstrained constraints' typing')



typingToScheme :: Env -> Src.Typing -> CanonicalM Scheme
typingToScheme env typing = do
  (ps :=> t) <- qualTypingToQualType env typing
  let vars =
        S.toList $ S.fromList $ collectVars t <> concat (collectPredVars <$> ps)
  return $ quantify vars (ps :=> t)


qualTypingToQualType :: Env -> Src.Typing -> CanonicalM (Qual Type)
qualTypingToQualType env t@(Src.Source _ _ typing) = case typing of
  Src.TRConstrained constraints typing' -> do
    t  <- typingToType env typing'
    ps <- mapM (constraintToPredicate env t) constraints
    return $ ps :=> t

  _ -> ([] :=>) <$> typingToType env t


constraintToPredicate :: Env -> Type -> Src.Typing -> CanonicalM Pred
constraintToPredicate env t (Src.Source _ _ (Src.TRComp n typings)) = do
  let s = buildVarSubsts t
  ts <- mapM
    (\case
      Src.Source _ _ (Src.TRSingle var) ->
        return $ apply s $ TVar $ TV var Star

      fullTyping@(Src.Source _ _ (Src.TRComp n typings')) -> do
        apply s <$> typingToType env fullTyping
    )
    typings

  return $ IsIn n ts


typingToType :: Env -> Src.Typing -> CanonicalM Type
typingToType env (Src.Source _ _ (Src.TRSingle t))
  | t == "Number" = return tNumber
  | t == "Boolean" = return tBool
  | t == "String" = return tStr
  | t == "()" = return tUnit
  | isLower $ head t = return (TVar $ TV t Star)
  | otherwise = do
    h <- lookupADT env t
    case h of
      (TAlias _ _ _ t) -> updateAliasVars (getConstructorCon h) []
      t                -> return $ getConstructorCon t


typingToType env (Src.Source _ area (Src.TRComp t ts))
  | isLower . head $ t = do
    params <- mapM (typingToType env) ts
    return $ foldl TApp (TVar $ TV t (buildKind (length ts))) params
  | otherwise = do
    h <- lookupADT env t

    let (Forall ks (_ :=> rr)) = quantify (ftv h) ([] :=> h)

    let kargs =
          (\case
              (TGen x) -> ks !! x
              _        -> Star
            )
            <$> getConstructorArgs rr

    params <- mapM
      (\(typin, k) -> do
        pt <- typingToType env typin
        case pt of
          TVar (TV n _) -> return $ TVar (TV n k)
          _             -> return pt
      )
      (zip ts kargs)
    case h of
      (TAlias _ _ tvs t) -> updateAliasVars (getConstructorCon h) params
      t                  -> return $ foldl TApp (getConstructorCon t) params


typingToType env (Src.Source _ _ (Src.TRArr l r)) = do
  l' <- typingToType env l
  r' <- typingToType env r
  return $ l' `fn` r'

typingToType env (Src.Source _ _ (Src.TRRecord fields)) = do
  fields' <- mapM (typingToType env) fields
  return $ TRecord fields' False

typingToType env (Src.Source _ _ (Src.TRTuple elems)) = do
  elems' <- mapM (typingToType env) elems
  let tupleT = getTupleCtor (length elems)
  return $ foldl TApp tupleT elems'


getConstructorArgs :: Type -> [Type]
getConstructorArgs t = case t of
  TApp l r -> getConstructorArgs l <> [r]
  TCon _   -> []
  _        -> [t]


updateAliasVars :: Type -> [Type] -> CanonicalM Type
updateAliasVars t args = do
  case t of
    TAlias _ _ vars t' ->
      let instArgs = M.fromList $ zip vars args

          update :: Type -> CanonicalM Type
          update ty = case ty of
            TVar tv -> case M.lookup tv instArgs of
              Just x  -> return x
              Nothing -> return ty
            TApp l r -> do
              l' <- update l
              r' <- update r
              return $ TApp l' r'
            TCon _       -> return ty
            TRecord fs o -> do
              fs' <- mapM update fs
              return $ TRecord fs' o
      in  update t'

    _ -> return t
