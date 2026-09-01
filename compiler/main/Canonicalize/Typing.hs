{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Canonicalize.Typing where


import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Canonicalize.CanonicalM
import           Canonicalize.Env
import           Canonicalize.EnvUtils
import           Infer.Type
import           Infer.Scheme
import           Infer.Unify
import           Infer.Infer
import           Infer.Instantiate
import           Infer.Substitute
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Char
import           Error.Error
import           Error.Context
import           Explain.Location              ( Area )
import           Control.Monad                  ( when )
import           Control.Monad.Except
import           Data.List
import Debug.Trace
import Text.Show.Pretty
import qualified Data.Maybe as Maybe
import Data.Hashable (hash)


canonicalizeTyping :: Src.Typing -> CanonicalM Can.Typing
canonicalizeTyping (Src.Source area _ t) = case t of
  Src.TRSingle name -> do
    let nameToPush = if "." `isInfixOf` name then takeWhile (/= '.') name else name
    pushNameAccess nameToPush
    return $ Can.Canonical area (Can.TRSingle name)

  Src.TRComp name typings -> do
    let nameToPush = if "." `isInfixOf` name then takeWhile (/= '.') name else name
    pushNameAccess nameToPush
    typings' <- mapM canonicalizeTyping typings
    return $ Can.Canonical area (Can.TRComp name typings')

  Src.TRArr left right -> do
    left'  <- canonicalizeTyping left
    right' <- canonicalizeTyping right
    return $ Can.Canonical area (Can.TRArr left' right')

  Src.TRRecord fields base -> do
    fields' <- mapM (\(area, t) -> (area,) <$> canonicalizeTyping t) fields
    base'   <- mapM canonicalizeTyping base
    return $ Can.Canonical area (Can.TRRecord fields' base')

  Src.TRTuple typings -> do
    typings' <- mapM canonicalizeTyping typings
    return $ Can.Canonical area (Can.TRTuple typings')

  Src.TRConstrained constraints typing -> do
    constraints' <- mapM canonicalizeTyping constraints
    typing'      <- canonicalizeTyping typing
    return $ Can.Canonical area (Can.TRConstrained constraints' typing')


canonicalizeTyping' :: Src.Typing -> Can.Typing
canonicalizeTyping' (Src.Source area _ t) = case t of
  Src.TRSingle name ->
    Can.Canonical area (Can.TRSingle name)

  Src.TRComp name typings ->
    let typings' = canonicalizeTyping' <$> typings
    in  Can.Canonical area (Can.TRComp name typings')

  Src.TRArr left right -> do
    let left'  = canonicalizeTyping' left
    let right' = canonicalizeTyping' right
    Can.Canonical area (Can.TRArr left' right')

  Src.TRRecord fields base ->
    let fields' = (\(area, t) -> (area, canonicalizeTyping' t)) <$> fields
        base'   = canonicalizeTyping' <$> base
    in  Can.Canonical area (Can.TRRecord fields' base')

  Src.TRTuple typings -> 
    let typings' = canonicalizeTyping' <$> typings
    in  Can.Canonical area (Can.TRTuple typings')

  Src.TRConstrained constraints typing -> 
    let constraints' = canonicalizeTyping' <$> constraints
        typing'      = canonicalizeTyping' typing
    in  Can.Canonical area (Can.TRConstrained constraints' typing')



typingToScheme :: Env -> Src.Typing -> CanonicalM Scheme
typingToScheme env typing = do
  (ps :=> t) <- qualTypingToQualType env typing
  let vars = S.toList $ S.fromList $ collectVars t <> concat (collectPredVars <$> ps)
  return $ quantify vars (ps :=> t)


qualTypingToQualType :: Env -> Src.Typing -> CanonicalM (Qual Type)
qualTypingToQualType env t@(Src.Source _ _ typing) = case typing of
  Src.TRConstrained constraints typing' -> do
    t  <- typingToType env (KindRequired Star) typing'
    ps <- mapM (constraintToPredicate env t) constraints
    return $ ps :=> t

  _ -> ([] :=>) <$> typingToType env (KindRequired Star) t


constraintToPredicate :: Env -> Type -> Src.Typing -> CanonicalM Pred
constraintToPredicate env t (Src.Source _ _ (Src.TRComp n typings)) = do
  -- Imported interfaces are resolved lazily during canonicalisation.  Check
  -- arity whenever the interface is already available locally; otherwise
  -- retain the predicate and let the normal resolved-interface path validate
  -- it later.
  case M.lookup n (envInterfaces env) of
    Just (Interface tvs _ _) -> when (length typings /= length tvs) $
      throwError $ CompilationError
        (WrongInterfaceArgCount n (length tvs) (length typings))
        NoContext
    Nothing -> return ()
  let s = buildVarSubsts t
  ts <- mapM
    (\case
      Src.Source _ _ (Src.TRSingle var)                   -> return $ apply s $ TVar $ TV (hash var) Star

      fullTyping@(Src.Source _ _ (Src.TRComp _ _)) -> do
        apply s <$> typingToType env (KindRequired Star) fullTyping

      _ -> undefined
    )
    typings

  return $ IsIn n ts Nothing

constraintToPredicate _ _ _ = undefined


data KindRequirement
  = KindRequired Kind
  | AnyKind


validateKind :: Env -> Area -> KindRequirement -> Type -> CanonicalM Type
validateKind _ _ AnyKind parsedType =
  return parsedType
validateKind env area (KindRequired expected) parsedType
  | expected == Row
  , Just row <- typeAsRow parsedType = return row
  | kind parsedType == expected = return parsedType
  | otherwise =
      throwError $ CompilationError
        (TypingHasWrongKind parsedType expected (kind parsedType))
        (Context (envCurrentPath env) area)


typeAsRow :: Type -> Maybe Type
typeAsRow ty = case ty of
  TRowEmpty -> Just TRowEmpty
  TRowExtend{} -> Just ty
  TVar tv | kind tv == Row -> Just ty
  TRecordRow row optionalFields -> Just (rowFromFields optionalFields row)
  TAlias _ _ _ inner -> typeAsRow inner
  _ -> Nothing


typingToType :: Env -> KindRequirement -> Src.Typing -> CanonicalM Type
typingToType env kindNeeded (Src.Source area _ (Src.TRSingle t))
  | t == "Integer" = validateKind env area kindNeeded tInteger
  | t == "Float"   = validateKind env area kindNeeded tFloat
  | t == "Byte"    = validateKind env area kindNeeded tByte
  | t == "Short"   = validateKind env area kindNeeded tShort
  | t == "Boolean" = validateKind env area kindNeeded tBool
  | t == "String"  = validateKind env area kindNeeded tStr
  | t == "Char"    = validateKind env area kindNeeded tChar
  | t == "{}"      = validateKind env area kindNeeded tUnit
  | isLower $ head t =
      let variableKind = case kindNeeded of
            KindRequired k -> k
            AnyKind        -> Star
      in  return (TVar $ TV (hash t) variableKind)
  | otherwise = do
    pushTypeAccess t
    h <- catchError
      (lookupADT env t)
      (\(CompilationError e _) -> throwError $ CompilationError e (Context (envCurrentPath env) area))

    parsedType <- case h of
      (TAlias _ id vars _) ->
        if not (null vars) then
          throwError $ CompilationError (WrongAliasArgCount id (length vars) 0) (Context (envCurrentPath env) area)
        else
          updateAliasVars (getConstructorCon h) []

      t                -> return $ getConstructorCon t

    validateKind env area kindNeeded parsedType



typingToType env kindNeeded (Src.Source area _ (Src.TRComp t ts))
  | isLower . head $ t = do
    params <- mapM (typingToType env AnyKind) ts
    validateKind env area kindNeeded
      (foldl' TApp (TVar $ TV (hash t) (buildKind (length ts))) params)
  | otherwise = do
    pushTypeAccess t
    h <- catchError
      (lookupADT env t)
      (\(CompilationError e _) -> throwError $ CompilationError e (Context (envCurrentPath env) area))

    let (Forall ks (_ :=> rr)) = quantify (ftvList h) ([] :=> h)

    let kargs = case h of
          TAlias _ _ tvs _ -> kind <$> tvs
          _ ->
            (\case
                TGen x -> ks !! x
                _      -> Star
              )
              <$> getConstructorArgs rr

    case h of
      TAlias _ id tvs _
        | length tvs /= length ts ->
            throwError $ CompilationError
              (WrongAliasArgCount id (length tvs) (length ts))
              (Context (envCurrentPath env) area)
      _
        | length ts > length kargs ->
            throwError $ CompilationError
              (TypingHasWrongKind h Star (kind h))
              (Context (envCurrentPath env) area)
      _ -> return ()

    params <- mapM
      (\(typin, k) -> do
        pt <- typingToType env (KindRequired k) typin
        case pt of
          TVar (TV n _) -> return $ TVar (TV n k)
          _             -> return pt
      )
      (zip ts kargs)

    parsedType <- case h of
      TAlias{} ->
        updateAliasVars (getConstructorCon h) params

      t ->
        return $ foldl' TApp (getConstructorCon t) params

    validateKind env area kindNeeded parsedType


typingToType env kindNeeded (Src.Source area _ (Src.TRArr l r)) = do
  l' <- typingToType env (KindRequired Star) l
  r' <- typingToType env (KindRequired Star) r
  validateKind env area kindNeeded (l' `fn` r')

typingToType env kindNeeded (Src.Source area _ (Src.TRRecord fields base)) = do
  fields' <- mapM (typingToType env (KindRequired Star)) (snd <$> fields)
  base'   <- mapM (typingToType env (KindRequired Row)) base

  rowTail <- case base' of
    Nothing -> return TRowEmpty
    Just baseType -> asRowTail baseType

  let (baseFields, _) = visibleRow rowTail
      fieldNames = S.toList $ M.keysSet fields' `S.union` M.keysSet baseFields
  pushRecordToDerive fieldNames

  -- A record expression itself has kind Star even though its internal tail
  -- has kind Row. Nested record syntax in a spread position is converted to
  -- its row above rather than embedded as an ill-kinded row tail.
  let row = rowFromFields fields' rowTail
  case kindNeeded of
    KindRequired Row -> return row
    _ -> validateKind env area kindNeeded (TRecordRow row mempty)
  where
    asRowTail ty = case ty of
      TRowEmpty -> return TRowEmpty
      TRowExtend{} -> return ty
      TVar tv | kind tv == Row -> return ty
      TRecordRow row optionalFields ->
        return (rowFromFields optionalFields row)
      TAlias _ _ _ inner -> asRowTail inner
      _ -> throwError $ CompilationError
        (TypingHasWrongKind ty Row (kind ty))
        (Context (envCurrentPath env) area)

typingToType env kindNeeded (Src.Source area _ (Src.TRTuple elems)) = do
  elems' <- mapM (typingToType env (KindRequired Star)) elems
  let tupleT = getTupleCtor (length elems)
  validateKind env area kindNeeded (foldl' TApp tupleT elems')

-- Never happens as it's handled in the qualTypingToQualType function
typingToType _ _ (Src.Source _ _ (Src.TRConstrained _ _)) = undefined


getConstructorArgs :: Type -> [Type]
getConstructorArgs t = case t of
  TApp l r ->
    getConstructorArgs l <> [r]

  TCon _ _ _ ->
    []

  TAlias _ _ tvars _ ->
    TVar <$> tvars

  _ ->
    [t]



updateAliasVars :: Type -> [Type] -> CanonicalM Type
updateAliasVars t args = do
  case t of
    TAlias _ _ vars t' ->
      let instArgs = M.fromList $ zip vars args
          update :: Type -> CanonicalM Type
          update ty = case ty of
            TVar tv ->
              case M.lookup tv instArgs of
                Just x  ->
                  return x

                Nothing -> case tv of
                  TV i k -> return $ TVar (TV i k)

            TApp l r -> do
              l' <- update l
              r' <- update r
              return $ TApp l' r'

            TRowEmpty ->
              return TRowEmpty

            TRowExtend label fieldType tail -> do
              fieldType' <- update fieldType
              tail' <- update tail
              return $ TRowExtend label fieldType' tail'

            TRecordRow row optionalFields -> do
              row' <- update row
              optionalFields' <- mapM update optionalFields
              return $ TRecordRow row' optionalFields'

            TCon _ _ _ ->
              return ty

            _ -> undefined
      in  update t'

    _ -> return t
