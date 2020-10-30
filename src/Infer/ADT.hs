{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Infer.ADT where

import qualified Data.Map as M
import           Control.Monad.Except
import           Data.Char                      ( isLower )

import           AST.AST
import           Infer.Type


buildADTTypes :: [ADT] -> Infer ADTs
buildADTTypes = buildADTTypes' M.empty


buildADTTypes' :: ADTs -> [ADT] -> Infer ADTs
buildADTTypes' _    []    = return M.empty
buildADTTypes' adts [adt] = do
  (k, v) <- buildADTType adts adt
  return $ M.singleton k v
buildADTTypes' adts (adt : xs) = do
  a    <- buildADTTypes' adts [adt]
  next <- buildADTTypes' (M.union a adts) xs
  return $ M.union a next


buildADTType :: ADTs -> ADT -> Infer (String, Type)
buildADTType adts ADT { adtname, adtparams } = case M.lookup adtname adts of
  Just t  -> throwError $ ADTAlreadyDefined t
  Nothing -> return (adtname, TComp adtname (TVar . TV <$> adtparams))


resolveADTs :: ADTs -> [ADT] -> Infer Vars
resolveADTs tadts adts = mergeVars <$> mapM (resolveADT tadts) adts
 where
  mergeVars []   = M.empty
  mergeVars vars = foldr1 M.union vars


resolveADT :: ADTs -> ADT -> Infer Vars
resolveADT tadts ADT { adtname, adtconstructors, adtparams } =
  foldr1 M.union
    <$> mapM (resolveADTConstructor tadts adtname adtparams) adtconstructors


-- TODO: Verify that Constructors aren't already in the global space or else throw a name clash error
resolveADTConstructor :: ADTs -> Name -> [Name] -> ADTConstructor -> Infer Vars
resolveADTConstructor tadts n params ADTConstructor { adtcname, adtcargs } = do
  let t = buildADTConstructorReturnType n params
  case adtcargs of
    Just cargs -> do
      t' <- mapM (argToType tadts n params) cargs
      let ctype = foldr1 TArr (t' <> [t])
      return $ M.fromList [(adtcname, Forall (TV <$> params) ctype)]
    Nothing -> return $ M.fromList [(adtcname, Forall (TV <$> params) t)]

buildADTConstructorReturnType :: Name -> [Name] -> Type
buildADTConstructorReturnType tname tparams =
  TComp tname $ TVar . TV <$> tparams

-- TODO: This should probably be merged with typingToType somehow
argToType :: ADTs -> Name -> [Name] -> TypeRef -> Infer Type
argToType tadts _ params (TRSingle n)
  | n == "Num" = return $ TCon CNum
  | n == "Bool" = return $ TCon CBool
  | n == "String" = return $ TCon CString
  | isLower (head n) && (n `elem` params) = return $ TVar $ TV n
  | isLower (head n) = throwError $ UnboundVariable n
  | otherwise = case M.lookup n tadts of
    Just a  -> return a
    -- If the lookup gives a Nothing, it should most likely be an undefined type error ?
    Nothing -> return $ TCon $ CUserDef n

argToType tadts name params (TRComp tname targs) = case M.lookup tname tadts of
  -- TODO: Verify the length of tparams and make sure it matches the one of targs ! otherwise
  -- we have a type application error.
  Just (TComp n _) -> TComp n <$> mapM (argToType tadts name params) targs
  Nothing          -> return $ TCon $ CUserDef name

argToType tadts name params (TRArr l r) = do
  l' <- argToType tadts name params l
  r' <- argToType tadts name params r
  return $ TArr l' r'

argToType tadts name params (TRRecord f) = do
  f' <- mapM (argToType tadts name params) f
  return $ TRecord f'