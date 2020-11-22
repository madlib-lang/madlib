{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Infer.ADT where

import qualified Data.Map                      as M
import           Control.Monad.Except
import           Data.Char                      ( isLower )

import           AST.Source
import           Infer.Type
import           Infer.Infer
import           Error.Error
import           Explain.Reason
import           Explain.Meta


buildADTTypes :: FilePath -> [ADT] -> Infer ADTs
buildADTTypes astPath = buildADTTypes' astPath M.empty


buildADTTypes' :: FilePath -> ADTs -> [ADT] -> Infer ADTs
buildADTTypes' _       _    []    = return M.empty
buildADTTypes' astPath adts [adt] = do
  (k, v) <- buildADTType astPath adts adt
  return $ M.singleton k v
buildADTTypes' astPath adts (adt : xs) = do
  a    <- buildADTTypes' astPath adts [adt]
  next <- buildADTTypes' astPath (M.union a adts) xs
  return $ M.union a next


buildADTType :: FilePath -> ADTs -> ADT -> Infer (String, Type)
buildADTType astPath adts ADT { adtname, adtparams } =
  case M.lookup adtname adts of
    Just t -> throwError $ InferError (ADTAlreadyDefined t) NoReason
    Nothing ->
      return (adtname, TComp astPath adtname (TVar . TV <$> adtparams))


resolveADTs :: FilePath -> ADTs -> [ADT] -> Infer Vars
resolveADTs astPath tadts adts =
  mergeVars <$> mapM (resolveADT astPath tadts) adts
 where
  mergeVars []   = M.empty
  mergeVars vars = foldr1 M.union vars


resolveADT :: FilePath -> ADTs -> ADT -> Infer Vars
resolveADT astPath tadts ADT { adtname, adtconstructors, adtparams } =
  foldr1 M.union
    <$> mapM (resolveADTConstructor astPath tadts adtname adtparams)
             adtconstructors


-- TODO: Verify that Constructors aren't already in the global space or else throw a name clash error
-- Use lookupADT for that
resolveADTConstructor
  :: FilePath -> ADTs -> Name -> [Name] -> Constructor -> Infer Vars
resolveADTConstructor astPath tadts n params (Constructor cname cparams) = do
  let t = buildADTConstructorReturnType astPath n params
  t' <- mapM (argToType tadts n params) cparams
  let ctype = foldr1 TArr (t' <> [t])
  return $ M.fromList [(cname, Forall (TV <$> params) ctype)]

buildADTConstructorReturnType :: FilePath -> Name -> [Name] -> Type
buildADTConstructorReturnType astPath tname tparams =
  TComp astPath tname $ TVar . TV <$> tparams


-- TODO: This should probably be merged with typingToType somehow
argToType :: ADTs -> Name -> [Name] -> Typing -> Infer Type
argToType tadts _ params (Meta _ _ (TRSingle n))
  | n == "Number" = return $ TCon CNum
  | n == "Boolean" = return $ TCon CBool
  | n == "String" = return $ TCon CString
  | isLower (head n) && (n `elem` params) = return $ TVar $ TV n
  | isLower (head n) = throwError $ InferError (UnboundVariable n) NoReason
  | otherwise = case M.lookup n tadts of
    Just a  -> return a
    -- If the lookup gives a Nothing, it should most likely be an undefined type error ?
    Nothing -> return $ TCon $ CUserDef n

argToType tadts name params (Meta _ _ (TRComp tname targs)) =
  case M.lookup tname tadts of
  -- TODO: Verify the length of tparams and make sure it matches the one of targs ! otherwise
  -- we have a type application error.
    Just (TComp "TBD" n _) ->
      TComp "TBD" n <$> mapM (argToType tadts name params) targs
    Nothing -> return $ TCon $ CUserDef name

argToType tadts name params (Meta _ _ (TRArr l r)) = do
  l' <- argToType tadts name params l
  r' <- argToType tadts name params r
  return $ TArr l' r'

argToType tadts name params (Meta _ _ (TRRecord f)) = do
  f' <- mapM (argToType tadts name params) f
  return $ TRecord f' False
