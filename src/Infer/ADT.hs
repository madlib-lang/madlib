{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import           Infer.Instantiate              ( newTVar )


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
buildADTType astPath adts adt = case M.lookup (adtname adt) adts of
  Just t  -> throwError $ InferError (ADTAlreadyDefined t) NoReason
  Nothing -> return
    (adtname adt, TComp astPath (adtname adt) (TVar . TV <$> adtparams adt))


resolveADTs :: Env -> FilePath -> ADTs -> [ADT] -> Infer Vars
resolveADTs priorEnv astPath tadts adts =
  mergeVars <$> mapM (resolveADT priorEnv astPath tadts) adts
 where
  mergeVars []   = M.empty
  mergeVars vars = foldr1 M.union vars


resolveADT :: Env -> FilePath -> ADTs -> ADT -> Infer Vars
resolveADT priorEnv astPath tadts adt =
  let name   = adtname adt
      ctors  = adtconstructors adt
      params = adtparams adt
  in  foldr1 M.union
        <$> mapM (resolveADTConstructor priorEnv astPath tadts name params)
                 ctors


-- TODO: Verify that Constructors aren't already in the global space or else throw a name clash error
-- Use lookupADT for that
resolveADTConstructor
  :: Env -> FilePath -> ADTs -> Name -> [Name] -> Constructor -> Infer Vars
resolveADTConstructor priorEnv astPath tadts n params (Constructor cname cparams)
  = do
    let t = buildADTConstructorReturnType astPath n params
    t' <- mapM (argToType priorEnv tadts n params) cparams
    let ctype = foldr1 TArr (t' <> [t])
    return $ M.fromList [(cname, Forall (TV <$> params) ctype)]

buildADTConstructorReturnType :: FilePath -> Name -> [Name] -> Type
buildADTConstructorReturnType astPath tname tparams =
  TComp astPath tname $ TVar . TV <$> tparams


-- TODO: This should probably be merged with typingToType somehow
argToType :: Env -> ADTs -> Name -> [Name] -> Typing -> Infer Type
argToType _ tadts _ params (Meta _ _ (TRSingle n))
  | n == "Number" = return $ TCon CNum
  | n == "Boolean" = return $ TCon CBool
  | n == "String" = return $ TCon CString
  | isLower (head n) && (n `elem` params) = return $ TVar $ TV n
  | isLower (head n) = newTVar
  | -- A free var that is not in type params
    otherwise = case M.lookup n tadts of
    Just a  -> return a
    Nothing -> throwError $ InferError (UnknownType n) NoReason

argToType priorEnv tadts name params (Meta _ _ (TRComp tname targs)) =
  case M.lookup tname tadts of
  -- TODO: Verify the length of tparams and make sure it matches the one of targs ! otherwise
  -- we have a type application error.
    Just (TComp fp n _) ->
      TComp fp n <$> mapM (argToType priorEnv tadts name params) targs
    Nothing -> if tname == "List"
      then
        TComp "Prelude" tname
          <$> mapM (argToType priorEnv tadts name params) targs
      else case M.lookup tname (envadts priorEnv) of
        Just (TComp path tname _) ->
          TComp path tname <$> mapM (argToType priorEnv tadts name params) targs
        Nothing -> throwError $ InferError (UnknownType tname) NoReason

argToType priorEnv tadts name params (Meta _ _ (TRArr l r)) = do
  l' <- argToType priorEnv tadts name params l
  r' <- argToType priorEnv tadts name params r
  return $ TArr l' r'

argToType priorEnv tadts name params (Meta _ _ (TRRecord f)) = do
  f' <- mapM (argToType priorEnv tadts name params) f
  return $ TRecord f' False

argToType priorEnv tadts name params (Meta _ _ (TRTuple elems)) = do
  elems' <- mapM (argToType priorEnv tadts name params) elems
  return $ TTuple elems'
