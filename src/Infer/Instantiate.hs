{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Instantiate where


import Infer.Type
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Infer.Substitute


letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']


newTVar :: Infer Type
newTVar = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ TV (letters !! count s)


instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const newTVar) as
  let s = M.fromList $ zip as as'
  return $ apply s t