{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Instantiate where


import           Infer.Type
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Infer.Infer
import Debug.Trace
import Text.Show.Pretty


letters :: [Char]
letters = ['a' .. 'z']

newTVar :: Kind -> Infer Type
newTVar k = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ TV (letters !! (count s `mod` 26) : show (count s)) k


instantiate :: Scheme -> Infer (Qual Type)
instantiate (Forall ks qt) = do
  ts <- mapM newTVar ks
  return (inst ts qt)

-- instantiateOne :: Scheme -> Qual Type
-- instantiateOne sc = unsafeRun $ instantiate sc

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TApp l r           ) = TApp (inst ts l) (inst ts r)
  inst ts (TGen n             ) = ts !! n
  inst ts (TRecord fields base) = TRecord (M.map (inst ts) fields) (inst ts <$> base)
  inst _  t                     = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t maybeArea) = IsIn c (inst ts t) maybeArea
