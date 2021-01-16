{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Instantiate where


import           Infer.Type
import qualified Data.Map                      as M
import           Control.Monad
import           Control.Monad.State
import           Infer.Substitute
import           Infer.Infer
import           Debug.Trace                    ( trace )
import           Text.Show.Pretty               ( ppShow )


letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']


newTVar :: Kind -> Infer Type
newTVar k = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar $ TV (letters !! count s) k


instantiate :: Scheme -> Infer (Qual Type)
instantiate (Forall ks qt) = do
  ts <- mapM newTVar ks
  return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TApp l r        ) = TApp (inst ts l) (inst ts r)
  inst ts (TGen n          ) = ts !! n
  inst ts (TRecord fields o) = TRecord (M.map (inst ts) fields) o
  inst _  t                  = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)
