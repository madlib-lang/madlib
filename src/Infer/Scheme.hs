module Infer.Scheme where


import           Infer.Type
import           Infer.Substitute
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Debug.Trace                    ( trace )
import           Text.Show.Pretty               ( ppShow )


quantify :: [TVar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where
  vs' = [ v | v <- ftv qt, v `elem` vs ]
  ks  = map kind vs'
  s   = M.fromList $ zip vs' (map TGen [0 ..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)
