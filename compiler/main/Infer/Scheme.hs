module Infer.Scheme where


import           Infer.Type
import           Infer.Substitute
import qualified Data.Map                      as M
import qualified Data.Set                      as S


quantify :: [TVar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where
  vsSet = S.fromList vs
  vs'   = filter (`S.member` vsSet) (ftvList qt)  -- ORDER from ftvList qt, FILTER by vs membership
  ks    = map kind vs'
  s     = M.fromList $ zip vs' (map TGen [0 ..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)
