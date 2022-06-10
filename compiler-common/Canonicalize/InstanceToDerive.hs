{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Canonicalize.InstanceToDerive where

import           Data.Hashable
import           GHC.Generics       hiding(Constructor)
import           AST.Canonical
import qualified Data.Set           as Set


-- List of typedecl found in the AST for which we need to derive
-- an instance of Eq
data InstanceToDerive
  = TypeDeclToDerive TypeDecl
  | RecordToDerive (Set.Set String)
  deriving(Eq, Show, Ord, Generic, Hashable)
