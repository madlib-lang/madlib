{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Error.Backtrace where

import           Data.Hashable
import           GHC.Generics hiding(Constructor)
import qualified AST.Canonical                        as Can


type Backtrace = [BTNode]

data BTNode
  = BTExp Can.Exp
  | BTInstance Can.Instance
  | BTConstructor Can.Constructor
  deriving(Eq, Show, Generic, Hashable)
