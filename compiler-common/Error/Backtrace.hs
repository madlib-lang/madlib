module Error.Backtrace where

import qualified AST.Canonical                 as Can


type Backtrace = [BTNode]

data BTNode
  = BTExp Can.Exp
  | BTInstance Can.Instance
  | BTConstructor Can.Constructor
  deriving(Eq, Show)
