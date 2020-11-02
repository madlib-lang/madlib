module Explain.Reason where

import           Explain.Location
import qualified AST.Source                    as Src


data Explanation = WrongTypeApplied Src.Exp
                 | VariableNotDeclared Src.Exp
                 deriving(Show, Eq)

data Reason
  = Reason Explanation Area
  | NoReason
  deriving(Show, Eq)
