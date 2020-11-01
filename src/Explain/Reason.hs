module Explain.Reason where

import           Explain.Location
import qualified AST.Source              as Src
import           Infer.Type

data Expected = Expected Src.Exp Type deriving(Show, Eq)
data Actual = Actual Src.Exp Type deriving(Show, Eq)

-- data Expectation = TypeExpectation Expected Actual deriving(Show, Eq)

newtype Explanation = WrongTypeApplied Src.Exp deriving(Show, Eq)

data Reason
  = Reason Explanation Area
  -- = Reason { rarea :: Area, rexp :: Src.Exp, rexpectation :: Expectation }
  | NoReason
  deriving(Show, Eq)
