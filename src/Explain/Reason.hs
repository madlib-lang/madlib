module Explain.Reason where

import           Explain.Location
import qualified AST.Source                    as Src


data Explanation = WrongTypeApplied Src.Exp
                 | VariableNotDeclared Src.Exp
                 -- IfElseBranchTypesDontMatch ifExp :: Src.Exp falsyExp :: Src.Exp
                 | IfElseBranchTypesDontMatch Src.Exp Src.Exp
                 -- IfElseCondIsNotBool ifExp :: Src.Exp condExp :: Src.Exp
                 | IfElseCondIsNotBool Src.Exp Src.Exp
                 -- PatternTypeError switch :: Src.Exp pattern :: Src.Pattern
                 | PatternTypeError Src.Exp Src.Pattern
                 deriving(Show, Eq)

-- TODO: I think we should get rid of that Area in the Reason
-- in favor of more meaningful Src.Exp in Explanations.
data Reason
  = Reason Explanation FilePath Area
  | NoReason
  deriving(Show, Eq)
