module Explain.Reason where

import           Explain.Location
import qualified AST.Source                    as Src
import           Infer.Type


data Explanation
  -- WrongTypeApplied fn :: Src.Exp arg :: Src.Exp
  = WrongTypeApplied Src.Exp Src.Exp
  | VariableNotDeclared Src.Exp
  -- IfElseBranchTypesDontMatch ifExp :: Src.Exp falsyExp :: Src.Exp
  | IfElseBranchTypesDontMatch Src.Exp Src.Exp
  -- IfElseCondIsNotBool ifExp :: Src.Exp condExp :: Src.Exp
  | IfElseCondIsNotBool Src.Exp Src.Exp
  -- PatternTypeError switch :: Src.Exp pattern :: Src.Pattern
  | PatternTypeError Src.Exp Src.Pattern
  -- PatternTypeError switch :: Src.Exp pattern :: Src.Pattern
  | PatternConstructorDoesNotExist Src.Exp Src.Pattern
  | WrongImport Src.Import
  -- For now it just holds the Exp, not the TypedExp
  -- More info here: https://github.com/open-sorcerers/madlib/issues/8
  | TypeAndTypingMismatch Src.Exp Src.Typing Type Type
  deriving(Show, Eq)

-- TODO: I think we should get rid of that Area in the Reason
-- in favor of more meaningful Src.Exp in Explanations.
-- TODO: Make Explanation a stack so that we can know that a field
-- access failed inside an App or Abs without overwriting the Explanation ?
data Reason
  = Reason Explanation FilePath Area
  | SimpleReason FilePath Area
  | AreaReason Area
  | NoReason
  deriving(Show, Eq)
