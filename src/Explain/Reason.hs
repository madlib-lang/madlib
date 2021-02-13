module Explain.Reason where

import           Explain.Location
import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Infer.Type


data Explanation
  -- WrongTypeApplied fn :: Can.Exp arg :: Can.Exp
  = WrongTypeApplied Can.Exp Can.Exp
  | VariableNotDeclared Can.Exp
  -- IfElseBranchTypesDontMatch ifExp :: Can.Exp falsyExp :: Can.Exp
  | IfElseBranchTypesDontMatch Can.Exp Can.Exp
  -- IfElseCondIsNotBool ifExp :: Can.Exp condExp :: Can.Exp
  | IfElseCondIsNotBool Can.Exp Can.Exp
  -- PatternTypeError switch :: Can.Exp pattern :: Can.Pattern
  | PatternTypeError Can.Exp Can.Pattern
  -- PatternTypeError switch :: Can.Exp pattern :: Can.Pattern
  | PatternConstructorDoesNotExist Can.Exp Can.Pattern
  | WrongImport Src.Import
  -- For now it just holds the Exp, not the TypedExp
  -- More info here: https://github.com/open-sorcerers/madlib/issues/8
  | TypeAndTypingMismatch Can.Exp Can.Typing Type Type
  deriving(Show, Eq)

-- TODO: I think we should get rid of that Area in the Reason
-- in favor of more meaningful Can.Exp in Explanations.
-- TODO: Make Explanation a stack so that we can know that a field
-- access failed inside an App or Abs without overwriting the Explanation ?
data Reason
  = Reason Explanation FilePath Area
  | SimpleReason FilePath Area
  | AreaReason Area
  | NoReason
  deriving(Show, Eq)
