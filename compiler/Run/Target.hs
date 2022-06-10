module Run.Target where

data Target
  = TNode
  | TBrowser
  | TLLVM
  | TAny
  deriving(Eq, Show)
