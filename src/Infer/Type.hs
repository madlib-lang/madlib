module Infer.Type where

import qualified Data.Map                      as M

newtype TVar = TV String
  deriving (Show, Eq, Ord)


data Type
  = TVar TVar         -- Variable type
  | TCon TCon         -- Constant type
  | TArr Type Type    -- Arrow type
  | TComp String [Type] -- Composite type
  | TRecord (M.Map String Type) -- Record type
  | TAny
  deriving (Show, Eq, Ord)


data TCon
  = CString
  | CNum
  | CBool
  | CVoid
  | CUserDef String -- Is this one an Alias ? Or should it just go ?
  deriving (Show, Eq, Ord)

infixr `TArr`


data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)
