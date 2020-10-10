module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar         -- Variable type
  | TCon TCon         -- Constant type
  | TArr Type Type    -- Arrow type
  -- TODO: Maybe move back to TComp TCon [TVar] and construct the Type from TVar
  -- in needed places
  -- TODO: Rename TADT ?
  | TComp TCon [Type] -- Composite type
  deriving (Show, Eq, Ord)

data TCon
  = CString
  | CNum
  | CBool
  | CUserDef String
  deriving (Show, Eq, Ord)

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)
