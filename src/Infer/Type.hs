{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Infer.Type where

import qualified Data.Map                      as M


type Vars = M.Map String Scheme
type ADTs = M.Map String Type
type Typings = M.Map String Scheme
type Imports = M.Map String Type

data Env
  = Env
    { envvars :: Vars
    , envadts :: ADTs
    , envimports :: Imports
    , envcurrentpath :: FilePath
    }
    deriving(Eq, Show)


newtype TVar = TV String
  deriving (Show, Eq, Ord)


data Type
  = TVar TVar         -- Variable type
  | TCon TCon         -- Constant type
  | TArr Type Type    -- Arrow type
  | TComp String [Type] -- Composite type
  | TRecord (M.Map String Type) -- Record type
  deriving (Show, Eq, Ord)


data TCon
  = CString
  | CNum
  | CBool
  | CVoid
  | CUserDef String -- Is this one an Alias ? Or should it just go ?
  deriving (Show, Eq, Ord)

num :: Type
num = TCon CNum

bool :: Type
bool = TCon CBool

str :: Type
str = TCon CString

infixr `TArr`


data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)


type Substitution = M.Map TVar Type


arrowReturnType :: Type -> Type
arrowReturnType (TArr _ (TArr y x)) = arrowReturnType (TArr y x)
arrowReturnType (TArr _ x         ) = x
arrowReturnType x                   = x
