module AST.Solved where

import qualified Data.Map                      as M

import           Infer.Type
import           Explain.Location

data Solved a = Solved Type Area a deriving(Eq, Show)

getType :: Exp -> Type
getType (Solved t _ _) = t

extractExp :: Exp -> Exp_
extractExp (Solved _ (Area _ _) e) = e

data AST =
  AST
    { aimports   :: [Import]
    , aexps      :: [Exp]
    , aadts      :: [ADT]
    , apath      :: Maybe FilePath
    }
    deriving(Eq, Show)

data Import
  = NamedImport [Name] FilePath
  | DefaultImport Name FilePath
  deriving(Eq, Show)

data ADT =
  ADT
    { adtname :: Name
    , adtparams :: [Name]
    , adtconstructors :: [ADTConstructor]
    }
    deriving(Eq, Show)

data ADTConstructor
  = ADTConstructor       { adtcname :: Name, adtcargs :: Maybe [Typing] }
  deriving(Eq, Show)

data Typing
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing)
  deriving(Eq, Show)


type Is = Solved Is_
data Is_ = Is Pattern Exp deriving(Eq, Show)

data Pattern
  = PVar Name
  | PAny
  | PCtor Name [Pattern]
  | PNum String
  | PStr String
  | PBool String
  | PCon Name
  | PUserDef Name
  | PRecord (M.Map Name Pattern)
  deriving(Eq, Show)

data Field
  = Field (Name, Exp)
  | Spread Exp
  deriving(Eq, Show)

type Exp = Solved Exp_

data Exp_ = LInt String
          | LStr String
          | LBool String
          | JSExp String
          | App Exp Exp
          | FieldAccess Exp Exp
          | Abs Name Exp
          | Assignment Name Exp
          | Export Exp
          | Var Name
          | TypedExp Exp Typing
          | ListConstructor [Exp]
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST
