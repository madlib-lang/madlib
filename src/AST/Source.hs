module AST.Source where

import qualified Data.Map                      as M

import           Explain.Meta


data AST =
  AST
    { aimports   :: [Import]
    , aexps      :: [Exp]
    , aadts      :: [ADT]
    , apath      :: Maybe FilePath
    }
    deriving(Eq, Show)

type Import = Meta Import_

data Import_
  = NamedImport [Name] FilePath
  | DefaultImport Name FilePath
  deriving(Eq, Show)

-- TODO:
-- data ADT = ADT Name [Name] [ADTConstructor]
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


type Typing = Meta Typing_

data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing)
  deriving(Eq, Show)


type Is = Meta Is_
data Is_ = Is Pattern Exp deriving(Eq, Show)


type Pattern = Meta Pattern_
data Pattern_
  = PVar Name
  | PAny
  | PCtor Name [Pattern]
  | PNum String
  | PStr String
  | PBool String
  | PCon Name
  | PUserDef Name
  | PRecord (M.Map Name Pattern)
  | PList [Pattern]
  deriving(Eq, Show)


data Field
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)


data ListItem
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show)


type Exp = Meta Exp_

data Exp_ = LInt String
          | LStr String
          | LBool String
          | Var Name
          | App Exp Exp
          | Abs Name Exp
          | FieldAccess Exp Exp
          | Assignment Name Exp
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Export Exp
          | TypedExp Exp Typing
          | ListConstructor [ListItem]
          | JSExp String
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST
