module AST.Source where

import qualified Data.Map                      as M

import           Explain.Location
import           Explain.Meta


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

data Typing
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing)
  deriving(Eq, Show)

-- data Case =
--   Case
--     { casepos :: Area
--     , casepattern :: Pattern
--     , caseexp :: Exp
--     }
--     deriving(Eq, Show)
-- TODO:

type Case = Meta Case_
data Case_ = Case Pattern Exp deriving(Eq, Show)

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

type Fields = M.Map Name Exp

type Exp = Meta Exp_

data Exp_ = LInt String
          | LStr String
          | LBool String
          | Var Name
          | App Exp Exp
          | Abs Name Exp
          | FieldAccess Exp Exp
          | Assignment Name Exp
          | Record Fields
          | If Exp Exp Exp
          | Switch Exp [Case]
          | Export Exp
          | TypedExp Exp Typing
          | ListConstructor [Exp]
          | JSExp String
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST
