module AST.Solved where

import qualified Data.Map as M

import Infer.Type
import Explain.Location

data Solved a = Solved Type (Located a) deriving(Show, Eq)

getType :: SExp -> Type
getType (Solved t _) = t

extractExp :: SExp -> Exp
extractExp (Solved _ (Located _ e)) = e

data AST =
  AST
    { aimports   :: [Import]
    , aexps      :: [SExp]
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

data Case =
  Case
    { casepos :: Loc
    , casetype :: Maybe Type
    , casepattern :: Pattern
    , caseexp :: SExp
    }
    deriving(Eq, Show)

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

type Fields = M.Map Name SExp

type SExp = Solved Exp

data Exp = LInt String
         | LStr String
         | LBool String
         | JSExp String
         | App SExp SExp
         | FieldAccess SExp SExp
         | Abs Name SExp
         | Assignment Name SExp
         | Export SExp
         | Var Name
         | TypedExp SExp Typing
         | ListConstructor [SExp]
         | Record Fields
         | If SExp SExp SExp
         | Switch SExp [Case]
         deriving(Eq, Show)

type Name  = String


-- AST TABLE

type Table = M.Map FilePath AST
