{-# LANGUAGE TupleSections #-}
module AST.Source where

import qualified Data.Map as M

import qualified Infer.Type as T
import           Explain.Location


data AST =
  AST
    { aimports   :: [Import]
    , aexps      :: [LExp]
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

data Case =
  Case
    { casepos :: Loc
    , casetype :: Maybe T.Type
    , casepattern :: Pattern
    , caseexp :: LExp
    }
    deriving(Eq, Show)
-- TODO:
-- data Case = Case Pattern LExp

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

type Fields = M.Map Name LExp

type LExp = Located Exp

data Exp = LInt String
         | LStr String
         | LBool String
         | Var Name
         | App LExp LExp
         | Abs Name LExp
         | FieldAccess LExp LExp
         | Assignment Name LExp
         | Record Fields
         | If LExp LExp LExp
         | Switch LExp [Case]
         | Export LExp
         | TypedExp LExp Typing
         | ListConstructor [LExp]
         | JSExp String
         deriving(Eq, Show)

type Name  = String

getLoc :: LExp -> Loc
getLoc (Located l _) = l



-- AST TABLE

type Table = M.Map FilePath AST
