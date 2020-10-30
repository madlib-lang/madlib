module AST.AST where

import qualified Data.Map as M

import           Infer.Type
import           Explain.Context

data AST =
  AST
    { aimports   :: [Import]
    , aexps      :: [Exp]
    , aadts       :: [ADT]
    , apath      :: Maybe FilePath
    }
    deriving(Eq, Show)

data Import 
  = NamedImport   { ipos :: Loc, inames :: [Name], ipath :: FilePath }
  | DefaultImport { ipos :: Loc, ialias :: Name, ipath :: FilePath }
  deriving(Eq, Show)

-- TODO: Add Loc
data ADT =
  ADT
    { adtname :: Name
    , adtparams :: [Name]
    , adtconstructors :: [ADTConstructor]
    }
    deriving(Eq, Show)

-- TODO: Add Loc
data ADTConstructor
  = ADTConstructor       { adtcname :: Name, adtcargs :: Maybe [TypeRef] }
  deriving(Eq, Show)

-- TODO: Rename
data TypeRef
  = TRSingle Name
  | TRComp Name [TypeRef]
  | TRArr TypeRef TypeRef
  | TRRecord (M.Map Name TypeRef)
  deriving(Eq, Show)

data Case =
  Case
    { casepos :: Loc
    , casetype :: Maybe Type
    , casepattern :: Pattern
    , caseexp :: Exp
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

data Exp = LInt            { epos :: Loc, etype :: Maybe Type, eval :: String }
         | LStr            { epos :: Loc, etype :: Maybe Type, eval :: String }
         | LBool           { epos :: Loc, etype :: Maybe Type, eval :: String }
         | JSExp           { epos :: Loc, etype :: Maybe Type, econtent :: String }
         | App             { epos :: Loc, etype :: Maybe Type, eabs :: Exp, earg :: Exp, efieldAccess :: Bool }
         | Abs             { epos :: Loc, etype :: Maybe Type, eparam :: Name, ebody :: Exp }
         | Assignment      { epos :: Loc, etype :: Maybe Type, eexp :: Exp, ename :: Name, eexported :: Bool }
         | Var             { epos :: Loc, etype :: Maybe Type, ename :: Name }
         | TypedExp        { epos :: Loc, etype :: Maybe Type, eexp :: Exp, etyping :: TypeRef }
         | ListConstructor { epos :: Loc, etype :: Maybe Type, eelems :: [Exp] }
         | Record          { epos :: Loc, etype :: Maybe Type, erfields :: M.Map Name Exp }
         | Switch          { epos :: Loc, etype :: Maybe Type, ecases :: [Case], eexp :: Exp }
         deriving(Eq, Show)

type Name  = String
newtype Typing = Typing String deriving(Eq, Show)