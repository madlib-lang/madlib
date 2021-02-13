{-# LANGUAGE NamedFieldPuns #-}
module AST.Solved where

import qualified Data.Map                      as M

import qualified Infer.Type                    as Ty
import           Explain.Location

data Solved a = Solved Ty.Type Area a deriving(Eq, Show)

data AST =
  AST
    { aimports    :: [Import]
    , aexps       :: [Exp]
    , atypedecls  :: [TypeDecl]
    , ainterfaces :: [Interface]
    , ainstances  :: [Instance]
    , apath       :: Maybe FilePath
    }
    deriving(Eq, Show)

data Import
  = NamedImport [Name] FilePath FilePath
  | DefaultImport Name FilePath FilePath
  deriving(Eq, Show)

-- data Interface = Interface Constraints Name [Name] (M.Map Name Typing) deriving(Eq, Show)

-- data Instance = Instance Constraints Name [Typing] (M.Map Name (Exp, Ty.Scheme)) deriving(Eq, Show)

data Interface = Interface Name [Ty.Pred] [Ty.TVar] (M.Map Name Ty.Scheme) deriving(Eq, Show)

data Instance = Instance Name [Ty.Pred] Ty.Pred (M.Map Name (Exp, Ty.Scheme)) deriving(Eq, Show)

data TypeDecl
  = ADT
      { adtname :: Name
      , adtparams :: [Name]
      , adtconstructors :: [Constructor]
      , adtexported :: Bool
      }
  | Alias
      { aliasname :: Name
      , aliasparams :: [Name]
      , aliastype :: Typing
      , aliasexported :: Bool
      }
    deriving(Eq, Show)

data Constructor
  = Constructor Name [Typing]
  deriving(Eq, Show)

type Constraints = [Typing]

data Typing
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
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
  | PRecord (M.Map Name Pattern)
  | PList [Pattern]
  | PTuple [Pattern]
  | PSpread Pattern
  deriving(Eq, Show)

data Field
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)

data ListItem
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show)

type Exp = Solved Exp_


data ClassRefPred
  = CRPNode String [Ty.Type] Bool [ClassRefPred] -- Bool to control if it's a var or a concrete dictionary
  deriving(Eq, Show)

data PlaceholderRef
  = ClassRef String [ClassRefPred] Bool Bool -- first bool is call (Class...), second bool is var (class_var vs class.selector)
  | MethodRef String String Bool
  deriving(Eq, Show)

data Exp_ = LNum String
          | LStr String
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | JSExp String
          | App Exp Exp Bool
          | FieldAccess Exp Exp
          | NamespaceAccess Name
          | Abs Name [Exp]
          | Assignment Name Exp
          | Export Exp
          | Var Name
          | TypedExp Exp Ty.Scheme
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Placeholder (PlaceholderRef, [Ty.Type]) Exp
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST


-- Functions

getType :: Solved a -> Ty.Type
getType (Solved t _ _) = t

extractExp :: Exp -> Exp_
extractExp (Solved _ (Area _ _) e) = e

getConstructorName :: Constructor -> String
getConstructorName (Constructor name _) = name

isADTExported :: TypeDecl -> Bool
isADTExported adt = case adt of
  ADT { adtexported } -> adtexported
  _                   -> False

isExport :: Exp -> Bool
isExport a = case a of
  (Solved _ _ (Export _)) -> True

  (Solved _ _ (TypedExp (Solved _ _ (Export _)) _)) -> True

  _ -> False

