{-# LANGUAGE NamedFieldPuns #-}
module AST.Solved where

import qualified Data.Map                      as M

import qualified Infer.Type                    as Ty
import           Explain.Location

data Solved a
  = Solved Ty.Type Area a
  | Untyped Area a
  deriving(Eq, Show, Ord)

data AST =
  AST
    { aimports    :: [Import]
    , aexps       :: [Exp]
    , atypedecls  :: [TypeDecl]
    , ainterfaces :: [Interface]
    , ainstances  :: [Instance]
    , apath       :: Maybe FilePath
    }
    deriving(Eq, Show, Ord)

type Import = Solved Import_
data Import_
  = NamedImport [Solved Name] FilePath FilePath
  | DefaultImport (Solved Name) FilePath FilePath
  deriving(Eq, Show, Ord)

type Interface = Solved Interface_
data Interface_ = Interface Name [Ty.Pred] [Ty.TVar] (M.Map Name Ty.Scheme) (M.Map Name Typing) deriving(Eq, Show, Ord)

type Instance = Solved Instance_
data Instance_ = Instance Name [Ty.Pred] Ty.Pred (M.Map Name (Exp, Ty.Scheme)) deriving(Eq, Show, Ord)

type TypeDecl = Solved TypeDecl_
data TypeDecl_
  = ADT
      { adtname :: Name
      , adtparams :: [Name]
      , adtconstructors :: [Constructor]
      , adtType :: Ty.Type
      , adtexported :: Bool
      }
  | Alias
      { aliasname :: Name
      , aliasparams :: [Name]
      , aliastype :: Typing
      , aliasexported :: Bool
      }
    deriving(Eq, Show, Ord)

type Constructor = Solved Constructor_
data Constructor_
  = Constructor Name [Typing] Ty.Type
  deriving(Eq, Show, Ord)

type Constraints = [Typing]

type Typing = Solved Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show, Ord)


type Is = Solved Is_
data Is_ = Is Pattern Exp deriving(Eq, Show, Ord)

type Pattern = Solved Pattern_
data Pattern_
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
  deriving(Eq, Show, Ord)

type Field = Solved Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show, Ord)

type ListItem = Solved ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show, Ord)


data ClassRefPred
  = CRPNode String [Ty.Type] Bool [ClassRefPred] -- Bool to control if it's a var or a concrete dictionary
  deriving(Eq, Show, Ord)

data PlaceholderRef
  = ClassRef String [ClassRefPred] Bool Bool -- first bool is call (Class...), second bool is var (class_var vs class.selector)
  | MethodRef String String Bool
  deriving(Eq, Show, Ord)

type Exp = Solved Exp_
data Exp_ = LNum String
          | LStr String
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | JSExp String
          | App Exp Exp Bool
          | Access Exp Exp
          | Abs (Solved Name) [Exp]
          | Assignment Name Exp
          | Export Exp
          | NameExport Name
          | TypeExport Name
          | Var Name
          | TypedExp Exp Ty.Scheme
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Placeholder (PlaceholderRef, [Ty.Type]) Exp
          deriving(Eq, Show, Ord)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST


-- Functions

getType :: Solved a -> Ty.Type
getType (Solved t _ _) = t

getArea :: Solved a -> Area
getArea (Solved _ a _) = a

extractExp :: Exp -> Exp_
extractExp (Solved _ (Area _ _) e) = e

getConstructorName :: Constructor -> String
getConstructorName (Untyped _ (Constructor name _ _)) = name

isADTExported :: TypeDecl -> Bool
isADTExported adt = case adt of
  Untyped _ ADT { adtexported } -> adtexported
  _                             -> False

isExportOnly :: Exp -> Bool
isExportOnly a = case a of
  (Solved _ _ (Export _)) -> True

  (Solved _ _ (TypedExp (Solved _ _ (Export _)) _)) -> True

  _ -> False


isNameExport :: Exp -> Bool
isNameExport a = case a of
  (Solved _ _ (NameExport _)) -> True

  _ -> False

isTypedExp :: Exp -> Bool
isTypedExp a = case a of
  (Solved _ _ (TypedExp _ _)) -> True
  _                           -> False

getNameExportName :: Exp -> Name
getNameExportName a = case a of
  (Solved _ _ (NameExport name)) -> name


isExport :: Exp -> Bool
isExport a = case a of
  (Solved _ _ (Export _)) -> True

  (Solved _ _ (TypedExp (Solved _ _ (Export _)) _)) -> True

  (Solved _ _ (NameExport _)) -> True

  _ -> False

getValue :: Solved a -> a
getValue (Solved _ _ a) = a
getValue (Untyped _ a ) = a

getExpName :: Exp -> Maybe String
getExpName (Solved _ _ exp) = case exp of
  Assignment name _ -> return name

  TypedExp (Solved _ _ (Assignment name _)) _ -> return name

  TypedExp (Solved _ _ (Export (Solved _ _ (Assignment name _)))) _ -> return name

  Export (Solved _ _ (Assignment name _)) -> return name

  _                 -> Nothing


getInstanceMethods :: Instance -> [Exp]
getInstanceMethods inst = case inst of
  Untyped _ (Instance _ _ _ methods) -> M.elems $ M.map fst methods
