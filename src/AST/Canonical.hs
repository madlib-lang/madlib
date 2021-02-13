{-# LANGUAGE NamedFieldPuns #-}
module AST.Canonical where


import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Map                      as M


data Canonical a = Canonical Area a deriving(Eq, Show)


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

data Interface = Interface Name [Ty.Pred] [Ty.TVar] (M.Map Name Ty.Scheme) deriving(Eq, Show)

data Instance = Instance Name [Ty.Pred] Ty.Pred (M.Map Name Exp) deriving(Eq, Show)

type Import = Canonical Import_

-- The second FilePath parameter is the absolute path to that module
data Import_
  = NamedImport [Name] FilePath FilePath
  | DefaultImport Name FilePath FilePath
  deriving(Eq, Show)


data Constructor = Constructor Name [Typing] Ty.Scheme deriving(Eq, Show)

getCtorScheme :: Constructor -> Ty.Scheme
getCtorScheme (Constructor _ _ sc) = sc

getCtorName :: Constructor -> Name
getCtorName (Constructor name _ _) = name

data TypeDecl
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
    deriving(Eq, Show)


type Typing = Canonical Typing_

type Constraints = [Typing]

data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show)


type Is = Canonical Is_
data Is_ = Is Pattern Exp deriving(Eq, Show)


type Pattern = Canonical Pattern_
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
  deriving(Eq, Show)


data Field
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)


data ListItem
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show)


type Exp = Canonical Exp_

data Exp_ = LNum String
          | LStr String
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | Var Name
          | App Exp Exp Bool
          | Abs Name [Exp]
          | FieldAccess Exp Exp
          | NamespaceAccess Name
          | Assignment Name Exp
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Export Exp
          | TypedExp Exp Ty.Scheme
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | JSExp String
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST


-- Functions

isTypeDeclExported :: TypeDecl -> Bool
isTypeDeclExported td = case td of
  ADT { adtexported }     -> adtexported
  Alias { aliasexported } -> aliasexported

getTypeDeclName :: TypeDecl -> String
getTypeDeclName td = case td of
  ADT { adtname }     -> adtname
  Alias { aliasname } -> aliasname

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Canonical _ (NamedImport   _ _ n) -> n
  Canonical _ (DefaultImport _ _ n) -> n

isAssignment :: Exp -> Bool
isAssignment exp = case exp of
  Canonical _ (Assignment _ _) -> True
  _                            -> False

getArea :: Canonical a -> Area
getArea (Canonical a _) = a

getExpName :: Exp -> Maybe String
getExpName (Canonical _ exp) = case exp of
  Assignment name                              _ -> return name

  TypedExp   (Canonical _ (Assignment name _)) _ -> return name

  TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) _ ->
    return name

  Export (Canonical _ (Assignment name _)) -> return name

  _ -> Nothing
