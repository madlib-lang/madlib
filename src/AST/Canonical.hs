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

type Interface = Canonical Interface_
data Interface_ = Interface Name [Ty.Pred] [Ty.TVar] (M.Map Name Ty.Scheme) (M.Map Name Typing) deriving(Eq, Show)

type Instance = Canonical Instance_
data Instance_ = Instance Name [Ty.Pred] Ty.Pred (M.Map Name Exp) deriving(Eq, Show)

type Import = Canonical Import_
-- The second FilePath parameter is the absolute path to that module
data Import_
  = NamedImport [Canonical Name] FilePath FilePath
  | TypeImport [Canonical Name] FilePath FilePath
  | DefaultImport (Canonical Name) FilePath FilePath
  deriving(Eq, Show)

type Constructor = Canonical Constructor_
data Constructor_ = Constructor Name [Typing] Ty.Scheme deriving(Eq, Show)

getCtorScheme :: Constructor -> Ty.Scheme
getCtorScheme (Canonical _ (Constructor _ _ sc)) = sc

getCtorName :: Constructor -> Name
getCtorName (Canonical _ (Constructor name _ _)) = name

getCtors :: TypeDecl -> [Constructor]
getCtors (Canonical _ td) = case td of
  Alias{} -> []
  ADT{}   -> adtconstructors td

type TypeDecl = Canonical TypeDecl_
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
    deriving(Eq, Show)



type Constraints = [Typing]

type Typing = Canonical Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
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

type Field = Canonical Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)


type ListItem = Canonical ListItem_
data ListItem_
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
          | Abs (Canonical Name) [Exp]
          | Access Exp Exp
          | Assignment Name Exp
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Export Exp
          | NameExport Name
          | TypeExport Name
          | TypedExp Exp Ty.Scheme
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | JSExp String
          | JSXExpChild Exp
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST


-- Functions

getImportNames :: Import -> [Canonical Name]
getImportNames imp = case imp of
  Canonical _ (NamedImport names _ _) -> names
  Canonical _ TypeImport{}            -> []
  Canonical _ DefaultImport{}         -> []

getImportAlias :: Import -> Maybe (Canonical Name)
getImportAlias imp = case imp of
  Canonical _ NamedImport{}             -> Nothing
  Canonical _ TypeImport{}              -> Nothing
  Canonical _ (DefaultImport alias _ _) -> Just alias

getImportTypeNames :: Import -> [Canonical Name]
getImportTypeNames imp = case imp of
  Canonical _ (NamedImport names _ _) -> []
  Canonical _ (TypeImport  names _ _) -> names
  Canonical _ DefaultImport{}         -> []

isTypeImport :: Import -> Bool
isTypeImport imp = case imp of
  Canonical _ TypeImport{} -> True
  _                        -> False

isTypeDeclExported :: TypeDecl -> Bool
isTypeDeclExported td = case td of
  Canonical _ ADT { adtexported }     -> adtexported
  Canonical _ Alias { aliasexported } -> aliasexported

getTypeDeclName :: TypeDecl -> String
getTypeDeclName td = case td of
  Canonical _ ADT { adtname }     -> adtname
  Canonical _ Alias { aliasname } -> aliasname

isTypeExport :: Exp -> Bool
isTypeExport exp = case exp of
  Canonical _ (TypeExport _) -> True
  _                          -> False

getTypeExportName :: Exp -> String
getTypeExportName exp = case exp of
  Canonical _ (TypeExport name) -> name

getExportName :: Exp -> Maybe Name
getExportName exp = case exp of
  Canonical _ (NameExport name) -> return name

  Canonical _ (Export (Canonical _ (Assignment name _))) -> return name

  Canonical _ (TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) _) -> return name

  _ -> Nothing

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Canonical _ (NamedImport   _ _ n) -> n
  Canonical _ (TypeImport    _ _ n) -> n
  Canonical _ (DefaultImport _ _ n) -> n

isAssignment :: Exp -> Bool
isAssignment exp = case exp of
  Canonical _ (Export (Canonical _ (Assignment _ _))) -> True
  Canonical _ (Assignment _ _) -> True
  _ -> False

getArea :: Canonical a -> Area
getArea (Canonical a _) = a

getCanonicalContent :: Canonical a -> a
getCanonicalContent (Canonical _ a) = a

getExpName :: Exp -> Maybe String
getExpName (Canonical _ exp) = case exp of
  Assignment name _ -> return name

  TypedExp (Canonical _ (Assignment name _)) _ -> return name

  TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) _ -> return name

  Export (Canonical _ (Assignment name _)) -> return name

  _                 -> Nothing


getExportNameAndScheme :: Exp -> (Maybe String, Maybe Ty.Scheme)
getExportNameAndScheme (Canonical _ exp) = case exp of
  TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) sc -> (Just name, Just sc)

  Export (Canonical _ (Assignment name _)) -> (Just name, Nothing)

  _ -> (Nothing, Nothing)
