{-# LANGUAGE NamedFieldPuns #-}
module AST.Canonical where


import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Map                      as M


data Canonical a = Canonical Area a deriving(Eq, Show, Ord)

-- data NameRef
--   = Local String
--   | Foreign FilePath String
--   | UnQualified String


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

type Interface = Canonical Interface_
data Interface_ = Interface Name [Ty.Pred] [Ty.TVar] (M.Map Name Ty.Scheme) (M.Map Name Typing) deriving(Eq, Show, Ord)

type Instance = Canonical Instance_
data Instance_ = Instance Name [Ty.Pred] Ty.Pred (M.Map Name Exp) deriving(Eq, Show, Ord)

type Import = Canonical Import_
-- The second FilePath parameter is the absolute path to that module
data Import_
  = NamedImport [Canonical Name] FilePath FilePath
  | TypeImport [Canonical Name] FilePath FilePath
  | DefaultImport (Canonical Name) FilePath FilePath
  | ImportAll FilePath FilePath
  deriving(Eq, Show, Ord)

type Constructor = Canonical Constructor_
data Constructor_ = Constructor Name [Typing] Ty.Scheme Ty.Type deriving(Eq, Show, Ord)

getCtorScheme :: Constructor -> Ty.Scheme
getCtorScheme (Canonical _ (Constructor _ _ sc _)) = sc

getCtorName :: Constructor -> Name
getCtorName (Canonical _ (Constructor name _ _ _)) = name

getCtorType :: Constructor -> Ty.Type
getCtorType (Canonical _ (Constructor _ _ _ t)) = t

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
    deriving(Eq, Show, Ord)



type Constraints = [Typing]

type Typing = Canonical Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show, Ord)


type Is = Canonical Is_
data Is_ = Is Pattern Exp deriving(Eq, Show, Ord)


type Pattern = Canonical Pattern_
data Pattern_
  = PVar Name
  | PAny
  | PCon Name [Pattern]
  | PNum String
  | PStr String
  | PChar Char
  | PBool String
  | PRecord (M.Map Name Pattern)
  | PList [Pattern]
  | PTuple [Pattern]
  | PSpread Pattern
  deriving(Eq, Show, Ord)

type Field = Canonical Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show, Ord)


type ListItem = Canonical ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show, Ord)


type Exp = Canonical Exp_
data Exp_ = LNum String
          | LFloat String
          | LStr String
          | LChar Char
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
          | Do [Exp]
          | Where Exp [Is]
          | Export Exp
          | NameExport Name
          | TypeExport Name
          | TypedExp Exp Typing Ty.Scheme
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | JSExp String
          | Extern Ty.Scheme Name Name
          deriving(Eq, Show, Ord)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST


-- Functions

getImportNames :: Import -> [Canonical Name]
getImportNames imp = case imp of
  Canonical _ (NamedImport names _ _) ->
    names

  _ ->
    []


getImportAlias :: Import -> Maybe (Canonical Name)
getImportAlias imp = case imp of
  Canonical _ (DefaultImport alias _ _) ->
    Just alias

  _->
    Nothing


getImportTypeNames :: Import -> [Canonical Name]
getImportTypeNames imp = case imp of
  Canonical _ (TypeImport  names _ _) ->
    names

  _ ->
    []

getImportNamespace :: Import -> Maybe String
getImportNamespace imp = case imp of
  Canonical _ (DefaultImport (Canonical _ name) _ _) ->
    Just name

  _ ->
    Nothing


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
  Canonical _ (NameExport name) ->
    return name

  Canonical _ (Export (Canonical _ (Assignment name _))) ->
    return name

  Canonical _ (TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) _ _) ->
    return name

  Canonical _ (Export (Canonical _ (Extern _ name _))) ->
    return name

  _ ->
    Nothing

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Canonical _ (NamedImport   _ _ n) -> n
  Canonical _ (TypeImport    _ _ n) -> n
  Canonical _ (DefaultImport _ _ n) -> n
  Canonical _ (ImportAll _ n)       -> n

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
  Assignment name _ ->
    return name

  TypedExp (Canonical _ (Assignment name _)) _ _ ->
    return name

  TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) _ _ ->
    return name

  Export (Canonical _ (Assignment name _)) ->
    return name

  _                 ->
    Nothing


getExportNameAndScheme :: Exp -> (Maybe String, Maybe Ty.Scheme)
getExportNameAndScheme (Canonical _ exp) = case exp of
  TypedExp (Canonical _ (Export (Canonical _ (Assignment name _)))) _ sc ->
    (Just name, Just sc)

  Export (Canonical _ (Assignment name _)) ->
    (Just name, Nothing)

  _ ->
    (Nothing, Nothing)



getTypedExpExp :: Exp -> Exp
getTypedExpExp exp = case exp of
  Canonical _ (Export (Canonical _ (TypedExp e _ _))) ->
    e

  Canonical _ (TypedExp e _ _) ->
    e



getFieldName :: Field -> Maybe String
getFieldName field = case field of
  Canonical _ (Field (name, _)) ->
    Just name

  _ ->
    Nothing


isSpread :: Field -> Bool
isSpread field = case field of
  Canonical _ (FieldSpread _) ->
    True

  _ ->
    False

hasSpread :: [Field] -> Bool
hasSpread =
  any isSpread


isNamedAbs :: Exp -> Bool
isNamedAbs exp = case exp of
  Canonical _ (Assignment _ (Canonical _ (Abs _ _))) ->
    True

  Canonical _ (Export (Canonical _ (Assignment _ (Canonical _ (Abs _ _))))) ->
    True

  _ ->
    False
