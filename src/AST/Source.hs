module AST.Source where


import           Explain.Meta
import           Explain.Location
import qualified Data.Map                      as M

data Source a = Source (Infos a) Area a deriving(Eq, Show)

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

type Import = Source Import_
-- The second FilePath parameter is the absolute path to that module
data Import_
  = NamedImport [Source Name] FilePath FilePath
  | TypeImport [Source Name] FilePath FilePath
  | DefaultImport (Source Name) FilePath FilePath
  deriving(Eq, Show)

type TypeDecl = Source TypeDecl_
data TypeDecl_
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


type Interface = Source Interface_
data Interface_ = Interface Constraints Name [Name] (M.Map Name Typing) deriving(Eq, Show)

type Instance = Source Instance_
data Instance_ = Instance Constraints Name [Typing] (M.Map Name Exp) deriving(Eq, Show)

type Constructor = Source Constructor_
data Constructor_
  = Constructor Name [Typing]
  deriving(Eq, Show)


type Typing = Source Typing_

type Constraints = [Typing]

data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing) -- Maybe typing for the possible extension
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show)


type Is = Source Is_
data Is_ = Is Pattern Exp deriving(Eq, Show)


type Pattern = Source Pattern_
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

type Field = Source Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)


type ListItem = Source ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show)


type Exp = Source Exp_
data Exp_ = LNum String
          | LStr String
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | Var Name
          | App Exp Exp Bool
          | Abs (Source Name) [Exp]
          | Access Exp Exp
          | Assignment Name Exp
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Export Exp
          | NameExport Name
          | TypeExport Name
          | TypedExp Exp Typing
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | Pipe [Exp]
          | JSExp String
          | JsxTag Name [JsxProp] [Exp]
          deriving(Eq, Show)


type JsxProp = Source JsxProp_
data JsxProp_ = JsxProp Name Exp deriving(Eq, Show)


type Name = String


-- AST TABLE

type Table = M.Map FilePath AST

-- Functions

getImportNames :: Import -> [Source Name]
getImportNames imp = case imp of
  Source _ _ (NamedImport names _ n) -> names
  Source _ _ DefaultImport{}         -> []
  Source _ _ TypeImport{}            -> []

getImportTypeNames :: Import -> [Source Name]
getImportTypeNames imp = case imp of
  Source _ _ (NamedImport names _ _) -> []
  Source _ _ (TypeImport names _ _)  -> names
  Source _ _ DefaultImport{}         -> []

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Source _ _ (NamedImport   _ _ n) -> n
  Source _ _ (TypeImport   _ _ n)  -> n
  Source _ _ (DefaultImport _ _ n) -> n

getImportPath :: Import -> (Import, FilePath)
getImportPath imp@(Source _ _ (NamedImport   _ p _)) = (imp, p)
getImportPath imp@(Source _ _ (TypeImport   _ p _))  = (imp, p)
getImportPath imp@(Source _ _ (DefaultImport _ p _)) = (imp, p)

getArea :: Source a -> Area
getArea (Source _ a _) = a

getSourceContent :: Source a -> a
getSourceContent (Source _ _ a) = a
