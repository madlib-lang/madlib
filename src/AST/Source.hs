module AST.Source where

import           Explain.Location
import qualified Data.Map                      as M


data Source a = Source Area a deriving(Eq, Show)

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
  | ImportAll FilePath FilePath
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



type Constraints = [Typing]

type Typing = Source Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing) -- Maybe typing for the possible extension
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show)


type Is = Source Is_
data Is_ = Is Pattern Symbol Exp deriving(Eq, Show)


data PatternField
  = PatternField (Source Name) Pattern
  | PatternFieldShorthand (Source Name)
  deriving(Eq, Show)


type Pattern = Source Pattern_
data Pattern_
  = PVar Name
  | PNum String
  | PStr String
  | PBool String
  | PAny
  | PCon (Source Name) Symbol [Pattern] Symbol
  | PNullaryCon (Source Name)
  | PRecord Symbol [PatternField] Symbol
  | PList Symbol [Pattern] Symbol
  | PTuple Symbol [Pattern] Symbol
  | PSpread Symbol Pattern
  deriving(Eq, Show)

type Field = Source Field_
data Field_
  = Field (Name, Exp)
  | FieldShorthand Name
  | FieldSpread Exp
  deriving(Eq, Show)


type ListItem = Source ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show)

type DictItem = Source DictItem_
data DictItem_ = DictItem Exp Exp deriving(Eq, Show)

data Keyword
  = Keyword String Area
  deriving(Eq, Show)

data Symbol
  = Symbol String Area
  deriving(Eq, Show)

type Exp = Source Exp_
data Exp_
  = LNum String
  | LStr String
  | LBool String
  | LUnit
  | TemplateString [Exp]
  | Var Name
  | UnOp Exp Exp
  | BinOp Exp Exp Exp
  | App Exp [Exp]
  | Abs [Source Name] [Exp]
  | AbsWithMultilineBody [Source Name] [Exp]
  | Return Exp
  | Access Exp Exp
  | Assignment Name Symbol Exp
  | Record [Field]
  | If Keyword Exp Exp Keyword Exp
  | Ternary Exp Symbol Exp Symbol Exp
  | Where Keyword Exp Symbol [Is] Symbol
  | WhereAbs Keyword Symbol [Is] Symbol
  | Do [Exp]
  | DoAssignment Name Exp
  | Export Exp
  | NameExport Name
  | TypeExport Name
  | TypedExp Exp Typing
  | NamedTypedExp Name Exp Typing
  | ListConstructor [ListItem]
  | Dictionary [DictItem]
  | TupleConstructor [Exp]
  | Pipe [Exp]
  | JSExp String
  | JsxTag Name [JsxProp] [JsxChild]
  | JsxAutoClosedTag Name [JsxProp]
  | Parenthesized Area Exp Area
  deriving(Eq, Show)


data JsxChild
  = JsxChild Exp
  | JsxExpChild Exp
  | JsxSpreadChild Exp
  deriving(Eq, Show)

type JsxProp = Source JsxProp_
data JsxProp_ = JsxProp Name Exp deriving(Eq, Show)


type Name = String


-- AST TABLE

type Table = M.Map FilePath AST

-- Functions

getImportNames :: Import -> [Source Name]
getImportNames imp = case imp of
  Source _ (NamedImport names _ n) -> names
  Source _ DefaultImport{}         -> []
  Source _ TypeImport{}            -> []
  Source _ ImportAll{}             -> []

getImportTypeNames :: Import -> [Source Name]
getImportTypeNames imp = case imp of
  Source _ (NamedImport names _ _) -> []
  Source _ (TypeImport  names _ _) -> names
  Source _ DefaultImport{}         -> []
  Source _ ImportAll{}             -> []

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Source _ (NamedImport   _ _ n) -> n
  Source _ (TypeImport    _ _ n) -> n
  Source _ (DefaultImport _ _ n) -> n
  Source _ (ImportAll _ n) -> n

getImportPath :: Import -> (Import, FilePath)
getImportPath imp@(Source _ (NamedImport   _ p _)) = (imp, p)
getImportPath imp@(Source _ (TypeImport    _ p _)) = (imp, p)
getImportPath imp@(Source _ (DefaultImport _ p _)) = (imp, p)
getImportPath imp@(Source _ (ImportAll p _)) = (imp, p)

getArea :: Source a -> Area
getArea (Source a _) = a

getSourceContent :: Source a -> a
getSourceContent (Source _ a) = a
