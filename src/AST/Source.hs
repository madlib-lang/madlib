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
  = NamedImport [Name] FilePath FilePath
  | DefaultImport Name FilePath FilePath
  deriving(Eq, Show)

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

data Interface = Interface Constraints Name [Name] (M.Map Name Typing) deriving(Eq, Show)

data Instance = Instance Constraints Name [Typing] (M.Map Name Exp) deriving(Eq, Show)

data Constructor
  = Constructor Name [Typing]
  deriving(Eq, Show)


type Typing = Source Typing_

type Constraints = [Typing]

data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing)
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


data Field
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)


data ListItem
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
          | Abs Name [Exp]
          | FieldAccess Exp Exp
          | NamespaceAccess Name
          | Assignment Name Exp
          | Record [Field]
          | If Exp Exp Exp
          | Where Exp [Is]
          | Export Exp
          | TypedExp Exp Typing
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | Pipe [Exp]
          | JSExp String
          deriving(Eq, Show)

type Name = String


-- AST TABLE

type Table = M.Map FilePath AST

-- Functions

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Source _ _ (NamedImport   _ _ n) -> n
  Source _ _ (DefaultImport _ _ n) -> n

getImportPath :: Import -> (Import, FilePath)
getImportPath imp@(Source _ _ (NamedImport   _ p _)) = (imp, p)
getImportPath imp@(Source _ _ (DefaultImport _ p _)) = (imp, p)

getArea :: Source a -> Area
getArea (Source _ a _) = a
