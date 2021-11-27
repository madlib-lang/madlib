module Generate.LLVM.Optimized where


import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Map                      as M


data Optimized a
  = Optimized Ty.Type Area a
  | Untyped Area a
  deriving(Eq, Show)

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

type Import = Optimized Import_
data Import_
  = NamedImport [Optimized Name] FilePath FilePath
  | DefaultImport (Optimized Name) FilePath FilePath
  deriving(Eq, Show)

type Interface = Optimized Interface_
data Interface_ = Interface Name [Ty.Pred] [String] (M.Map Name Ty.Scheme) (M.Map Name Typing) deriving(Eq, Show)

type Instance = Optimized Instance_
data Instance_ = Instance Name [Ty.Pred] String (M.Map Name (Exp, Ty.Scheme)) deriving(Eq, Show)

type TypeDecl = Optimized TypeDecl_
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

type Constructor = Optimized Constructor_
data Constructor_
  = Constructor Name [Typing] Ty.Type
  deriving(Eq, Show)

type Constraints = [Typing]

type Typing = Optimized Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show)


type Is = Optimized Is_
data Is_ = Is Pattern Exp deriving(Eq, Show)

type Pattern = Optimized Pattern_
data Pattern_
  = PVar Name
  | PAny
  | PCon Name [Pattern]
  | PNum String
  | PStr String
  | PBool String
  | PRecord (M.Map Name Pattern)
  | PList [Pattern]
  | PTuple [Pattern]
  | PSpread Pattern
  deriving(Eq, Show)

type Field = Optimized Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)

type ListItem = Optimized ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show)


data ClassRefPred
  = CRPNode String String Bool [ClassRefPred] -- Bool to control if it's a var or a concrete dictionary
  deriving(Eq, Show)

data PlaceholderRef
  = ClassRef String [ClassRefPred] Bool Bool -- first bool is call (Class...), second bool is var (class_var vs class.selector)
  | MethodRef String String Bool
  deriving(Eq, Show)

type Exp = Optimized Exp_
data Exp_ = LNum String
          | LStr String
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | JSExp String
          | App Exp Exp Bool
          | Access Exp Exp
          | Abs Name [Exp]
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
          | Do [Exp]
          | Where Exp [Is]
          | Placeholder (PlaceholderRef, String) Exp
          | Closure Name [Exp]
          | ClosureDef Name [Exp] Name [Exp]
          -- ^ Closure name | env ( Only Var exps ) | param | body
          deriving(Eq, Show)

type Name = String


-- AST TABLE
type Table = M.Map FilePath AST

-- Functions

getStartLine :: Exp -> Int
getStartLine (Optimized _ (Area (Loc _ line _) _) _) = line
getStartLine (Untyped (Area (Loc _ line _) _) _    ) = line

getValue :: Optimized a -> a
getValue (Optimized _ _ a) = a
getValue (Untyped _ a    ) = a


getType :: Optimized a -> Ty.Type
getType (Optimized t _ _) = t


isTopLevelFunction :: Exp -> Bool
isTopLevelFunction exp = case exp of
  Optimized _ _ (Assignment _ (Optimized _ _ Abs{})) ->
    True

  Optimized _ _ (TypedExp (Optimized _ _ (Assignment _ (Optimized _ _ Abs{}))) _) ->
    True

  Optimized _ _ (Export (Optimized _ _ (Assignment _ (Optimized _ _ Abs{})))) ->
    True

  Optimized _ _ (TypedExp (Optimized _ _ (Export (Optimized _ _ (Assignment _ (Optimized _ _ Abs{}))))) _) ->
    True

  _ ->
    False

isClosureDef :: Exp -> Bool
isClosureDef exp = case exp of
  Optimized _ _ ClosureDef{} ->
    True

  _ ->
    False

isADT :: TypeDecl -> Bool
isADT td = case td of
  Untyped _ ADT {} -> True
  _                -> False
