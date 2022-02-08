module AST.Core where


import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Map                      as M


data Core a
  = Typed (Ty.Qual Ty.Type) Area a
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
    deriving(Eq, Show)

type Import = Core Import_
data Import_
  = NamedImport [Core Name] FilePath FilePath
  | DefaultImport (Core Name) FilePath FilePath
  deriving(Eq, Show)

type Interface = Core Interface_
data Interface_ = Interface Name [Ty.Pred] [String] (M.Map Name Ty.Scheme) (M.Map Name Typing) deriving(Eq, Show)

type Instance = Core Instance_
data Instance_ = Instance Name [Ty.Pred] String (M.Map Name (Exp, Ty.Scheme)) deriving(Eq, Show)

type TypeDecl = Core TypeDecl_
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

type Constructor = Core Constructor_
data Constructor_
  = Constructor Name [Typing] Ty.Type
  deriving(Eq, Show)

type Constraints = [Typing]

type Typing = Core Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show)


type Is = Core Is_
data Is_ = Is Pattern Exp deriving(Eq, Show)

type Pattern = Core Pattern_
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

type Field = Core Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show)

type ListItem = Core ListItem_
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


data DefinitionType
  = BasicDefinition
  | TCEOptimizableDefinition
  deriving(Eq, Show)

data CallType
  = SimpleCall
  | RecursiveTailCall
  deriving(Eq, Show)


type Exp = Core Exp_
data Exp_
  -- TODO: put literals under a single Literal constructor with a LiteralType param to distinguish
  = LNum String
  | LFloat String
  | LStr String
  | LBool String
  | LUnit
  | JSExp String
  -- TODO: figure something out for Definition name. Maybe Definition DefinitionName DefinitionType [Name] [Exp]
  -- with DefinitionName = Anonymous | Named String
  | Definition DefinitionType [Name] [Exp]
  | Call CallType Exp [Exp]
  | Access Exp Exp
  | Assignment Name Exp
  | Export Exp
  | NameExport Name
  | Var Name
  | ListConstructor [ListItem]
  | TupleConstructor [Exp]
  | Record [Field]
  | If Exp Exp Exp
  | Do [Exp]
  | Where Exp [Is]
  | Placeholder (PlaceholderRef, String) Exp
  | Extern (Ty.Qual Ty.Type) Name Name
  deriving(Eq, Show)

type Name = String


-- AST TABLE
type Table = M.Map FilePath AST

-- Functions

getType :: Core a -> Ty.Type
getType (Typed (_ Ty.:=> t) _ _) = t


getQualType :: Core a -> Ty.Qual Ty.Type
getQualType (Typed t _ _) = t


getExpName :: Exp -> Maybe String
getExpName (Untyped _ _)    = Nothing
getExpName (Typed _ _ exp) = case exp of
  Assignment name _ ->
    return name

  Export (Typed _ _ (Assignment name _)) ->
    return name

  Extern _ name _ ->
    return name

  Export (Typed _ _ (Extern _ name _)) ->
    return name

  _ ->
    Nothing


getStartLine :: Exp -> Int
getStartLine (Typed _ (Area (Loc _ line _) _) _) = line
getStartLine (Untyped (Area (Loc _ line _) _) _    ) = line

getValue :: Core a -> a
getValue (Typed _ _ a) = a
getValue (Untyped _ a    ) = a


getListItemExp :: ListItem -> Exp
getListItemExp li = case li of
  Typed _ _ (ListItem e) ->
    e

  Typed _ _ (ListSpread e) ->
    e

getFieldExp :: Field -> Exp
getFieldExp li = case li of
  Typed _ _ (Field (_, e)) ->
    e

  Typed _ _ (FieldSpread e) ->
    e


getIsExpression :: Is -> Exp
getIsExpression is = case is of
  Typed _ _ (Is _ exp) ->
    exp


isTopLevelFunction :: Exp -> Bool
isTopLevelFunction exp = case exp of
  Typed _ _ (Assignment _ (Typed _ _ Definition{})) ->
    True

  Typed _ _ (Export (Typed _ _ (Assignment _ (Typed _ _ Definition{})))) ->
    True

  _ ->
    False
