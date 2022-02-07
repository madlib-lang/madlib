module AST.ClosureConverted where


import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Map                      as M


data ClosureConverted a
  = Typed (Ty.Qual Ty.Type) Area a
  -- = Typed Ty.Type Area a
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

type Import = ClosureConverted Import_
data Import_
  = NamedImport [ClosureConverted Name] FilePath FilePath
  | DefaultImport (ClosureConverted Name) FilePath FilePath
  deriving(Eq, Show, Ord)

type Interface = ClosureConverted Interface_
data Interface_ = Interface Name [Ty.Pred] [String] (M.Map Name Ty.Scheme) (M.Map Name Typing) deriving(Eq, Show, Ord)

type Instance = ClosureConverted Instance_
data Instance_ = Instance Name [Ty.Pred] String (M.Map Name (Exp, Ty.Scheme)) deriving(Eq, Show, Ord)

type TypeDecl = ClosureConverted TypeDecl_
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
    deriving(Eq, Show, Ord)

type Constructor = ClosureConverted Constructor_
data Constructor_
  = Constructor Name [Typing] Ty.Type
  deriving(Eq, Show, Ord)

type Constraints = [Typing]

type Typing = ClosureConverted Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show, Ord)


type Is = ClosureConverted Is_
data Is_ = Is Pattern Exp deriving(Eq, Show, Ord)


type Pattern = ClosureConverted Pattern_
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
  deriving(Eq, Show, Ord)


type Field = ClosureConverted Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show, Ord)


type ListItem = ClosureConverted ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show, Ord)


data ClassRefPred
  = CRPNode String String Bool [ClassRefPred] -- Bool to control if it's a var or a concrete dictionary
  deriving(Eq, Show, Ord)


data PlaceholderRef
  = ClassRef String [ClassRefPred] Bool Bool -- first bool is call (Class...), second bool is var (class_var vs class.selector)
  | MethodRef String String Bool
  deriving(Eq, Show, Ord)


data DefinitionType
  = BasicDefinition
  | TCEOptimizableDefinition
  deriving(Eq, Show, Ord)

data CallType
  = SimpleCall
  | RecursiveTailCall
  deriving(Eq, Show, Ord)


type Exp = ClosureConverted Exp_
data Exp_ = LNum String
          | LFloat String
          | LStr String
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | JSExp String
          | Call CallType Exp [Exp]
          | Access Exp Exp
          -- TODO: probably add an export boolean here?
          | Definition DefinitionType Name [Name] [Exp]
          -- ^ name of the function | params | body
          | Assignment Name Exp Bool
          -- ^ name | exp assigned | isTopLevel
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
          | Extern (Ty.Qual Ty.Type) Name Name

          -- ^ Closure name | env ( Only Var exps ) | param | body
          deriving(Eq, Show, Ord)

type Name = String


-- AST TABLE
type Table = M.Map FilePath AST

-- Functions

getStartLine :: Exp -> Int
getStartLine (Typed _ (Area (Loc _ line _) _) _) = line
getStartLine (Untyped (Area (Loc _ line _) _) _    ) = line

getValue :: ClosureConverted a -> a
getValue (Typed _ _ a) = a
getValue (Untyped _ a    ) = a


getType :: ClosureConverted a -> Ty.Type
getType (Typed (_ Ty.:=> t) _ _) = t

getQualType :: ClosureConverted a -> Ty.Qual Ty.Type
getQualType (Typed qt _ _) = qt


isTopLevelFunction :: Exp -> Bool
isTopLevelFunction exp = case exp of
  Typed _ _ Definition{} ->
    True

  _ ->
    False


isSpreadField :: Field -> Bool
isSpreadField field = case field of
  Typed _ _ (FieldSpread _) ->
    True

  _ ->
    False


isExtern :: Exp -> Bool
isExtern exp = case exp of
  Typed _ _ Extern{} ->
    True

  Typed _ _ (Export (Typed _ _ Extern{})) ->
    True

  _ ->
    False

isTypeExport :: Exp -> Bool
isTypeExport exp = case exp of
  Untyped _ (TypeExport _) ->
    True

  _ ->
    False


isADT :: TypeDecl -> Bool
isADT td = case td of
  Untyped _ ADT {} -> True
  _                -> False

getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Untyped _ (NamedImport   _ _ n) ->
    n

  Untyped _ (DefaultImport _ _ n) ->
    n

  _ ->
    undefined

getConstructorName :: Constructor -> String
getConstructorName constructor = case constructor of
  Typed _ _ (Constructor name _ _) ->
    name

  Untyped _ (Constructor name _ _) ->
    name
