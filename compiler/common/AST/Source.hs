{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module AST.Source where

import           Explain.Location
import qualified Data.Map                      as M
import           Data.Hashable
import           GHC.Generics hiding(Constructor)


data SourceTarget
  = TargetLLVM
  | TargetJS
  | TargetAll
  deriving(Eq, Show, Generic, Hashable)

data Source a = Source Area SourceTarget a deriving(Eq, Show, Generic, Hashable)

data AST =
  AST
    { aimports    :: [Import]
    , aexps       :: [Exp]
    , atypedecls  :: [TypeDecl]
    , ainterfaces :: [Interface]
    , ainstances  :: [Instance]
    , apath       :: Maybe FilePath
    }
    deriving(Eq, Show, Generic, Hashable)

type Import = Source Import_
-- The second FilePath parameter is the absolute path to that module
data Import_
  = NamedImport [Source Name] FilePath FilePath
  | TypeImport [Source Name] FilePath FilePath
  | DefaultImport (Source Name) FilePath FilePath
  deriving(Eq, Show, Generic, Hashable)

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
    deriving(Eq, Show, Generic, Hashable)


type Interface = Source Interface_
data Interface_
  = Interface Constraints Name [Name] (M.Map Name Typing)
  deriving(Eq, Show, Generic, Hashable)

type Instance = Source Instance_
data Instance_
  = Instance Constraints Name [Typing] (M.Map Name Exp)
  deriving(Eq, Show, Generic, Hashable)

type Constructor = Source Constructor_
data Constructor_
  = Constructor Name [Typing]
  deriving(Eq, Show, Generic, Hashable)



type Constraints = [Typing]

type Typing = Source Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  -- (Area, Typing) <- Area of the name
  -- Maybe typing for the possible extension
  | TRRecord (M.Map Name (Area, Typing)) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show, Generic, Hashable)


type Is = Source Is_
data Is_
  = Is Pattern Exp
  deriving(Eq, Show, Generic, Hashable)


data PatternField
  = PatternField (Source Name) Pattern
  | PatternFieldShorthand (Source Name)
  deriving(Eq, Show, Generic, Hashable)


type Pattern = Source Pattern_
data Pattern_
  = PVar Name
  | PNum String
  | PFloat String
  | PStr String
  | PChar Char
  | PBool String
  | PAny
  | PCon (Source Name) [Pattern]
  | PNullaryCon (Source Name)
  | PRecord [PatternField]
  | PList [Pattern]
  | PTuple [Pattern]
  | PSpread Pattern
  deriving(Eq, Show, Generic, Hashable)

type Field = Source Field_
data Field_
  = Field (Name, Exp)
  | FieldShorthand Name
  | FieldSpread Exp
  deriving(Eq, Show, Generic, Hashable)


type ListItem = Source ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show, Generic, Hashable)

type DictItem = Source DictItem_
data DictItem_
  = DictItem Exp Exp
  deriving(Eq, Show, Generic, Hashable)


type Exp = Source Exp_
data Exp_
  = LNum String
  | LFloat String
  | LStr String
  | LChar Char
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
  | Assignment Name Exp
  | Record [Field]
  | If Exp Exp Exp
  | Ternary Exp Exp Exp
  | Where Exp [Is]
  | WhereAbs [Is]
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
  | Extern Typing Name Name
  | IfTarget SourceTarget
  | ElseIfTarget SourceTarget
  | EndIfTarget
  | ConstructorAccess Name Name Exp
  | TypedHole
  deriving(Eq, Show, Generic, Hashable)


data JsxChild
  = JsxChild Exp
  | JsxExpChild Exp
  | JsxSpreadChild Exp
  deriving(Eq, Show, Generic, Hashable)

type JsxProp = Source JsxProp_
data JsxProp_
  = JsxProp Name Exp
  deriving(Eq, Show, Generic, Hashable)


type Name = String


-- AST TABLE

type Table = M.Map FilePath AST

-- Functions

isMacroExp :: Exp -> Bool
isMacroExp exp = case exp of
  Source _ _ (IfTarget _) ->
    True

  Source _ _ (ElseIfTarget _) ->
    True

  Source _ _ EndIfTarget ->
    True

  _ ->
    False


isTypeImport :: Import -> Bool
isTypeImport imp = case imp of
  Source _ _ TypeImport{} ->
    True

  _ ->
    False


getImportNames :: Import -> [Source Name]
getImportNames imp = case imp of
  Source _ _ (NamedImport names _ _) ->
    names

  Source _ _ DefaultImport{}         ->
    []

  Source _ _ TypeImport{}            ->
    []


getImportTypeNames :: Import -> [Source Name]
getImportTypeNames imp = case imp of
  Source _ _ (NamedImport _ _ _) ->
    []

  Source _ _ (TypeImport  names _ _) ->
    names

  Source _ _ DefaultImport{}         ->
    []


getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Source _ _ (NamedImport   _ _ n) ->
    n

  Source _ _ (TypeImport    _ _ n) ->
    n

  Source _ _ (DefaultImport _ _ n) ->
    n


getImportPath :: Import -> (Import, FilePath)
getImportPath imp@(Source _ _ (NamedImport   _ p _)) =
  (imp, p)

getImportPath imp@(Source _ _ (TypeImport    _ p _)) =
  (imp, p)

getImportPath imp@(Source _ _ (DefaultImport _ p _)) =
  (imp, p)


getArea :: Source a -> Area
getArea (Source area _ _) =
  area

getSourceTarget :: Source a -> SourceTarget
getSourceTarget (Source _ sourceTarget _) =
  sourceTarget

getSourceContent :: Source a -> a
getSourceContent (Source _ _ a) =
  a


getInstanceTypings :: Instance -> [Typing]
getInstanceTypings inst = case inst of
  Source _ _ (Instance _ _ typings _) ->
    typings

getInstanceConstraintTypings :: Instance -> [Typing]
getInstanceConstraintTypings inst = case inst of
  Source _ _ (Instance constraints _ _ _) ->
    constraints

isAbs :: Exp -> Bool
isAbs exp = case exp of
  Source _ _ (Abs _ _) ->
    True

  Source _ _ (AbsWithMultilineBody _ _) ->
    True

  _ ->
    False


-- Used to detected unused variables
getLocalOrNotExportedAssignmentName :: Exp -> Maybe (Source String)
getLocalOrNotExportedAssignmentName (Source (Area (Loc a l c) _) target exp) = case exp of
  Assignment name _ ->
    return $ Source (Area (Loc a l c) (Loc (a + length name) l (c + length name))) target name

  TypedExp (Source (Area (Loc a l c) _) target' (Assignment name _)) _ ->
    return $ Source (Area (Loc a l c) (Loc (a + length name) l (c + length name))) target' name

  NamedTypedExp name _ _ ->
    return $ Source (Area (Loc a l c) (Loc (a + length name) l (c + length name))) target name

  _ ->
    Nothing
