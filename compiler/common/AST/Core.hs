{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module AST.Core where

import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Map                      as M
import           Data.Hashable
import           GHC.Generics hiding(Constructor)


data RecursionDirection
  = LeftRecursion
  | RightRecursion
  | BothRecursion
  deriving(Eq, Show, Ord, Generic, Hashable)

data ConstructorRecursionInfo
  = ConstructorRecursionInfo String Int
  -- ^ String: constructor name, Int: arg position
  deriving(Eq, Show, Ord, Generic, Hashable)

data RecursionKind
  = PlainRecursion
  | ListRecursion RecursionDirection
  | ConstructorRecursion (Maybe ConstructorRecursionInfo)
  | BooleanAndRecursion
  | BooleanOrRecursion
  | NotOptimizable
  deriving(Eq, Show, Ord, Generic, Hashable)

data Metadata
  = RecursionEnd RecursionKind
  | RecursiveDefinition RecursionKind
  | RecursiveCall RecursionKind
  | ReferenceParameter
  | ReferenceAllocation
  | ReferenceStore
  | ReferenceArgument
  | MutatingFunctionRef
  deriving(Eq, Show, Ord, Generic, Hashable)

-- TODO: remove Area, we don't care anymore at this stage
-- And make qual types simple types
data Core a
  = Typed (Ty.Qual Ty.Type) Area [Metadata] a
  | Untyped Area [Metadata] a
  deriving(Eq, Ord, Generic, Hashable)

instance Show a => Show (Core a) where
  show x = case x of
    Typed _ _ _ a ->
      show a
    Untyped _ _ a ->
      show a

data AST =
  AST
    { aimports    :: [Import]
    , aexps       :: [Exp]
    , atypedecls  :: [TypeDecl]
    , apath       :: Maybe FilePath
    }
    deriving(Eq, Show, Generic, Hashable)

data ImportType
  = DefinitionImport Int
  | ConstructorImport
  | ExpressionImport
  deriving(Eq, Show, Generic, Hashable)

data ImportInfo
  = ImportInfo Name ImportType
  deriving(Eq, Show, Generic, Hashable)

type Import = Core Import_
data Import_
  = NamedImport [Core ImportInfo] FilePath FilePath
  deriving(Eq, Show, Generic, Hashable)

type TypeDecl = Core TypeDecl_
data TypeDecl_
  = ADT
    { adtname :: Name
    , adtparams :: [Name]
    , adtconstructors :: [Constructor]
    , adtexported :: Bool
    }
    deriving(Eq, Show, Generic, Hashable)

-- TODO: remove typing
type Constructor = Core Constructor_
data Constructor_
  = Constructor Name [Typing] Ty.Type
  deriving(Eq, Show, Generic, Hashable)

type Constraints = [Typing]

type Typing = Core Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name Typing) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show, Generic, Hashable)


type Is = Core Is_
data Is_
  = Is Pattern Exp
  deriving(Eq, Show, Generic, Hashable)

type Pattern = Core Pattern_
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
  deriving(Eq, Show, Generic, Hashable)

type Field = Core Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show, Generic, Hashable)

type ListItem = Core ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show, Generic, Hashable)


data Literal
  = LNum String
  | LFloat String
  | LStr String
  | LChar Char
  | LBool String
  | LUnit
  deriving(Eq, Show, Generic, Hashable)


type Exp = Core Exp_
data Exp_
  = Literal Literal
  | JSExp String
  | Definition [Core Name] [Exp]
  | Call Exp [Exp]
  | Access Exp Exp
  | Assignment Name Exp
  | Export Exp
  | NameExport Name
  | Var Name Bool
  -- ^ Bool isConstructor
  | ListConstructor [ListItem]
  | TupleConstructor [Exp]
  | Record [Field]
  | If Exp Exp Exp
  | Do [Exp]
  | Where Exp [Is]
  | Extern (Ty.Qual Ty.Type) Name Name
  | TypedHole
  deriving(Eq, Show, Generic, Hashable)

type Name = String

-- Functions

getType :: Core a -> Ty.Type
getType (Typed (_ Ty.:=> t) _ _ _) = t


getQualType :: Core a -> Ty.Qual Ty.Type
getQualType (Typed t _ _ _) = t


getArea :: Core a -> Area
getArea core = case core of
  Typed _ area _ _ ->
    area

  Untyped area _ _ ->
    area


getMetadata :: Core a -> [Metadata]
getMetadata core = case core of
  Typed _ _ metadata _ ->
    metadata

  Untyped _ metadata _ ->
    metadata


updateQualType :: Ty.Qual Ty.Type -> Core a -> Core a
updateQualType qt core = case core of
  Typed _ area metadata e ->
    Typed qt area metadata e

  _ ->
    core


getExpName :: Exp -> Maybe String
getExpName Untyped{}         = Nothing
getExpName (Typed _ _ _ exp) = case exp of
  Assignment name _ ->
    return name

  Export (Typed _ _ _ (Assignment name _)) ->
    return name

  Extern _ name _ ->
    return name

  Export (Typed _ _ _ (Extern _ name _)) ->
    return name

  _ ->
    Nothing


getStartLine :: Exp -> Int
getStartLine (Typed _ (Area (Loc _ line _) _) _ _) = line
getStartLine (Untyped (Area (Loc _ line _) _) _ _    ) = line

getValue :: Core a -> a
getValue (Typed _ _ _ a) = a
getValue (Untyped _ _ a    ) = a


mapListItem :: (Exp -> Exp) -> ListItem -> ListItem
mapListItem f item = case item of
  Typed qt area metadata (ListItem e) ->
    Typed qt area metadata (ListItem (f e))

  Typed qt area metadata (ListSpread e) ->
    Typed qt area metadata (ListSpread (f e))

  _ ->
    item

mapIs :: (Exp -> Exp) -> Is -> Is
mapIs f is = case is of
  Typed qt area metadata (Is pat e) ->
    Typed qt area metadata (Is pat (f e))

  _ ->
    is

mapRecordField :: (Exp -> Exp) -> Field -> Field
mapRecordField f field = case field of
  Typed qt area metadata (Field (n, e)) ->
    Typed qt area metadata (Field (n, f e))

  Typed qt area metadata (FieldSpread e) ->
    Typed qt area metadata (FieldSpread (f e))

  _ ->
    field

getListItemExp :: ListItem -> Exp
getListItemExp li = case li of
  Typed _ _ _ (ListItem e) ->
    e

  Typed _ _ _ (ListSpread e) ->
    e

getFieldExp :: Field -> Exp
getFieldExp li = case li of
  Typed _ _ _ (Field (_, e)) ->
    e

  Typed _ _ _ (FieldSpread e) ->
    e


getIsExpression :: Is -> Exp
getIsExpression is = case is of
  Typed _ _ _ (Is _ exp) ->
    exp


getConstructorArity :: Constructor -> Int
getConstructorArity constructor = case constructor of
  Typed _ _ _ (Constructor _ params _) ->
    length params

  Untyped _ _ (Constructor _ params _) ->
    length params


-- Should be called on top level [Exp] nodes
isTopLevelFunction :: Exp -> Bool
isTopLevelFunction exp = case exp of
  Typed _ _ _ (Assignment _ (Typed _ _ _ Definition{})) ->
    True

  Typed _ _ _ (Export (Typed _ _ _ (Assignment _ (Typed _ _ _ Definition{})))) ->
    True

  _ ->
    False


-- Should be called on top level [Exp] nodes
isTopLevelAssignment :: Exp -> Bool
isTopLevelAssignment exp = case exp of
  Typed _ _ _ (Assignment _ _) ->
    True

  Typed _ _ _ (Export (Typed _ _ _ (Assignment _ _))) ->
    True

  _ ->
    False


isADT :: TypeDecl -> Bool
isADT td = case td of
  Untyped _ _ ADT {} ->
    True

  _                ->
    False


isExtern :: Exp -> Bool
isExtern exp = case exp of
  Typed _ _ _ Extern{} ->
    True

  Typed _ _ _ (Export (Typed _ _ _ Extern{})) ->
    True

  _ ->
    False


isSpreadField :: Field -> Bool
isSpreadField field = case field of
  Typed _ _ _ (FieldSpread _) ->
    True

  _ ->
    False


getFieldName :: Field -> Maybe String
getFieldName field = case field of
  Typed _ _ _ (Field (name, _)) ->
    Just name

  _ ->
    Nothing


getConstructorName :: Constructor -> String
getConstructorName constructor = case constructor of
  Typed _ _ _ (Constructor name _ _) ->
    name

  Untyped _ _ (Constructor name _ _) ->
    name


getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Untyped _ _ (NamedImport   _ _ n) ->
    n

  _ ->
    undefined


isTCODefinition :: [Metadata] -> Bool
isTCODefinition = any isTCODefinitionMetada

isTCODefinitionMetada :: Metadata -> Bool
isTCODefinitionMetada meta = case meta of
  RecursiveDefinition NotOptimizable ->
    False

  RecursiveDefinition _ ->
    True

  _ ->
    False

isPlainRecursiveCall :: [Metadata] -> Bool
isPlainRecursiveCall = elem (RecursiveCall PlainRecursion)

isPlainRecursiveDefinition :: [Metadata] -> Bool
isPlainRecursiveDefinition = elem (RecursiveDefinition PlainRecursion)

isPlainRecursionEnd :: [Metadata] -> Bool
isPlainRecursionEnd = elem (RecursionEnd PlainRecursion)

isRightListRecursiveCall :: [Metadata] -> Bool
isRightListRecursiveCall = elem (RecursiveCall (ListRecursion RightRecursion))

isRightListRecursiveDefinition :: [Metadata] -> Bool
isRightListRecursiveDefinition = elem (RecursiveDefinition (ListRecursion RightRecursion))

isRightListRecursionEnd :: [Metadata] -> Bool
isRightListRecursionEnd = elem (RecursionEnd (ListRecursion RightRecursion))

isConstructorRecursiveDefinition :: [Metadata] -> Bool
isConstructorRecursiveDefinition = elem (RecursiveDefinition (ConstructorRecursion Nothing))

isConstructorRecursionEnd :: [Metadata] -> Bool
isConstructorRecursionEnd = elem (RecursionEnd (ConstructorRecursion Nothing))

isConstructorRecursiveCall :: [Metadata] -> Bool
isConstructorRecursiveCall metadata = case metadata of
  (RecursiveCall (ConstructorRecursion _) : _) ->
    True

  (_ : next) ->
    isConstructorRecursiveCall next

  _ ->
    False

getConstructorRecursionInfo :: [Metadata] -> Maybe ConstructorRecursionInfo
getConstructorRecursionInfo metadata = case metadata of
  (RecursiveCall (ConstructorRecursion info) : _) ->
    info

  (_ : next) ->
    getConstructorRecursionInfo next

  _ ->
    Nothing


isReferenceAllocation :: [Metadata] -> Bool
isReferenceAllocation = elem ReferenceAllocation

isReferenceStore :: [Metadata] -> Bool
isReferenceStore = elem ReferenceStore

isReferenceParameter :: [Metadata] -> Bool
isReferenceParameter = elem ReferenceParameter

isReferenceArgument :: [Metadata] -> Bool
isReferenceArgument = elem ReferenceArgument

isReferenceToMutatingFunction :: [Metadata] -> Bool
isReferenceToMutatingFunction = elem MutatingFunctionRef

getImportName :: Core ImportInfo -> String
getImportName info = case info of
  Typed _ _ _ (ImportInfo n _) ->
    n

isJSExp :: Exp -> Bool
isJSExp exp = case exp of
  Typed _ _ _ (JSExp _) ->
    True

  _ ->
    False

getJSExpContent :: Exp -> String
getJSExpContent exp = case exp of
  Typed _ _ _ (JSExp js) ->
    js

  _ ->
    ""
