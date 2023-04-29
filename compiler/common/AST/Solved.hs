{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module AST.Solved where

import qualified Data.Map                      as M
import qualified Data.List                     as List

import qualified Infer.Type                    as Ty
import           Explain.Location
import qualified Data.Maybe as Maybe
import           Data.Hashable
import           GHC.Generics hiding(Constructor)



data Solved a
  = Typed (Ty.Qual Ty.Type) Area a
  | Untyped Area a
  deriving(Eq, Show, Ord, Generic, Hashable)

-- instance Show a => Show (Solved a) where
--   show x = case x of
--     Typed _ _ a ->
--       show a
--     Untyped _ a ->
--       show a


data AST =
  AST
    { aimports    :: [Import]
    , aexps       :: [Exp]
    , atypedecls  :: [TypeDecl]
    , ainterfaces :: [Interface]
    , ainstances  :: [Instance]
    , apath       :: Maybe FilePath
    }
    deriving(Eq, Show, Ord, Generic, Hashable)

type Import = Solved Import_
data Import_
  = NamedImport [Solved Name] FilePath FilePath
  | DefaultImport (Solved Name) FilePath FilePath
  deriving(Eq, Show, Ord, Generic, Hashable)

type Interface = Solved Interface_
data Interface_
  = Interface Name [Ty.Pred] [Ty.TVar] (M.Map Name Ty.Scheme) (M.Map Name Typing)
  deriving(Eq, Show, Ord, Generic, Hashable)

type Instance = Solved Instance_
data Instance_
  = Instance Name [Ty.Pred] Ty.Pred (M.Map Name (Exp, Ty.Scheme))
  deriving(Eq, Show, Ord, Generic, Hashable)

type TypeDecl = Solved TypeDecl_
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
    deriving(Eq, Show, Ord, Generic, Hashable)

type Constructor = Solved Constructor_
data Constructor_
  = Constructor Name [Typing] Ty.Type
  deriving(Eq, Show, Ord, Generic, Hashable)

type Constraints = [Typing]

type Typing = Solved Typing_
data Typing_
  = TRSingle Name
  | TRComp Name [Typing]
  | TRArr Typing Typing
  | TRRecord (M.Map Name (Area, Typing)) (Maybe Typing)
  | TRTuple [Typing]
  | TRConstrained Constraints Typing -- List of constrains and the typing it applies to
  deriving(Eq, Show, Ord, Generic, Hashable)


type Is = Solved Is_
data Is_
  = Is Pattern Exp
  deriving(Eq, Show, Ord, Generic, Hashable)

type Pattern = Solved Pattern_
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
  deriving(Eq, Show, Ord, Generic, Hashable)

type Field = Solved Field_
data Field_
  = Field (Name, Exp)
  | FieldSpread Exp
  deriving(Eq, Show, Ord, Generic, Hashable)

type ListItem = Solved ListItem_
data ListItem_
  = ListItem Exp
  | ListSpread Exp
  deriving(Eq, Show, Ord, Generic, Hashable)


data ClassRefPred
  = CRPNode String [Ty.Type] Bool [ClassRefPred] -- Bool to control if it's a var or a concrete dictionary
  deriving(Eq, Show, Ord, Generic, Hashable)

data PlaceholderRef
  = ClassRef String [ClassRefPred] Bool Bool -- first bool is call (Class...), second bool is var (class_var vs class.selector)
  | MethodRef String String Bool -- bool is isVar
  deriving(Eq, Show, Ord, Generic, Hashable)

type Exp = Solved Exp_
data Exp_ = LNum String
          | LFloat String
          | LStr String
          | LChar Char
          | LBool String
          | LUnit
          | TemplateString [Exp]
          | JSExp String
          | App Exp Exp Bool
          | Access Exp Exp
          | Abs (Solved Name) [Exp]
          | Assignment Name Exp
          | Export Exp
          | NameExport Name
          | TypeExport Name
          | Var Name Bool
          -- ^ Bool isConstructor
          | TypedExp Exp Typing Ty.Scheme
          | ListConstructor [ListItem]
          | TupleConstructor [Exp]
          | Record [Field]
          | If Exp Exp Exp
          | Do [Exp]
          | Where Exp [Is]
          | Placeholder (PlaceholderRef, [Ty.Type]) Exp
          | Extern (Ty.Qual Ty.Type) Name Name
          | TypedHole
          deriving(Eq, Show, Ord, Generic, Hashable)


type Name = String


-- AST TABLE

type Table = M.Map FilePath AST


-- Functions

getAllExpsFromGlobalScope :: AST -> [(String, Exp)]
getAllExpsFromGlobalScope ast =
  let exps = Maybe.mapMaybe
        (\e -> case getExpName e of
                  Just n ->
                    Just (n, e)

                  _ ->
                    Nothing
        )
        (aexps ast)
      methods = Maybe.mapMaybe
        (\e -> case getExpName e of
                  Just n ->
                    Just (n, e)

                  _ ->
                    Nothing
        )
        (concat $ getInstanceMethods <$> ainstances ast)
  in  methods ++ exps


getType :: Solved a -> Ty.Type
getType (Typed (_ Ty.:=> t) _ _) = t


getQualType :: Solved a -> Ty.Qual Ty.Type
getQualType (Typed t _ _) = t


getArea :: Solved a -> Area
getArea a = case a of
  Typed _ area _ ->
    area

  Untyped area _ ->
    area


extractExp :: Exp -> Exp_
extractExp (Typed _ (Area _ _) e) = e


getDefaultImportNames :: AST -> [String]
getDefaultImportNames ast =
  let imports = aimports ast
  in  Maybe.mapMaybe
        (\case
          Untyped _ (DefaultImport (Untyped _ n) _ _) ->
            Just n

          _ ->
            Nothing
        )
        imports


findForeignModuleForImportedName :: String -> AST -> Maybe FilePath
findForeignModuleForImportedName expName ast =
  let imports = aimports ast
      found = List.find
                (\case
                  Untyped _ (DefaultImport (Untyped _ n) _ _) | n == expName ->
                    True

                  Untyped _ (NamedImport names _ _) ->
                    if expName `elem` (map getValue names) then
                      True
                    else
                      False

                  _ ->
                    False
                )
                imports
  in  getImportAbsolutePath <$> found
        


getADTConstructors :: TypeDecl -> Maybe [Constructor]
getADTConstructors td = case td of
  Untyped _ ADT { adtconstructors } ->
    Just adtconstructors

  _ ->
    Nothing


unsafeGetADTConstructors :: TypeDecl -> [Constructor]
unsafeGetADTConstructors td = case td of
  Untyped _ ADT { adtconstructors } ->
    adtconstructors


getConstructorName :: Constructor -> String
getConstructorName (Untyped _ (Constructor name _ _)) = name


getConstructorTypings :: Constructor -> [Typing]
getConstructorTypings (Untyped _ (Constructor _ typings _)) = typings


getConstructorType :: Constructor -> Ty.Type
getConstructorType (Untyped _ (Constructor _ _ t)) = t


isADTExported :: TypeDecl -> Bool
isADTExported adt = case adt of
  Untyped _ ADT { adtexported } ->
    adtexported

  _ ->
    False


isSpreadPattern :: Pattern -> Bool
isSpreadPattern pattern = case pattern of
  Typed _ _ (PSpread _) ->
    True

  _ ->
    False


getSpreadPattern :: [Pattern] -> Maybe Pattern
getSpreadPattern patterns = List.find isSpreadPattern patterns


isAliasExported :: TypeDecl -> Bool
isAliasExported alias = case alias of
  Untyped _ Alias { aliasexported } ->
    aliasexported

  _ ->
    False


isAlias :: TypeDecl -> Bool
isAlias td = case td of
  Untyped _ Alias {} ->
    True

  _                  ->
    False


isADT :: TypeDecl -> Bool
isADT td = case td of
  Untyped _ ADT {} ->
    True

  _ ->
    False


isExportOnly :: Exp -> Bool
isExportOnly a = case a of
  (Typed _ _ (Export _)) ->
    True

  (Typed _ _ (TypedExp (Typed _ _ (Export _)) _ _)) ->
    True

  _ ->
    False


isNameExport :: Exp -> Bool
isNameExport a = case a of
  (Typed _ _ (NameExport _)) ->
    True

  _ ->
    False


isTypeExport :: Exp -> Bool
isTypeExport a = case a of
  (Untyped _ (TypeExport _)) ->
    True

  _ ->
    False


isExtern :: Exp -> Bool
isExtern a = case a of
  (Typed _ _ Extern {}) ->
    True

  Typed _ _ (Export (Typed _ _ Extern {})) ->
    True

  _ ->
    False


getTypeExportName :: Exp -> Name
getTypeExportName a = case a of
  Untyped _ (TypeExport name) ->
    name

  _ ->
    undefined


isTypeOrNameExport :: Exp -> Bool
isTypeOrNameExport exp = isNameExport exp || isTypeExport exp


isTypedExp :: Exp -> Bool
isTypedExp a = case a of
  Typed _ _ TypedExp{} ->
    True

  Typed _ _ Extern{} ->
    True

  Typed _ _ (Export (Typed _ _ Extern{})) ->
    True

  _ ->
    False


getNameExportName :: Exp -> Name
getNameExportName a = case a of
  Typed _ _ (NameExport name) ->
    name

  _ ->
    undefined


isExport :: Exp -> Bool
isExport a = case a of
  (Typed _ _ (Export _)) -> True

  (Typed _ _ (TypedExp (Typed _ _ (Export _)) _ _)) -> True

  (Typed _ _ (NameExport _)) -> True

  _ -> False


getValue :: Solved a -> a
getValue (Typed _ _ a) = a
getValue (Untyped _ a ) = a


getExpName :: Exp -> Maybe String
getExpName (Untyped _ _)    = Nothing
getExpName (Typed _ _ exp) = case exp of
  Assignment name _ ->
    return name

  TypedExp (Typed _ _ (Assignment name _)) _ _ ->
    return name

  TypedExp (Typed _ _ (Export (Typed _ _ (Assignment name _)))) _ _ ->
    return name

  Export (Typed _ _ (Assignment name _)) ->
    return name

  Extern _ name _ ->
    return name

  Export (Typed _ _ (Extern _ name _)) ->
    return name

  _ ->
    Nothing


getAllMethods :: AST -> [Exp]
getAllMethods AST{ ainstances } =
  concat $ getInstanceMethods <$> ainstances


getInstanceMethods :: Instance -> [Exp]
getInstanceMethods inst = case inst of
  Untyped _ (Instance _ _ _ methods) -> M.elems $ M.map fst methods


getInstanceName :: Instance -> String
getInstanceName inst = case inst of
  Untyped _ (Instance name _ _ _) ->
    name


extractExportedADTs :: AST -> [TypeDecl]
extractExportedADTs ast =
  let typeExports = getTypeExportName <$> filter isTypeExport (aexps ast)
  in  filter
    (\td@(Untyped _ adt) -> isADT td && (isADTExported td || adtname adt `elem` typeExports))
    $ atypedecls ast


extractExportedAliases :: AST -> [TypeDecl]
extractExportedAliases ast =
  let typeExports = getTypeExportName <$> filter isTypeExport (aexps ast)
  in  filter
    (\td@(Untyped _ alias) -> isAlias td && (isAliasExported td || aliasname alias `elem` typeExports))
    $ atypedecls ast


extractExportedExps :: AST -> M.Map Name Exp
extractExportedExps AST { aexps, apath } = case apath of
  Just _ ->
    M.fromList $ bundleExports <$> filter isExport aexps

  Nothing ->
    mempty


bundleExports :: Exp -> (Name, Exp)
bundleExports e'@(Typed _ _ exp) = case exp of
  Export (Typed _ _ (Assignment n _)) ->
    (n, e')

  Export (Typed _ _ (Extern _ n _)) ->
    (n, e')

  TypedExp (Typed _ _ (Export (Typed _ _ (Assignment n _)))) _ _ ->
    (n, e')

  NameExport n ->
    (n, e')


getImportAbsolutePath :: Import -> FilePath
getImportAbsolutePath imp = case imp of
  Untyped _ (NamedImport   _ _ n) ->
    n

  Untyped _ (DefaultImport _ _ n) ->
    n


isPlaceholderExp :: Exp -> Bool
isPlaceholderExp exp = case exp of
  Typed _ _ (Placeholder _ _) ->
    True

  _ ->
    False

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

-- Get fn and args as a list
collectAppArgs :: Bool -> Exp -> (Exp, [Exp])
collectAppArgs isFirst app = case app of
  Typed _ _ (App next arg isFinal) | not isFinal || isFirst ->
    let (nextFn, nextArgs) = collectAppArgs False next
    in  (nextFn, nextArgs <> [arg])

  b ->
    (b, [])

isNamedAbs :: Exp -> Bool
isNamedAbs exp = case exp of
  Typed _ _ (Assignment _ (Typed _ _ (Abs _ _))) ->
    True

  Typed _ _ (TypedExp (Typed _ _ (Assignment _ (Typed _ _ (Abs _ _)))) _ _) ->
    True

  _ ->
    False


isAbs :: Exp -> Bool
isAbs exp = case exp of
  Typed _ _ (Assignment _ (Typed _ _ (Abs _ _))) ->
    True

  Typed _ _ (Export (Typed _ _ (Assignment _ (Typed _ _ (Abs _ _))))) ->
    True

  Typed _ _ (TypedExp (Typed _ _ (Assignment _ (Typed _ _ (Abs _ _)))) _ _) ->
    True

  Typed _ _ (TypedExp (Typed _ _ (Export (Typed _ _ (Assignment _ (Typed _ _ (Abs _ _)))))) _ _) ->
    True

  _ ->
    False
