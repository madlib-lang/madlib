{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Generate.LLVM.Types
  ( -- * Core LLVM type constants
    boxType
  , listType
  , stringType
  , papType
  , recordType
    -- * Type exclusion list
  , tConExclude
    -- * sizeof helper
  , sizeof'
    -- * Type building
  , buildLLVMType
  , buildLLVMType'
  , buildLLVMParamType
    -- * Tuple/record field type (native storage type for primitive elements)
  , tupleFieldLLVMType
  , primitiveTupleFieldType
  , recordFieldLLVMTypes
    -- * Constructor helpers
  , retrieveConstructorStructType
  , retrieveConstructorMaxArity
  , isEnumADT
  , isNewtypeADT
  , isSingleConstructorADT
    -- * Tuple arity detection
  , tupleArity
    -- * Record helpers
  , flatRecordType
  , recordFieldIndex
    -- * Symbol helper
  , adtSymbol
  ) where

import qualified Data.Map                     as Map
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe

import qualified LLVM.AST.Type                as Type
import qualified LLVM.AST.Constant            as Constant
import           LLVM.AST.AddrSpace           (AddrSpace(..))
import qualified LLVM.AST.Operand             as Operand

import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import qualified Infer.Type                   as IT
import qualified Driver.Query                 as Query
import qualified Rock
import qualified AST.Solved                   as Slv


-- Core LLVM type constants

boxType :: Type.Type
boxType = Type.ptr Type.i8

listType :: Type.Type
listType = Type.PointerType (Type.StructureType False [boxType, boxType]) (AddrSpace 1)

stringType :: Type.Type
stringType = Type.PointerType Type.i8 (AddrSpace 1)

papType :: Type.Type
papType = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType, Type.i8]

recordType :: Type.Type
recordType = Type.ptr $ Type.StructureType False [Type.i32, boxType]

tConExclude :: [String]
tConExclude = ["Array", "ByteArray", "(->)", "(,)", "(,,)", "(,,,)", "(,,,,)", "(,,,,,)", "(,,,,,,)", "(,,,,,,,)", "(,,,,,,,,)", "(,,,,,,,,,)"]


-- sizeof helper

sizeof' :: Type.Type -> Constant.Constant
sizeof' t = Constant.PtrToInt szPtr (Type.IntegerType 64)
  where
    ptrType = Type.PointerType t (AddrSpace 0)
    nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
    szPtr   = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]


-- Tuple arity detection

-- | Detect tuple type constructor name and return its arity.
-- "(,)" -> Just 2, "(,,)" -> Just 3, etc.
tupleArity :: String -> Maybe Int
tupleArity name
  | length name >= 3
  , head name == '('
  , last name == ')'
  , let inner = init (tail name)
  , not (null inner)
  , all (== ',') inner
  = Just (length inner + 1)
  | otherwise = Nothing


-- Type building

buildLLVMType :: Env -> SymbolTable -> IT.Qual IT.Type -> Type.Type
buildLLVMType env symbolTable (ps IT.:=> t) = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    Type.double

  IT.TCon (IT.TC "Byte" IT.Star) "prelude" _ ->
    Type.i8

  IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
    Type.i32

  IT.TCon (IT.TC "Char" IT.Star) "prelude" _ ->
    Type.i32

  IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
    Type.i64

  IT.TCon (IT.TC "String" IT.Star) "prelude" _ ->
    stringType

  IT.TCon (IT.TC "Boolean" IT.Star) "prelude" _ ->
    Type.i1

  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    Type.ptr Type.i1

  IT.TVar _ | IT.hasNumberPred ps ->
    Type.i64

  IT.TApp (IT.TCon (IT.TC "List" (IT.Kfun IT.Star IT.Star)) "prelude" _) _ ->
    listType

  IT.TRecord fields _ optionalFields ->
    let allFields  = Map.union fields optionalFields
        fieldTypes = Map.elems allFields
    in  if null fieldTypes then
          recordType
        else
          Type.ptr $ Type.StructureType False (primitiveTupleFieldType . ([] IT.:=>) <$> fieldTypes)

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude" _) _) _ ->
    let arity = List.length $ IT.getParamTypes t
    in  Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False

  _ | isTupleType t ->
    let elemTypes = getTupleElemTypes t
        fieldTypes = tupleFieldLLVMType env symbolTable <$> elemTypes
    in  Type.ptr $ Type.StructureType False fieldTypes

  _ | IT.isTCon t ->
    if IT.getTConName t `List.notElem` tConExclude && IT.getTConName t /= "" then
      retrieveConstructorStructType env symbolTable t
    else
      Type.ptr Type.i8

  _ ->
    Type.ptr Type.i8


buildLLVMType' :: Rock.MonadFetch Query.Query m => IT.Qual IT.Type -> m (Type.Type, Maybe (String, Symbol))
buildLLVMType' (ps IT.:=> t) = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    return (Type.double, Nothing)

  IT.TCon (IT.TC "Byte" IT.Star) "prelude" _ ->
    return (Type.i8, Nothing)

  IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
    return (Type.i32, Nothing)

  IT.TCon (IT.TC "Char" IT.Star) "prelude" _ ->
    return (Type.i32, Nothing)

  IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
    return (Type.i64, Nothing)

  IT.TCon (IT.TC "String" IT.Star) "prelude" _ ->
    return (stringType, Nothing)

  IT.TCon (IT.TC "Boolean" IT.Star) "prelude" _ ->
    return (Type.i1, Nothing)

  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    return (Type.ptr Type.i1, Nothing)

  IT.TVar _ | IT.hasNumberPred ps ->
    return (Type.i64, Nothing)

  IT.TApp (IT.TCon (IT.TC "List" (IT.Kfun IT.Star IT.Star)) "prelude" _) _ ->
    return (listType, Nothing)

  IT.TRecord fields _ optionalFields -> do
    let allFields  = Map.union fields optionalFields
        fieldTypes = Map.elems allFields
    if null fieldTypes then
      return (recordType, Nothing)
    else
      return (Type.ptr $ Type.StructureType False (primitiveTupleFieldType . ([] IT.:=>) <$> fieldTypes), Nothing)

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude" _) _) _ -> do
    let arity = List.length $ IT.getParamTypes t
    return (Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False, Nothing)

  _ | isTupleType t ->
    let elemTypes  = getTupleElemTypes t
        fieldTypes = primitiveTupleFieldType <$> elemTypes
    in  return (Type.ptr $ Type.StructureType False fieldTypes, Nothing)

  _ | IT.isTCon t ->
    if IT.getTConName t `List.notElem` tConExclude && IT.getTConName t /= "" then do
      let adtTypePath = IT.getTConPath t
      let adtTypeName = IT.getTConName t
      maybeADT <- Rock.fetch $ Query.ForeignTypeDeclaration adtTypePath adtTypeName
      case maybeADT of
        Nothing ->
          return (boxType, Nothing)

        Just (Slv.Untyped _ adt) -> do
          let maxArity = Slv.findMaximumConstructorArity (Slv.adtconstructors adt)
          let ctorCount = List.length (Slv.adtconstructors adt)
          let sym = adtSymbol maxArity ctorCount
          if maxArity == 0 then
            return
              ( Type.i64
              , Just (adtTypePath <> "_" <> adtTypeName, sym)
              )
          else if ctorCount == 1 && maxArity == 1 then
            -- Newtype: single constructor, single field — erased to boxType
            return
              ( boxType
              , Just (adtTypePath <> "_" <> adtTypeName, sym)
              )
          else if ctorCount == 1 then
            -- Single-constructor: no tag field needed
            return
              ( Type.ptr $ Type.StructureType False (List.replicate maxArity boxType)
              , Just (adtTypePath <> "_" <> adtTypeName, sym)
              )
          else
            return
              ( Type.ptr $ Type.StructureType False (Type.i64 : List.replicate maxArity boxType)
              , Just (adtTypePath <> "_" <> adtTypeName, sym)
              )
    else
      return (Type.ptr Type.i8, Nothing)

  _ ->
    return (Type.ptr Type.i8, Nothing)


buildLLVMParamType :: Env -> SymbolTable -> IT.Type -> Type.Type
buildLLVMParamType env symbolTable t = case t of
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude" _) _) _ ->
    papType

  _ ->
    buildLLVMType env symbolTable ([] IT.:=> t)


-- | The direct storage type for a tuple element.
-- Primitive types are stored unboxed (i64, double, i1, etc.) to avoid
-- inttoptr/ptrtoint round-trips at construction and pattern-match sites.
-- Non-primitive types (strings, lists, ADTs, functions) remain as boxType (i8*).
-- Does not need Env or SymbolTable — primitive detection is type-driven only.
tupleFieldLLVMType :: Env -> SymbolTable -> IT.Qual IT.Type -> Type.Type
tupleFieldLLVMType _ _ = primitiveTupleFieldType

-- | Pure version of tupleFieldLLVMType, used where Env/SymbolTable are unavailable.
-- Primitive types are stored with their native LLVM type to avoid inttoptr/ptrtoint
-- round-trips at tuple construction and pattern-match sites.
-- Non-primitive types (strings, lists, ADTs, functions) remain as boxType (i8*).
primitiveTupleFieldType :: IT.Qual IT.Type -> Type.Type
primitiveTupleFieldType (ps IT.:=> t) = case t of
  IT.TCon (IT.TC "Float"   IT.Star) _ _ -> Type.double
  IT.TCon (IT.TC "Byte"    IT.Star) _ _ -> Type.i8
  IT.TCon (IT.TC "Short"   IT.Star) _ _ -> Type.i32
  IT.TCon (IT.TC "Char"    IT.Star) _ _ -> Type.i32
  IT.TCon (IT.TC "Integer" IT.Star) _ _ -> Type.i64
  IT.TCon (IT.TC "Boolean" IT.Star) _ _ -> Type.i1
  IT.TVar _ | IT.hasNumberPred ps       -> Type.i64
  _                                     -> boxType


-- Symbol helper (used by buildLLVMType')

adtSymbol :: Int -> Int -> Symbol
adtSymbol maxArity constructorCount =
  Symbol (ADTSymbol maxArity constructorCount) (Operand.ConstantOperand (Constant.Null boxType))


-- Constructor helpers

retrieveConstructorStructType :: Env -> SymbolTable -> IT.Type -> Type.Type
retrieveConstructorStructType _ symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol 0 _) _) ->
          Type.i64

        Just (Symbol (ADTSymbol maxArity ctorCount) _)
          | ctorCount == 1 && maxArity == 1 ->
            -- Newtype: single constructor, single field — erased to boxType
            boxType
          | ctorCount == 1 ->
            -- Single-constructor: no tag field needed
            Type.ptr $ Type.StructureType False (List.replicate maxArity boxType)
          | otherwise ->
            Type.ptr $ Type.StructureType False (Type.i64 : List.replicate maxArity boxType)

        _ ->
          boxType

retrieveConstructorMaxArity :: SymbolTable -> IT.Type -> Int
retrieveConstructorMaxArity symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol maxArity _) _) ->
          maxArity

        _ ->
          1


-- | Check if an ADT type is an enum (all constructors have zero fields).
isEnumADT :: SymbolTable -> IT.Type -> Bool
isEnumADT symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol 0 _) _) -> True
        _                               -> False


-- | Check if an ADT is a newtype (single constructor, single field).
isNewtypeADT :: SymbolTable -> IT.Type -> Bool
isNewtypeADT symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol 1 1) _) -> True
        _                               -> False

-- | Check if an ADT has a single constructor (with 2+ fields).
isSingleConstructorADT :: SymbolTable -> IT.Type -> Bool
isSingleConstructorADT symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol maxArity 1) _) -> maxArity >= 2
        _                                      -> False


-- Internal helpers

-- | Check if a type is a tuple type by examining its constructor name.
isTupleType :: IT.Type -> Bool
isTupleType t = case getTupleTConName t of
  Just name -> case tupleArity name of
    Just _ -> True
    Nothing -> False
  Nothing -> False

-- | Get the arity of a tuple type.
tupleArityFromType :: IT.Type -> Int
tupleArityFromType t = case getTupleTConName t of
  Just name -> case tupleArity name of
    Just n -> n
    Nothing -> 2  -- fallback
  Nothing -> 2  -- fallback

-- | Extract the constructor name from nested TApp to find tuple constructors.
getTupleTConName :: IT.Type -> Maybe String
getTupleTConName (IT.TApp l _) = getTupleTConName l
getTupleTConName (IT.TCon (IT.TC name _) "prelude" _) = Just name
getTupleTConName _ = Nothing

-- | Extract the element types from a tuple type.
-- For #[a, b, c], returns [a, b, c] in left-to-right order.
getTupleElemTypes :: IT.Type -> [IT.Qual IT.Type]
getTupleElemTypes t = reverse (go t)
  where
    go (IT.TApp l r) = ([] IT.:=> r) : go l
    go _             = []


-- Record helpers

-- | Build a flat struct type for a record with known fields.
-- Fields are ordered alphabetically by name (Map.keys ordering).
-- Primitive fields use their native LLVM type (i64, double, etc.); others use boxType.
flatRecordType :: IT.Type -> Type.Type
flatRecordType (IT.TRecord fields _ optionalFields) =
  let allFields  = Map.union fields optionalFields
      fieldTypes = Map.elems allFields
  in  if null fieldTypes then recordType
      else Type.ptr $ Type.StructureType False (primitiveTupleFieldType . ([] IT.:=>) <$> fieldTypes)
flatRecordType _ = recordType

-- | Ordered list of LLVM field types for a record type (same ordering as Map.keys).
-- Used when constructing or accessing record fields with native primitive types.
recordFieldLLVMTypes :: IT.Type -> [Type.Type]
recordFieldLLVMTypes (IT.TRecord fields _ optionalFields) =
  (primitiveTupleFieldType . ([] IT.:=>)) <$> Map.elems (Map.union fields optionalFields)
recordFieldLLVMTypes _ = []

-- | Get the index of a field in a flat record struct.
-- Fields are ordered alphabetically by name (Map.keys ordering).
recordFieldIndex :: String -> IT.Type -> Integer
recordFieldIndex fieldName (IT.TRecord fields _ optionalFields) =
  let allFields = Map.union fields optionalFields
  in  case List.elemIndex fieldName (Map.keys allFields) of
        Just i  -> fromIntegral i
        Nothing -> error $ "Record field '" <> fieldName <> "' not found in record type"
recordFieldIndex fieldName _ = error $ "recordFieldIndex called on non-record type for field '" <> fieldName <> "'"
