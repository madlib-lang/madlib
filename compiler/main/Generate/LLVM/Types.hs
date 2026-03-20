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
    -- * Constructor helpers
  , retrieveConstructorStructType
  , retrieveConstructorMaxArity
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
papType = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]

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

  IT.TRecord fields _ optionalFields -> do
    let allFields = Map.union fields optionalFields
    let n = Map.size allFields
    if n > 0 then
      Type.ptr $ Type.StructureType False (List.replicate n boxType)
    else
      recordType

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude" _) _) _ ->
    let arity = List.length $ IT.getParamTypes t
    in  Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False

  _ | isTupleType t ->
    let n = tupleArityFromType t
    in  Type.ptr $ Type.StructureType False (List.replicate n boxType)

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
    let allFields = Map.union fields optionalFields
    let n = Map.size allFields
    if n > 0 then
      return (Type.ptr $ Type.StructureType False (List.replicate n boxType), Nothing)
    else
      return (recordType, Nothing)

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude" _) _) _ -> do
    let arity = List.length $ IT.getParamTypes t
    return (Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False, Nothing)

  _ | isTupleType t ->
    let n = tupleArityFromType t
    in  return (Type.ptr $ Type.StructureType False (List.replicate n boxType), Nothing)

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
          return
            ( Type.ptr $ Type.StructureType False (Type.i64 : List.replicate maxArity boxType)
            , Just (adtTypePath <> "_" <> adtTypeName, adtSymbol maxArity)
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


-- Symbol helper (used by buildLLVMType')

adtSymbol :: Int -> Symbol
adtSymbol maxArity =
  Symbol (ADTSymbol maxArity) (Operand.ConstantOperand (Constant.Null boxType))


-- Constructor helpers

retrieveConstructorStructType :: Env -> SymbolTable -> IT.Type -> Type.Type
retrieveConstructorStructType _ symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol maxArity) _) ->
          Type.ptr $ Type.StructureType False (Type.i64 : List.replicate maxArity boxType)

        _ ->
          boxType

retrieveConstructorMaxArity :: SymbolTable -> IT.Type -> Int
retrieveConstructorMaxArity symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "_" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol maxArity) _) ->
          maxArity

        _ ->
          1


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


-- Record helpers

-- | Build a flat struct type for a record with known fields.
-- Fields are ordered alphabetically by name (Map.keys ordering).
flatRecordType :: IT.Type -> Type.Type
flatRecordType (IT.TRecord fields _ optionalFields) =
  let allFields = Map.union fields optionalFields
      n = Map.size allFields
  in  Type.ptr $ Type.StructureType False (List.replicate n boxType)
flatRecordType _ = recordType

-- | Get the index of a field in a flat record struct.
-- Fields are ordered alphabetically by name (Map.keys ordering).
recordFieldIndex :: String -> IT.Type -> Integer
recordFieldIndex fieldName (IT.TRecord fields _ optionalFields) =
  let allFields = Map.union fields optionalFields
  in  fromIntegral $ Maybe.fromMaybe 0 (List.elemIndex fieldName (Map.keys allFields))
recordFieldIndex _ _ = 0
