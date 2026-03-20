{-# LANGUAGE OverloadedStrings #-}
module Generate.LLVM.Boxing
  ( box
  , unbox
  ) where

import           LLVM.AST.Type                as Type
import           LLVM.AST.AddrSpace           (AddrSpace(..))
import qualified LLVM.AST.Operand             as Operand

import           LLVM.IRBuilder.Monad         (MonadIRBuilder)
import           LLVM.IRBuilder.Module        (MonadModuleBuilder)
import           LLVM.IRBuilder.Instruction   (ptrtoint, inttoptr, bitcast)

import           Generate.LLVM.TypeOf         (Typed(typeOf))
import           Generate.LLVM.Emit          (emitSafeBitcast)
import           Generate.LLVM.Env
import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Types         (boxType, listType, stringType, papType, recordType, buildLLVMType)
import qualified Infer.Type                   as IT


-- | Unbox a boxed value (i8*) to its native LLVM type based on the Madlib type.
unbox :: (MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Qual IT.Type -> Operand.Operand -> m Operand.Operand
unbox env symbolTable qt@(ps IT.:=> t) what = case t of
  IT.TCon (IT.TC "Float" _) _ _ -> do
    asInt <- ptrtoint what Type.i64
    bitcast asInt Type.double

  IT.TCon (IT.TC "Byte" _) _ _ -> do
    ptrtoint what Type.i8

  IT.TCon (IT.TC "Short" _) _ _ -> do
    ptrtoint what Type.i32

  IT.TCon (IT.TC "Char" _) _ _ -> do
    ptrtoint what Type.i32

  IT.TCon (IT.TC "Integer" _) _ _ -> do
    ptrtoint what Type.i64

  IT.TCon (IT.TC "Boolean" _) _ _ -> do
    ptrtoint what Type.i1

  -- boxed strings are char**
  IT.TCon (IT.TC "String" _) _ _ -> do
    emitSafeBitcast what stringType

  IT.TCon (IT.TC "Unit" _) _ _ -> do
    emitSafeBitcast what $ Type.ptr Type.i1

  -- boxed lists are i8*
  -- unboxed lists are { i8*, i8* }*
  IT.TApp (IT.TCon (IT.TC "List" _) _ _) _ -> do
    emitSafeBitcast what listType

  IT.TRecord{} -> do
    emitSafeBitcast what recordType

  -- This should be called for parameters that are closures or returned closures
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" _) _ _) _) _ ->
    emitSafeBitcast what papType

  IT.TVar _ | IT.hasNumberPred ps -> do
    ptrtoint what Type.i64

  -- That handles tuple types
  _ -> do
    let llvmType = buildLLVMType env symbolTable qt
    emitSafeBitcast what llvmType


-- | Box a native value into a boxed representation (i8*).
-- Uses typeOf to determine the type — this will be replaced with TypedOperand later.
box :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand.Operand -> m Operand.Operand
box what = case typeOf what of
  -- Float: bitcast double -> i64, then inttoptr to box
  Type.FloatingPointType _ -> do
    asInt <- bitcast what Type.i64
    inttoptr asInt boxType

  -- Integer
  Type.IntegerType 64 -> do
    inttoptr what boxType

  -- Char
  Type.IntegerType 32 -> do
    inttoptr what boxType

  -- Byte
  Type.IntegerType 8 -> do
    inttoptr what boxType

  -- Boolean
  Type.IntegerType 1 -> do
    inttoptr what boxType

  -- String
  Type.PointerType (Type.IntegerType 8) (AddrSpace 1) -> do
    emitSafeBitcast what boxType

  -- List
  Type.PointerType (Type.StructureType False [Type.PointerType (Type.IntegerType 8) _, Type.PointerType (Type.IntegerType 8) _]) (AddrSpace 1) -> do
    emitSafeBitcast what boxType

  -- Pointless?
  Type.PointerType (Type.IntegerType 8) _ ->
    return what

  -- Any pointer type
  _ ->
    emitSafeBitcast what boxType
