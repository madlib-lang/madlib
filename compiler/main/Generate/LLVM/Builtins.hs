{-# LANGUAGE OverloadedStrings #-}
module Generate.LLVM.Builtins
  ( -- * Constant operand helpers
    i8ConstOp
  , i32ConstOp
  , i64ConstOp
  , doubleConstOp
    -- * Boolean constant
  , true
    -- * Runtime function references
  , mainInit
  , initExtra
  , typedHoleReached
  , arrayOutOfBounds
  , initEventLoop
  , startEventLoop
  , gcMalloc
  , gcMallocAtomic
  , applyPAP
  , applyPAP1
  , applyPAP2
  , buildRecord
  , selectField
  , madlistHasMinLength
  , madlistHasLength
  , madlistSingleton
  , madlistPush
  , madlistConcat
  , areStringsEqual
  , areStringsNotEqual
  , strConcat
  ) where

import           LLVM.AST                     (mkName, Operand)
import qualified LLVM.AST.Type                as Type
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand

import           LLVM.IRBuilder.Constant      as C

import           Generate.LLVM.Types          (boxType, listType, stringType, recordType)


-- Constant operand helpers

i8ConstOp :: Integer -> Operand
i8ConstOp i = Operand.ConstantOperand $ Constant.Int 8 i

i32ConstOp :: Integer -> Operand
i32ConstOp i = Operand.ConstantOperand $ Constant.Int 32 i

i64ConstOp :: Integer -> Operand
i64ConstOp i = Operand.ConstantOperand $ Constant.Int 64 i

doubleConstOp :: Double -> Operand
doubleConstOp i = C.double i


-- Boolean constant

true :: Operand
true = Operand.ConstantOperand (Constant.Int 1 1)


-- Runtime function references

mainInit :: Operand
mainInit =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [Type.i32, Type.ptr (Type.ptr Type.i8)] False) (mkName "__main__init__"))

initExtra :: Operand
initExtra =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (mkName "madlib__process__internal__initExtra"))

typedHoleReached :: Operand
typedHoleReached =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (mkName "madlib__process__internal__typedHoleReached"))

arrayOutOfBounds :: Operand
arrayOutOfBounds =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [Type.i64, Type.i64] False) (mkName "madlib__process__internal__arrayOutOfBounds"))

initEventLoop :: Operand
initEventLoop =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (mkName "__initEventLoop__"))

startEventLoop :: Operand
startEventLoop =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (mkName "__startEventLoop__"))

gcMalloc :: Operand
gcMalloc =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (mkName "GC_malloc"))

gcMallocAtomic :: Operand
gcMallocAtomic =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (mkName "GC_malloc_atomic"))

applyPAP :: Operand
applyPAP =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.ptr Type.i8, Type.i32] True) (mkName "__applyPAP__"))

applyPAP1 :: Operand
applyPAP1 =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType] False) (mkName "__applyPAP1__"))

applyPAP2 :: Operand
applyPAP2 =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType, boxType] False) (mkName "__applyPAP2__"))

buildRecord :: Operand
buildRecord =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType recordType [Type.i32, boxType] True) (mkName "madlib__record__internal__buildRecord"))

selectField :: Operand
selectField =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [stringType, recordType] False) (mkName "madlib__record__internal__selectField"))

madlistHasMinLength :: Operand
madlistHasMinLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.i64, listType] False) (mkName "madlib__list__internal__hasMinLength"))

madlistHasLength :: Operand
madlistHasLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.i64, listType] False) (mkName "madlib__list__internal__hasLength"))

madlistSingleton :: Operand
madlistSingleton =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [Type.ptr Type.i8] False) (mkName "madlib__list__singleton"))

madlistPush :: Operand
madlistPush =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [Type.ptr Type.i8, listType] False) (mkName "madlib__list__internal__push"))

madlistConcat :: Operand
madlistConcat =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [listType, listType] False) (mkName "madlib__list__concat"))

areStringsEqual :: Operand
areStringsEqual =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (mkName "madlib__string__internal__areStringsEqual"))

areStringsNotEqual :: Operand
areStringsNotEqual =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (mkName "madlib__string__internal__areStringsNotEqual"))

strConcat :: Operand
strConcat =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType stringType [stringType, stringType] False) (mkName "madlib__string__internal__concat"))
