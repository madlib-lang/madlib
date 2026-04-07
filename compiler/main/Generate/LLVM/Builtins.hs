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
  , gcDisable
  , gcEnable
    -- * Perceus RC runtime function references
  , rcAlloc
  , rcInc
  , rcDec
  , rcDecWithDrop
  , rcIsUnique
  , rcReuse
  , applyPAP
  , applyPAP1
  , applyPAP2
  , applyPAP3
  , applyPAP4
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
    -- * Allocation helpers
  , isAtomicType
  , isRCType
  , chooseMalloc
  , chooseRCAlloc
  ) where

import           LLVM.AST                     (mkName, Operand)
import qualified LLVM.AST.Type                as Type
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand

import           LLVM.IRBuilder.Constant      as C

import           Generate.LLVM.Types          (boxType, listType, stringType, recordType)
import qualified Infer.Type                   as IT


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

gcDisable :: Operand
gcDisable =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (mkName "GC_disable"))

gcEnable :: Operand
gcEnable =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (mkName "GC_enable"))


-- ---------------------------------------------------------------------------
-- Perceus RC runtime function references
-- ---------------------------------------------------------------------------

-- | rc_alloc(i64 size) -> i8*
-- Allocates `size` payload bytes with refcount=1 prepended as an 8-byte header.
rcAlloc :: Operand
rcAlloc =
  Operand.ConstantOperand (Constant.GlobalReference
    (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False)
    (mkName "rc_alloc"))

-- | rc_inc(i8* ptr) -> void
-- Increments the refcount of ptr (no-op on NULL or immortal values).
rcInc :: Operand
rcInc =
  Operand.ConstantOperand (Constant.GlobalReference
    (Type.ptr $ Type.FunctionType Type.void [Type.ptr Type.i8] False)
    (mkName "rc_inc"))

-- | rc_dec(i8* ptr) -> void
-- Decrements the refcount; frees when it reaches 0 (no child cleanup).
rcDec :: Operand
rcDec =
  Operand.ConstantOperand (Constant.GlobalReference
    (Type.ptr $ Type.FunctionType Type.void [Type.ptr Type.i8] False)
    (mkName "rc_dec"))

-- | rc_dec_with_drop(i8* ptr, void(*)(i8*) drop) -> void
-- Decrements the refcount; calls drop(ptr) before freeing to release children.
rcDecWithDrop :: Operand
rcDecWithDrop =
  Operand.ConstantOperand (Constant.GlobalReference
    (Type.ptr $ Type.FunctionType Type.void
      [ Type.ptr Type.i8
      , Type.ptr (Type.FunctionType Type.void [Type.ptr Type.i8] False)
      ] False)
    (mkName "rc_dec_with_drop"))

-- | rc_is_unique(i8* ptr) -> i1
-- Returns 1 if ptr has refcount == 1 (uniquely owned).
rcIsUnique :: Operand
rcIsUnique =
  Operand.ConstantOperand (Constant.GlobalReference
    (Type.ptr $ Type.FunctionType Type.i1 [Type.ptr Type.i8] False)
    (mkName "rc_is_unique"))

-- | rc_reuse(i8* ptr, i64 new_size) -> i8*
-- Returns ptr if it is uniquely owned and large enough for new_size bytes;
-- otherwise allocates fresh.  Used for FBIP in-place reuse.
rcReuse :: Operand
rcReuse =
  Operand.ConstantOperand (Constant.GlobalReference
    (Type.ptr $ Type.FunctionType (Type.ptr Type.i8)
      [Type.ptr Type.i8, Type.i64] False)
    (mkName "rc_reuse"))


applyPAP :: Operand
applyPAP =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.ptr Type.i8, Type.i32] True) (mkName "__applyPAP__"))

applyPAP1 :: Operand
applyPAP1 =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType] False) (mkName "__applyPAP1__"))

applyPAP2 :: Operand
applyPAP2 =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType, boxType] False) (mkName "__applyPAP2__"))

applyPAP3 :: Operand
applyPAP3 =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType, boxType, boxType] False) (mkName "__applyPAP3__"))

applyPAP4 :: Operand
applyPAP4 =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType, boxType, boxType, boxType] False) (mkName "__applyPAP4__"))

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


-- Allocation helpers

-- | Check if a Madlib type is atomic (contains no GC-scannable pointers).
-- Atomic types can use GC_malloc_atomic for allocation, reducing GC scan work.
isAtomicType :: IT.Type -> Bool
isAtomicType t = case t of
  IT.TCon (IT.TC "Integer" _) _ _ -> True
  IT.TCon (IT.TC "Float" _) _ _   -> True
  IT.TCon (IT.TC "Boolean" _) _ _ -> True
  IT.TCon (IT.TC "Byte" _) _ _    -> True
  IT.TCon (IT.TC "Short" _) _ _   -> True
  IT.TCon (IT.TC "Char" _) _ _    -> True
  IT.TCon (IT.TC "Unit" _) _ _    -> True
  _ -> False

-- | Check if a Madlib type is heap-allocated and RC-managed.
-- Returns False for primitive types that are boxed via inttoptr/ptrtoint
-- (Integer, Float, Boolean, Byte, Short, Char, Unit).
-- Returns True for types that are genuinely heap-allocated (Strings, Lists,
-- ADTs with fields, Records, Tuples, Functions/PAPs, Arrays, ByteArrays).
--
-- IMPORTANT: Enum ADTs (all zero-arity constructors) are represented as
-- plain i64 tag values — not heap pointers — and must also return False.
-- That check requires symbol table information and is done at call sites.
isRCType :: IT.Type -> Bool
isRCType = not . isAtomicType

-- | Choose GC_malloc or GC_malloc_atomic based on whether all field types are atomic.
chooseMalloc :: [IT.Type] -> Operand
chooseMalloc fieldTypes
  | all isAtomicType fieldTypes = gcMallocAtomic
  | otherwise = gcMalloc

-- | Choose rc_alloc for all field types (no GC distinction needed under RC).
chooseRCAlloc :: [IT.Type] -> Operand
chooseRCAlloc _ = rcAlloc
