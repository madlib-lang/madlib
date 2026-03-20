{-# LANGUAGE OverloadedStrings #-}
module Generate.LLVM.Operators
  ( -- * Arithmetic
    generateAdd
  , generateSub
  , generateMul
  , generateDiv
  , generateMod
  , generateUnaryMinus
    -- * Bitwise
  , generateBitwiseNot
    -- * Comparison
  , generateEq
  , generateGt
  , generateLt
  , generateGte
  , generateLte
    -- * Concatenation
  , generateStrConcat
  , generateListConcat
  ) where

import           LLVM.AST                           (Operand)
import qualified LLVM.AST.Type                      as Type
import qualified LLVM.AST.Constant                  as Constant
import qualified LLVM.AST.Operand                   as Operand
import qualified LLVM.AST.IntegerPredicate          as IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate    as FloatingPointPredicate

import           LLVM.IRBuilder.Monad               (MonadIRBuilder)
import           LLVM.IRBuilder.Module              (MonadModuleBuilder)
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Constant            as C

import qualified Infer.Type                         as IT
import           Generate.LLVM.Builtins             (i8ConstOp, i32ConstOp, i64ConstOp, areStringsEqual, strConcat, madlistConcat)


-- | Helper: is Float type?
isFloat :: IT.Type -> Bool
isFloat (IT.TCon (IT.TC "Float" IT.Star) "prelude" _) = True
isFloat _ = False

-- | Helper: is Integer type?
isInteger :: IT.Type -> Bool
isInteger (IT.TCon (IT.TC "Integer" IT.Star) "prelude" _) = True
isInteger _ = False

-- | Helper: is Short type?
isShort :: IT.Type -> Bool
isShort (IT.TCon (IT.TC "Short" IT.Star) "prelude" _) = True
isShort _ = False

-- | Helper: is Byte type?
isByte :: IT.Type -> Bool
isByte (IT.TCon (IT.TC "Byte" IT.Star) "prelude" _) = True
isByte _ = False


-- Arithmetic operators

generateAdd :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateAdd t l r
  | isFloat t   = fadd l r
  | otherwise   = add l r

generateSub :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateSub t l r
  | isFloat t   = fsub l r
  | otherwise   = sub l r

generateMul :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateMul t l r
  | isFloat t   = fmul l r
  | otherwise   = mul l r

generateDiv :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateDiv t l r
  | isFloat t   = fdiv l r
  | isInteger t = do
      l' <- sitofp l Type.double
      r' <- sitofp r Type.double
      fdiv l' r'
  | isShort t   = do
      l' <- sitofp l Type.double
      r' <- sitofp r Type.double
      fdiv l' r'
  | isByte t    = do
      l' <- uitofp l Type.double
      r' <- uitofp r Type.double
      fdiv l' r'
  | otherwise   = fdiv l r  -- fallback

generateMod :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateMod t l r
  | isFloat t   = frem l r
  | isByte t    = urem l r
  | otherwise   = srem l r

generateUnaryMinus :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> m Operand
generateUnaryMinus t op
  | isFloat t   = fmul op (C.double (-1))
  | isInteger t = mul op (i64ConstOp (-1))
  | isShort t   = mul op (C.int32 (-1))
  | isByte t    = mul op (C.int8 (-1))
  | otherwise   = error $ "generateUnaryMinus: unsupported type"


-- Bitwise not

generateBitwiseNot :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> m Operand
generateBitwiseNot t op
  | isInteger t = xor op (i64ConstOp (-1))
  | isShort t   = xor op (i32ConstOp (-1))
  | isByte t    = xor op (i8ConstOp (-1))
  | otherwise   = error $ "generateBitwiseNot: unsupported type"


-- Comparison operators

generateEq :: (MonadIRBuilder m, MonadModuleBuilder m)
           => (Operand -> [(Operand, [a])] -> m Operand)
           -> IT.Type -> Operand -> Operand -> m Operand
generateEq callFn t l r = case t of
  IT.TCon (IT.TC "String" IT.Star) "prelude" _ ->
    callFn areStringsEqual [(l, []), (r, [])]
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    fcmp FloatingPointPredicate.OEQ l r
  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    return $ Operand.ConstantOperand $ Constant.Int 1 1
  _ ->
    icmp IntegerPredicate.EQ l r

generateGt :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateGt t l r = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    fcmp FloatingPointPredicate.OGT l r
  IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SGT l r
  IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SGT l r
  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    return $ Operand.ConstantOperand $ Constant.Int 1 0
  _ ->
    icmp IntegerPredicate.UGT l r  -- Byte, Boolean, Char

generateLt :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateLt t l r = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    fcmp FloatingPointPredicate.OLT l r
  IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SLT l r
  IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SLT l r
  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    return $ Operand.ConstantOperand $ Constant.Int 1 0
  _ ->
    icmp IntegerPredicate.ULT l r

generateGte :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateGte t l r = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    fcmp FloatingPointPredicate.OGE l r
  IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SGE l r
  IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SGE l r
  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    return $ Operand.ConstantOperand $ Constant.Int 1 1
  _ ->
    icmp IntegerPredicate.UGE l r

generateLte :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> Operand -> m Operand
generateLte t l r = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
    fcmp FloatingPointPredicate.OLE l r
  IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SLE l r
  IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
    icmp IntegerPredicate.SLE l r
  IT.TCon (IT.TC "{}" IT.Star) "prelude" _ ->
    return $ Operand.ConstantOperand $ Constant.Int 1 1
  _ ->
    icmp IntegerPredicate.ULE l r


-- String and List concatenation

generateStrConcat :: (MonadIRBuilder m, MonadModuleBuilder m)
                  => (Operand -> [(Operand, [a])] -> m Operand)
                  -> Operand -> Operand -> m Operand
generateStrConcat callFn l r =
  callFn strConcat [(l, []), (r, [])]

generateListConcat :: (MonadIRBuilder m, MonadModuleBuilder m)
                   => (Operand -> [(Operand, [a])] -> m Operand)
                   -> Operand -> Operand -> m Operand
generateListConcat callFn l r =
  callFn madlistConcat [(l, []), (r, [])]

