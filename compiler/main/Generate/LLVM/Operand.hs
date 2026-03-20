{-# LANGUAGE OverloadedStrings #-}
module Generate.LLVM.Operand where

import           LLVM.AST            (mkName)
import qualified LLVM.AST.Type       as Type
import qualified LLVM.AST.Operand    as Operand
import qualified LLVM.AST.Constant   as Constant
import           LLVM.AST.AddrSpace  (AddrSpace(..))


-- | A value paired with its known LLVM type, eliminating the need for typeOf calls.
data TypedOperand = TypedOperand
  { tType    :: !Type.Type
  , tOperand :: !Operand.Operand
  } deriving (Show)


-- | Wrap a constant with its type.
typedConst :: Type.Type -> Constant.Constant -> TypedOperand
typedConst ty c = TypedOperand ty (Operand.ConstantOperand c)


-- | Wrap a local reference.
typedLocal :: Type.Type -> Operand.Operand -> TypedOperand
typedLocal = TypedOperand


-- | Create a typed null pointer.
typedNull :: Type.Type -> TypedOperand
typedNull ty = typedConst ty (Constant.Null ty)


-- | Extract the raw LLVM operand.
raw :: TypedOperand -> Operand.Operand
raw = tOperand


-- | Create a typed global reference for a function.
typedGlobalFn :: Type.Type -> [Type.Type] -> Bool -> String -> TypedOperand
typedGlobalFn retTy argTys isVarArg name =
  let fnTy  = Type.FunctionType retTy argTys isVarArg
      ptrTy = Type.PointerType fnTy (AddrSpace 0)
  in  TypedOperand ptrTy (Operand.ConstantOperand (Constant.GlobalReference ptrTy (mkName name)))


-- | Get the return type from a function-typed operand.
returnTypeOf :: TypedOperand -> Type.Type
returnTypeOf (TypedOperand (Type.PointerType (Type.FunctionType r _ _) _) _) = r
returnTypeOf (TypedOperand (Type.FunctionType r _ _) _) = r
returnTypeOf t = error $ "returnTypeOf: not a function type: " ++ show (tType t)
