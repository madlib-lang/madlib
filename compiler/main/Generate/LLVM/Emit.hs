{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.LLVM.Emit
  ( -- * Call kinds and positions
    CallKind(..)
  , CallPosition(..)
    -- * Emitting calls
  , emitCall
  , emitCallVoid
    -- * Emitting functions
  , emitFunction
  , emitExternVarArgs
  , emitExternWithAttributes
    -- * Memory operations
  , emitLoad
  , emitStore
  , emitGEP
  , emitGEPInbounds
  , emitAlloca
    -- * Cast operations
  , emitBitcast
  , emitAddrSpaceCast
  , emitSafeBitcast
  , emitPtrToInt
  , emitIntToPtr
    -- * Alignment
  , naturalAlignment
    -- * Re-exports for backward compat
  , storeWithMetadata
  ) where

import           GHC.Stack (HasCallStack)
import           Data.ByteString.Short (ShortByteString)
import           Data.Word (Word32)
import           Control.Monad (forM)

import           LLVM.AST (MDRef, MDNode, Instruction(..), Type(..), Operand(..), Name, Definition(..), Parameter(..), mkName)
import qualified LLVM.AST                      as AST
import           LLVM.AST.Type                 (void, ptr)
import qualified LLVM.AST.Type                 as Type
import           LLVM.AST.AddrSpace            (AddrSpace(..))
import qualified LLVM.AST.Constant             as Constant
import qualified LLVM.AST.CallingConvention     as CC
import qualified LLVM.AST.Global               as Global
import           LLVM.AST.Attribute            (ParameterAttribute, FunctionAttribute(..))
import qualified LLVM.AST.FunctionAttribute     as FunctionAttribute
import           LLVM.AST.Linkage              (Linkage(..))
import           LLVM.IRBuilder                (MonadIRBuilder, MonadModuleBuilder, emitInstrVoid, emitInstr, emitDefn,
                                                ParameterName(..), IRBuilderT, named, fresh, runIRBuilderT, emptyIRBuilder)
import qualified LLVM.IRBuilder.Instruction     as Instruction

import           Generate.LLVM.Operand         (TypedOperand(..), returnTypeOf, raw)


-- | Whether a call targets an internal madlib function or an external/FFI function.
data CallKind
  = InternalCall    -- ^ Uses fastcc, nounwind
  | ExternalCall    -- ^ Uses C calling convention
  deriving (Eq, Show)


-- | Whether a call is in tail position.
data CallPosition
  = TailPosition
  | NonTailPosition
  deriving (Eq, Show)


callingConventionFor :: CallKind -> CC.CallingConvention
callingConventionFor InternalCall = CC.Fast
callingConventionFor ExternalCall = CC.C


tailCallKindFor :: CallPosition -> Maybe AST.TailCallKind
tailCallKindFor TailPosition    = Just AST.Tail
tailCallKindFor NonTailPosition = Nothing


-- | Emit a function call. Reads the return type from the function's TypedOperand
-- rather than calling typeOf. Applies calling convention based on CallKind.
emitCall :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m)
         => CallKind
         -> CallPosition
         -> [(ShortByteString, MDRef MDNode)]  -- ^ Debug metadata
         -> TypedOperand                        -- ^ Function to call
         -> [(Operand, [ParameterAttribute])]   -- ^ Arguments
         -> m Operand
emitCall kind pos metadata fn args = do
  let retTy = returnTypeOf fn
      instr = Call
        { tailCallKind       = tailCallKindFor pos
        , callingConvention  = callingConventionFor kind
        , returnAttributes   = []
        , function           = Right (raw fn)
        , arguments          = args
        , functionAttributes = []
        , metadata           = metadata
        }
  case retTy of
    VoidType -> emitInstrVoid instr >> pure (ConstantOperand (Constant.Undef void))
    _        -> emitInstr retTy instr


-- | Emit a void function call (convenience wrapper).
emitCallVoid :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m)
             => CallKind
             -> CallPosition
             -> [(ShortByteString, MDRef MDNode)]
             -> TypedOperand
             -> [(Operand, [ParameterAttribute])]
             -> m ()
emitCallVoid kind pos metadata fn args = do
  _ <- emitCall kind pos metadata fn args
  return ()


-- | Emit a function definition with nounwind and fastcc for internal functions.
emitFunction
  :: MonadModuleBuilder m
  => [(ShortByteString, MDRef MDNode)]  -- ^ Debug metadata
  -> Name                                -- ^ Function name
  -> [(Type, ParameterName)]             -- ^ Parameter types and name hints
  -> Type                                -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())      -- ^ Body builder
  -> m Operand
emitFunction metadata label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition Global.functionDefaults
      { Global.name                = label
      , Global.parameters          = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , Global.returnType          = retty
      , Global.basicBlocks         = blocks
      , Global.callingConvention   = CC.Fast
      , Global.functionAttributes  = [Right NoUnwind]
      , Global.metadata            = metadata
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ Constant.GlobalReference funty label


-- | Declare an external varargs function.
emitExternVarArgs :: MonadModuleBuilder m => Name -> [Type] -> Type -> m Operand
emitExternVarArgs nm argtys retty = do
  emitDefn $ GlobalDefinition Global.functionDefaults
    { Global.name        = nm
    , Global.parameters  = ([Parameter ty (mkName "") [] | ty <- argtys], True)
    , Global.returnType  = retty
    }
  let funty = ptr $ FunctionType retty argtys True
  pure $ ConstantOperand $ Constant.GlobalReference funty nm


-- | Declare an external function with specific attributes.
emitExternWithAttributes :: MonadModuleBuilder m
                         => [FunctionAttribute]
                         -> Name -> [Type] -> Type -> m Operand
emitExternWithAttributes attrs nm argtys retty = do
  emitDefn $ GlobalDefinition Global.functionDefaults
    { Global.name                = nm
    , Global.parameters          = ([Parameter ty (mkName "") [] | ty <- argtys], False)
    , Global.returnType          = retty
    , Global.functionAttributes  = Right <$> attrs
    }
  let funty = ptr $ FunctionType retty argtys False
  pure $ ConstantOperand $ Constant.GlobalReference funty nm


-- | Compute natural alignment for a type (in bytes).
naturalAlignment :: Type -> Word32
naturalAlignment (IntegerType 1)              = 1
naturalAlignment (IntegerType 8)              = 1
naturalAlignment (IntegerType 32)             = 4
naturalAlignment (IntegerType 64)             = 8
naturalAlignment (FloatingPointType Type.DoubleFP) = 8
naturalAlignment (FloatingPointType Type.FloatFP)  = 4
naturalAlignment (PointerType _ _)            = 8
naturalAlignment (StructureType _ _)          = 8
naturalAlignment _                            = 8


-- | Load with natural alignment.
emitLoad :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
emitLoad ptr = Instruction.load ptr 0


-- | Store with natural alignment.
emitStore :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Operand -> m ()
emitStore addr val = Instruction.store addr 0 val


-- | GEP with inbounds.
emitGEPInbounds :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
emitGEPInbounds addr indices = Instruction.gep addr indices


-- | GEP (alias for backward compat).
emitGEP :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
emitGEP = emitGEPInbounds


-- | Alloca instruction.
emitAlloca :: (MonadIRBuilder m, MonadModuleBuilder m) => Type -> m Operand
emitAlloca ty = Instruction.alloca ty Nothing 0


-- | Bitcast.
emitBitcast :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> Type -> m Operand
emitBitcast = Instruction.bitcast


-- | Address space cast.
emitAddrSpaceCast :: MonadIRBuilder m => Operand -> Type -> m Operand
emitAddrSpaceCast op t = emitInstr t $ AddrSpaceCast op t []


-- | Safe bitcast: uses addrspacecast when address spaces differ, bitcast otherwise.
emitSafeBitcast :: MonadIRBuilder m => Operand -> Type -> m Operand
emitSafeBitcast op t = case (getTypeOf op, t) of
  (PointerType _ (AddrSpace l), PointerType _ (AddrSpace r)) | r /= l ->
    emitAddrSpaceCast op t
  _ ->
    Instruction.bitcast op t
  where
    -- Minimal type recovery for pointer address space check only
    getTypeOf (LocalReference ty _) = ty
    getTypeOf (ConstantOperand c) = constType c
    getTypeOf _ = VoidType

    constType (Constant.GlobalReference ty _) = ty
    constType (Constant.Null ty) = ty
    constType (Constant.BitCast _ ty) = ty
    constType (Constant.IntToPtr _ ty) = ty
    constType (Constant.GetElementPtr _ addr _) = getGEPType (constType addr)
    constType _ = VoidType

    getGEPType (PointerType t _) = t
    getGEPType t = t


-- | PtrToInt.
emitPtrToInt :: MonadIRBuilder m => Operand -> Type -> m Operand
emitPtrToInt = Instruction.ptrtoint


-- | IntToPtr.
emitIntToPtr :: MonadIRBuilder m => Operand -> Type -> m Operand
emitIntToPtr = Instruction.inttoptr


-- | Store with debug metadata (backward compat, re-exported).
storeWithMetadata :: MonadIRBuilder m => [(ShortByteString, MDRef MDNode)] -> Operand -> Word32 -> Operand -> m ()
storeWithMetadata metadata addr align val = emitInstrVoid $ Store False addr val Nothing align metadata
