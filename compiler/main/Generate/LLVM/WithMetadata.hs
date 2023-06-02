{-# LANGUAGE DisambiguateRecordFields #-}
module Generate.LLVM.WithMetadata where
import GHC.Stack (HasCallStack)
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder, emitInstrVoid, emitInstr, ParameterName (NoParameterName, ParameterName), IRBuilderT, named, fresh, runIRBuilderT, emptyIRBuilder, emitDefn)
import Data.ByteString.Short (ShortByteString)
import LLVM.AST (MDRef, MDNode, Instruction (Call, tailCallKind, callingConvention, returnAttributes, function, arguments, functionAttributes, metadata), Type (FunctionType, VoidType, PointerType), Operand (ConstantOperand, LocalReference), Name, Definition (GlobalDefinition), Parameter (Parameter))
import LLVM.AST.Operand (Operand)
import LLVM.AST.Attribute (ParameterAttribute)
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Type (void, ptr)
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Typed
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Constant as C
import Control.Monad (forM)


callWithMetadata :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m) => [(ShortByteString, MDRef MDNode)] -> Operand -> [(Operand, [ParameterAttribute])] -> m Operand
callWithMetadata metadata fun args = do
  let instr = Call {
    tailCallKind = Nothing
  , callingConvention = CC.C
  , returnAttributes = []
  , function = Right fun
  , arguments = args
  , functionAttributes = []
  , metadata = metadata
  }
  case typeOf fun of
    (FunctionType r _ _) -> case r of
      VoidType -> emitInstrVoid instr >> pure (ConstantOperand (Constant.Undef void))
      _        -> emitInstr r instr
    (PointerType (FunctionType r _ _) _) -> case r of
      VoidType -> emitInstrVoid instr >> pure (ConstantOperand (Constant.Undef void))
      _        -> emitInstr r instr
    _ -> error "Cannot call non-function (Malformed AST)."

functionWithMetadata
  :: MonadModuleBuilder m
  => [(ShortByteString, MDRef MDNode)]
  -> Name  -- ^ Function name
  -> [(Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
functionWithMetadata metadata label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition Global.functionDefaults
      { Global.name = label
      , Global.parameters = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , Global.returnType = retty
      , Global.basicBlocks = blocks
      , Global.metadata = metadata
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label
