{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Eta reduce" #-}
module Generate.LLVM.LLVM where


import qualified Data.Map                     as Map
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as TextEncoding
import           Data.ByteString              as ByteString (ByteString, unpack)
import qualified Control.Monad                as Monad
import qualified Control.Monad.Fix            as MonadFix
import qualified Control.Monad.Writer         as Writer
import qualified Control.Monad.State          as State
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           GHC.Stack                    (HasCallStack)
import           System.Random                (randomIO)
import           Text.Show.Pretty             (ppShow)

import           LLVM.AST                     as AST hiding (function)
import           LLVM.AST.Type                as Type
import           LLVM.AST.AddrSpace           (AddrSpace(..))
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand hiding (Module)
import qualified LLVM.AST.IntegerPredicate    as IntegerPredicate
import qualified LLVM.AST.Linkage             as Linkage
import qualified LLVM.AST.Global              as Global
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Constant      as C
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction   as Instruction hiding (gep)

import           AST.Core                     as Core
import qualified Infer.Type                   as IT
import           Explain.Location
import qualified Driver.Query                 as Query
import qualified Rock
import           Run.Options

import           Generate.LLVM.TypeOf         (Typed(typeOf))
import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import           Generate.LLVM.Emit           (emitGEP)
import           Generate.LLVM.WithMetadata   (functionWithMetadata, callWithMetadata, callFastWithMetadata, callMallocWithMetadata, storeWithMetadata, declareWithAttributes, callWithAttributes)
import           Generate.LLVM.Debug
import           Generate.LLVM.Helper
import           Generate.LLVM.Types          (boxType, listType, stringType, papType, recordType, tConExclude, sizeof', buildLLVMType, buildLLVMType', buildLLVMParamType, tupleFieldLLVMType, primitiveTupleFieldType, retrieveConstructorStructType, retrieveConstructorMaxArity, isNewtypeADT, isSingleConstructorADT, adtSymbol, flatRecordType, recordFieldLLVMTypes, recordFieldIndex)
import           Generate.LLVM.Boxing         (box, unbox)
import           Generate.LLVM.Builtins
import qualified Generate.LLVM.Operators      as Ops
import qualified Generate.LLVM.PatternMatch   as PM
import qualified Generate.LLVM.Function       as Fn
import qualified Generate.LLVM.Module         as Mod

-- | Inbounds GEP — shadows LLVM.IRBuilder.Instruction.gep
gep :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
gep = emitGEP


addrspacecast :: MonadIRBuilder m => Operand -> Type -> m Operand
addrspacecast op t =
  emitInstr t $ AddrSpaceCast op t []

safeBitcast :: MonadIRBuilder m => Operand -> Type -> m Operand
safeBitcast op t = case (typeOf op, t) of
  (Type.PointerType _ (AddrSpace l), Type.PointerType _ (AddrSpace r)) | r /= l ->
    addrspacecast op t

  _ ->
    bitcast op t


varSymbol :: Operand -> Symbol
varSymbol =
  Symbol VariableSymbol

localVarSymbol :: Operand -> Operand -> Symbol
localVarSymbol ptr =
  Symbol (LocalVariableSymbol ptr)

tcoParamSymbol :: Operand -> Operand -> Symbol
tcoParamSymbol ptr =
  Symbol (TCOParamSymbol ptr)

fnSymbol :: Int -> Operand -> Symbol
fnSymbol arity =
  Symbol (FunctionSymbol arity)

topLevelSymbol :: Operand -> Symbol
topLevelSymbol =
  Symbol TopLevelAssignment

-- | Conservative linearity check for in-place TRMC dispatch.
-- Returns True if the expression is provably a temporary (not aliased).
-- Variable references are considered potentially aliased.
-- Conditional expressions (If/Where/Do) are only linear if ALL branches are linear,
-- since any branch could evaluate to an aliased variable.
isLinearListArg :: Core.Exp -> Bool
isLinearListArg exp = case exp of
  Core.Typed _ _ _ (Core.Var _ _)       -> False  -- variable reference, might be aliased
  Core.Typed _ _ _ (Core.If _ t f)      -> isLinearListArg t && isLinearListArg f
  Core.Typed _ _ _ (Core.Where _ iss)   -> all isLinearListArgBranch iss
  Core.Typed _ _ _ (Core.Do exps)       -> not (null exps) && isLinearListArg (last exps)
  _                                     -> True  -- function call result, literal, constructor

isLinearListArgBranch :: Core.Is -> Bool
isLinearListArgBranch is = case is of
  Core.Typed _ _ _ (Core.Is _ body) -> isLinearListArg body
  _                                 -> True

constructorSymbol :: Operand -> Int -> Int -> Symbol
constructorSymbol ctor id arity =
  Symbol (ConstructorSymbol id arity) ctor

storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
storeItem basePtr _ (item, index) = do
  ptr <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  store ptr 0 item
  return ()

-- | True if the LLVM type is a pointer (i.e., a heap-allocated RC-managed value).
-- i64, i1, double etc. are primitives that never have RC headers.
isPointerType :: Type.Type -> Bool
isPointerType (Type.PointerType _ _) = True
isPointerType _                      = False

-- | Emit rc_inc on an operand, only if it has pointer type.
-- Safe to call on any operand — non-pointers are silently ignored.
emitRCInc :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m ()
emitRCInc op = case typeOf op of
  Type.PointerType _ _ -> do
    boxed <- safeBitcast op boxType
    _ <- call rcInc [(boxed, [])]
    return ()
  _ -> return ()

-- | Emit rc_dec on an operand, only if it has pointer type.
emitRCDec :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m ()
emitRCDec op = case typeOf op of
  Type.PointerType _ _ -> do
    boxed <- safeBitcast op boxType
    _ <- call rcDec [(boxed, [])]
    return ()
  _ -> return ()

storeArrayItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
storeArrayItem basePtr _ (item, index) = do
  ptr <- gep basePtr [i32ConstOp index]
  store ptr 0 item
  return ()


-- | Allocate a new list node from the arena (chunked allocation for right-list TCO).
-- If the arena chunk is full, allocates a new chunk with doubled capacity.
-- Returns the new node as an i8* pointer (needs addrspacecast to listType afterwards).
allocateArenaNode :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> Area -> m Operand
allocateArenaNode env area = mdo
  let nodeType = Type.StructureType False [boxType, boxType]
  let Just arenaPtr'    = arenaPtr <$> recursionData env
  let Just arenaIndex'  = arenaIndex <$> recursionData env
  let Just arenaCap'    = arenaCapacity <$> recursionData env

  idx <- load arenaIndex' 0
  cap <- load arenaCap' 0

  needsNewChunk <- icmp IntegerPredicate.UGE idx cap
  condBr needsNewChunk newChunkBlock continueBlock

  newChunkBlock <- block `named` "arena.newchunk"
  -- Grow geometrically, but cap chunk size to avoid giant single allocations.
  doubledCap <- mul cap (i64ConstOp 2)
  capAtLeastHalfMax <- icmp IntegerPredicate.UGE cap (i64ConstOp 131072)
  newCap <- Instruction.select capAtLeastHalfMax (i64ConstOp 262144) doubledCap
  -- Allocate new chunk: newCap * sizeof(Node)
  nodeSize <- Instruction.ptrtoint (Operand.ConstantOperand $ sizeof' nodeType) Type.i64
  chunkBytes <- mul newCap nodeSize
  newArena <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(chunkBytes, [])]
  newArena' <- safeBitcast newArena (Type.ptr nodeType)
  store arenaPtr' 0 newArena'
  store arenaCap' 0 newCap
  store arenaIndex' 0 (i64ConstOp 0)

  br continueBlock

  continueBlock <- block `named` "arena.continue"
  -- Load current arena and index (may have been updated by newChunkBlock)
  currentIdx <- load arenaIndex' 0
  currentArena <- load arenaPtr' 0

  -- GEP into arena at current index
  nodePtr <- gep currentArena [currentIdx]
  -- Cast to i8* for compatibility with existing code
  nodePtr' <- safeBitcast nodePtr (Type.ptr Type.i8)

  -- Increment index
  nextIdx <- add currentIdx (i64ConstOp 1)
  store arenaIndex' 0 nextIdx

  return nodePtr'




-- | Allocate memory for a struct, using stack allocation (alloca) when the
-- expression is marked as StackAllocatable, otherwise using heap allocation.
-- Returns an i8* pointer (same as callMallocWithMetadata).
allocateStruct :: (MonadIRBuilder m, MonadModuleBuilder m) => Env -> Area -> [Core.Metadata] -> Type.Type -> Operand -> m Operand
allocateStruct env area metadata structType mallocFn =
  if Core.isStackAllocatable metadata then do
    ptr <- alloca structType Nothing 0
    safeBitcast ptr (Type.ptr Type.i8)
  else
    callMallocWithMetadata (makeDILocation env area) mallocFn [(Operand.ConstantOperand $ sizeof' structType, [])]


typingStrWithoutHash :: String -> String
typingStrWithoutHash = List.takeWhile (/= '_')


emptyList :: (MonadIO m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> Area -> m Operand
emptyList env area = do
  let listStructType = Type.StructureType False [boxType, boxType]
      emptyInit = Constant.Struct Nothing False [Constant.Null boxType, Constant.Null boxType]

  r <- liftIO randomIO
  nm <- freshName (stringToShortByteString $ "empty_list_" ++ show (r :: Int))

  emitDefn $ GlobalDefinition globalVariableDefaults
    { Global.name        = nm
    , Global.type'       = listStructType
    , Global.linkage     = Linkage.Internal
    , Global.isConstant  = True
    , Global.initializer = Just emptyInit
    , Global.unnamedAddr = Just GlobalAddr
    }

  let globalRef = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr listStructType) nm
  addrspacecast globalRef listType

buildStr :: (MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Env -> Area -> String -> m Operand
buildStr _env _area s = do
  let asText     = Text.pack s
      bs         = TextEncoding.encodeUtf8 asText
      bytes      = ByteString.unpack bs
      charCodes  = (fromEnum <$> bytes) ++ [0]
      charCodes' = toInteger <$> charCodes

  let llvmVals  = fmap (Constant.Int 8) charCodes'
  let char      = IntegerType 8
  let charArray = Constant.Array char llvmVals
  let ty        = typeOf charArray

  r <- randomIO
  nm <- freshName (stringToShortByteString $ show (r :: Int))

  emitDefn $ GlobalDefinition globalVariableDefaults
    { Global.name                  = nm
    , Global.type'                 = ty
    , Global.linkage               = Linkage.External
    , Global.isConstant            = True
    , Global.initializer           = Just charArray
    , Global.unnamedAddr           = Just GlobalAddr
    }

  let op = Operand.ConstantOperand $ Constant.GetElementPtr True
                           (Constant.GlobalReference (ptr ty) nm)
                           [(Constant.Int 32 0), (Constant.Int 32 0)]
  safeBitcast op stringType


-- | Unwrap Do blocks in an expression to get the innermost non-Do expression.
-- This is needed because coverage instrumentation wraps expressions in Do blocks,
-- but retrieveArgs needs to see the original Var/metadata for proper boxing.
unwrapDoExp :: Core.Exp -> Core.Exp
unwrapDoExp exp = case exp of
  Core.Typed _ _ _ (Core.Do exps) | not (List.null exps) ->
    unwrapDoExp (List.last exps)
  _ -> exp


retrieveArgs :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [[Core.Metadata]] -> [Core.Exp] -> [(SymbolTable, Operand, Maybe Operand)] -> m [Operand]
retrieveArgs metadata argExps exps =
  mapM
    (\(metadata, argExp, (st, arg, maybeBoxedArg)) ->
      let innerExp = unwrapDoExp argExp
          effectiveMetadata = Core.getMetadata innerExp
      in case maybeBoxedArg of
      Just boxed | Core.isReferenceArgument metadata || Core.isReferenceArgument effectiveMetadata ->
        return boxed

      _ ->
        -- Check if the argument is a variable with a known boxed form
        case innerExp of
          Core.Typed _ _ _ (Core.Var name _) ->
            case Map.lookup name st of
              Just (Symbol (BoxedVariableSymbol boxed) _) ->
                return boxed
              _ | typeOf arg == boxType ->
                return arg
              _ ->
                box arg

          _ | typeOf arg == boxType ->
            return arg

          _ ->
            box arg
    )
    (List.zip3 metadata argExps exps)


generateApplicationForKnownFunction :: (Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Env -> SymbolTable -> Area -> IT.Qual IT.Type -> Int -> Operand -> [Core.Exp] -> m (SymbolTable, Operand, Maybe Operand)
generateApplicationForKnownFunction env symbolTable area returnQualType arity fnOperand args
  | List.length args == arity = do
      -- We have a known call!
      args'   <- mapM (generateExp env symbolTable) args
      args''  <- retrieveArgs (Core.getMetadata <$> args) args args'
      let args''' = (, []) <$> args''

      ret <- callFastWithMetadata (makeDILocation env area) fnOperand args'''
      unboxed <- unbox env symbolTable returnQualType ret

      return (symbolTable, unboxed, Just ret)
  | List.length args > arity = do
      -- We have extra args so we do the known call and the applyPAP the resulting partial application
      let (args', remainingArgs) = List.splitAt arity args
      args''   <- mapM (generateExp env symbolTable) args'
      args'''  <- retrieveArgs (Core.getMetadata <$> args') args' args''
      let args'''' = (, []) <$> args'''

      pap <- callFastWithMetadata (makeDILocation env area) fnOperand args''''

      let argc = i32ConstOp (fromIntegral $ List.length remainingArgs)
      remainingArgs'  <- mapM (generateExp env symbolTable) remainingArgs
      remainingArgs'' <- retrieveArgs (Core.getMetadata <$> remainingArgs) remainingArgs remainingArgs'
      let remainingArgs''' = (, []) <$> remainingArgs''

      ret <-
        if List.length remainingArgs''' == 1 then
          callWithMetadata (makeDILocation env area) applyPAP1 $ (pap, []) : remainingArgs'''
        else if List.length remainingArgs''' == 2 then
          callWithMetadata (makeDILocation env area) applyPAP2 $ (pap, []) : remainingArgs'''
        else if List.length remainingArgs''' == 3 then
          callWithMetadata (makeDILocation env area) applyPAP3 $ (pap, []) : remainingArgs'''
        else if List.length remainingArgs''' == 4 then
          callWithMetadata (makeDILocation env area) applyPAP4 $ (pap, []) : remainingArgs'''
        else
          callWithMetadata (makeDILocation env area) applyPAP $ [(pap, []), (argc, [])] ++ remainingArgs'''

      unboxed <- unbox env symbolTable returnQualType ret

      return (symbolTable, unboxed, Just ret)
  | otherwise = do
      -- We don't have enough args, so we create a new PAP
      let papStructType           = Type.StructureType False [boxType, Type.i32, Type.i32, boxType, Type.i8]
      let arity'                  = i32ConstOp (fromIntegral arity)
      let argCount                = List.length args
      let amountOfArgsToBeApplied = i32ConstOp (fromIntegral (arity - argCount))
      let envType                 = Type.StructureType False (List.replicate argCount boxType)

      boxedFn  <- box fnOperand

      args'     <- mapM (generateExp env symbolTable) args
      boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args args'

      -- We compute capture atomicity metadata at compile-time. Actual runtime atomic
      -- allocation for PAP envs is guarded by MADLIB_PAP_ATOMIC_ENV and defaults off.
      let hasMutationRef = any (Core.isReferenceArgument . Core.getMetadata) args
      let envArgTypes = (\a -> let (_ IT.:=> at') = Core.getQualType a in at') <$> args
      let hasAtomicEnv = not hasMutationRef && all isAtomicType envArgTypes
      let envIsAtomic = i8ConstOp (if hasAtomicEnv then 1 else 0)
      let envMallocFn = rcAlloc
      envPtr  <- callMallocWithMetadata (makeDILocation env area) envMallocFn [(Operand.ConstantOperand $ sizeof' envType, [])]
      envPtr' <- safeBitcast envPtr (Type.ptr envType)

      Monad.foldM_
        (\_ (boxed, index, argType, unboxed) ->
          case typeOf unboxed of
            Type.PointerType (Type.StructureType _ [Type.PointerType _ _, Type.IntegerType 32, Type.IntegerType 32, Type.PointerType _ _, Type.IntegerType 8]) _ | IT.isFunctionType argType && Maybe.isJust (recursionData env) -> do
              unboxed' <- load unboxed 0
              newPAP <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' papStructType, [])]
              newPAP' <- bitcast newPAP papType
              store newPAP' 0 unboxed'
              storeItem envPtr' () (newPAP, index)

            _ -> do
              -- Perceus: rc_inc each captured heap value entering the PAP env.
              -- Without this, values with refcount=1 that are later rc_dec'd at a
              -- ReferenceStore site will be freed while the PAP env still holds the ptr.
              -- Guard: skip rc_inc for primitive types (Float, Integer, Boolean, etc.)
              -- whose values are encoded as inttoptr pointers, not actual heap pointers.
              Monad.when (isPointerType (typeOf boxed) && not (isAtomicType argType)) $ emitRCInc boxed
              storeItem envPtr' () (boxed, index)
        )
        ()
        $ List.zip4 boxedArgs [0..] (Core.getType <$> args) ((\(_, a, _) -> a) <$> args')

      papPtr  <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' papStructType, [])]
      papPtr' <- safeBitcast papPtr (Type.ptr papStructType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (amountOfArgsToBeApplied, 2), (envPtr, 3), (envIsAtomic, 4)]

      return (symbolTable, papPtr', Just papPtr)


buildReferencePAP :: (MonadIO m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Env -> Area -> Int -> Operand.Operand -> m (SymbolTable, Operand.Operand, Maybe Operand.Operand)
buildReferencePAP symbolTable env area arity fn = do
  -- Emit a global constant PAP instead of runtime GC_MALLOC allocation.
  -- Reference PAPs have no environment (missingArgCount == arity), so they
  -- are identical every time and can be hoisted to a global constant.
  let papStructType = Type.StructureType False [boxType, Type.i32, Type.i32, boxType, Type.i8]

  -- Get the function's constant reference for the global initializer
  let fnConstant = case fn of
        Operand.ConstantOperand c -> Just c
        _ -> Nothing

  case fnConstant of
    Just fnConst -> do
      -- Build a global constant PAP with empty env metadata.
      let fnBitcast = Constant.BitCast fnConst boxType
      let arityConst = Constant.Int 32 (fromIntegral arity)
      let nullEnv = Constant.Null boxType
      let envAtomicConst = Constant.Int 8 0
      let papInit = Constant.Struct Nothing False [fnBitcast, arityConst, arityConst, nullEnv, envAtomicConst]

      r <- liftIO randomIO
      nm <- freshName (stringToShortByteString $ "pap_" ++ show (r :: Int))

      emitDefn $ GlobalDefinition globalVariableDefaults
        { Global.name        = nm
        , Global.type'       = papStructType
        , Global.linkage     = Linkage.Internal
        , Global.isConstant  = False  -- PAP consumers may read fields via GEP
        , Global.initializer = Just papInit
        , Global.unnamedAddr = Just GlobalAddr
        }

      let papRef = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr papStructType) nm
      return (symbolTable, papRef, Just papRef)

    Nothing -> do
      -- Fallback: dynamic PAP allocation for non-constant function operands
      let arity'  = i32ConstOp (fromIntegral arity)
      let envIsAtomic = i8ConstOp 0
      let nullEnv = Operand.ConstantOperand (Constant.Null boxType)
      boxedFn  <- box fn
      papPtr   <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' papStructType, [])]
      papPtr'  <- safeBitcast papPtr (Type.ptr papStructType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2), (nullEnv, 3), (envIsAtomic, 4)]
      return (symbolTable, papPtr', Just papPtr)

-- | Unwrap Do blocks in function position of Call nodes.
-- When coverage or other transformations wrap a function reference in a Do block
-- (e.g., Do [sideEffect, Var "fn"]), the Call handler's pattern matching on the
-- function expression fails, causing operators to fall through to the PAP dispatch
-- path and crash. This function normalizes such calls by hoisting the side-effect
-- expressions out of the function position and into the args (as preceding Do effects).
--
-- Example: Call (Do [tracker, Var "+"]) [x, y]
--       -> the side effects from Do are generated first, then dispatched as Call (Var "+") [x, y]
unwrapCallFnDo :: Core.Exp -> (Core.Exp, [Core.Exp])
unwrapCallFnDo fn = case fn of
  Core.Typed _ _ _ (Core.Do exps) | not (List.null exps) ->
    let sideEffects = List.init exps
        actualFn    = List.last exps
        (innerFn, moreSideEffects) = unwrapCallFnDo actualFn
    in  (innerFn, sideEffects ++ moreSideEffects)
  _ -> (fn, [])


-- | Normalize expressions by hoisting Do-block side effects from sub-expressions.
-- When coverage or other transformations wrap sub-expressions in Do blocks
-- (e.g., Do [sideEffect, actualExpr]), pattern matching and boxing logic in
-- the LLVM codegen can break because it expects to see the actual expression
-- directly, not wrapped in a Do.
--
-- This function normalizes all expression types that have sub-expressions by:
-- 1. Unwrapping Do blocks from sub-expressions
-- 2. Collecting all side-effect expressions
-- 3. Wrapping the cleaned expression in a single Do with all side effects first
--
-- Examples:
--   Call (Do [t1, Var "+"]) [Do [t2, x], y]  ->  Do [t1, t2, Call (Var "+") [x, y]]
--   TupleConstructor [Do [t1, e1], e2]        ->  Do [t1, TupleConstructor [e1, e2]]
normalizeDoWrappers :: Core.Exp -> Core.Exp
normalizeDoWrappers exp = case exp of
  Core.Typed qt area metadata (Core.Call fn args) ->
    let (unwrappedFn, fnSideEffects) = unwrapCallFnDo fn
        (unwrappedArgs, argSideEffects) = unzip $ map unwrapCallFnDo args
        allSideEffects = fnSideEffects ++ concat argSideEffects
    in  if List.null allSideEffects
          then exp
          else Core.Typed qt area metadata (Core.Do (allSideEffects ++ [Core.Typed qt area metadata (Core.Call unwrappedFn unwrappedArgs)]))

  Core.Typed qt area metadata (Core.TupleConstructor exps) ->
    let (unwrappedExps, allEffects) = unzip $ map unwrapCallFnDo exps
        sideEffects = concat allEffects
    in  if List.null sideEffects
          then exp
          else Core.Typed qt area metadata (Core.Do (sideEffects ++ [Core.Typed qt area metadata (Core.TupleConstructor unwrappedExps)]))

  Core.Typed qt area metadata (Core.Record fields) ->
    let (unwrappedFields, sideEffects) = unzip $ map unwrapFieldDo fields
        allSideEffects = concat sideEffects
    in  if List.null allSideEffects
          then exp
          else Core.Typed qt area metadata (Core.Do (allSideEffects ++ [Core.Typed qt area metadata (Core.Record unwrappedFields)]))

  _ -> exp
  where
    unwrapFieldDo :: Core.Field -> (Core.Field, [Core.Exp])
    unwrapFieldDo field = case field of
      Core.Typed fqt fa fm (Core.Field (name, value)) ->
        let (unwrappedValue, effects) = unwrapCallFnDo value
        in  (Core.Typed fqt fa fm (Core.Field (name, unwrappedValue)), effects)
      Core.Typed fqt fa fm (Core.FieldSpread value) ->
        let (unwrappedValue, effects) = unwrapCallFnDo value
        in  (Core.Typed fqt fa fm (Core.FieldSpread unwrappedValue), effects)
      _ -> (field, [])


-- returns a (SymbolTable, Operand, Maybe Operand) where the maybe operand is a possible boxed value when available
generateExp :: (Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Env -> SymbolTable -> Core.Exp -> m (SymbolTable, Operand, Maybe Operand)
generateExp env symbolTable exp = case normalizeDoWrappers exp of
  Core.Typed _ _ metadata (Core.Call (Core.Typed _ _ _ (Var op _)) [(Core.Typed _ _ _ (Core.Call _ recArgs)), arg2])
    | (op == "+" && Core.isLeftAdditionRecursiveCall metadata) || (op == "*" && Core.isLeftMultiplicationRecursiveCall metadata) -> do
      let Just params   = boxedParams <$> recursionData env
      let Just holePtr' = holePtr <$> recursionData env
      let Just continue = continueRef <$> recursionData env

      (_, arg2', _) <- generateExp env symbolTable arg2

      args'  <- mapM (generateExp env symbolTable) recArgs
      let unboxedArgs = (\(_, x, _) -> x) <$> args'

      -- We need to reverse because we may have some closured variables in the params and these need not be updated
      let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> recArgs) (List.reverse params) (List.reverse unboxedArgs)
      mapM_ (\(qt', ptr, exp) -> Fn.updateTCOArg symbolTable qt' ptr exp) paramUpdateData

      holeValue <- load holePtr' 0
      updatedHole <-
        if Core.getType arg2 == IT.tFloat then
          if Core.isLeftMultiplicationRecursiveCall metadata then
            fmul holeValue arg2'
          else
            fadd holeValue arg2'
        else
          if Core.isLeftMultiplicationRecursiveCall metadata then
            mul holeValue arg2'
          else
            add holeValue arg2'
      store holePtr' 0 updatedHole
      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))
      return (symbolTable, updatedHole, Nothing)

  Core.Typed qt@(_ IT.:=> t) area metadata e | Core.isAdditionRecursionEnd metadata || Core.isMultiplicationRecursionEnd metadata -> do
    (_, endValue, _) <- generateExp env symbolTable (Core.Typed qt area [] e)

    let Just holePtr' = holePtr <$> recursionData env
    holeValue <- load holePtr' 0
    updatedHole <-
      if t == IT.tFloat then
        if Core.isMultiplicationRecursionEnd metadata then
          fmul holeValue endValue
        else
          fadd holeValue endValue
      else
        if Core.isMultiplicationRecursionEnd metadata then
          mul holeValue endValue
        else
          add holeValue endValue
    store holePtr' 0 updatedHole
    return (symbolTable, updatedHole, Nothing)

  Core.Typed qt area metadata e | Core.isInPlaceListRecursionEnd metadata -> mdo
    -- In-place end: link the base-case expression to the last processed node's next field.
    -- For map (base case []), this is a no-op (stores null into an already-null next).
    -- For ++ (base case b), this links the second list to the last node.
    -- Special case: if no InPlaceListRecursiveCall ever fired (e.g. filter removed all elements,
    -- or the input list was empty), return the end expression directly.
    (_, endList, _) <- generateExp env symbolTable (Core.Typed qt area [] e)
    let Just startOperand  = start        <$> recursionData env
    let Just prevEndPtr    = prevEnd =<< recursionData env
    let Just anyNodeAdded' = anyNodeAdded <$> recursionData env
    prevEnd'   <- load prevEndPtr 0
    nodeAdded  <- load anyNodeAdded' 0

    condBr nodeAdded linkBlock emptyBlock

    linkBlock <- block `named` "inplace.link"
    endListRaw <- addrspacecast endList boxType
    storeItem prevEnd' () (endListRaw, 1)
    br mergeBlock

    emptyBlock <- block `named` "inplace.empty"
    br mergeBlock

    mergeBlock <- block `named` "inplace.merge"
    result <- phi [(startOperand, linkBlock), (endList, emptyBlock)]
    return (symbolTable, result, Nothing)

  Core.Typed qt area metadata e | Core.isRightListRecursionEnd metadata -> mdo
    -- Generate the base-case expression (e.g. [] or a tail list)
    (_, endList, _) <- generateExp env symbolTable (Core.Typed qt area [] e)

    let Just startOperand  = start        <$> recursionData env
    let Just endPtr        = end          <$> recursionData env
    let Just anyNodeAdded' = anyNodeAdded <$> recursionData env
    end'       <- load endPtr 0
    nodeAdded  <- load anyNodeAdded' 0

    -- If no ListRecursiveCall ever fired (e.g. filter removed all elements),
    -- return the base-case expression directly; the uninitialized arena sentinel
    -- is not a valid list value the runtime would recognise.
    condBr nodeAdded appendBlock emptyBlock

    appendBlock <- block `named` "trmc.append"
    endValue  <- gep endList [i32ConstOp 0, i32ConstOp 0]
    endValue' <- load endValue 0
    endNext   <- gep endList [i32ConstOp 0, i32ConstOp 1]
    endNext'  <- load endNext 0
    storeItem end' () (endValue', 0)
    storeItem end' () (endNext', 1)
    br mergeBlock

    emptyBlock <- block `named` "trmc.empty"
    br mergeBlock

    mergeBlock <- block `named` "trmc.merge"
    result <- phi [(startOperand, appendBlock), (endList, emptyBlock)]
    return (symbolTable, result, Nothing)

  Core.Typed qt area metadata e | Core.isConstructorRecursionEnd metadata -> do
    -- TODO: generate exp without the metadata and append its result to the end
    (_, endValue, _) <- generateExp env symbolTable (Core.Typed qt area [] e)

    let Just startOperand = start <$> recursionData env
    let Just holePtr'     = holePtr <$> recursionData env

    holePtr'' <- load holePtr' 0
    store holePtr'' 0 endValue

    finalValue  <- gep startOperand [i32ConstOp 0, i32ConstOp 1]
    finalValue' <- load finalValue 0

    return (symbolTable, finalValue', Nothing)

  Core.Typed (_ IT.:=> t) area metadata (Core.Call (Core.Typed _ _ _ (Core.Var constructorName True)) args) | Core.isConstructorRecursiveCall metadata -> do
    let constructedType@(Type.PointerType structType _) = retrieveConstructorStructType env symbolTable t
    case getConstructorRecursionInfo metadata of
      Just (ConstructorRecursionInfo _ position) -> do
        -- holePtr' :: i8***
        let Just holePtr'      = holePtr <$> recursionData env
        -- holePtr'' :: i8**
        holePtr'' <- load holePtr' 0

        args' <-
          mapM
            (\(index, arg) ->
              if index == position then do
                return $ Operand.ConstantOperand (Constant.Null constructedType)
              else do
                (_, arg', _) <- generateExp env symbolTable arg
                return arg'
            )
            (List.zip [0..] args)
        args'' <- mapM box args'

        let index = case Map.lookup constructorName symbolTable of
                    Just (Symbol (ConstructorSymbol id _) _) ->
                      id

                    _ ->
                      error $ "Constructor not found in symbol table: " <> constructorName

        constructed     <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' structType, [])]
        constructed'    <- safeBitcast constructed constructedType

        -- store the constructor data in the struct
        let isSingleCtor = isSingleConstructorADT symbolTable t
        if isSingleCtor then
          -- Single-constructor: no tag field, fields start at 0
          Monad.foldM_ (storeItem constructed') () $ List.zip args'' [0..]
        else
          -- Multi-constructor: tag at 0, fields at 1+
          Monad.foldM_ (storeItem constructed') () $ List.zip args'' [1..] ++ [(i64ConstOp (fromIntegral index), 0)]

        store holePtr'' 0 constructed'

        -- newHole :: i8**
        let fieldOffset = if isSingleCtor then position else position + 1
        newHole   <- gep constructed' [i32ConstOp 0, i32ConstOp (fromIntegral fieldOffset)]
        newHole'' <- bitcast newHole (Type.ptr constructedType)
        store holePtr' 0 newHole''

        case args!!position of
          Core.Typed _ area _ (Core.Call _ recArgs) -> do
            let llvmType = buildLLVMType env symbolTable (getQualType exp)
            let Just continue = continueRef <$> recursionData env
            let Just params = boxedParams <$> recursionData env

            storeWithMetadata (makeDILocation env area) continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

            recArgs' <- mapM (generateExp env symbolTable) recArgs
            let unboxedArgs = (\(_, x, _) -> x) <$> recArgs'

            -- We need to reverse because we may have some closured variables in the params and these need not be updated
            let paramUpdatesData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> recArgs) (List.reverse params) (List.reverse unboxedArgs)
            mapM_ (\(qt', ptr, exp) -> Fn.updateTCOArg symbolTable qt' ptr exp) paramUpdatesData

            return (symbolTable, Operand.ConstantOperand (Constant.Undef llvmType), Nothing)

          _ ->
            error "Unreachable: non-recursive call in constructor recursion position"

      Nothing ->
        error "Unreachable: constructor recursion without recursion data"

  Core.Typed (_ IT.:=> t) area varMetadata (Core.Var n _) ->
    case Map.lookup n symbolTable of
      Just (Symbol (FunctionSymbol 0) fnPtr) -> do
        pap <- callFastWithMetadata (makeDILocation env area) fnPtr []
        return (symbolTable, pap, Nothing)

      Just (Symbol (FunctionSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable env area arity fnPtr

      Just (Symbol TopLevelAssignment ptr) -> do
        loaded <- load ptr 0
        return (symbolTable, loaded, Nothing)

      Just (Symbol (LocalVariableSymbol ptr) value) ->
        if Maybe.isJust (recursionData env) then
          return (symbolTable, value, Nothing)
        else do
          ptr' <- safeBitcast ptr (Type.ptr (typeOf value))
          loaded <- load ptr' 0
          return (symbolTable, loaded, Just ptr)

      Just (Symbol (ConstructorSymbol index 0) _) -> do
        let maxArity = retrieveConstructorMaxArity symbolTable t
        if maxArity == 0 then do
          -- Enum ADT: represent as plain i64 tag value
          let tagVal = i64ConstOp (fromIntegral index)
          return (symbolTable, tagVal, Nothing)
        else do
          let structType = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType

          -- Emit a global constant for zero-arg constructors (Nothing, True, False, etc.)
          -- since they are immutable and identical every time.
          let tagConst = Constant.Int 64 (fromIntegral index)
          let nullFields = List.replicate maxArity (Constant.Null boxType)
          let adtInit = Constant.Struct Nothing False (tagConst : nullFields)

          r <- liftIO randomIO
          nm <- freshName (stringToShortByteString $ "ctor_" ++ show index ++ "_" ++ show (r :: Int))

          emitDefn $ GlobalDefinition globalVariableDefaults
            { Global.name        = nm
            , Global.type'       = structType
            , Global.linkage     = Linkage.Internal
            , Global.isConstant  = True
            , Global.initializer = Just adtInit
            , Global.unnamedAddr = Just GlobalAddr
            }

          let ctorRef = Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr structType) nm
          return (symbolTable, ctorRef, Nothing)

      Just (Symbol (ConstructorSymbol _ arity) fnPtr) -> do
        buildReferencePAP symbolTable env area arity fnPtr

      Just (Symbol (TCOParamSymbol ptr) _) -> do
        loaded <- load ptr 0
        return (symbolTable, loaded, Nothing)

      Just (Symbol (BoxedVariableSymbol boxed) var) ->
        return (symbolTable, var, Just boxed)

      Just (Symbol _ var) ->
        return (symbolTable, var, Nothing)

      Nothing ->
        error $ "Var not found " <> n <> "\nExp: " <> ppShow exp

  -- TODO: Export nodes are stripped from closure convertion currently, we'll need this back soon.
  -- Core.Typed _ _ _ (Core.Export e) -> do
  --   generateExp env symbolTable e

  Core.Typed _ _ metadata (Core.Assignment lhs@(Core.Typed _ _ _ (Core.Access r@(Core.Typed (_ IT.:=> recType) _ _ _) (Core.Typed _ _ _ (Core.Var ('.' : fieldName) _)))) e) -> do
    if Core.isReferenceStore metadata then do
        (_, exp', _) <- generateExp env { isTopLevel = False } symbolTable e
        (_, r', _)   <- generateExp env { isTopLevel = False } symbolTable r
        let expQt     = Core.getQualType e
            fieldType = primitiveTupleFieldType expQt

        case recType of
          IT.TRecord fields _ optionalFields -> do
            let allFields   = Map.union fields optionalFields
                bareTypes   = Map.elems allFields
                structType  = Type.StructureType False ((primitiveTupleFieldType . ([] IT.:=>)) <$> bareTypes)
                index       = recordFieldIndex fieldName recType
            storeVal <-
              if fieldType == boxType
                then box exp'
                else if typeOf exp' == fieldType
                       then return exp'
                       else unbox env symbolTable expQt exp'
            recordOperand' <- safeBitcast r' (Type.ptr structType)
            fieldPtr       <- gep recordOperand' [i32ConstOp 0, i32ConstOp index]
            store fieldPtr 0 storeVal
            let unit = Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1)
            return (symbolTable, unit, Nothing)

          or ->
            error $ "found: " <> ppShow or
      else do
        error $ "bad LHS: " <> ppShow lhs

  Core.Typed _ area metadata (Core.Assignment lhs@(Core.Typed _ _ _ (Core.ArrayAccess arr index)) e) -> do
    if Core.isReferenceStore metadata then mdo
      (_, arrOperand, _) <- generateExp env symbolTable arr
      (_, indexOperand, _) <- generateExp env symbolTable index
      (_, exp', _) <- generateExp env { isTopLevel = False } symbolTable e
      exp'' <- box exp'
      let arrayType = Type.ptr $ Type.StructureType False [i64, i64, Type.ptr $ Type.ptr i8]
      arrOperand' <- safeBitcast arrOperand arrayType

      len <- gep arrOperand' [i32ConstOp 0, i32ConstOp 0]
      len' <- load len 0

      negativeIndex <- icmp IntegerPredicate.SLT indexOperand (Operand.ConstantOperand (Constant.Int 64 0))
      tooLarge <- icmp IntegerPredicate.SGE indexOperand len'
      outOfBound <- Instruction.or negativeIndex tooLarge
      condBr outOfBound outOfBoundBlock allGoodBlock

      outOfBoundBlock <- block `named` "outOfBoundBlock"
      callWithMetadata (makeDILocation env area) arrayOutOfBounds [(indexOperand, []), (len', [])]
      br exitBlock

      allGoodBlock <- block `named` "allGoodBlock"
      items <- gep arrOperand' [i32ConstOp 0, i32ConstOp 2]
      items' <- load items 0
      item <- gep items' [indexOperand]
      br exitBlock

      exitBlock <- block `named` "exitBlock"
      valuePtr <- phi [(item, allGoodBlock), (Operand.ConstantOperand $ Constant.Null (Type.ptr $ Type.ptr i8), outOfBoundBlock)]

      store valuePtr 0 exp''
      let unit = Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1)
      return (symbolTable, unit, Nothing)

    else do
      error $ "bad LHS: " <> ppShow lhs


  Core.Typed _ area metadata (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) e) -> do
    if isTopLevel env then do
      (_, exp', _) <- generateExp env { isTopLevel = False } symbolTable e
      let t = typeOf exp'
      g <- global (AST.mkName name) t $ Constant.Undef t
      store g 0 exp'
      Writer.tell $ Map.singleton name (topLevelSymbol g)
      return (Map.insert name (topLevelSymbol g) symbolTable, exp', Nothing)
    else
      if Core.isReferenceAllocation metadata then do
        let expType = buildLLVMType env symbolTable (Core.getQualType exp) --typeOf exp'
            ptrType = Type.ptr expType
        ptr  <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand (sizeof' expType), [])]
        declareVariable env area False name ptr
        ptr' <- safeBitcast ptr ptrType
        v <- load ptr' 0

        (_, exp', _) <-
          generateExp
            env { isTopLevel = False }
            (Map.insert name (localVarSymbol ptr v) symbolTable)
            e

        store ptr' 0 exp'
        return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Just ptr')
      else if Core.isReferenceStore metadata then do
        (_, exp', _) <- generateExp env { isTopLevel = False } symbolTable e
        let expMadlibType = Core.getType e
            isHeapManaged = isPointerType (typeOf exp') && not (isAtomicType expMadlibType)
        case Map.lookup name symbolTable of
          Just (Symbol (LocalVariableSymbol ptr) _) -> do
            ptr' <- safeBitcast ptr (Type.ptr $ typeOf exp')
            -- Perceus: rc_inc new value before overwriting the ref cell.
            -- We do NOT rc_dec the old value here because the new value may reference
            -- the old value (e.g. acc := [x, ...acc] — new list's tail IS old acc).
            -- The old value's lifetime is managed at scope exit / enclosing scope.
            -- Guard: only for heap-managed types (not primitives encoded as inttoptr)
            Monad.when isHeapManaged $
              emitRCInc exp'
            store ptr' 0 exp'
            return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Just ptr)

          -- Case of mutation within a tco optimized function
          Just (Symbol (TCOParamSymbol ptr) _) -> do
            ptr' <- safeBitcast ptr (Type.ptr $ typeOf exp')
            -- Perceus: rc_inc new value only (same reasoning as above).
            Monad.when isHeapManaged $
              emitRCInc exp'
            store ptr' 0 exp'
            return (Map.insert name (tcoParamSymbol ptr exp') symbolTable, exp', Just ptr)

          or ->
            error $ "found: " <> ppShow or
      else do
        (_, exp', _) <- generateExp env { isTopLevel = False } symbolTable e
        Monad.when (envIsDebugBuild env) $ do
          ptr <- alloca (typeOf exp') Nothing 0
          declareVariable env area False name ptr
          storeWithMetadata (makeDILocation env area) ptr 0 exp'
        -- Look up the boxed form from the source variable's symbol, if available.
        -- We can't use maybeBoxed from generateExp because for LocalVariableSymbol reads
        -- it returns the reference cell pointer, which is NOT the boxed form of the value.
        -- We unwrap Do blocks (from coverage instrumentation) to find the inner Var.
        let sym = case unwrapDoExp e of
              Core.Typed _ _ _ (Core.Var srcName _) ->
                case Map.lookup srcName symbolTable of
                  Just (Symbol (BoxedVariableSymbol boxed) _) ->
                    Symbol (BoxedVariableSymbol boxed) exp'
                  _ ->
                    varSymbol exp'
              _ ->
                varSymbol exp'
        return (Map.insert name sym symbolTable, exp', Nothing)

  Core.Typed (_ IT.:=> t) area _ (Core.Call (Core.Typed _ _ _ (Core.Var fnName _)) [Core.Typed _ _ _ (Core.ListConstructor items)])
    | "fromList" `List.isInfixOf` fnName && IT.getConstructorCon t == IT.tArrayCon && List.all (not . Core.isListSpread) items -> do
      let items' = List.map getListItemExp items
      items'' <- mapM (generateExp env symbolTable) items'
      let items''' = List.map (\(_, i, _) -> i) items''
      items'''' <- mapM box items'''
      itemsArray <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand (Constant.Mul False False (sizeof' (Type.ptr i8)) (Constant.Int 64 (fromIntegral $ List.length items))), [])]
      itemsArray' <- safeBitcast itemsArray (Type.ptr $ Type.ptr i8)
      let arrayType = Type.StructureType False [i64, i64, Type.ptr $ Type.ptr i8]
      arr <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' arrayType, [])]
      arr' <- safeBitcast arr (Type.ptr arrayType)

      Monad.foldM_ (storeArrayItem itemsArray') () (List.zip items'''' [0..])
      Monad.foldM_ (storeItem arr') () [(i64ConstOp (fromIntegral $ List.length items), 0), (i64ConstOp (fromIntegral $ List.length items), 1), (itemsArray', 2)]

      arr'' <- box arr'
      return (symbolTable, arr'', Nothing)

  Core.Typed (_ IT.:=> t) area _ (Core.Call (Core.Typed _ _ _ (Core.Var fnName _)) [Core.Typed _ _ _ (Core.ListConstructor items)])
    | "fromList" `List.isInfixOf` fnName && t == IT.tByteArray && List.all (not . Core.isListSpread) items -> do
      let items' = List.map getListItemExp items
      items'' <- mapM (generateExp env symbolTable) items'
      let items''' = List.map (\(_, i, _) -> i) items''
      itemsArray <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand (Constant.Mul False False (sizeof' i8) (Constant.Int 64 (fromIntegral $ List.length items))), [])]
      itemsArray' <- safeBitcast itemsArray (Type.ptr i8)
      let arrayType = Type.StructureType False [i64, i64, Type.ptr i8]
      arr <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' arrayType, [])]
      arr' <- safeBitcast arr (Type.ptr arrayType)

      Monad.foldM_ (storeArrayItem itemsArray') () (List.zip items''' [0..])
      Monad.foldM_ (storeItem arr') () [(i64ConstOp (fromIntegral $ List.length items), 0), (i64ConstOp (fromIntegral $ List.length items), 1), (itemsArray', 2)]

      arr'' <- box arr'
      return (symbolTable, arr'', Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "%" _)) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env symbolTable rightOperand
    result                <- Ops.generateMod (getType leftOperand) leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "!" _)) [operand]) -> do
    (_, operand', _) <- generateExp env symbolTable operand
    result           <- add operand' (Operand.ConstantOperand $ Constant.Int 1 1)
    return (symbolTable, result, Nothing)

  Core.Typed _ area _ (Core.Call (Core.Typed _ _ _ (Core.Var "++" _)) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env symbolTable rightOperand
    result <- callWithMetadata (makeDILocation env area) strConcat [(leftOperand', []), (rightOperand', [])]
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "&&" _)) [leftOperand, rightOperand]) -> mdo
    (_, leftOperand', _)  <- generateExp env symbolTable leftOperand
    andLhs <- currentBlock
    condBr leftOperand' andRhs andOutput

    andRhs <- block `named` "and.rhs"
    (_, rightOperand', _) <- generateExp env symbolTable rightOperand
    result                <- Instruction.and leftOperand' rightOperand'
    andRhs'               <- currentBlock
    br andOutput

    andOutput <- block `named` "and.output"
    output <- phi [(leftOperand', andLhs), (result, andRhs')]
    return (symbolTable, output, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "||" _)) [leftOperand, rightOperand]) -> mdo
    (_, leftOperand', _)  <- generateExp env symbolTable leftOperand
    orLhs <- currentBlock
    condBr leftOperand' orOutput orRhs

    orRhs <- block `named` "or.rhs"
    (_, rightOperand', _) <- generateExp env symbolTable rightOperand
    result                <- Instruction.or leftOperand' rightOperand'
    orRhs'                <- currentBlock
    br orOutput

    orOutput <- block `named` "or.output"
    output <- phi [(leftOperand', orLhs), (result, orRhs')]

    return (symbolTable, output, Nothing)

  Core.Typed qt _ metadata (Core.Call fn args) -> case fn of
    Core.Typed _ _ _ (Var "+" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateAdd (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "-" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateSub (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "unary-minus" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      result                <- Ops.generateUnaryMinus (getType (List.head args)) leftOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "*" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateMul (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "/" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateDiv (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "<<" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- shl leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">>" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- ashr leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">>>" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- lshr leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "|" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Instruction.or leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "&" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Instruction.and leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "^" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Instruction.xor leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "~" False) -> do
      (_, operand', _) <- generateExp env symbolTable (List.head args)
      result <- Ops.generateBitwiseNot (getType (List.head args)) operand'
      return (symbolTable, result, Nothing)

    Core.Typed _ area _ (Var "==" False) | getType (List.head args) `List.elem` [IT.tInteger, IT.tShort, IT.tByte, IT.tFloat, IT.tStr, IT.tBool, IT.tUnit, IT.tChar] -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateEq (callWithMetadata (makeDILocation env area)) (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateGt (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "<" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateLt (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">=" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateGte (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "<=" False) -> do
      (_, leftOperand', _)  <- generateExp env symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env symbolTable (args !! 1)
      result                <- Ops.generateLte (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    _ | Core.isPlainRecursiveCall metadata -> do
        let llvmType      = buildLLVMType env symbolTable (getQualType exp)
        let Just continue = continueRef <$> recursionData env
        let Just params   = boxedParams <$> recursionData env
        store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

        args'  <- mapM (generateExp env symbolTable) args
        let unboxedArgs = (\(_, x, _) -> x) <$> args'

        -- We need to reverse because we may have some closured variables in the params and these need not be updated
        let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> args) (List.reverse params) (List.reverse unboxedArgs)
        mapM_ (\(qt', ptr, exp) -> Fn.updateTCOArg symbolTable qt' ptr exp) paramUpdateData

        return (symbolTable, Operand.ConstantOperand (Constant.Undef llvmType), Nothing)

    Core.Typed (_ IT.:=> t) area _ (Core.Var functionName _) -> case Map.lookup functionName symbolTable of
      Just (Symbol (ConstructorSymbol index arity) fnOperand) -> do
        let constructorType = IT.getReturnType t
        let maxArity = retrieveConstructorMaxArity symbolTable constructorType

        if List.length args == arity then do
          -- optimize known calls to constructors to simple allocations without function call
          args'   <- mapM (generateExp env symbolTable) args
          args''  <- retrieveArgs (Core.getMetadata <$> args) args args'

          if isNewtypeADT symbolTable constructorType then do
            -- Newtype: single constructor, single field — just return the value
            return (symbolTable, List.head args'', Nothing)
          else if isSingleConstructorADT symbolTable constructorType then do
            -- Single-constructor: no tag field
            let structType = Type.StructureType False $ List.replicate maxArity boxType
            let argTypes   = (\a -> let (_ IT.:=> at') = Core.getQualType a in at') <$> args
            let mallocFn   = if arity == maxArity then chooseRCAlloc argTypes else rcAlloc
            structPtr     <- allocateStruct env area metadata structType mallocFn
            structPtr'    <- safeBitcast structPtr $ Type.ptr structType
            Monad.foldM_ (storeItem structPtr') () $ List.zip args'' [0..]
            return (symbolTable, structPtr', Nothing)
          else do
            -- Multi-constructor: struct with tag
            let structType = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType
            let argTypes   = (\a -> let (_ IT.:=> at') = Core.getQualType a in at') <$> args
            let mallocFn   = if arity == maxArity then chooseRCAlloc argTypes else rcAlloc
            structPtr     <- allocateStruct env area metadata structType mallocFn
            structPtr'    <- safeBitcast structPtr $ Type.ptr structType
            Monad.foldM_ (storeItem structPtr') () $ List.zip args'' [1..] ++ [(i64ConstOp (fromIntegral index), 0)]
            return (symbolTable, structPtr', Nothing)
        else
          generateApplicationForKnownFunction env symbolTable area qt arity fnOperand args

      Just (Symbol (FunctionSymbol arity) fnOperand) ->
        -- Check for in-place TRMC variant: if the list argument is a temporary
        -- (not a variable reference), dispatch to the in-place version
        case Map.lookup (functionName ++ "__inplace") symbolTable of
          Just (Symbol (InPlaceFunctionSymbol _ listParamIdx) inplaceOp)
            | listParamIdx < List.length args
            , isLinearListArg (args !! listParamIdx) ->
              generateApplicationForKnownFunction env symbolTable area qt arity inplaceOp args
          _ ->
            generateApplicationForKnownFunction env symbolTable area qt arity fnOperand args

      Just (Symbol symbolType pap) -> do
        -- We apply a partial application
        let argsApplied = List.length args
        let argc        = i32ConstOp (fromIntegral argsApplied)

        pap' <- case symbolType of
          TopLevelAssignment ->
            load pap 0

          -- LocalVariableSymbol p ->
          --   load p 0

          _ ->
            return pap

        pap'' <- safeBitcast pap' boxType

        args'     <- mapM (generateExp env symbolTable) args
        boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args args'

        ret       <-
          if argsApplied == 1 then
            callWithMetadata (makeDILocation env area) applyPAP1 $ [(pap'', [])] ++ ((,[]) <$> boxedArgs)
          else if argsApplied == 2 then
            callWithMetadata (makeDILocation env area) applyPAP2 $ [(pap'', [])] ++ ((,[]) <$> boxedArgs)
          else if argsApplied == 3 then
            callWithMetadata (makeDILocation env area) applyPAP3 $ [(pap'', [])] ++ ((,[]) <$> boxedArgs)
          else if argsApplied == 4 then
            callWithMetadata (makeDILocation env area) applyPAP4 $ [(pap'', [])] ++ ((,[]) <$> boxedArgs)
          else
            callWithMetadata (makeDILocation env area) applyPAP $ [(pap'', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed   <- unbox env symbolTable qt ret
        return (symbolTable, unboxed, Just ret)

      _ ->
        error $ "Function not found " <> functionName <> "\narea: " <> ppShow area <> "\nST: " <> ppShow symbolTable

    Core.Typed _ area _ _ -> do
      (_, pap, _) <- generateExp env symbolTable fn
      pap' <- safeBitcast pap boxType

      let argc = i32ConstOp (fromIntegral $ List.length args)

      args'  <- mapM (generateExp env symbolTable) args
      boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args args'

      ret <-
        if List.length args == 1 then
            callWithMetadata (makeDILocation env area) applyPAP1 $ [(pap', [])] ++ ((,[]) <$> boxedArgs)
        else if List.length args == 2 then
            callWithMetadata (makeDILocation env area) applyPAP2 $ [(pap', [])] ++ ((,[]) <$> boxedArgs)
        else if List.length args == 3 then
            callWithMetadata (makeDILocation env area) applyPAP3 $ [(pap', [])] ++ ((,[]) <$> boxedArgs)
        else if List.length args == 4 then
            callWithMetadata (makeDILocation env area) applyPAP4 $ [(pap', [])] ++ ((,[]) <$> boxedArgs)
        else
          callWithMetadata (makeDILocation env area) applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
      unboxed <- unbox env symbolTable qt ret
      return (symbolTable, unboxed, Just ret)

    _ ->
      error "Unreachable: Call with untyped function expression"

  Core.Typed _ area _ Core.TypedHole -> do
    callWithMetadata (makeDILocation env area) typedHoleReached []
    return (symbolTable, Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1), Nothing)

  Core.Typed (_ IT.:=> t) _ _ (Core.Literal (Core.LNum n)) -> case t of
    IT.TCon (IT.TC "Float" _) _ _ ->
      return (symbolTable, C.double (read n), Nothing)

    IT.TCon (IT.TC "Integer" _) _ _ ->
      return (symbolTable, C.int64 (read n), Nothing)

    IT.TCon (IT.TC "Short" _) _ _ ->
      return (symbolTable, C.int32 (read n), Nothing)

    IT.TCon (IT.TC "Byte" _) _ _ ->
      return (symbolTable, C.int8 (read n), Nothing)

    _ ->
      return (symbolTable, C.int64 (read n), Nothing)

  Core.Typed _ _ _ (Core.Literal (Core.LFloat n)) -> do
    return (symbolTable, C.double (read n), Nothing)

  Core.Typed _ _ _ (Core.Literal (Core.LBool b)) -> do
    let value =
          if b == "true" then
            1
          else
            0
    return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 value, Nothing)

  Core.Typed _ _ _ (Core.Literal Core.LUnit) -> do
    return (symbolTable, Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1), Nothing)

  Core.Typed _ area _ (Core.Literal (Core.LStr s)) -> do
    addr <- buildStr env area s
    return (symbolTable, addr, Nothing)

  Core.Typed _ _ _ (Core.Literal (Core.LChar c)) -> do
    return (symbolTable, Operand.ConstantOperand $ Constant.Int 32 (fromIntegral $ fromEnum c), Nothing)

  Core.Typed _ _ _ (Core.Do exps) -> do
    (ret, boxed) <- Fn.generateDoExps makeFunctionCtx env symbolTable exps
    return (symbolTable, ret, boxed)

  Core.Typed _ area metadata (Core.TupleConstructor exps) -> do
    exps' <- mapM (generateExp env symbolTable) exps
    -- Use native field types for primitive elements (avoids inttoptr/ptrtoint round-trips).
    -- Non-primitive elements fall back to boxType (i8*) as before.
    let elemQualTypes  = Core.getQualType <$> exps
        fieldLLVMTypes = tupleFieldLLVMType env symbolTable <$> elemQualTypes
        tupleType      = Type.StructureType False fieldLLVMTypes
        -- For primitive fields, use the unboxed value; for boxed fields, use the boxed form.
        -- exps' triples: (symbolTable, unboxed, maybeBoxed)
        -- Note: 'unboxed' may actually be boxed (i8*) if the value came as a function parameter.
        -- In that case we must explicitly unbox it to the native primitive type before storing.
        getFieldValue (_, unboxed, _) fieldType qt =
          if fieldType == boxType
            then if typeOf unboxed == boxType
                   then return unboxed
                   else box unboxed
            else if typeOf unboxed == fieldType
                   then return unboxed
                   else unbox env symbolTable qt unboxed
    fieldVals <- mapM (\((triple, ft), qt) -> getFieldValue triple ft qt) (List.zip (List.zip exps' fieldLLVMTypes) elemQualTypes)
    let expsWithIds = List.zip fieldVals [0..]
        elemTypes   = (\e -> let (_ IT.:=> et) = Core.getQualType e in et) <$> exps
        mallocFn    = chooseRCAlloc elemTypes
    tuplePtr  <- allocateStruct env area metadata tupleType mallocFn
    tuplePtr' <- safeBitcast tuplePtr (Type.ptr tupleType)
    Monad.foldM_ (storeItem tuplePtr') () expsWithIds

    return (symbolTable, tuplePtr', Nothing)

  Core.Typed _ area _ (Core.ListConstructor []) -> do
    -- an empty list is { value: null, next: null }
    emptyList' <- emptyList env area
    return (symbolTable, emptyList', Nothing)

  Core.Typed _ _ metadata (Core.ListConstructor [
      Core.Typed _ area _ (Core.ListItem li),
      Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
    ]) | Core.isInPlaceListRecursiveCall metadata -> do
      -- In-place: overwrite the current input node's value field; advance via its original next ptr.
      let Just continue      = continueRef  <$> recursionData env
      let Just params        = boxedParams  <$> recursionData env
      let Just endPtr        = end          <$> recursionData env
      let Just prevEndPtr    = prevEnd =<< recursionData env
      let Just anyNodeAdded' = anyNodeAdded <$> recursionData env
      endValue <- load endPtr 0   -- current input node (listType, addrspace 1)

      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))
      store anyNodeAdded' 0 (Operand.ConstantOperand (Constant.Int 1 1))

      -- Save current node as prevEnd (for RecursionEnd to link the base-case expression)
      store prevEndPtr 0 endValue

      args'  <- mapM (generateExp env symbolTable) args
      let unboxedArgs = (\(_, x, _) -> x) <$> args'

      (_, item, maybeBoxedItem) <- generateExp env symbolTable li
      item' <- case maybeBoxedItem of
        Just boxed ->
          return boxed

        Nothing ->
          box item

      -- Overwrite the value field; keep the original next pointer untouched
      storeItem endValue () (item', 0)

      -- Advance end to the original next node
      nextField <- gep endValue [i32ConstOp 0, i32ConstOp 1]
      nextRaw   <- load nextField 0          -- i8* (boxType)
      nextNode  <- addrspacecast nextRaw listType
      store endPtr 0 nextNode

      -- We need to reverse because we may have some closured variables in the params and these need not be updated
      let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> args) (List.reverse params) (List.reverse unboxedArgs)
      mapM_ (\(qt, ptr, exp) -> Fn.updateTCOArg symbolTable qt ptr exp) paramUpdateData

      return (symbolTable, Operand.ConstantOperand (Constant.Undef (typeOf endValue)), Nothing)

  Core.Typed _ _ metadata (Core.ListConstructor [
      Core.Typed _ area _ (Core.ListItem li),
      Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
    ]) | Core.isRightListRecursiveCall metadata -> do
      let Just continue      = continueRef  <$> recursionData env
      let Just params        = boxedParams  <$> recursionData env
      let Just endPtr        = end          <$> recursionData env
      let Just anyNodeAdded' = anyNodeAdded <$> recursionData env
      endValue <- load endPtr 0

      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))
      store anyNodeAdded' 0 (Operand.ConstantOperand (Constant.Int 1 1))

      args'  <- mapM (generateExp env symbolTable) args
      let unboxedArgs = (\(_, x, _) -> x) <$> args'

      (_, item, maybeBoxedItem) <- generateExp env symbolTable li
      item' <- case maybeBoxedItem of
        Just boxed ->
          return boxed

        Nothing ->
          box item

      newNode <- allocateArenaNode env area
      newNode' <- addrspacecast newNode listType
      storeItem endValue () (item', 0)
      storeItem endValue () (newNode, 1)

      -- end = end.next
      store endPtr 0 newNode'

      -- We need to reverse because we may have some closured variables in the params and these need not be updated
      let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> args) (List.reverse params) (List.reverse unboxedArgs)
      mapM_ (\(qt, ptr, exp) -> Fn.updateTCOArg symbolTable qt ptr exp) paramUpdateData

      -- return (symbolTable, Operand.ConstantOperand (Constant.Undef Type.i8), Nothing)
      return (symbolTable, Operand.ConstantOperand (Constant.Undef (typeOf endValue)), Nothing)

  Core.Typed _ _ metadata (Core.ListConstructor [
      Core.Typed _ area1 _ (Core.ListItem li1),
      Core.Typed _ area2 _ (Core.ListItem li2),
      Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
    ]) | Core.isRightListRecursiveCall metadata -> do
      let Just continue      = continueRef  <$> recursionData env
      let Just params        = boxedParams  <$> recursionData env
      let Just endPtr        = end          <$> recursionData env
      let Just anyNodeAdded' = anyNodeAdded <$> recursionData env
      endValue <- load endPtr 0

      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))
      store anyNodeAdded' 0 (Operand.ConstantOperand (Constant.Int 1 1))

      args'  <- mapM (generateExp env symbolTable) args
      let unboxedArgs = (\(_, x, _) -> x) <$> args'

      (_, item1, maybeBoxedItem1) <- generateExp env symbolTable li1
      item1' <- case maybeBoxedItem1 of
        Just boxed ->
          return boxed

        Nothing ->
          box item1

      (_, item2, maybeBoxedItem2) <- generateExp env symbolTable li2
      item2' <- case maybeBoxedItem2 of
        Just boxed ->
          return boxed

        Nothing ->
          box item2

      newNode1 <- allocateArenaNode env area1
      newNode1' <- addrspacecast newNode1 listType
      storeItem endValue () (item1', 0)
      storeItem endValue () (newNode1, 1)

      newNode2 <- allocateArenaNode env area2
      newNode2' <- addrspacecast newNode2 listType
      storeItem newNode1' () (item2', 0)
      storeItem newNode1' () (newNode2, 1)

      -- end = end.next
      store endPtr 0 newNode2'

      -- We need to reverse because we may have some closured variables in the params and these need not be updated
      let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> args) (List.reverse params) (List.reverse unboxedArgs)
      mapM_ (\(qt, ptr, exp) -> Fn.updateTCOArg symbolTable qt ptr exp) paramUpdateData

      -- return (symbolTable, Operand.ConstantOperand (Constant.Undef Type.i8), Nothing)
      return (symbolTable, Operand.ConstantOperand (Constant.Undef (typeOf endValue)), Nothing)


  Core.Typed _ _ _ (Core.ListConstructor listItems) -> do
    let allAreItems = all (\case { Core.Typed _ _ _ (Core.ListItem _) -> True; _ -> False }) listItems
    if allAreItems && List.length listItems > 1 then do
      -- Bulk allocation: all items are ListItem, no spreads.
      -- Allocate (n+1) nodes in a single GC_MALLOC call (n items + 1 sentinel).
      let nodeType = Type.StructureType False [boxType, boxType]
      let n = List.length listItems
      let totalSize = Operand.ConstantOperand $ Constant.Int 64 (fromIntegral $ n * 16 + 16)  -- each node = 2 pointers = 16 bytes
      let area = case List.head listItems of { Core.Typed _ a _ _ -> a; _ -> emptyArea }

      -- Evaluate all items left-to-right first
      evaluatedItems <- mapM (\case
        Core.Typed _ _ _ (Core.ListItem item) -> do
          item' <- generateExp env symbolTable item
          items <- retrieveArgs [Core.getMetadata item] [item] [item']
          return (List.head items)
        _ -> error "Unreachable: checked allAreItems above"
        ) listItems

      -- Single bulk allocation for all nodes
      bulkPtr <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(totalSize, [])]
      bulkPtr' <- safeBitcast bulkPtr (Type.ptr nodeType)

      -- Fill each node: node[i].value = item[i], node[i].next = &node[i+1]
      Monad.forM_ (List.zip [0..] evaluatedItems) $ \(i, itemVal) -> do
        nodePtr <- gep bulkPtr' [Operand.ConstantOperand (Constant.Int 32 (fromIntegral (i :: Int)))]
        nodePtr' <- addrspacecast nodePtr listType
        storeItem nodePtr' () (itemVal, 0)
        -- next pointer: must bitcast from { i8*, i8* }* to i8* for the node's next field
        nextPtr <- gep bulkPtr' [Operand.ConstantOperand (Constant.Int 32 (fromIntegral (i + 1)))]
        nextPtr' <- safeBitcast nextPtr boxType
        storeItem nodePtr' () (nextPtr', 1)

      -- Initialize sentinel node (last): value = null, next = null
      sentinelPtr <- gep bulkPtr' [Operand.ConstantOperand (Constant.Int 32 (fromIntegral n))]
      sentinelPtr' <- addrspacecast sentinelPtr listType
      storeItem sentinelPtr' () (Operand.ConstantOperand (Constant.Null boxType), 0)
      storeItem sentinelPtr' () (Operand.ConstantOperand (Constant.Null boxType), 1)

      -- Return pointer to first node
      head' <- addrspacecast bulkPtr' listType
      return (symbolTable, head', Nothing)
    else do
      -- Original path: handles spreads and single-item lists
      tail <- case List.last listItems of
        Core.Typed _ area _ (Core.ListItem lastItem) -> do
          item <- generateExp env symbolTable lastItem
          items <- retrieveArgs [Core.getMetadata lastItem] [lastItem] [item]
          callWithMetadata (makeDILocation env area) madlistSingleton [(List.head items, [])]

        Core.Typed _ _ _ (Core.ListSpread spread) -> do
          (_, e, _) <- generateExp env symbolTable spread
          return e

        _ -> error "Unreachable: list constructor with untyped item"

      list <- Monad.foldM
        (\list' i -> case i of
          Core.Typed _ area _ (Core.ListItem item) -> do
            item' <- generateExp env symbolTable item
            items <- retrieveArgs [Core.getMetadata item] [item] [item']

            newHead <- callMallocWithMetadata (makeDILocation env area) rcAlloc [(Operand.ConstantOperand $ sizeof' (Type.StructureType False [boxType, boxType]), [])]
            newHead' <- safeBitcast newHead listType
            nextPtr <- gep newHead' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
            nextPtr' <- addrspacecast nextPtr (Type.ptr listType)
            storeWithMetadata (makeDILocation env area) nextPtr' 0 list'
            valuePtr <- gep newHead' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]

            storeWithMetadata (makeDILocation env area) valuePtr 0 (List.head items)
            return newHead'

          Core.Typed _ area _ (Core.ListSpread spread) -> do
            (_, spread, _)  <- generateExp env symbolTable spread
            callWithMetadata (makeDILocation env area) madlistConcat [(spread, []), (list', [])]

          _ -> error "Unreachable: list constructor with untyped item"
        )
        tail
        (List.reverse $ List.init listItems)

      return (symbolTable, list, Nothing)

  Core.Typed qt@(_ IT.:=> recType) area metadata (Core.Record fields) -> do
    let (base, fields') = List.partition isSpreadField fields
    let sortedFields = List.sortOn (Maybe.fromMaybe "" . Core.getFieldName) fields'
    -- Use the result record type to determine the struct size (not just explicit fields)
    let allFieldNames = case recType of
          IT.TRecord fs _ os -> Map.keys (Map.union fs os)
          _                  -> []
    let allFieldTypes = case recType of
          IT.TRecord fs _ os -> Map.elems (Map.union fs os)
          _                  -> []
    let fieldLLVMTypes = (primitiveTupleFieldType . ([] IT.:=>)) <$> allFieldTypes
    let structType = Type.StructureType False fieldLLVMTypes

    -- Allocate a single flat struct for the record (stack or heap based on escape analysis)
    let mallocFn = chooseRCAlloc allFieldTypes
    recordPtr  <- allocateStruct env area metadata structType mallocFn
    recordPtr' <- safeBitcast recordPtr (Type.ptr structType)

    -- If there's a spread base, copy its fields into the result struct
    case base of
      [Core.Typed _ _ _ (Core.FieldSpread exp)] -> do
        (_, baseOperand, _) <- generateExp env symbolTable exp
        let baseRecType     = let (_ IT.:=> bt) = Core.getQualType exp in bt
        let baseFieldNames  = case baseRecType of
              IT.TRecord fs _ os -> Map.keys (Map.union fs os)
              _                  -> []
        let baseFieldTypes' = case baseRecType of
              IT.TRecord fs _ os -> Map.elems (Map.union fs os)
              _                  -> []
        let baseStructType  = Type.StructureType False ((primitiveTupleFieldType . ([] IT.:=>)) <$> baseFieldTypes')
        basePtr <- safeBitcast baseOperand (Type.ptr baseStructType)
        -- Copy each base field to its position in the result struct (field types are preserved)
        Monad.forM_ baseFieldNames $ \fieldName -> do
          let srcIndex = recordFieldIndex fieldName baseRecType
          let dstIndex = recordFieldIndex fieldName recType
          srcPtr <- gep basePtr [i32ConstOp 0, i32ConstOp srcIndex]
          srcVal <- load srcPtr 0
          dstPtr <- gep recordPtr' [i32ConstOp 0, i32ConstOp dstIndex]
          store dstPtr 0 srcVal

      _ -> return ()

    -- Store each field value at its index in the flat struct.
    -- For primitive fields, store the native value directly (avoids inttoptr/ptrtoint round-trips).
    -- For boxed fields, box the value first as before.
    Monad.forM_ sortedFields $ \field -> case field of
      Core.Typed _ _ _ (Core.Field (name, value)) -> do
        (_, val, maybeBoxed) <- generateExp env symbolTable value
        let qt        = Core.getQualType value
            fieldType = primitiveTupleFieldType qt
            index     = recordFieldIndex name recType
        fieldVal <-
          if fieldType == boxType
            then case maybeBoxed of
                   Just boxed -> return boxed
                   Nothing    -> box val
            else if typeOf val == fieldType
                   then return val
                   else unbox env symbolTable qt val
        fieldPtr <- gep recordPtr' [i32ConstOp 0, i32ConstOp index]
        store fieldPtr 0 fieldVal

      _ -> return ()

    return (symbolTable, recordPtr', Nothing)

  -- typedef struct madlib__array__Array {
  --   int64_t length;
  --   int64_t capacity;
  --   void **items;
  -- } madlib__array__Array_t;
  Core.Typed qt area _ (Core.ArrayAccess arr index) -> mdo
    (_, arrOperand, _) <- generateExp env symbolTable arr
    (_, indexOperand, _) <- generateExp env symbolTable index
    let arrayType = Type.ptr $ Type.StructureType False [i64, i64, Type.ptr $ Type.ptr i8]
    arrOperand' <- safeBitcast arrOperand arrayType

    len <- gep arrOperand' [i32ConstOp 0, i32ConstOp 0]
    len' <- load len 0

    negativeIndex <- icmp IntegerPredicate.SLT indexOperand (Operand.ConstantOperand (Constant.Int 64 0))
    tooLarge <- icmp IntegerPredicate.SGE indexOperand len'
    outOfBound <- Instruction.or negativeIndex tooLarge
    condBr outOfBound outOfBoundBlock allGoodBlock

    outOfBoundBlock <- block `named` "outOfBoundBlock"
    callWithMetadata (makeDILocation env area) arrayOutOfBounds [(indexOperand, []), (len', [])]
    br exitBlock

    allGoodBlock <- block `named` "allGoodBlock"
    items <- gep arrOperand' [i32ConstOp 0, i32ConstOp 2]
    items' <- load items 0
    item <- gep items' [indexOperand]
    br exitBlock

    exitBlock <- block `named` "exitBlock"
    ret <- phi [(item, allGoodBlock), (Operand.ConstantOperand $ Constant.Null (Type.ptr $ Type.ptr i8), outOfBoundBlock)]
    ret' <- load ret 0
    ret'' <- unbox env symbolTable qt ret'

    return (symbolTable, ret'', Just ret')

  Core.Typed qt _ _ (Core.Access record@(Core.Typed (_ IT.:=> recType) _ _ _) (Core.Typed _ area _ (Core.Var ('.' : fieldName) _))) -> do
    (_, recordOperand, _) <- generateExp env symbolTable record
    value <- case recType of
      IT.TRecord fields _ optionalFields -> do
        let allFields  = Map.union fields optionalFields
            bareTypes  = Map.elems allFields
            structType = Type.StructureType False ((primitiveTupleFieldType . ([] IT.:=>)) <$> bareTypes)
            index      = recordFieldIndex fieldName recType
        recordOperand' <- safeBitcast recordOperand (Type.ptr structType)
        fieldPtr       <- gep recordOperand' [i32ConstOp 0, i32ConstOp index]
        load fieldPtr 0

      _ -> do
        nameOperand <- buildStr env area fieldName
        callWithMetadata (makeDILocation env area) selectField [(nameOperand, []), (recordOperand, [])]

    -- If the field is already in its native LLVM type, skip unboxing to avoid
    -- a redundant ptrtoint/bitcast round-trip.
    let nativeTy = buildLLVMType env symbolTable qt
    value' <- if typeOf value == nativeTy
                then return value
                else unbox env symbolTable qt value
    return (symbolTable, value', Just value)


  Core.Typed _ _ _ (Core.If cond truthy falsy) -> mdo
    (symbolTable', cond', _) <- generateExp env symbolTable cond
    condBr cond' ifThen ifElse

    ifThen <- block `named` "if.then"
    (_, truthy', _) <- generateExp env symbolTable' truthy
    ifThen' <- currentBlock
    br ifExit

    ifElse <- block `named` "if.else"
    (_, falsy', _) <- generateExp env symbolTable' falsy
    ifElse' <- currentBlock
    br ifExit

    ifExit <- block `named` "if.exit"
    ret <- phi [(truthy', ifThen'), (falsy', ifElse')]

    return (symbolTable', ret, Nothing)

  Core.Typed _ _ _ (Core.While cond body) -> mdo
    _ <- currentBlock
    br loop

    loop <- block `named` "loop"
    (_, cond', _) <- generateExp env symbolTable cond
    condBr cond' bodyBlock afterLoop

    bodyBlock <- block `named` "body"
    generateExp env symbolTable body
    br loop

    afterLoop <- block `named` "loopExit"
    let unit = Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1)
    return (symbolTable, unit, Nothing)

  Core.Typed _ _ _ (Core.Where exp iss) -> mdo
    (_, exp', _) <- generateExp env symbolTable exp
    let ctx = PM.PatternCtx
                { PM.ctxGenerateExp = generateExp
                , PM.ctxBuildStr    = buildStr
                , PM.ctxSafeBitcast = safeBitcast
                , PM.ctxStoreItem   = storeItem
                , PM.ctxAddrSpaceCast = addrspacecast
                }
    branches     <- PM.generateBranches ctx env symbolTable exitBlock exp' iss

    exitBlock    <- block `named` "exitBlock"
    ret          <- phi branches

    return (symbolTable, ret, Nothing)

  Core.Typed qt@(_ IT.:=> t) _ _ (Core.NameExport n) -> do
    let ref = Operand.ConstantOperand $ Constant.GlobalReference (buildLLVMType env symbolTable qt) (AST.mkName n)
    if IT.isFunctionType t then do
      let arity = List.length $ IT.getParamTypes t
      Writer.tell $ Map.singleton n (fnSymbol arity ref)
    else
      Writer.tell $ Map.singleton n (varSymbol ref)
    return (symbolTable, ref, Nothing)

  _ ->
    error $ "not implemented\n\n" ++ ppShow exp


-- | Create the FunctionCtx for Function.hs callbacks
makeFunctionCtx :: (Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Fn.FunctionCtx m
makeFunctionCtx = Fn.FunctionCtx
  { Fn.ctxGenerateExp  = generateExp
  , Fn.ctxSafeBitcast  = safeBitcast
  , Fn.ctxAddrSpaceCast = addrspacecast
  , Fn.ctxStoreItem    = storeItem
  }


-- | Re-export compileModule with callbacks filled in
compileModule :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m) => Options -> Core.AST -> m (SymbolTable, Env, ByteString.ByteString)
compileModule = Mod.compileModule makeFunctionCtx safeBitcast


-- | Re-export buildTarget
buildTarget :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m) => Options -> [String] -> FilePath -> m ()
buildTarget = Mod.buildTarget
