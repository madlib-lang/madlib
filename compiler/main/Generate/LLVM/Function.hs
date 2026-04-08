{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.Function
  ( FunctionCtx(..)
  , generateExternFunction
  , generateFunction
  , generateTopLevelFunction
  , addTopLevelFnToSymbolTable
  , generateDoExps
  , generateBody
  , generateExps
  , generateConstructor
  , findMaximumConstructorArity
  , generateConstructorsForADT
  , generateConstructors
  , generateTopLevelFunctions
  , makeParamName
  , expsForMain
  , topLevelFunctions
  , updateTCOArg
  , fnSymbol
  , topLevelSymbol
  , constructorSymbol
  , storeItem
  ) where

import qualified Data.Map                     as Map
import qualified Data.List                    as List
import qualified Control.Monad                as Monad
import qualified Control.Monad.Fix            as MonadFix
import qualified Control.Monad.Writer         as Writer
import qualified Control.Monad.State          as State

import           LLVM.AST                     as AST hiding (function)
import           LLVM.AST.Type                as Type
import qualified LLVM.AST.Global              as Global
import           LLVM.AST.ParameterAttribute  as ParameterAttribute
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand
import           Generate.LLVM.TypeOf         (Typed(typeOf))

import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Instruction   as Instruction hiding (gep)
import           Generate.LLVM.Emit          (emitGEP)
import           LLVM.IRBuilder.Constant      as C

import           AST.Core                     as Core
import qualified Infer.Type                   as IT
import           Infer.Type                   (isFunctionType)
import           Explain.Location             (Area)
import           Data.ByteString.Short        (ShortByteString)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.IORef

import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import           Generate.LLVM.Types          (boxType, listType, papType, sizeof', buildLLVMType, buildLLVMParamType, retrieveConstructorStructType, adtSymbol)
import           Generate.LLVM.Builtins       (i8ConstOp, i32ConstOp, i64ConstOp, doubleConstOp, gcDisable, gcEnable, rcAlloc, chooseRCAlloc, isAtomicType, rcDec)
import           Generate.LLVM.Boxing         (box, unbox)
import           Generate.LLVM.WithMetadata   (functionWithMetadata, callWithMetadata, callMallocWithMetadata, storeWithMetadata)
import           Generate.LLVM.Debug
import           Generate.LLVM.Helper
import           GHC.Stack (HasCallStack)

-- | Inbounds GEP — shadows LLVM.IRBuilder.Instruction.gep
gep :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
gep = emitGEP

maxSupportedPAPArity :: Int
maxSupportedPAPArity = 50

mkPAPArityError :: String -> Int -> String
mkPAPArityError name arity =
  "LLVM codegen arity limit reached for '" <> name <> "' (arity " <> show arity
  <> ", supported max " <> show maxSupportedPAPArity <> "). "
  <> "Raise runtime/src/apply-pap.* max arity before compiling this function."


-- | Determine if a right-list recursive function is eligible for an in-place clone.
-- Returns Just listParamIndex if the function destructures a list parameter via a where clause,
-- Nothing for list-building functions (repeat, range) that don't take a list input.
findListParamForInPlace :: [Core.Metadata] -> [Core String] -> [Core.Exp] -> Maybe Int
findListParamForInPlace metadata params body
  | Core.isRightListRecursiveDefinition metadata =
      let dictCount = List.length $ List.filter ("$" `List.isPrefixOf`) (Core.getValue <$> params)
          nonDictParamNames = Core.getValue <$> List.drop dictCount params
          maybeScrutinee = case last body of
            Core.Typed _ _ _ (Core.Where (Core.Typed _ _ _ (Core.Var name _)) _) -> Just name
            Core.Typed _ _ _ (Core.Do exps) -> case last exps of
              Core.Typed _ _ _ (Core.Where (Core.Typed _ _ _ (Core.Var name _)) _) -> Just name
              _ -> Nothing
            _ -> Nothing
      in  case maybeScrutinee of
            Just name -> case List.elemIndex name nonDictParamNames of
              Just idx ->
                -- Only generate in-place version when the recursive call passes the
                -- scrutinee's tail directly (a Var), not a reconstructed list.
                -- Functions like 'flatten' that pass [xs, ...vs] (a ListConstructor)
                -- to the recursive call are NOT safe for in-place mutation.
                if hasListConstructorInRecursiveArg dictCount (List.elemIndex name nonDictParamNames) body
                   || hasMultiItemRecursiveBranch body
                  then Nothing
                  else Just (dictCount + idx)
              Nothing  -> Nothing
            Nothing -> Nothing
  | otherwise = Nothing

-- | Returns True if any recursive list branch has 2+ items before the spread.
-- Such branches (like ['\\', '"', ...go(cs)]) produce more output nodes than input
-- nodes consumed, making in-place mutation unsafe (would overwrite unconsumed input).
hasMultiItemRecursiveBranch :: [Core.Exp] -> Bool
hasMultiItemRecursiveBranch = any checkExp
  where
    checkExp exp = case exp of
      Core.Typed _ _ meta (Core.ListConstructor items)
        | Core.isRightListRecursiveCall meta ->
            let itemCount = List.length $ List.filter isListItem items
            in  itemCount > 1
      Core.Typed _ _ _ e -> checkContent e
      _ -> False

    checkContent e = case e of
      Core.Where _ iss    -> any (\is -> case is of { Core.Typed _ _ _ (Core.Is _ x) -> checkExp x; _ -> False }) iss
      Core.If _ t f       -> checkExp t || checkExp f
      Core.Do exps        -> any checkExp exps
      Core.Definition _ b -> any checkExp b
      _                   -> False

    isListItem (Core.Typed _ _ _ (Core.ListItem _)) = True
    isListItem _                                      = False


-- | Returns True if any recursive cons cell in the body has a recursive call
-- that passes a ListConstructor as the list argument (at the scrutinee position).
-- Such functions (like 'flatten') are NOT safe for in-place mutation because
-- they reconstruct a new list for the recursive call rather than passing a tail.
-- Functions like 'map' pass the tail variable directly and ARE safe.
hasListConstructorInRecursiveArg :: Int -> Maybe Int -> [Core.Exp] -> Bool
hasListConstructorInRecursiveArg dictCount maybeIdx body = case maybeIdx of
  Nothing  -> False
  Just idx ->
    let listArgIdx = dictCount + idx
    in  any (checkExp listArgIdx) body
  where
    -- The TCE pass marks the ListConstructor node (e.g. [x, ...rec(...)]) with
    -- RecursiveCall metadata when the spread contains a recursive call.
    -- We check if that spread's recursive call receives a ListConstructor argument.
    checkExp listArgIdx exp = case exp of
      Core.Typed _ _ meta (Core.ListConstructor items)
        | Core.isRightListRecursiveCall meta ->
            -- The spread is the last item; check if the recursive call inside it
            -- has a ListConstructor at the list-argument position.
            case lastMaybe items of
              Just (Core.Typed _ _ _ (Core.ListSpread spread)) ->
                spreadHasListConstructorArg listArgIdx spread
              _ -> False
      Core.Typed _ _ _ e ->
        checkExpContent listArgIdx e
      _ ->
        False

    -- Walk into the spread expression to find the recursive Call and check its args.
    spreadHasListConstructorArg listArgIdx exp = case exp of
      Core.Typed _ _ _ (Core.Call _ args) ->
        listArgIdx < List.length args && isListConstructor (args !! listArgIdx)
      Core.Typed _ _ _ e ->
        checkExpContentForCall listArgIdx e
      _ -> False

    checkExpContentForCall listArgIdx e = case e of
      Core.If _ t f    -> spreadHasListConstructorArg listArgIdx t || spreadHasListConstructorArg listArgIdx f
      Core.Do exps     -> any (spreadHasListConstructorArg listArgIdx) exps
      _                -> False

    checkExpContent listArgIdx e = case e of
      Core.Where _ iss -> any (checkIs listArgIdx) iss
      Core.If _ t f    -> checkExp listArgIdx t || checkExp listArgIdx f
      Core.Do exps     -> any (checkExp listArgIdx) exps
      Core.Definition _ body' -> any (checkExp listArgIdx) body'
      _                -> False

    checkIs listArgIdx is = case is of
      Core.Typed _ _ _ (Core.Is _ exp) -> checkExp listArgIdx exp
      _ -> False

    isListConstructor exp = case exp of
      Core.Typed _ _ _ (Core.ListConstructor _) -> True
      _                                          -> False

    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)


-- | Callback context to break the circular dependency with LLVM.hs (generateExp lives there).
data FunctionCtx m = FunctionCtx
  { ctxGenerateExp  :: Env -> SymbolTable -> Core.Exp -> m (SymbolTable, Operand, Maybe Operand)
  , ctxSafeBitcast  :: Operand -> Type -> m Operand
  , ctxAddrSpaceCast :: Operand -> Type -> m Operand
  , ctxStoreItem    :: Operand -> () -> (Operand, Integer) -> m ()
  , ctxRegisterScopeDrop :: Operand -> IT.Type -> m ()
    -- ^ Register a (value, madlib-type) pair to be rc_dec'd at the enclosing
    -- function scope exit.  Called from generateExp when a ScopeDrop-annotated
    -- Assignment is processed.  Each generateFunction call installs a fresh
    -- implementation backed by a per-function IORef.
  , ctxEmitRCDec :: Operand -> m ()
    -- ^ Emit an rc_dec call on the given operand.  Provided as a callback to
    -- break the circular dependency between Function.hs and LLVM.hs.
  }

-- Symbol constructors (duplicated from LLVM.hs to avoid circular imports)

localVarSymbol :: Operand -> Operand -> Symbol
localVarSymbol ptr = Symbol (LocalVariableSymbol ptr)

tcoParamSymbol :: Operand -> Operand -> Symbol
tcoParamSymbol ptr = Symbol (TCOParamSymbol ptr)

fnSymbol :: Int -> Operand -> Symbol
fnSymbol arity = Symbol (FunctionSymbol arity)

topLevelSymbol :: Operand -> Symbol
topLevelSymbol = Symbol TopLevelAssignment

constructorSymbol :: Operand -> Int -> Int -> Symbol
constructorSymbol ctor id arity = Symbol (ConstructorSymbol id arity) ctor


-- | Store an item into a struct field at a given index.
storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> () -> (Operand, Integer) -> m ()
storeItem basePtr _ (item, index) = do
  ptr <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  store ptr 0 item
  return ()


makeParamName :: String -> ParameterName
makeParamName = ParameterName . stringToShortByteString


updateTCOArg :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> IT.Qual IT.Type -> Operand -> Operand -> m (SymbolTable, Operand, Maybe Operand)
updateTCOArg symbolTable (_ IT.:=> t) ptr exp =
  if IT.isFunctionType t then do
    ptr' <- load ptr 0
    exp' <- load exp 0
    store ptr' 0 exp'
    return (symbolTable, exp, Nothing)
  else do
    store ptr 0 exp
    return (symbolTable, exp, Nothing)


generateExps :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
             => FunctionCtx m -> Env -> SymbolTable -> [Core.Exp] -> m ()
generateExps ctx env symbolTable exps = case exps of
  [exp] -> do
    ctxGenerateExp ctx env { isTopLevel = isTopLevelAssignment exp } symbolTable exp
    return ()

  (exp : es) -> do
    (symbolTable', _, _) <- ctxGenerateExp ctx env { isTopLevel = isTopLevelAssignment exp } symbolTable exp
    generateExps ctx env symbolTable' es

  _ ->
    return ()


generateDoExps :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
               => FunctionCtx m -> Env -> SymbolTable -> [Core.Exp] -> m (Operand, Maybe Operand)
generateDoExps ctx env symbolTable exps = case exps of
  [exp] -> do
    (_, result, boxed) <- ctxGenerateExp ctx env symbolTable exp
    return (result, boxed)

  (exp : es) -> do
    (symbolTable', _, _) <- ctxGenerateExp ctx env symbolTable exp
    generateBody ctx env symbolTable' es

  [] ->
    error "Unreachable: generateDoExps called with empty expression list"


generateBody :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
             => FunctionCtx m -> Env -> SymbolTable -> [Core.Exp] -> m (Operand, Maybe Operand)
generateBody ctx env symbolTable exps = case exps of
  [exp] -> do
    (_, result, boxed) <- ctxGenerateExp ctx env symbolTable exp
    return (result, boxed)

  (exp : es) -> do
    (symbolTable', val, _) <- ctxGenerateExp ctx env symbolTable exp
    -- If this expression is a ScopeDrop assignment, register the value for rc_dec at scope exit.
    registerDropIfNeeded ctx exp val
    generateBody ctx env symbolTable' es

  [] ->
    error "Unreachable: generateBody called with empty expression list"


-- | After generating a non-last expression, check if it's a ScopeDrop assignment
-- and register the bound value with ctxRegisterScopeDrop.
registerDropIfNeeded :: Monad m => FunctionCtx m -> Core.Exp -> Operand -> m ()
registerDropIfNeeded ctx exp val = case exp of
  Core.Typed _ _ metadata (Core.Assignment _ rhs)
    | Core.isScopeDrop metadata
    , not (isAtomicType (Core.getType rhs)) ->
      ctxRegisterScopeDrop ctx val (Core.getType rhs)
  _ -> return ()


generateExternFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Qual IT.Type -> String -> Int -> Operand -> m SymbolTable
generateExternFunction env symbolTable (ps IT.:=> t) functionName arity foreignFn = do
  let paramTypes    = ([] IT.:=>) <$> IT.getParamTypes t
      amountOfDicts = List.length ps
      totalArity    = arity + amountOfDicts
      params'       = List.replicate totalArity (boxType, NoParameterName)
      functionName' = AST.mkName functionName

  Monad.when (totalArity > maxSupportedPAPArity) $
    error (mkPAPArityError functionName totalArity)

  function <- functionWithMetadata [] functionName' params' boxType $ \params -> do
    block `named` "entry"
    let typesWithParams = List.zip paramTypes (List.drop amountOfDicts params)
    let dictParams      = List.take amountOfDicts params
    unboxedParams <- mapM (uncurry (unbox env symbolTable)) typesWithParams

    -- Generate body
    result <- call foreignFn ((, []) <$> (dictParams ++ unboxedParams))

    -- box the result
    boxed <- box result
    ret boxed

  Writer.tell $ Map.singleton functionName (fnSymbol totalArity function)
  return $ Map.insert functionName (fnSymbol totalArity function) symbolTable


generateFunction :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadModuleBuilder m)
                 => FunctionCtx (IRBuilderT m) -> Env -> SymbolTable -> [Core.Metadata] -> IT.Qual IT.Type -> Area -> String -> [Core String] -> [Core.Exp] -> m SymbolTable
generateFunction ctx env symbolTable metadata (ps IT.:=> t) area functionName coreParams body = do
  let totalArity = List.length coreParams
  Monad.when (totalArity > maxSupportedPAPArity) $
    error (mkPAPArityError functionName totalArity)

  let paramTypes    = (\t' -> IT.selectPredsForType ps t' IT.:=> t') <$> IT.getParamTypes t
      params'       = (boxType,) . makeParamName <$> (Core.getValue <$> coreParams)
      functionName' = AST.mkName functionName
      dictCount     = List.length $ List.filter ("$" `List.isPrefixOf`) (Core.getValue <$> coreParams)

  (env', debugMetadata) <-
    if envIsDebugBuild env then do
      id <- newMetadataId
      let env' = env { envCurrentSubProgramSymbolIndex = Just id }
      let meta = makeDISubprogram env area id functionName
      emitDefn meta
      return (env', [("dbg", MDRef (MetadataNodeID id))])
    else
      return (env, [])

  -- Per-function IORef collecting (operand, madlib-type) pairs to rc_dec at scope exit.
  -- Fresh for each generateFunction call so nested functions don't interfere.
  scopeDropRef <- liftIO $ newIORef []

  let ctx' = ctx
        { ctxRegisterScopeDrop = \op t ->
            liftIO $ modifyIORef scopeDropRef ((op, t) :)
        }

  -- Helper: emit rc_dec for all accumulated scope drops, skipping the return value.
  let emitScopeDrops returnOp = do
        drops <- liftIO $ readIORef scopeDropRef
        mapM_ (\(op, t) ->
          Monad.when (op /= returnOp && not (isAtomicType t)) $
            ctxEmitRCDec ctx' op
          ) drops

  function <- functionWithMetadata debugMetadata functionName' params' boxType $ \params ->
    if Core.isTCODefinition metadata then mdo
      -- NOTE: for TCO definitions, we use ctx (no-op scope drop) rather than ctx'.
      -- Scope drops do not work with TCO loops because the IORef would accumulate
      -- entries across iterations, causing double-frees. TCO param lifetimes are
      -- managed separately via updateTCOArg (Phase C.3).
      entry          <- block `named` "entry"
      continue       <- alloca Type.i1 Nothing 0

      let typesWithParams = List.zip paramTypes params
      unboxedParams <- mapM (uncurry (unbox env' symbolTable)) typesWithParams

      allocatedParams <-
        mapM
          (\(param, coreParam) -> do
            ptr <- alloca (typeOf param) Nothing 0
            storeWithMetadata (makeDILocation env' (Core.getArea coreParam)) ptr 0 param
            return ptr
          )
          (List.zip unboxedParams coreParams)

      recData <-
            if Core.isPlainRecursiveDefinition metadata then do
              return $
                PlainRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = List.drop dictCount allocatedParams
                  }
            else if Core.isInPlaceListRecursiveDefinition metadata then do
              -- In-place list recursion: reuse input list nodes instead of arena allocation.
              -- Every branch produces exactly one output per input element, so we overwrite
              -- the value field of each input node and keep next pointers unchanged.
              -- NOTE: this assumes the input list is not aliased elsewhere.
              let nodeType = Type.StructureType False [boxType, boxType]
              let nonDictParams = List.drop dictCount allocatedParams
              -- Find the param that the where clause destructures (the input list).
              -- For map(f, list) it's list; for concat(a, b) it's a.
              let nonDictCoreParams = List.drop dictCount coreParams
                  nonDictParamNames = Core.getValue <$> nonDictCoreParams
                  scrutineeParamName = case last body of
                    Core.Typed _ _ _ (Core.Where (Core.Typed _ _ _ (Core.Var name _)) _) -> name
                    Core.Typed _ _ _ (Core.Do exps) -> case last exps of
                      Core.Typed _ _ _ (Core.Where (Core.Typed _ _ _ (Core.Var name _)) _) -> name
                      _ -> last nonDictParamNames
                    _ -> last nonDictParamNames
                  inputListParamPtr = case List.elemIndex scrutineeParamName nonDictParamNames of
                    Just idx -> nonDictParams !! idx
                    Nothing  -> last nonDictParams

              -- Load the head of the input list (listType = addrspace(1) ptr)
              start' <- load inputListParamPtr 0

              -- end tracks current write position; starts at head of input list
              end <- alloca listType Nothing 0
              store end 0 start'

              -- Dummy arena fields (unused in in-place path, satisfy the record)
              arenaPtr'   <- alloca (Type.ptr nodeType) Nothing 0
              arenaIndex' <- alloca Type.i64 Nothing 0
              arenaCap'   <- alloca Type.i64 Nothing 0

              -- prevEnd tracks the last processed node so RecursionEnd can link
              -- the base-case expression (e.g. [] or the second arg of ++)
              prevEndPtr   <- alloca listType Nothing 0
              store prevEndPtr 0 start'

              -- anyNodeAdded: dummy for in-place path (prevEnd serves the same role)
              anyNodeAdded' <- alloca Type.i1 Nothing 0
              store anyNodeAdded' 0 (Operand.ConstantOperand (Constant.Int 1 0))

              return $
                RightListRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = nonDictParams
                  , start = start'
                  , end = end
                  , arenaPtr = arenaPtr'
                  , arenaIndex = arenaIndex'
                  , arenaCapacity = arenaCap'
                  , chunkTracker = Nothing
                  , prevEnd = Just prevEndPtr
                  , anyNodeAdded = anyNodeAdded'
                  }
            else if Core.isRightListRecursiveDefinition metadata then do
              let nodeType = Type.StructureType False [boxType, boxType]
              -- Start with a slightly larger chunk to reduce allocator churn
              -- in large right-recursive list builders.
              let initialChunkSize = 32
              let chunkBytes = Operand.ConstantOperand $ Constant.Int 64 (fromIntegral initialChunkSize * 16)  -- each node is 2 pointers = 16 bytes

              -- Keep traced arena chunks for list recursion.
              -- Atomic chunks require a persistent heap-rooted ownership model for
              -- all chunk segments; until that exists, traced chunks are the safe
              -- choice for correctness.
              arena       <- callMallocWithMetadata [] rcAlloc [(chunkBytes, [])]
              arena'      <- ctxSafeBitcast ctx arena (Type.ptr nodeType)
              start'      <- ctxAddrSpaceCast ctx arena listType

              arenaPtr'    <- alloca (Type.ptr nodeType) Nothing 0
              store arenaPtr' 0 arena'

              arenaIndex'  <- alloca Type.i64 Nothing 0
              store arenaIndex' 0 (i64ConstOp 1)  -- index 0 is the start node

              arenaCap'    <- alloca Type.i64 Nothing 0
              store arenaCap' 0 (i64ConstOp (fromIntegral initialChunkSize))

              end            <- alloca listType Nothing 0
              store end 0 start'

              anyNodeAdded'  <- alloca Type.i1 Nothing 0
              store anyNodeAdded' 0 (Operand.ConstantOperand (Constant.Int 1 0))

              return $
                RightListRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = List.drop dictCount allocatedParams
                  , start = start'
                  , end = end
                  , arenaPtr = arenaPtr'
                  , arenaIndex = arenaIndex'
                  , arenaCapacity = arenaCap'
                  , chunkTracker = Nothing
                  , prevEnd = Nothing
                  , anyNodeAdded = anyNodeAdded'
                  }
            else if Core.isConstructorRecursiveDefinition metadata then do
              let returnType = IT.getReturnType t
                  constructedType = retrieveConstructorStructType env' symbolTable returnType
              start  <- callMallocWithMetadata [] rcAlloc [(Operand.ConstantOperand $ sizeof' (Type.StructureType False [Type.i64, constructedType]), [])]
              start' <- ctxSafeBitcast ctx start (Type.ptr (Type.StructureType False [Type.i64, constructedType]))
              end    <- alloca constructedType Nothing 0
              hole   <- gep start' [i32ConstOp 0, i32ConstOp 1]

              holePtr <- alloca (Type.ptr constructedType) Nothing 0
              store holePtr 0 hole

              return $
                ConstructorRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = List.drop dictCount allocatedParams
                  , start = start'
                  , end = end
                  , holePtr = holePtr
                  }
            else if Core.isAdditionRecursiveDefinition metadata || Core.isMultiplicationRecursiveDefinition metadata then do
              let returnType = IT.getReturnType t
              let llvmType = buildLLVMType env' symbolTable ([] IT.:=> returnType)
              holePtr <- alloca llvmType Nothing 0
              let initialValue =
                    if returnType == IT.tInteger then
                      if Core.isMultiplicationRecursiveDefinition metadata then
                        i64ConstOp 1
                      else
                        i64ConstOp 0
                    else if returnType == IT.tByte then
                      if Core.isMultiplicationRecursiveDefinition metadata then
                        i8ConstOp 1
                      else
                        i8ConstOp 0
                    else if returnType == IT.tShort then
                      if Core.isMultiplicationRecursiveDefinition metadata then
                        i32ConstOp 1
                      else
                        i32ConstOp 0
                    else if returnType == IT.tFloat then
                      if Core.isMultiplicationRecursiveDefinition metadata then
                        doubleConstOp 1
                      else
                        doubleConstOp 0
                    else
                      error $ "Unsupported type for arithmetic recursion: " <> show returnType
              store holePtr 0 initialValue

              return $
                ArithmeticRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = List.drop dictCount allocatedParams
                  , holePtr = holePtr
                  }
            else
              error "Unreachable: unrecognized TCO recursion kind"
      -- Disable GC during right-list TRMC loops: the arena allocator only adds
      -- live data (all nodes are reachable via the list chain), so GC scans
      -- during the loop are pure overhead. Re-enabled after the loop exits.
      let isListRecursive = Core.isRightListRecursiveDefinition metadata
                         || Core.isInPlaceListRecursiveDefinition metadata
      Monad.when isListRecursive $ do
        _ <- call gcDisable []
        return ()

      br loop

      loop <- block `named` "loop"
      store continue 0 (Operand.ConstantOperand (Constant.Int 1 0))

      paramsWithNames <- mapM
        (\(Typed paramQt paramArea metadata paramName, allocatedParam, param) ->
            if Core.isReferenceParameter metadata then do
              param' <- ctxSafeBitcast ctx param (Type.ptr boxType)
              loaded <- load param' 0
              unboxed <- unbox env' symbolTable paramQt loaded
              return (paramName, localVarSymbol param unboxed, unboxed)
            else do
              unboxed <- unbox env' symbolTable paramQt param
              Monad.when (envIsDebugBuild env') $ do
                ptr <- alloca (typeOf unboxed) Nothing 0
                declareVariable env' paramArea False paramName ptr
                storeWithMetadata (makeDILocation env' paramArea) ptr 0 unboxed
              return (paramName, tcoParamSymbol allocatedParam unboxed, unboxed)
        )
        (List.zip3 coreParams allocatedParams params)
      let symbolTableWithParams = symbolTable <> Map.fromList (List.map (\(a, b, _) -> (a, b)) paramsWithNames)


      -- Generate body (use ctx — no scope drops in TCO loops)
      (generatedBody, maybeBoxed) <- generateBody ctx env' { recursionData = Just recData } symbolTableWithParams body

      shouldLoop <- load continue 0
      condBr shouldLoop loop afterLoop

      afterLoop <- block `named` "loopExit"

      -- Re-enable GC after right-list TRMC loop
      Monad.when isListRecursive $ do
        _ <- call gcEnable []
        return ()

      case maybeBoxed of
        Just boxed ->
          ret boxed

        Nothing -> do
          -- box the result
          boxed <- box generatedBody
          ret boxed
    else do
      block `named` "entry"

      paramsWithNames <- mapM
        (\(Typed paramQt paramArea metadata paramName, param) ->
            if Core.isReferenceParameter metadata then do
              param' <- ctxSafeBitcast ctx' param (Type.ptr boxType)
              loaded <- load param' 0
              unboxed <- unbox env' symbolTable paramQt loaded
              return (paramName, localVarSymbol param unboxed, unboxed)
            else do
              unboxed <- unbox env' symbolTable paramQt param
              Monad.when (envIsDebugBuild env') $ do
                ptr <- alloca (typeOf unboxed) Nothing 0
                declareVariable env' paramArea False paramName ptr
                storeWithMetadata (makeDILocation env' paramArea) ptr 0 unboxed
              -- Cache the original boxed param (i8*) to avoid re-boxing when forwarding
              return (paramName, Symbol (BoxedVariableSymbol param) unboxed, unboxed)
        )
        (List.zip coreParams params)

      let symbolTableWithParams = symbolTable <> Map.fromList (List.map (\(a, b, _) -> (a, b)) paramsWithNames)

      (generatedBody, _) <- generateBody ctx' env' symbolTableWithParams body

      -- Emit rc_dec for scope-dropped bindings, excluding the return value.
      boxedResult <- box generatedBody
      emitScopeDrops boxedResult

      ret boxedResult

  Writer.tell $ Map.singleton functionName (fnSymbol totalArity function)
  return $ Map.insert functionName (fnSymbol totalArity function) symbolTable


generateTopLevelFunction :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadModuleBuilder m)
                         => FunctionCtx (IRBuilderT m) -> Env -> SymbolTable -> Core.Exp -> m SymbolTable
generateTopLevelFunction ctx env symbolTable topLevelFunction = case topLevelFunction of
  Core.Typed _ area _ (Core.Assignment (Core.Typed _ _ _ (Core.Var functionName _)) (Core.Typed qt _ metadata (Core.Definition params body))) -> do
    symbolTable' <- generateFunction ctx env symbolTable metadata qt area functionName params body
    -- Generate in-place TRMC clone for eligible list-recursive functions.
    let maybeListParamIdx = findListParamForInPlace metadata params body
    case maybeListParamIdx of
      Just listParamIdx -> do
        let inPlaceMetadata = map Core.rewriteMetadataToInPlace metadata
            inPlaceBody = map Core.rewriteExpToInPlace body
            inPlaceName = functionName ++ "__inplace"
            totalArity = List.length params
        symbolTable'' <- generateFunction ctx env symbolTable' inPlaceMetadata qt area inPlaceName params inPlaceBody
        let fnType = Type.ptr $ Type.FunctionType boxType (List.replicate totalArity boxType) False
            fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName inPlaceName))
            inPlaceSymbol = Symbol (InPlaceFunctionSymbol totalArity listParamIdx) fnRef
        Writer.tell $ Map.singleton inPlaceName inPlaceSymbol
        return $ Map.insert inPlaceName inPlaceSymbol symbolTable''
      Nothing ->
        return symbolTable'

  Core.Typed _ _ _ (Core.Extern (ps IT.:=> t) name originalName) -> do
    let paramTypes  = IT.getParamTypes t
        paramTypes' = buildLLVMParamType env symbolTable <$> paramTypes
        dictTypes   = boxType <$ ps
        returnType  = IT.getReturnType t
        returnType' = buildLLVMParamType env symbolTable returnType

    ext <- extern (AST.mkName originalName) (dictTypes ++ paramTypes') returnType'
    generateExternFunction env symbolTable (ps IT.:=> t) name (List.length paramTypes) ext

  _ ->
    return symbolTable


addTopLevelFnToSymbolTable :: Env -> SymbolTable -> Core.Exp -> SymbolTable
addTopLevelFnToSymbolTable env symbolTable topLevelFunction = case topLevelFunction of
  Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var functionName _)) (Core.Typed _ _ metadata (Core.Definition params body))) ->
    let arity  = List.length params
        _ = if arity > maxSupportedPAPArity then error (mkPAPArityError functionName arity) else ()
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
        base   = Map.insert functionName (fnSymbol arity fnRef) symbolTable
    in  case findListParamForInPlace metadata params body of
          Just listParamIdx ->
            let inPlaceName = functionName ++ "__inplace"
                inPlaceFnRef = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName inPlaceName))
            in  Map.insert inPlaceName (Symbol (InPlaceFunctionSymbol arity listParamIdx) inPlaceFnRef) base
          Nothing -> base

  Core.Typed _ _ _ (Core.Extern (_ IT.:=> t) functionName _) ->
    let arity  = List.length $ IT.getParamTypes t
        _ = if arity > maxSupportedPAPArity then error (mkPAPArityError functionName arity) else ()
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Core.Typed qt@(_ IT.:=> t) _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _) ->
    if IT.isFunctionType t then
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType, Type.i8]
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable
    else
      let expType   = buildLLVMType env symbolTable qt
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable

  _ ->
    symbolTable


generateTopLevelFunctions :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadModuleBuilder m)
                          => FunctionCtx (IRBuilderT m) -> Env -> SymbolTable -> [Core.Exp] -> m SymbolTable
generateTopLevelFunctions ctx env symbolTable0 topLevelFunctions' = do
  -- Declare GC_disable/GC_enable once for the module (used by TRMC loop optimization)
  emitDefn $ AST.GlobalDefinition Global.functionDefaults
    { Global.name = AST.mkName "GC_disable", Global.parameters = ([], False), Global.returnType = Type.void }
  emitDefn $ AST.GlobalDefinition Global.functionDefaults
    { Global.name = AST.mkName "GC_enable", Global.parameters = ([], False), Global.returnType = Type.void }
  Monad.foldM step symbolTable0 topLevelFunctions'
 where
  step symbolTable fn = do
    symbolTable' <- generateTopLevelFunction ctx env symbolTable fn
    return $! symbolTable <> symbolTable'


generateConstructor :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m)
                    => Int -> Int -> Env -> SymbolTable -> (Core.Constructor, Int) -> m SymbolTable
generateConstructor maxArity ctorCount env symbolTable (constructor, index) = case constructor of
  Core.Untyped _ _ (Core.Constructor constructorName _ t) -> do
    let paramTypes     = IT.getParamTypes t
    let arity          = List.length paramTypes
    let paramLLVMTypes = (,NoParameterName) <$> List.replicate arity boxType

    Monad.when (arity > maxSupportedPAPArity) $
      error (mkPAPArityError constructorName arity)

    constructor' <- if ctorCount == 1 && arity == 1 then
      -- Newtype: single constructor, single field — identity function
      functionWithMetadata [] (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
        block `named` "entry"
        ret (List.head params)
    else if ctorCount == 1 then do
      -- Single-constructor: no tag field needed
      let structType = Type.StructureType False $ List.replicate maxArity boxType
      let mallocFn   = chooseRCAlloc paramTypes
      functionWithMetadata [] (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
        block `named` "entry"
        structPtr     <- callMallocWithMetadata (makeDILocation env (Core.getArea constructor)) mallocFn [(Operand.ConstantOperand $ sizeof' structType, [])]
        structPtr'    <- bitcast structPtr $ Type.ptr structType
        Monad.foldM_ (storeItem structPtr') () $ List.zip params [0..]
        boxed <- box structPtr'
        ret boxed
    else do
      -- Multi-constructor: struct with tag
      let structType = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType
      let mallocFn   = chooseRCAlloc paramTypes
      functionWithMetadata [] (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
        block `named` "entry"
        structPtr     <- callMallocWithMetadata (makeDILocation env (Core.getArea constructor)) mallocFn [(Operand.ConstantOperand $ sizeof' structType, [])]
        structPtr'    <- bitcast structPtr $ Type.ptr structType
        Monad.foldM_ (storeItem structPtr') () $ List.zip params [1..] ++ [(i64ConstOp (fromIntegral index), 0)]
        boxed <- box structPtr'
        ret boxed

    Writer.tell $ Map.singleton constructorName (constructorSymbol constructor' index arity)
    return $ Map.insert constructorName (constructorSymbol constructor' index arity) symbolTable

  _ ->
    error "Unreachable: generateConstructor called with non-constructor expression"


findMaximumConstructorArity :: [Constructor] -> Int
findMaximumConstructorArity constructors =
  List.foldr max 0 (Core.getConstructorArity <$> constructors)


generateConstructorsForADT :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m)
                           => Env -> SymbolTable -> Core.TypeDecl -> m SymbolTable
generateConstructorsForADT env symbolTable adt = case adt of
  Core.Untyped _ _ Core.ADT { Core.adtconstructors, Core.adtname } -> do
    let sortedConstructors  = List.sortBy (\a b -> compare (getConstructorName a) (getConstructorName b)) adtconstructors
        indexedConstructors = List.zip sortedConstructors [0..]
        maxArity            = findMaximumConstructorArity adtconstructors
        ctorCount           = List.length adtconstructors
        symbolTable'        = Map.insert (envASTPath env <> "_" <> adtname) (adtSymbol maxArity ctorCount) symbolTable
    Writer.tell $ Map.singleton (envASTPath env <> "_" <> adtname) (adtSymbol maxArity ctorCount)
    Monad.foldM (generateConstructor maxArity ctorCount env) symbolTable' indexedConstructors

  _ ->
    return symbolTable


generateConstructors :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m)
                     => Env -> SymbolTable -> [Core.TypeDecl] -> m SymbolTable
generateConstructors env symbolTable tds =
  Monad.foldM (generateConstructorsForADT env) symbolTable tds


expsForMain :: [Core.Exp] -> [Core.Exp]
expsForMain =
  List.filter (not . \e -> isTopLevelFunction e || isExtern e)


topLevelFunctions :: [Core.Exp] -> [Core.Exp]
topLevelFunctions =
  List.filter $ \e -> isTopLevelFunction e || isExtern e
