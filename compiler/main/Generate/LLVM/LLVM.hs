{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import           Generate.LLVM.WithMetadata   (functionWithMetadata, callWithMetadata, callMallocWithMetadata, storeWithMetadata, declareWithAttributes, callWithAttributes)
import           Generate.LLVM.Debug
import           Generate.LLVM.Helper
import           Generate.LLVM.Types          (boxType, listType, stringType, papType, recordType, tConExclude, sizeof', buildLLVMType, buildLLVMType', buildLLVMParamType, retrieveConstructorStructType, retrieveConstructorMaxArity, adtSymbol)
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

constructorSymbol :: Operand -> Int -> Int -> Symbol
constructorSymbol ctor id arity =
  Symbol (ConstructorSymbol id arity) ctor

storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
storeItem basePtr _ (item, index) = do
  ptr <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  store ptr 0 item
  return ()

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
  -- Double the capacity
  newCap <- mul cap (i64ConstOp 2)
  -- Allocate new chunk: newCap * sizeof(Node)
  nodeSize <- Instruction.ptrtoint (Operand.ConstantOperand $ sizeof' nodeType) Type.i64
  chunkBytes <- mul newCap nodeSize
  newArena <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(chunkBytes, [])]
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

-- | Choose GC_malloc or GC_malloc_atomic based on whether all field types are atomic.
chooseMalloc :: [IT.Type] -> Operand
chooseMalloc fieldTypes
  | all isAtomicType fieldTypes = gcMallocAtomic
  | otherwise = gcMalloc


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


emptyList :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> Area -> m Operand
emptyList env area = do
  emptyList  <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' (Type.StructureType False [boxType, boxType]), [])]
  emptyList' <- addrspacecast emptyList listType
  storeWithMetadata (makeDILocation env area) emptyList' 0 (Operand.ConstantOperand $ Constant.Struct Nothing False [Constant.Null boxType, Constant.Null boxType])

  return emptyList'

sanitizeStr s = case s of
  [] ->
    []

  '\\':'"':more ->
    '\\':'"':sanitizeStr more

  '"':more ->
    '\\':'"':sanitizeStr more

  c:more ->
    c:sanitizeStr more

buildStr :: (MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Env -> Area -> String -> m Operand
buildStr _env _area s = do
  let sanitized = sanitizeStr s
  let parsed = read $ "\"" ++ sanitized ++ "\"" :: String
  let asText     = Text.pack parsed
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


retrieveArgs :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [[Core.Metadata]] -> [(SymbolTable, Operand, Maybe Operand)] -> m [Operand]
retrieveArgs metadata exps =
  mapM
    (\(metadata, (_, arg, maybeBoxedArg)) -> case maybeBoxedArg of
      Just boxed | Core.isReferenceArgument metadata ->
        return boxed

      Just _ ->
        box arg

      Nothing ->
        box arg
    )
    (List.zip metadata exps)


generateApplicationForKnownFunction :: (Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Env -> SymbolTable -> Area -> IT.Qual IT.Type -> Int -> Operand -> [Core.Exp] -> m (SymbolTable, Operand, Maybe Operand)
generateApplicationForKnownFunction env symbolTable area returnQualType arity fnOperand args
  | List.length args == arity = do
      -- We have a known call!
      args'   <- mapM (generateExp env { isLast = False } symbolTable) args
      args''  <- retrieveArgs (Core.getMetadata <$> args) args'
      let args''' = (, []) <$> args''

      ret <- callWithMetadata (makeDILocation env area) fnOperand args'''
      unboxed <- unbox env symbolTable returnQualType ret

      return (symbolTable, unboxed, Just ret)
  | List.length args > arity = do
      -- We have extra args so we do the known call and the applyPAP the resulting partial application
      let (args', remainingArgs) = List.splitAt arity args
      args''   <- mapM (generateExp env { isLast = False } symbolTable) args'
      args'''  <- retrieveArgs (Core.getMetadata <$> args') args''
      let args'''' = (, []) <$> args'''

      pap <- callWithMetadata (makeDILocation env area) fnOperand args''''

      let argc = i32ConstOp (fromIntegral $ List.length remainingArgs)
      remainingArgs'  <- mapM (generateExp env { isLast = False } symbolTable) remainingArgs
      remainingArgs'' <- retrieveArgs (Core.getMetadata <$> remainingArgs) remainingArgs'
      let remainingArgs''' = (, []) <$> remainingArgs''

      ret <-
        if List.length remainingArgs''' == 1 then
          callWithMetadata (makeDILocation env area) applyPAP1 $ (pap, []) : remainingArgs'''
        else if List.length remainingArgs''' == 2 then
          callWithMetadata (makeDILocation env area) applyPAP2 $ (pap, []) : remainingArgs'''
        else
          callWithMetadata (makeDILocation env area) applyPAP $ [(pap, []), (argc, [])] ++ remainingArgs'''

      unboxed <- unbox env symbolTable returnQualType ret

      return (symbolTable, unboxed, Just ret)
  | otherwise = do
      -- We don't have enough args, so we create a new PAP
      let papStructType           = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
      let arity'                  = i32ConstOp (fromIntegral arity)
      let argCount                = List.length args
      let amountOfArgsToBeApplied = i32ConstOp (fromIntegral (arity - argCount))
      let envType                 = Type.StructureType False (List.replicate argCount boxType)

      boxedFn  <- box fnOperand

      args'     <- mapM (generateExp env { isLast = False } symbolTable) args
      boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'

      envPtr  <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' envType, [])]
      envPtr' <- safeBitcast envPtr (Type.ptr envType)

      Monad.foldM_
        (\_ (boxed, index, argType, unboxed) ->
          case typeOf unboxed of
            Type.PointerType (Type.StructureType _ [Type.PointerType _ _, Type.IntegerType 32, Type.IntegerType 32, Type.PointerType _ _]) _ | IT.isFunctionType argType && Maybe.isJust (recursionData env) -> do
              unboxed' <- load unboxed 0
              newPAP <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' papStructType, [])]
              newPAP' <- bitcast newPAP papType
              store newPAP' 0 unboxed'
              storeItem envPtr' () (newPAP, index)

            _ ->
              storeItem envPtr' () (boxed, index)
        )
        ()
        $ List.zip4 boxedArgs [0..] (Core.getType <$> args) ((\(_, a, _) -> a) <$> args')

      papPtr  <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' papStructType, [])]
      papPtr' <- safeBitcast papPtr (Type.ptr papStructType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (amountOfArgsToBeApplied, 2), (envPtr, 3)]

      return (symbolTable, papPtr', Just papPtr)


buildReferencePAP :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Env -> Area -> Int -> Operand.Operand -> m (SymbolTable, Operand.Operand, Maybe Operand.Operand)
buildReferencePAP symbolTable env area arity fn = do
  let papType = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
  let arity'  = i32ConstOp (fromIntegral arity)

  boxedFn  <- box fn

  papPtr   <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' papType, [])]
  papPtr'  <- safeBitcast papPtr (Type.ptr papType)
  Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

  return (symbolTable, papPtr', Just papPtr)

-- returns a (SymbolTable, Operand, Maybe Operand) where the maybe operand is a possible boxed value when available
generateExp :: (Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => Env -> SymbolTable -> Core.Exp -> m (SymbolTable, Operand, Maybe Operand)
generateExp env symbolTable exp = case exp of
  Core.Typed _ _ metadata (Core.Call (Core.Typed _ _ _ (Var "+" _)) [(Core.Typed _ _ _ (Core.Call _ recArgs)), arg2])
    | Core.isLeftAdditionRecursiveCall metadata || Core.isLeftMultiplicationRecursiveCall metadata -> do
      let Just params   = boxedParams <$> recursionData env
      let Just holePtr' = holePtr <$> recursionData env
      let Just continue = continueRef <$> recursionData env

      (_, arg2', _) <- generateExp env symbolTable arg2

      args'  <- mapM (generateExp env { isLast = False } symbolTable) recArgs
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

  Core.Typed qt area metadata e | Core.isRightListRecursionEnd metadata -> do
    -- TODO: generate exp without the metadata and append its result to the end
    (_, endList, _) <- generateExp env symbolTable (Core.Typed qt area [] e)
    endValue        <- gep endList [i32ConstOp 0, i32ConstOp 0]
    endValue'       <- load endValue 0
    endNext         <- gep endList [i32ConstOp 0, i32ConstOp 1]
    endNext'        <- load endNext 0

    let Just startOperand = start <$> recursionData env
    let Just endPtr       = end <$> recursionData env
    end' <- load endPtr 0
    storeItem end' () (endValue', 0)
    storeItem end' () (endNext', 1)
    return (symbolTable, startOperand, Nothing)

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

        constructed     <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' structType, [])]
        constructed'    <- safeBitcast constructed constructedType

        -- store the constructor data in the struct
        Monad.foldM_ (storeItem constructed') () $ List.zip args'' [1..] ++ [(i64ConstOp (fromIntegral index), 0)]

        store holePtr'' 0 constructed'

        -- newHole :: i8**
        newHole   <- gep constructed' [i32ConstOp 0, i32ConstOp (fromIntegral $ position + 1)]
        newHole'' <- bitcast newHole (Type.ptr constructedType)
        store holePtr' 0 newHole''

        case args!!position of
          Core.Typed _ area _ (Core.Call _ recArgs) -> do
            let llvmType = buildLLVMType env symbolTable (getQualType exp)
            let Just continue = continueRef <$> recursionData env
            let Just params = boxedParams <$> recursionData env

            storeWithMetadata (makeDILocation env area) continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

            recArgs' <- mapM (generateExp env { isLast = False } symbolTable) recArgs
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
        pap <- callWithMetadata (makeDILocation env area) fnPtr []
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
        let structType = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType

          -- allocate memory for the structure (stack or heap based on escape analysis)
        structPtr     <- allocateStruct env area varMetadata structType gcMalloc
        structPtr'    <- safeBitcast structPtr $ Type.ptr structType

        -- store the constructor data in the struct
        Monad.foldM_ (storeItem structPtr') () [(i64ConstOp (fromIntegral index), 0)]
        return (symbolTable, structPtr', Nothing)

      Just (Symbol (ConstructorSymbol _ arity) fnPtr) -> do
        buildReferencePAP symbolTable env area arity fnPtr

      Just (Symbol (TCOParamSymbol ptr) _) -> do
        loaded <- load ptr 0
        return (symbolTable, loaded, Nothing)

      Just (Symbol _ var) ->
        return (symbolTable, var, Nothing)

      Nothing ->
        error $ "Var not found " <> n <> "\nExp: " <> ppShow exp

  -- TODO: Export nodes are stripped from closure convertion currently, we'll need this back soon.
  -- Core.Typed _ _ _ (Core.Export e) -> do
  --   generateExp env { isLast = False } symbolTable e

  Core.Typed _ _ metadata (Core.Assignment lhs@(Core.Typed _ _ _ (Core.Access r@(Core.Typed (_ IT.:=> recordType) _ _ _) (Core.Typed _ _ _ (Core.Var ('.' : fieldName) _)))) e) -> do
    if Core.isReferenceStore metadata then do
        (_, exp', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable e
        (_, r', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable r
        exp'' <- box exp'

        case recordType of
          IT.TRecord fields _ _ -> do
            recordOperand' <- safeBitcast r' (Type.ptr $ Type.StructureType False [Type.i32, boxType])
            let fieldType = Type.StructureType False [stringType, boxType]
            let index = fromIntegral $ Maybe.fromMaybe 0 (List.elemIndex fieldName (Map.keys fields))
            fieldsOperand   <- gep recordOperand' [i32ConstOp 0, i32ConstOp 1] -- i8**
            fieldsOperand'  <- load fieldsOperand 0 -- i8*
            fieldsOperand'' <- safeBitcast fieldsOperand' (Type.ptr fieldType)
            field           <- gep fieldsOperand'' [i32ConstOp index]
            valuePtr        <- gep field [i32ConstOp 0, i32ConstOp 1]
            store valuePtr 0 exp''
            let unit = Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1)
            return (symbolTable, unit, Nothing)

          or ->
            error $ "found: " <> ppShow or
      else do
        error $ "bad LHS: " <> ppShow lhs

  Core.Typed _ area metadata (Core.Assignment lhs@(Core.Typed _ _ _ (Core.ArrayAccess arr index)) e) -> do
    if Core.isReferenceStore metadata then mdo
      (_, arrOperand, _) <- generateExp env { isLast = False } symbolTable arr
      (_, indexOperand, _) <- generateExp env { isLast = False } symbolTable index
      (_, exp', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable e
      exp'' <- box exp'
      let arrayType = Type.ptr $ Type.StructureType False [i64, i64, Type.ptr $ Type.ptr i8]
      arrOperand' <- safeBitcast arrOperand arrayType

      len <- gep arrOperand' [i32ConstOp 0, i32ConstOp 0]
      len' <- load len 0

      outOfBound <- icmp IntegerPredicate.SGE indexOperand len'
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
      (_, exp', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable e
      let t = typeOf exp'
      g <- global (AST.mkName name) t $ Constant.Undef t
      store g 0 exp'
      Writer.tell $ Map.singleton name (topLevelSymbol g)
      return (Map.insert name (topLevelSymbol g) symbolTable, exp', Nothing)
    else
      if Core.isReferenceAllocation metadata then do
        let expType = buildLLVMType env symbolTable (Core.getQualType exp) --typeOf exp'
            ptrType = Type.ptr expType
        ptr  <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand (sizeof' expType), [])]
        declareVariable env area False name ptr
        ptr' <- safeBitcast ptr ptrType
        v <- load ptr' 0

        (_, exp', _) <-
          generateExp
            env { isLast = False, isTopLevel = False }
            (Map.insert name (localVarSymbol ptr v) symbolTable)
            e

        store ptr' 0 exp'
        return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Just ptr')
      else if Core.isReferenceStore metadata then do
        (_, exp', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable e
        case Map.lookup name symbolTable of
          Just (Symbol (LocalVariableSymbol ptr) _) -> do
            ptr' <- safeBitcast ptr (Type.ptr $ typeOf exp')
            store ptr' 0 exp'
            return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Just ptr)

          -- Case of mutation within a tco optimized function
          Just (Symbol (TCOParamSymbol ptr) _) -> do
            ptr' <- safeBitcast ptr (Type.ptr $ typeOf exp')
            store ptr' 0 exp'
            return (Map.insert name (tcoParamSymbol ptr exp') symbolTable, exp', Just ptr)

          or ->
            error $ "found: " <> ppShow or
      else do
        (_, exp', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable e
        Monad.when (envIsDebugBuild env) $ do
          ptr <- alloca (typeOf exp') Nothing 0
          declareVariable env area False name ptr
          storeWithMetadata (makeDILocation env area) ptr 0 exp'
        return (Map.insert name (varSymbol exp') symbolTable, exp', Nothing)

  Core.Typed (_ IT.:=> t) area _ (Core.Call (Core.Typed _ _ _ (Core.Var fnName _)) [Core.Typed _ _ _ (Core.ListConstructor items)])
    | "fromList" `List.isInfixOf` fnName && IT.getConstructorCon t == IT.tArrayCon && List.all (not . Core.isListSpread) items -> do
      let items' = List.map getListItemExp items
      items'' <- mapM (generateExp env { isLast = False } symbolTable) items'
      let items''' = List.map (\(_, i, _) -> i) items''
      items'''' <- mapM box items'''
      itemsArray <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand (Constant.Mul False False (sizeof' (Type.ptr i8)) (Constant.Int 64 (fromIntegral $ List.length items))), [])]
      itemsArray' <- safeBitcast itemsArray (Type.ptr $ Type.ptr i8)
      let arrayType = Type.StructureType False [i64, i64, Type.ptr $ Type.ptr i8]
      arr <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' arrayType, [])]
      arr' <- safeBitcast arr (Type.ptr arrayType)

      Monad.foldM_ (storeArrayItem itemsArray') () (List.zip items'''' [0..])
      Monad.foldM_ (storeItem arr') () [(i64ConstOp (fromIntegral $ List.length items), 0), (i64ConstOp (fromIntegral $ List.length items), 1), (itemsArray', 2)]

      arr'' <- box arr'
      return (symbolTable, arr'', Nothing)

  Core.Typed (_ IT.:=> t) area _ (Core.Call (Core.Typed _ _ _ (Core.Var fnName _)) [Core.Typed _ _ _ (Core.ListConstructor items)])
    | "fromList" `List.isInfixOf` fnName && t == IT.tByteArray && List.all (not . Core.isListSpread) items -> do
      let items' = List.map getListItemExp items
      items'' <- mapM (generateExp env { isLast = False } symbolTable) items'
      let items''' = List.map (\(_, i, _) -> i) items''
      itemsArray <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand (Constant.Mul False False (sizeof' i8) (Constant.Int 64 (fromIntegral $ List.length items))), [])]
      itemsArray' <- safeBitcast itemsArray (Type.ptr i8)
      let arrayType = Type.StructureType False [i64, i64, Type.ptr i8]
      arr <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' arrayType, [])]
      arr' <- safeBitcast arr (Type.ptr arrayType)

      Monad.foldM_ (storeArrayItem itemsArray') () (List.zip items''' [0..])
      Monad.foldM_ (storeItem arr') () [(i64ConstOp (fromIntegral $ List.length items), 0), (i64ConstOp (fromIntegral $ List.length items), 1), (itemsArray', 2)]

      arr'' <- box arr'
      return (symbolTable, arr'', Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "%" _)) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- srem leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "!" _)) [operand]) -> do
    (_, operand', _) <- generateExp env { isLast = False } symbolTable operand
    result           <- add operand' (Operand.ConstantOperand $ Constant.Int 1 1)
    return (symbolTable, result, Nothing)

  Core.Typed _ area _ (Core.Call (Core.Typed _ _ _ (Core.Var "++" _)) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result <- callWithMetadata (makeDILocation env area) strConcat [(leftOperand', []), (rightOperand', [])]
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "&&" _)) [leftOperand, rightOperand]) -> mdo
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    andLhs <- currentBlock
    condBr leftOperand' andRhs andOutput

    andRhs <- block `named` "and.rhs"
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- Instruction.and leftOperand' rightOperand'
    andRhs'               <- currentBlock
    br andOutput

    andOutput <- block `named` "and.output"
    output <- phi [(leftOperand', andLhs), (result, andRhs')]
    return (symbolTable, output, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "||" _)) [leftOperand, rightOperand]) -> mdo
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    orLhs <- currentBlock
    condBr leftOperand' orOutput orRhs

    orRhs <- block `named` "or.rhs"
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- Instruction.or leftOperand' rightOperand'
    orRhs'                <- currentBlock
    br orOutput

    orOutput <- block `named` "or.output"
    output <- phi [(leftOperand', orLhs), (result, orRhs')]

    return (symbolTable, output, Nothing)

  Core.Typed qt _ metadata (Core.Call fn args) -> case fn of
    Core.Typed _ _ _ (Var "+" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateAdd (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "-" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateSub (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "unary-minus" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      result                <- Ops.generateUnaryMinus (getType (List.head args)) leftOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "*" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateMul (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "/" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateDiv (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "<<" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- shl leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">>" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- ashr leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">>>" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- lshr leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "|" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Instruction.or leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "&" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Instruction.and leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "^" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Instruction.xor leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "~" False) -> do
      (_, operand', _) <- generateExp env { isLast = False } symbolTable (List.head args)
      result <- Ops.generateBitwiseNot (getType (List.head args)) operand'
      return (symbolTable, result, Nothing)

    Core.Typed _ area _ (Var "==" False) | getType (List.head args) `List.elem` [IT.tInteger, IT.tShort, IT.tByte, IT.tFloat, IT.tStr, IT.tBool, IT.tUnit, IT.tChar] -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateEq (callWithMetadata (makeDILocation env area)) (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateGt (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "<" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateLt (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var ">=" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateGte (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    Core.Typed _ _ _ (Var "<=" False) -> do
      (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
      (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
      result                <- Ops.generateLte (getType (List.head args)) leftOperand' rightOperand'
      return (symbolTable, result, Nothing)

    _ | Core.isPlainRecursiveCall metadata -> do
        let llvmType      = buildLLVMType env symbolTable (getQualType exp)
        let Just continue = continueRef <$> recursionData env
        let Just params   = boxedParams <$> recursionData env
        store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

        args'  <- mapM (generateExp env { isLast = False } symbolTable) args
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
          args'   <- mapM (generateExp env { isLast = False } symbolTable) args
          args''  <- retrieveArgs (Core.getMetadata <$> args) args'

          let structType = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType
          -- Use atomic malloc when all fields are primitive and no padding slots exist
          let argTypes   = (\a -> let (_ IT.:=> at') = Core.getQualType a in at') <$> args
          let mallocFn   = if arity == maxArity then chooseMalloc argTypes else gcMalloc

            -- allocate memory for the structure (stack or heap based on escape analysis)
          structPtr     <- allocateStruct env area metadata structType mallocFn
          structPtr'    <- safeBitcast structPtr $ Type.ptr structType

          -- store the constructor data in the struct
          Monad.foldM_ (storeItem structPtr') () $ List.zip args'' [1..] ++ [(i64ConstOp (fromIntegral index), 0)]
          return (symbolTable, structPtr', Nothing)
        else
          generateApplicationForKnownFunction env symbolTable area qt arity fnOperand args

      Just (Symbol (FunctionSymbol arity) fnOperand) ->
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

        args'     <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'

        ret       <-
          if argsApplied == 1 then
            callWithMetadata (makeDILocation env area) applyPAP1 $ [(pap'', [])] ++ ((,[]) <$> boxedArgs)
          else if argsApplied == 2 then
            callWithMetadata (makeDILocation env area) applyPAP2 $ [(pap'', [])] ++ ((,[]) <$> boxedArgs)
          else
            callWithMetadata (makeDILocation env area) applyPAP $ [(pap'', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed   <- unbox env symbolTable qt ret
        return (symbolTable, unboxed, Just ret)

      _ ->
        error $ "Function not found " <> functionName <> "\narea: " <> ppShow area <> "\nST: " <> ppShow symbolTable

    Core.Typed _ area _ _ -> do
      (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
      pap' <- safeBitcast pap boxType

      let argc = i32ConstOp (fromIntegral $ List.length args)

      args'  <- mapM (generateExp env { isLast = False } symbolTable) args
      boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'

      ret <-
        if List.length args == 1 then
            callWithMetadata (makeDILocation env area) applyPAP1 $ [(pap', [])] ++ ((,[]) <$> boxedArgs)
        else if List.length args == 2 then
            callWithMetadata (makeDILocation env area) applyPAP2 $ [(pap', [])] ++ ((,[]) <$> boxedArgs)
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
    (ret, boxed) <- Fn.generateDoExps makeFunctionCtx env { isLast = False } symbolTable exps
    return (symbolTable, ret, boxed)

  Core.Typed _ area metadata (Core.TupleConstructor exps) -> do
    exps'     <- mapM (generateExp env { isLast = False } symbolTable) exps
    boxedExps <- retrieveArgs (Core.getMetadata <$> exps) exps'
    let expsWithIds = List.zip boxedExps [0..]
        tupleType   = Type.StructureType False (typeOf <$> boxedExps)
        elemTypes   = (\e -> let (_ IT.:=> et) = Core.getQualType e in et) <$> exps
        mallocFn    = chooseMalloc elemTypes
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
    ]) | Core.isRightListRecursiveCall metadata -> do
      let Just continue = continueRef <$> recursionData env
      let Just params   = boxedParams <$> recursionData env
      let Just endPtr   = end <$> recursionData env
      endValue <- load endPtr 0

      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

      args'  <- mapM (generateExp env { isLast = False } symbolTable) args
      let unboxedArgs = (\(_, x, _) -> x) <$> args'

      (_, item, maybeBoxedItem) <- generateExp env symbolTable li
      item' <- case maybeBoxedItem of
        Just boxed ->
          return boxed

        Nothing ->
          box item

      newNode <- allocateArenaNode env area
      newNode' <- addrspacecast newNode listType
      storeItem newNode' () (Operand.ConstantOperand (Constant.Null boxType), 0)
      storeItem newNode' () (Operand.ConstantOperand (Constant.Null boxType), 1)
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
      let Just continue = continueRef <$> recursionData env
      let Just params   = boxedParams <$> recursionData env
      let Just endPtr   = end <$> recursionData env
      endValue <- load endPtr 0

      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

      args'  <- mapM (generateExp env { isLast = False } symbolTable) args
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
      storeItem newNode1' () (Operand.ConstantOperand (Constant.Null boxType), 0)
      storeItem newNode1' () (Operand.ConstantOperand (Constant.Null boxType), 1)
      storeItem endValue () (item1', 0)
      storeItem endValue () (newNode1, 1)

      newNode2 <- allocateArenaNode env area2
      newNode2' <- addrspacecast newNode2 listType
      storeItem newNode2' () (Operand.ConstantOperand (Constant.Null boxType), 0)
      storeItem newNode2' () (Operand.ConstantOperand (Constant.Null boxType), 1)
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
    tail <- case List.last listItems of
      Core.Typed _ area _ (Core.ListItem lastItem) -> do
        item <- generateExp env { isLast = False } symbolTable lastItem
        items <- retrieveArgs [Core.getMetadata lastItem] [item]
        callWithMetadata (makeDILocation env area) madlistSingleton [(List.head items, [])]

      Core.Typed _ _ _ (Core.ListSpread spread) -> do
        (_, e, _) <- generateExp env { isLast = False } symbolTable spread
        return e

      _ -> error "Unreachable: list constructor with untyped item"

    list <- Monad.foldM
      (\list' i -> case i of
        Core.Typed _ area _ (Core.ListItem item) -> do
          item' <- generateExp env { isLast = False } symbolTable item
          items <- retrieveArgs [Core.getMetadata item] [item']

          newHead <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' (Type.StructureType False [boxType, boxType]), [])]
          newHead' <- safeBitcast newHead listType
          nextPtr <- gep newHead' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          nextPtr' <- addrspacecast nextPtr (Type.ptr listType)
          storeWithMetadata (makeDILocation env area) nextPtr' 0 list'
          valuePtr <- gep newHead' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]

          storeWithMetadata (makeDILocation env area) valuePtr 0 (List.head items)
          return newHead'

        Core.Typed _ area _ (Core.ListSpread spread) -> do
          (_, spread, _)  <- generateExp env { isLast = False } symbolTable spread
          callWithMetadata (makeDILocation env area) madlistConcat [(spread, []), (list', [])]

        _ -> error "Unreachable: list constructor with untyped item"
      )
      tail
      (List.reverse $ List.init listItems)

    return (symbolTable, list, Nothing)

  Core.Typed _ area _ (Core.Record fields) -> do
    let (base, fields') = List.partition isSpreadField fields
    let fieldCount = i32ConstOp (fromIntegral $ List.length fields')
    let sortedFields = List.sortOn (Maybe.fromMaybe "" . Core.getFieldName) fields'

    base' <- case base of
      [Core.Typed _ _ _ (Core.FieldSpread exp)] -> do
        field  <- generateExp env { isLast = False } symbolTable exp
        fields <- retrieveArgs [Core.getMetadata exp] [field]
        return (List.head fields)

      _ ->
        return $ Operand.ConstantOperand (Constant.Null boxType)

    fields'' <- mapM (generateField symbolTable) sortedFields

    record <- callWithMetadata (makeDILocation env area) buildRecord $ [(fieldCount, []), (base', [])] ++ ((,[]) <$> fields'')
    return (symbolTable, record, Nothing)
    where
      generateField :: (Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m, MonadIO m) => SymbolTable -> Core.Field -> m Operand
      generateField symbolTable field = case field of
        Core.Typed _ area _ (Core.Field (name, value)) -> do
          let fieldType = Type.StructureType False [stringType, boxType]
          nameOperand <- buildStr env area name
          field  <- generateExp env { isLast = False } symbolTable value
          fields <- retrieveArgs [Core.getMetadata value] [field]

          -- TODO: use alloca instead
          fieldPtr    <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' fieldType, [])]
          fieldPtr'   <- safeBitcast fieldPtr (Type.ptr fieldType)

          Monad.foldM_ (storeItem fieldPtr') () [(nameOperand, 0), (List.head fields, 1)]
          return fieldPtr'

        _ ->
          error "Unreachable: record field spread with unexpected field list"

  -- typedef struct madlib__array__Array {
  --   int64_t length;
  --   int64_t capacity;
  --   void **items;
  -- } madlib__array__Array_t;
  Core.Typed qt area _ (Core.ArrayAccess arr index) -> mdo
    (_, arrOperand, _) <- generateExp env { isLast = False } symbolTable arr
    (_, indexOperand, _) <- generateExp env { isLast = False } symbolTable index
    let arrayType = Type.ptr $ Type.StructureType False [i64, i64, Type.ptr $ Type.ptr i8]
    arrOperand' <- safeBitcast arrOperand arrayType

    len <- gep arrOperand' [i32ConstOp 0, i32ConstOp 0]
    len' <- load len 0

    outOfBound <- icmp IntegerPredicate.SGE indexOperand len'
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

  Core.Typed qt _ _ (Core.Access record@(Core.Typed (_ IT.:=> recordType) _ _ _) (Core.Typed _ area _ (Core.Var ('.' : fieldName) _))) -> do
    (_, recordOperand, _) <- generateExp env { isLast = False } symbolTable record
    value <- case recordType of
      IT.TRecord fields _ _ -> do
        recordOperand' <- safeBitcast recordOperand (Type.ptr $ Type.StructureType False [Type.i32, boxType])
        let fieldType = Type.StructureType False [stringType, boxType]
        let index = fromIntegral $ Maybe.fromMaybe 0 (List.elemIndex fieldName (Map.keys fields))
        fieldsOperand   <- gep recordOperand' [i32ConstOp 0, i32ConstOp 1] -- i8**
        fieldsOperand'  <- load fieldsOperand 0 -- i8*
        fieldsOperand'' <- safeBitcast fieldsOperand' (Type.ptr fieldType)
        field           <- gep fieldsOperand'' [i32ConstOp index]
        value           <- gep field [i32ConstOp 0, i32ConstOp 1]
        load value 0

      _ -> do
        nameOperand <- buildStr env area fieldName
        callWithMetadata (makeDILocation env area) selectField [(nameOperand, []), (recordOperand, [])]

    value' <- unbox env symbolTable qt value
    return (symbolTable, value', Just value)


  Core.Typed _ _ _ (Core.If cond truthy falsy) -> mdo
    (symbolTable', cond', _) <- generateExp env { isLast = False } symbolTable cond
    condBr cond' ifThen ifElse

    ifThen <- block `named` "if.then"
    (_, truthy', _) <- generateExp env { isLast = False } symbolTable' truthy
    ifThen' <- currentBlock
    br ifExit

    ifElse <- block `named` "if.else"
    (_, falsy', _) <- generateExp env { isLast = False } symbolTable' falsy
    ifElse' <- currentBlock
    br ifExit

    ifExit <- block `named` "if.exit"
    ret <- phi [(truthy', ifThen'), (falsy', ifElse')]

    return (symbolTable', ret, Nothing)

  Core.Typed _ _ _ (Core.While cond body) -> mdo
    _ <- currentBlock
    br loop

    loop <- block `named` "loop"
    (_, cond', _) <- generateExp env { isLast = False } symbolTable cond
    condBr cond' bodyBlock afterLoop

    bodyBlock <- block `named` "body"
    generateExp env { isLast = False } symbolTable body
    br loop

    afterLoop <- block `named` "loopExit"
    let unit = Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1)
    return (symbolTable, unit, Nothing)

  Core.Typed _ _ _ (Core.Where exp iss) -> mdo
    (_, exp', _) <- generateExp env { isLast = False } symbolTable exp
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
