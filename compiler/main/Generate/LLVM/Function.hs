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
import           Control.Monad.IO.Class       (MonadIO)

import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import           Generate.LLVM.Types          (boxType, listType, papType, sizeof', buildLLVMType, buildLLVMParamType, retrieveConstructorStructType, adtSymbol)
import           Generate.LLVM.Builtins       (i8ConstOp, i32ConstOp, i64ConstOp, doubleConstOp, gcMalloc)
import           Generate.LLVM.Boxing         (box, unbox)
import           Generate.LLVM.WithMetadata   (functionWithMetadata, callWithMetadata, callMallocWithMetadata, storeWithMetadata)
import           Generate.LLVM.Debug
import           Generate.LLVM.Helper
import           GHC.Stack (HasCallStack)

-- | Inbounds GEP — shadows LLVM.IRBuilder.Instruction.gep
gep :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
gep = emitGEP


-- | Callback context to break the circular dependency with LLVM.hs (generateExp lives there).
data FunctionCtx m = FunctionCtx
  { ctxGenerateExp  :: Env -> SymbolTable -> Core.Exp -> m (SymbolTable, Operand, Maybe Operand)
  , ctxSafeBitcast  :: Operand -> Type -> m Operand
  , ctxAddrSpaceCast :: Operand -> Type -> m Operand
  , ctxStoreItem    :: Operand -> () -> (Operand, Integer) -> m ()
  }

-- Symbol constructors (duplicated from LLVM.hs to avoid circular imports)

varSymbol :: Operand -> Symbol
varSymbol = Symbol VariableSymbol

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
    (symbolTable', _, _) <- ctxGenerateExp ctx env symbolTable exp
    generateBody ctx env symbolTable' es

  [] ->
    error "Unreachable: generateBody called with empty expression list"


generateExternFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Qual IT.Type -> String -> Int -> Operand -> m SymbolTable
generateExternFunction env symbolTable (ps IT.:=> t) functionName arity foreignFn = do
  let paramTypes    = ([] IT.:=>) <$> IT.getParamTypes t
      amountOfDicts = List.length ps
      params'       = List.replicate (arity + amountOfDicts) (boxType, NoParameterName)
      functionName' = AST.mkName functionName

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

  Writer.tell $ Map.singleton functionName (fnSymbol (arity + amountOfDicts) function)
  return $ Map.insert functionName (fnSymbol (arity + amountOfDicts) function) symbolTable


generateFunction :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadModuleBuilder m)
                 => FunctionCtx (IRBuilderT m) -> Env -> SymbolTable -> [Core.Metadata] -> IT.Qual IT.Type -> Area -> String -> [Core String] -> [Core.Exp] -> m SymbolTable
generateFunction ctx env symbolTable metadata (ps IT.:=> t) area functionName coreParams body = do
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

  function <- functionWithMetadata debugMetadata functionName' params' boxType $ \params ->
    if Core.isTCODefinition metadata then mdo
      entry          <- block `named` "entry"
      continue       <- alloca Type.i1 Nothing 0

      let typesWithParams = List.zip paramTypes params
      unboxedParams <- mapM (uncurry (unbox env' symbolTable)) typesWithParams

      allocatedParams <-
        mapM
          (\(param, coreParam) -> do
            ptr <- alloca (typeOf param) Nothing 0
            storeWithMetadata (makeDILocation env (Core.getArea coreParam)) ptr 0 param
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
            else if Core.isRightListRecursiveDefinition metadata then do
              let nodeType = Type.StructureType False [boxType, boxType]
              let initialChunkSize = 32
              let chunkBytes = Operand.ConstantOperand $ Constant.Int 64 (fromIntegral initialChunkSize * 16)  -- each node is 2 pointers = 16 bytes

              arena       <- callMallocWithMetadata [] gcMalloc [(chunkBytes, [])]
              arena'      <- ctxSafeBitcast ctx arena (Type.ptr nodeType)
              start'      <- ctxAddrSpaceCast ctx arena listType

              arenaPtr'    <- alloca (Type.ptr nodeType) Nothing 0
              store arenaPtr' 0 arena'

              arenaIndex'  <- alloca Type.i64 Nothing 0
              store arenaIndex' 0 (i64ConstOp 1)  -- index 0 is the start node

              arenaCap'    <- alloca Type.i64 Nothing 0
              store arenaCap' 0 (i64ConstOp (fromIntegral initialChunkSize))

              end         <- alloca listType Nothing 0
              store end 0 start'

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
                  }
            else if Core.isConstructorRecursiveDefinition metadata then do
              let returnType = IT.getReturnType t
                  constructedType = retrieveConstructorStructType env' symbolTable returnType
              start  <- callMallocWithMetadata [] gcMalloc [(Operand.ConstantOperand $ sizeof' (Type.StructureType False [Type.i64, constructedType]), [])]
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
              Monad.when (envIsDebugBuild env) $ do
                ptr <- alloca (typeOf unboxed) Nothing 0
                declareVariable env paramArea False paramName ptr
                storeWithMetadata (makeDILocation env paramArea) ptr 0 unboxed
              return (paramName, tcoParamSymbol allocatedParam unboxed, unboxed)
        )
        (List.zip3 coreParams allocatedParams params)
      let symbolTableWithParams = symbolTable <> Map.fromList (List.map (\(a, b, _) -> (a, b)) paramsWithNames)


      -- Generate body
      (generatedBody, maybeBoxed) <- generateBody ctx env' { recursionData = Just recData } symbolTableWithParams body

      shouldLoop <- load continue 0
      condBr shouldLoop loop afterLoop

      afterLoop <- block `named` "loopExit"

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
              param' <- ctxSafeBitcast ctx param (Type.ptr boxType)
              loaded <- load param' 0
              unboxed <- unbox env' symbolTable paramQt loaded
              return (paramName, localVarSymbol param unboxed, unboxed)
            else do
              unboxed <- unbox env' symbolTable paramQt param
              Monad.when (envIsDebugBuild env) $ do
                ptr <- alloca (typeOf unboxed) Nothing 0
                declareVariable env paramArea False paramName ptr
                storeWithMetadata (makeDILocation env paramArea) ptr 0 unboxed
              -- Cache the original boxed param (i8*) to avoid re-boxing when forwarding
              return (paramName, Symbol (BoxedVariableSymbol param) unboxed, unboxed)
        )
        (List.zip coreParams params)

      let symbolTableWithParams = symbolTable <> Map.fromList (List.map (\(a, b, _) -> (a, b)) paramsWithNames)

      (generatedBody, _) <- generateBody ctx env' symbolTableWithParams body

      boxed <- box generatedBody
      ret boxed

  Writer.tell $ Map.singleton functionName (fnSymbol (List.length coreParams) function)
  return $ Map.insert functionName (fnSymbol (List.length coreParams) function) symbolTable


generateTopLevelFunction :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadModuleBuilder m)
                         => FunctionCtx (IRBuilderT m) -> Env -> SymbolTable -> Core.Exp -> m SymbolTable
generateTopLevelFunction ctx env symbolTable topLevelFunction = case topLevelFunction of
  Core.Typed _ area _ (Core.Assignment (Core.Typed _ _ _ (Core.Var functionName _)) (Core.Typed qt _ metadata (Core.Definition params body))) -> do
    generateFunction ctx env symbolTable metadata qt area functionName params body

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
  Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var functionName _)) (Core.Typed _ _ _ (Core.Definition params _))) ->
    let arity  = List.length params
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Core.Typed _ _ _ (Core.Extern (_ IT.:=> t) functionName _) ->
    let arity  = List.length $ IT.getParamTypes t
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Core.Typed qt@(_ IT.:=> t) _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _) ->
    if IT.isFunctionType t then
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
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
generateTopLevelFunctions ctx env symbolTable0 topLevelFunctions' =
  Monad.foldM step symbolTable0 topLevelFunctions'
 where
  step symbolTable fn = do
    symbolTable' <- generateTopLevelFunction ctx env symbolTable fn
    return $! symbolTable <> symbolTable'


generateConstructor :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m)
                    => Int -> Env -> SymbolTable -> (Core.Constructor, Int) -> m SymbolTable
generateConstructor maxArity env symbolTable (constructor, index) = case constructor of
  Core.Untyped _ _ (Core.Constructor constructorName _ t) -> do
    let paramTypes     = IT.getParamTypes t
    let arity          = List.length paramTypes
    let structType     = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType
    let paramLLVMTypes = (,NoParameterName) <$> List.replicate arity boxType

    constructor' <- functionWithMetadata [] (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
      block `named` "entry"
      structPtr     <- callMallocWithMetadata (makeDILocation env (Core.getArea constructor)) gcMalloc [(Operand.ConstantOperand $ sizeof' structType, [])]
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
        symbolTable'        = Map.insert (envASTPath env <> "_" <> adtname) (adtSymbol maxArity) symbolTable
    Writer.tell $ Map.singleton (envASTPath env <> "_" <> adtname) (adtSymbol maxArity)
    Monad.foldM (generateConstructor maxArity env) symbolTable' indexedConstructors

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
