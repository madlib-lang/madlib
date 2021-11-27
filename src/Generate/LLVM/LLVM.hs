{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Generate.LLVM.LLVM where


import Data.Text.Lazy.IO as T
import           Data.ByteString.Short as ShortByteString
import qualified Data.Map              as Map
import qualified Data.Text             as Text
import qualified Data.Set              as Set
import qualified Data.List             as List
import qualified Data.Maybe            as Maybe
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char              as Char
import qualified Control.Monad          as Monad
import qualified Control.Monad.Fix      as MonadFix
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Writer   as Writer
import qualified Control.Monad.Writer.Class as Writer
import           Data.ByteString as ByteString
import           Data.ByteString.Char8 as Char8
import           System.Process

import           LLVM.Pretty
import           LLVM.Target
import           LLVM.Module
import           LLVM.AST                        as AST hiding (function)
import           LLVM.AST.Type                   as Type
import           LLVM.AST.AddrSpace              as AddrSpace
import           LLVM.AST.ParameterAttribute     as ParameterAttribute
import           LLVM.AST.Typed
import qualified LLVM.AST.Float                  as Float
import qualified LLVM.AST.Constant               as Constant
import qualified LLVM.AST.Operand                as Operand
import qualified LLVM.AST.IntegerPredicate       as IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.AST.Global                 as Global

import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Constant         as C
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction      as Instruction
import           LLVM.Context (withContext)
import           Generate.LLVM.Optimized         as Opt

import qualified Data.String.Utils               as List
import qualified Infer.Type                      as InferredType
import           Infer.Type (isFunctionType)
import           LLVM.PassManager
import           LLVM.CodeGenOpt (Level)
import           LLVM.AST.Constant (Constant(Null))
import qualified LLVM.Prelude as FloatingPointPredicate
import           LLVM.Transforms (Pass(TailCallElimination))
import           Codec.Binary.UTF8.String as UTF8
import           Codec.Binary.UTF8.Generic as GEN
import           Text.Show.Pretty
import           Debug.Trace
import qualified Control.Monad.Fix as Writer
import qualified Utils.Path        as Path
import qualified Data.ByteString.Lazy.Char8    as BLChar8

import qualified Utils.Hash                    as Hash
import LLVM.Internal.ObjectFile (ObjectFile(ObjectFile))
import qualified Data.Tuple as Tuple
import Explain.Location
import System.FilePath (takeDirectory, joinPath)



sizeof :: Type.Type -> Constant.Constant
sizeof t = Constant.PtrToInt szPtr (Type.IntegerType 64)
  where
     ptrType = Type.PointerType t (AddrSpace 0)
     nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
     szPtr   = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]

addrspacecast :: MonadIRBuilder m => Operand -> Type -> m Operand
addrspacecast op t =
  emitInstr t $ AddrSpaceCast op t []


newtype Env
  = Env { dictionaryIndices :: Map.Map String (Map.Map String (Int, Int)) }
  -- ^ Map InterfaceName (Map MethodName (index, arity))
  deriving(Eq, Show)


data SymbolType
  = VariableSymbol
  | LocalVariableSymbol Operand
  -- ^ operand is a ptr to the value for mutation
  | FunctionSymbol Int
  -- ^ arity
  | MethodSymbol Int
  -- ^ arity
  | ConstructorSymbol Int Int
  -- ^ unique id ( index ) | arity
  -- ^ contains the uncurried version of a function to be called if all args are provided and the param count
  | TopLevelAssignment
  -- ^ amount of items in the env
  | DictionarySymbol (Map.Map String Int) -- <- index of the method for each name in the dict
  deriving(Eq, Show)


data Symbol
  = Symbol SymbolType Operand
  deriving(Eq, Show)


type SymbolTable
  = Map.Map String Symbol


varSymbol :: Operand -> Symbol
varSymbol =
  Symbol VariableSymbol

localVarSymbol :: Operand -> Operand -> Symbol
localVarSymbol ptr =
  Symbol (LocalVariableSymbol ptr)

fnSymbol :: Int -> Operand -> Symbol
fnSymbol arity =
  Symbol (FunctionSymbol arity)

methodSymbol :: Int -> Operand -> Symbol
methodSymbol arity =
  Symbol (MethodSymbol arity)

topLevelSymbol :: Operand -> Symbol
topLevelSymbol =
  Symbol TopLevelAssignment

constructorSymbol :: Operand -> Int -> Int -> Symbol
constructorSymbol ctor id arity =
  Symbol (ConstructorSymbol id arity) ctor

isMethodSymbol :: Symbol -> Bool
isMethodSymbol symbol = case symbol of
  Symbol (MethodSymbol _) _ ->
    True

  _ ->
    False

isDictSymbol :: Symbol -> Bool
isDictSymbol symbol = case symbol of
  Symbol (DictionarySymbol _) _ ->
    True

  _ ->
    False




stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack


true :: Operand
true = Operand.ConstantOperand (Constant.Int 1 1)

initEventLoop :: Operand
initEventLoop =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName "__initEventLoop__"))

startEventLoop :: Operand
startEventLoop =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName "__startEventLoop__"))

gcMalloc :: Operand
gcMalloc =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "GC_malloc"))

applyPAP :: Operand
applyPAP =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.ptr Type.i8, Type.i32] True) (AST.mkName "__applyPAP__"))

buildRecord :: Operand
buildRecord =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType recordType [Type.i32, boxType] True) (AST.mkName "__buildRecord__"))

selectField :: Operand
selectField =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [stringType, recordType] False) (AST.mkName "__selectField__"))

madlistHasMinLength :: Operand
madlistHasMinLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.double, listType] False) (AST.mkName "MadList_hasMinLength"))

madlistHasLength :: Operand
madlistHasLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.double, listType] False) (AST.mkName "MadList_hasLength"))

madlistSingleton :: Operand
madlistSingleton =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [Type.ptr Type.i8] False) (AST.mkName "MadList_singleton"))

madlistPush :: Operand
madlistPush =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [Type.ptr Type.i8, listType] False) (AST.mkName "__MadList_push__"))

madlistConcat :: Operand
madlistConcat =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [listType, listType] False) (AST.mkName "MadList_concat"))

malloc :: Operand
malloc =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "malloc"))

strEq :: Operand
strEq =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (AST.mkName "__streq__"))

strConcat :: Operand
strConcat =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType stringType [stringType, stringType] False) (AST.mkName "__strConcat__"))

i32ConstOp :: Integer -> Operand
i32ConstOp i = Operand.ConstantOperand $ Constant.Int 32 i

i64ConstOp :: Integer -> Operand
i64ConstOp i = Operand.ConstantOperand $ Constant.Int 64 i


storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
storeItem basePtr _ (item, index) = do
  ptr <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  store ptr 8 item
  return ()


-- Mostly used for boxing/unboxing and therefore does just one Level
buildLLVMType :: InferredType.Type -> Type.Type
buildLLVMType t = case t of
  InferredType.TCon (InferredType.TC "Float" InferredType.Star) "prelude" ->
    Type.double

  InferredType.TCon (InferredType.TC "Byte" InferredType.Star) "prelude" ->
    Type.i8

  InferredType.TCon (InferredType.TC "Integer" InferredType.Star) "prelude" ->
    Type.i64

  InferredType.TCon (InferredType.TC "String" InferredType.Star) "prelude" ->
    stringType

  InferredType.TCon (InferredType.TC "Boolean" InferredType.Star) "prelude" ->
    Type.i1

  InferredType.TCon (InferredType.TC "()" InferredType.Star) "prelude" ->
    Type.ptr Type.i8

  InferredType.TApp (InferredType.TCon (InferredType.TC "List" (InferredType.Kfun InferredType.Star InferredType.Star)) "prelude") _ ->
    listType

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") left) right ->
    let tLeft  = buildLLVMType left
        tRight = buildLLVMType right
    in  Type.ptr $ Type.FunctionType boxType [boxType] False

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(,)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") a) b ->
    let tA = buildLLVMType a
        tB = buildLLVMType b
    in  Type.ptr $ Type.StructureType False [boxType, boxType]

  _ ->
    Type.ptr Type.i8

buildLLVMParamType :: InferredType.Type -> Type.Type
buildLLVMParamType t = case t of
  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") left) right ->
    papType

  _ ->
    buildLLVMType t


boxType :: Type.Type
boxType =
  Type.ptr Type.i8

listType :: Type.Type
listType =
  Type.PointerType (Type.StructureType False [boxType, boxType]) (AddrSpace 1)

stringType :: Type.Type
stringType =
  Type.PointerType Type.i8 (AddrSpace 1)

papType :: Type.Type
papType =
  Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]

recordType :: Type.Type
recordType =
  Type.ptr $ Type.StructureType False [Type.i32, boxType]


unbox :: (MonadIRBuilder m, MonadModuleBuilder m) => InferredType.Type -> Operand -> m Operand
unbox t what = case t of
  InferredType.TCon (InferredType.TC "Float" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.double
    load ptr 8

  InferredType.TCon (InferredType.TC "Byte" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i8
    load ptr 8

  InferredType.TCon (InferredType.TC "Integer" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i64
    load ptr 8

  InferredType.TCon (InferredType.TC "Boolean" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i1
    load ptr 8

  -- boxed strings are char**
  InferredType.TCon (InferredType.TC "String" _) _ -> do
    ptr <- bitcast what $ Type.ptr stringType
    load ptr 8

  -- boxed lists are { i8*, i8* }**
  InferredType.TApp (InferredType.TCon (InferredType.TC "List" _) _) _ -> do
    -- -- bitcast what listType
    ptr <- bitcast what (Type.ptr listType)
    load ptr 8

  InferredType.TRecord fields _ -> do
    bitcast what recordType


  -- TODO: check that we need this
  -- This should be called for parameters that are closures
  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" _) _) p) b ->
    bitcast what $ Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
    -- return what

  _ ->
    bitcast what (buildLLVMType t)

box :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
box what = case typeOf what of
  -- Float 
  Type.FloatingPointType _ -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof Type.double, [])]
    ptr' <- bitcast ptr (Type.ptr Type.double)
    store ptr' 8 what
    bitcast ptr' boxType

  -- Integer
  Type.IntegerType 64 -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof Type.i64, [])]
    ptr' <- bitcast ptr (Type.ptr Type.i64)
    store ptr' 8 what
    bitcast ptr' boxType

  -- Byte
  Type.IntegerType 8 -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof Type.i8, [])]
    ptr' <- bitcast ptr (Type.ptr Type.i8)
    store ptr' 8 what
    bitcast ptr' boxType

  -- Boolean
  Type.IntegerType 1 -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof Type.i1, [])]
    ptr' <- bitcast ptr (Type.ptr Type.i1)
    store ptr' 8 what
    bitcast ptr' boxType

  -- String
  Type.PointerType (Type.IntegerType 8) (AddrSpace 1) -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof stringType, [])]
    ptr' <- bitcast ptr (Type.ptr stringType)
    store ptr' 8 what
    bitcast ptr' boxType

  -- List
  Type.PointerType (Type.StructureType False [Type.PointerType (Type.IntegerType 8) _, Type.PointerType (Type.IntegerType 8) _]) (AddrSpace 1) -> do
    ptr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof listType, [])]
    ptr' <- bitcast ptr (Type.ptr listType)
    store ptr' 8 what
    bitcast ptr' boxType

  -- Pointless?
  Type.PointerType (Type.IntegerType 8) _ ->
    return what

  -- closure?
  -- t@(Type.PointerType (Type.StructureType _ _) _) -> do
  --   return what

  -- Any pointer type
  _ ->
    bitcast what boxType



buildStr :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => String -> m Operand
buildStr s = do
  let parser = ReadP.readP_to_S $ ReadP.many $ ReadP.readS_to_P Char.readLitChar
  let parsed = fst $ List.last $ parser s
  -- we need to add 0 to terminate a C string
  let charCodes = (fromEnum <$> parsed) ++ [0]
  -- 92, 110 == \n
  let charCodes'   = List.replace [92, 110] [10] charCodes
  -- 92, 34 == \"
  let charCodes''  = List.replace [92, 34] [34] charCodes'
  let charCodes''' = toInteger <$> charCodes
  addr <- call gcMalloc [(i64ConstOp (fromIntegral $ List.length charCodes'''), [])]
  addr' <- addrspacecast addr stringType
  let charCodesWithIds = List.zip charCodes''' [0..]

  Monad.foldM_ (storeChar addr') () charCodesWithIds
  return addr'
  where
    storeChar :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> () -> (Integer, Integer) -> m ()
    storeChar basePtr _ (charCode, index) = do
      ptr  <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 index)]
      store ptr 8 (Operand.ConstantOperand (Constant.Int 8 charCode))
      return ()



-- (Placeholder
  --  ( ClassRef
      --  "Applicative"
      --  [ CRPNode "Semigroup" "List" False []
      --  , CRPNode "Functor" "Identity" False []
      --  ]
      --  True
      --  False
  --  , "WriterT"
  --  )
collectCRPDicts :: (Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> [ClassRefPred] -> m [Operand.Operand]
collectCRPDicts symbolTable crpNodes = case crpNodes of
  (CRPNode interfaceName typingStr _ subNodes : next) ->
    let dictName = "$" <> interfaceName <> "$" <> typingStr
    in  case Map.lookup dictName symbolTable of
          Just (Symbol (DictionarySymbol methodIndices) dict) -> case subNodes of
            [] -> do
              nextNodes <- collectCRPDicts symbolTable next
              return $ dict : nextNodes

            subNodes' -> do
              subNodeDicts <- collectCRPDicts symbolTable subNodes'
              dict'        <- applyClassRefPreds symbolTable (Map.size methodIndices) dict subNodeDicts
              nextNodes    <- collectCRPDicts symbolTable next
              return $ dict' : nextNodes

          _ ->
            undefined

  [] ->
    return []

getMethodsInDict :: (Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Int -> Operand -> m [Operand]
getMethodsInDict symbolTable index dict = case index of
  0 -> do
    method  <- gep dict [i32ConstOp 0, i32ConstOp 0]
    method' <- box method
    return [method']

  n -> do
    method      <- gep dict [i32ConstOp 0, i32ConstOp (fromIntegral n)]
    method'     <- box method
    nextMethods <- getMethodsInDict symbolTable (index - 1) dict
    return $ nextMethods ++ [method']

applyClassRefPreds :: (Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Int -> Operand -> [Operand] -> m Operand
applyClassRefPreds symbolTable methodCount dict classRefPreds = case classRefPreds of
  [] ->
    return dict

  preds -> do
    methods        <- getMethodsInDict symbolTable (methodCount - 1) dict

    classRefPreds'  <- mapM box preds
    appliedMethods  <- mapM (\m -> call applyPAP ([(m, []), (i32ConstOp (fromIntegral $ List.length classRefPreds'), [])] ++ ((,[]) <$> classRefPreds'))) methods
    let papType = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
    appliedMethods' <- mapM (`bitcast` papType) appliedMethods
    appliedMethods'' <- mapM (`load` 8) appliedMethods'

    let appliedDictType = Type.StructureType False (typeOf <$> appliedMethods'')
    appliedDict  <- call gcMalloc [(Operand.ConstantOperand $ sizeof appliedDictType, [])]
    appliedDict' <- bitcast appliedDict (Type.ptr appliedDictType)
    Monad.foldM_ (storeItem appliedDict') () $ List.zip appliedMethods'' [0..]

    return appliedDict'

collectDictArgs :: (Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m ([Operand.Operand], Exp)
collectDictArgs symbolTable exp = case exp of
  Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds True _, typingStr) exp') -> do
    let dictName = "$" <> interfaceName <> "$" <> typingStr
    crpNodes <- collectCRPDicts symbolTable classRefPreds
    (nextArgs, nextExp) <- collectDictArgs  symbolTable exp'
    case Map.lookup dictName symbolTable of
      Just (Symbol (DictionarySymbol methodIndices) dict) -> do
        dict' <- applyClassRefPreds symbolTable (Map.size methodIndices) dict crpNodes
        return (dict' : nextArgs, nextExp)

      Just (Symbol _ dict) ->
        return (dict : nextArgs, nextExp)

      _ ->
        error $ "dict not found: '" <> dictName <> "'"

  _ ->
    return ([], exp)


generateApplicationForKnownFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> InferredType.Type -> Int -> Operand -> [Exp] -> m (SymbolTable, Operand)
generateApplicationForKnownFunction env symbolTable returnType arity fnOperand args
  | List.length args == arity = do
      -- We have a known call!
      args'   <- mapM (generateExp env symbolTable) args
      args''  <- mapM (box . snd) args'
      let args''' = (, []) <$> args''

      ret <- call fnOperand args'''
      unboxed <- unbox returnType ret

      return (symbolTable, unboxed)
  | List.length args > arity = do
      -- We have extra args so we do the known call and the applyPAP the resulting partial application
      let (args', remainingArgs) = List.splitAt arity args
      args''   <- mapM (generateExp env symbolTable) args'
      args'''  <- mapM (box . snd) args''
      let args'''' = (, []) <$> args'''

      pap <- call fnOperand args''''

      let argc = i32ConstOp (fromIntegral $ List.length remainingArgs)
      remainingArgs'  <- mapM (generateExp env symbolTable) remainingArgs
      remainingArgs'' <- mapM (box . snd) remainingArgs'
      let remainingArgs''' = (, []) <$> remainingArgs''

      ret <- call applyPAP $ [(pap, []), (argc, [])] ++ remainingArgs'''
      unboxed <- unbox returnType ret

      return (symbolTable, unboxed)
  | otherwise = do
      -- We don't have enough args, so we create a new PAP
      let papType                 = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
      let arity'                  = i32ConstOp (fromIntegral arity)
      let argCount                = List.length args
      let amountOfArgsToBeApplied = i32ConstOp (fromIntegral (arity - argCount))
      let envType                 = Type.StructureType False (List.replicate argCount boxType)

      boxedFn  <- box fnOperand

      args'  <- mapM (generateExp env symbolTable) args
      let args'' = snd <$> args'
      boxedArgs <- mapM box args''

      envPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof envType, [])]
      envPtr' <- bitcast envPtr (Type.ptr envType)
      Monad.foldM_ (storeItem envPtr') () $ List.zip boxedArgs [0..]

      papPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
      papPtr' <- bitcast papPtr (Type.ptr papType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (amountOfArgsToBeApplied, 2), (envPtr, 3)]

      return (symbolTable, papPtr')


buildReferencePAP :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Int -> Operand.Operand -> m (SymbolTable, Operand.Operand)
buildReferencePAP symbolTable arity fn = do
  let papType = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
  let arity'  = i32ConstOp (fromIntegral arity)

  boxedFn  <- box fn

  papPtr   <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
  papPtr'  <- bitcast papPtr (Type.ptr papType)
  Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

  return (symbolTable, papPtr')


generateExp :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Exp -> m (SymbolTable, Operand)
generateExp env symbolTable exp = case exp of
  Optimized t _ (Var n) ->
    case Map.lookup n symbolTable of
      Just (Symbol (FunctionSymbol 0) fnPtr) -> do
        -- Handle special nullary cases like assignment methods
        pap <- call fnPtr []
        return (symbolTable, pap)

      Just (Symbol (FunctionSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol (MethodSymbol 0) fnPtr) -> do
        -- Handle special nullary cases like assignment methods
        pap <- call fnPtr []
        return (symbolTable, (trace ("NAME: "<>n<>"\nPAP: "<>ppShow pap) pap))

      Just (Symbol (MethodSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol TopLevelAssignment ptr) -> do
        loaded <- load ptr 8
        return (symbolTable, loaded)

      Just (Symbol (LocalVariableSymbol ptr) value) -> do
        return (symbolTable, value)
        -- loaded <- load ptr 8
        -- return (symbolTable, loaded)
        -- case t of
        --   -- InferredType.TCon (InferredType.TC "String" InferredType.Star) _ ->
        --   --   return (symbolTable, ptr)

        --   _ ->
        --     return (symbolTable, value)

      Just (Symbol (ConstructorSymbol _ 0) fnPtr) -> do
        -- Nullary constructors need to be called directly to retrieve the value
        constructed <- call fnPtr []
        return (symbolTable, constructed)

      Just (Symbol (ConstructorSymbol _ arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol _ var) ->
        return (symbolTable, var)

      Nothing ->
        error $ "Var not found " <> n <> "\n\n" <> ppShow symbolTable

  -- (Placeholder (ClassRef "Functor" [] False True , "b183")
  Optimized t _ (Placeholder (ClassRef interface _ False True, typingStr) _) -> do
    let (dictNameParams, innerExp) = gatherAllPlaceholders exp
    let wrapperType = List.foldr InferredType.fn (InferredType.tVar "a") (InferredType.tVar "a" <$ dictNameParams)
    fnName <- freshName (stringToShortByteString "dictionaryWrapper")
    let (Name fnName') = fnName
    symbolTable' <-
      generateFunction
        env
        symbolTable
        False
        wrapperType
        (Char8.unpack (ShortByteString.fromShort fnName'))
        dictNameParams
        [innerExp]
    return (symbolTable', Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType (List.replicate (List.length dictNameParams) boxType) False) fnName))
    where
      gatherAllPlaceholders :: Exp -> ([String], Exp)
      gatherAllPlaceholders ph = case ph of
        Optimized t _ (Placeholder (ClassRef interface _ False True, typingStr) e) ->
          let (nextDictParams, nextExp) = gatherAllPlaceholders e
              dictParamName      = "$" <> interface <> "$" <> typingStr
          in  (dictParamName : nextDictParams, nextExp)

        _ ->
          ([], ph)


  -- (Placeholder (MethodRef "Show" "show" True, "j9")
  Optimized (_ InferredType.:=> t) _ (Placeholder (MethodRef interface methodName True, typingStr) _) -> do
    let dictName = "$" <> interface <> "$" <> typingStr
    case Map.lookup dictName symbolTable of
      Just (Symbol _ dict) -> do
        let interfaceMap   = Maybe.fromMaybe Map.empty $ Map.lookup interface (dictionaryIndices env)
        let (index, arity) = Maybe.fromMaybe (0, 0) $ Map.lookup methodName interfaceMap
        let papType        = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
        dict'      <- bitcast dict (Type.ptr $ Type.StructureType False (List.replicate (Map.size interfaceMap) papType))
        methodPAP  <- gep dict' [i32ConstOp 0, i32ConstOp (fromIntegral index)]

        -- If arity is 0 ( ex: mempty ), we need to call the function and get the value as there will be
        -- no App wrapping the MethodRef. In other words that will never be called and a direct value is
        -- expected
        if arity == 0 then do
          methodFn   <- gep methodPAP [i32ConstOp 0, i32ConstOp 0]
          methodFn'  <- bitcast methodFn (Type.ptr $ Type.ptr $ Type.FunctionType boxType [] False)
          methodFn'' <- load methodFn' 8
          value      <- call methodFn'' (trace ("DICT NAME: "<>dictName<>"\nTYPE: "<>ppShow t<>"\nMETHODFN'': "<>ppShow methodFn'') [])
          return (symbolTable, value)
        else
          return (symbolTable, methodPAP)

      _ ->
        error $ "dict not found: '"<>dictName <>"\n\n"<>ppShow exp

  -- (Placeholder ( ClassRef "Functor" [] True False , "List" )
  Optimized (_ InferredType.:=> t) _ (Placeholder (ClassRef interface _ True _, typingStr) _) -> do
    (dictArgs, fn) <- collectDictArgs symbolTable exp
    (_, pap) <- generateExp env symbolTable fn
    pap' <- bitcast pap boxType
    let argc = i32ConstOp $ fromIntegral (List.length dictArgs)
    dictArgs' <- mapM box dictArgs
    let dictArgs'' = (,[]) <$> dictArgs'
    ret <- call applyPAP $ [(pap', []), (argc, [])] ++ dictArgs''
    unboxed <- unbox t ret
    return (symbolTable, unboxed)

  -- Most likely a method that has to be applied some dictionaries
  Optimized t _ (Placeholder (MethodRef interface methodName False, typingStr) _) -> do
    let methodName' = "$" <> interface <> "$" <> typingStr <> "$" <> methodName
    case Map.lookup methodName' symbolTable of
      Just (Symbol (MethodSymbol arity) fnOperand) -> do
        (_, pap) <- buildReferencePAP symbolTable arity fnOperand
        return (symbolTable, pap)

      _ ->
        error "m"

  Optimized _ _ (Export e) -> do
    generateExp env symbolTable e

  Optimized (ps InferredType.:=> ty) _ (Assignment name e isTopLevel) -> do
    (symbolTable', exp') <- generateExp env symbolTable e

    if isTopLevel then do
      let t = typeOf exp'
      g <- global (AST.mkName name) t $ Constant.Undef t
      store g 8 exp'
      Writer.tell $ Map.singleton name (topLevelSymbol g)
      return (Map.insert name (topLevelSymbol g) symbolTable, exp')
    else
      case Map.lookup name symbolTable of
        Just (Symbol (LocalVariableSymbol ptr) value) ->
          -- TODO: handle strings properly here
          case typeOf exp' of
            Type.PointerType t _ | ty == InferredType.TCon (InferredType.TC "String" InferredType.Star) "prelude" -> do
              ptr' <- bitcast ptr $ Type.ptr stringType
              store ptr' 8 exp'
              return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp')

            Type.PointerType _ _ | InferredType.isListType ty -> do
              ptr' <- bitcast ptr $ Type.ptr listType
              -- exp'' <- addrspacecast exp' listType
              store ptr' 8 exp'
              return (Map.insert name (localVarSymbol ptr' exp') symbolTable, exp')


            Type.PointerType t _  -> do
              ptr'   <- bitcast ptr $ typeOf exp'
              loaded <- load exp' 8
              store ptr' 8 loaded
              return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp')

            _ | InferredType.hasNumberPred ps -> do
              ptr'   <- bitcast ptr $ Type.ptr Type.i64
              exp''  <- bitcast exp' Type.i64
              store ptr' 8 exp''
              return (Map.insert name (localVarSymbol ptr' exp') symbolTable, exp')

            _ -> do
              ptr' <- bitcast ptr (Type.ptr $ typeOf exp')
              store ptr' 8 exp'
              return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp')

        Just (Symbol _ _) ->
          return (Map.insert name (varSymbol exp') symbolTable, exp')

        Nothing -> do
          return (Map.insert name (varSymbol exp') symbolTable, exp')

  Optimized _ _ (App (Optimized _ _ (Var "!")) [operand]) -> do
    (_, operand') <- generateExp env symbolTable operand
    result        <- add operand' (Operand.ConstantOperand $ Constant.Int 1 1)
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "-")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- fsub leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "+")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- fadd leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "*")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- fmul leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "/")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- fdiv leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "++")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- call strConcat [(leftOperand', []), (rightOperand', [])]
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "==")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    -- TODO: add special check for strings, tuples, records etc
    result <-
      case getType leftOperand of
        InferredType.TCon (InferredType.TC "Integer" _) _ ->
          icmp IntegerPredicate.EQ leftOperand' rightOperand'

        _ ->
          fcmp FloatingPointPredicate.OEQ leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "!=")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- fcmp FloatingPointPredicate.ONE leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized (_ InferredType.:=> t) _ (App (Optimized _ _ (Var ">")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- case t of
      InferredType.TCon (InferredType.TC "Float" _) _ ->
        fcmp FloatingPointPredicate.OGT leftOperand' rightOperand'

      InferredType.TCon (InferredType.TC "Byte" _) _ ->
        icmp IntegerPredicate.UGT leftOperand' rightOperand'

      _ ->
        icmp IntegerPredicate.SGT leftOperand' rightOperand'

    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "<")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp env symbolTable leftOperand
    (_, rightOperand') <- generateExp env symbolTable rightOperand
    result             <- fcmp FloatingPointPredicate.OLT leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized (_ InferredType.:=> t) _ (App fn args) -> case fn of
    -- Calling a known method
    Optimized _ _ (Placeholder (MethodRef interface methodName False, typingStr) _) -> case methodName of
      "+" -> case typingStr of
        "Integer" -> do
          (_, leftOperand')  <- generateExp env symbolTable (List.head args)
          (_, rightOperand') <- generateExp env symbolTable (args !! 1)
          result             <- add leftOperand' rightOperand'
          return (symbolTable, result)

        "Float" -> do
          (_, leftOperand')  <- generateExp env symbolTable (List.head args)
          (_, rightOperand') <- generateExp env symbolTable (args!!1)
          result             <- fadd leftOperand' rightOperand'
          return (symbolTable, result)

        _ ->
          undefined

      "-" -> case typingStr of
        "Integer" -> do
          (_, leftOperand')  <- generateExp env symbolTable (List.head args)
          (_, rightOperand') <- generateExp env symbolTable (args !! 1)
          result             <- sub leftOperand' rightOperand'
          return (symbolTable, result)

        "Float" -> do
          (_, leftOperand')  <- generateExp env symbolTable (List.head args)
          (_, rightOperand') <- generateExp env symbolTable (args!!1)
          result             <- fsub leftOperand' rightOperand'
          return (symbolTable, result)

        _ ->
          undefined



      _ -> do
        let dictName    = "$" <> interface <> "$" <> typingStr
        let methodName' = dictName <> "$" <> methodName
        case Map.lookup methodName' symbolTable of
          Just (Symbol (MethodSymbol arity) fnOperand) ->
            generateApplicationForKnownFunction env symbolTable t arity fnOperand args

          _ ->
            error $ "method not found\n\n" <> ppShow symbolTable <> "\nwanted: " <> methodName'

    Optimized _ _ (Opt.Var functionName) -> case Map.lookup functionName symbolTable of
      Just (Symbol (ConstructorSymbol _ arity) fnOperand) ->
        generateApplicationForKnownFunction env symbolTable t arity fnOperand args

      Just (Symbol (FunctionSymbol arity) fnOperand) ->
        generateApplicationForKnownFunction env symbolTable t arity fnOperand args

      Just (Symbol symbolType pap) -> mdo
        -- We apply a partial application
        let argsApplied = List.length args

        pap' <-
          if symbolType == TopLevelAssignment then
            load pap 8
          else
            return pap

        pap'' <- bitcast pap' boxType

        let argc = i32ConstOp (fromIntegral argsApplied)

        args'  <- mapM (generateExp env symbolTable) args
        let args'' = snd <$> args'
        boxedArgs <- mapM box args''

        ret <- call applyPAP $ [(pap'', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox t ret
        return (symbolTable, unboxed)

      _ ->
        error $ "Function not found " <> functionName <> "\n\n" <> ppShow symbolTable

    _ -> case fn of
      Opt.Optimized _ _ (Opt.Placeholder (Opt.MethodRef "Number" _ True, _) _) -> do
        (_, pap) <- generateExp env symbolTable fn
        pap' <- bitcast pap boxType

        let argc = i32ConstOp (fromIntegral $ List.length args)

        args'  <- mapM (generateExp env symbolTable) args
        let args'' = snd <$> args'
        boxedArgs <- mapM box args''

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        ret' <- bitcast ret $ Type.ptr Type.i64
        ret'' <- load ret' 8
        return (symbolTable, ret'')

      _ -> do
        (_, pap) <- generateExp env symbolTable fn
        pap' <- bitcast pap boxType

        let argc = i32ConstOp (fromIntegral $ List.length args)

        args'  <- mapM (generateExp env symbolTable) args
        let args'' = snd <$> args'
        boxedArgs <- mapM box args''

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox t ret
        return (symbolTable, unboxed)

  Optimized (_ InferredType.:=> t) _ (LNum n) -> case t of
    InferredType.TCon (InferredType.TC "Float" _) _ ->
      return (symbolTable, C.double (read n))

    InferredType.TCon (InferredType.TC "Integer" _) _ ->
      return (symbolTable, C.int64 (read n))

    _ ->
      return (symbolTable, C.int64 (read n))

  Optimized _ _ (LFloat n) -> do
    return (symbolTable, C.double (read n))

  Optimized _ _ (LBool b) -> do
    let value =
          if b == "true" then
            1
          else
            0
    return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 value)

  Optimized _ _ LUnit -> do
    return (symbolTable, Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1))

  Optimized _ _ (LStr (leading : s)) | leading == '"' || leading == '\'' -> do
    addr <- buildStr (List.init s)
    return (symbolTable, addr)

  Optimized _ _ (LStr s) -> do
    addr <- buildStr s
    return (symbolTable, addr)

  Optimized _ _ (TemplateString parts) -> do
    parts' <- mapM (generateExp env symbolTable) parts
    let parts'' = snd <$> parts'
    asOne  <- Monad.foldM
      (\prev next -> call strConcat [(prev, []), (next, [])])
      (List.head parts'')
      (List.tail parts'')
    return (symbolTable, asOne)

  Optimized _ _ (Opt.Do exps) -> do
    ret <- generateBody env symbolTable exps
    return (symbolTable, ret)

  Optimized _ _ (TupleConstructor exps) -> do
    exps'     <- mapM ((snd <$>). generateExp env symbolTable) exps
    boxedExps <- mapM box exps'
    let expsWithIds = List.zip boxedExps [0..]
        tupleType   = Type.StructureType False (typeOf <$> boxedExps)
    tuplePtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof tupleType, [])]
    tuplePtr' <- bitcast tuplePtr (Type.ptr tupleType)
    Monad.foldM_ (storeItem tuplePtr') () expsWithIds

    return (symbolTable, tuplePtr')

  Optimized _ _ (ListConstructor []) -> do
    -- an empty list is { value: null, next: null }
    emptyList  <- call gcMalloc [(Operand.ConstantOperand $ sizeof (Type.StructureType False [boxType, boxType]), [])]
    emptyList' <- addrspacecast emptyList listType
    store emptyList' 8 (Operand.ConstantOperand $ Constant.Struct Nothing False [Constant.Null boxType, Constant.Null boxType])

    return (symbolTable, emptyList')

  Optimized _ _ (ListConstructor listItems) -> do
    tail <- case List.last listItems of
      Optimized _ _ (ListItem lastItem) -> do
        (symbolTable', lastItem') <- generateExp env symbolTable lastItem
        lastItem''                <- box lastItem'
        call madlistSingleton [(lastItem'', [])]

      Optimized _ _ (ListSpread spread) -> do
        (_, spread') <- generateExp env symbolTable spread
        return spread'

      cannotHappen ->
        undefined

    list <- Monad.foldM
      (\list' i -> case i of
        Optimized _ _ (ListItem item) -> do
          (_, item') <- generateExp env symbolTable item
          item''     <- box item'
          call madlistPush [(item'', []), (list', [])]

        Optimized _ _ (ListSpread spread) -> do
          (_, spread') <- generateExp env symbolTable spread
          call madlistConcat [((trace ("SPREAD': "<>ppShow spread'<>"\nLIST': "<>ppShow list') spread'), []), (list', [])]

        cannotHappen ->
          undefined
      )
      tail
      (List.reverse $ List.init listItems)

    -- list' <- bitcast list listType
    return (symbolTable, list)

  Optimized _ _ (Record fields) -> do

    let (base, fields') = List.partition isSpreadField fields
    let fieldCount = i32ConstOp (fromIntegral $ List.length fields')

    base' <- case base of
      [Optimized _ _ (FieldSpread exp)] -> do
        (_, exp') <- generateExp env symbolTable exp
        return exp'

      _ ->
        return $ Operand.ConstantOperand (Constant.Null boxType)

    fields'' <- mapM (generateField symbolTable) fields'

    record <- call buildRecord $ [(fieldCount, []), (base', [])] ++ ((,[]) <$> fields'')
    return (symbolTable, record)
    where
      generateField :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Field -> m Operand
      generateField symbolTable field = case field of
        Optimized _ _ (Field (name, value)) -> do
          let fieldType = Type.StructureType False [stringType, boxType]
          nameOperand <- buildStr name -- workaround for now, we need to remove the wrapping "
          (_, value') <- generateExp env symbolTable value
          value''     <- box value'

          fieldPtr    <- call gcMalloc [(Operand.ConstantOperand $ sizeof fieldType, [])]
          fieldPtr'   <- bitcast fieldPtr (Type.ptr fieldType)

          Monad.foldM_ (storeItem fieldPtr') () [(nameOperand, 0), (value'', 1)]
          return fieldPtr'

        _ ->
          undefined

  Optimized (_ InferredType.:=> t) _ (Access record (Optimized _ _ (Var ('.' : fieldName)))) -> do
    nameOperand        <- buildStr fieldName
    (_, recordOperand) <- generateExp env symbolTable record
    value <- call selectField [(nameOperand, []), (recordOperand, [])]

    value' <- unbox t value
    return (symbolTable, value')


  Optimized (_ InferredType.:=> t) _ (If cond truthy falsy) -> mdo
    (symbolTable', cond') <- generateExp env symbolTable cond
    test  <- icmp IntegerPredicate.EQ cond' true
    condBr test truthyBlock falsyBlock

    truthyBlock <- block `named` "truthyBlock"
    (symbolTable'', truthy') <- generateExp env symbolTable' truthy
    -- truthy'' <- box truthy'
    br exitBlock

    falsyBlock <- block `named` "falsyBlock"
    (symbolTable''', falsy') <- generateExp env symbolTable' falsy
    -- falsy'' <- box falsy'
    br exitBlock

    exitBlock <- block `named` "condBlock"
    ret <- phi [(truthy', truthyBlock), (falsy', falsyBlock)]

    -- ret' <- unbox t ret

    return (symbolTable', ret)

  Optimized _ _ (Where exp iss) -> mdo
    (_, exp') <- generateExp env symbolTable exp
    branches  <- generateBranches env symbolTable exitBlock exp' iss

    exitBlock <- block `named` "exitBlock"
    ret <- phi branches

    return (symbolTable, ret)

  Optimized _ _ (TypedExp exp _) ->
    generateExp env symbolTable exp

  Optimized (_ InferredType.:=> t) _ (NameExport n) -> do
    let ref = Operand.ConstantOperand $ Constant.GlobalReference (buildLLVMType t) (AST.mkName n)
    if InferredType.isFunctionType t then do
      let arity = List.length $ InferredType.getParamTypes t
      Writer.tell $ Map.singleton n (fnSymbol arity ref)
    else
      Writer.tell $ Map.singleton n (varSymbol ref)
    return (symbolTable, ref)


  _ ->
    error $ "not implemented\n\n" ++ ppShow exp




generateBranches :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> AST.Name -> Operand -> [Is] -> m [(Operand, AST.Name)]
generateBranches env symbolTable exitBlock whereExp iss = case iss of
  (is : next) -> do
    branch <- generateBranch env symbolTable (not (List.null next)) exitBlock whereExp is
    next'  <- generateBranches env symbolTable exitBlock whereExp next
    return $ branch ++ next'

  [] ->
    return []


generateBranch :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Bool -> AST.Name -> Operand -> Is -> m [(Operand, AST.Name)]
generateBranch env symbolTable hasMore exitBlock whereExp is = case is of
  Optimized _ _ (Is pat exp) -> mdo
    currBlock <- currentBlock
    test <- generateBranchTest symbolTable pat whereExp
    condBr test branchExpBlock nextBlock

    branchExpBlock <- block `named` "branchExpBlock"
    symbolTable' <- generateSymbolTableForPattern symbolTable whereExp pat
    (_, branch) <- generateExp env symbolTable' exp
    -- the exp might contain a where or if expression generating new blocks in between.
    -- therefore we need to get the block that contains the register reference in which
    -- it is defined. 
    retBlock <- currentBlock
    -- branch' <- box branch
    br exitBlock

    (nextBlock, finalPhi) <-
      if hasMore then do
        b <- block `named` "nextBlock"
        return (b, [])
      else do
        let def = Operand.ConstantOperand (Constant.Null (typeOf branch))
        return (exitBlock, [(def, currBlock)])


    return $ (branch, retBlock) : finalPhi

  _ ->
    undefined


generateSymbolTableForIndexedData :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> SymbolTable -> (Pattern, Integer) -> m SymbolTable
generateSymbolTableForIndexedData basePtr symbolTable (pat, index) = do
  ptr  <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
  ptr' <- load ptr 8
  ptr'' <- unbox (getType pat) (trace ("PAT TYPE: "<>ppShow (getType pat)) ptr')
  generateSymbolTableForPattern symbolTable ptr'' pat


generateSymbolTableForList :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Operand -> [Pattern] -> m SymbolTable
generateSymbolTableForList symbolTable basePtr pats = case pats of
  (pat : next) -> case pat of
    Optimized _ _ (PSpread spread) ->
      generateSymbolTableForPattern symbolTable basePtr spread

    _ -> do
      valuePtr      <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
      valuePtr'     <- load valuePtr 8
      valuePtr''    <- unbox (getType pat) valuePtr'
      symbolTable'  <- generateSymbolTableForPattern symbolTable valuePtr'' pat
      nextNodePtr   <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
      -- i8*
      nextNodePtr'  <- load nextNodePtr 8
      -- { i8*, i8* }*
      nextNodePtr'' <- addrspacecast nextNodePtr' listType
      generateSymbolTableForList symbolTable' nextNodePtr'' next

  [] ->
    return symbolTable



generateSymbolTableForPattern :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Operand -> Pattern -> m SymbolTable
generateSymbolTableForPattern symbolTable baseExp pat = case pat of
  Optimized t _ (PVar n) -> do
    return $ Map.insert n (varSymbol baseExp) symbolTable

  Optimized _ _ PAny ->
    return symbolTable

  Optimized _ _ PNum{} ->
    return symbolTable

  Optimized _ _ PBool{} ->
    return symbolTable

  Optimized _ _ PStr{} ->
    return symbolTable

  Optimized _ _ (PTuple pats) -> do
    let patsWithIds = List.zip pats [0..]
    Monad.foldM (generateSymbolTableForIndexedData baseExp) symbolTable patsWithIds

  Optimized _ _ (PList pats) ->
    generateSymbolTableForList symbolTable baseExp pats

  Optimized _ _ (PCon name pats) -> do
    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- bitcast baseExp constructorType
    let patsWithIds = List.zip pats [1..]
    Monad.foldM (generateSymbolTableForIndexedData constructor') symbolTable patsWithIds

  Optimized _ _ (PRecord fieldPatterns) -> do
    subPatterns <- mapM (getFieldPattern baseExp) $ Map.toList fieldPatterns
    Monad.foldM (\previousTable (field, pat) -> generateSymbolTableForPattern previousTable field pat) symbolTable subPatterns

  _ ->
    undefined


generateSubPatternTest :: (MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Operand -> (Pattern, Operand) -> m Operand
generateSubPatternTest symbolTable prev (pat', ptr) = do
  v <- load ptr 8
  v' <- unbox (getType pat') v
  curr <- generateBranchTest symbolTable pat' v'
  prev `Instruction.and` curr


generateListSubPatternTest :: (MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Operand -> [Pattern] -> m Operand
generateListSubPatternTest symbolTable basePtr pats = case pats of
  (pat : next) -> case pat of
    Optimized _ _ (PSpread spread) ->
      return true

    _ -> do
      valuePtr      <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
      valuePtr'     <- load valuePtr 8
      valuePtr''    <- unbox (getType pat) valuePtr'
      test          <- generateBranchTest symbolTable pat valuePtr''
      nextNodePtr   <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
      -- i8*
      nextNodePtr'  <- load nextNodePtr 8
      -- { i8*, i8* }*
      nextNodePtr'' <- addrspacecast nextNodePtr' listType
      nextTest      <- generateListSubPatternTest symbolTable nextNodePtr'' next
      test `Instruction.and` nextTest

  [] ->
    return true


generateBranchTest :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Pattern -> Operand -> m Operand
generateBranchTest symbolTable pat value = case pat of
  Optimized _ _ (PNum n) ->
    fcmp FloatingPointPredicate.OEQ (C.double (read n)) value

  Optimized _ _ (PBool "true") ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 1) value

  Optimized _ _ (PBool _) ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 0) value

  Optimized _ _ (PStr s) -> do
    s' <- buildStr (List.init . List.tail $ s)
    call strEq [(s', []), (value, [])]

  Optimized _ _ PAny ->
    return true

  Optimized _ _ PVar{} ->
    return true

  Optimized _ _ (PTuple pats) -> do
    let indices = List.take (List.length pats) [0..]
    itemPtrs <- getStructPointers indices value
    let patsWithPtrs = List.zip pats itemPtrs
    Monad.foldM (generateSubPatternTest symbolTable) true patsWithPtrs

  Optimized _ _ (PList pats) -> do
    let hasSpread = List.any isSpread pats

    -- test that the length of the given list is at least as long as the pattern items
    lengthTest <-
      if hasSpread then do
        call madlistHasMinLength [(C.double (fromIntegral $ List.length pats - 1), []), (value, [])]
      else
        call madlistHasLength [(C.double (fromIntegral $ List.length pats), []), (value, [])]

    subPatternsTest <- generateListSubPatternTest symbolTable value pats
    lengthTest `Instruction.and` subPatternsTest
    where
      isSpread :: Pattern -> Bool
      isSpread pat = case pat of
        Optimized _ _ PSpread{} ->
          True

        _ ->
          False

  Optimized _ _ (PRecord fieldPatterns) -> do
    subPatterns <- mapM (getFieldPattern value) $ Map.toList fieldPatterns
    subTests    <- mapM (uncurry (generateBranchTest symbolTable) . Tuple.swap) subPatterns
    Monad.foldM Instruction.and (List.head subTests) (List.tail subTests)

  Optimized _ _ (PCon name pats) -> do
    let constructorId = case Map.lookup name symbolTable of
          Just (Symbol (ConstructorSymbol id _) _) ->
            i64ConstOp $ fromIntegral id

          _ ->
            error $ "Constructor '" <> name <> "' not found!"

    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- bitcast value constructorType
    let argIds = fromIntegral <$> List.take (List.length pats) [1..]
    constructorArgPtrs <- getStructPointers argIds constructor'
    let patsWithPtrs = List.zip pats constructorArgPtrs

    id <- gep constructor' [i32ConstOp 0, i32ConstOp 0]
    id' <- load id 8
    testIds <- icmp IntegerPredicate.EQ constructorId id'

    testSubPatterns <- Monad.foldM (generateSubPatternTest symbolTable) true patsWithPtrs

    testIds `Instruction.and` testSubPatterns

  _ ->
    undefined


getFieldPattern :: (MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m) => Operand -> (String, Pattern) -> m (Operand, Pattern)
getFieldPattern record (fieldName, fieldPattern) = do
  nameOperand <- buildStr fieldName
  field       <- call selectField [(nameOperand, []), (record, [])]
  field'      <- unbox (getType fieldPattern) field
  return (field', fieldPattern)


getStructPointers :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [Integer] -> Operand -> m [Operand]
getStructPointers ids ptr = case ids of
  (index : nextIndices) -> do
    ptr'  <- gep ptr [i32ConstOp 0, i32ConstOp index]
    next  <- getStructPointers nextIndices ptr
    return $ ptr' : next

  [] ->
    return []


generateExps :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Exp] -> m ()
generateExps env symbolTable exps = case exps of
  [exp] -> do
    generateExp env symbolTable exp
    return ()

  (exp : es) -> do
    (symbolTable', _) <- generateExp env symbolTable exp
    generateExps env symbolTable' es

  _ ->
    return ()


generateExternFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> InferredType.Type -> String -> Int -> Operand -> m SymbolTable
generateExternFunction symbolTable t functionName arity foreignFn = do
  let paramTypes    = InferredType.getParamTypes t
      params'       = List.replicate arity (boxType, NoParameterName)
      functionName' = AST.mkName functionName

  function <- function functionName' params' boxType $ \params -> do
    let typesWithParams = List.zip paramTypes params
    unboxedParams <- mapM (uncurry unbox) typesWithParams
    -- Generate body
    result <- call foreignFn ((, []) <$> unboxedParams)

    -- box the result
    boxed <- box result
    ret boxed

  return $ Map.insert functionName (fnSymbol arity function) symbolTable


makeParamName :: String -> ParameterName
makeParamName = ParameterName . stringToShortByteString


generateFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Bool -> InferredType.Type -> String -> [String] -> [Exp] -> m SymbolTable
generateFunction env symbolTable isMethod t functionName paramNames body = do
  let paramTypes    = InferredType.getParamTypes t
      params'       = (boxType,) . makeParamName <$> paramNames
      functionName' = AST.mkName functionName

  function <- function functionName' params' boxType $ \params -> mdo
    let typesWithParams = List.zip paramTypes params
    unboxedParams <- mapM (uncurry unbox) typesWithParams
    let paramsWithNames       = Map.fromList $ List.zip paramNames (uncurry localVarSymbol <$> List.zip params unboxedParams)
        symbolTableWithParams = symbolTable <> paramsWithNames

    -- Generate body
    generatedBody <- generateBody env symbolTableWithParams body

    -- box the result
    boxed <- box generatedBody
    ret boxed

  let symbolConstructor =
        if isMethod then
          methodSymbol
        else
          fnSymbol

  Writer.tell $ Map.singleton functionName (symbolConstructor (List.length paramNames) function)
  return $ Map.insert functionName (symbolConstructor (List.length paramNames) function) symbolTable


generateTopLevelFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Exp -> m SymbolTable
generateTopLevelFunction env symbolTable topLevelFunction = case topLevelFunction of
  Optimized (_ InferredType.:=> t) _ (TopLevelAbs functionName params body) -> do
    generateFunction env symbolTable False t functionName params body

  Optimized _ _ (Extern (_ InferredType.:=> t) name originalName) -> do
    let paramTypes  = InferredType.getParamTypes t
        paramTypes' = buildLLVMParamType <$> paramTypes
        returnType  = InferredType.getReturnType t
        returnType' = buildLLVMParamType returnType

    ext <- extern (AST.mkName originalName) paramTypes' returnType'
    generateExternFunction symbolTable t name (List.length paramTypes) ext

  _ ->
    return symbolTable


addTopLevelFnToSymbolTable :: SymbolTable -> Exp -> SymbolTable
addTopLevelFnToSymbolTable symbolTable topLevelFunction = case topLevelFunction of
  Optimized _ _ (TopLevelAbs functionName params _) ->
    let arity  = List.length params
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Optimized _ _ (Extern (_ InferredType.:=> t) functionName originalName) ->
    let arity  = List.length $ InferredType.getParamTypes t
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Optimized (_ InferredType.:=> t) _ (Assignment name exp _) ->
    if isFunctionType t then
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable
    else
      let expType   = buildLLVMType t
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable

  _ ->
    symbolTable


listToIndices :: [a] -> [Int]
listToIndices l | List.null l = []
                | otherwise   = [0 .. List.length l - 1]





generateBody :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Exp] -> m Operand
generateBody env symbolTable exps = case exps of
  [exp] -> do
    (_, result) <- generateExp env symbolTable exp
    return result

  (exp : es) -> do
    (symbolTable', _) <- generateExp env symbolTable exp
    generateBody env symbolTable' es

  _ ->
    undefined



extractEnvArgs :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand.Operand -> [Int] -> m [Operand.Operand]
extractEnvArgs envPtr indices = case indices of
  (index : is) -> do
    itemPtr   <- gep envPtr [i32ConstOp 0, i32ConstOp (fromIntegral index)]
    item      <- load itemPtr 8

    nextItems <- extractEnvArgs envPtr is

    return $ item : nextItems

  _ ->
    return []


getClosureParamNames :: [Exp] -> [String]
getClosureParamNames exps =
  (\(Optimized t _ (Var n)) -> n) <$> exps


generateTopLevelFunctions :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> [Exp] -> m SymbolTable
generateTopLevelFunctions env symbolTable topLevelFunctions = case topLevelFunctions of
  (fn : fns) -> do
    symbolTable' <- generateTopLevelFunction env symbolTable fn
    generateTopLevelFunctions env (symbolTable <> symbolTable') fns

  [] ->
    return symbolTable


{-
type R = R(Maybe Number, List String)

constructor R becomes:
i8* (i8*, i8*)*

internally it builds a struct to represent the constructor:
{ i64, i8*, i8* }
the i64 is the type of constructor ( simply the index )
the two i8* are the content of the created type
-}
-- TODO: still need to generate closured constructors
generateConstructor :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> (Constructor, Int) -> m SymbolTable
generateConstructor symbolTable (constructor, index) = case constructor of
  Untyped _ (Constructor constructorName _ t) -> do
    let paramTypes     = InferredType.getParamTypes t
    let arity          = List.length paramTypes
    let structType     = Type.StructureType False $ Type.IntegerType 64 : List.replicate arity boxType
    let paramLLVMTypes = (,NoParameterName) <$> List.replicate arity boxType

    constructor' <- function (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
    -- allocate memory for the structure
      structPtr     <- call gcMalloc [(Operand.ConstantOperand $ sizeof structType, [])]
      structPtr'    <- bitcast structPtr $ Type.ptr structType

      -- store the constructor data in the struct
      Monad.foldM_ (storeItem structPtr') () $ List.zip params [1..] ++ [(i64ConstOp (fromIntegral index), 0)]

      boxed <- box structPtr'
      ret boxed

    Writer.tell $ Map.singleton constructorName (constructorSymbol constructor' index arity)
    return $ Map.insert constructorName (constructorSymbol constructor' index arity) symbolTable

  _ ->
    undefined


generateConstructorsForADT :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> TypeDecl -> m SymbolTable
generateConstructorsForADT symbolTable adt = case adt of
  Untyped _ ADT { adtconstructors } ->
    let indexedConstructors = List.zip adtconstructors [0..]
    in  Monad.foldM generateConstructor symbolTable indexedConstructors

  _ ->
    return symbolTable


generateConstructors :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> [TypeDecl] -> m SymbolTable
generateConstructors symbolTable tds =
  let adts = List.filter isADT tds
  in  Monad.foldM generateConstructorsForADT symbolTable tds


buildDictValues :: SymbolTable -> [String] -> [Constant.Constant]
buildDictValues symbolTable methodNames = case methodNames of
  (n : ns) ->
    case Map.lookup n symbolTable of
      Just (Symbol _ method) ->
        -- This should be a PAP
        let methodType = typeOf method
            methodRef  = Constant.GlobalReference methodType (AST.mkName n)
            methodRef' = Constant.BitCast methodRef boxType
            arity      = List.length $ getLLVMParameterTypes methodType
            papType    = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
            pap        = Constant.Struct Nothing False [methodRef', Constant.Int 32 (fromIntegral arity), Constant.Int 32 (fromIntegral arity), Constant.Undef boxType]
            next       = buildDictValues symbolTable ns
        in  pap : next

      _ ->
        undefined

  [] ->
    []


generateMethod :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> String -> Exp -> m SymbolTable
generateMethod env symbolTable methodName exp = case exp of
  -- TODO: handle overloaded methods that should be passed a dictionary
  Opt.Optimized (_ InferredType.:=> t) _ (TopLevelAbs _ params body) ->
    generateFunction env symbolTable True t methodName params body

  -- TODO: reconsider this
  Opt.Optimized (_ InferredType.:=> t) _ (Opt.Assignment _ exp _) -> do
    let paramTypes  = InferredType.getParamTypes t
        arity       = List.length paramTypes
        params'     = (,NoParameterName) <$> (boxType <$ paramTypes)

    f <- function (AST.mkName methodName) params' boxType $ \params -> do
      (symbolTable, exp') <- generateExp env symbolTable exp

      retVal <-
        if arity > 0 then do
          pap <- bitcast exp' boxType
          call applyPAP $ [(pap, []), (i32ConstOp (fromIntegral arity), [])] ++ ((,[]) <$> params)
        else do
          box exp'

      ret retVal

    Writer.tell $ Map.singleton methodName (methodSymbol arity f)
    return $ Map.insert methodName (methodSymbol arity f) symbolTable

  _ ->
    undefined


generateInstance :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Instance -> m SymbolTable
generateInstance env symbolTable inst = case inst of
  Untyped _ (Instance interface preds typingStr methods) -> do
    let instanceName = "$" <> interface <> "$" <> typingStr
        prefixedMethods = Map.mapKeys ((instanceName <> "$") <>) methods
        prefixedMethods' = (\(name, method) -> (name, method)) <$> Map.toList prefixedMethods
        prefixedMethodNames = fst <$> prefixedMethods'
    symbolTable' <- Monad.foldM (\symbolTable (name, (method, _)) -> generateMethod env symbolTable name method) symbolTable prefixedMethods'
    let methodConstants = buildDictValues symbolTable' prefixedMethodNames

    dict <- global (AST.mkName instanceName) (Type.StructureType False (typeOf <$> methodConstants)) $ Constant.Struct Nothing False methodConstants

    Writer.tell $ Map.singleton instanceName (Symbol (DictionarySymbol (Map.fromList  $ List.zip (Map.keys methods) [0..])) dict)
    return $ Map.insert instanceName (Symbol (DictionarySymbol (Map.fromList  $ List.zip (Map.keys methods) [0..])) dict) symbolTable'

  _ ->
    undefined



generateInstances :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> [Instance] -> m SymbolTable
generateInstances env =
  Monad.foldM (generateInstance env)


expsForMain :: [Exp] -> [Exp]
expsForMain =
  List.filter (not . \e -> isTopLevelFunction e || isExtern e)


topLevelFunctions :: [Exp] -> [Exp]
topLevelFunctions =
  List.filter $ \e -> isTopLevelFunction e || isExtern e


buildDictionaryIndices :: [Interface] -> Map.Map String (Map.Map String (Int, Int))
buildDictionaryIndices interfaces = case interfaces of
  (Untyped _ (Interface name _ _ methods _) : next) ->
    let nextMap   = buildDictionaryIndices next
        methodMap = Map.fromList
          $ (\((methodName, InferredType.Forall _ (_ InferredType.:=> t)), index) ->
              (methodName, (index, List.length $ InferredType.getParamTypes t))
            ) <$> List.zip (Map.toList methods) [0..]
    in  Map.insert name methodMap nextMap

  _ ->
    Map.empty


getLLVMParameterTypes :: Type.Type -> [Type.Type]
getLLVMParameterTypes t = case t of
  Type.PointerType (Type.FunctionType _ paramTypes _) _ ->
    paramTypes

  _ ->
    undefined

getLLVMReturnType :: Type.Type -> Type.Type
getLLVMReturnType t = case t of
  Type.PointerType (Type.FunctionType returnType _ _) _ ->
    returnType

  _ ->
    undefined


generateExternalForName :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> String -> m ()
generateExternalForName symbolTable name = case Map.lookup name symbolTable of
  Just (Symbol (FunctionSymbol _) symbol) -> do
    let t          = typeOf symbol
        paramTypes = getLLVMParameterTypes t
        returnType = getLLVMReturnType t
    extern (AST.mkName name) paramTypes returnType
    return ()

  Just (Symbol (MethodSymbol _) symbol) -> do
    let t          = typeOf symbol
        paramTypes = getLLVMParameterTypes t
        returnType = getLLVMReturnType t
    extern (AST.mkName name) paramTypes returnType
    return ()

  Just (Symbol (ConstructorSymbol _ _) symbol) -> do
    let t          = typeOf symbol
        paramTypes = getLLVMParameterTypes t
        returnType = getLLVMReturnType t
    extern (AST.mkName name) paramTypes returnType
    return ()

  Just (Symbol TopLevelAssignment symbol) -> do
    let (Type.PointerType t _) = typeOf symbol
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = t }
    let def = AST.GlobalDefinition g
    emitDefn def
    return ()

  Just (Symbol (DictionarySymbol _) symbol) -> do
    let (Type.PointerType t _) = typeOf symbol
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = t }
    let def = AST.GlobalDefinition g
    emitDefn def
    return ()

  Just (Symbol _ symbol) -> do
    let t = typeOf symbol
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = t }
    let def = AST.GlobalDefinition g
    emitDefn def
    return ()

  _ ->
    error $ "import not found\n\n" <> ppShow symbolTable <> "\nlooked for: "<>name


generateExternForImportName :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Optimized String -> m ()
generateExternForImportName symbolTable optimizedName = case optimizedName of
  Untyped _ name ->
    generateExternalForName symbolTable name

  _ ->
    undefined


generateImport :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Import -> m ()
generateImport symbolTable imp = case imp of
  Untyped _ (NamedImport names _ _) ->
    mapM_ (generateExternForImportName symbolTable) names

  _ ->
    undefined

generateExternsForImportedInstances :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => SymbolTable -> m ()
generateExternsForImportedInstances symbolTable = do
  let dictsAndMethods    = Map.filter (\s -> isDictSymbol s || isMethodSymbol s) symbolTable
      dictAndMethodNames = Map.keys dictsAndMethods
  mapM_ (generateExternalForName symbolTable) dictAndMethodNames


generateHashFromPath :: FilePath -> String
generateHashFromPath =
  Hash.hash . BLChar8.pack

hashModulePath :: AST -> String
hashModulePath ast =
  generateHashFromPath $ Maybe.fromMaybe "" (apath ast)


callModuleFunctions :: (MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> [String] -> m ()
callModuleFunctions symbolTable allModuleHashes = case allModuleHashes of
  (hash : next) -> do
    let functionName = "__" <> hash <> "__moduleFunction"
    case Map.lookup functionName symbolTable of
      Just (Symbol _ f) -> do

        call f []

      _ ->
        undefined
    callModuleFunctions symbolTable next

  [] ->
    return ()

generateModuleFunctionExternals :: (MonadModuleBuilder m) => SymbolTable -> [String] -> m ()
generateModuleFunctionExternals symbolTable allModuleHashes = case allModuleHashes of
  (hash : next) -> do
    let functionName = "__" <> hash <> "__moduleFunction"
    case Map.lookup functionName symbolTable of
      Just (Symbol _ f) -> do
        extern (AST.mkName functionName) [] Type.void

      _ ->
        undefined
    generateModuleFunctionExternals symbolTable next

  [] ->
    return ()


-- buildFloatCoercionFunction :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => SymbolTable -> m SymbolTable
-- buildFloatCoercionFunction symbolTable = do
--   toFloat <- extern (AST.mkName "__toFloat__") [boxType] boxType
--   f <- function (AST.mkName "$Number$Float$coerce") [(boxType, NoParameterName)] boxType $ \[intParam] -> do
--     call toFloat [(intParam, [])]
--     return ()

--   let symbolTable' = case Map.lookup "$Number$Float" symbolTable of
--         Just (Symbol (DictionarySymbol) )

--   return symbolTable


buildNumberModule :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => Env -> [String] -> SymbolTable -> m ()
buildNumberModule env currentModuleHashes initialSymbolTable = do
  extern (AST.mkName "__addIntegers__")       [boxType, boxType] (Type.ptr Type.i64)
  extern (AST.mkName "__substractIntegers__") [boxType, boxType] (Type.ptr Type.i64)
  extern (AST.mkName "__multiplyIntegers__")  [boxType, boxType] (Type.ptr Type.i64)
  extern (AST.mkName "__numberToInteger__")   [boxType] boxType

  extern (AST.mkName "__addBytes__")          [boxType, boxType] (Type.ptr Type.i8)
  extern (AST.mkName "__substractBytes__")    [boxType, boxType] (Type.ptr Type.i8)
  extern (AST.mkName "__multiplyBytes__")     [boxType, boxType] (Type.ptr Type.i8)
  extern (AST.mkName "__numberToByte__")      [boxType] boxType

  extern (AST.mkName "__addFloats__")         [boxType, boxType] (Type.ptr Type.double)
  extern (AST.mkName "__substractFloats__")   [boxType, boxType] (Type.ptr Type.double)
  extern (AST.mkName "__multiplyFloats__")    [boxType, boxType] (Type.ptr Type.double)
  extern (AST.mkName "__numberToFloat__")     [boxType] boxType

  let addIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i64) [boxType, boxType] False) "__addIntegers__")
      substractIntegers = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i64) [boxType, boxType] False) "__substractIntegers__")
      multiplyIntegers  = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i64) [boxType, boxType] False) "__multiplyIntegers__")
      numberToInteger   = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "__numberToInteger__")

      addBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType] False) "__addBytes__")
      substractBytes    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType] False) "__substractBytes__")
      multiplyBytes     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [boxType, boxType] False) "__multiplyBytes__")
      numberToByte      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "__numberToByte__")

      addFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.double) [boxType, boxType] False) "__addFloats__")
      substractFloats   = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.double) [boxType, boxType] False) "__substractFloats__")
      multiplyFloats    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.double) [boxType, boxType] False) "__multiplyFloats__")
      numberToFloat     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "__numberToFloat__")

      symbolTableWithCBindings =
        Map.insert "__addFloats__" (fnSymbol 2 addFloats)
        $ Map.insert "__substractFloats__" (fnSymbol 2 substractFloats)
        $ Map.insert "__multiplyFloats__" (fnSymbol 2 multiplyFloats)
        $ Map.insert "__numberToFloat__" (fnSymbol 1 numberToFloat)

        $ Map.insert "__addBytes__" (fnSymbol 2 addFloats)
        $ Map.insert "__substractBytes__" (fnSymbol 2 substractFloats)
        $ Map.insert "__multiplyBytes__" (fnSymbol 2 multiplyFloats)
        $ Map.insert "__numberToByte__" (fnSymbol 1 numberToFloat)

        $ Map.insert "__numberToInteger__" (fnSymbol 1 numberToInteger)
        $ Map.insert "__addIntegers__" (fnSymbol 2 addIntegers)
        $ Map.insert "__substractIntegers__" (fnSymbol 2 substractIntegers)
        $ Map.insert "__multiplyIntegers__" (fnSymbol 2 multiplyIntegers) initialSymbolTable

      numberType           = InferredType.TVar (InferredType.TV "a" InferredType.Star)
      numberPred           = InferredType.IsIn "Number" [numberType] Nothing
      numberQualType       = [numberPred] InferredType.:=> numberType
      addNumbersType       = numberType `InferredType.fn` numberType `InferredType.fn` numberType
      addNumbersQualType   = [numberPred] InferredType.:=> (numberType `InferredType.fn` numberType `InferredType.fn` numberType)
      coerceNumberType     = numberType `InferredType.fn` numberType
      coerceNumberQualType = [numberPred] InferredType.:=> (numberType `InferredType.fn` numberType)
      addIntegersType      = InferredType.tInteger `InferredType.fn` InferredType.tInteger `InferredType.fn` InferredType.tInteger

  let integerNumberInstance =
        Untyped emptyArea
          ( Instance "Number" [] "Integer"
              (Map.fromList
                [ ( "+"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "+" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__addIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "-" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__substractIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "*" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__multiplyIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Optimized coerceNumberQualType emptyArea (TopLevelAbs "__coerceNumber__" ["a"] [
                        Optimized numberQualType emptyArea (App (Optimized coerceNumberQualType emptyArea (Var "__numberToInteger__")) [
                          Optimized numberQualType emptyArea (Var "a")
                        ])
                    ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                ]
              )
          )

  let byteNumberInstance =
        Untyped emptyArea
          ( Instance "Number" [] "Byte"
              (Map.fromList
                [ ( "+"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "+" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__addBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "-" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__substractBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "*" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__multiplyBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Optimized coerceNumberQualType emptyArea (TopLevelAbs "__coerceNumber__" ["a"] [
                        Optimized numberQualType emptyArea (App (Optimized coerceNumberQualType emptyArea (Var "__numberToByte__")) [
                          Optimized numberQualType emptyArea (Var "a")
                        ])
                    ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                ]
              )
          )

  let floatNumberInstance =
        Untyped emptyArea
          ( Instance "Number" [] "Float"
              (Map.fromList
                [ ( "+"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "+" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__addFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Float" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "-" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__substractFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Float" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Optimized addNumbersQualType emptyArea (TopLevelAbs "*" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized addNumbersQualType emptyArea (Var "__multiplyFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Float" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Optimized coerceNumberQualType emptyArea (TopLevelAbs "__coerceNumber__" ["a"] [
                        Optimized numberQualType emptyArea (App (Optimized coerceNumberQualType emptyArea (Var "__numberToFloat__")) [
                          Optimized numberQualType emptyArea (Var "a")
                        ])
                    ])
                    , InferredType.Forall [InferredType.Star] $ [InferredType.IsIn "Number" [InferredType.TGen 0] Nothing] InferredType.:=> (InferredType.TGen 0 `InferredType.fn` InferredType.TGen 0)
                    )
                  )
                ]
              )
          )
  symbolTable'  <- generateInstances env symbolTableWithCBindings [integerNumberInstance, byteNumberInstance, floatNumberInstance]
  return ()


buildModule' :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => Env -> Bool -> [String] -> SymbolTable -> AST -> m ()
buildModule' env isMain currentModuleHashes initialSymbolTable ast = do
  let symbolTableWithTopLevel   = List.foldr (flip addTopLevelFnToSymbolTable) initialSymbolTable (aexps ast)

  mapM_ (generateImport initialSymbolTable) $ aimports ast
  generateExternsForImportedInstances initialSymbolTable

  symbolTable   <- generateConstructors symbolTableWithTopLevel (atypedecls ast)
  symbolTable'  <- generateInstances env symbolTable (ainstances ast)
  symbolTable'' <- generateTopLevelFunctions env symbolTable' (topLevelFunctions $ aexps ast)

  externVarArgs (AST.mkName "__applyPAP__")    [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)

  externVarArgs (AST.mkName "__buildRecord__") [Type.i32, boxType] recordType
  extern (AST.mkName "__selectField__")        [stringType, recordType] boxType
  extern (AST.mkName "__streq__")              [stringType, stringType] Type.i1
  extern (AST.mkName "__strConcat__")          [stringType, stringType] stringType
  extern (AST.mkName "MadList_hasMinLength")   [Type.double, listType] Type.i1
  extern (AST.mkName "MadList_hasLength")      [Type.double, listType] Type.i1
  extern (AST.mkName "MadList_singleton")      [Type.ptr Type.i8] listType
  extern (AST.mkName "__MadList_push__")       [Type.ptr Type.i8, listType] listType
  extern (AST.mkName "MadList_concat")         [listType, listType] listType

  -- void* memcpy( void* dest, const void* src, std::size_t count );
  extern (AST.mkName "GC_malloc")              [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "malloc")                 [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "calloc")                 [Type.i32, Type.i32] (Type.ptr Type.i8)

  Monad.when isMain $ do
    extern (AST.mkName "__initEventLoop__")    [] Type.void
    extern (AST.mkName "__startEventLoop__")   [] Type.void
    generateModuleFunctionExternals symbolTable (Set.toList $ Set.fromList currentModuleHashes)

  let moduleFunctionName =
        if isMain then
          "main"
        else
          "__" <> hashModulePath ast <> "__moduleFunction"

  moduleFunction <- function (AST.mkName moduleFunctionName) [] void $ \_ -> do
    entry <- block `named` "entry";
    Monad.when isMain $ do
      call initEventLoop []
      callModuleFunctions symbolTable (Set.toList $ Set.fromList currentModuleHashes)
    generateExps env symbolTable'' (expsForMain $ aexps ast)
    Monad.when isMain $ do
      call startEventLoop []
      return ()
    retVoid

  Writer.tell $ Map.singleton moduleFunctionName (fnSymbol 0 moduleFunction)
  return ()


toLLVMModule :: Env -> Bool -> [String] -> SymbolTable -> AST -> (AST.Module, SymbolTable)
toLLVMModule initialEnv isMain currentModuleHashes symbolTable ast =
  let moduleName =
        if isMain then
          "main"
        else
          hashModulePath ast
  in  Writer.runWriter $ buildModuleT (stringToShortByteString moduleName) (buildModule' initialEnv isMain currentModuleHashes symbolTable ast)


generateAST :: Bool -> Table -> (ModuleTable, SymbolTable, [String], Env) -> AST -> (ModuleTable, SymbolTable, [String], Env)
generateAST isMain astTable (moduleTable, symbolTable, processedHashes, initialEnv) ast@AST{ apath = Just apath } =
  let imports                          = aimports ast
      alreadyProcessedPaths            = Map.keys moduleTable
      importPathsToProcess             = List.filter (`List.notElem` alreadyProcessedPaths) $ getImportAbsolutePath <$> imports
      astsForImports                   = Maybe.mapMaybe (`Map.lookup` astTable) importPathsToProcess
      (moduleTableWithImports, symbolTableWithImports, importHashes, envFromImports) =
        List.foldl (generateAST False astTable) (moduleTable, symbolTable, processedHashes, initialEnv) astsForImports

      computedDictionaryIndices        = buildDictionaryIndices $ ainterfaces ast
      envForAST                        = Env { dictionaryIndices = computedDictionaryIndices <> dictionaryIndices envFromImports }
      (newModule, newSymbolTableTable) = toLLVMModule envForAST isMain (processedHashes ++ importHashes) symbolTableWithImports ast

      updatedModuleTable               = Map.insert apath newModule moduleTableWithImports
      moduleHash                       = hashModulePath ast
  in  (updatedModuleTable, symbolTableWithImports <> newSymbolTableTable, processedHashes ++ importHashes ++ [moduleHash], envForAST)

generateAST _ _ _ _ =
  undefined



type ModuleTable = Map.Map FilePath AST.Module

{-
  Generates all modules

  generatedTable: the already generated modules
  symbolTable: accumulated symbolTable, used to map imports to extern instructions
  astTable: input asts to generate
  entrypoint: main module to generate
-}
generateTableModules :: ModuleTable -> SymbolTable -> Table -> FilePath -> ModuleTable
generateTableModules generatedTable symbolTable astTable entrypoint = case Map.lookup entrypoint astTable of
  Just ast ->
    let (numbersModule, symbolTable') = Writer.runWriter $ buildModuleT (stringToShortByteString "number") (buildNumberModule Env { dictionaryIndices = Map.empty } [] symbolTable)
        numbersModulePath = joinPath [takeDirectory entrypoint, "default", "numbers.mad"]
        dictionaryIndices = Map.fromList [ ("Number", Map.fromList [("*", (0, 2)), ("+", (1, 2)), ("-", (2, 2)), ("__coerceNumber__", (3, 1))]) ]
        (moduleTable, _, _, _) = generateAST True astTable (generatedTable, symbolTable', [], Env { dictionaryIndices = dictionaryIndices }) ast
    in  Map.insert numbersModulePath numbersModule moduleTable

  _ ->
    undefined


compileModule :: FilePath -> FilePath -> FilePath -> AST.Module -> IO FilePath
compileModule outputFolder rootPath astPath astModule = do
  let outputPath = Path.computeLLVMTargetPath outputFolder rootPath astPath

  Prelude.putStrLn outputPath
  T.putStrLn $ ppllvm astModule

  withHostTargetMachineDefault $ \target -> do
    withContext $ \ctx -> do
      withModuleFromAST ctx astModule $ \mod' -> do
        mod'' <- withPassManager defaultCuratedPassSetSpec { optLevel = Just 1 } $ \pm -> do
          runPassManager pm mod'
          return mod'
        writeObjectToFile target (File outputPath) mod''

  return outputPath

generateTable :: FilePath -> FilePath -> Table -> FilePath -> IO ()
generateTable outputFolder rootPath astTable entrypoint = do
  let moduleTable  = generateTableModules Map.empty Map.empty astTable entrypoint
  objectFilePaths <- mapM (uncurry $ compileModule outputFolder rootPath) $ Map.toList moduleTable

  let objectFilePathsForCli = List.unwords objectFilePaths

  callCommand $ "clang++ -foptimize-sibling-calls -g -stdlib=libc++ -v " <> objectFilePathsForCli <> " ./c-bindings/lib/libgc.a ./c-bindings/lib/libuv.a ./c-bindings/build/lib.a -o a.out"
