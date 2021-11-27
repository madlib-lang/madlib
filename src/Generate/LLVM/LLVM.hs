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
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as TextEncoding
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
import qualified Infer.Type                      as IT
import           Infer.Type (isFunctionType)
import           LLVM.PassManager
import           LLVM.CodeGenOpt (Level)
import           LLVM.AST.Constant (Constant(Null))
import qualified LLVM.Prelude as FloatingPointPredicate
import           LLVM.Transforms (Pass(TailCallElimination), defaultGCOVProfiler)
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
import System.FilePath (takeDirectory, joinPath, takeFileName, dropExtension)
import qualified LLVM.AST.Linkage as Linkage
import System.Directory



sizeof :: Type.Type -> Constant.Constant
sizeof t = Constant.PtrToInt szPtr (Type.IntegerType 64)
  where
     ptrType = Type.PointerType t (AddrSpace 0)
     nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
     szPtr   = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]

addrspacecast :: MonadIRBuilder m => Operand -> Type -> m Operand
addrspacecast op t =
  emitInstr t $ AddrSpaceCast op t []


data Env
  = Env { dictionaryIndices :: Map.Map String (Map.Map String (Int, Int)), isLast :: Bool }
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

dictCtor :: Operand
dictCtor =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) (AST.mkName "__dict_ctor__"))

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

areStringsEqual :: Operand
areStringsEqual =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (AST.mkName "__areStringsEqual__"))

areStringsNotEqual :: Operand
areStringsNotEqual =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (AST.mkName "__areStringsNotEqual__"))

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
buildLLVMType :: IT.Type -> Type.Type
buildLLVMType t = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" ->
    Type.double

  IT.TCon (IT.TC "Byte" IT.Star) "prelude" ->
    Type.i8

  IT.TCon (IT.TC "Integer" IT.Star) "prelude" ->
    Type.i64

  IT.TCon (IT.TC "String" IT.Star) "prelude" ->
    stringType

  IT.TCon (IT.TC "Boolean" IT.Star) "prelude" ->
    Type.i1

  IT.TCon (IT.TC "()" IT.Star) "prelude" ->
    Type.ptr Type.i1

  IT.TApp (IT.TCon (IT.TC "List" (IT.Kfun IT.Star IT.Star)) "prelude") _ ->
    listType

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude") left) right ->
    let tLeft  = buildLLVMType left
        tRight = buildLLVMType right
    in  Type.ptr $ Type.FunctionType boxType [boxType] False

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(,)" _) "prelude") _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,)" _) "prelude") _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,)" _) "prelude") _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,,)" _) "prelude") _) _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,,,)" _) "prelude") _) _) _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,,,,)" _) "prelude") _) _) _) _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType, boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,,,,,)" _) "prelude") _) _) _) _) _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType, boxType, boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,,,,,,)" _) "prelude") _) _) _) _) _) _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType, boxType, boxType, boxType, boxType, boxType]

  IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TApp (IT.TCon (IT.TC "(,,,,,,,,,)" _) "prelude") _) _) _) _) _) _) _) _) _) _ ->
    Type.ptr $ Type.StructureType False [boxType, boxType, boxType, boxType, boxType, boxType, boxType, boxType, boxType, boxType]

  _ ->
    Type.ptr Type.i8

buildLLVMParamType :: IT.Type -> Type.Type
buildLLVMParamType t = case t of
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude") left) right ->
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


unbox :: (MonadIRBuilder m, MonadModuleBuilder m) => IT.Type -> Operand -> m Operand
unbox t what = case t of
  IT.TCon (IT.TC "Float" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.double
    load ptr 8

  IT.TCon (IT.TC "Byte" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i8
    load ptr 8

  IT.TCon (IT.TC "Integer" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i64
    load ptr 8

  IT.TCon (IT.TC "Boolean" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i1
    load ptr 8

  -- boxed strings are char**
  IT.TCon (IT.TC "String" _) _ -> do
    ptr <- bitcast what $ Type.ptr stringType
    load ptr 8

  IT.TCon (IT.TC "Unit" _) _ -> do
    bitcast what $ Type.ptr Type.i1

  -- boxed lists are { i8*, i8* }**
  IT.TApp (IT.TCon (IT.TC "List" _) _) _ -> do
    -- -- bitcast what listType
    ptr <- bitcast what (Type.ptr listType)
    load ptr 8

  IT.TRecord fields _ -> do
    bitcast what recordType

  -- This should be called for parameters that are closures or returned closures
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" _) _) p) b ->
    bitcast what papType

  -- That handles tuple types
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
  let parser     = ReadP.readP_to_S $ ReadP.many $ ReadP.readS_to_P Char.readLitChar
      parsed     = fst $ List.last $ parser s
      asText     = Text.pack parsed
      bs         = TextEncoding.encodeUtf8 asText
      bytes      = ByteString.unpack bs
      charCodes  = (fromEnum <$> bytes) ++ [0]
      charCodes' = toInteger <$> charCodes
  addr  <- call gcMalloc [(i64ConstOp (fromIntegral $ List.length charCodes'), [])]
  addr' <- addrspacecast addr stringType

  let charCodesWithIds = List.zip charCodes' [0..]

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
            error $ "dict not found: "<>dictName

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


retrieveArgs :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [(SymbolTable, Operand, Maybe Operand)] -> m [Operand]
retrieveArgs =
  mapM (\(_, arg, maybeBoxedArg) -> case maybeBoxedArg of
          Just boxed ->
            return boxed

          Nothing ->
            box arg
       )


generateApplicationForKnownFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Type -> Int -> Operand -> [Exp] -> m (SymbolTable, Operand, Maybe Operand)
generateApplicationForKnownFunction env symbolTable returnType arity fnOperand args
  | List.length args == arity = do
      -- We have a known call!
      args'   <- mapM (generateExp env { isLast = False } symbolTable) args
      args''  <- retrieveArgs args'
      let args''' = (, []) <$> args''

      ret <- call fnOperand args'''
      unboxed <- unbox returnType ret

      return (symbolTable, unboxed, Just ret)
  | List.length args > arity = do
      -- We have extra args so we do the known call and the applyPAP the resulting partial application
      let (args', remainingArgs) = List.splitAt arity args
      args''   <- mapM (generateExp env { isLast = False } symbolTable) args'
      args'''  <- retrieveArgs args''
      let args'''' = (, []) <$> args'''

      pap <- call fnOperand args''''

      let argc = i32ConstOp (fromIntegral $ List.length remainingArgs)
      remainingArgs'  <- mapM (generateExp env { isLast = False } symbolTable) remainingArgs
      remainingArgs'' <- retrieveArgs remainingArgs'
      let remainingArgs''' = (, []) <$> remainingArgs''

      ret <- call applyPAP $ [(pap, []), (argc, [])] ++ remainingArgs'''
      unboxed <- unbox returnType ret

      return (symbolTable, unboxed, Just ret)
  | otherwise = do
      -- We don't have enough args, so we create a new PAP
      let papType                 = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
      let arity'                  = i32ConstOp (fromIntegral arity)
      let argCount                = List.length args
      let amountOfArgsToBeApplied = i32ConstOp (fromIntegral (arity - argCount))
      let envType                 = Type.StructureType False (List.replicate argCount boxType)

      boxedFn  <- box fnOperand

      args'  <- mapM (generateExp env { isLast = False } symbolTable) args
      boxedArgs <- retrieveArgs args'

      envPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof envType, [])]
      envPtr' <- bitcast envPtr (Type.ptr envType)
      Monad.foldM_ (storeItem envPtr') () $ List.zip boxedArgs [0..]

      papPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
      papPtr' <- bitcast papPtr (Type.ptr papType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (amountOfArgsToBeApplied, 2), (envPtr, 3)]

      return (symbolTable, papPtr', Just papPtr)


buildReferencePAP :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Int -> Operand.Operand -> m (SymbolTable, Operand.Operand, Maybe Operand.Operand)
buildReferencePAP symbolTable arity fn = do
  let papType = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
  let arity'  = i32ConstOp (fromIntegral arity)

  boxedFn  <- box fn

  papPtr   <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
  papPtr'  <- bitcast papPtr (Type.ptr papType)
  Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

  return (symbolTable, papPtr', Just papPtr)

-- returns a (SymbolTable, Operand, Maybe Operand) where the maybe operand is a possible boxed value when available
generateExp :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Exp -> m (SymbolTable, Operand, Maybe Operand)
generateExp env symbolTable exp = case exp of
  Optimized (ps IT.:=> t) _ (Var n) ->
    case Map.lookup n symbolTable of
      Just (Symbol (FunctionSymbol 0) fnPtr) -> do
        -- Handle special nullary cases like assignment methods
        pap <- call fnPtr []
        return (symbolTable, pap, Nothing)

      Just (Symbol (FunctionSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol (MethodSymbol 0) fnPtr) -> do
        -- Handle special nullary cases like assignment methods or mempty
        pap     <- call fnPtr []
        unboxed <- unbox t pap
        return (symbolTable, unboxed, Just pap)

      Just (Symbol (MethodSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol TopLevelAssignment ptr) -> do
        loaded <- load ptr 8
        return (symbolTable, loaded, Nothing)

      Just (Symbol (LocalVariableSymbol ptr) value) -> do
        return (symbolTable, value, Just ptr)

      Just (Symbol (ConstructorSymbol _ 0) fnPtr) -> do
        -- Nullary constructors need to be called directly to retrieve the value
        constructed <- call fnPtr []
        return (symbolTable, constructed, Nothing)

      Just (Symbol (ConstructorSymbol _ arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol _ var) ->
        return (symbolTable, var, Nothing)

      Nothing ->
        error $ "Var not found " <> n <> "\nExp: "<>ppShow exp

  -- (Placeholder (ClassRef "Functor" [] False True , "b183")
  Optimized t _ (Placeholder (ClassRef interface _ False True, typingStr) _) -> do
    let (dictNameParams, innerExp) = gatherAllPlaceholders exp
    let wrapperType = List.foldr IT.fn (IT.tVar "a") (IT.tVar "a" <$ dictNameParams)
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
    return (symbolTable', Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType (List.replicate (List.length dictNameParams) boxType) False) fnName), Nothing)
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
  Optimized (_ IT.:=> t) _ (Placeholder (MethodRef interface methodName True, typingStr) _) -> do
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
          value      <- call methodFn'' []
          return (symbolTable, value, Nothing)
        else
          return (symbolTable, methodPAP, Nothing)

      _ ->
        error $ "dict not found: '"<>dictName <>"\n\n"<>ppShow exp

  -- (Placeholder ( ClassRef "Functor" [] True False , "List" )
  Optimized (_ IT.:=> t) _ (Placeholder (ClassRef interface _ True _, typingStr) _) -> do
    (dictArgs, fn) <- collectDictArgs symbolTable exp
    (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
    pap' <- bitcast pap boxType
    let argc = i32ConstOp $ fromIntegral (List.length dictArgs)
    dictArgs' <- mapM box dictArgs
    let dictArgs'' = (,[]) <$> dictArgs'
    ret <- call applyPAP $ [(pap', []), (argc, [])] ++ dictArgs''
    unboxed <- unbox t ret
    return (symbolTable, unboxed, Just ret)

  -- Most likely a method that has to be applied some dictionaries
  Optimized t _ (Placeholder (MethodRef interface methodName False, typingStr) _) -> do
    let methodName' = "$" <> interface <> "$" <> typingStr <> "$" <> methodName
    case Map.lookup methodName' symbolTable of
      Just (Symbol (MethodSymbol arity) fnOperand) -> do
        (_, pap, boxedPAP) <- buildReferencePAP symbolTable arity fnOperand
        return (symbolTable, pap, boxedPAP)

      _ ->
        error $ "method with name '" <> methodName' <> "' not found!"

  Optimized _ _ (Export e) -> do
    generateExp env { isLast = False } symbolTable e

  Optimized (ps IT.:=> ty) _ (Assignment name e isTopLevel) -> do
    (symbolTable', exp', _) <- generateExp env { isLast = False } symbolTable e

    if isTopLevel then do
      let t = typeOf exp'
      g <- global (AST.mkName name) t $ Constant.Undef t
      store g 8 exp'
      Writer.tell $ Map.singleton name (topLevelSymbol g)
      return (Map.insert name (topLevelSymbol g) symbolTable, exp', Nothing)
    else
      case Map.lookup name symbolTable of
        Just (Symbol (LocalVariableSymbol ptr) value) ->
          -- TODO: handle strings properly here
          case typeOf (trace ("ASSIGNMENT: "<>name) exp') of
            Type.PointerType t _ | ty == IT.TCon (IT.TC "String" IT.Star) "prelude" -> do
              ptr' <- bitcast ptr $ Type.ptr stringType
              store ptr' 8 exp'
              return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Nothing)

            Type.PointerType _ _ | IT.isListType ty -> do
              ptr' <- bitcast ptr $ Type.ptr listType
              store ptr' 8 exp'
              return (Map.insert name (localVarSymbol ptr' exp') symbolTable, exp', Nothing)


            Type.PointerType t _  -> do
              ptr'   <- bitcast ptr $ typeOf exp'
              loaded <- load exp' 8
              store ptr' 8 loaded
              return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Nothing)

            _ | IT.hasNumberPred ps -> do
              ptr'   <- bitcast ptr $ Type.ptr Type.i64
              exp''  <- bitcast exp' Type.i64
              store ptr' 8 exp''
              return (Map.insert name (localVarSymbol ptr' exp') symbolTable, exp', Nothing)

            _ -> do
              ptr' <- bitcast ptr (Type.ptr $ typeOf exp')
              store ptr' 8 exp'
              return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Nothing)

        Just (Symbol _ _) ->
          return (Map.insert name (varSymbol exp') symbolTable, exp', Nothing)

        Nothing -> do
          boxed <- box exp'
          return (Map.insert name (localVarSymbol boxed exp') symbolTable, exp', Just boxed)

  Optimized _ _ (App (Optimized _ _ (Var "!")) [operand]) -> do
    (_, operand', _) <- generateExp env { isLast = False } symbolTable operand
    result           <- add operand' (Operand.ConstantOperand $ Constant.Int 1 1)
    return (symbolTable, result, Nothing)

  Optimized _ _ (App (Optimized _ _ (Var "-")) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- fsub leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Optimized _ _ (App (Optimized _ _ (Var "+")) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- fadd leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Optimized _ _ (App (Optimized _ _ (Var "*")) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result             <- fmul leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Optimized _ _ (App (Optimized _ _ (Var "/")) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env symbolTable rightOperand
    result                <- fdiv leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Optimized _ _ (App (Optimized _ _ (Var "++")) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- call strConcat [(leftOperand', []), (rightOperand', [])]
    return (symbolTable, result, Nothing)

  Optimized _ _ (App (Optimized _ _ (Placeholder _ (Optimized _ _ (Var "!=")))) [leftOperand@(Optimized (_ IT.:=> t) _ _), rightOperand])
    | t `List.elem`
        [ IT.TCon (IT.TC "Byte" IT.Star) "prelude"
        , IT.TCon (IT.TC "Integer" IT.Star) "prelude"
        , IT.TCon (IT.TC "Float" IT.Star) "prelude"
        , IT.TCon (IT.TC "Boolean" IT.Star) "prelude"
        , IT.TCon (IT.TC "Unit" IT.Star) "prelude"
        , IT.TCon (IT.TC "String" IT.Star) "prelude"
        ] -> case t of
              IT.TCon (IT.TC "Byte" IT.Star) "prelude" -> do
                (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
                (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
                result                <- icmp IntegerPredicate.NE leftOperand' rightOperand'
                return (symbolTable, result, Nothing)

              IT.TCon (IT.TC "Integer" IT.Star) "prelude" -> do
                (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
                (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
                result                <- icmp IntegerPredicate.NE leftOperand' rightOperand'
                return (symbolTable, result, Nothing)

              IT.TCon (IT.TC "Boolean" IT.Star) "prelude" -> do
                (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
                (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
                result                <- icmp IntegerPredicate.NE leftOperand' rightOperand'
                return (symbolTable, result, Nothing)

              IT.TCon (IT.TC "Float" IT.Star) "prelude" -> do
                (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
                (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
                result                <- fcmp FloatingPointPredicate.ONE leftOperand' rightOperand'
                return (symbolTable, result, Nothing)

              IT.TCon (IT.TC "Unit" IT.Star) "prelude" ->
                return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 0, Nothing)

              IT.TCon (IT.TC "String" IT.Star) "prelude" -> do
                (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
                (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
                result                <- call areStringsNotEqual [(leftOperand', []), (rightOperand', [])]
                return (symbolTable, result, Nothing)

              _ ->
                undefined

  -- Optimized (_ IT.:=> t) _ (App (Optimized _ _ (Var "<=")) [leftOperand, rightOperand]) -> do
  --   (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
  --   (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
  --   result             <- case t of
  --     IT.TCon (IT.TC "Float" _) _ ->
  --       fcmp FloatingPointPredicate.OLE leftOperand' rightOperand'

  --     IT.TCon (IT.TC "Byte" _) _ ->
  --       icmp IntegerPredicate.ULE leftOperand' rightOperand'

  --     _ ->
  --       icmp IntegerPredicate.SLE leftOperand' rightOperand'
  --   return (symbolTable, result, Nothing)

  Optimized (_ IT.:=> t) _ (App (Optimized _ _ (Var "&&")) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- Instruction.and leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Optimized (_ IT.:=> t) _ (App fn args) -> case fn of
    -- Calling a known method
    Optimized _ _ (Placeholder (MethodRef interface methodName False, typingStr) _) -> case methodName of
      "==" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- fcmp FloatingPointPredicate.OEQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "String" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- call areStringsEqual [(leftOperand', []), (rightOperand', [])]
          return (symbolTable, result, Nothing)

        "Boolean" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Unit" -> do
          return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 1, Nothing)

        _ ->
          undefined

      "+" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- add leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- add leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fadd leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "-" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- sub leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- sub leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fsub leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "*" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- mul leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- mul leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fmul leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "unary-minus" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          result                <- mul leftOperand' (i64ConstOp (-1))
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          result                <- sub leftOperand' (C.int8 256)
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          result                <- fmul leftOperand' (C.double (-1))
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      ">" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.SGT leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.UGT leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fcmp FloatingPointPredicate.OGT leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "<" -> case trace ("TYPING-STR: "<>typingStr) typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.SLT (trace ("LEFT: "<>ppShow leftOperand'<>"\nRIGHT: "<>ppShow rightOperand') leftOperand') rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.ULT leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fcmp FloatingPointPredicate.OLT leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      ">=" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.SGE leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.UGE leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fcmp FloatingPointPredicate.OGE leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "<=" -> case typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.SLE leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.ULE leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fcmp FloatingPointPredicate.OLE leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

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
        args'  <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs args'

        ret <- call applyPAP $ [(pap'', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox t ret
        return (symbolTable, unboxed, Just ret)

      _ ->
        error $ "Function not found " <> functionName

    _ -> case fn of
      Opt.Optimized _ _ (Opt.Placeholder (Opt.MethodRef "Number" _ True, _) _) -> do
        (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
        pap' <- bitcast pap boxType

        let argc = i32ConstOp (fromIntegral $ List.length args)

        args'  <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs args'

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox t ret
        return (symbolTable, unboxed, Just ret)

      _ -> do
        (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
        pap' <- bitcast pap boxType

        let argc = i32ConstOp (fromIntegral $ List.length args)

        args'  <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs args'

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox t ret
        return (symbolTable, unboxed, Just ret)

  Optimized (_ IT.:=> t) _ (LNum n) -> case t of
    IT.TCon (IT.TC "Float" _) _ ->
      return (symbolTable, C.double (read n), Nothing)

    IT.TCon (IT.TC "Integer" _) _ ->
      return (symbolTable, C.int64 (read n), Nothing)

    IT.TCon (IT.TC "Byte" _) _ ->
      return (symbolTable, C.int8 (read n), Nothing)

    _ ->
      return (symbolTable, C.int64 (read n), Nothing)

  Optimized _ _ (LFloat n) -> do
    return (symbolTable, C.double (read n), Nothing)

  Optimized _ _ (LBool b) -> do
    let value =
          if b == "true" then
            1
          else
            0
    return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 value, Nothing)

  Optimized _ _ LUnit -> do
    return (symbolTable, Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1), Nothing)

  Optimized _ _ (LStr (leading : s)) | leading == '"' || leading == '\'' -> do
    addr <- buildStr (List.init s)
    return (symbolTable, addr, Nothing)

  Optimized _ _ (LStr s) -> do
    addr <- buildStr s
    return (symbolTable, addr, Nothing)

  Optimized _ _ (TemplateString parts) -> do
    parts' <- mapM (generateExp env { isLast = False } symbolTable) parts
    let parts'' = (\(_, a, _) -> a) <$> parts'
    asOne  <- Monad.foldM
      (\prev next -> call strConcat [(prev, []), (next, [])])
      (List.head parts'')
      (List.tail parts'')
    return (symbolTable, asOne, Nothing)

  Optimized _ _ (Opt.Do exps) -> do
    (ret, boxed) <- generateDoExps env { isLast = False } symbolTable exps
    return (symbolTable, ret, boxed)

  Optimized _ _ (TupleConstructor exps) -> do
    exps'     <- mapM (((\(_, a, _) -> a) <$>). generateExp env { isLast = False } symbolTable) exps
    boxedExps <- mapM box exps'
    let expsWithIds = List.zip boxedExps [0..]
        tupleType   = Type.StructureType False (typeOf <$> boxedExps)
    tuplePtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof tupleType, [])]
    tuplePtr' <- bitcast tuplePtr (Type.ptr tupleType)
    Monad.foldM_ (storeItem tuplePtr') () expsWithIds

    return (symbolTable, tuplePtr', Nothing)

  Optimized _ _ (ListConstructor []) -> do
    -- an empty list is { value: null, next: null }
    emptyList  <- call gcMalloc [(Operand.ConstantOperand $ sizeof (Type.StructureType False [boxType, boxType]), [])]
    emptyList' <- addrspacecast emptyList listType
    store emptyList' 8 (Operand.ConstantOperand $ Constant.Struct Nothing False [Constant.Null boxType, Constant.Null boxType])

    return (symbolTable, emptyList', Nothing)

  Optimized _ _ (ListConstructor listItems) -> do
    tail <- case List.last listItems of
      Optimized _ _ (ListItem lastItem) -> do
        (symbolTable', lastItem', _) <- generateExp env { isLast = False } symbolTable lastItem
        lastItem''                <- box lastItem'
        call madlistSingleton [(lastItem'', [])]

      Optimized _ _ (ListSpread spread) -> do
        (_, spread', _) <- generateExp env { isLast = False } symbolTable spread
        return spread'

      cannotHappen ->
        undefined

    list <- Monad.foldM
      (\list' i -> case i of
        Optimized _ _ (ListItem item) -> do
          (_, item', _) <- generateExp env { isLast = False } symbolTable item
          item''     <- box item'
          call madlistPush [(item'', []), (list', [])]

        Optimized _ _ (ListSpread spread) -> do
          (_, spread', _) <- generateExp env { isLast = False } symbolTable spread
          call madlistConcat [(spread', []), (list', [])]

        cannotHappen ->
          undefined
      )
      tail
      (List.reverse $ List.init listItems)

    return (symbolTable, list, Nothing)

  Optimized _ _ (Record fields) -> do

    let (base, fields') = List.partition isSpreadField fields
    let fieldCount = i32ConstOp (fromIntegral $ List.length fields')

    base' <- case base of
      [Optimized _ _ (FieldSpread exp)] -> do
        (_, exp', _) <- generateExp env { isLast = False } symbolTable exp
        return exp'

      _ ->
        return $ Operand.ConstantOperand (Constant.Null boxType)

    fields'' <- mapM (generateField symbolTable) fields'

    record <- call buildRecord $ [(fieldCount, []), (base', [])] ++ ((,[]) <$> fields'')
    return (symbolTable, record, Nothing)
    where
      generateField :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Field -> m Operand
      generateField symbolTable field = case field of
        Optimized _ _ (Field (name, value)) -> do
          let fieldType = Type.StructureType False [stringType, boxType]
          nameOperand <- buildStr name -- workaround for now, we need to remove the wrapping "
          (_, value', _) <- generateExp env { isLast = False } symbolTable value
          value''     <- box value'

          fieldPtr    <- call gcMalloc [(Operand.ConstantOperand $ sizeof fieldType, [])]
          fieldPtr'   <- bitcast fieldPtr (Type.ptr fieldType)

          Monad.foldM_ (storeItem fieldPtr') () [(nameOperand, 0), (value'', 1)]
          return fieldPtr'

        _ ->
          undefined

  Optimized (_ IT.:=> t) _ (Access record (Optimized _ _ (Var ('.' : fieldName)))) -> do
    nameOperand        <- buildStr fieldName
    (_, recordOperand, _) <- generateExp env { isLast = False } symbolTable record
    value <- call selectField [(nameOperand, []), (recordOperand, [])]

    value' <- unbox t value
    return (symbolTable, value', Just value)


  Optimized (_ IT.:=> t) _ (If cond truthy falsy) ->
    if isLast env then mdo
      (symbolTable', cond', _) <- generateExp env { isLast = False } symbolTable cond
      -- test  <- icmp IntegerPredicate.EQ cond' true
      condBr cond' ifThen ifElse

      ifThen <- block `named` "if.then"
      (symbolTable'', truthy', maybeBoxedTruthy) <- generateExp env symbolTable' truthy
      truthyValue <- case maybeBoxedTruthy of
        Just boxed ->
          return boxed

        Nothing ->
          box truthy'
      ifThen' <- currentBlock
      br ifExit

      ifElse <- block `named` "if.else"
      (symbolTable''', falsy', maybeBoxedFalsy) <- generateExp env symbolTable' falsy
      falsyValue <- case maybeBoxedFalsy of
        Just boxed ->
          return boxed

        Nothing ->
          box falsy'
      ifElse' <- currentBlock
      br ifExit

      ifExit <- block `named` "if.exit"
      ret <- phi [(truthyValue, ifThen'), (falsyValue, ifElse')]

      -- this value comes boxed as that will directly be returned
      return (symbolTable', ret, Just ret)
    else mdo
      (symbolTable', cond', _) <- generateExp env { isLast = False } symbolTable cond
      test  <- icmp IntegerPredicate.EQ cond' true
      condBr test ifThen ifElse

      ifThen <- block `named` "if.then"
      (symbolTable'', truthy', _) <- generateExp env symbolTable' truthy
      ifThen' <- currentBlock
      br ifExit

      ifElse <- block `named` "if.else"
      (symbolTable''', falsy', _) <- generateExp env symbolTable' falsy
      ifElse' <- currentBlock
      br ifExit

      ifExit <- block `named` "if.exit"
      ret <- phi [(truthy', ifThen'), (falsy', ifElse')]

      return (symbolTable', ret, Nothing)

  Optimized _ _ (Where exp iss) -> mdo
    (_, exp', _) <- generateExp env { isLast = False } symbolTable exp
    branches     <- generateBranches env symbolTable exitBlock exp' iss

    exitBlock    <- block `named` "exitBlock"
    ret          <- phi branches

    if isLast env then
      return (symbolTable, ret, Just ret)
    else
      return (symbolTable, ret, Nothing)


  Optimized _ _ (TypedExp exp _) ->
    generateExp env { isLast = False } symbolTable exp

  Optimized (_ IT.:=> t) _ (NameExport n) -> do
    let ref = Operand.ConstantOperand $ Constant.GlobalReference (buildLLVMType t) (AST.mkName n)
    if IT.isFunctionType t then do
      let arity = List.length $ IT.getParamTypes t
      Writer.tell $ Map.singleton n (fnSymbol arity ref)
    else
      Writer.tell $ Map.singleton n (varSymbol ref)
    return (symbolTable, ref, Nothing)


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
    test      <- generateBranchTest symbolTable pat whereExp
    condBr test branchExpBlock nextBlock

    branchExpBlock <- block `named` "branchExpBlock"
    symbolTable' <- generateSymbolTableForPattern symbolTable whereExp pat
    (_, branchResult, maybeBoxedBranchResult) <- generateExp env symbolTable' exp
    branchResult' <-
      if isLast env then
        case maybeBoxedBranchResult of
          Just boxed ->
            return boxed

          Nothing ->
            box branchResult
      else
        return branchResult
    -- the exp might contain a where or if expression generating new blocks in between.
    -- therefore we need to get the block that contains the register reference in which
    -- it is defined. 
    retBlock <- currentBlock
    br exitBlock

    (nextBlock, finalPhi) <-
      if hasMore then do
        b <- block `named` "nextBlock"
        return (b, [])
      else do
        let def = Operand.ConstantOperand (Constant.Undef (typeOf branchResult'))
        return (exitBlock, [(def, currBlock)])

    return $ (branchResult', retBlock) : finalPhi

  _ ->
    undefined


generateSymbolTableForIndexedData :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> SymbolTable -> (Pattern, Integer) -> m SymbolTable
generateSymbolTableForIndexedData basePtr symbolTable (pat, index) = do
  ptr  <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  ptr' <- load ptr 8
  ptr'' <- unbox (getType pat) ptr'
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
    -- TODO: depending on the type of the pattern we need to generate fcmp or icmp
    fcmp FloatingPointPredicate.OEQ (C.double (read n)) value

  Optimized _ _ (PBool "true") ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 1) value

  Optimized _ _ (PBool _) ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 0) value

  Optimized _ _ (PStr s) -> do
    s' <- buildStr (List.init . List.tail $ s)
    call areStringsEqual [(s', []), (value, [])]

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
            -- This is necessary to make the special case of Dictionary constructor
            if "Dictionary" `List.isSuffixOf` name then
              i64ConstOp 0
            else
              error $ "Constructor '" <> name <> "' not found!"

    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- bitcast value constructorType
    let argIds = fromIntegral <$> List.take (List.length pats) [1..]
    constructorArgPtrs <- getStructPointers argIds constructor'
    let patsWithPtrs = List.zip pats constructorArgPtrs

    id              <- gep constructor' [i32ConstOp 0, i32ConstOp 0]
    id'             <- load id 8
    testIds         <- icmp IntegerPredicate.EQ constructorId id'

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
    (symbolTable', _, _) <- generateExp env symbolTable exp
    generateExps env symbolTable' es

  _ ->
    return ()


generateExternFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> IT.Type -> String -> Int -> Operand -> m SymbolTable
generateExternFunction symbolTable t functionName arity foreignFn = do
  let paramTypes    = IT.getParamTypes t
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

  Writer.tell $ Map.singleton functionName (fnSymbol arity function)
  return $ Map.insert functionName (fnSymbol arity function) symbolTable


makeParamName :: String -> ParameterName
makeParamName = ParameterName . stringToShortByteString


generateFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Bool -> IT.Type -> String -> [String] -> [Exp] -> m SymbolTable
generateFunction env symbolTable isMethod t functionName paramNames body = do
  let paramTypes    = IT.getParamTypes t
      params'       = (boxType,) . makeParamName <$> paramNames
      functionName' = AST.mkName functionName

  function <- function functionName' params' boxType $ \params -> mdo
    let typesWithParams = List.zip paramTypes params
    unboxedParams <- mapM (uncurry unbox) typesWithParams
    let paramsWithNames       = Map.fromList $ List.zip paramNames (uncurry localVarSymbol <$> List.zip params unboxedParams)
        symbolTableWithParams = symbolTable <> paramsWithNames

    -- Generate body
    (generatedBody, maybeBoxed) <- generateBody env symbolTableWithParams body

    case maybeBoxed of
      Just boxed ->
        ret boxed

      Nothing -> do
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
  Optimized (_ IT.:=> t) _ (TopLevelAbs functionName params body) -> do
    generateFunction env symbolTable False t functionName params body

  Optimized _ _ (Extern (_ IT.:=> t) name originalName) -> do
    let paramTypes  = IT.getParamTypes t
        paramTypes' = buildLLVMParamType <$> paramTypes
        returnType  = IT.getReturnType t
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

  Optimized _ _ (Extern (_ IT.:=> t) functionName originalName) ->
    let arity  = List.length $ IT.getParamTypes t
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Optimized (_ IT.:=> t) _ (Assignment name exp _) ->
    if IT.isFunctionType t then
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable
    else
      let expType   = buildLLVMType t
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable

  Optimized _ _ (TypedExp (Optimized (_ IT.:=> t) _ (Assignment name exp _)) _) ->
    if IT.isFunctionType t then
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable
    else
      let expType   = buildLLVMType t
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable

  Optimized _ _ (Export (Optimized (_ IT.:=> t) _ (Assignment name exp _))) ->
    if IT.isFunctionType t then
      let expType   = Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable
    else
      let expType   = buildLLVMType t
          globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
      in  Map.insert name (topLevelSymbol globalRef) symbolTable

  Optimized _ _ (TypedExp (Optimized _ _ (Export (Optimized (_ IT.:=> t) _ (Assignment name exp _)))) _) ->
    if IT.isFunctionType t then
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





generateDoExps :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Exp] -> m (Operand, Maybe Operand)
generateDoExps env symbolTable exps = case exps of
  [exp] -> do
    (_, result, boxed) <- generateExp env symbolTable exp
    return (result, boxed)

  (exp : es) -> do
    (symbolTable', _, _) <- generateExp env { isLast = False } symbolTable exp
    generateBody env symbolTable' es

  _ ->
    undefined

generateBody :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Exp] -> m (Operand, Maybe Operand)
generateBody env symbolTable exps = case exps of
  [exp] -> do
    (_, result, boxed) <- generateExp env { isLast = True } symbolTable exp
    return (result, boxed)

  (exp : es) -> do
    (symbolTable', _, _) <- generateExp env symbolTable exp
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
    let paramTypes     = IT.getParamTypes t
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
  Opt.Optimized (_ IT.:=> t) _ (TopLevelAbs _ params body) ->
    generateFunction env symbolTable True t methodName params body

  -- TODO: reconsider this
  Opt.Optimized (_ IT.:=> t) _ (Opt.Assignment _ exp _) -> do
    let paramTypes  = IT.getParamTypes t
        arity       = List.length paramTypes
        params'     = (,NoParameterName) <$> (boxType <$ paramTypes)

    f <- function (AST.mkName methodName) params' boxType $ \params -> do
      (symbolTable, exp', _) <- generateExp env symbolTable exp

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


addMethodToSymbolTable :: SymbolTable -> Exp -> SymbolTable
addMethodToSymbolTable symbolTable topLevelFunction = case topLevelFunction of
  Optimized _ _ (TopLevelAbs functionName params _) ->
    let arity  = List.length params
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (methodSymbol arity fnRef) symbolTable

  Optimized (_ IT.:=> t) _ (Assignment name exp _) ->
    let paramTypes  = IT.getParamTypes t
        arity       = List.length paramTypes
        paramTypes' = boxType <$ paramTypes
        expType     = Type.ptr $ Type.FunctionType boxType paramTypes' False
        globalRef   = Operand.ConstantOperand (Constant.GlobalReference expType (AST.mkName name))
    in  Map.insert name (methodSymbol arity globalRef) symbolTable

  _ ->
    symbolTable


updateMethodName :: Exp -> String -> Exp
updateMethodName exp newName = case exp of
  Optimized t area (TopLevelAbs name params body) ->
    Optimized t area (TopLevelAbs newName params body)

  Optimized t area (Assignment name exp isTopLevel) ->
    Optimized t area (Assignment newName exp isTopLevel)

  _ ->
    undefined

addInstanceToSymbolTable :: SymbolTable -> Instance -> SymbolTable
addInstanceToSymbolTable symbolTable inst = case inst of
  Untyped _ (Instance interface preds typingStr methods) -> do
    let instanceName = "$" <> interface <> "$" <> typingStr
        prefixedMethods = Map.mapKeys ((instanceName <> "$") <>) methods
        updatedMethods  = Map.elems $ Map.mapWithKey (\name (exp, sc) -> updateMethodName exp name) prefixedMethods
    List.foldl' addMethodToSymbolTable symbolTable updatedMethods

  _ ->
    undefined


addInstancesToSymbolTable :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> [Instance] -> m SymbolTable
addInstancesToSymbolTable env =
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
          $ (\((methodName, IT.Forall _ (_ IT.:=> t)), index) ->
              (methodName, (index, List.length $ IT.getParamTypes t))
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
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = t, Global.linkage = Linkage.External }
    let def = AST.GlobalDefinition g
    emitDefn def
    return ()

  Just (Symbol (DictionarySymbol _) symbol) -> do
    let (Type.PointerType t _) = typeOf symbol
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = t, Global.linkage = Linkage.External }
    let def = AST.GlobalDefinition g
    emitDefn def
    return ()

  Just (Symbol _ symbol) -> do
    let t = typeOf symbol
    let g = globalVariableDefaults { Global.name = AST.mkName name, Global.type' = t, Global.linkage = Linkage.External }
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
  mapM_ (generateExternalForName symbolTable) (Set.toList $ Set.fromList dictAndMethodNames)


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



eqVars :: [String]
eqVars = (:"") <$> ['a'..]

eqNumbers :: [String]
eqNumbers = show <$> [1..]

getTupleName :: Int -> String
getTupleName arity = case arity of
  2         ->
    "Tuple_2"

  3        ->
    "Tuple_3"

  4       ->
    "Tuple_4"

  5      ->
    "Tuple_5"

  6     ->
    "Tuple_6"

  7    ->
    "Tuple_7"

  8   ->
    "Tuple_8"

  9  ->
    "Tuple_9"

  10 ->
    "Tuple_10"

  _ ->
    "Tuple_unknown"

-- generates AST for a tupleN instance. n must be >= 2
buildTupleNEqInstance :: Int -> Instance
buildTupleNEqInstance n =
  let tvarNames          = List.take n eqVars
      eqDictNames        = ("$Eq$" ++) <$> tvarNames
      tvars              = (\name -> IT.TVar (IT.TV name IT.Star)) <$> tvarNames
      dictTVars          = IT.TVar (IT.TV "eqDict" IT.Star) <$ eqDictNames
      preds              = (\var -> IT.IsIn "Eq" [var] Nothing) <$> tvars
      tupleName          = getTupleName n
      tupleType          = List.foldl' IT.TApp tupleHeadType tvars
      methodQualType     = preds IT.:=> List.foldr IT.fn IT.tBool (dictTVars ++ (tupleType <$ tvars))
      tupleHeadType      = IT.getTupleCtor n
      tupleQualType      = preds IT.:=> List.foldl' IT.TApp tupleHeadType tvars
      whereExpQualType   = preds IT.:=> IT.TApp (IT.TApp IT.tTuple2 tupleType) tupleType
      isQualType         = preds IT.:=> (IT.TApp (IT.TApp IT.tTuple2 tupleType) tupleType `IT.fn` IT.tBool)
      leftTupleVarNames  = ("a" ++) <$> eqNumbers
      rightTupleVarNames = ("b" ++) <$> eqNumbers
      leftTuplePatterns  =
        (\(tvName, var) ->
          Optimized ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea (PVar var)
        ) <$> List.zip tvarNames leftTupleVarNames
      rightTuplePatterns =
        (\(tvName, var) ->
          Optimized ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea (PVar var)
        ) <$> List.zip tvarNames rightTupleVarNames

      leftVars =
        (\(tvName, var) ->
          Optimized ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea (Var var)
        ) <$> List.zip tvarNames leftTupleVarNames
      rightVars =
        (\(tvName, var) ->
          Optimized ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea (Var var)
        ) <$> List.zip tvarNames rightTupleVarNames

      eqMethods =
        (\tvName ->
          Optimized
            ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> (IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.tBool))
            emptyArea
            (Placeholder (MethodRef "Eq" "==" True, tvName) (
              Optimized
              ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> (IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.tBool))
              emptyArea
              (Var "==")
            ))
        ) <$> tvarNames

      conditions = (\(method, leftVar, rightVar) -> Optimized ([] IT.:=> IT.tBool) emptyArea (App method [leftVar, rightVar])) <$> List.zip3 eqMethods leftVars rightVars
      andApp = \left right -> Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized ([] IT.:=> (IT.tBool `IT.fn` IT.tBool `IT.fn` IT.tBool)) emptyArea (Var "&&")) [left, right])
      condition = List.foldr andApp (Optimized ([] IT.:=> IT.tBool) emptyArea (LBool "true")) conditions


  in  Untyped emptyArea (Instance
        "Eq"
        preds
        tupleName
        (Map.fromList
          [ ( "=="
            , ( Optimized methodQualType emptyArea (TopLevelAbs "==" (eqDictNames ++ ["a", "b"]) [
                  Optimized ([] IT.:=> IT.tBool) emptyArea (Where (
                    Optimized whereExpQualType emptyArea (TupleConstructor [
                      Optimized tupleQualType emptyArea (Var "a"),
                      Optimized tupleQualType emptyArea (Var "b")
                    ])
                  ) [
                    Optimized isQualType emptyArea (Is
                      (Optimized whereExpQualType emptyArea (PTuple [
                        Optimized tupleQualType emptyArea (PTuple leftTuplePatterns),
                        Optimized tupleQualType emptyArea (PTuple rightTuplePatterns)
                      ]))
                      condition
                    )
                  ])
                ])
              , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
              )
            )
          ]
        )
      )


buildRuntimeModule :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => Env -> [String] -> SymbolTable -> m ()
buildRuntimeModule env currentModuleHashes initialSymbolTable = do
  externVarArgs (AST.mkName "__applyPAP__")    [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc")              [Type.i64] (Type.ptr Type.i8)

  -- Number Integer
  extern (AST.mkName "__addIntegers__")       [boxType, boxType] boxType
  extern (AST.mkName "__substractIntegers__") [boxType, boxType] boxType
  extern (AST.mkName "__multiplyIntegers__")  [boxType, boxType] boxType
  extern (AST.mkName "__gtIntegers__")        [boxType, boxType] boxType
  extern (AST.mkName "__ltIntegers__")        [boxType, boxType] boxType
  extern (AST.mkName "__gteIntegers__")       [boxType, boxType] boxType
  extern (AST.mkName "__lteIntegers__")       [boxType, boxType] boxType
  extern (AST.mkName "__numberToInteger__")   [boxType] boxType

  -- Number Byte
  extern (AST.mkName "__addBytes__")          [boxType, boxType] boxType
  extern (AST.mkName "__substractBytes__")    [boxType, boxType] boxType
  extern (AST.mkName "__multiplyBytes__")     [boxType, boxType] boxType
  extern (AST.mkName "__gtBytes__")           [boxType, boxType] boxType
  extern (AST.mkName "__ltBytes__")           [boxType, boxType] boxType
  extern (AST.mkName "__gteBytes__")          [boxType, boxType] boxType
  extern (AST.mkName "__lteBytes__")          [boxType, boxType] boxType
  extern (AST.mkName "__numberToByte__")      [boxType] boxType

  -- Number Float
  extern (AST.mkName "__addFloats__")         [boxType, boxType] boxType
  extern (AST.mkName "__substractFloats__")   [boxType, boxType] boxType
  extern (AST.mkName "__multiplyFloats__")    [boxType, boxType] boxType
  extern (AST.mkName "__gtFloats__")          [boxType, boxType] boxType
  extern (AST.mkName "__ltFloats__")          [boxType, boxType] boxType
  extern (AST.mkName "__gteFloats__")         [boxType, boxType] boxType
  extern (AST.mkName "__lteFloats__")         [boxType, boxType] boxType
  extern (AST.mkName "__numberToFloat__")     [boxType] boxType

  -- Eq
  extern (AST.mkName "__eqInteger__")         [boxType, boxType] boxType
  extern (AST.mkName "__eqByte__")            [boxType, boxType] boxType
  extern (AST.mkName "__eqFloat__")           [boxType, boxType] boxType
  extern (AST.mkName "__eqString__")          [boxType, boxType] boxType
  extern (AST.mkName "__eqBoolean__")         [boxType, boxType] boxType
  extern (AST.mkName "__eqList__")            [boxType, boxType, boxType] boxType
  extern (AST.mkName "__eqDictionary__")      [boxType, boxType, boxType, boxType] boxType

      -- Number Integer
  let addIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__addIntegers__")
      substractIntegers = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__substractIntegers__")
      multiplyIntegers  = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__multiplyIntegers__")
      gtIntegers        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__gtIntegers__")
      ltIntegers        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__ltIntegers__")
      gteIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__gteIntegers__")
      lteIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__lteIntegers__")
      numberToInteger   = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "__numberToInteger__")

      -- Number Byte
      addBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__addBytes__")
      substractBytes    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__substractBytes__")
      multiplyBytes     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__multiplyBytes__")
      gtBytes           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__gtBytes__")
      ltBytes           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__ltBytes__")
      gteBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__gteBytes__")
      lteBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__lteBytes__")
      numberToByte      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "__numberToByte__")

      -- Number Float
      addFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__addFloats__")
      substractFloats   = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__substractFloats__")
      multiplyFloats    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__multiplyFloats__")
      gtFloats          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__gtFloats__")
      ltFloats          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__ltFloats__")
      gteFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__gteFloats__")
      lteFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__lteFloats__")
      numberToFloat     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "__numberToFloat__")

      -- Eq Integer
      eqInteger         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__eqInteger__")

      -- Eq Byte
      eqByte            = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__eqInteger__")

      -- Eq Float
      eqFloat           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__eqInteger__")

      -- Eq String
      eqString          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__eqString__")

      -- Eq Boolean
      eqBoolean         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "__eqBoolean__")

      -- Eq List
      eqList            = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType, boxType] False) "__eqList__")

      -- Eq Dictionary
      eqDictionary      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType, boxType, boxType] False) "__eqDictionary__")

      symbolTableWithCBindings =
        -- Number Float
        Map.insert "__addFloats__" (fnSymbol 2 addFloats)
        $ Map.insert "__substractFloats__" (fnSymbol 2 substractFloats)
        $ Map.insert "__multiplyFloats__" (fnSymbol 2 multiplyFloats)
        $ Map.insert "__gtFloats__" (fnSymbol 2 gtFloats)
        $ Map.insert "__ltFloats__" (fnSymbol 2 ltFloats)
        $ Map.insert "__gteFloats__" (fnSymbol 2 gteFloats)
        $ Map.insert "__lteFloats__" (fnSymbol 2 lteFloats)
        $ Map.insert "__numberToFloat__" (fnSymbol 1 numberToFloat)

        -- Number Byte
        $ Map.insert "__addBytes__" (fnSymbol 2 addFloats)
        $ Map.insert "__substractBytes__" (fnSymbol 2 substractFloats)
        $ Map.insert "__multiplyBytes__" (fnSymbol 2 multiplyFloats)
        $ Map.insert "__gtBytes__" (fnSymbol 2 gtFloats)
        $ Map.insert "__ltBytes__" (fnSymbol 2 ltFloats)
        $ Map.insert "__gteBytes__" (fnSymbol 2 gteFloats)
        $ Map.insert "__lteBytes__" (fnSymbol 2 lteFloats)
        $ Map.insert "__numberToByte__" (fnSymbol 1 numberToFloat)

        -- Number Integer
        $ Map.insert "__numberToInteger__" (fnSymbol 1 numberToInteger)
        $ Map.insert "__addIntegers__" (fnSymbol 2 addIntegers)
        $ Map.insert "__substractIntegers__" (fnSymbol 2 substractIntegers)
        $ Map.insert "__multiplyIntegers__" (fnSymbol 2 multiplyIntegers)
        $ Map.insert "__gtIntegers__" (fnSymbol 2 gtIntegers)
        $ Map.insert "__ltIntegers__" (fnSymbol 2 ltIntegers)
        $ Map.insert "__gteIntegers__" (fnSymbol 2 gteIntegers)
        $ Map.insert "__lteIntegers__" (fnSymbol 2 lteIntegers)

        -- Eq
        $ Map.insert "__eqInteger__" (fnSymbol 2 eqInteger)
        $ Map.insert "__eqByte__" (fnSymbol 2 eqByte)
        $ Map.insert "__eqFloat__" (fnSymbol 2 eqFloat)
        $ Map.insert "__eqString__" (fnSymbol 2 eqString)
        $ Map.insert "__eqBoolean__" (fnSymbol 2 eqBoolean)
        $ Map.insert "__eqList__" (fnSymbol 3 eqList)
        $ Map.insert "__eqDictionary__" (fnSymbol 4 eqDictionary) initialSymbolTable

      numberType               = IT.TVar (IT.TV "a" IT.Star)
      numberPred               = IT.IsIn "Number" [numberType] Nothing
      numberQualType           = [numberPred] IT.:=> numberType
      numberOperationQualType  = [numberPred] IT.:=> (numberType `IT.fn` numberType `IT.fn` numberType)
      numberComparisonQualType = [numberPred] IT.:=> (numberType `IT.fn` numberType `IT.fn` IT.tBool)
      coerceNumberQualType     = [numberPred] IT.:=> (numberType `IT.fn` numberType)

  -- TODO: Add "unary-minus" method for number instances
  let integerNumberInstance =
        Untyped emptyArea
          ( Instance "Number" [] "Integer"
              (Map.fromList
                [ ( "+"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "+" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__addIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "-" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__substractIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "*" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__multiplyIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">"
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs ">" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__gtIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<"
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs "<" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__ltIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( ">="
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs ">=" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__gteIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<="
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs "<=" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__lteIntegers__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Optimized coerceNumberQualType emptyArea (TopLevelAbs "__coerceNumber__" ["a"] [
                        Optimized numberQualType emptyArea (App (Optimized coerceNumberQualType emptyArea (Var "__numberToInteger__")) [
                          Optimized numberQualType emptyArea (Var "a")
                        ])
                    ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
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
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "+" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__addBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "-" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__substractBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "*" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__multiplyBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">"
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs ">" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__gtBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<"
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs "<" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__ltBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( ">="
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs ">=" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__gteBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<="
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs "<=" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__lteBytes__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Optimized coerceNumberQualType emptyArea (TopLevelAbs "__coerceNumber__" ["a"] [
                        Optimized numberQualType emptyArea (App (Optimized coerceNumberQualType emptyArea (Var "__numberToByte__")) [
                          Optimized numberQualType emptyArea (Var "a")
                        ])
                    ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
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
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "+" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__addFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "-" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__substractFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Optimized numberOperationQualType emptyArea (TopLevelAbs "*" ["a", "b"] [
                        Optimized numberQualType emptyArea (App (Optimized numberOperationQualType emptyArea (Var "__multiplyFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">"
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs ">" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__gtFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<"
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs "<" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__ltFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( ">="
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs ">=" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__gteFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<="
                  , ( Optimized numberComparisonQualType emptyArea (TopLevelAbs "<=" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized numberComparisonQualType emptyArea (Var "__lteFloats__")) [
                          Optimized numberQualType emptyArea (Var "a"),
                          Optimized numberQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Optimized coerceNumberQualType emptyArea (TopLevelAbs "__coerceNumber__" ["a"] [
                        Optimized numberQualType emptyArea (App (Optimized coerceNumberQualType emptyArea (Var "__numberToFloat__")) [
                          Optimized numberQualType emptyArea (Var "a")
                        ])
                    ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                ]
              )
          )

  let varType             = IT.TVar (IT.TV "a" IT.Star)
      eqPred              = IT.IsIn "Eq" [varType] Nothing
      eqVarQualType       = [eqPred] IT.:=> varType
      eqOperationQualType = [eqPred] IT.:=> (varType `IT.fn` varType `IT.fn` IT.tBool)
      eqOperationType     = varType `IT.fn` varType `IT.fn` IT.tBool

      integerEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [] "Integer"
              (Map.fromList
                [ ( "=="
                  , ( Optimized eqOperationQualType emptyArea (TopLevelAbs "==" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized eqOperationQualType emptyArea (Var "__eqInteger__")) [
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      byteEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [] "Byte"
              (Map.fromList
                [ ( "=="
                  , ( Optimized eqOperationQualType emptyArea (TopLevelAbs "==" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized eqOperationQualType emptyArea (Var "__eqByte__")) [
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      floatEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [] "Float"
              (Map.fromList
                [ ( "=="
                  , ( Optimized eqOperationQualType emptyArea (TopLevelAbs "==" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized eqOperationQualType emptyArea (Var "__eqFloat__")) [
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      stringEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [] "String"
              (Map.fromList
                [ ( "=="
                  , ( Optimized eqOperationQualType emptyArea (TopLevelAbs "==" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized eqOperationQualType emptyArea (Var "__eqString__")) [
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      booleanEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [] "Boolean"
              (Map.fromList
                [ ( "=="
                  , ( Optimized eqOperationQualType emptyArea (TopLevelAbs "==" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized eqOperationQualType emptyArea (Var "__eqBoolean__")) [
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      unitEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [] "Unit"
              (Map.fromList
                [ ( "=="
                  , ( Optimized eqOperationQualType emptyArea (TopLevelAbs "==" ["a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (LBool "true")
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )


      dictType             = IT.TVar (IT.TV "dict" IT.Star)
      overloadedEqType     = dictType `IT.fn` varType `IT.fn` varType `IT.fn` IT.tBool
      overloadedEqQualType = [eqPred] IT.:=> overloadedEqType

      listEqInstance =
        Untyped emptyArea
          ( Instance "Eq" [IT.IsIn "Eq" [IT.TVar (IT.TV "a" IT.Star)] Nothing] "List"
              (Map.fromList
                [ ( "=="
                  , ( Optimized overloadedEqQualType emptyArea (TopLevelAbs "==" ["eqDict", "a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized overloadedEqQualType emptyArea (Var "__eqList__")) [
                          Optimized ([] IT.:=> dictType) emptyArea (Var "eqDict"),
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      dictionaryEqPreds    = [IT.IsIn "Eq" [IT.TVar (IT.TV "a" IT.Star)] Nothing, IT.IsIn "Eq" [IT.TVar (IT.TV "b" IT.Star)] Nothing]
      dictionaryEqType     = dictType `IT.fn` dictType `IT.fn` varType `IT.fn` varType `IT.fn` IT.tBool
      dictionaryEqQualType = dictionaryEqPreds IT.:=> dictionaryEqType

      dictionaryEqInstance =
        Untyped emptyArea
          ( Instance "Eq" dictionaryEqPreds "Dictionary"
              (Map.fromList
                [ ( "=="
                  , ( Optimized dictionaryEqQualType emptyArea (TopLevelAbs "==" ["eqDictA", "eqDictB", "a", "b"] [
                        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized dictionaryEqQualType emptyArea (Var "__eqDictionary__")) [
                          Optimized ([] IT.:=> dictType) emptyArea (Var "eqDictA"),
                          Optimized ([] IT.:=> dictType) emptyArea (Var "eqDictB"),
                          Optimized eqVarQualType emptyArea (Var "a"),
                          Optimized eqVarQualType emptyArea (Var "b")
                        ])
                      ])
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      tupleEqInstances = buildTupleNEqInstance <$> [2..10]

  generateFunction env symbolTableWithCBindings False overloadedEqType "!=" ["$Eq$eqVar", "a", "b"] [
      Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized overloadedEqQualType emptyArea (Var "!")) [
        Optimized ([] IT.:=> IT.tBool) emptyArea (App (Optimized overloadedEqQualType emptyArea (Placeholder (MethodRef "Eq" "==" True, "eqVar") (Optimized eqOperationQualType emptyArea (Var "==")))) [
          Optimized eqVarQualType emptyArea (Var "a"),
          Optimized eqVarQualType emptyArea (Var "b")
        ])
      ])
    ]

  generateInstances
    env
    symbolTableWithCBindings
    (
      [ integerNumberInstance
      , byteNumberInstance
      , floatNumberInstance
      , integerEqInstance
      , floatEqInstance
      , stringEqInstance
      , booleanEqInstance
      , unitEqInstance
      , listEqInstance
      , dictionaryEqInstance
      ] ++ tupleEqInstances
    )
  return ()


buildModule' :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => Env -> Bool -> [String] -> SymbolTable -> AST -> m ()
buildModule' env isMain currentModuleHashes initialSymbolTable ast = do
  let symbolTableWithTopLevel = List.foldr (flip addTopLevelFnToSymbolTable) initialSymbolTable (aexps ast)
      symbolTableWithMethods  = List.foldr (flip addInstanceToSymbolTable) symbolTableWithTopLevel (ainstances ast)
      symbolTableWithDefaults = Map.insert "__dict_ctor__" (fnSymbol 2 dictCtor) symbolTableWithMethods

  mapM_ (generateImport initialSymbolTable) $ aimports ast
  generateExternsForImportedInstances initialSymbolTable

  symbolTable   <- generateConstructors symbolTableWithDefaults (atypedecls ast)
  symbolTable'  <- generateInstances env symbolTable (ainstances ast)
  symbolTable'' <- generateTopLevelFunctions env symbolTable' (topLevelFunctions $ aexps ast)

  externVarArgs (AST.mkName "__applyPAP__")    [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)

  extern (AST.mkName "__dict_ctor__")          [boxType, boxType] boxType
  externVarArgs (AST.mkName "__buildRecord__") [Type.i32, boxType] recordType
  extern (AST.mkName "__selectField__")        [stringType, recordType] boxType
  extern (AST.mkName "__areStringsEqual__")    [stringType, stringType] Type.i1
  extern (AST.mkName "__areStringsNotEqual__") [stringType, stringType] Type.i1
  extern (AST.mkName "__strConcat__")          [stringType, stringType] stringType
  extern (AST.mkName "MadList_hasMinLength")   [Type.double, listType] Type.i1
  extern (AST.mkName "MadList_hasLength")      [Type.double, listType] Type.i1
  extern (AST.mkName "MadList_singleton")      [Type.ptr Type.i8] listType
  extern (AST.mkName "__MadList_push__")       [Type.ptr Type.i8, listType] listType
  extern (AST.mkName "MadList_concat")         [listType, listType] listType

  extern (AST.mkName "GC_malloc")              [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "malloc")                 [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "calloc")                 [Type.i32, Type.i32] (Type.ptr Type.i8)

  extern (AST.mkName "!=")                     [boxType, boxType, boxType] boxType

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
  if Map.member apath moduleTable then
    (moduleTable, symbolTable, processedHashes, initialEnv)
  else
    let imports                          = aimports ast
        alreadyProcessedPaths            = Map.keys moduleTable
        importPathsToProcess             = List.filter (`List.notElem` alreadyProcessedPaths) $ getImportAbsolutePath <$> imports
        astsForImports                   = Maybe.mapMaybe (`Map.lookup` astTable) importPathsToProcess
        (moduleTableWithImports, symbolTableWithImports, importHashes, envFromImports) =
          List.foldl' (generateAST False astTable) (moduleTable, symbolTable, processedHashes, initialEnv) astsForImports

        computedDictionaryIndices        = buildDictionaryIndices $ ainterfaces ast
        envForAST                        = Env { dictionaryIndices = computedDictionaryIndices <> dictionaryIndices envFromImports, isLast = False }
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
    let dictionaryIndices = Map.fromList [ ("Number", Map.fromList [("*", (0, 2)), ("+", (1, 2)), ("-", (2, 2)), ("<", (3, 2)), ("<=", (4, 2)), (">", (5, 2)), (">=", (6, 2)), ("__coerceNumber__", (7, 1))]), ("Eq", Map.fromList [("==", (0, 2))]) ]
        (numbersModule, symbolTable') = Writer.runWriter $ buildModuleT (stringToShortByteString "number") (buildRuntimeModule Env { dictionaryIndices = dictionaryIndices, isLast = False } [] symbolTable)
        numbersModulePath = joinPath [takeDirectory entrypoint, "default", "numbers.mad"]
        (moduleTable, _, _, _) = generateAST True astTable (generatedTable, symbolTable', [], Env { dictionaryIndices = dictionaryIndices, isLast = False }) ast
    in  Map.insert numbersModulePath numbersModule moduleTable

  _ ->
    error $ "AST '" <> entrypoint <> "' not found in:\n" <> ppShow (Map.keys astTable)


compileModule :: FilePath -> FilePath -> FilePath -> AST.Module -> IO FilePath
compileModule outputFolder rootPath astPath astModule = do
  let outputPath = Path.computeLLVMTargetPath outputFolder rootPath astPath

  Prelude.putStrLn outputPath
  T.putStrLn $ ppllvm astModule

  withHostTargetMachineDefault $ \target -> do
    withContext $ \ctx -> do
      withModuleFromAST ctx astModule $ \mod' -> do
        dataLayout  <- getTargetMachineDataLayout target
        triple      <- getTargetMachineTriple target
        libraryInfo <- withTargetLibraryInfo triple return
        mod'' <-
          withPassManager
          defaultCuratedPassSetSpec
            { optLevel                = Just 2
            , useInlinerWithThreshold = Just 150
            , dataLayout              = Just dataLayout
            , targetLibraryInfo       = Just libraryInfo
            }
          $ \pm -> do
            runPassManager pm mod'
            return mod'
        -- mod'' <-
        --   withPassManager
        --     defaultPassSetSpec
        --     { transforms        = [defaultGCOVProfiler]
        --     , targetMachine     = Just target
        --     , dataLayout        = Just dataLayout
        --     , targetLibraryInfo = Just libraryInfo
        --     }
        --     $ \pm -> do
        --       runPassManager pm mod'
        --       return mod'

        createDirectoryIfMissing True $ takeDirectory outputPath
        -- writeBitcodeToFile (File ((dropExtension outputPath) ++ ".bc")) mod''
        -- callCommand $ "opt-9 -O0 --insert-gcov-profiling --instrprof " ++ (dropExtension outputPath) ++ ".bc -o " ++ (dropExtension outputPath) ++ "2.bc"
        -- callCommand $ "llc-9 -tailcallopt -O0 -filetype=obj " ++ (dropExtension outputPath) ++ ".bc"
        writeObjectToFile target (File outputPath) mod''

  return outputPath

generateTable :: FilePath -> FilePath -> Table -> FilePath -> IO ()
generateTable outputFolder rootPath astTable entrypoint = do
  let moduleTable  = generateTableModules Map.empty Map.empty astTable entrypoint
  objectFilePaths <- mapM (uncurry $ compileModule outputFolder rootPath) $ Map.toList moduleTable

  let objectFilePathsForCli = List.unwords objectFilePaths

  callCommand $ "clang++ --coverage -fprofile-instr-generate -fcoverage-mapping -foptimize-sibling-calls -g -stdlib=libc++ -v " <> objectFilePathsForCli <> " ./runtime/lib/libgc.a ./runtime/lib/libuv.a ./runtime/build/runtime.a -o a.out"
  -- callCommand $ "clang++ -foptimize-sibling-calls -g -stdlib=libc++ -v " <> objectFilePathsForCli <> " ./runtime/lib/libgc.a ./runtime/lib/libuv.a ./runtime/build/runtime.a -o a.out"
