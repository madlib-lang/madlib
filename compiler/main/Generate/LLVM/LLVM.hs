{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use let" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Eta reduce" #-}
module Generate.LLVM.LLVM where


import           Data.ByteString.Short        as ShortByteString
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char                    as Char
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as TextEncoding
import qualified Data.Text.Lazy               as LazyText
import qualified Control.Monad                as Monad
import qualified Control.Monad.Fix            as MonadFix
import qualified Control.Monad.Identity       as Identity
import qualified Control.Monad.Writer         as Writer
import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import           Run.Options
import qualified Driver.Query                 as Query
import qualified Rock
import           Data.ByteString as ByteString
import           Data.ByteString.Char8        as Char8
import           System.Process
import           System.Environment.Executable

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
import           LLVM.Pretty

import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Constant         as C
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction      as Instruction
import           LLVM.Context (withContext)
import           AST.Core                        as Core
import qualified Data.String.Utils               as List
import qualified Infer.Type                      as IT
import           Infer.Type (isFunctionType)
import           LLVM.PassManager
import qualified LLVM.Prelude as FloatingPointPredicate
import           Text.Show.Pretty
import qualified Control.Monad.Fix             as Writer
import qualified Utils.Path                    as Path
import qualified Data.ByteString.Lazy.Char8    as BLChar8

import qualified Utils.Hash                    as Hash
import qualified Data.Tuple                    as Tuple
import           Explain.Location
import           System.FilePath (takeDirectory, takeExtension, makeRelative, joinPath, splitPath, takeFileName, dropExtension, dropFileName)
import qualified LLVM.AST.Linkage              as Linkage
import qualified Distribution.System           as DistributionSystem
import qualified Data.Text.Lazy.IO             as Text
import           Debug.Trace
import qualified Data.Functor.Constant         as Operand
import qualified Utils.IO                      as IOUtils
import Control.Monad.IO.Class


sizeof :: Type.Type -> Constant.Constant
sizeof t = Constant.PtrToInt szPtr (Type.IntegerType 64)
  where
     ptrType = Type.PointerType t (AddrSpace 0)
     nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
     szPtr   = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]

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

methodSymbol :: Int -> Operand -> Symbol
methodSymbol arity =
  Symbol (MethodSymbol arity)

topLevelSymbol :: Operand -> Symbol
topLevelSymbol =
  Symbol TopLevelAssignment

constructorSymbol :: Operand -> Int -> Int -> Symbol
constructorSymbol ctor id arity =
  Symbol (ConstructorSymbol id arity) ctor

adtSymbol :: Int -> Symbol
adtSymbol maxArity =
  Symbol (ADTSymbol maxArity) (Operand.ConstantOperand (Constant.Null boxType))

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

mainInit :: Operand
mainInit =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [Type.i32, Type.ptr (Type.ptr Type.i8)] False) (AST.mkName "__main__init__"))

initExtra :: Operand
initExtra =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName "madlib__process__internal__initExtra"))

typedHoleReached :: Operand
typedHoleReached =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName "madlib__process__internal__typedHoleReached"))

initEventLoop :: Operand
initEventLoop =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName "__initEventLoop__"))

startEventLoop :: Operand
startEventLoop =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName "__startEventLoop__"))

gcMalloc :: Operand
gcMalloc =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "GC_malloc"))

gcMallocAtomic :: Operand
gcMallocAtomic =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "GC_malloc_atomic"))

applyPAP :: Operand
applyPAP =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.ptr Type.i8, Type.i32] True) (AST.mkName "__applyPAP__"))

dictCtor :: Operand
dictCtor =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) (AST.mkName "__dict_ctor__"))

buildRecord :: Operand
buildRecord =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType recordType [Type.i32, boxType] True) (AST.mkName "madlib__record__internal__buildRecord"))

selectField :: Operand
selectField =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [stringType, recordType] False) (AST.mkName "madlib__record__internal__selectField"))

madlistHasMinLength :: Operand
madlistHasMinLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.i64, listType] False) (AST.mkName "madlib__list__internal__hasMinLength"))

madlistHasLength :: Operand
madlistHasLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.i64, listType] False) (AST.mkName "madlib__list__internal__hasLength"))

madlistSingleton :: Operand
madlistSingleton =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [Type.ptr Type.i8] False) (AST.mkName "madlib__list__singleton"))

madlistPush :: Operand
madlistPush =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [Type.ptr Type.i8, listType] False) (AST.mkName "madlib__list__internal__push"))

madlistConcat :: Operand
madlistConcat =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [listType, listType] False) (AST.mkName "madlib__list__concat"))

areStringsEqual :: Operand
areStringsEqual =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (AST.mkName "madlib__string__internal__areStringsEqual"))

areStringsNotEqual :: Operand
areStringsNotEqual =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [stringType, stringType] False) (AST.mkName "madlib__string__internal__areStringsNotEqual"))

strConcat :: Operand
strConcat =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType stringType [stringType, stringType] False) (AST.mkName "madlib__string__internal__concat"))

i32ConstOp :: Integer -> Operand
i32ConstOp i = Operand.ConstantOperand $ Constant.Int 32 i

i64ConstOp :: Integer -> Operand
i64ConstOp i = Operand.ConstantOperand $ Constant.Int 64 i


storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
storeItem basePtr _ (item, index) = do
  ptr <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  store ptr 0 item
  return ()


-- Mostly used for boxing/unboxing and therefore does just one Level
buildLLVMType :: Env -> SymbolTable -> IT.Qual IT.Type -> Type.Type
buildLLVMType env symbolTable (ps IT.:=> t) = case t of
  IT.TCon (IT.TC "Float" IT.Star) "prelude" ->
    Type.double

  IT.TCon (IT.TC "Byte" IT.Star) "prelude" ->
    Type.i8

  IT.TCon (IT.TC "Char" IT.Star) "prelude" ->
    Type.i32

  IT.TCon (IT.TC "Integer" IT.Star) "prelude" ->
    Type.i64

  IT.TCon (IT.TC "String" IT.Star) "prelude" ->
    stringType

  IT.TCon (IT.TC "Boolean" IT.Star) "prelude" ->
    Type.i1

  IT.TCon (IT.TC "{}" IT.Star) "prelude" ->
    Type.ptr Type.i1

  IT.TVar _ | IT.hasNumberPred ps ->
    Type.i64

  IT.TApp (IT.TCon (IT.TC "List" (IT.Kfun IT.Star IT.Star)) "prelude") _ ->
    listType

  IT.TRecord _ _ _ -> do
    recordType

  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude") _) _ ->
    let arity = List.length $ IT.getParamTypes t
    in  Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False

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

  _ | IT.isTCon t ->
    if IT.getTConName t `List.notElem` tConExclude && IT.getTConName t /= "" then
      retrieveConstructorStructType env symbolTable t
    else
      Type.ptr Type.i8

  _ ->
    Type.ptr Type.i8

buildLLVMParamType :: Env -> SymbolTable -> IT.Type -> Type.Type
buildLLVMParamType env symbolTable t = case t of
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" (IT.Kfun IT.Star (IT.Kfun IT.Star IT.Star))) "prelude") _) _ ->
    papType

  _ ->
    buildLLVMType env symbolTable ([] IT.:=> t)


typingStrWithoutHash :: String -> String
typingStrWithoutHash = List.takeWhile (/= '_')


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

tConExclude :: [String]
tConExclude = ["Array", "Dictionary", "ByteArray", "(,)", "(,,)", "(,,,)", "(,,,,)", "(,,,,,)", "(,,,,,,)", "(,,,,,,,)", "(,,,,,,,,)", "(,,,,,,,,,)"]


retrieveConstructorStructType :: Env -> SymbolTable -> IT.Type -> Type.Type
retrieveConstructorStructType env symbolTable t =
  let astPath = IT.getTConPath t
      tName   = IT.getTConName t
      key     = astPath <> "__" <> tName
  in  case Map.lookup key symbolTable of
        Just (Symbol (ADTSymbol maxArity) _) ->
          Type.ptr $ Type.StructureType False (Type.i64 : List.replicate maxArity boxType)

        e ->
          boxType
          -- error $ "type not found: "<>ppShow key<>"\nfound: "<>ppShow e<>"\nST: "<>ppShow symbolTable<>"\ncurrent module: "<>ppShow (envASTPath env)


unbox :: (MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Qual IT.Type -> Operand -> m Operand
unbox env symbolTable qt@(ps IT.:=> t) what = case t of
  IT.TCon (IT.TC "Float" _) _ -> do
    ptr <- alloca boxType Nothing 0
    store ptr 0 what
    ptr' <- bitcast ptr (Type.ptr Type.double)
    load ptr' 0

  IT.TCon (IT.TC "Byte" _) _ -> do
    ptrtoint what Type.i8

  IT.TCon (IT.TC "Char" _) _ -> do
    ptrtoint what Type.i32

  IT.TCon (IT.TC "Integer" _) _ -> do
    ptrtoint what Type.i64

  IT.TCon (IT.TC "Boolean" _) _ -> do
    ptrtoint what Type.i1

  -- boxed strings are char**
  IT.TCon (IT.TC "String" _) _ -> do
    safeBitcast what stringType

  IT.TCon (IT.TC "Unit" _) _ -> do
    safeBitcast what $ Type.ptr Type.i1

  -- boxed lists are { i8*, i8* }**
  IT.TApp (IT.TCon (IT.TC "List" _) _) _ -> do
    -- ptr <- safeBitcast what (Type.ptr listType)
    -- load ptr 0
    safeBitcast what listType

  IT.TRecord _ _ _ -> do
    safeBitcast what recordType

  -- This should be called for parameters that are closures or returned closures
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" _) _) _) _ ->
    safeBitcast what papType

  IT.TVar _ | IT.hasNumberPred ps -> do
    ptrtoint what Type.i64

  -- That handles tuple types
  _ -> do
    let llvmType = buildLLVMType env symbolTable qt
    safeBitcast what llvmType


box :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
box what = case typeOf what of
  -- Float 
  Type.FloatingPointType _ -> do
    ptr <- alloca Type.double Nothing 0
    boxWrap <- alloca (Type.ptr boxType) Nothing 0
    store ptr 0 what
    ptr' <- bitcast ptr (Type.ptr boxType)
    store boxWrap 0 ptr'
    loaded <- load boxWrap 0
    load loaded 0

  -- Integer
  Type.IntegerType 64 -> do
    inttoptr what boxType

  -- Char
  Type.IntegerType 32 -> do
    inttoptr what boxType

  -- Byte
  Type.IntegerType 8 -> do
    inttoptr what boxType

  -- Boolean
  Type.IntegerType 1 -> do
    inttoptr what boxType

  -- String
  Type.PointerType (Type.IntegerType 8) (AddrSpace 1) -> do
    safeBitcast what boxType

  -- List
  Type.PointerType (Type.StructureType False [Type.PointerType (Type.IntegerType 8) _, Type.PointerType (Type.IntegerType 8) _]) (AddrSpace 1) -> do
    safeBitcast what boxType

  -- Pointless?
  Type.PointerType (Type.IntegerType 8) _ ->
    return what

  -- Any pointer type
  _ ->
    safeBitcast what boxType


emptyList :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => m Operand
emptyList = do
  emptyList  <- call gcMalloc [(Operand.ConstantOperand $ sizeof (Type.StructureType False [boxType, boxType]), [])]
  emptyList' <- addrspacecast emptyList listType
  store emptyList' 0 (Operand.ConstantOperand $ Constant.Struct Nothing False [Constant.Null boxType, Constant.Null boxType])

  return emptyList'


buildStr :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => String -> m Operand
buildStr s = do
  let parser     = ReadP.readP_to_S $ ReadP.many $ ReadP.readS_to_P Char.readLitChar
      parsed     = fst $ List.last $ parser s
      asText     = Text.pack parsed
      bs         = TextEncoding.encodeUtf8 asText
      bytes      = ByteString.unpack bs
      charCodes  = (fromEnum <$> bytes) ++ [0]
      charCodes' = toInteger <$> charCodes
  addr  <- call gcMallocAtomic [(i64ConstOp (fromIntegral $ List.length charCodes'), [])]
  addr' <- addrspacecast addr stringType

  let charCodesWithIds = List.zip charCodes' [0..]

  Monad.foldM_ (storeChar addr') () charCodesWithIds
  return addr'
  where
    storeChar :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> () -> (Integer, Integer) -> m ()
    storeChar basePtr _ (charCode, index) = do
      ptr  <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 index)]
      store ptr 0 (Operand.ConstantOperand (Constant.Int 8 charCode))
      return ()



-- (Core.Placeholder
  --  ( ClassRef
      --  "Applicative"
      --  [ CRPNode "Semigroup" "List" False []
      --  , CRPNode "Functor" "Identity" False []
      --  ]
      --  True
      --  False
  --  , "WriterT"
  --  )
collectCRPDicts :: (Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> [Core.ClassRefPred] -> m [Operand.Operand]
collectCRPDicts symbolTable crpNodes = case crpNodes of
  (Core.CRPNode interfaceName typingStr _ subNodes : next) ->
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

          -- this case may happen when the dict is coming from a parameter
          -- at this point we have no info about it and it's already applied
          -- but it might be necessary to apply it to another dict
          Just (Symbol _ dict) -> do
            nextNodes <- collectCRPDicts symbolTable next
            return $ dict : nextNodes

          _ ->
            error $ "dict not found: "<>dictName<>"\n"<>ppShow (Map.lookup "$Inspect$e368" symbolTable)

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
    appliedMethods' <- mapM (`safeBitcast` papType) appliedMethods
    appliedMethods'' <- mapM (`load` 0) appliedMethods'

    let appliedDictType = Type.StructureType False (typeOf <$> appliedMethods'')
    appliedDict  <- call gcMalloc [(Operand.ConstantOperand $ sizeof appliedDictType, [])]
    appliedDict' <- safeBitcast appliedDict (Type.ptr appliedDictType)
    Monad.foldM_ (storeItem appliedDict') () $ List.zip appliedMethods'' [0..]

    return appliedDict'

collectDictArgs :: (Writer.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Core.Exp -> m ([Operand.Operand], Core.Exp)
collectDictArgs symbolTable exp = case exp of
  Core.Typed _ _ _ (Core.Placeholder (Core.ClassRef interfaceName classRefPreds True _, typingStr) exp') -> do
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


generateApplicationForKnownFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Qual IT.Type -> Int -> Operand -> [Core.Exp] -> m (SymbolTable, Operand, Maybe Operand)
generateApplicationForKnownFunction env symbolTable returnQualType arity fnOperand args
  | List.length args == arity = do
      -- We have a known call!
      args'   <- mapM (generateExp env { isLast = False } symbolTable) args
      args''  <- retrieveArgs (Core.getMetadata <$> args) args'
      let args''' = (, []) <$> args''

      ret <- call fnOperand args'''
      unboxed <- unbox env symbolTable returnQualType ret

      return (symbolTable, unboxed, Just ret)
  | List.length args > arity = do
      -- We have extra args so we do the known call and the applyPAP the resulting partial application
      let (args', remainingArgs) = List.splitAt arity args
      args''   <- mapM (generateExp env { isLast = False } symbolTable) args'
      args'''  <- retrieveArgs (Core.getMetadata <$> args') args''
      let args'''' = (, []) <$> args'''

      pap <- call fnOperand args''''

      let argc = i32ConstOp (fromIntegral $ List.length remainingArgs)
      remainingArgs'  <- mapM (generateExp env { isLast = False } symbolTable) remainingArgs
      remainingArgs'' <- retrieveArgs (Core.getMetadata <$> remainingArgs) remainingArgs'
      let remainingArgs''' = (, []) <$> remainingArgs''

      ret <- call applyPAP $ [(pap, []), (argc, [])] ++ remainingArgs'''
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

      envPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof envType, [])]
      envPtr' <- safeBitcast envPtr (Type.ptr envType)

      Monad.foldM_
        (\_ (boxed, index, argType, unboxed) ->
          case typeOf unboxed of
            Type.PointerType (Type.StructureType _ [Type.PointerType _ _, Type.IntegerType 32, Type.IntegerType 32, Type.PointerType _ _]) _ | IT.isFunctionType argType && Maybe.isJust (recursionData env) -> do
              unboxed' <- load unboxed 0
              newPAP <- call gcMalloc [(Operand.ConstantOperand $ sizeof papStructType, [])]
              newPAP' <- bitcast newPAP papType
              store newPAP' 0 unboxed'
              storeItem envPtr' () (newPAP, index)

            _ ->
              storeItem envPtr' () (boxed, index)
        )
        ()
        $ List.zip4 boxedArgs [0..] (Core.getType <$> args) ((\(_, a, _) -> a) <$> args')

      papPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof papStructType, [])]
      papPtr' <- safeBitcast papPtr (Type.ptr papStructType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (amountOfArgsToBeApplied, 2), (envPtr, 3)]

      return (symbolTable, papPtr', Just papPtr)


buildReferencePAP :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Int -> Operand.Operand -> m (SymbolTable, Operand.Operand, Maybe Operand.Operand)
buildReferencePAP symbolTable arity fn = do
  let papType = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
  let arity'  = i32ConstOp (fromIntegral arity)

  boxedFn  <- box fn

  papPtr   <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
  papPtr'  <- safeBitcast papPtr (Type.ptr papType)
  Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

  return (symbolTable, papPtr', Just papPtr)

-- returns a (SymbolTable, Operand, Maybe Operand) where the maybe operand is a possible boxed value when available
generateExp :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Core.Exp -> m (SymbolTable, Operand, Maybe Operand)
generateExp env symbolTable exp = case exp of
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

  Core.Typed (_ IT.:=> t) _ metadata (Core.Call (Core.Typed _ _ _ (Core.Var constructorName True)) args) | Core.isConstructorRecursiveCall metadata -> do
    let constructedType@(Type.PointerType structType _) = retrieveConstructorStructType env symbolTable t
    case getConstructorRecursionInfo metadata of
      Just (ConstructorRecursionInfo _ position) -> do
        -- holePtr' :: i8***
        let Just holePtr'      = holePtr <$> recursionData env
        -- holePtr'' :: i8**
        holePtr'' <- load holePtr' 0

        args' <- mapM (\(index, arg) ->
                          if index == position then do
                            return $ Operand.ConstantOperand (Constant.Null constructedType)
                            -- hole <- call gcMalloc [(Operand.ConstantOperand (sizeof Type.i8), [])]
                            -- safeBitcast hole constructedType
                          else do
                            (_, arg', _) <- generateExp env symbolTable arg
                            return arg'
                      ) (List.zip [0..] args)
        args'' <- mapM box args'

        let index = case Map.lookup constructorName symbolTable of
                    Just (Symbol (ConstructorSymbol id _) _) ->
                      id

                    _ ->
                      undefined

        constructed     <- call gcMalloc [(Operand.ConstantOperand $ sizeof structType, [])]
        constructed'    <- safeBitcast constructed constructedType

        -- store the constructor data in the struct
        Monad.foldM_ (storeItem constructed') () $ List.zip args'' [1..] ++ [(i64ConstOp (fromIntegral index), 0)]

        store holePtr'' 0 constructed'

        -- newHole :: i8**
        newHole   <- gep constructed' [i32ConstOp 0, i32ConstOp (fromIntegral $ position + 1)]
        newHole'' <- bitcast newHole (Type.ptr constructedType)
        store holePtr' 0 newHole''

        case args!!position of
          Core.Typed _ _ _ (Core.Call _ recArgs) -> do
            let llvmType      = buildLLVMType env symbolTable (getQualType exp)
            let Just continue = continueRef <$> recursionData env
            let Just params   = boxedParams <$> recursionData env

            store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

            let filteredArgs = List.filter (not . IT.isPlaceholderDict . Core.getQualType) recArgs

            recArgs'  <- mapM (generateExp env { isLast = False } symbolTable) recArgs
            let unboxedArgs = (\(_, x, _) -> x) <$> recArgs'

            -- We need to reverse because we may have some closured variables in the params and these need not be updated
            let paramUpdatesData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> filteredArgs) (List.reverse params) (List.reverse unboxedArgs)
            mapM_ (\(qt', ptr, exp) -> updateTCOArg symbolTable qt' ptr exp) paramUpdatesData

            return (symbolTable, Operand.ConstantOperand (Constant.Undef llvmType), Nothing)

          _ ->
            undefined

      Nothing ->
        undefined

  Core.Typed (_ IT.:=> t) _ _ (Core.Var n _) ->
    case Map.lookup n symbolTable of
      Just (Symbol (FunctionSymbol 0) fnPtr) -> do
        pap <- call fnPtr []
        return (symbolTable, pap, Nothing)

      Just (Symbol (FunctionSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      -- Just (Symbol (MethodSymbol 0) fnPtr) -> do
      --   -- Handle special nullary cases like assignment methods or mempty
      --   pap     <- call fnPtr []
      --   unboxed <- unbox env symbolTable qt pap
      --   return (symbolTable, unboxed, Just pap)

      Just (Symbol (MethodSymbol arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

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

      Just (Symbol (ConstructorSymbol _ 0) fnPtr) -> do
        let constructedType = retrieveConstructorStructType env symbolTable t
        -- Nullary constructors need to be called directly to retrieve the value
        constructed   <- call fnPtr []
        constructed'  <- safeBitcast constructed constructedType
        return (symbolTable, constructed', Nothing)

      Just (Symbol (ConstructorSymbol _ arity) fnPtr) -> do
        buildReferencePAP symbolTable arity fnPtr

      Just (Symbol (TCOParamSymbol ptr) _) -> do
        loaded <- load ptr 0
        return (symbolTable, loaded, Nothing)

      Just (Symbol _ var) ->
        return (symbolTable, var, Nothing)

      Nothing ->
        error $ "Var not found " <> n <> "\nExp: " <> ppShow exp

  -- (Core.Placeholder (ClassRef "Functor" [] False True , "b183")
  Core.Typed _ _ _ (Core.Placeholder (Core.ClassRef _ _ False True, _) _) -> do
    let (dictNameParams, innerExp) = gatherAllPlaceholders exp
    let wrapperType = [] IT.:=> List.foldr IT.fn (IT.tVar "a") (IT.tVar "a" <$ dictNameParams)
    fnName <- freshName (stringToShortByteString "dictionaryWrapper")
    let (Name fnName') = fnName
    symbolTable' <-
      generateFunction
        env
        symbolTable
        False
        []
        wrapperType
        (Char8.unpack (ShortByteString.fromShort fnName'))
        (Core.Typed ([] IT.:=> IT.tVar "dict") emptyArea [] <$> dictNameParams)
        [innerExp]
    return (symbolTable', Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType (List.replicate (List.length dictNameParams) boxType) False) fnName), Nothing)
    where
      gatherAllPlaceholders :: Core.Exp -> ([String], Core.Exp)
      gatherAllPlaceholders ph = case ph of
        Core.Typed _ _ _ (Core.Placeholder (Core.ClassRef interface _ False True, typingStr) e) ->
          let (nextDictParams, nextExp) = gatherAllPlaceholders e
              dictParamName      = "$" <> interface <> "$" <> typingStr
          in  (dictParamName : nextDictParams, nextExp)

        _ ->
          ([], ph)


  -- (Core.Placeholder (Core.MethodRef "Show" "show" True, "j9")
  Core.Typed _ _ _ (Core.Placeholder (Core.MethodRef interface methodName True, typingStr) _) -> do
    let dictName = "$" <> interface <> "$" <> typingStr
    case Map.lookup dictName symbolTable of
      Just (Symbol _ dict) -> do
        let interfaceMap   = case Map.lookup interface (dictionaryIndices env) of
              Just indices ->
                indices

              Nothing ->
                error $ "index table for interface '" <> interface <> "' not found"
        let (index, arity) = Maybe.fromMaybe (0, 0) $ Map.lookup methodName interfaceMap
        let papType        = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
        dict'      <- safeBitcast dict (Type.ptr $ Type.StructureType False (List.replicate (Map.size interfaceMap) papType))
        methodPAP  <- gep dict' [i32ConstOp 0, i32ConstOp (fromIntegral index)]

        -- If arity is 0 ( ex: mempty ), we need to call the function and get the value as there will be
        -- no call wrapping the Core.MethodRef. In other words that will never be called and a direct value is
        -- expected
        if arity == 0 then do
          methodFn   <- gep methodPAP [i32ConstOp 0, i32ConstOp 0]
          methodFn'  <- safeBitcast methodFn (Type.ptr $ Type.ptr $ Type.FunctionType boxType [] False)
          methodFn'' <- load methodFn' 0
          value      <- call methodFn'' []
          return (symbolTable, value, Nothing)
        else
          return (symbolTable, methodPAP, Nothing)

      _ ->
        error $ "dict not found: '"<>dictName <>"\n\n"<>ppShow exp

  -- Most likely a method that has to be applied some dictionaries
  Core.Typed qt _ _ (Core.Placeholder (Core.MethodRef interface methodName False, typingStr) _) -> do
    let methodName' = "$" <> interface <> "$" <> typingStr <> "$" <> methodName
    case Map.lookup methodName' symbolTable of
      Just (Symbol (MethodSymbol 0) fnPtr) -> do
        -- Handle special nullary cases like assignment methods or mempty
        pap     <- call fnPtr []
        unboxed <- unbox env symbolTable qt pap
        return (symbolTable, unboxed, Nothing)
        -- return (symbolTable, unboxed, Just pap)

      Just (Symbol (MethodSymbol arity) fnOperand) -> do
        buildReferencePAP symbolTable arity fnOperand

      _ ->
        error $ "method with name '" <> methodName' <> "' not found!"

  -- TODO: Export nodes are stripped from closure convertion currently, we'll need this back soon.
  -- Core.Typed _ _ _ (Core.Export e) -> do
  --   generateExp env { isLast = False } symbolTable e

  Core.Typed _ _ metadata (Core.Assignment name e) -> do
    (_, exp', _) <- generateExp env { isLast = False, isTopLevel = False } symbolTable e

    if isTopLevel env then do
      let t = typeOf exp'
      g <- global (AST.mkName name) t $ Constant.Undef t
      store g 0 exp'
      Writer.tell $ Map.singleton name (topLevelSymbol g)
      return (Map.insert name (topLevelSymbol g) symbolTable, exp', Nothing)
    else
      if Core.isReferenceAllocation metadata then do
        let expType = typeOf exp'
            ptrType = Type.ptr expType
        ptr  <- call gcMalloc [(Operand.ConstantOperand (sizeof expType), [])]
        ptr' <- safeBitcast ptr ptrType
        store ptr' 0 exp'
        return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Just ptr')
      else if Core.isReferenceStore metadata then
        case Map.lookup name symbolTable of
          Just (Symbol (LocalVariableSymbol ptr) _) -> do
            ptr' <- safeBitcast ptr (Type.ptr $ typeOf exp')
            store ptr' 0 exp'
            return (Map.insert name (localVarSymbol ptr exp') symbolTable, exp', Just ptr)

          or ->
            error $ "found: " <> ppShow or
      else
        return (Map.insert name (varSymbol exp') symbolTable, exp', Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "%" _)) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- srem leftOperand' rightOperand'
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "!" _)) [operand]) -> do
    (_, operand', _) <- generateExp env { isLast = False } symbolTable operand
    result           <- add operand' (Operand.ConstantOperand $ Constant.Int 1 1)
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Var "++" _)) [leftOperand, rightOperand]) -> do
    (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable leftOperand
    (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable rightOperand
    result                <- call strConcat [(leftOperand', []), (rightOperand', [])]
    return (symbolTable, result, Nothing)

  Core.Typed _ _ _ (Core.Call (Core.Typed _ _ _ (Core.Placeholder _ (Core.Typed _ _ _ (Core.Var "!=" _)))) [leftOperand@(Core.Typed (_ IT.:=> t) _ _ _), rightOperand])
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
    Core.Typed _ _ _ (Var "==" False) | getType (List.head args) `List.elem` [IT.tInteger, IT.tByte, IT.tFloat, IT.tStr, IT.tBool, IT.tUnit, IT.tChar] ->
      case getType (List.head args) of
        IT.TCon (IT.TC "String" IT.Star) "prelude" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- call areStringsEqual [(leftOperand', []), (rightOperand', [])]
          return (symbolTable, result, Nothing)

        IT.TCon (IT.TC "Integer" IT.Star) "prelude" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        IT.TCon (IT.TC "Byte" IT.Star) "prelude" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        IT.TCon (IT.TC "Float" IT.Star) "prelude" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- fcmp FloatingPointPredicate.OEQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        IT.TCon (IT.TC "Char" IT.Star) "prelude" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        IT.TCon (IT.TC "Boolean" IT.Star) "prelude" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.EQ leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        IT.TCon (IT.TC "{}" IT.Star) "prelude" -> do
          return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 1, Nothing)

    -- Calling a known method
    Core.Typed _ _ _ (Core.Placeholder (Core.MethodRef interface methodName False, typingStr) _) -> case methodName of
      "<<" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- shl leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- shl leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      ">>" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- ashr leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- ashr leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      ">>>" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- lshr leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- lshr leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "|" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- Instruction.or leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- Instruction.or leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "&" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- Instruction.and leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- Instruction.and leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "^" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- Instruction.xor leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- Instruction.xor leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "~" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, operand', _) <- generateExp env { isLast = False } symbolTable (List.head args)
          result           <- Instruction.xor operand' (i64ConstOp (-1))
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, operand', _) <- generateExp env { isLast = False } symbolTable (List.head args)
          result           <- Instruction.xor operand' (Operand.ConstantOperand (Constant.Int 8 (-1)))
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "+" -> case typingStrWithoutHash typingStr of
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

      "-" -> case typingStrWithoutHash typingStr of
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

      "*" -> case typingStrWithoutHash typingStr of
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

      "/" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          leftOperand''         <- sitofp leftOperand' Type.double
          rightOperand''        <- sitofp rightOperand' Type.double
          result                <- fdiv leftOperand'' rightOperand''
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          leftOperand''         <- uitofp leftOperand' Type.double
          rightOperand''        <- uitofp rightOperand' Type.double
          result                <- fdiv leftOperand'' rightOperand''
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args!!1)
          result                <- fdiv leftOperand' rightOperand'
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      "unary-minus" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          result                <- mul leftOperand' (i64ConstOp (-1))
          return (symbolTable, result, Nothing)

        "Byte" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          result                <- mul leftOperand' (C.int8 (-1))
          return (symbolTable, result, Nothing)

        "Float" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          result                <- fmul leftOperand' (C.double (-1))
          return (symbolTable, result, Nothing)

        _ ->
          undefined

      ">" -> case typingStrWithoutHash typingStr of
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

      "<" -> case typingStrWithoutHash typingStr of
        "Integer" -> do
          (_, leftOperand', _)  <- generateExp env { isLast = False } symbolTable (List.head args)
          (_, rightOperand', _) <- generateExp env { isLast = False } symbolTable (args !! 1)
          result                <- icmp IntegerPredicate.SLT leftOperand' rightOperand'
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

      ">=" -> case typingStrWithoutHash typingStr of
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

      "<=" -> case typingStrWithoutHash typingStr of
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
            generateApplicationForKnownFunction env symbolTable qt arity fnOperand args

          _ ->
            error $ "method not found\n\n" <> ppShow symbolTable <> "\nwanted: " <> methodName'

    _ | Core.isPlainRecursiveCall metadata -> do
        let llvmType      = buildLLVMType env symbolTable (getQualType exp)
        let Just continue = continueRef <$> recursionData env
        let Just params   = boxedParams <$> recursionData env
        store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

        let filteredArgs = List.filter (not . IT.isPlaceholderDict . Core.getQualType) args

        args'  <- mapM (generateExp env { isLast = False } symbolTable) filteredArgs
        let unboxedArgs = (\(_, x, _) -> x) <$> args'

        -- We need to reverse because we may have some closured variables in the params and these need not be updated
        let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> filteredArgs) (List.reverse params) (List.reverse unboxedArgs)
        mapM_ (\(qt', ptr, exp) -> updateTCOArg symbolTable qt' ptr exp) paramUpdateData

        return (symbolTable, Operand.ConstantOperand (Constant.Undef llvmType), Nothing)

    Core.Typed _ area _ (Core.Var functionName _) -> case Map.lookup functionName symbolTable of
      Just (Symbol (ConstructorSymbol _ arity) fnOperand) ->
        generateApplicationForKnownFunction env symbolTable qt arity fnOperand args

      Just (Symbol (FunctionSymbol arity) fnOperand) ->
        generateApplicationForKnownFunction env symbolTable qt arity fnOperand args

      Just (Symbol symbolType pap) -> do
        -- We apply a partial application
        let argsApplied = List.length args
        let argc        = i32ConstOp (fromIntegral argsApplied)

        pap' <-
          if symbolType == TopLevelAssignment then
            load pap 0
          else
            return pap

        pap'' <- safeBitcast pap' boxType

        args'     <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'

        ret       <- call applyPAP $ [(pap'', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed   <- unbox env symbolTable qt ret
        return (symbolTable, unboxed, Just ret)

      _ ->
        error $ "Function not found " <> functionName <> "\narea: " <> ppShow area <> "\nST: " <> ppShow symbolTable

    _ -> case fn of
      -- With this we enable tail call optimization for overloaded functions
      -- because if we apply a PAP we loose the direct call and thus TCO.
      Core.Typed _ _ _ (Core.Placeholder (Core.ClassRef _ _ True _, _) _) -> do
        (dictArgs, fn') <- collectDictArgs symbolTable fn
        dictArgs'       <- mapM box dictArgs

        args'     <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'
        let allArgs = dictArgs' ++ boxedArgs

        case fn' of
          Core.Typed _ _ _ (Core.Var functionName _) -> case Map.lookup functionName symbolTable of
            Just (Symbol (FunctionSymbol arity) fnOperand) | arity == List.length allArgs -> do
              ret <- call fnOperand ((,[]) <$> allArgs)
              unboxed <- unbox env symbolTable qt ret
              return (symbolTable, unboxed, Just ret)

            _ -> do
              (_, pap, _) <- generateExp env { isLast = False } symbolTable fn'

              pap' <- safeBitcast pap boxType

              let argc = i32ConstOp (fromIntegral $ List.length allArgs)
              ret     <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> allArgs)
              unboxed <- unbox env symbolTable qt ret
              return (symbolTable, unboxed, Just ret)

          _ -> do
            (_, pap, _) <- generateExp env { isLast = False } symbolTable fn'

            pap' <- safeBitcast pap boxType

            let argc = i32ConstOp (fromIntegral $ List.length allArgs)
            ret     <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> allArgs)
            unboxed <- unbox env symbolTable qt ret
            return (symbolTable, unboxed, Just ret)

      Core.Typed _ _ _ (Core.Placeholder (Core.MethodRef "Number" _ True, _) _) -> do
        (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
        pap' <- safeBitcast pap boxType

        let argc = i32ConstOp (fromIntegral $ List.length args)

        args'  <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox env symbolTable qt ret
        return (symbolTable, unboxed, Just ret)

      _ -> do
        (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
        pap' <- safeBitcast pap boxType

        let argc = i32ConstOp (fromIntegral $ List.length args)

        args'  <- mapM (generateExp env { isLast = False } symbolTable) args
        boxedArgs <- retrieveArgs (Core.getMetadata <$> args) args'

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox env symbolTable qt ret
        return (symbolTable, unboxed, Just ret)

  -- (Core.Placeholder ( ClassRef "Functor" [] True False , "List" )
  Core.Typed qt _ _ (Core.Placeholder (Core.ClassRef _ _ True _, _) _) -> do
    (dictArgs, fn) <- collectDictArgs symbolTable exp
    (_, pap, _) <- generateExp env { isLast = False } symbolTable fn
    pap' <- safeBitcast pap boxType
    let argc = i32ConstOp $ fromIntegral (List.length dictArgs)
    dictArgs' <- mapM box dictArgs
    let dictArgs'' = (,[]) <$> dictArgs'
    ret <- call applyPAP $ [(pap', []), (argc, [])] ++ dictArgs''
    unboxed <- unbox env symbolTable qt ret
    return (symbolTable, unboxed, Just ret)

  Core.Typed _ _ _ Core.TypedHole -> do
    call typedHoleReached []
    return (symbolTable, Operand.ConstantOperand $ Constant.Null (Type.ptr Type.i1), Nothing)

  Core.Typed (_ IT.:=> t) _ _ (Core.Literal (Core.LNum n)) -> case t of
    IT.TCon (IT.TC "Float" _) _ ->
      return (symbolTable, C.double (read n), Nothing)

    IT.TCon (IT.TC "Integer" _) _ ->
      return (symbolTable, C.int64 (read n), Nothing)

    IT.TCon (IT.TC "Byte" _) _ ->
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

  Core.Typed _ _ _ (Core.Literal (Core.LStr (leading : s))) | leading == '"' || leading == '\'' -> do
    addr <- if List.null s then buildStr [] else buildStr (List.init s)
    return (symbolTable, addr, Nothing)

  Core.Typed _ _ _ (Core.Literal (Core.LStr s)) -> do
    addr <- buildStr s
    return (symbolTable, addr, Nothing)

  Core.Typed _ _ _ (Core.Literal (Core.LChar c)) -> do
    return (symbolTable, Operand.ConstantOperand $ Constant.Int 32 (fromIntegral $ fromEnum c), Nothing)

  Core.Typed _ _ _ (Core.Do exps) -> do
    (ret, boxed) <- generateDoExps env { isLast = False } symbolTable exps
    return (symbolTable, ret, boxed)

  Core.Typed _ _ _ (Core.TupleConstructor exps) -> do
    -- exps'     <- mapM (((\(_, a, _) -> a) <$>). generateExp env { isLast = False } symbolTable) exps
    exps'     <- mapM (generateExp env { isLast = False } symbolTable) exps
    boxedExps <- retrieveArgs (Core.getMetadata <$> exps) exps'
    let expsWithIds = List.zip boxedExps [0..]
        tupleType   = Type.StructureType False (typeOf <$> boxedExps)
    tuplePtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof tupleType, [])]
    tuplePtr' <- safeBitcast tuplePtr (Type.ptr tupleType)
    Monad.foldM_ (storeItem tuplePtr') () expsWithIds

    return (symbolTable, tuplePtr', Nothing)

  Core.Typed _ _ _ (Core.ListConstructor []) -> do
    -- an empty list is { value: null, next: null }
    emptyList' <- emptyList
    return (symbolTable, emptyList', Nothing)

  Core.Typed _ _ metadata (Core.ListConstructor [
      Core.Typed _ _ _ (Core.ListItem li),
      Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
    ]) | Core.isRightListRecursiveCall metadata -> do
      let Just continue = continueRef <$> recursionData env
      let Just params   = boxedParams <$> recursionData env
      let Just endPtr   = end <$> recursionData env
      endValue <- load endPtr 0

      store continue 0 (Operand.ConstantOperand (Constant.Int 1 1))

      let filteredArgs = List.filter (not . IT.isPlaceholderDict . Core.getQualType) args

      args'  <- mapM (generateExp env { isLast = False } symbolTable) filteredArgs
      let unboxedArgs = (\(_, x, _) -> x) <$> args'

      (_, item, maybeBoxedItem) <- generateExp env symbolTable li
      item' <- case maybeBoxedItem of
        Just boxed ->
          return boxed

        Nothing ->
          box item

      newNode <- call gcMalloc [(Operand.ConstantOperand $ sizeof (Type.StructureType False [boxType, boxType]), [])]
      newNode' <- addrspacecast newNode listType
      storeItem newNode' () (Operand.ConstantOperand (Constant.Null boxType), 0)
      storeItem newNode' () (Operand.ConstantOperand (Constant.Null boxType), 1)
      storeItem endValue () (item', 0)
      storeItem endValue () (newNode, 1)

      -- end = end.next
      store endPtr 0 newNode'

      -- We need to reverse because we may have some closured variables in the params and these need not be updated
      let paramUpdateData = List.reverse $ List.zip3 (List.reverse $ Core.getQualType <$> filteredArgs) (List.reverse params) (List.reverse unboxedArgs)
      mapM_ (\(qt, ptr, exp) -> updateTCOArg symbolTable qt ptr exp) paramUpdateData

      -- return (symbolTable, Operand.ConstantOperand (Constant.Undef Type.i8), Nothing)
      return (symbolTable, Operand.ConstantOperand (Constant.Undef (typeOf endValue)), Nothing)


  Core.Typed _ _ _ (Core.ListConstructor listItems) -> do
    tail <- case List.last listItems of
      Core.Typed _ _ _ (Core.ListItem lastItem) -> do
        item <- generateExp env { isLast = False } symbolTable lastItem
        items <- retrieveArgs [Core.getMetadata lastItem] [item]
        call madlistSingleton [(List.head items, [])]

      Core.Typed _ _ _ (Core.ListSpread spread) -> do
        (_, e, _) <- generateExp env { isLast = False } symbolTable spread
        return e

    list <- Monad.foldM
      (\list' i -> case i of
        Core.Typed _ _ _ (Core.ListItem item) -> do
          item' <- generateExp env { isLast = False } symbolTable item
          items <- retrieveArgs [Core.getMetadata item] [item']
          call madlistPush [(List.head items, []), (list', [])]

        Core.Typed _ _ _ (Core.ListSpread spread) -> do
          (_, spread, _)  <- generateExp env { isLast = False } symbolTable spread
          call madlistConcat [(spread, []), (list', [])]
      )
      tail
      (List.reverse $ List.init listItems)

    return (symbolTable, list, Nothing)

  Core.Typed _ _ _ (Core.Record fields) -> do
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

    record <- call buildRecord $ [(fieldCount, []), (base', [])] ++ ((,[]) <$> fields'')
    return (symbolTable, record, Nothing)
    where
      generateField :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Core.Field -> m Operand
      generateField symbolTable field = case field of
        Core.Typed _ _ _ (Core.Field (name, value)) -> do
          let fieldType = Type.StructureType False [stringType, boxType]
          nameOperand <- buildStr name -- workaround for now, we need to remove the wrapping "
          field  <- generateExp env { isLast = False } symbolTable value
          fields <- retrieveArgs [Core.getMetadata value] [field]

          fieldPtr    <- call gcMalloc [(Operand.ConstantOperand $ sizeof fieldType, [])]
          fieldPtr'   <- safeBitcast fieldPtr (Type.ptr fieldType)

          Monad.foldM_ (storeItem fieldPtr') () [(nameOperand, 0), (List.head fields, 1)]
          return fieldPtr'

        _ ->
          undefined

  Core.Typed qt _ _ (Core.Access record@(Core.Typed (_ IT.:=> recordType) _ _ _) (Core.Typed _ _ _ (Core.Var ('.' : fieldName) _))) -> do
    (_, recordOperand, _) <- generateExp env { isLast = False } symbolTable record
    value <- case recordType of
      IT.TRecord fields Nothing _ -> do
        recordOperand' <- safeBitcast recordOperand (Type.ptr $ Type.StructureType False [Type.i32, boxType])
        let fieldType = Type.StructureType False [stringType, boxType]
        let index = fromIntegral $ Maybe.fromMaybe 0 (List.elemIndex fieldName (Map.keys fields))
        fieldsOperand   <- gep recordOperand' [i32ConstOp 0, i32ConstOp 1] -- i8**
        fieldsOperand'  <- load fieldsOperand 0 -- i8*
        fieldsOperand'' <- safeBitcast fieldsOperand' (Type.ptr (Type.ptr fieldType))
        field           <- gep fieldsOperand'' [i32ConstOp index]
        field'          <- load field 0
        value           <- gep field' [i32ConstOp 0, i32ConstOp 1]
        load value 0

      _ -> do
        nameOperand <- buildStr fieldName
        call selectField [(nameOperand, []), (recordOperand, [])]

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

  Core.Typed _ _ _ (Core.Where exp iss) -> mdo
    (_, exp', _) <- generateExp env { isLast = False } symbolTable exp
    branches     <- generateBranches env symbolTable exitBlock exp' iss

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


generateBranches :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> AST.Name -> Operand -> [Core.Is] -> m [(Operand, AST.Name)]
generateBranches env symbolTable exitBlock whereExp iss = case iss of
  (is : next) -> do
    branch <- generateBranch env symbolTable (not (List.null next)) exitBlock whereExp is
    next'  <- generateBranches env symbolTable exitBlock whereExp next
    return $ branch ++ next'

  [] ->
    return []


generateBranch :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Bool -> AST.Name -> Operand -> Core.Is -> m [(Operand, AST.Name)]
generateBranch env symbolTable hasMore exitBlock whereExp is = case is of
  Core.Typed _ _ _ (Core.Is pat exp) -> mdo
    test      <- generateBranchTest env symbolTable pat whereExp
    currBlock <- currentBlock
    condBr test branchExpBlock nextBlock

    branchExpBlock <- block `named` "branchExpBlock"
    symbolTable' <- generateSymbolTableForPattern env symbolTable whereExp pat
    (_, branchResult, _) <- generateExp env symbolTable' exp
    branchResult' <- return branchResult
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


generateSymbolTableForIndexedData :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> Operand -> SymbolTable -> (Core.Pattern, Integer) -> m SymbolTable
generateSymbolTableForIndexedData env basePtr symbolTable (pat, index) = do
  ptr   <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  ptr'  <- load ptr 0
  ptr'' <- unbox env symbolTable (getQualType pat) ptr'
  generateSymbolTableForPattern env symbolTable ptr'' pat


generateSymbolTableForList :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Operand -> [Core.Pattern] -> m SymbolTable
generateSymbolTableForList env symbolTable basePtr pats = case pats of
  (pat : next) -> case pat of
    Core.Typed _ _ _ (Core.PSpread spread) ->
      generateSymbolTableForPattern env symbolTable basePtr spread

    _ -> do
      valuePtr      <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
      valuePtr'     <- load valuePtr 0
      valuePtr''    <- unbox env symbolTable (getQualType pat) valuePtr'
      symbolTable'  <- generateSymbolTableForPattern env symbolTable valuePtr'' pat
      nextNodePtr   <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
      -- i8*
      nextNodePtr'  <- load nextNodePtr 0
      -- { i8*, i8* }*
      nextNodePtr'' <- addrspacecast nextNodePtr' listType
      generateSymbolTableForList env symbolTable' nextNodePtr'' next

  [] ->
    return symbolTable



generateSymbolTableForPattern :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Operand -> Core.Pattern -> m SymbolTable
generateSymbolTableForPattern env symbolTable baseExp pat = case pat of
  Core.Typed _ _ _ (Core.PVar n) -> do
    return $ Map.insert n (varSymbol baseExp) symbolTable

  Core.Typed _ _ _ Core.PAny ->
    return symbolTable

  Core.Typed _ _ _ Core.PNum{} ->
    return symbolTable

  Core.Typed _ _ _ Core.PBool{} ->
    return symbolTable

  Core.Typed _ _ _ Core.PStr{} ->
    return symbolTable

  Core.Typed _ _ _ Core.PChar{} ->
    return symbolTable

  Core.Typed _ _ _ (Core.PTuple pats) -> do
    let patsWithIds = List.zip pats [0..]
    Monad.foldM (generateSymbolTableForIndexedData env baseExp) symbolTable patsWithIds

  Core.Typed _ _ _ (Core.PList pats) ->
    generateSymbolTableForList env symbolTable baseExp pats

  Core.Typed _ _ _ (Core.PCon _ pats) -> do
    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- safeBitcast baseExp constructorType
    let patsWithIds = List.zip pats [1..]
    Monad.foldM (generateSymbolTableForIndexedData env constructor') symbolTable patsWithIds

  Core.Typed _ _ _ (Core.PRecord fieldPatterns) -> do
    subPatterns <- mapM (getFieldPattern env symbolTable baseExp) $ Map.toList fieldPatterns
    Monad.foldM (\previousTable (field, pat) -> generateSymbolTableForPattern env previousTable field pat) symbolTable subPatterns

  _ ->
    undefined


generateSubPatternTest :: (MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Operand -> (Core.Pattern, Operand) -> m Operand
generateSubPatternTest env symbolTable prev (pat', ptr) = do
  v <- load ptr 0
  v' <- unbox env symbolTable (getQualType pat') v
  curr <- generateBranchTest env symbolTable pat' v'
  prev `Instruction.and` curr


generateListSubPatternTest :: (MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Operand -> [Core.Pattern] -> m Operand
generateListSubPatternTest env symbolTable basePtr pats = case pats of
  (pat : next) -> case pat of
    Core.Typed _ _ _ (Core.PSpread _) ->
      return true

    _ -> do
      valuePtr      <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
      valuePtr'     <- load valuePtr 0
      valuePtr''    <- unbox env symbolTable (getQualType pat) valuePtr'
      test          <- generateBranchTest env symbolTable pat valuePtr''
      nextNodePtr   <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
      -- i8*
      nextNodePtr'  <- load nextNodePtr 0
      -- { i8*, i8* }*
      nextNodePtr'' <- addrspacecast nextNodePtr' listType
      nextTest      <- generateListSubPatternTest env symbolTable nextNodePtr'' next
      test `Instruction.and` nextTest

  [] ->
    return true


generateBranchTest :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> Core.Pattern -> Operand -> m Operand
generateBranchTest env symbolTable pat value = case pat of
  Core.Typed (_ IT.:=> t) _ _ (Core.PNum n) -> case t of
    IT.TCon (IT.TC "Byte" IT.Star) "prelude" ->
      icmp IntegerPredicate.EQ (C.int8 (read n)) value

    IT.TCon (IT.TC "Integer" IT.Star) "prelude" ->
      icmp IntegerPredicate.EQ (C.int64 (read n)) value

    IT.TCon (IT.TC "Float" IT.Star) "prelude" ->
      fcmp FloatingPointPredicate.OEQ (C.double (read n)) value

    _ ->
      icmp IntegerPredicate.EQ (C.int64 (read n)) value

  Core.Typed _ _ _ (Core.PBool "true") ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 1) value

  Core.Typed _ _ _ (Core.PBool _) ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 0) value

  Core.Typed _ _ _ (Core.PStr s) -> do
    s' <- buildStr (List.init . List.tail $ s)
    call areStringsEqual [(s', []), (value, [])]

  Core.Typed _ _ _ (Core.PChar c) -> do
    let char = Operand.ConstantOperand $ Constant.Int 32 (fromIntegral $ fromEnum c)
    icmp IntegerPredicate.EQ char value

  Core.Typed _ _ _ Core.PAny ->
    return true

  Core.Typed _ _ _ Core.PVar{} ->
    return true

  Core.Typed _ _ _ (Core.PTuple pats) -> do
    let indices = List.take (List.length pats) [0..]
    itemPtrs <- getStructPointers indices value
    let patsWithPtrs = List.zip pats itemPtrs
    Monad.foldM (generateSubPatternTest env symbolTable) true patsWithPtrs

  Core.Typed _ _ _ (Core.PList pats) -> mdo
    let hasSpread = List.any isSpread pats

    lengthTestBlock' <- currentBlock
    lengthTest <-
      if hasSpread then do
        call madlistHasMinLength [(C.int64 (fromIntegral $ List.length pats - 1), []), (value, [])]
      else
        call madlistHasLength [(C.int64 (fromIntegral $ List.length pats), []), (value, [])]
    condBr lengthTest subPatternTestBlock testResultBlock

    subPatternTestBlock <- block `named` "subPatternTestBlock"
    subPatternsTest <- generateListSubPatternTest env symbolTable value pats
    subPatternTestBlock' <- currentBlock
    br testResultBlock

    testResultBlock <- block `named` "testResultBlock"
    phi [(subPatternsTest, subPatternTestBlock'), (lengthTest, lengthTestBlock')]

    where
      isSpread :: Core.Pattern -> Bool
      isSpread pat = case pat of
        Core.Typed _ _ _ Core.PSpread{} ->
          True

        _ ->
          False

  Core.Typed _ _ _ (Core.PRecord fieldPatterns) -> do
    subPatterns <- mapM (getFieldPattern env symbolTable value) $ Map.toList fieldPatterns
    subTests    <- mapM (uncurry (generateBranchTest env symbolTable) . Tuple.swap) subPatterns
    Monad.foldM Instruction.and (List.head subTests) (List.tail subTests)

  Core.Typed _ _ _ (Core.PCon name pats) -> do
    let constructorId = case Map.lookup name symbolTable of
          Just (Symbol (ConstructorSymbol id _) _) ->
            i64ConstOp $ fromIntegral id

          _ ->
            -- This is necessary to make the special case of Dictionary constructor
            if "Dictionary" `List.isSuffixOf` name then
              i64ConstOp 0
            else
              error $ "Core.Constructor '" <> name <> "' not found!"

    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- safeBitcast value constructorType
    let argIds = fromIntegral <$> List.take (List.length pats) [1..]
    constructorArgPtrs <- getStructPointers argIds constructor'
    let patsWithPtrs = List.zip pats constructorArgPtrs

    id              <- gep constructor' [i32ConstOp 0, i32ConstOp 0]
    id'             <- load id 0
    testIds         <- icmp IntegerPredicate.EQ constructorId id'

    testSubPatterns <- Monad.foldM (generateSubPatternTest env symbolTable) true patsWithPtrs

    testIds `Instruction.and` testSubPatterns

  _ ->
    undefined


getFieldPattern :: (MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Operand -> (String, Core.Pattern) -> m (Operand, Core.Pattern)
getFieldPattern env symbolTable record (fieldName, fieldPattern) = do
  nameOperand <- buildStr fieldName
  field       <- call selectField [(nameOperand, []), (record, [])]
  field'      <- unbox env symbolTable (getQualType fieldPattern) field
  return (field', fieldPattern)


getStructPointers :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [Integer] -> Operand -> m [Operand]
getStructPointers ids ptr = case ids of
  (index : nextIndices) -> do
    ptr'  <- gep ptr [i32ConstOp 0, i32ConstOp index]
    next  <- getStructPointers nextIndices ptr
    return $ ptr' : next

  [] ->
    return []


generateExps :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Core.Exp] -> m ()
generateExps env symbolTable exps = case exps of
  [exp] -> do
    generateExp env { isTopLevel = isTopLevelAssignment exp } symbolTable exp
    return ()

  (exp : es) -> do
    (symbolTable', _, _) <- generateExp env { isTopLevel = isTopLevelAssignment exp } symbolTable exp
    generateExps env symbolTable' es

  _ ->
    return ()


generateExternFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> IT.Qual IT.Type -> String -> Int -> Operand -> m SymbolTable
generateExternFunction env symbolTable (ps IT.:=> t) functionName arity foreignFn = do
  let paramTypes    = ([] IT.:=>) <$> IT.getParamTypes t
      amountOfDicts = List.length ps
      params'       = List.replicate (arity + amountOfDicts) (boxType, NoParameterName)
      functionName' = AST.mkName functionName

  function <- function functionName' params' boxType $ \params -> do
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


makeParamName :: String -> ParameterName
makeParamName = ParameterName . stringToShortByteString


generateFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Bool -> [Core.Metadata] -> IT.Qual IT.Type -> String -> [Core String] -> [Core.Exp] -> m SymbolTable
generateFunction env symbolTable isMethod metadata (ps IT.:=> t) functionName coreParams body = do
  let paramTypes    = (\t' -> IT.selectPredsForType ps t' IT.:=> t') <$> IT.getParamTypes t
      params'       = (boxType,) . makeParamName <$> (Core.getValue <$> coreParams)
      functionName' = AST.mkName functionName
      dictCount     = List.length $ List.filter ("$" `List.isPrefixOf`) (Core.getValue <$> coreParams)

  function <- function functionName' params' boxType $ \params ->
    if Core.isTCODefinition metadata then mdo
      entry          <- block `named` "entry"
      continue       <- alloca Type.i1 Nothing 0

      let typesWithParams = List.zip paramTypes params
      unboxedParams <- mapM (uncurry (unbox env symbolTable)) typesWithParams

      allocatedParams <-
        mapM
          (\param -> do
            ptr <- alloca (typeOf param) Nothing 0
            store ptr 0 param
            return ptr
          )
          unboxedParams

      recData <-
            if Core.isPlainRecursiveDefinition metadata then do
              return $
                PlainRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = List.drop dictCount allocatedParams
                  }
            else if Core.isRightListRecursiveDefinition metadata then do
              start       <- call gcMalloc [(Operand.ConstantOperand $ sizeof (Type.StructureType False [boxType, boxType]), [])]
              start'      <- addrspacecast start listType
              end         <- alloca listType Nothing 0
              store end 0 start'

              return $
                RightListRecursionData
                  { entryBlockName = entry
                  , continueRef = continue
                  , boxedParams = List.drop dictCount allocatedParams
                  , start = start'
                  , end = end
                  }
            else if Core.isConstructorRecursiveDefinition metadata then do
              let returnType = IT.getReturnType t
                  constructedType = retrieveConstructorStructType env symbolTable returnType
              -- contains an unused index and the value that will be returned at the end of recursion
              start  <- call gcMalloc [(Operand.ConstantOperand $ sizeof (Type.StructureType False [Type.i64, constructedType]), [])]
              start' <- safeBitcast start (Type.ptr (Type.StructureType False [Type.i64, constructedType]))
              end    <- alloca constructedType Nothing 0
              -- hole :: i8**
              hole   <- gep start' [i32ConstOp 0, i32ConstOp 1]

              -- holePtr: i8***
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
            else
              undefined
      br loop

      loop <- block `named` "loop"
      store continue 0 (Operand.ConstantOperand (Constant.Int 1 0))

      let paramsWithNames       = Map.fromList $ List.zip (Core.getValue <$> coreParams) (uncurry tcoParamSymbol <$> List.zip allocatedParams unboxedParams)
          symbolTableWithParams = symbolTable <> paramsWithNames


      -- Generate body
      (generatedBody, maybeBoxed) <- generateBody env { recursionData = Just recData } symbolTableWithParams body

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
        (\(Typed paramQt _ metadata paramName, param) ->
            if Core.isReferenceParameter metadata then do
              param' <- safeBitcast param (Type.ptr boxType)
              loaded <- load param' 0
              unboxed <- unbox env symbolTable paramQt loaded
              return (paramName, localVarSymbol param unboxed)
            else do
              unboxed <- unbox env symbolTable paramQt param
              return (paramName, varSymbol unboxed)
        )
        (List.zip coreParams params)

      let symbolTableWithParams = symbolTable <> Map.fromList paramsWithNames

      -- Generate body
      (generatedBody, _) <- generateBody env symbolTableWithParams body

      boxed <- box generatedBody
      ret boxed

  let symbolConstructor =
        if isMethod then
          methodSymbol
        else
          fnSymbol

  Writer.tell $ Map.singleton functionName (symbolConstructor (List.length coreParams) function)
  return $ Map.insert functionName (symbolConstructor (List.length coreParams) function) symbolTable


generateTopLevelFunction :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Core.Exp -> m SymbolTable
generateTopLevelFunction env symbolTable topLevelFunction = case topLevelFunction of
  Core.Typed _ _ _ (Core.Assignment functionName (Core.Typed qt _ metadata (Core.Definition params body))) -> do
    generateFunction env symbolTable False metadata qt functionName params body

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


-- TODO: for now closure convertion strips exports but at some point we want these back
-- in order to generate minimal symboltables with only exported stuff.
-- At that point we'll have to process the commented Export expressions.
addTopLevelFnToSymbolTable :: Env -> SymbolTable -> Core.Exp -> SymbolTable
addTopLevelFnToSymbolTable env symbolTable topLevelFunction = case topLevelFunction of
  Core.Typed _ _ _ (Core.Assignment functionName (Core.Typed _ _ _ (Core.Definition params _))) ->
    let arity  = List.length params
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Core.Typed _ _ _ (Core.Extern (_ IT.:=> t) functionName _) ->
    let arity  = List.length $ IT.getParamTypes t
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  Map.insert functionName (fnSymbol arity fnRef) symbolTable

  Core.Typed qt@(_ IT.:=> t) _ _ (Core.Assignment name _) ->
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


generateDoExps :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Core.Exp] -> m (Operand, Maybe Operand)
generateDoExps env symbolTable exps = case exps of
  [exp] -> do
    (_, result, _) <- generateExp env symbolTable exp
    return (result, Nothing)

  (exp : es) -> do
    (symbolTable', _, _) <- generateExp env { isLast = False } symbolTable exp
    generateBody env symbolTable' es

  _ ->
    undefined


generateBody :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Env -> SymbolTable -> [Core.Exp] -> m (Operand, Maybe Operand)
generateBody env symbolTable exps = case exps of
  [exp] -> do
    (_, result, _) <- generateExp env { isLast = True } symbolTable exp
    return (result, Nothing)
    -- return (result, boxed)

  (exp : es) -> do
    (symbolTable', _, _) <- generateExp env symbolTable exp
    generateBody env symbolTable' es

  _ ->
    undefined


generateTopLevelFunctions :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> [Core.Exp] -> m SymbolTable
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
generateConstructor :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Int -> SymbolTable -> (Core.Constructor, Int) -> m SymbolTable
generateConstructor maxArity symbolTable (constructor, index) = case constructor of
  Core.Untyped _ _ (Core.Constructor constructorName _ t) -> do
    let paramTypes     = IT.getParamTypes t
    let arity          = List.length paramTypes
    let structType     = Type.StructureType False $ Type.IntegerType 64 : List.replicate maxArity boxType
    let paramLLVMTypes = (,NoParameterName) <$> List.replicate arity boxType

    constructor' <- function (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
      block `named` "entry"
      -- allocate memory for the structure
      structPtr     <- call gcMalloc [(Operand.ConstantOperand $ sizeof structType, [])]
      structPtr'    <- safeBitcast structPtr $ Type.ptr structType

      -- store the constructor data in the struct
      Monad.foldM_ (storeItem structPtr') () $ List.zip params [1..] ++ [(i64ConstOp (fromIntegral index), 0)]

      boxed <- box structPtr'
      ret boxed

    Writer.tell $ Map.singleton constructorName (constructorSymbol constructor' index arity)
    return $ Map.insert constructorName (constructorSymbol constructor' index arity) symbolTable

  _ ->
    undefined


findMaximumConstructorArity :: [Constructor] -> Int
findMaximumConstructorArity constructors =
  List.foldr max 0 (Core.getConstructorArity <$> constructors)


generateConstructorsForADT :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Core.TypeDecl -> m SymbolTable
generateConstructorsForADT env symbolTable adt = case adt of
  Core.Untyped _ _ Core.ADT { Core.adtconstructors, Core.adtname } -> do
    let sortedConstructors  = List.sortBy (\a b -> compare (getConstructorName a) (getConstructorName b)) adtconstructors
        indexedConstructors = List.zip sortedConstructors [0..]
        maxArity            = findMaximumConstructorArity adtconstructors
        symbolTable'        = Map.insert (envASTPath env <> "__" <> adtname) (adtSymbol maxArity) symbolTable
    Writer.tell $ Map.singleton (envASTPath env <> "__" <> adtname) (adtSymbol maxArity)
    Monad.foldM (generateConstructor maxArity) symbolTable' indexedConstructors

  _ ->
    return symbolTable


generateConstructors :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> [Core.TypeDecl] -> m SymbolTable
generateConstructors env symbolTable tds =
  Monad.foldM (generateConstructorsForADT env) symbolTable tds


buildDictValues :: Env -> SymbolTable -> [(String, Core.Exp)] -> [Constant.Constant]
buildDictValues env symbolTable methods = case methods of
  ((methodName, methodExp) : ns) ->
    let arity      = List.length $ IT.getParamTypes (Core.getType methodExp)
        methodType =
          case methodExp of
            Core.Typed _ _ _ (Core.Assignment _ (Core.Typed _ _ _ (Core.Definition params _))) ->
              Type.ptr $ Type.FunctionType boxType (List.replicate (List.length params) boxType) False

            _ ->
              if arity == 0 then
                Type.ptr $ Type.FunctionType boxType [] False
              else
                Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False

        methodRef  = Constant.GlobalReference methodType (AST.mkName methodName)
        methodRef' = Constant.BitCast methodRef boxType
        pap        = Constant.Struct Nothing False [methodRef', Constant.Int 32 (fromIntegral arity), Constant.Int 32 (fromIntegral arity), Constant.Undef boxType]
        next       = buildDictValues env symbolTable ns
    in  pap : next

  [] ->
    []


generateMethod :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> String -> Core.Exp -> m SymbolTable
generateMethod env symbolTable methodName exp = case exp of
  -- TODO: handle overloaded methods that should be passed a dictionary
  Core.Typed _ _ _ (Core.Assignment _ (Core.Typed qt _ metadata (Core.Definition params body))) ->
    generateFunction env symbolTable True metadata qt methodName params body

  -- TODO: reconsider this
  Core.Typed (_ IT.:=> t) _ _ (Core.Assignment _ exp) -> do
    let paramTypes  = IT.getParamTypes t
        arity       = List.length paramTypes
        params'     = (,NoParameterName) <$> (boxType <$ paramTypes)

    f <- function (AST.mkName methodName) params' boxType $ \params -> do
      block `named` "entry"
      (_, exp', _) <- generateExp env symbolTable exp

      retVal <-
        if arity > 0 then do
          pap <- safeBitcast exp' boxType
          call applyPAP $ [(pap, []), (i32ConstOp (fromIntegral arity), [])] ++ ((,[]) <$> params)
        else do
          box exp'

      ret retVal

    Writer.tell $ Map.singleton methodName (methodSymbol arity f)
    return $ Map.insert methodName (methodSymbol arity f) symbolTable

  _ ->
    undefined


generateInstance :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> Core.Instance -> m SymbolTable
generateInstance env symbolTable inst = case inst of
  Core.Untyped _ _ (Core.Instance interface _ typingStr methods) -> do
    let instanceName     = "$" <> interface <> "$" <> typingStr
        prefixedMethods  = Map.mapKeys ((instanceName <> "$") <>) methods
        prefixedMethods' = (\(name, method) -> (name, method)) <$> Map.toList prefixedMethods
        -- for recursive types we need to have the current dict in the symbol table
    let methodConstants = buildDictValues env symbolTable (Map.toList (fst <$> prefixedMethods))
    dict <- global (AST.mkName instanceName) (Type.StructureType False (typeOf <$> methodConstants)) $ Constant.Struct Nothing False methodConstants
    symbolTableWithDict <- return $ Map.insert instanceName (Symbol (DictionarySymbol (Map.fromList $ List.zip (Map.keys methods) [0..])) dict) symbolTable
    symbolTable' <- Monad.foldM (\symbolTable (name, (method, _)) -> generateMethod env symbolTable name method) symbolTableWithDict prefixedMethods'


    Writer.tell $ Map.singleton instanceName (Symbol (DictionarySymbol (Map.fromList $ List.zip (Map.keys methods) [0..])) dict)
    return $ Map.insert instanceName (Symbol (DictionarySymbol (Map.fromList  $ List.zip (Map.keys methods) [0..])) dict) symbolTable'

  _ ->
    undefined


generateInstances :: (Writer.MonadWriter SymbolTable m, MonadFix.MonadFix m, MonadModuleBuilder m) => Env -> SymbolTable -> [Core.Instance] -> m SymbolTable
generateInstances env =
  Monad.foldM (generateInstance env)


addMethodToSymbolTable :: SymbolTable -> Core.Exp -> ([(String, Operand)], SymbolTable)
addMethodToSymbolTable symbolTable topLevelFunction = case topLevelFunction of
  Core.Typed _ _ _ (Core.Assignment functionName (Core.Typed _ _ _ (Core.Definition params _))) ->
    let arity  = List.length params
        fnType = Type.ptr $ Type.FunctionType boxType (List.replicate arity boxType) False
        fnRef  = Operand.ConstantOperand (Constant.GlobalReference fnType (AST.mkName functionName))
    in  ([(functionName, fnRef)], Map.insert functionName (methodSymbol arity fnRef) symbolTable)

  Core.Typed (_ IT.:=> t) _ _ (Core.Assignment name _) ->
    let paramTypes  = IT.getParamTypes t
        arity       = List.length paramTypes
        paramTypes' = boxType <$ paramTypes
        expType     = Type.ptr $ Type.FunctionType boxType paramTypes' False
        globalRef   = Operand.ConstantOperand (Constant.GlobalReference expType (AST.mkName name))
    in  ([(name, globalRef)], Map.insert name (methodSymbol arity globalRef) symbolTable)

  _ ->
    ([], symbolTable)


updateMethodName :: Core.Exp -> String -> Core.Exp
updateMethodName exp newName = case exp of
  Core.Typed _ _ assignmentMetadata (Core.Assignment _ (Core.Typed t area definitionMetadata (Core.Definition params body))) ->
    Core.Typed t area assignmentMetadata (Core.Assignment newName (Core.Typed t area definitionMetadata (Core.Definition params body)))

  Core.Typed t area metadata (Core.Assignment _ exp) ->
    Core.Typed t area metadata (Core.Assignment newName exp)

  _ ->
    undefined

addInstanceToSymbolTable :: SymbolTable -> Core.Instance -> SymbolTable
addInstanceToSymbolTable symbolTable inst = case inst of
  Core.Untyped _ _ (Core.Instance interface _ typingStr methods) -> do
    let instanceName = "$" <> interface <> "$" <> typingStr
        prefixedMethods = Map.mapKeys ((instanceName <> "$") <>) methods
        updatedMethods  = Map.elems $ Map.mapWithKey (\name (exp, _) -> updateMethodName exp name) prefixedMethods

    let (methodOperands, symbolTable') =
          List.foldl'
            (\(currentMtds, currentTable) e ->
                let (mtds, st') = addMethodToSymbolTable currentTable e
                in  (currentMtds ++ mtds, st')
            )
            ([], symbolTable)
            updatedMethods
        dictType = Type.ptr $ Type.StructureType False (typeOf <$> (snd <$> methodOperands))
        dict = Operand.ConstantOperand (Constant.GlobalReference dictType (AST.mkName instanceName))
        dictInfo = Map.fromList $ List.zip (fst <$> methodOperands) [0..]

    Map.insert instanceName (Symbol (DictionarySymbol dictInfo) dict) symbolTable'

  _ ->
    undefined


expsForMain :: [Core.Exp] -> [Core.Exp]
expsForMain =
  List.filter (not . \e -> isTopLevelFunction e || isExtern e)


topLevelFunctions :: [Core.Exp] -> [Core.Exp]
topLevelFunctions =
  List.filter $ \e -> isTopLevelFunction e || isExtern e


buildDictionaryIndices :: [Core.Interface] -> Map.Map String (Map.Map String (Int, Int))
buildDictionaryIndices interfaces = case interfaces of
  (Core.Untyped _ _ (Core.Interface name _ _ methods _) : next) ->
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


generateExternForImportName :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Core.Core String -> m ()
generateExternForImportName symbolTable optimizedName = case optimizedName of
  Core.Untyped _ _ name ->
    generateExternalForName symbolTable name

  _ ->
    undefined


generateImport :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Import -> m ()
generateImport symbolTable imp = case imp of
  Core.Untyped _ _ (NamedImport names _ _) ->
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


generateModuleFunctionExternals :: (MonadModuleBuilder m) => [FilePath] -> m ()
generateModuleFunctionExternals allModulePaths = case allModulePaths of
  (path : next) -> do
    let functionName = "__" <> generateHashFromPath path <> "__moduleFunction"
    extern (AST.mkName functionName) [] Type.void

    generateModuleFunctionExternals next

  [] ->
    return ()


callModuleFunctions :: (MonadIRBuilder m, MonadModuleBuilder m) => [FilePath] -> m ()
callModuleFunctions allModulePaths = case allModulePaths of
  (path : next) -> do
    let functionName = "__" <> generateHashFromPath path <> "__moduleFunction"
    call (Operand.ConstantOperand $ Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [] False) (AST.mkName functionName)) []

    callModuleFunctions next

  [] ->
    return ()


tupleVars :: [String]
tupleVars = (:"") <$> ['a'..]

tupleNumbers :: [String]
tupleNumbers = show <$> [1..]

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
buildTupleNEqInstance :: Int -> Core.Instance
buildTupleNEqInstance n =
  let tvarNames          = List.take n tupleVars
      eqDictNames        = ("$Eq$" ++) <$> tvarNames
      tvars              = (\name -> IT.TVar (IT.TV name IT.Star)) <$> tvarNames
      dictTVars          = IT.TVar (IT.TV "eqDict" IT.Star) <$ eqDictNames
      preds              = (\var -> IT.IsIn "Eq" [var] Nothing) <$> tvars
      tupleName          = getTupleName n
      tupleType          = List.foldl' IT.TApp tupleHeadType tvars
      methodQualType     = preds IT.:=> List.foldr IT.fn IT.tBool (dictTVars ++ (tupleType <$ List.take 2 tvars))
      tupleHeadType      = IT.getTupleCtor n
      tupleQualType      = preds IT.:=> List.foldl' IT.TApp tupleHeadType tvars
      whereExpQualType   = preds IT.:=> IT.TApp (IT.TApp IT.tTuple2 tupleType) tupleType
      isQualType         = preds IT.:=> (IT.TApp (IT.TApp IT.tTuple2 tupleType) tupleType `IT.fn` IT.tBool)
      leftTupleVarNames  = ("a" ++) <$> tupleNumbers
      rightTupleVarNames = ("b" ++) <$> tupleNumbers
      leftTuplePatterns  =
        (\(tvName, var) ->
          Core.Typed ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea [] (PVar var)
        ) <$> List.zip tvarNames leftTupleVarNames
      rightTuplePatterns =
        (\(tvName, var) ->
          Core.Typed ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea [] (PVar var)
        ) <$> List.zip tvarNames rightTupleVarNames

      leftVars =
        (\(tvName, var) ->
          Core.Typed ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea [] (Core.Var var False)
        ) <$> List.zip tvarNames leftTupleVarNames
      rightVars =
        (\(tvName, var) ->
          Core.Typed ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea [] (Core.Var var False)
        ) <$> List.zip tvarNames rightTupleVarNames

      eqMethods =
        (\tvName ->
          Core.Typed
            ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> (IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.tBool))
            emptyArea
            []
            (Core.Placeholder (Core.MethodRef "Eq" "==" True, tvName) (
              Core.Typed
              ([IT.IsIn "Eq" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> (IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.tBool))
              emptyArea
              []
              (Core.Var "==" False)
            ))
        ) <$> tvarNames

      conditions = (\(method, leftVar, rightVar) -> Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call method [leftVar, rightVar])) <$> List.zip3 eqMethods leftVars rightVars
      andApp = \left right -> Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed ([] IT.:=> (IT.tBool `IT.fn` IT.tBool `IT.fn` IT.tBool)) emptyArea [] (Core.Var "&&" False)) [left, right])
      condition = List.foldr andApp (Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Literal $ LBool "true")) conditions

  in  Core.Untyped emptyArea [] (Core.Instance
        "Eq"
        preds
        tupleName
        (Map.fromList
          [ ( "=="
            -- Note, the dicts need to be inverted as this happens during dict resolution after type checking
            , ( Core.Typed methodQualType emptyArea [] (Assignment "==" (
                  Core.Typed methodQualType emptyArea [] (Core.Definition (List.reverse (Core.Typed ([] IT.:=> IT.tVar "eqDict") emptyArea [] <$> eqDictNames) ++ (Core.Typed tupleQualType emptyArea [] <$> ["a", "b"])) [
                    Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Where (
                      Core.Typed whereExpQualType emptyArea [] (TupleConstructor [
                        Core.Typed tupleQualType emptyArea [] (Core.Var "a" False),
                        Core.Typed tupleQualType emptyArea [] (Core.Var "b" False)
                      ])
                    ) [
                      Core.Typed isQualType emptyArea [] (Is
                        (Core.Typed whereExpQualType emptyArea [] (PTuple [
                          Core.Typed tupleQualType emptyArea [] (PTuple leftTuplePatterns),
                          Core.Typed tupleQualType emptyArea [] (PTuple rightTuplePatterns)
                        ]))
                        condition
                      )
                    ])
                  ])
                ))
              , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
              )
            )
          ]
        )
      )


-- TODO: remove this once codegen is based on Core!!
stringConcat :: Core.Exp
stringConcat =
  Core.Typed ([] IT.:=> (IT.tStr `IT.fn` IT.tStr `IT.fn` IT.tStr)) emptyArea [] (Core.Var "++" False)


templateStringToCalls :: [Core.Exp] -> Core.Exp
templateStringToCalls exps = case exps of
  [e@(Core.Typed qt area _ _), e'@(Core.Typed qt' area' _ _)] ->
    Core.Typed
      ((IT.preds qt ++ IT.preds qt') IT.:=> IT.tStr)
      (mergeAreas area area')
      []
      (Core.Call stringConcat [e, e'])

  (e@(Core.Typed qt area _ _) : e'@(Core.Typed qt' area' _ _) : next) ->
    let concatenated =
          Core.Typed
            ((IT.preds qt ++ IT.preds qt') IT.:=> IT.tStr)
            (mergeAreas area area')
            []
            (Core.Call stringConcat [e, e'])
        nextStr@(Core.Typed qt'' area'' _ _) = templateStringToCalls next
    in  Core.Typed
          ((IT.preds qt ++ IT.preds qt' ++ IT.preds qt'') IT.:=> IT.tStr)
          (mergeAreas area area'')
          []
          (Core.Call stringConcat [concatenated, nextStr])

  [last] ->
    last

-- generates AST for a tupleN instance. n must be >= 2
buildTupleNInspectInstance :: Int -> Core.Instance
buildTupleNInspectInstance n =
  let tvarNames          = List.take n tupleVars
      inspectDictNames   = ("$Inspect$" ++) <$> tvarNames
      tvars              = (\name -> IT.TVar (IT.TV name IT.Star)) <$> tvarNames
      dictTVars          = IT.TVar (IT.TV "inspectDict" IT.Star) <$ inspectDictNames
      preds              = (\var -> IT.IsIn "Inspect" [var] Nothing) <$> tvars
      tupleName          = getTupleName n
      tupleType          = List.foldl' IT.TApp tupleHeadType tvars
      methodQualType     = preds IT.:=> List.foldr IT.fn IT.tStr (dictTVars ++ (tupleType <$ List.take 1 tvars))
      tupleHeadType      = IT.getTupleCtor n
      tupleQualType      = preds IT.:=> List.foldl' IT.TApp tupleHeadType tvars
      whereExpQualType   = preds IT.:=> IT.TApp (IT.TApp IT.tTuple2 tupleType) tupleType
      isQualType         = preds IT.:=> (IT.TApp (IT.TApp IT.tTuple2 tupleType) tupleType `IT.fn` IT.tStr)
      tupleVarNames  = ("a" ++) <$> tupleNumbers
      tuplePatterns  =
        (\(tvName, var) ->
          Core.Typed ([IT.IsIn "Inspect" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea [] (PVar var)
        ) <$> List.zip tvarNames tupleVarNames

      vars =
        (\(tvName, var) ->
          Core.Typed ([IT.IsIn "Inspect" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> IT.TVar (IT.TV tvName IT.Star)) emptyArea [] (Core.Var var False)
        ) <$> List.zip tvarNames tupleVarNames

      inspectMethods =
        (\tvName ->
          Core.Typed
            ([IT.IsIn "Inspect" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> (IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.tStr))
            emptyArea
            []
            (Core.Placeholder (Core.MethodRef "Inspect" "inspect" True, tvName) (
              Core.Typed
              ([IT.IsIn "Inspect" [IT.TVar (IT.TV tvName IT.Star)] Nothing] IT.:=> (IT.TVar (IT.TV tvName IT.Star) `IT.fn` IT.tStr))
              emptyArea
              []
              (Core.Var "inspect" False)
            ))
        ) <$> tvarNames

      inspectedItems = (\(method, var) -> Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Core.Call method [var])) <$> List.zip inspectMethods vars
      commaSeparatedItems = List.intersperse (Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Literal $ LStr ", ")) inspectedItems
      wrappedItems = [Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Literal $ LStr "#[")] ++ commaSeparatedItems ++ [Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Literal $ LStr "]")]
      inspectedTuple = templateStringToCalls wrappedItems

  in  Core.Untyped emptyArea [] (Core.Instance
        "Inspect"
        preds
        tupleName
        (Map.fromList
          [ ( "inspect"
            -- Note, the dicts need to be inverted as this happens during dict resolution after type checking
            , ( Core.Typed methodQualType emptyArea [] (Core.Assignment "==" (
                  Core.Typed methodQualType emptyArea [] (Core.Definition (List.reverse (Core.Typed ([] IT.:=> IT.tVar "inspectDict") emptyArea [] <$> inspectDictNames) ++ [Core.Typed tupleQualType emptyArea [] "tuple"]) [
                    Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Where (
                      Core.Typed whereExpQualType emptyArea [] (TupleConstructor [
                        Core.Typed tupleQualType emptyArea [] (Core.Var "tuple" False)
                      ])
                    ) [
                      Core.Typed isQualType emptyArea [] (Is
                        (Core.Typed whereExpQualType emptyArea [] (PTuple [
                          Core.Typed tupleQualType emptyArea [] (PTuple tuplePatterns)
                        ]))
                        inspectedTuple
                      )
                    ])
                  ])
                ))
              , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
              )
            )
          ]
        )
      )


buildDefaultInstancesModule :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => Env -> [String] -> SymbolTable -> m ()
buildDefaultInstancesModule env _ initialSymbolTable = do
  let preludeHash = generateHashFromPath "prelude"
  externVarArgs (AST.mkName "__applyPAP__")               [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc")                         [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc_atomic")                  [Type.i64] (Type.ptr Type.i8)

  extern (AST.mkName "madlib__string__internal__concat")  [stringType, stringType] stringType

  -- Inspect Integer
  extern (AST.mkName "madlib__number__internal__inspectInteger")   [boxType] boxType
  -- Inspect Byte
  extern (AST.mkName "madlib__number__internal__inspectByte")      [boxType] boxType
  -- Inspect Char
  extern (AST.mkName "madlib__char__internal__inspect")            [boxType] boxType
  -- Inspect String
  extern (AST.mkName "madlib__string__internal__inspect")          [boxType] boxType
  -- Inspect Float
  extern (AST.mkName "madlib__number__internal__inspectFloat")     [boxType] boxType
  -- Inspect Boolean
  extern (AST.mkName "madlib__boolean__internal__inspectBoolean")  [boxType] boxType
  -- Inspect ByteArray
  extern (AST.mkName "madlib__bytearray__internal__inspect")       [boxType] boxType
  -- Inspect List
  extern (AST.mkName "madlib__list__internal__inspect")            [boxType, boxType] boxType
  -- Inspect Array
  extern (AST.mkName "madlib__array__internal__inspect")           [boxType, boxType] boxType
  -- Inspect Dictionary
  extern (AST.mkName "madlib__dictionary__internal__inspect")      [boxType, boxType, boxType] boxType

  -- Number Integer
  extern (AST.mkName "madlib__number__internal__addIntegers")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__substractIntegers") [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__multiplyIntegers")  [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__divideIntegers")    [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__gtIntegers")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__ltIntegers")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__gteIntegers")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__lteIntegers")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__numberToInteger")   [boxType] boxType
  extern (AST.mkName "madlib__number__internal__negateInteger")     [boxType] boxType

  -- Bits Integer
  extern (AST.mkName "madlib__number__internal__andIntegers")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__orIntegers")         [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__xorIntegers")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__complementIntegers") [boxType] boxType
  extern (AST.mkName "madlib__number__internal__leftShiftIntegers")  [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__rightShiftIntegers") [boxType, boxType] boxType

  -- Number Byte
  extern (AST.mkName "madlib__number__internal__addBytes")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__substractBytes") [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__multiplyBytes")  [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__divideBytes")    [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__gtBytes")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__ltBytes")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__gteBytes")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__lteBytes")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__numberToByte")   [boxType] boxType
  extern (AST.mkName "madlib__number__internal__negateByte")     [boxType] boxType

  -- Bits Byte
  extern (AST.mkName "madlib__number__internal__andBytes")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__orBytes")         [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__xorBytes")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__complementBytes") [boxType] boxType
  extern (AST.mkName "madlib__number__internal__leftShiftBytes")  [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__rightShiftBytes") [boxType, boxType] boxType

  -- Number Float
  extern (AST.mkName "madlib__number__internal__addFloats")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__substractFloats") [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__multiplyFloats")  [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__divideFloats")    [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__gtFloats")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__ltFloats")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__gteFloats")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__lteFloats")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__numberToFloat")   [boxType] boxType
  extern (AST.mkName "madlib__number__internal__negateFloat")     [boxType] boxType

  -- Eq
  extern (AST.mkName "madlib__number__internal__eqInteger") [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__eqByte")    [boxType, boxType] boxType
  extern (AST.mkName "madlib__char__internal__eq")          [boxType, boxType] boxType
  extern (AST.mkName "madlib__number__internal__eqFloat")   [boxType, boxType] boxType
  extern (AST.mkName "madlib__string__internal__eq")        [boxType, boxType] boxType
  extern (AST.mkName "madlib__boolean__internal__eq")       [boxType, boxType] boxType
  extern (AST.mkName "madlib__list__internal__eq")          [boxType, boxType, boxType] boxType
  extern (AST.mkName "madlib__array__internal__eq")         [boxType, boxType, boxType] boxType
  extern (AST.mkName "madlib__bytearray__internal__eq")     [boxType, boxType] boxType
  extern (AST.mkName "madlib__dictionary__internal__eq")    [boxType, boxType, boxType, boxType] boxType

      -- Number Integer
  let addIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__addIntegers")
      substractIntegers = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__substractIntegers")
      multiplyIntegers  = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__multiplyIntegers")
      divideIntegers    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__divideIntegers")
      gtIntegers        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__gtIntegers")
      ltIntegers        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__ltIntegers")
      gteIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__gteIntegers")
      lteIntegers       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__lteIntegers")
      numberToInteger   = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__numberToInteger")
      negateInteger     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__negateInteger")

      -- Bits Integer
      andIntegers        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__andIntegers")
      orIntegers         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__orIntegers")
      xorIntegers        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__xorIntegers")
      complementIntegers = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__complementIntegers")
      leftShiftIntegers  = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__leftShiftIntegers")
      rightShiftIntegers = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__rightShiftIntegers")

      -- Number Byte
      addBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__addBytes")
      substractBytes    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__substractBytes")
      multiplyBytes     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__multiplyBytes")
      divideBytes       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__divideBytes")
      gtBytes           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__gtBytes")
      ltBytes           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__ltBytes")
      gteBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__gteBytes")
      lteBytes          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__lteBytes")
      numberToByte      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__numberToByte")
      negateByte        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__negateByte")

      -- Bits Byte
      andBytes        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__andBytes")
      orBytes         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__orBytes")
      xorBytes        = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__xorBytes")
      complementBytes = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__complementBytes")
      leftShiftBytes  = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__leftShiftBytes")
      rightShiftBytes = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__rightShiftBytes")

      -- Number Float
      addFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__addFloats")
      substractFloats   = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__substractFloats")
      multiplyFloats    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__multiplyFloats")
      divideFloats      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__divideFloats")
      gtFloats          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__gtFloats")
      ltFloats          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__ltFloats")
      gteFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__gteFloats")
      lteFloats         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__lteFloats")
      numberToFloat     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__numberToFloat")
      negateFloat       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__negateFloat")

      -- Eq Integer
      eqInteger         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__eqInteger")

      -- Eq Byte
      eqByte            = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__eqByte")

      -- Eq Char
      eqChar            = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__char__internal__eq")

      -- Eq Float
      eqFloat           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__number__internal__eqFloat")

      -- Eq String
      eqString          = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__string__internal__eq")

      -- Eq Boolean
      eqBoolean         = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__boolean__internal__eq")

      -- Eq List
      eqList            = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType, boxType] False) "madlib__list__internal__eq")

      -- Eq Array
      eqArray           = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType, boxType] False) "madlib__array__internal__eq")

      -- Eq ByteArray
      eqByteArray       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__bytearray__internal__eq")

      -- Eq Dictionary
      eqDictionary      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType, boxType, boxType] False) "madlib__dictionary__internal__eq")

      -- Inspect Integer
      inspectInteger    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__inspectInteger")

      -- Inspect Byte
      inspectByte       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__inspectByte")

      -- Inspect Char
      inspectChar       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__char__internal__inspect")

      -- Inspect String
      inspectString       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__string__internal__inspect")

      -- Inspect Float
      inspectFloat      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__number__internal__inspectFloat")

      -- Inspect Boolean
      inspectBoolean    = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__boolean__internal__inspectBoolean")

      -- Inspect ByteArray
      inspectByteArray  = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) "madlib__bytearray__internal__inspect")

      -- Inspect List
      inspectList       = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__list__internal__inspect")

      -- Inspect Array
      inspectArray      = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False) "madlib__array__internal__inspect")

      -- Inspect Dictionary
      inspectDictionary = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType, boxType, boxType] False) "madlib__dictionary__internal__inspect")


      symbolTableWithCBindings =
        -- Inspect Integer
        Map.insert "madlib__number__internal__inspectInteger" (fnSymbol 1 inspectInteger)
        -- Inspect Byte
        $ Map.insert "madlib__number__internal__inspectByte" (fnSymbol 1 inspectByte)
        -- Inspect Char
        $ Map.insert "madlib__char__internal__inspect" (fnSymbol 1 inspectChar)
        -- Inspect Char
        $ Map.insert "madlib__string__internal__inspect" (fnSymbol 1 inspectString)
        -- Inspect Float
        $ Map.insert "madlib__number__internal__inspectFloat" (fnSymbol 1 inspectFloat)
        -- Inspect Boolean
        $ Map.insert "madlib__boolean__internal__inspectBoolean" (fnSymbol 1 inspectBoolean)
        -- Inspect ByteArray
        $ Map.insert "madlib__bytearray__internal__inspect" (fnSymbol 1 inspectByteArray)
        -- Inspect List
        $ Map.insert "madlib__list__internal__inspect" (fnSymbol 2 inspectList)
        -- Inspect Array
        $ Map.insert "madlib__array__internal__inspect" (fnSymbol 2 inspectArray)
        -- Inspect Dictionary
        $ Map.insert "madlib__dictionary__internal__inspect" (fnSymbol 3 inspectDictionary)
        -- Number Float
        $ Map.insert "madlib__number__internal__addFloats" (fnSymbol 2 addFloats)
        $ Map.insert "madlib__number__internal__substractFloats" (fnSymbol 2 substractFloats)
        $ Map.insert "madlib__number__internal__multiplyFloats" (fnSymbol 2 multiplyFloats)
        $ Map.insert "madlib__number__internal__divideFloats" (fnSymbol 2 divideFloats)
        $ Map.insert "madlib__number__internal__gtFloats" (fnSymbol 2 gtFloats)
        $ Map.insert "madlib__number__internal__ltFloats" (fnSymbol 2 ltFloats)
        $ Map.insert "madlib__number__internal__gteFloats" (fnSymbol 2 gteFloats)
        $ Map.insert "madlib__number__internal__lteFloats" (fnSymbol 2 lteFloats)
        $ Map.insert "madlib__number__internal__numberToFloat" (fnSymbol 1 numberToFloat)
        $ Map.insert "madlib__number__internal__negateFloat" (fnSymbol 1 negateFloat)

        -- Number Byte
        $ Map.insert "madlib__number__internal__addBytes" (fnSymbol 2 addBytes)
        $ Map.insert "madlib__number__internal__substractBytes" (fnSymbol 2 substractBytes)
        $ Map.insert "madlib__number__internal__multiplyBytes" (fnSymbol 2 multiplyBytes)
        $ Map.insert "madlib__number__internal__divideBytes" (fnSymbol 2 divideBytes)
        $ Map.insert "madlib__number__internal__gtBytes" (fnSymbol 2 gtBytes)
        $ Map.insert "madlib__number__internal__ltBytes" (fnSymbol 2 ltBytes)
        $ Map.insert "madlib__number__internal__gteBytes" (fnSymbol 2 gteBytes)
        $ Map.insert "madlib__number__internal__lteBytes" (fnSymbol 2 lteBytes)
        $ Map.insert "madlib__number__internal__numberToByte" (fnSymbol 1 numberToByte)
        $ Map.insert "madlib__number__internal__negateByte" (fnSymbol 1 negateByte)

        -- Bits Byte
        $ Map.insert "madlib__number__internal__andBytes" (fnSymbol 2 andBytes)
        $ Map.insert "madlib__number__internal__orBytes" (fnSymbol 2 orBytes)
        $ Map.insert "madlib__number__internal__xorBytes" (fnSymbol 2 xorBytes)
        $ Map.insert "madlib__number__internal__complementBytes" (fnSymbol 1 complementBytes)
        $ Map.insert "madlib__number__internal__leftShiftBytes" (fnSymbol 2 leftShiftBytes)
        $ Map.insert "madlib__number__internal__rightShiftBytes" (fnSymbol 2 rightShiftBytes)

        -- Number Integer
        $ Map.insert "madlib__number__internal__numberToInteger" (fnSymbol 1 numberToInteger)
        $ Map.insert "madlib__number__internal__addIntegers" (fnSymbol 2 addIntegers)
        $ Map.insert "madlib__number__internal__substractIntegers" (fnSymbol 2 substractIntegers)
        $ Map.insert "madlib__number__internal__multiplyIntegers" (fnSymbol 2 multiplyIntegers)
        $ Map.insert "madlib__number__internal__divideIntegers" (fnSymbol 2 divideIntegers)
        $ Map.insert "madlib__number__internal__gtIntegers" (fnSymbol 2 gtIntegers)
        $ Map.insert "madlib__number__internal__ltIntegers" (fnSymbol 2 ltIntegers)
        $ Map.insert "madlib__number__internal__gteIntegers" (fnSymbol 2 gteIntegers)
        $ Map.insert "madlib__number__internal__lteIntegers" (fnSymbol 2 lteIntegers)
        $ Map.insert "madlib__number__internal__negateInteger" (fnSymbol 1 negateInteger)

        -- Bits Integer
        $ Map.insert "madlib__number__internal__andIntegers" (fnSymbol 2 andIntegers)
        $ Map.insert "madlib__number__internal__orIntegers" (fnSymbol 2 orIntegers)
        $ Map.insert "madlib__number__internal__xorIntegers" (fnSymbol 2 xorIntegers)
        $ Map.insert "madlib__number__internal__complementIntegers" (fnSymbol 1 complementIntegers)
        $ Map.insert "madlib__number__internal__leftShiftIntegers" (fnSymbol 2 leftShiftIntegers)
        $ Map.insert "madlib__number__internal__rightShiftIntegers" (fnSymbol 2 rightShiftIntegers)

        -- Eq
        $ Map.insert "madlib__number__internal__eqInteger" (fnSymbol 2 eqInteger)
        $ Map.insert "madlib__number__internal__eqByte" (fnSymbol 2 eqByte)
        $ Map.insert "madlib__char__internal__eq" (fnSymbol 2 eqChar)
        $ Map.insert "madlib__number__internal__eqFloat" (fnSymbol 2 eqFloat)
        $ Map.insert "madlib__string__internal__eq" (fnSymbol 2 eqString)
        $ Map.insert "madlib__boolean__internal__eq" (fnSymbol 2 eqBoolean)
        $ Map.insert "madlib__list__internal__eq" (fnSymbol 3 eqList)
        $ Map.insert "madlib__array__internal__eq" (fnSymbol 3 eqArray)
        $ Map.insert "madlib__bytearray__internal__eq" (fnSymbol 2 eqByteArray)
        $ Map.insert "madlib__dictionary__internal__eq" (fnSymbol 4 eqDictionary) initialSymbolTable

      numberType               = IT.TVar (IT.TV "a" IT.Star)
      numberPred               = IT.IsIn "Number" [numberType] Nothing
      numberQualType           = [numberPred] IT.:=> numberType
      numberOperationQualType  = [numberPred] IT.:=> (numberType `IT.fn` numberType `IT.fn` numberType)
      numberComparisonQualType = [numberPred] IT.:=> (numberType `IT.fn` numberType `IT.fn` IT.tBool)
      coerceNumberQualType     = [numberPred] IT.:=> (numberType `IT.fn` numberType)

  -- TODO: Add "unary-minus" method for number instances
  let integerNumberInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Number" [] ("Integer_" <> preludeHash)
              (Map.fromList
                [ ( "+"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "+" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__addIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "-" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__substractIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "*" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__multiplyIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "/"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "*" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__divideIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment ">" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__gtIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "<" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__ltIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( ">="
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment ">=" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__gteIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<="
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "<=" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__lteIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "__coerceNumber__" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__numberToInteger" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "unary-minus"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "unary-minus" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__negateInteger" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                ]
              )
          )

  let integerBitsInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Bits" [] ("Integer_" <> preludeHash)
              (Map.fromList
                [ ( "&"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "&" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__andIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "|"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "|" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__orIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "^"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "^" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__xorIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "~"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "~" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__complementIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "<<"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "<<" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__leftShiftIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">>"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment ">>" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__rightShiftIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">>>"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment ">>>" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__rightShiftIntegers" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                ]
              )
          )

  let byteNumberInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Number" [] ("Byte_" <> preludeHash)
              (Map.fromList
                [ ( "+"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__addBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__substractBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__multiplyBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "/"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__divideBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__gtBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__ltBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( ">="
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__gteBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<="
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__lteBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "__coerceNumber__" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__numberToByte" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "unary-minus"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "unary-minus" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__negateByte" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                ]
              )
          )


  let byteBitsInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Bits" [] ("Byte_" <> preludeHash)
              (Map.fromList
                [ ( "&"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "&" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__andBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "|"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "|" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__orBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "^"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "^" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__xorBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "~"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "~" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__complementBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "<<"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "<<" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__leftShiftBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">>"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment ">>" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__rightShiftBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">>>"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment ">>>" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__rightShiftBytes" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                ]
              )
          )

  let floatNumberInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Number" [] ("Float_" <> preludeHash)
              (Map.fromList
                [ ( "+"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__addFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "-"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__substractFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "*"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__multiplyFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "/"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberOperationQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed numberOperationQualType emptyArea [] (Core.Var "madlib__number__internal__divideFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Float" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( ">"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__gtFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<"
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__ltFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( ">="
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__gteFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "<="
                  , ( Core.Typed numberComparisonQualType emptyArea [] (Core.Assignment "" (
                        Core.Typed numberComparisonQualType emptyArea [] (Core.Definition (Core.Typed numberQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed numberComparisonQualType emptyArea [] (Core.Var "madlib__number__internal__lteFloats" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed numberQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                , ( "__coerceNumber__"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "__coerceNumber__" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__numberToFloat" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                , ( "unary-minus"
                  , ( Core.Typed coerceNumberQualType emptyArea [] (Core.Assignment "unary-minus" (
                        Core.Typed coerceNumberQualType emptyArea [] (Core.Definition [Core.Typed numberQualType emptyArea [] "a"] [
                          Core.Typed numberQualType emptyArea [] (Core.Call (Core.Typed coerceNumberQualType emptyArea [] (Core.Var "madlib__number__internal__negateFloat" False)) [
                            Core.Typed numberQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Number" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0)
                    )
                  )
                ]
              )
          )

  let varType  = IT.TVar (IT.TV "a" IT.Star)
      dictType = IT.TVar (IT.TV "dict" IT.Star)

  let inspectPred               = IT.IsIn "Inspect" [varType] Nothing
      inspectVarQualType        = [inspectPred] IT.:=> varType
      byteArrayInspectQualType  = [] IT.:=> (IT.tByteArray `IT.fn` IT.tStr)
      overloadedInspectType     = dictType `IT.fn` varType `IT.fn` IT.tStr
      overloadedInspectQualType = [inspectPred] IT.:=> overloadedInspectType

      byteArrayInspectInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Inspect" [] ("ByteArray_" <> preludeHash)
              (Map.fromList
                [ ( "inspect"
                  , ( Core.Typed byteArrayInspectQualType emptyArea [] (Core.Assignment "inspect" (
                        Core.Typed byteArrayInspectQualType emptyArea [] (Core.Definition [Core.Typed inspectVarQualType emptyArea [] "a"] [
                          Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Core.Call (Core.Typed byteArrayInspectQualType emptyArea [] (Core.Var "madlib__bytearray__internal__inspect" False)) [
                              Core.Typed inspectVarQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [] byteArrayInspectQualType
                    )
                  )
                ]
              )
          )

      arrayInspectInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Inspect" [IT.IsIn "Inspect" [IT.TVar (IT.TV "a" IT.Star)] Nothing] ("Array_" <> preludeHash)
              (Map.fromList
                [ ( "inspect"
                  , ( Core.Typed overloadedInspectQualType emptyArea [] (Core.Assignment "inspect" (
                        Core.Typed overloadedInspectQualType emptyArea [] (Core.Definition [Core.Typed ([] IT.:=> dictType) emptyArea [] "inspectDict", Core.Typed inspectVarQualType emptyArea [] "a"] [
                          Core.Typed ([] IT.:=> IT.tStr) emptyArea [] (Core.Call (Core.Typed overloadedInspectQualType emptyArea [] (Core.Var "madlib__array__internal__inspect" False)) [
                              Core.Typed ([] IT.:=> dictType) emptyArea [] (Core.Var "inspectDict" False),
                              Core.Typed inspectVarQualType emptyArea [] (Core.Var "a" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] overloadedInspectQualType
                    )
                  )
                ]
              )
          )

      tupleInspectInstances = buildTupleNInspectInstance <$> [2..10]

  let eqPred              = IT.IsIn "Eq" [varType] Nothing
      eqVarQualType       = [eqPred] IT.:=> varType
      eqOperationQualType = [eqPred] IT.:=> (varType `IT.fn` varType `IT.fn` IT.tBool)

      fnEqInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Eq" [] "a_arr_b"
              (Map.fromList
                [ ( "=="
                  , ( Core.Typed eqOperationQualType emptyArea [] (Core.Assignment "==" (
                        Core.Typed eqOperationQualType emptyArea [] (Core.Definition (Core.Typed eqVarQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Literal $ LBool "true")
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )


      overloadedEqType     = dictType `IT.fn` varType `IT.fn` varType `IT.fn` IT.tBool
      overloadedEqQualType = [eqPred] IT.:=> overloadedEqType

      arrayEqInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Eq" [IT.IsIn "Eq" [IT.TVar (IT.TV "a" IT.Star)] Nothing] ("Array_" <> preludeHash)
              (Map.fromList
                [ ( "=="
                  , ( Core.Typed overloadedEqQualType emptyArea [] (Core.Assignment "==" (
                        Core.Typed overloadedEqQualType emptyArea [] (Core.Definition (Core.Typed ([] IT.:=> dictType) emptyArea [] "eqDict" : (Core.Typed eqVarQualType emptyArea [] <$> ["a", "b"])) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed overloadedEqQualType emptyArea [] (Core.Var "madlib__array__internal__eq" False)) [
                            Core.Typed ([] IT.:=> dictType) emptyArea [] (Core.Var "eqDict" False),
                            Core.Typed eqVarQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed eqVarQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      byteArrayEqInstance =
        Core.Untyped emptyArea []
          ( Core.Instance "Eq" [] ("ByteArray_" <> preludeHash)
              (Map.fromList
                [ ( "=="
                  , ( Core.Typed eqOperationQualType emptyArea [] (Core.Assignment "==" (
                        Core.Typed eqOperationQualType emptyArea [] (Core.Definition (Core.Typed eqVarQualType emptyArea [] <$> ["a", "b"]) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed eqOperationQualType emptyArea [] (Core.Var "madlib__bytearray__internal__eq" False)) [
                            Core.Typed eqVarQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed eqVarQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
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
        Core.Untyped emptyArea []
          ( Core.Instance "Eq" dictionaryEqPreds ("Dictionary_" <> preludeHash)
              (Map.fromList
                [ ( "=="
                  -- Note, the dicts need to be inverted as this happens during dict resolution after type checking
                  , ( Core.Typed dictionaryEqQualType emptyArea [] (Core.Assignment "==" (
                        Core.Typed dictionaryEqQualType emptyArea [] (Core.Definition (Core.Typed ([] IT.:=> dictType) emptyArea [] "eqDictB" : Core.Typed ([] IT.:=> dictType) emptyArea [] "eqDictA" : (Core.Typed eqVarQualType emptyArea [] <$> ["a", "b"])) [
                          Core.Typed ([] IT.:=> IT.tBool) emptyArea [] (Core.Call (Core.Typed dictionaryEqQualType emptyArea [] (Core.Var "madlib__dictionary__internal__eq" False)) [
                            Core.Typed ([] IT.:=> dictType) emptyArea [] (Core.Var "eqDictA" False),
                            Core.Typed ([] IT.:=> dictType) emptyArea [] (Core.Var "eqDictB" False),
                            Core.Typed eqVarQualType emptyArea [] (Core.Var "a" False),
                            Core.Typed eqVarQualType emptyArea [] (Core.Var "b" False)
                          ])
                        ])
                      ))
                    , IT.Forall [IT.Star] $ [IT.IsIn "Eq" [IT.TGen 0] Nothing] IT.:=> (IT.TGen 0 `IT.fn` IT.TGen 0 `IT.fn` IT.tBool)
                    )
                  )
                ]
              )
          )

      tupleEqInstances = buildTupleNEqInstance <$> [2..10]

  generateInstances
    env
    symbolTableWithCBindings
    (
      -- Number
      [ integerNumberInstance
      , byteNumberInstance
      , floatNumberInstance

      -- Bits
      , byteBitsInstance
      , integerBitsInstance

        -- Eq
      , arrayEqInstance
      , byteArrayEqInstance
      , dictionaryEqInstance
      , fnEqInstance

        -- Inspect
      , byteArrayInspectInstance
      , arrayInspectInstance
      ] ++ tupleEqInstances ++ tupleInspectInstances
    )
  return ()


generateLLVMModule :: (Writer.MonadWriter SymbolTable m, Writer.MonadFix m, MonadModuleBuilder m) => Env -> Bool -> [String] -> SymbolTable -> AST -> m ()
generateLLVMModule _ _ _ _ Core.AST{ Core.apath = Nothing } = undefined
generateLLVMModule env isMain currentModulePaths initialSymbolTable ast = do
  symbolTableWithConstructors <- generateConstructors env initialSymbolTable (atypedecls ast)
  let symbolTableWithTopLevel  = List.foldr (flip (addTopLevelFnToSymbolTable env)) symbolTableWithConstructors (aexps ast)
      symbolTableWithMethods   = List.foldr (flip addInstanceToSymbolTable) symbolTableWithTopLevel (ainstances ast)
      symbolTableWithDefaults  = Map.insert "__dict_ctor__" (fnSymbol 2 dictCtor) symbolTableWithMethods

  let moduleHash = hashModulePath ast
  let moduleFunctionName =
        if isMain then
          "main"
        else
          "__" <> moduleHash <> "__moduleFunction"

  mapM_ (generateImport initialSymbolTable) $ aimports ast
  generateExternsForImportedInstances initialSymbolTable

  symbolTable  <- generateInstances env symbolTableWithDefaults (ainstances ast)
  symbolTable' <- generateTopLevelFunctions env symbolTable (topLevelFunctions $ aexps ast)

  externVarArgs (AST.mkName "__applyPAP__")                          [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)
  externVarArgs (AST.mkName "madlib__record__internal__buildRecord") [Type.i32, boxType] recordType
  extern (AST.mkName "madlib__process__internal__typedHoleReached")  [] Type.void

  extern (AST.mkName "__dict_ctor__")                                [boxType, boxType] boxType
  extern (AST.mkName "madlib__record__internal__selectField")        [stringType, recordType] boxType
  extern (AST.mkName "madlib__string__internal__areStringsEqual")    [stringType, stringType] Type.i1
  extern (AST.mkName "madlib__string__internal__areStringsNotEqual") [stringType, stringType] Type.i1
  extern (AST.mkName "madlib__string__internal__concat")             [stringType, stringType] stringType

  extern (AST.mkName "madlib__list__internal__hasMinLength")         [Type.i64, listType] Type.i1
  extern (AST.mkName "madlib__list__internal__hasLength")            [Type.i64, listType] Type.i1
  extern (AST.mkName "madlib__list__singleton")                      [Type.ptr Type.i8] listType
  extern (AST.mkName "madlib__list__internal__push")                 [Type.ptr Type.i8, listType] listType
  extern (AST.mkName "madlib__list__concat")                         [listType, listType] listType

  extern (AST.mkName "GC_malloc")              [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc_atomic")       [Type.i64] (Type.ptr Type.i8)

  extern (AST.mkName "!=")                     [boxType, boxType, boxType] boxType

  Monad.when isMain $ do
    extern (AST.mkName "madlib__process__internal__initExtra")    [] Type.void
    extern (AST.mkName "__main__init__")                          [Type.i32, Type.ptr (Type.ptr Type.i8)] Type.void
    extern (AST.mkName "__initEventLoop__")                       [] Type.void
    extern (AST.mkName "__startEventLoop__")                      [] Type.void
    extern (AST.mkName "madlib__process__internal__getArgs")      [] listType
    generateModuleFunctionExternals currentModulePaths

  moduleFunction <-
    if isMain then do
      let getArgs     = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType listType [] False) "madlib__process__internal__getArgs")
      let (Symbol _ mainFunction) = Maybe.fromMaybe undefined $ Map.lookup ("__" <> moduleHash <> "__main") symbolTable'
      -- this function starts the runtime with a fresh stack etc
      function (AST.mkName "__main__start__") [] Type.void $ \_ -> do
        block `named` "entry"
        call initExtra []
        call initEventLoop []

        callModuleFunctions currentModulePaths

        generateExps env symbolTable' (expsForMain $ aexps ast)
        -- call user main that should be named like main_moduleHash
        mainArgs <- call getArgs []
        mainArgs' <- safeBitcast mainArgs boxType
        call mainFunction [(mainArgs', [])]
        call startEventLoop []
        retVoid

      let argc = (Type.i32, ParameterName $ stringToShortByteString "argc")
          argv = (Type.ptr (Type.ptr Type.i8), ParameterName $ stringToShortByteString "argv")
      function (AST.mkName moduleFunctionName) [argc, argv] Type.i32 $ \[argc, argv] -> do
        block `named` "entry"
        call mainInit [(argc, []), (argv, [])]
        ret $ i32ConstOp 0
    else do
      function (AST.mkName moduleFunctionName) [] Type.void $ \_ -> do
        block `named` "entry"
        generateExps env symbolTable' (expsForMain $ aexps ast)
        retVoid

  Writer.tell $ Map.singleton moduleFunctionName (fnSymbol 0 moduleFunction)
  Writer.tell symbolTableWithDefaults

defaultDictionaryIndices =
  Map.fromList
    [ ("Number", Map.fromList [("*", (0, 2)), ("+", (1, 2)), ("-", (2, 2)), ("/", (3, 2)), ("<", (4, 2)), ("<=", (5, 2)), (">", (6, 2)), (">=", (7, 2)), ("__coerceNumber__", (8, 1)), ("unary-minus", (9, 1))])
    , ("Bits", Map.fromList [("&", (0, 2)), ("<<", (1, 2)), (">>", (2, 2)), (">>>", (3, 2)), ("^", (4, 2)), ("|", (5, 2)), ("~", (6, 1))])
    , ("Eq", Map.fromList [("==", (0, 2))])
    , ("Inspect", Map.fromList [("inspect", (0, 1))])
    ]


generateModule :: (Rock.MonadFetch Query.Query m, Writer.MonadFix m) => Options -> AST -> m (AST.Module, SymbolTable, Env)
generateModule options ast@AST{ apath = Just modulePath } = do
  let imports                          = aimports ast
      importPaths                      = getImportAbsolutePath <$> imports

  symbolTablesWithEnvs <- mapM (Rock.fetch . Query.BuiltObjectFile) importPaths
  defaultSymbolTableWithEnv <- Rock.fetch Query.BuiltInBuiltObjectFile

  let dropThird (table, env, _) = (table, env)

  let allTablesAndEnvs = dropThird defaultSymbolTableWithEnv : (dropThird <$> symbolTablesWithEnvs)

  let symbolTable                  = mconcat $ fst <$> allTablesAndEnvs
  let dictionaryIndicesFromImports = mconcat $ dictionaryIndices . snd <$> allTablesAndEnvs
  let computedDictionaryIndices    = buildDictionaryIndices $ ainterfaces ast

  let isMain = optEntrypoint options == modulePath
  let envForAST =
        Env
          { dictionaryIndices = computedDictionaryIndices <> dictionaryIndicesFromImports <> defaultDictionaryIndices
          , isLast = False
          , isTopLevel = False
          , recursionData = Nothing
          , envASTPath = modulePath
          }
  let moduleName =
        if isMain then
          "main"
        else
          hashModulePath ast

  importModulePaths <-
    if isMain then do
      allPaths <- Rock.fetch $ Query.ModulePathsToBuild modulePath
      return $ List.filter (/= modulePath) allPaths
    else
      return []

  (mod, table) <- Writer.runWriterT $ buildModuleT (stringToShortByteString moduleName) (generateLLVMModule envForAST isMain importModulePaths symbolTable ast)
  return (mod, table, envForAST)

generateModule _ _ =
  undefined


compileDefaultModule :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m) => Options -> m (SymbolTable, Env, ByteString)
compileDefaultModule _ = do
  let (defaultInstancesModule, symbolTable') = Writer.runWriter $ buildModuleT (stringToShortByteString "number") (buildDefaultInstancesModule Env { dictionaryIndices = defaultDictionaryIndices, isLast = False, isTopLevel = False, recursionData = Nothing, envASTPath = "" } [] mempty)

  objectContent <- liftIO $ buildObjectFile defaultInstancesModule

  return
    ( symbolTable'
    , Env
      { dictionaryIndices = defaultDictionaryIndices
      , isLast = False
      , isTopLevel = False
      , recursionData = Nothing
      , envASTPath = ""
      }
    , objectContent
    )


compileModule :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m) => Options -> Core.AST -> m (SymbolTable, Env, ByteString.ByteString)
compileModule _ Core.AST { Core.apath = Nothing } = return (mempty, initialEnv, Char8.pack "")
compileModule options ast@Core.AST { Core.apath = Just modulePath } = do
  (astModule, table, env) <- generateModule options ast

  -- let pretty = ppllvm astModule
  -- liftIO $ Prelude.putStrLn (LazyText.unpack pretty)

  objectContent <- liftIO $ buildObjectFile astModule

  pathsToBuild <- Rock.fetch $ Query.ModulePathsToBuild (optEntrypoint options)
  let rest = List.dropWhile (/= modulePath) pathsToBuild
  let total = List.length pathsToBuild
  let curr = total - List.length rest + 1
  let currStr = if curr < 10 then " " <> show curr else show curr
  liftIO $ Prelude.putStrLn $ "[" <> currStr <> " of "<> show total<>"] Compiled '" <> modulePath <> "'"

  return (table, env, objectContent)


buildObjectFile :: AST.Module -> IO ByteString.ByteString
buildObjectFile astModule = do
  withHostTargetMachineDefault $ \target -> do
      withContext $ \ctx -> do
        withModuleFromAST ctx astModule $ \mod' -> do
          mod'' <-
            withPassManager
            defaultCuratedPassSetSpec
              { optLevel                = Just 3
              , useInlinerWithThreshold = Just 200
              }
            $ \pm -> do
              runPassManager pm mod'
              return mod'
          moduleObject target mod''


makeExecutablePath :: FilePath -> FilePath
makeExecutablePath output = case output of
  "./build/" ->
    case DistributionSystem.buildOS of
      DistributionSystem.Windows ->
        "a.exe"

      _ ->
        "a.out"

  or ->
    or


buildTarget :: (Rock.MonadFetch Query.Query m, MonadIO m, Writer.MonadFix m) => Options -> [String] -> FilePath -> m ()
buildTarget options staticLibs entrypoint = do
  let outputFolder = takeDirectory (optOutputPath options)
  modulePaths <- Rock.fetch $ Query.ModulePathsToBuild entrypoint

  Rock.fetch $ Query.BuiltObjectFile entrypoint

  let defaultInstancesModulePath =
        if takeExtension entrypoint == "" then
          joinPath [entrypoint, "__default__instances__.mad"]
        else
          joinPath [takeDirectory entrypoint, "__default__instances__.mad"]

  let objectFilePaths = Path.computeLLVMTargetPath outputFolder (optRootPath options) <$> (defaultInstancesModulePath : modulePaths)

  compilerPath <- liftIO getExecutablePath
  let executablePath = makeExecutablePath (optOutputPath options)

  let objectFilePathsForCli = List.unwords objectFilePaths
      runtimeLibPathOpt     = "-L\"" <> joinPath [takeDirectory compilerPath, "runtime", "lib"] <> "\""
      runtimeBuildPathOpt   = "-L\"" <> joinPath [takeDirectory compilerPath, "runtime", "build"] <> "\""

  liftIO $ IOUtils.putStrLnAndFlush "Linking.."

  case DistributionSystem.buildOS of
    DistributionSystem.OSX ->
      liftIO $ callCommand $
        "clang++ -dead_strip -foptimize-sibling-calls -stdlib=libc++ -O2 "
        <> objectFilePathsForCli
        <> " " <> runtimeLibPathOpt
        <> " " <> runtimeBuildPathOpt
        <> " " <> List.unwords staticLibs
        <> " -lruntime -lgc -lgccpp -luv -lpcre2-8"
        <> " -lcurl -framework CoreFoundation -framework SystemConfiguration -framework CoreFoundation -lssl -lcrypto -lz"
        <>" -o " <> executablePath

    DistributionSystem.Windows ->
      liftIO $ callCommand $
        "g++ -static "
        <> objectFilePathsForCli
        <> " " <> runtimeLibPathOpt
        <> " " <> runtimeBuildPathOpt
        <> " " <> List.unwords staticLibs
        <> " -lruntime -lmman -lgc -lgccpp -luv -lpcre2-8 -pthread -ldl -lws2_32 -liphlpapi -lUserEnv -lcurl -lz -lssl -lcrypto -lgdi32 -lcrypt32 -lwldap32 -lws2_32  -o " <> executablePath

    _ ->
      liftIO $ callCommand $
        "g++ -static "
        <> objectFilePathsForCli
        <> " " <> runtimeLibPathOpt
        <> " " <> runtimeBuildPathOpt
        <> " " <> List.unwords staticLibs
        <> " -lruntime -lgc -lgccpp -luv -lpcre2-8 -lcurl -lssl -lcrypto -lz -pthread -ldl -Wl,--allow-multiple-definition -o " <> executablePath
