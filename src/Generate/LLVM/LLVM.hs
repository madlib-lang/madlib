{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module Generate.LLVM.LLVM where


import Data.Text.Lazy.IO as T
import           Data.ByteString.Short as ShortByteString
import qualified Data.Map              as Map
import qualified Data.List             as List
import qualified Data.Maybe            as Maybe
import qualified Control.Monad         as Monad
import qualified Control.Monad.Fix     as MonadFix
import           Data.ByteString as ByteString
import           Data.ByteString.Char8 as Char8
import           System.Process

import           LLVM.Pretty
import           LLVM.Target
import           LLVM.Linking
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

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction as Instruction
import LLVM.Context (withContext)
import Generate.LLVM.Optimized as Opt
import Text.Show.Pretty
import Debug.Trace
import qualified Data.String.Utils as List
import Foreign.C
import qualified Infer.Type as InferredType
import Infer.Type (isFunctionType)
import LLVM.PassManager
import LLVM.CodeGenOpt (Level)
import LLVM.AST.Constant (Constant(Null))
import qualified LLVM.Prelude as FloatingPointPredicate
import LLVM.Transforms (Pass(TailCallElimination))


-- TODO: Move to util module
sizeof :: Type.Type -> Constant.Constant
sizeof t = Constant.PtrToInt szPtr (Type.IntegerType 64)
  where
     ptrType = Type.PointerType t (AddrSpace 0)
     nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
     szPtr   = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]


data SymbolType
  = VariableSymbol
  | FunctionSymbol Int
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

fnSymbol :: Int -> Operand -> Symbol
fnSymbol arity =
  Symbol (FunctionSymbol arity)

topLevelSymbol :: Operand -> Symbol
topLevelSymbol =
  Symbol TopLevelAssignment

constructorSymbol :: Operand -> Int -> Int -> Symbol
constructorSymbol ctor id arity =
  Symbol (ConstructorSymbol id arity) ctor






stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack


true :: Operand
true = Operand.ConstantOperand (Constant.Int 1 1)

gcMalloc :: Operand
gcMalloc =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "GC_malloc"))

applyPAP :: Operand
applyPAP =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.ptr Type.i8, Type.i32] True) (AST.mkName "__applyPAP__"))

madlistLength :: Operand
madlistLength =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [listType] False) (AST.mkName "MadList_length"))

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
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i1 [Type.ptr Type.i8, Type.ptr Type.i8] False) (AST.mkName "__streq__"))

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
  InferredType.TCon (InferredType.TC "Number" InferredType.Star) "prelude" ->
    Type.double

  InferredType.TCon (InferredType.TC "String" InferredType.Star) "prelude" ->
    Type.ptr Type.i8

  InferredType.TCon (InferredType.TC "Boolean" InferredType.Star) "prelude" ->
    Type.i1

  InferredType.TCon (InferredType.TC "()" InferredType.Star) "prelude" ->
    Type.ptr Type.i8

  InferredType.TApp (InferredType.TCon (InferredType.TC "List" (InferredType.Kfun InferredType.Star InferredType.Star)) "prelude") _ ->
    Type.ptr (Type.StructureType False [boxType, boxType])

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") left) right ->
    let tLeft  = buildLLVMType left
        tRight = buildLLVMType right
    in  Type.ptr $ Type.FunctionType boxType [boxType] False
    -- in  Type.ptr $ Type.FunctionType tRight [tLeft] False
    -- in  boxType

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(,)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") a) b ->
    let tA = buildLLVMType a
        tB = buildLLVMType b
    in  Type.ptr $ Type.StructureType False [boxType, boxType]

  _ ->
    Type.ptr Type.i8


boxType :: Type.Type
boxType =
  Type.ptr Type.i8

listType :: Type.Type
listType =
  Type.ptr $ Type.StructureType False [boxType, boxType]


unbox :: (MonadIRBuilder m, MonadModuleBuilder m) => InferredType.Type -> Operand -> m Operand
unbox t what = case t of
  InferredType.TCon (InferredType.TC "Number" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.double
    load ptr 8

  InferredType.TCon (InferredType.TC "Boolean" _) _ -> do
    ptr <- bitcast what $ Type.ptr Type.i1
    load ptr 8

  InferredType.TApp (InferredType.TCon (InferredType.TC "List" _) _) vt ->
    bitcast what listType

  -- TODO: check that we need this
  -- This should be called for parameters that are closures
  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" _) _) p) b ->
    bitcast what $ Type.ptr $ Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
    -- return what

  _ ->
    bitcast what (buildLLVMType t)

box :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
box what = case typeOf what of
  -- Number
  Type.FloatingPointType _ -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof Type.double, [])]
    ptr' <- bitcast ptr (Type.ptr Type.double)
    store ptr' 8 what
    bitcast ptr' boxType

  -- Boolean
  Type.IntegerType 1 -> do
    ptr <- call gcMalloc [(Operand.ConstantOperand $ sizeof Type.i1, [])]
    ptr' <- bitcast ptr (Type.ptr Type.i1)
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
  let s' = List.init . List.tail $ s
  -- we need to add 0 to terminate a C string
  let charCodes = (fromEnum <$> s') ++ [0]
  -- 92, 110 == \n
  let charCodes' = List.replace [92, 110] [10] charCodes
  let charCodes'' = toInteger <$> charCodes'
  addr <- call gcMalloc [(i64ConstOp (fromIntegral $ List.length charCodes''), [])]
  let charCodesWithIds = List.zip charCodes'' [0..]

  Monad.foldM_ (storeChar addr) () charCodesWithIds
  return addr
  where
    storeChar :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> () -> (Integer, Integer) -> m ()
    storeChar basePtr _ (charCode, index) = do
      ptr <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 index)]
      store ptr 8 (Operand.ConstantOperand (Constant.Int 8 charCode))
      return ()


collectDictArgs :: (MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m ([Operand.Operand], Exp)
collectDictArgs symbolTable exp = case exp of
  Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds True False, typingStr) exp') -> do
    let dictName = "$" <> interfaceName <> "$" <> typingStr
    (nextArgs, nextExp) <- collectDictArgs symbolTable exp'
    case Map.lookup dictName symbolTable of
      Just (Symbol (DictionarySymbol _) dict) ->
        return (dict : nextArgs, nextExp)

      _ ->
        undefined

  Optimized t _ (Placeholder (MethodRef _ _ False , _) _) ->
    return ([], exp)

  _ ->
    error $ "oups\n\n" <> ppShow exp


generateApplicationForKnownFunction :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> InferredType.Type -> Int -> Operand -> [Exp] -> m (SymbolTable, Operand)
generateApplicationForKnownFunction symbolTable returnType arity fnOperand args
  | List.length args == arity = do
      -- We have a known call!
      args'   <- mapM (generateExp symbolTable) args
      args''  <- mapM (box . snd) args'
      let args''' = (, []) <$> args''

      ret <- call fnOperand args'''
      unboxed <- unbox returnType ret

      return (symbolTable, unboxed)
  | List.length args > arity = do
      -- We have extra args so we do the known call and the applyPAP the resulting partial application
      let (args', remainingArgs) = List.splitAt arity args
      args''   <- mapM (generateExp symbolTable) args'
      args'''  <- mapM (box . snd) args''
      let args'''' = (, []) <$> args'''

      pap <- call fnOperand args''''

      let argc = i32ConstOp (fromIntegral $ List.length remainingArgs)
      remainingArgs' <- mapM (generateExp symbolTable) remainingArgs
      remainingArgs''  <- mapM (box . snd) remainingArgs'
      let remainingArgs''' = (, []) <$> remainingArgs''

      ret <- call applyPAP $ [(pap, []), (argc, [])] ++ remainingArgs'''
      unboxed <- unbox returnType ret

      return (symbolTable, ret)
  | otherwise = do
      -- We don't have enough args, so we create a new PAP
      let papType                 = Type.StructureType False [boxType, Type.i32, Type.i32, boxType]
      let arity'                  = i32ConstOp (fromIntegral arity)
      let argCount                = List.length args
      let amountOfArgsToBeApplied = i32ConstOp (fromIntegral (arity - argCount))
      let envType                 = Type.StructureType False (List.replicate argCount boxType)

      boxedFn  <- box fnOperand

      args'  <- mapM (generateExp symbolTable) args
      let args'' = snd <$> args'
      boxedArgs <- mapM box args''

      envPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof envType, [])]
      envPtr' <- bitcast envPtr (Type.ptr envType)
      Monad.foldM_ (storeItem envPtr') () $ List.zip boxedArgs [0..]

      papPtr  <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
      papPtr' <- bitcast papPtr (Type.ptr papType)
      Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (amountOfArgsToBeApplied, 2), (envPtr, 3)]

      return (symbolTable, papPtr')


generateExp :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m (SymbolTable, Operand)
generateExp symbolTable exp = case exp of
  Optimized _ _ (Var n) ->
    case Map.lookup n symbolTable of
      -- TODO: need to extract that and do the same for constructors
      Just (Symbol (FunctionSymbol arity) fn) -> do
        -- build PAP
        let papType = Type.StructureType False [boxType, Type.i32, Type.i32]
        let arity'  = i32ConstOp (fromIntegral arity)

        boxedFn  <- box fn

        papPtr   <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
        papPtr'  <- bitcast papPtr (Type.ptr papType)
        Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

        return (symbolTable, papPtr')

      Just (Symbol TopLevelAssignment ptr) -> do
        ptr' <- load ptr 8
        return (symbolTable, ptr')

      Just (Symbol _ var) ->
        return (symbolTable, var)

      Nothing ->
        error $ "Var not found " <> n <> "\n\n" <> ppShow symbolTable

    -- (Placeholder
    --  ( ClassRef "Functor" [] False True , "b183" )
  Optimized _ _ (Placeholder (ClassRef interface _ False True, typingStr) exp) -> do
    symbolTable' <-
      generateFunction
        symbolTable
        (InferredType.tVar "a" `InferredType.fn` InferredType.tVar "b")
        "whateva"
        ["$" <> interface <> "$" <> typingStr]
        [exp]
    return (symbolTable', Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType boxType [boxType] False) (AST.mkName "whateva")))
    where
      gatherAllPlaceholders :: Exp -> ([(PlaceholderRef, String)], Exp)
      gatherAllPlaceholders = undefined

-- (Placeholder
--   ( MethodRef
--       "Show" "show" True
--   , "j9"
--   )
  Optimized _ _ (Placeholder (MethodRef interface methodName True, typingStr) _) -> do
    let dictName = "$" <> interface <> "$" <> typingStr
    case Map.lookup dictName symbolTable of
      -- TODO: this should be a DictionarySymbol with indices
      Just (Symbol _ dict) -> do
        let index = 0
        dict' <- bitcast dict (Type.ptr $ Type.StructureType False [boxType])
        method  <- gep (trace ("DICT': "<>ppShow dict') dict') [i32ConstOp 0, i32ConstOp (fromIntegral index)]
        method' <- load method 8

        let papType = Type.StructureType False [boxType, Type.i32, Type.i32]
        let arity'  = i32ConstOp 2

        boxedFn  <- box method'

        papPtr   <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
        papPtr'  <- bitcast papPtr (Type.ptr papType)
        Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

        return (symbolTable, papPtr')

        -- return (symbolTable, method')

      _ ->
        undefined

  Optimized ty _ (Assignment name e isTopLevel) -> do
    (symbolTable', exp') <- generateExp symbolTable e

    if isTopLevel then do
      let t = typeOf exp'
      g <- global (AST.mkName name) t $ Constant.Undef t
      store g 8 exp'
      return (Map.insert name (varSymbol exp') symbolTable, exp')
    else
      return (Map.insert name (varSymbol exp') symbolTable, exp')

  Optimized _ _ (App (Optimized _ _ (Var "-")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fsub leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "+")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fadd leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "*")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fmul leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "/")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fdiv leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "==")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fcmp FloatingPointPredicate.OEQ leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "!=")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fcmp FloatingPointPredicate.ONE leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var ">")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fcmp FloatingPointPredicate.OGT leftOperand' rightOperand'
    return (symbolTable, result)

  Optimized _ _ (App (Optimized _ _ (Var "<")) [leftOperand, rightOperand]) -> do
    (_, leftOperand')  <- generateExp symbolTable leftOperand
    (_, rightOperand') <- generateExp symbolTable rightOperand
    result <- fcmp FloatingPointPredicate.OLT leftOperand' rightOperand'
    return (symbolTable, result)

    -- (Placeholder ( ClassRef "Functor" [] True False , "List" )
  Optimized t _ (Placeholder (ClassRef interface _ True False, typingStr) (Opt.Optimized _ _ (Opt.Var fnName))) -> do
    let dictName    = "$" <> interface <> "$" <> typingStr
    case Map.lookup dictName symbolTable of
      Just (Symbol _ dict) ->
        case Map.lookup fnName symbolTable of
          Just (Symbol _ fn) -> do
            dict' <- box dict
            ret <- call fn [(dict', [])]

            -- let papType = Type.StructureType False [boxType, Type.i32, Type.i32]
            -- let arity'  = i32ConstOp (fromIntegral 1)

            -- boxedFn  <- box ret

            -- papPtr   <- call gcMalloc [(Operand.ConstantOperand $ sizeof papType, [])]
            -- papPtr'  <- bitcast papPtr (Type.ptr papType)
            -- Monad.foldM_ (storeItem papPtr') () [(boxedFn, 0), (arity', 1), (arity', 2)]

            return (symbolTable, ret)

            -- return (symbolTable, ret)

  Optimized t _ (App fn args) -> case fn of
    -- (Placeholder ( ClassRef "Functor" [] True False , "List" )
    -- Optimized t _ (Placeholder (ClassRef interface _ True False, typingStr) (Opt.Optimized _ _ (Opt.Var fnName))) -> do
    --   let dictName    = "$" <> interface <> "$" <> typingStr
    --   case Map.lookup dictName symbolTable of
    --     Just (Symbol _ dict) ->
    --       case Map.lookup fnName symbolTable of
    --         Just (Symbol _ fn) -> do
    --           dict' <- box dict
    --           ret <- call fn [(dict', [])]

    --           return (symbolTable, ret)

    -- Calling a known method
    Optimized t _ (Placeholder (MethodRef interface methodName False, typingStr) _) -> do
      let dictName    = "$" <> interface <> "$" <> typingStr
      let methodName' = dictName <> "$" <> methodName
      case Map.lookup methodName' symbolTable of
        Just (Symbol (FunctionSymbol arity) fnOperand) ->
          generateApplicationForKnownFunction symbolTable t arity fnOperand args

        _ ->
          error $ "method not found" <> methodName

    Optimized _ _ (Opt.Var functionName) -> case Map.lookup functionName symbolTable of
      Just (Symbol (ConstructorSymbol _ arity) fnOperand) ->
        generateApplicationForKnownFunction symbolTable t arity fnOperand args

      Just (Symbol (FunctionSymbol arity) fnOperand) ->
        generateApplicationForKnownFunction symbolTable t arity fnOperand args

      Just (Symbol _ pap) -> mdo
        -- We apply a partial application
        let argsApplied = List.length args

        pap' <- bitcast pap boxType

        let argc = i32ConstOp (fromIntegral argsApplied)

        args'  <- mapM (generateExp symbolTable) args
        let args'' = snd <$> args'
        boxedArgs <- mapM box args''

        ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
        unboxed <- unbox t ret
        return (symbolTable, unboxed)

      _ ->
        undefined

    _ -> do
      (_, pap) <- generateExp symbolTable fn
      pap' <- bitcast pap boxType

      let argc = i32ConstOp (fromIntegral $ List.length args)

      args'  <- mapM (generateExp symbolTable) args
      let args'' = snd <$> args'
      boxedArgs <- mapM box args''

      ret <- call applyPAP $ [(pap', []), (argc, [])] ++ ((,[]) <$> boxedArgs)
      unboxed <- unbox t ret
      return (symbolTable, unboxed)

  Optimized _ _ (LNum n) -> do
    return (symbolTable, C.double (read n))

  Optimized _ _ (LBool b) -> do
    let value =
          if b == "true" then
            1
          else
            0
    return (symbolTable, Operand.ConstantOperand $ Constant.Int 1 value)

  Optimized _ _ (LStr s) -> do
    addr <- buildStr s
    return (symbolTable, addr)

  Optimized _ _ (Opt.Do exps) -> do
    ret <- generateBody symbolTable exps
    return (symbolTable, ret)

  Optimized _ _ (TupleConstructor exps) -> do
    exps' <- mapM ((snd <$>). generateExp symbolTable) exps
    boxedExps <- mapM box exps'
    let expsWithIds = List.zip boxedExps [0..]
        tupleType = Type.StructureType False (typeOf <$> boxedExps)
    tuplePtr <- call gcMalloc [(Operand.ConstantOperand $ sizeof tupleType, [])]
    tuplePtr' <- bitcast tuplePtr (Type.ptr tupleType)
    Monad.foldM_ (storeItem tuplePtr') () expsWithIds

    return (symbolTable, tuplePtr')

  Optimized _ _ (ListConstructor []) ->
    return (symbolTable, Operand.ConstantOperand (Constant.Null listType))

  Optimized _ _ (ListConstructor listItems) -> do
    tail <- case List.last listItems of
      Optimized _ _ (ListItem lastItem) -> do
        (symbolTable', lastItem') <- generateExp symbolTable lastItem
        lastItem'' <- box lastItem'
        call madlistSingleton [(lastItem'', [])]

      Optimized _ _ (ListSpread spread) -> do
        (_, spread') <- generateExp symbolTable spread
        return spread'

      cannotHappen ->
        undefined

    list <- Monad.foldM
      (\list' i -> case i of
        Optimized _ _ (ListItem item) -> do
          (_, item') <- generateExp symbolTable item
          item'' <- box item'
          call madlistPush [(item'', []), (list', [])]

        Optimized _ _ (ListSpread spread) -> do
          (_, spread') <- generateExp symbolTable spread
          call madlistConcat [(spread', []), (list', [])]

        cannotHappen ->
          undefined
      )
      tail
      (List.reverse $ List.init listItems)

    list' <- bitcast list listType
    return (symbolTable, list')


  Optimized t _ (If cond truthy falsy) -> mdo
    (symbolTable', cond') <- generateExp symbolTable cond
    test  <- icmp IntegerPredicate.EQ cond' true
    condBr test truthyBlock falsyBlock

    truthyBlock <- block `named` "truthyBlock"
    (symbolTable'', truthy') <- generateExp symbolTable' truthy
    truthy'' <- box truthy'
    br exitBlock

    falsyBlock <- block `named` "falsyBlock"
    (symbolTable''', falsy') <- generateExp symbolTable' falsy
    falsy'' <- box falsy'
    br exitBlock

    exitBlock <- block `named` "condBlock"
    ret <- phi [(truthy'', truthyBlock), (falsy'', falsyBlock)]

    ret' <- unbox t ret

    return (symbolTable', ret')


  Optimized t _ (Where exp iss) -> mdo
    (_, exp') <- generateExp symbolTable exp
    branches  <- generateBranches symbolTable exitBlock exp' iss

    exitBlock <- block `named` "exitBlock"
    ret <- phi branches

    return (symbolTable, ret)

  _ ->
    error $ "not implemented\n\n" ++ ppShow exp




generateBranches :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> AST.Name -> Operand -> [Is] -> m [(Operand, AST.Name)]
generateBranches symbolTable exitBlock whereExp iss = case iss of
  (is : next) -> do
    branch <- generateBranch symbolTable (not (List.null next)) exitBlock whereExp is
    next'  <- generateBranches symbolTable exitBlock whereExp next
    return $ branch ++ next'

  [] ->
    return []


generateBranch :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Bool -> AST.Name -> Operand -> Is -> m [(Operand, AST.Name)]
generateBranch symbolTable hasMore exitBlock whereExp is = case is of
  Optimized _ _ (Is pat exp) -> mdo
    currBlock <- currentBlock
    test <- generateBranchTest symbolTable pat whereExp
    condBr test branchExpBlock nextBlock

    branchExpBlock <- block `named` "branchExpBlock"
    symbolTable' <- generateSymbolTableForPattern symbolTable whereExp pat
    (_, branch) <- generateExp symbolTable' exp
    -- the exp might contain a where or if expression generating new blocks in between.
    -- therefore we need to get the block that contains the register reference in which
    -- it is defined. 
    retBlock <- currentBlock
    branch' <- box branch
    br exitBlock

    (nextBlock, finalPhi) <-
      if hasMore then do
        b <- block `named` "nextBlock"
        return (b, [])
      else do
        let def = Operand.ConstantOperand (Constant.Null (typeOf branch'))
        return (exitBlock, [(def, currBlock)])


    return $ (branch', retBlock) : finalPhi

  _ ->
    undefined


generateSymbolTableForIndexedData :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> SymbolTable -> (Pattern, Integer) -> m SymbolTable
generateSymbolTableForIndexedData basePtr symbolTable (pat, index) = do
  ptr  <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
  ptr' <- load ptr 8
  ptr'' <- unbox (getType pat) ptr'
  generateSymbolTableForPattern symbolTable ptr'' pat


generateSymbolTableForList :: (MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Operand -> [Pattern] -> m SymbolTable
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
      nextNodePtr'' <- bitcast nextNodePtr' listType
      generateSymbolTableForList symbolTable' nextNodePtr'' next

  [] ->
    return symbolTable



generateSymbolTableForPattern :: (MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Operand -> Pattern -> m SymbolTable
generateSymbolTableForPattern symbolTable baseExp pat = case pat of
  Optimized _ _ (PVar n) ->
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
      nextNodePtr'' <- bitcast nextNodePtr' listType
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
    s' <- buildStr s
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

getStructPointers :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [Integer] -> Operand -> m [Operand]
getStructPointers ids ptr = case ids of
  (index : next) -> do
    ptr'  <- gep ptr [i32ConstOp 0, i32ConstOp index]
    next  <- getStructPointers (List.tail ids) ptr
    return $ ptr' : next

  [] ->
    return []


generateExps :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> [Exp] -> m ()
generateExps symbolTable exps = case exps of
  [exp] -> do
    generateExp symbolTable exp
    return ()

  (exp : es) -> do
    (symbolTable', _) <- generateExp symbolTable exp
    generateExps symbolTable' es

  _ ->
    return ()



-- collectDictParams :: Exp -> ([(String, Type.Type)], Exp)
-- collectDictParams exp = case exp of
--   Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds False True, typingStr) exp') ->
--     let dictName = "$" <> interfaceName <> "$" <> typingStr
--         -- TODO: generate based on interface data, mainly we need to know how many methods we have in the interface
--         dictType = Type.ptr $ Type.StructureType False [Type.ptr $ Type.FunctionType boxType [boxType] False]
--         (nextParams, nextExp) = collectDictParams exp'
--     in  ((dictName, dictType) : nextParams, nextExp)

--   Optimized t _ (Abs paramName [body]) ->
--     ([], exp)

--   _ ->
--     undefined



-- generateFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> String -> Exp -> m SymbolTable
-- generateFunction symbolTable fnName abs = case abs of
--   Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds False True, typingStr) _) -> do
--     let (dictParams, Optimized t _ (Abs paramName [body])) = collectDictParams abs
--         dictParams' = (\(n, t) -> (t, ParameterName (stringToShortByteString n))) <$> dictParams
--         param = (boxType, ParameterName (stringToShortByteString paramName))
--     f <- function (AST.mkName fnName) (dictParams' ++ [param]) boxType $ \params -> do
--       lastParam <- unbox (InferredType.getParamType t) (List.last params)
--       let dictParamsSymbolTable = Map.fromList $ List.zip (fst <$> dictParams) (varSymbol <$> List.init params)
--       let symbolTable' = Map.insert paramName (varSymbol lastParam) $ dictParamsSymbolTable <> symbolTable
--       (_, exps) <- generateExp symbolTable' body
--       ret exps

--     return $ Map.insert fnName (fnSymbol f) symbolTable

--   _ ->
--     error $ "unhandled function!\n\n" <> ppShow abs


generateExternFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> InferredType.Type -> String -> Int -> Operand -> m SymbolTable
generateExternFunction symbolTable t functionName arity foreignFn = do
  let paramTypes    = InferredType.getParamTypes t
      params'       = List.replicate arity (boxType, NoParameterName)
      functionName' = AST.mkName functionName

  function <- function functionName' params' boxType $ \params -> do
    -- Generate body
    result <- call foreignFn ((, []) <$> params)

    -- box the result
    boxed <- box result
    ret boxed

  return $ Map.insert functionName (fnSymbol arity function) symbolTable


makeParamName :: String -> ParameterName
makeParamName = ParameterName . stringToShortByteString


generateFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> InferredType.Type -> String -> [String] -> [Exp] -> m SymbolTable
generateFunction symbolTable t functionName paramNames body = do
  let paramTypes    = InferredType.getParamTypes t
      params'       = (boxType,) . makeParamName <$> paramNames
      functionName' = AST.mkName functionName

  function <- function functionName' params' boxType $ \params -> mdo
    let typesWithParams = List.zip paramTypes params
    unboxedParams <- mapM (uncurry unbox) typesWithParams
    let paramsWithNames       = Map.fromList $ List.zip paramNames (varSymbol <$> unboxedParams)
        symbolTableWithParams = symbolTable <> paramsWithNames

    -- Generate body
    generatedBody <- generateBody symbolTableWithParams body

    -- box the result
    boxed <- box generatedBody
    ret boxed

  return $ Map.insert functionName (fnSymbol (List.length paramNames) function) symbolTable

generateTopLevelFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Exp -> m SymbolTable
generateTopLevelFunction symbolTable topLevelFunction = case topLevelFunction of
  Optimized t _ (TopLevelAbs functionName params body) -> do
    generateFunction symbolTable t functionName params body

  Optimized _ _ (Extern (_ InferredType.:=> t) name originalName) -> do
    let paramTypes  = InferredType.getParamTypes t
        paramTypes' = boxType <$ paramTypes
        returnType  = InferredType.getReturnType t
        returnType' = boxType

    ext <- extern (AST.mkName originalName) paramTypes' returnType'
    generateExternFunction symbolTable t name (List.length paramTypes) ext



  _ ->
    return symbolTable


-- {i8* (i8*, i8*)*, {}*}*
emptyClosureType :: Type.Type
emptyClosureType =
  Type.ptr $ Type.StructureType False
    [ Type.ptr $ Type.FunctionType boxType [boxType, boxType] False
    , Type.ptr $ Type.StructureType False []
    ]


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

  Optimized t _ (Assignment name exp _) ->
    let expType = buildLLVMType t
        globalRef = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr expType) (AST.mkName name))
    in  Map.insert name (topLevelSymbol globalRef) symbolTable

  _ ->
    symbolTable


listToIndices :: [a] -> [Int]
listToIndices l | List.null l = []
                | otherwise   = [0 .. List.length l - 1]





generateBody :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> [Exp] -> m Operand
generateBody symbolTable exps = case exps of
  [exp] -> do
    (_, result) <- generateExp symbolTable exp
    return result

  (exp : es) -> do
    (symbolTable', _) <- generateExp symbolTable exp
    generateBody symbolTable' es

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


generateTopLevelFunctions :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> [Exp] -> m SymbolTable
generateTopLevelFunctions symbolTable topLevelFunctions = case topLevelFunctions of
  (fn : fns) -> do
    symbolTable' <- generateTopLevelFunction symbolTable fn
    generateTopLevelFunctions (symbolTable <> symbolTable') fns

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
generateConstructor :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> (Constructor, Int) -> m SymbolTable
generateConstructor symbolTable (constructor, index) = case constructor of
  Untyped _ (Constructor constructorName _ t) -> do
    let paramTypes     = InferredType.getParamTypes t
    let arity          = List.length paramTypes
    let structType     = Type.StructureType False $ Type.IntegerType 64 : List.replicate arity boxType
    let paramLLVMTypes = (,NoParameterName) <$> List.replicate arity boxType

    constructor <- function (AST.mkName constructorName) paramLLVMTypes boxType $ \params -> do
    -- allocate memory for the structure
      structPtr     <- call gcMalloc [(Operand.ConstantOperand $ sizeof structType, [])]
      structPtr'    <- bitcast structPtr $ Type.ptr structType

      -- store the constructor data in the struct
      Monad.foldM_ (storeItem structPtr') () $ List.zip params [1..] ++ [(i64ConstOp (fromIntegral index), 0)]

      boxed <- box structPtr'
      ret boxed

    return $ Map.insert constructorName (constructorSymbol constructor index arity) symbolTable
    -- generateConstructorClosures symbolTable index n 0 (List.length paramTypes - 1) paramTypes

  _ ->
    undefined


generateConstructorsForADT :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> TypeDecl -> m SymbolTable
generateConstructorsForADT symbolTable adt = case adt of
  Untyped _ ADT { adtconstructors } ->
    let indexedConstructors = List.zip adtconstructors [0..]
    in  Monad.foldM generateConstructor symbolTable indexedConstructors

  _ ->
    undefined



generateConstructors :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> [TypeDecl] -> m SymbolTable
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
            next       = buildDictValues symbolTable ns
        in  methodRef : next

      Nothing ->
        undefined

  [] ->
    []


generateMethod :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> String -> Exp -> m SymbolTable
generateMethod symbolTable methodName exp = case exp of
  -- Optimized t _ (Abs param body) ->
  --   generateCurriedFunction True symbolTable t name [] param body

  -- Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds False True, typingStr) _) -> do
  --   let (dictParams, Optimized t _ (Abs paramName [body])) = collectDictParams exp
  --       dictParams' = (\(n, t) -> (t, ParameterName (stringToShortByteString n))) <$> dictParams
  --       param = (boxType, ParameterName (stringToShortByteString paramName))
  --   f <- function (AST.mkName name) (dictParams' ++ [param]) boxType $ \params -> do
  --     lastParam <- unbox (InferredType.getParamType t) (List.last params)
  --     let dictParamsSymbolTable = Map.fromList $ List.zip (fst <$> dictParams) (varSymbol <$> List.init params)
  --     let symbolTable' = Map.insert paramName (varSymbol lastParam) $ dictParamsSymbolTable <> symbolTable
  --     (_, exps) <- generateExp symbolTable' body
  --     ret exps

  --   return $ Map.insert name (fnSymbol f) symbolTable

  Opt.Optimized t _ (TopLevelAbs _ params body) ->
    generateFunction symbolTable t methodName params body

  Opt.Optimized _ _ (Opt.Assignment _ exp _) -> do
    -- generate a global
    f <- function (AST.mkName methodName) [] emptyClosureType $ \_ -> do
      (symbolTable, exp') <- generateExp symbolTable exp
      ret exp'

    return $ Map.insert methodName (fnSymbol 0 f) symbolTable

  _ ->
    undefined


generateInstance :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Instance -> m SymbolTable
generateInstance symbolTable inst = case inst of
  Untyped _ (Instance interface preds typingStr methods) -> do
    let instanceName = "$" <> interface <> "$" <> typingStr
        prefixedMethods = Map.mapKeys ((instanceName <> "$") <>) methods
        prefixedMethods' = (\(name, method) -> (name, method)) <$> Map.toList prefixedMethods
        prefixedMethodNames = fst <$> prefixedMethods'
    symbolTable' <- Monad.foldM (\symbolTable (name, (method, _)) -> generateMethod symbolTable name method) symbolTable prefixedMethods'
    let methodConstants = buildDictValues symbolTable' prefixedMethodNames

    dict <- global (AST.mkName instanceName) (Type.StructureType False (typeOf <$> methodConstants)) $ Constant.Struct Nothing False methodConstants

    return $ Map.insert instanceName (Symbol (DictionarySymbol (Map.fromList  $ List.zip (Map.keys methods) [0..])) dict) symbolTable'

  _ ->
    undefined



generateInstances :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> [Instance] -> m SymbolTable
generateInstances =
  Monad.foldM generateInstance


expsForMain :: [Exp] -> [Exp]
expsForMain =
  List.filter (not . \e -> isTopLevelFunction e || isExtern e)


topLevelFunctions :: [Exp] -> [Exp]
topLevelFunctions =
  List.filter $ \e -> isTopLevelFunction e || isExtern e


toLLVMModule :: AST -> AST.Module
toLLVMModule ast =
  buildModule "main" $ do
  let initialSymbolTable = List.foldr (flip addTopLevelFnToSymbolTable) mempty (aexps ast)

  symbolTable   <- generateInstances initialSymbolTable (ainstances ast)
  symbolTable'  <- generateConstructors symbolTable (atypedecls ast)
  symbolTable'' <- generateTopLevelFunctions (trace ("ST: "<>ppShow symbolTable') symbolTable') (topLevelFunctions $ aexps ast)

  externVarArgs (AST.mkName "__applyPAP__") [Type.ptr Type.i8, Type.i32] (Type.ptr Type.i8)
  extern (AST.mkName "malloc") [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc") [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "calloc") [Type.i32, Type.i32] (Type.ptr Type.i8)
  extern (AST.mkName "__streq__") [Type.ptr Type.i8, Type.ptr Type.i8] Type.i1
  extern (AST.mkName "MadList_hasMinLength") [Type.double, listType] Type.i1
  extern (AST.mkName "MadList_hasLength") [Type.double, listType] Type.i1
  extern (AST.mkName "MadList_singleton") [Type.ptr Type.i8] listType
  extern (AST.mkName "__MadList_push__") [Type.ptr Type.i8, listType] listType
  extern (AST.mkName "MadList_concat") [listType, listType] listType

  global (AST.mkName "$EMPTY_ENV") (Type.StructureType False []) (Constant.Struct Nothing False [])

  function "main" [] void $ \_ -> do
    entry <- block `named` "entry";
    generateExps symbolTable'' (expsForMain $ aexps ast)
    retVoid


generate :: AST -> IO ()
generate ast = do
  Prelude.putStrLn "generate llvm"
  let mod = toLLVMModule ast



  T.putStrLn $ ppllvm mod

  withHostTargetMachineDefault $ \target -> do
    withContext $ \ctx -> do
      withModuleFromAST ctx mod $ \mod' -> do
        mod'' <- withPassManager defaultCuratedPassSetSpec { optLevel = Just 1 } $ \pm -> do
          runPassManager pm mod'
          return mod'
        writeObjectToFile target (File "module.o") mod''

  callCommand "clang++ -g -stdlib=libc++ -v module.o generate-llvm/lib.o gc.a -o a.out"
