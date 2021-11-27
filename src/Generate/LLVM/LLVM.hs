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
import Generate.LLVM.Optimized
import Text.Show.Pretty
import Debug.Trace
import qualified Data.String.Utils as List
import Foreign.C
import qualified Infer.Type as InferredType
import Infer.Type (isFunctionType)
import LLVM.PassManager


-- TODO: Move to util module
sizeof :: Type.Type -> Constant.Constant
sizeof t = Constant.PtrToInt szPtr (Type.IntegerType 64)
  where
     ptrType = Type.PointerType t (AddrSpace 0)
     nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
     szPtr   = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]


data SymbolType
  = VariableSymbol
  | FunctionSymbol
  | ConstructorSymbol Int Int
  | ClosureSymbol
  -- ^ unique id ( index ) | arity
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

closureSymbol :: Operand -> Symbol
closureSymbol =
  Symbol ClosureSymbol

fnSymbol :: Operand -> Symbol
fnSymbol =
  Symbol FunctionSymbol

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



buildStr :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => String -> m Operand
buildStr s = do
  let s' = List.init . List.tail $ s
  -- we need to add 0 to terminate a C string
  let charCodes = (fromEnum <$> s') ++ [0]
  -- 92, 110 == \n
  let charCodes' = List.replace [92, 110] [10] charCodes
  let charCodes'' = toInteger <$> charCodes'
  addr <- call gcMalloc [(ConstantOperand (Constant.Int 64 (fromIntegral $ List.length charCodes'')), [])]
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


generateExp :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m (SymbolTable, Operand)
generateExp symbolTable exp = case exp of
  Optimized _ _ (Var "puts") ->
    return (symbolTable, Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i32 [ptr i8] False) (AST.mkName "puts")))

  Optimized _ _ (Var n) ->
    case Map.lookup n symbolTable of
      Just (Symbol FunctionSymbol global) ->
        return (symbolTable, global)

      Just (Symbol ClosureSymbol closurePtr) -> do
        -- closure <- load closurePtr 8
        return (symbolTable, closurePtr)

      Just (Symbol (ConstructorSymbol _ 0) var) -> do
        struct <- call var []
        return (symbolTable, struct)

      Just (Symbol _ var) -> do
        return (symbolTable, var)

      Nothing ->
        error $ "Var not found " <> n <> "\n\n" <> ppShow symbolTable

-- (Placeholder
--   ( MethodRef "Show" "show" False , "Number" )
--   (Optimized
--     (TApp
--         (TApp
--           (TCon (TC "(->)" (Kfun Star (Kfun Star Star))) "prelude")
--           (TVar (TV "e4" Star)))
--         (TCon (TC "String" Star) "prelude"))
--     (Area (Loc 101 10 1) (Loc 105 10 5))
--     (Var "show"))))
  Optimized _ _ (Placeholder (MethodRef interface methodName False, typingStr) _) -> do
    let dictName = "$" <> interface <> "$" <> typingStr
    case Map.lookup dictName symbolTable of
      Just (Symbol (DictionarySymbol methodIndices) dict) -> do
        let index = Maybe.fromMaybe 0 $ Map.lookup methodName methodIndices
        method <- gep dict [i32ConstOp 0, i32ConstOp (fromIntegral index)]
        method' <- load method 8
        return (symbolTable, method')

      _ ->
        undefined

-- (Placeholder
--   ( MethodRef
--       "Show" "show" True
--   , "j9"
--   )
  Optimized _ _ (Placeholder (MethodRef interface methodName True, typingStr) _) -> do
    let dictName = "$" <> interface <> "$" <> typingStr
    case Map.lookup dictName symbolTable of
      -- TODO: this should be a DictionarySymbol with indices
      Just (Symbol VariableSymbol dict) -> do
        let index = 0
        method <- gep dict [i32ConstOp 0, i32ConstOp (fromIntegral index)]
        method' <- load method 8
        return (symbolTable, method')

      _ ->
        undefined

  Optimized _ _ (TypedExp (Optimized _ _ (Assignment "puts" e)) _) ->
    return (symbolTable, Operand.ConstantOperand $ Constant.Null i8)

  Optimized _ _ (Assignment name e) -> do
    (symbolTable', exp') <- generateExp symbolTable e
    return (Map.insert name (varSymbol exp') symbolTable', exp')

  Optimized _ _ (App f arg _) ->
    case f of
      -- ( ClassRef "Show" [] True False , "Boolean" )
      Optimized _ _ (Placeholder (ClassRef interface classRefPreds True False, typingStr) _) -> do
        (dicts, f) <- collectDictArgs symbolTable f
        (symbolTable', f')    <- generateExp symbolTable f
        (symbolTable'', arg') <- generateExp symbolTable' arg
        arg'' <- box arg'
        case typeOf f' of
          Type.PointerType Type.FunctionType{} _ -> do
            res <- call f' (((,[]) <$> dicts) ++ [(arg'', [])])
            return (symbolTable'', res)

      _ -> do
        (symbolTable', f')    <- generateExp symbolTable f
        (symbolTable'', arg') <- generateExp symbolTable' arg

        case typeOf f' of
          Type.PointerType Type.FunctionType{} _ -> do
            res <- call f' [(arg', [])]
            return (symbolTable'', res)

          _ -> do
            arg'' <- box arg'


            let fnType = Type.ptr $ Type.StructureType False [Type.ptr $ Type.FunctionType boxType [boxType, boxType] False, boxType]

            unboxedFn <- bitcast f' fnType

            fn <- gep (trace ("F': "<>ppShow unboxedFn <> "\nfExp: "<>ppShow f) unboxedFn) [i32ConstOp 0, i32ConstOp 0]
            fn' <- load fn 8
            env <- gep unboxedFn [i32ConstOp 0, i32ConstOp 1]
            env' <- load env 8

            res <- call fn' [(env', []), (arg'', [])]
            return (symbolTable'', res)


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

  Optimized _ _ (TupleConstructor exps) -> do
    exps' <- mapM ((snd <$>). generateExp symbolTable) exps
    let expsWithIds = List.zip exps' [0..]
        tupleType = Type.StructureType False (typeOf <$> exps')
    tuplePtr <- call gcMalloc [(Operand.ConstantOperand $ sizeof tupleType, [])]
    tuplePtr' <- bitcast tuplePtr (Type.ptr tupleType)
    Monad.foldM_ (storeItem tuplePtr') () expsWithIds

    return (symbolTable, tuplePtr')

  Optimized _ _ (If cond truthy falsy) -> mdo
    (symbolTable', cond') <- generateExp symbolTable cond
    test  <- icmp IntegerPredicate.EQ cond' true
    condBr test truthyBlock falsyBlock

    truthyBlock <- block `named` "truthyBlock"
    (symbolTable'', truthy') <- generateExp symbolTable' truthy
    br exitBlock

    falsyBlock <- block `named` "falsyBlock"
    (symbolTable''', falsy') <- generateExp symbolTable' falsy
    br exitBlock

    exitBlock <- block `named` "condBlock"
    ret <- phi [(truthy', truthyBlock), (falsy', falsyBlock)]

    return (symbolTable', ret)


  Optimized _ _ (Where exp iss) -> mdo
    (_, exp') <- generateExp symbolTable exp
    branches  <- generateBranches symbolTable exitBlock exp' iss
    let (b, _) = List.head branches

    exitBlock <- block `named` "exitBlock"
    ret <- phi branches

    return (symbolTable, ret)


  {-
    Closures:
      { i8* (i8*, i8*)*, { [ENV] } }
      { functionRef, closured_args }
  -}
  Optimized _ _ (Closure name env) -> do
    envItems <- mapM (generateExp symbolTable) env
    envItems' <- mapM (box . snd) envItems
    let envWithIds = List.zip envItems' [0..]
    let envType = Type.StructureType False (typeOf <$> envItems')
    env' <- call gcMalloc [(Operand.ConstantOperand $ sizeof envType, [])]
    env'' <- bitcast env' (Type.ptr envType)
    Monad.foldM_ (storeItem env'') () envWithIds

    let f = case Map.lookup name symbolTable of
          Just (Symbol _ f') ->
            f'

          _ ->
            error $ "Closure '" <> name <> "' not found!\n\n" <> ppShow symbolTable

    let closureType = Type.StructureType False [typeOf f, typeOf env'']

    closure <- call gcMalloc [(Operand.ConstantOperand $ sizeof closureType, [])]
    closure' <- bitcast closure (Type.ptr closureType)
    Monad.foldM_ (storeItem closure') () [(f, 0), (env'', 1)]
    return (symbolTable, closure')


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
    br exitBlock

    (nextBlock, finalPhi) <-
      if hasMore then do
        b <- block `named` "nextBlock"
        return (b, [])
      else do
        let def = Operand.ConstantOperand (Constant.Null (typeOf branch))
        return (exitBlock, [(def, currBlock)])


    return $ (branch, branchExpBlock) : finalPhi

  _ ->
    undefined


generateSymbolTableForIndexedData :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> SymbolTable -> (Pattern, Integer) -> m SymbolTable
generateSymbolTableForIndexedData basePtr symbolTable (pat, index) = do
  ptr  <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
  ptr' <- load ptr 8
  generateSymbolTableForPattern symbolTable ptr' pat



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
  curr <- generateBranchTest symbolTable pat' v
  prev `Instruction.and` curr


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
    let ids = fromIntegral <$> List.take (List.length pats) [0..]
    itemPtrs <- getStructPointers ids value
    let patsWithPtrs = List.zip pats itemPtrs
    Monad.foldM (generateSubPatternTest symbolTable) true patsWithPtrs

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


buildLLVMType :: InferredType.Type -> Type.Type
buildLLVMType t = case t of
  InferredType.TCon (InferredType.TC "Number" InferredType.Star) "prelude" ->
    Type.double

  InferredType.TCon (InferredType.TC "String" InferredType.Star) "prelude" ->
    Type.ptr Type.i8

  InferredType.TCon (InferredType.TC "Boolean" InferredType.Star) "prelude" ->
    Type.i1

  InferredType.TCon (InferredType.TC "()" InferredType.Star) "prelude" ->
    Type.i1

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") left) right ->
    let tLeft  = buildLLVMType left
        tRight = buildLLVMType right
    in  Type.ptr $ Type.FunctionType tRight [tLeft] False

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(,)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") a) b ->
    let tA = buildLLVMType a
        tB = buildLLVMType b
    in  Type.ptr $ Type.StructureType False [tA, tB]

  _ ->
    Type.ptr Type.i8

boxType :: Type.Type
boxType = Type.ptr Type.i8

unbox :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => InferredType.Type -> Operand -> m Operand
unbox t what = case t of
  InferredType.TCon (InferredType.TC "Number" _) _ -> do
    int <- ptrtoint what Type.i64
    bitcast int (buildLLVMType t)

  InferredType.TCon (InferredType.TC "Boolean" _) _ -> do
    int <- ptrtoint what Type.i1
    bitcast int (buildLLVMType t)

  -- This should be called for parameters that are closures
    
  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" _) _) p) b -> do
    -- let closureType = Type.ptr $ Type.StructureType False [buildLLVMType t, Type.StructureType False []]
    -- bitcast what closureType
    return what
    -- load casted 8
  -- InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" _) _) p) b -> do
  --   let closureType = Type.StructureType False [buildLLVMType t, Type.StructureType False []]
  --   closure <- alloca closureType Nothing 0
  --   -- closure <- call malloc [(Operand.ConstantOperand  $ sizeof closureType, [])]
  --   closure' <- bitcast closure (Type.ptr boxType)
  --   store closure' 8 what
  --   casted <- bitcast closure' (Type.ptr closureType)
  --   load casted 8

  _ ->
    bitcast what (buildLLVMType t)

box :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
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


updateClosureType :: [InferredType.Type] -> Type.Type -> Type.Type
updateClosureType envArgs t = case t of
  Type.PointerType (Type.FunctionType ret args _) _ ->
    let envArgs' = buildLLVMType <$> envArgs
    in  Type.ptr $ Type.StructureType False [Type.ptr $ Type.FunctionType boxType (boxType <$ envArgs' ++ args) False, Type.StructureType False (boxType <$ envArgs')]

  _ ->
    t


collectDictParams :: Exp -> ([(String, Type.Type)], Exp)
collectDictParams exp = case exp of
  Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds False True, typingStr) exp') ->
    let dictName = "$" <> interfaceName <> "$" <> typingStr
        -- TODO: generate based on interface data, mainly we need to know how many methods we have in the interface
        dictType = Type.ptr $ Type.StructureType False [Type.ptr $ Type.FunctionType boxType [boxType] False]
        (nextParams, nextExp) = collectDictParams exp'
    in  ((dictName, dictType) : nextParams, nextExp)

  Optimized t _ (Abs paramName [body]) ->
    ([], exp)

  _ ->
    undefined



generateFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> String -> Exp -> m SymbolTable
generateFunction symbolTable fnName abs = case abs of
  Optimized t _ (Abs paramName [body]) -> do
    let param = (boxType, ParameterName (stringToShortByteString paramName))
        returnType' = case body of
          Optimized _ _ (Closure n args) ->
            updateClosureType (getType <$> args) (buildLLVMType $ InferredType.getReturnType t)
          _ ->
            boxType

    let closureName = fnName ++ "_closure"

    f <- function (AST.mkName closureName) [param] returnType' $ \[param'] -> do
      entry <- block `named` "entry"; do
        param'' <- unbox (InferredType.getParamType t) param'
        (_, exps) <- generateExp (Map.insert paramName (varSymbol param'') symbolTable) body
        exps' <- case body of
          Optimized _ _ Closure{} ->
            return exps
          _ ->
            box exps
        ret exps'

    let closureStruct =
          Constant.Struct
            Nothing
            False
            [ Constant.GlobalReference
                (Type.ptr $ Type.FunctionType returnType' [boxType] False)
                (AST.mkName closureName)
            , Constant.Int 64 0
            , Constant.Struct Nothing False []
            ]

    g <- global (AST.mkName fnName) (typeOf closureStruct) closureStruct

    return 
      $ Map.insert fnName (closureSymbol g)
      $ Map.insert closureName (fnSymbol f) symbolTable

  -- TODO: needs to handle closures
  Optimized t _ (Placeholder (ClassRef interfaceName classRefPreds False True, typingStr) _) -> do
    let (dictParams, Optimized t _ (Abs paramName [body])) = collectDictParams abs
        dictParams' = (\(n, t) -> (t, ParameterName (stringToShortByteString n))) <$> dictParams
        param = (boxType, ParameterName (stringToShortByteString paramName))
    f <- function (AST.mkName fnName) (dictParams' ++ [param]) boxType $ \params -> do
      lastParam <- unbox (InferredType.getParamType t) (List.last params)
      let dictParamsSymbolTable = Map.fromList $ List.zip (fst <$> dictParams) (varSymbol <$> List.init params)
      let symbolTable' = Map.insert paramName (varSymbol lastParam) $ dictParamsSymbolTable <> symbolTable
      (_, exps) <- generateExp symbolTable' body
      ret exps

    return $ Map.insert fnName (fnSymbol f) symbolTable

  _ ->
    error $ "unhandled function!\n\n" <> ppShow abs


makeParamName :: String -> ParameterName
makeParamName = ParameterName . stringToShortByteString

generateTopLevelFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Exp -> m SymbolTable
generateTopLevelFunction symbolTable topLevelFunction = case topLevelFunction of
  Optimized _ _ (TypedExp (Optimized _ _ (Export (Optimized _ _ (Assignment fnName abs@(Optimized t _ (Abs paramName body)))))) _) -> do
    generateClosure True symbolTable t fnName [] paramName body

  Optimized _ _ (Export (Optimized _ _ (Assignment fnName abs@(Optimized t _ (Abs paramName body))))) -> do
    generateClosure True symbolTable t fnName [] paramName body

  Optimized _ _ (TypedExp (Optimized _ _ (Assignment fnName abs@(Optimized t _ (Abs paramName body)))) _) -> do
    generateClosure True symbolTable t fnName [] paramName body

  Optimized _ _ (Assignment fnName abs@(Optimized t _ (Abs paramName body))) -> do
    generateClosure True symbolTable t fnName [] paramName body

  {-
    A ClosureDef is a function of type:
    i8* (i8*, i8*)
  -}
  Optimized t _ (ClosureDef fnName env paramName body) -> do
    generateClosure False symbolTable t fnName env paramName body



  -- Optimized t _ (ClosureDef fnName env paramName [body]) -> do
  --   let envParams = (\(Optimized t _ (Var n)) -> (boxType, ParameterName (stringToShortByteString n))) <$> env
  --       paramType  = buildLLVMType $ InferredType.getParamType t
  --       envParams' = envParams ++ [(boxType, ParameterName (stringToShortByteString paramName))]
  --       allParamTypes = (getType <$> env) ++ [InferredType.getParamType t]
  --       returnType = boxType
  --       returnType' = case body of
  --         Optimized _ _ (Closure n args) ->
  --           updateClosureType (getType <$> args) (buildLLVMType $ InferredType.getReturnType t)
  --         _ ->
  --           returnType

  --   f <- function (AST.mkName (trace ("curr FN: "<>ppShow fnName) fnName)) envParams' returnType' $ \params -> do
  --     entry <- block `named` "entry"; do
  --       vars' <- Monad.zipWithM unbox allParamTypes params
  --       (_, exps) <- generateExp (Map.fromList (List.zip (getClosureParamNames env ++ [paramName]) (varSymbol <$> vars')) <> symbolTable) body
  --       exps' <- case body of
  --         Optimized _ _ Closure{} ->
  --           return exps
  --         _ ->
  --           box exps
  --       ret exps'

  --   return $ Map.insert fnName (fnSymbol f) symbolTable

  _ ->
    return symbolTable


listToIndices :: [a] -> [Int]
listToIndices l | List.null l = []
                | otherwise   = [0 .. List.length l - 1]



generateClosure :: (MonadModuleBuilder m, MonadFix.MonadFix m) =>
  Bool
  -> Map.Map String Symbol
  -> InferredType.Type
  -> String
  -> [Optimized Exp_]
  -> [Char]
  -> [Exp]
  -> m (Map.Map String Symbol)
generateClosure isTopLevel symbolTable t fnName env paramName [body] = mdo
  let envType = Type.ptr $ Type.StructureType False (boxType <$ env)
      closureEnvNames = getClosureParamNames env
      closureEnvTypes = getType <$> env

      closureParams = [(boxType, makeParamName "env"), (boxType, makeParamName paramName)]

      paramType = InferredType.getParamType t

      functionName =
        if isTopLevel then
          AST.mkName $ fnName ++ "$fn"
        else
          AST.mkName fnName

  closure <- function functionName closureParams boxType $ \params -> do
    let envParam      = List.head params
        explicitParam = params !! 1

    -- unbox
    unwrappedEnv   <- bitcast envParam envType

    envArgs        <- extractEnvArgs unwrappedEnv (listToIndices closureEnvNames)
    unboxedEnvArgs <- Monad.zipWithM unbox closureEnvTypes envArgs

    unboxedParam   <- unbox paramType explicitParam


    -- Generate symbol table for the body
    let envMap                     = Map.fromList $ List.zip closureEnvNames unboxedEnvArgs
        fullMap                    = Map.insert paramName unboxedParam envMap
        symbolTableWithEnvAndParam = varSymbol <$> fullMap
        completeSymbolTable        = symbolTable <> symbolTableWithEnvAndParam


    withRecursion <-
      if isTopLevel then do
        let closureStruct =
              Constant.Struct
                Nothing
                False
                [ Constant.GlobalReference
                    (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False)
                    functionName
                , Constant.GlobalReference (Type.ptr $ Type.StructureType False []) (AST.mkName "$EMPTY_ENV")
                ]

        g <- global (AST.mkName fnName) (typeOf closureStruct) closureStruct
        return
          $ Map.insert (fnName ++ "$fn") (fnSymbol closure)
          $ Map.insert fnName (fnSymbol g) completeSymbolTable
      else
        return $ Map.insert fnName (fnSymbol closure) completeSymbolTable

    (_, generatedBody) <- generateExp withRecursion body

    -- TODO: we need to box that back to a i8*
    boxed <- box generatedBody
    ret boxed


  if isTopLevel then do
    let closureStruct =
          Constant.Struct
            Nothing
            False
            [ Constant.GlobalReference
                (Type.ptr $ Type.FunctionType boxType [boxType, boxType] False)
                functionName
            , Constant.GlobalReference (Type.ptr $ Type.StructureType False []) (AST.mkName "$EMPTY_ENV")
            ]

    g <- global (AST.mkName fnName) (typeOf closureStruct) closureStruct
    return
      $ Map.insert (fnName ++ "$fn") (fnSymbol closure)
      $ Map.insert fnName (fnSymbol g) symbolTable
  else
    return $ Map.insert fnName (fnSymbol closure) symbolTable

generateClosure _ _ _ _ _ _ _ = undefined


extractEnvArgs :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand.Operand -> [Int] -> m [Operand.Operand]
extractEnvArgs envPtr indices = case (trace ("INDICES: "<>ppShow indices) indices) of
  (index : is) -> do
    itemPtr   <- gep envPtr [i32ConstOp 0, i32ConstOp (fromIntegral index)]
    item      <- load itemPtr 8

    nextItems <- extractEnvArgs envPtr is

    return $ item : nextItems

  _ ->
    return []


-- extractEnvArgs :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand.Operand -> [(Int, String)] -> m [(String, Operand.Operand)]
-- extractEnvArgs envPtr argNames = case argNames of
--   ((index, n) : ns) -> do
--     itemPtr   <- gep envPtr [i32ConstOp 0, i32ConstOp (fromIntegral index)]
--     item      <- load itemPtr 8

--     nextItems <- extractEnvArgs envPtr ns

--     return $ (n, item) : nextItems

--   _ ->
--     return []


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
  Untyped _ (Constructor n _ t) -> do
    let paramTypes  = InferredType.getParamTypes t
        paramTypes' = boxType <$ paramTypes
        fType       = Type.ptr $ Type.FunctionType boxType paramTypes' False

    f <- function (AST.mkName n) ((, NoParameterName) <$> paramTypes') boxType $ \params -> do
      let structType = Type.StructureType False $ Type.i64 : (typeOf <$> params)
          size       = Operand.ConstantOperand $ sizeof structType
      constructorStruct   <- call gcMalloc [(size, [])]
      constructorStruct'  <- bitcast constructorStruct (Type.ptr structType)
      ptrConstructorIndex <- gep constructorStruct' [i32ConstOp 0, i32ConstOp 0]
      store ptrConstructorIndex 8 (i64ConstOp (fromIntegral index))
      Monad.foldM_ (storeItem constructorStruct') () (List.zip params [1..])
      boxed <- box constructorStruct'
      ret boxed

    return $ Map.insert n (constructorSymbol f index (List.length paramTypes)) symbolTable

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


generateInstance :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Instance -> m SymbolTable
generateInstance symbolTable inst = case inst of
  Untyped _ (Instance interface preds typingStr methods) -> do
    let instanceName = "$" <> interface <> "$" <> typingStr
        prefixedMethods = Map.mapKeys ((instanceName <> "$") <>) methods
        prefixedMethods' = (\(name, (Optimized _ _ (Assignment _ method), _)) -> (name, method)) <$> Map.toList prefixedMethods
        prefixedMethodNames = fst <$> prefixedMethods'
    symbolTable' <- Monad.foldM (\symbolTable (name, method) -> generateFunction symbolTable name method) symbolTable prefixedMethods'

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
  List.filter (not . \e -> isTopLevelFunction e || isClosureDef e)


topLevelFunctions :: [Exp] -> [Exp]
topLevelFunctions =
  List.filter $ \e -> isTopLevelFunction e || isClosureDef e


-- createFFI :: (MonadFix.MonadFix m, MonadModuleBuilder m) => String -> Type.Type -> Type -> m ()
-- createFFI fnName paramType returnType = do
--   let fnName'     = fnName ++ "$fn"
--       closureName = fnName


--   undefined



toLLVMModule :: AST -> AST.Module
toLLVMModule ast =
  buildModule "main" $ do
  symbolTable <- generateInstances Map.empty (ainstances ast)
  symbolTable' <- generateConstructors symbolTable (atypedecls ast)
  symbolTable'' <- generateTopLevelFunctions symbolTable' (topLevelFunctions $ aexps ast)

  extern (AST.mkName "puts") [ptr i8] i32
  extern (AST.mkName "malloc") [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc") [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "calloc") [Type.i32, Type.i32] (Type.ptr Type.i8)
  extern (AST.mkName "__streq__") [ptr i8, ptr i8] i1

  global (AST.mkName "$EMPTY_ENV") (Type.StructureType False []) (Constant.Struct Nothing False [])

  function "main" [] void $ \_ -> do
    entry <- block `named` "entry";
    generateExps (trace ("ST-FOR-EXPS: "<>ppShow symbolTable'') symbolTable'') (expsForMain $ aexps ast)
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
