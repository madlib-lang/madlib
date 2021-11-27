{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
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
import Generate.LLVM.Optimized (isClosureDef)
import qualified Infer.Type as InferredType



type SymbolTable = Map.Map String Operand



stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack


true :: Operand
true = Operand.ConstantOperand (Constant.Int 1 1)


generateExp :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m (SymbolTable, Operand)
generateExp symbolTable exp = case exp of
  Optimized _ _ (Var "puts") ->
    return (symbolTable, Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i32 [ptr i8] False) (AST.mkName "puts")))

  Optimized _ _ (Var n) ->
    case Map.lookup n (trace ("ST: "<>ppShow symbolTable) symbolTable) of
      Just global@(Operand.ConstantOperand Constant.GlobalReference{}) ->
        return (symbolTable, global)

      Just var -> do
        return (symbolTable, var)

      Nothing ->
        error $ "Var not found " <> n

  Optimized _ _ (TypedExp (Optimized _ _ (Assignment "puts" e)) _) ->
    return (symbolTable, Operand.ConstantOperand $ Constant.Null i8)

  Optimized _ _ (Assignment name e) -> do
    (symbolTable', exp') <- generateExp symbolTable e
    -- var                  <- alloca (typeOf exp') Nothing 4
    -- store var 4 exp'
    return (Map.insert name exp' symbolTable', exp')

  Optimized _ _ (App f arg _) -> do
    (symbolTable', f')    <- generateExp symbolTable (trace ("F: "<>ppShow f) f)
    (symbolTable'', arg') <- generateExp symbolTable' arg
    arg'' <- box arg'
    case typeOf (trace (ppShow f') f') of
      Type.PointerType Type.FunctionType{} _ -> do
        res <- call f' [(arg'', [])]
        return (symbolTable'', res)

      -- Closure call
      _ -> do
        let t = getType f
        closurePtr <- alloca (typeOf (trace ("T: "<>ppShow t) f')) Nothing 8
        store closurePtr 8 f'
        fn <- gep closurePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
        fn' <- load fn 8
        closuredArgs <- gep closurePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
        closuredArgs' <- load closuredArgs 8
        let argCount = case typeOf closuredArgs' of
              Type.StructureType _ items ->
                List.length items

              _ ->
                0

        closuredArgs'' <- mapM (\index -> gep closuredArgs [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)] >>= (`load` 8)) $ List.take argCount [0..]
        let closuredArgs''' = (, []) <$> closuredArgs''
        res <- call fn' (closuredArgs''' ++ [(arg'', [])])
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
    let s' = List.init . List.tail $ s
    -- we need to add 0 to terminate a C string
    let charCodes = (fromEnum <$> s') ++ [0]
    -- 92, 110 == \n
    let charCodes' = List.replace [92, 110] [10] charCodes
    let charCodes'' = toInteger <$> charCodes'
    addr <- call (Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "GC_malloc"))) [(ConstantOperand (Constant.Int 64 (fromIntegral $ List.length charCodes'')), [])]
    let charCodesWithIds = List.zip charCodes'' [0..]

    Monad.foldM_ (storeChar addr) () charCodesWithIds
    return (symbolTable, addr)
    where
      storeChar :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Integer, Integer) -> m ()
      storeChar basePtr _ (charCode, index) = do
        ptr <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 index)]
        store ptr 8 (Operand.ConstantOperand (Constant.Int 8 charCode))
        return ()

  Optimized _ _ (TupleConstructor exps) -> do
    exps' <- mapM ((snd <$>). generateExp symbolTable) exps
    let expsWithIds = List.zip exps' [0..]
    tuple <- alloca (Type.StructureType False (typeOf <$> exps')) Nothing 8
    Monad.foldM_ (storeItem tuple) () expsWithIds

    -- TODO: should not return allocad
    return (symbolTable, tuple)
    where


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
    branches  <- generateBranches symbolTable defaultBlock exitBlock exp' iss
    let (b, _) = List.head branches

    defaultBlock <- block `named` "defaultBlock"
    defaultRet <- return $ Operand.ConstantOperand (Constant.Null (typeOf b))
    br exitBlock

    exitBlock <- block `named` "exitBlock"
    ret <- phi $ branches <> [(defaultRet, defaultBlock)]

    return (symbolTable, ret)

  Optimized _ _ (Closure name env) -> do
    envItems <- mapM (generateExp symbolTable) env
    envItems' <- mapM (box . snd) envItems
    let envWithIds = List.zip envItems' [0..]
    env' <- alloca (Type.StructureType False (typeOf <$> envItems')) Nothing 8
    Monad.foldM_ (storeItem env') () envWithIds

    let f = case Map.lookup name symbolTable of
          Just f' ->
            f'

          _ ->
            error $ "Closure '" <> name <> "' not found!\n\n" <> ppShow symbolTable

    env'' <- load env' 8
    -- env''' <- bitcast env'' (Type.StructureType False (boxType <$ envItems))

    closure <- alloca (Type.StructureType False [typeOf f, typeOf env'']) Nothing 8
    Monad.foldM_ (storeItem closure) () [(f, 0), (env'', 1)]

    closure' <- load closure 8
    -- closure'' <- bitcast closure' (Type.StructureType False [typeOf f, typeOf env'''])
    return (symbolTable, closure')

  _ ->
    undefined

storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
storeItem basePtr _ (item, index) = do
  ptr <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
  store ptr 8 item
  return ()


generateBranches :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> AST.Name -> AST.Name -> Operand -> [Is] -> m [(Operand, AST.Name)]
generateBranches symbolTable defaultBlock exitBlock whereExp iss = case iss of
  [is] -> do
    branch <- generateBranch symbolTable defaultBlock False exitBlock whereExp is
    return [branch]

  (is : next) -> do
    branch <- generateBranch symbolTable defaultBlock True exitBlock whereExp is
    next'  <- generateBranches symbolTable defaultBlock exitBlock whereExp next
    return $ branch : next'

  [] ->
    return []


generateBranch :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> AST.Name -> Bool -> AST.Name -> Operand -> Is -> m (Operand, AST.Name)
generateBranch symbolTable defaultBlock hasMore exitBlock whereExp is = case is of
  Optimized _ _ (Is pat exp) -> mdo
    test <- generateBranchTest pat whereExp
    if hasMore then
      condBr test branchExpBlock nextBlock
    else
      condBr test branchExpBlock defaultBlock

    branchExpBlock <- block `named` "branchExpBlock"
    symbolTable' <- generateSymbolTableForPattern symbolTable whereExp pat
    (_, branch) <- generateExp symbolTable' exp
    br exitBlock

    nextBlock <- block `named` "nextBlock"

    return (branch, branchExpBlock)

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
    return $ Map.insert n baseExp symbolTable

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

  _ ->
    undefined


generateBranchTest :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Pattern -> Operand -> m Operand
generateBranchTest pat value = case pat of
  Optimized _ _ (PNum n) ->
    fcmp FloatingPointPredicate.OEQ (C.double (read n)) value

  Optimized _ _ (PBool "true") ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 1) value

  Optimized _ _ (PBool _) ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 0) value

  Optimized _ _ PAny ->
    return true

  Optimized _ _ PVar{} ->
    return true

  Optimized _ _ (PTuple pats) -> do
    let ids = fromIntegral <$> List.take (List.length pats) [0..]
    itemPtrs <- getTuplePointers ids value
    let patsWithPtrs = List.zip pats itemPtrs
    Monad.foldM
      (\prev (pat', ptr) -> do
        v <- load ptr 8
        curr <- generateBranchTest pat' v
        prev `Instruction.and` curr
      )
      true
      patsWithPtrs

  _ ->
    undefined

getTuplePointers :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => [Integer] -> Operand -> m [Operand]
getTuplePointers ids ptr = case ids of
  (index : next) -> do
    ptr'  <- gep ptr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
    next  <- getTuplePointers (List.tail ids) ptr
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

  _ ->
    bitcast what (buildLLVMType t)

box :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> m Operand
box what = case typeOf what of
  Type.FloatingPointType _ -> do
    ptr <- alloca Type.double Nothing 8
    store ptr 8 what
    bitcast ptr boxType

  -- Pointless?
  Type.PointerType (Type.IntegerType 8) _ ->
    return what

  _ ->
    bitcast what boxType


updateClosureType :: [InferredType.Type] -> Type.Type -> Type.Type
updateClosureType envArgs t = case t of
  Type.PointerType (Type.FunctionType ret args _) _ ->
    let envArgs' = buildLLVMType <$> envArgs
    in  Type.StructureType False [Type.ptr $ Type.FunctionType boxType (boxType <$ envArgs' ++ args) False, Type.StructureType False (boxType <$ envArgs')]

  _ ->
    t

generateTopLevelFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Exp -> m SymbolTable
generateTopLevelFunction symbolTable topLevelFunction = case topLevelFunction of
  Optimized _ _ (TypedExp (Optimized _ _ (Assignment fnName (Optimized t _ (Abs paramName [body])))) _) -> do
    let returnType  = boxType --buildLLVMType (getType body)
    let paramType   = boxType --buildLLVMType $ InferredType.getParamType t
        returnType' = case body of
          Optimized _ _ (Closure n args) ->
            updateClosureType (getType <$> args) (buildLLVMType $ InferredType.getReturnType t)
          _ ->
            returnType
    f <- function (AST.mkName fnName) [(boxType, ParameterName (stringToShortByteString paramName))] returnType' $ \[param] -> do
      entry <- block `named` "entry"; do
        -- var <- alloca paramType Nothing 8
        -- store var 4 param
        var' <- unbox (InferredType.getParamType t) param
        (_, exps) <- generateExp (Map.insert paramName var' symbolTable) body
        exps' <- case body of
          Optimized _ _ Closure{} ->
            return exps
            --bitcast exps returnType'
          _ ->
            box exps
        ret exps'
    return $ Map.insert fnName f symbolTable

  Optimized _ _ (Assignment fnName (Optimized t _ (Abs paramName [body]))) -> do
    let returnType  = boxType --buildLLVMType (getType body)
    let paramType   = boxType --buildLLVMType $ InferredType.getParamType t
        -- returnType' = case body of
        --   Optimized _ _ (Closure n args) ->
        --     updateClosureType (getType <$> args) returnType
        --   _ ->
        --     returnType
    f <- function (AST.mkName fnName) [(boxType, ParameterName (stringToShortByteString paramName))] boxType $ \[param] -> do
      entry <- block `named` "entry"; do
        -- var <- alloca paramType Nothing 4
        -- store var 4 param
        var' <- unbox (InferredType.getParamType t) param
        (_, exps) <- generateExp (Map.insert paramName var' symbolTable) body
        exps' <- box exps
        ret exps'
    return $ Map.insert fnName f symbolTable

  Optimized t _ (ClosureDef fnName env paramName [body]) -> do
    let envParams = (\(Optimized t _ (Var n)) -> (boxType, ParameterName (stringToShortByteString n))) <$> env
        paramType  = buildLLVMType $ InferredType.getParamType t
        envParams' = envParams ++ [(boxType, ParameterName (stringToShortByteString paramName))]
        returnType = boxType
        allParamTypes = (getType <$> env) ++ [InferredType.getParamType t]
    f <- function (AST.mkName fnName) envParams' returnType $ \params -> do
      entry <- block `named` "entry"; do
        -- vars <- mapM (\p -> alloca boxType Nothing 4) params
        -- mapM_ (\(p, v) -> store v 4 p) (List.zip params vars)
        vars' <- Monad.zipWithM unbox allParamTypes params
        (_, exps) <- generateExp (Map.fromList (List.zip (getClosureParamNames env ++ [paramName]) vars')) body
        exps' <- box exps
        ret exps'
    return $ Map.insert fnName f symbolTable

  _ ->
    return symbolTable

getClosureParamNames :: [Exp] -> [String]
getClosureParamNames exps =
  (\(Optimized t _ (Var n)) -> n) <$> exps


generateTopLevelFunctions :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> [Exp] -> m SymbolTable
generateTopLevelFunctions symbolTable topLevelFunctions = case topLevelFunctions of
  (fn : fns) -> do
    symbolTable' <- generateTopLevelFunction symbolTable fn
    generateTopLevelFunctions symbolTable' fns

  [] ->
    return symbolTable




expsForMain :: [Exp] -> [Exp]
expsForMain =
  List.filter (not . \e -> isTopLevelFunction e || isClosureDef e)


topLevelFunctions :: [Exp] -> [Exp]
topLevelFunctions =
  List.filter $ \e -> isTopLevelFunction e || isClosureDef e


toLLVMModule :: AST -> AST.Module
toLLVMModule ast =
  buildModule "main" $ do
  symbolTable <- generateTopLevelFunctions Map.empty (topLevelFunctions $ aexps ast)

  extern (AST.mkName "puts") [ptr i8] i32
  extern (AST.mkName "malloc") [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "GC_malloc") [Type.i64] (Type.ptr Type.i8)
  extern (AST.mkName "calloc") [Type.i32, Type.i32] (Type.ptr Type.i8)
  extern (AST.mkName "__streq__") [ptr i8, ptr i8] i1

  function "main" [] void $ \_ -> mdo
    entry <- block `named` "entry";
    generateExps symbolTable (expsForMain $ aexps ast)
    retVoid


generate :: AST -> IO ()
generate ast = do
  Prelude.putStrLn "generate llvm"
  let mod = toLLVMModule ast

  T.putStrLn $ ppllvm mod

  withHostTargetMachineDefault $ \target -> do
    withContext $ \ctx -> do
      withModuleFromAST ctx mod $ \mod' ->
        writeObjectToFile target (File "module.o") mod'

  callCommand "clang++ -g -stdlib=libc++ -v module.o generate-llvm/lib.o gc.a -o a.out"
