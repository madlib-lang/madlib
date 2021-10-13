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
        var' <- load var 4
        return (symbolTable, var')

      Nothing ->
        error $ "Var not found " <> n

  Optimized _ _ (TypedExp (Optimized _ _ (Assignment "puts" e)) _) ->
    return (symbolTable, Operand.ConstantOperand $ Constant.Null i8)

  Optimized _ _ (Assignment name e) -> do
    (symbolTable', exp') <- generateExp symbolTable e
    var                  <- alloca (typeOf exp') Nothing 4
    store var 4 exp'
    return (Map.insert name var symbolTable', exp')

  Optimized _ _ (App f arg _) -> do
    (symbolTable', f')    <- generateExp symbolTable (trace ("F: "<>ppShow f) f)
    (symbolTable'', arg') <- generateExp symbolTable' arg
    case typeOf (trace (ppShow f') f') of
      Type.PointerType Type.FunctionType{} _ -> do
        res <- call f' [(arg', [])]
        return (symbolTable'', res)

      -- Closure call
      _ -> do
        fn <- gep f' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
        fn' <- load fn 8
        closuredArgs <- gep f' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
        closuredArgs' <- load closuredArgs 8
        let argCount = case typeOf (trace ("closuredArgs': "<>ppShow closuredArgs') closuredArgs') of
              Type.PointerType (Type.StructureType _ items) _ ->
                List.length items

              _ ->
                0
        firstArg <- gep (trace ("arg count: "<>show argCount) closuredArgs') [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
        firstArg' <- load firstArg 8
        closuredArgs'' <- mapM (\index -> gep closuredArgs' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)] >>= (`load` 8)) $ List.take argCount [0..]
        let closuredArgs''' = (, []) <$> closuredArgs''
        res <- call fn' (closuredArgs''' ++ [(arg', [])])
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
    addr <- call (Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (AST.mkName "malloc"))) [(ConstantOperand (Constant.Int 64 (fromIntegral $ List.length charCodes'')), [])]
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
    (_, exp')     <- generateExp symbolTable exp
    branches <- generateBranches symbolTable defaultBlock exitBlock exp' iss
    -- branch@(b, _) <- generateBranch symbolTable defaultBlock False exitBlock exp' (List.head iss)
    let (b, _) = List.head branches

    defaultBlock <- block `named` "defaultBlock"
    defaultRet <- return $ Operand.ConstantOperand (Constant.Null (typeOf b))
    br exitBlock

    exitBlock <- block `named` "exitBlock"
    ret <- phi $ branches <> [(defaultRet, defaultBlock)]

    return (symbolTable, ret)

  Optimized _ _ (Closure name env) -> do
    envItems <- mapM (generateExp symbolTable) env
    let envWithIds = List.zip (snd <$> envItems) [0..]
    env' <- alloca (Type.StructureType False (typeOf . snd <$> envItems)) Nothing 8
    Monad.foldM_ (storeItem (trace ("STORE: "<>ppShow env') env')) () envWithIds

    -- TODO: needs to be computed
    let fType = Type.ptr $ Type.FunctionType Type.double ((typeOf . snd <$> envItems) ++ [Type.double]) False
    closure <- alloca (Type.StructureType False [fType, typeOf env']) Nothing 8
    let f = ConstantOperand (Constant.GlobalReference fType (AST.mkName name))
    Monad.foldM_ (storeItem closure) () [(f, 0), (env', 1)]
    return (symbolTable, closure)

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
  ptr <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
  generateSymbolTableForPattern symbolTable ptr pat


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
    ptr' <- gep ptr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
    next <- getTuplePointers (List.tail ids) ptr
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


buildLLVMType :: Exp -> Type.Type
buildLLVMType (Optimized t area e) = case t of
  InferredType.TCon (InferredType.TC "Number" InferredType.Star) "prelude" ->
    Type.double

  InferredType.TCon (InferredType.TC "String" InferredType.Star) "prelude" ->
    Type.ptr Type.i8

  InferredType.TCon (InferredType.TC "Boolean" InferredType.Star) "prelude" ->
    Type.i1

  InferredType.TCon (InferredType.TC "()" InferredType.Star) "prelude" ->
    Type.i1

  InferredType.TApp (InferredType.TApp (InferredType.TCon (InferredType.TC "(->)" (InferredType.Kfun InferredType.Star (InferredType.Kfun InferredType.Star InferredType.Star))) "prelude") left) right ->
    case e of
      Closure name args ->
        let tLeft  = buildLLVMType (Optimized left area e)
            tRight = buildLLVMType (Optimized right area e)
            closureArgs = buildLLVMType <$> args
        in  Type.ptr $ Type.StructureType False [Type.ptr $ Type.FunctionType tRight (closureArgs ++ [tLeft]) False, Type.ptr $ Type.StructureType False closureArgs]

      _ ->
        let tLeft  = buildLLVMType (Optimized left area e)
            tRight = buildLLVMType (Optimized right area e)
        in  Type.ptr $ Type.FunctionType tRight [tLeft] False

  _ ->
    Type.double

generateTopLevelFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Exp -> m SymbolTable
generateTopLevelFunction symbolTable topLevelFunction = case topLevelFunction of
  Optimized _ _ (Assignment fnName (Optimized _ _ (Abs paramName [body]))) -> mdo
    let returnType = buildLLVMType body
    f <- function (AST.mkName fnName) [(Type.double, ParameterName (stringToShortByteString paramName))] returnType $ \[param] -> mdo
      entry <- block `named` "entry"; mdo
        var <- alloca Type.double Nothing 4
        store var 4 param
        (_, exps) <- generateExp (Map.singleton paramName var) body
        ret exps
    return $ Map.insert fnName f symbolTable

  Optimized _ _ (ClosureDef fnName env paramName [body]) -> do
    let envParams = (Type.double,) . ParameterName . stringToShortByteString <$> (env ++ [paramName])
    f <- function (AST.mkName fnName) envParams Type.double $ \params -> mdo
      entry <- block `named` "entry"; do
        vars <- mapM (\_ -> alloca Type.double Nothing 4) params
        mapM_ (\(p, v) -> store v 4 p) (List.zip params vars)
        (_, exps) <- generateExp (Map.fromList (List.zip (env ++ [paramName]) vars)) body
        ret exps
    -- let f' = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.double [Type.double] False) (AST.mkName fnName))
    return $ Map.insert fnName f symbolTable

  _ ->
    return symbolTable


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
  buildModule "main" $ mdo
  symbolTable <- generateTopLevelFunctions Map.empty (topLevelFunctions $ aexps ast)

  extern (AST.mkName "puts") [ptr i8] i32
  extern (AST.mkName "malloc") [Type.i64] (Type.ptr Type.i8)
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

  callCommand "clang++ -stdlib=libc++ -v module.o generate-llvm/lib.o -o a.out"
