{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Generate.LLVM where


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
import           LLVM.AST                    as AST hiding (function)
import           LLVM.AST.Type               as Type
import           LLVM.AST.AddrSpace          as AddrSpace
import           LLVM.AST.ParameterAttribute as ParameterAttribute
import           LLVM.AST.Typed
import qualified LLVM.AST.Float              as Float
import qualified LLVM.AST.Constant           as Constant
import qualified LLVM.AST.Operand            as Operand
import qualified LLVM.AST.IntegerPredicate   as IntegerPredicate

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.Context (withContext)
import AST.Optimized
import Text.Show.Pretty
import Debug.Trace
import qualified Data.String.Utils as List
import Foreign.C



type SymbolTable = Map.Map String Operand



nameFromStr :: String -> AST.Name
nameFromStr s = Name (ShortByteString.toShort $ Char8.pack s)

stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack


true :: Operand
true = Operand.ConstantOperand (Constant.Int 1 1)


generateExp :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m (SymbolTable, Operand)
generateExp symbolTable exp = case exp of
  Optimized _ _ (Var "puts") ->
    return (symbolTable, Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.i32 [ptr i8] False) (nameFromStr "puts")))

  Optimized _ _ (Var n) ->
    case Map.lookup n symbolTable of
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
    (symbolTable', f')    <- generateExp symbolTable f
    (symbolTable'', arg') <- generateExp symbolTable' arg
    res                   <- call f' [(arg', [])]
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
    addr <- call (Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType (Type.ptr Type.i8) [Type.i64] False) (nameFromStr "malloc"))) [(ConstantOperand (Constant.Int 64 (fromIntegral $ List.length charCodes'')), [])]
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
      storeItem :: (MonadIRBuilder m, MonadModuleBuilder m) =>  Operand -> () -> (Operand, Integer) -> m ()
      storeItem basePtr _ (item, index) = do
        ptr <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 index)]
        store ptr 8 item
        return ()

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


  Optimized _ _ (Where exp iss) -> do
    (symbolTable', exp') <- generateExp symbolTable exp
    generateBranch symbolTable' exp' (List.head iss)

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

  Optimized _ _ (PTuple pats) -> do
    let patsWithIds = List.zip pats [0..]
    Monad.foldM (generateSymbolTableForIndexedData baseExp) symbolTable patsWithIds

  _ ->
    undefined


generateBranch :: (MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Operand -> Is -> m (SymbolTable, Operand)
generateBranch symbolTable whereExp is = case is of
  Optimized _ _ (Is pat exp) -> do
    symbolTable' <- generateSymbolTableForPattern symbolTable whereExp pat
    generateExp symbolTable' exp

  _ ->
    undefined

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


generateTopLevelFunction :: (MonadFix.MonadFix m, MonadModuleBuilder m) => SymbolTable -> Exp -> m SymbolTable
generateTopLevelFunction symbolTable topLevelFunction = case topLevelFunction of
  Optimized _ _ (Assignment fnName (Optimized _ _ (Abs paramName [body]))) -> do
    f <- function (nameFromStr fnName) [(Type.double, ParameterName (stringToShortByteString paramName))] Type.double $ \[param] -> mdo
      entry <- block `named` "entry"; do
        var <- alloca Type.double Nothing 4
        store var 4 param
        (_, exps) <- generateExp (Map.singleton paramName var) body
        ret exps
    let f' = Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.double [Type.double] False) (nameFromStr fnName))
    return $ Map.insert fnName f' symbolTable

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
  List.filter (not . isTopLevelFunction)


topLevelFunctions :: [Exp] -> [Exp]
topLevelFunctions =
  List.filter isTopLevelFunction


toLLVMModule :: AST -> AST.Module
toLLVMModule ast =
  buildModule "main" $ mdo
  symbolTable <- generateTopLevelFunctions Map.empty (topLevelFunctions $ aexps ast)

  extern (nameFromStr "puts") [ptr i8] i32
  extern (nameFromStr "malloc") [Type.i64] (Type.ptr Type.i8)

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

  callCommand "clang++ -stdlib=libc++ -v module.o generate-llvm/tuple.o -o a.out"
