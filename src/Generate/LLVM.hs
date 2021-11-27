{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Generate.LLVM where


import Data.Text.Lazy.IO as T
import           Data.ByteString.Short as ShortByteString
import qualified Data.Map   as Map
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import           Data.ByteString as ByteString
import           Data.ByteString.Char8 as Char8
import           System.Process

import           LLVM.Pretty
import           LLVM.Target
import           LLVM.Module 
import           LLVM.AST                    as AST hiding (function)
import           LLVM.AST.Type               as Type
import           LLVM.AST.ParameterAttribute as ParameterAttribute
import           LLVM.AST.Typed
import qualified LLVM.AST.Float              as Float
import qualified LLVM.AST.Constant           as Constant
import qualified LLVM.AST.Operand            as Operand

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.Context (withContext)
import AST.Optimized



simple :: IO ()
simple = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo
  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
    entry <- block `named` "entry"; do
      c <- add a b
      ret c

type SymbolTable = Map.Map String Operand



nameFromStr :: String -> AST.Name
nameFromStr s = Name (ShortByteString.toShort $ Char8.pack s)

stringToShortByteString :: String -> ShortByteString.ShortByteString
stringToShortByteString = ShortByteString.toShort . Char8.pack


generateExp :: (MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> Exp -> m (SymbolTable, Operand)
generateExp symbolTable exp = case exp of
  Optimized _ _ (Var n) ->
    case Map.lookup n symbolTable of
      Just global@(Operand.ConstantOperand Constant.GlobalReference{}) ->
        return (symbolTable, global)

      Just var -> do
        var' <- load var 4
        return (symbolTable, var')

      Nothing ->
        error $ "Var not found " <> n

  Optimized _ _ (Assignment name e) -> do
    (symbolTable', exp') <- generateExp symbolTable e
    var <- alloca (typeOf exp') Nothing 4
    store var 4 exp'
    return (Map.insert name var symbolTable', exp')

  Optimized _ _ (App f arg _) -> do
    (symbolTable', f') <- generateExp symbolTable f
    (symbolTable'', arg') <- generateExp symbolTable' arg
    res <- call f' [(arg', [])]
    return (symbolTable'', res)

  Optimized _ _ (LNum num) -> do
    return (symbolTable, C.double (read num))

  Optimized _ _ (LStr s) -> do
    s'  <- globalStringPtr s (nameFromStr "s")
    s'' <- load (ConstantOperand s') 4
    return (symbolTable, s'')

  _ ->
    undefined


generateExps :: (MonadIRBuilder m, MonadModuleBuilder m) => SymbolTable -> [Exp] -> m ()
generateExps symbolTable exps = case exps of
  [exp] -> do
    generateExp symbolTable exp
    return ()

  (exp : es) -> do
    (symbolTable', _) <- generateExp symbolTable exp
    generateExps symbolTable' es

  _ ->
    return ()


generateTopLevelFunction :: (MonadModuleBuilder m) => SymbolTable -> Exp -> m SymbolTable
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


generateTopLevelFunctions :: (MonadModuleBuilder m) => SymbolTable -> [Exp] -> m SymbolTable
generateTopLevelFunctions symbolTable topLevelFunctions = case topLevelFunctions of
  (fn : fns) -> do
    symbolTable' <- generateTopLevelFunction symbolTable fn
    generateTopLevelFunctions symbolTable' fns

  [] ->
    return symbolTable


expsForMain :: [Exp] -> [Exp]
expsForMain = List.filter (not . isTopLevelFunction)

topLevelFunctions :: [Exp] -> [Exp]
topLevelFunctions = List.filter isTopLevelFunction



toLLVMModule :: AST -> AST.Module
toLLVMModule ast =
  let
  in  buildModule "main" $ mdo
      symbolTable <- generateTopLevelFunctions Map.empty (topLevelFunctions $ aexps ast)

      function "main" [] void $ \_ -> mdo
        entry <- block `named` "entry"; do
          generateExps symbolTable (expsForMain $ aexps ast)
          retVoid


generate :: AST -> IO ()
generate ast = do
  Prelude.putStrLn "generate llvm"
  simple
--   let mod = generateModule
  let mod = toLLVMModule ast

  T.putStrLn $ ppllvm mod

  withHostTargetMachineDefault $ \target -> do
    withContext $ \ctx -> do
      withModuleFromAST ctx mod $ \mod' ->
        writeObjectToFile target (File "module.o") mod'

  callCommand "clang -v module.o -o a.out"
