{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Generate.LLVM.PatternMatch
  ( PatternCtx(..)
  , GenerateExpFn
  , generateBranches
  , generateBranch
  , generateBranchTest
  , generateSymbolTableForPattern
  , generateSymbolTableForIndexedData
  , generateSymbolTableForList
  , generateSubPatternTest
  , generateListSubPatternTest
  , getFieldPattern
  , getStructPointers
  ) where

import qualified Data.Map                     as Map
import qualified Data.List                    as List
import qualified Data.Tuple                   as Tuple
import qualified Control.Monad                as Monad
import qualified Control.Monad.Fix            as MonadFix
import qualified Control.Monad.Writer         as Writer
import qualified Control.Monad.State          as State

import           LLVM.AST                     as AST hiding (function)
import           LLVM.AST.Type                as Type
import           LLVM.AST.AddrSpace           (AddrSpace(..))
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand
import qualified LLVM.AST.IntegerPredicate    as IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate as FloatingPointPredicate

import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Module        (MonadModuleBuilder)
import           LLVM.IRBuilder.Instruction   as Instruction hiding (gep)
import           Generate.LLVM.Emit          (emitGEP)
import           LLVM.IRBuilder.Constant      as C

import           AST.Core                     as Core
import qualified Infer.Type                   as IT
import           Explain.Location             (Area)
import           Data.ByteString.Short        (ShortByteString)

import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Env
import           Generate.LLVM.Types          (boxType, listType, stringType, recordType, sizeof')
import           Generate.LLVM.Builtins       (i32ConstOp, i64ConstOp, true, areStringsEqual, madlistHasMinLength, madlistHasLength, selectField, buildRecord, gcMalloc)
import           Generate.LLVM.Boxing         (unbox)
import           Generate.LLVM.Debug          (makeDILocation)
import           Generate.LLVM.WithMetadata   (callWithMetadata, callMallocWithMetadata)
import           Generate.LLVM.TypeOf         (Typed(typeOf))

import           Control.Monad.IO.Class       (MonadIO)
import           GHC.Stack (HasCallStack)

-- | Inbounds GEP — shadows LLVM.IRBuilder.Instruction.gep
gep :: (HasCallStack, MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
gep = emitGEP


-- | Type alias for the generateExp callback from LLVM.hs
type GenerateExpFn m = Env -> SymbolTable -> Core.Exp -> m (SymbolTable, Operand, Maybe Operand)

-- | Type alias for the buildStr callback from LLVM.hs
type BuildStrFn m = Env -> Area -> String -> m Operand

-- | Type alias for the safeBitcast function
type SafeBitcastFn m = Operand -> Type -> m Operand

-- | Type alias for the storeItem function
type StoreItemFn m = Operand -> () -> (Operand, Integer) -> m ()

-- | Bundle of callback functions needed by pattern matching
data PatternCtx m = PatternCtx
  { ctxGenerateExp :: GenerateExpFn m
  , ctxBuildStr    :: BuildStrFn m
  , ctxSafeBitcast :: SafeBitcastFn m
  , ctxStoreItem   :: StoreItemFn m
  , ctxAddrSpaceCast :: Operand -> Type -> m Operand
  }


generateBranches :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
                 => PatternCtx m -> Env -> SymbolTable -> AST.Name -> Operand -> [Core.Is] -> m [(Operand, AST.Name)]
generateBranches ctx env symbolTable exitBlock whereExp iss = case iss of
  (is : next) -> do
    branch <- generateBranch ctx env symbolTable (not (List.null next)) exitBlock whereExp is
    next'  <- generateBranches ctx env symbolTable exitBlock whereExp next
    return $ branch ++ next'

  [] ->
    return []


generateBranch :: (MonadIO m, Writer.MonadWriter SymbolTable m, State.MonadState Int m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
               => PatternCtx m -> Env -> SymbolTable -> Bool -> AST.Name -> Operand -> Core.Is -> m [(Operand, AST.Name)]
generateBranch ctx env symbolTable hasMore exitBlock whereExp is = case is of
  Core.Typed _ _ _ (Core.Is pat exp) -> mdo
    test      <- generateBranchTest ctx env symbolTable pat whereExp
    currBlock <- currentBlock
    condBr test branchExpBlock nextBlock

    branchExpBlock <- block `named` "branchExpBlock"
    symbolTable' <- generateSymbolTableForPattern ctx env symbolTable whereExp pat
    (_, branchResult, _) <- ctxGenerateExp ctx env symbolTable' exp
    branchResult' <- return branchResult
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
    error "Unreachable: generateBranch called with invalid branch expression"


generateSymbolTableForIndexedData :: (MonadIO m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
                                  => PatternCtx m -> Env -> Operand -> SymbolTable -> (Core.Pattern, Integer) -> m SymbolTable
generateSymbolTableForIndexedData ctx env basePtr symbolTable (pat, index) = do
  ptr   <- gep basePtr [i32ConstOp 0, i32ConstOp index]
  ptr'  <- load ptr 0
  ptr'' <- unbox env symbolTable (getQualType pat) ptr'
  generateSymbolTableForPattern ctx env symbolTable ptr'' pat


generateSymbolTableForList :: (MonadIO m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
                           => PatternCtx m -> Env -> SymbolTable -> Operand -> [Core.Pattern] -> m SymbolTable
generateSymbolTableForList ctx env symbolTable basePtr pats = case pats of
  (pat : next) -> case pat of
    Core.Typed _ _ _ (Core.PSpread spread) ->
      generateSymbolTableForPattern ctx env symbolTable basePtr spread

    _ -> do
      valuePtr      <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
      valuePtr'     <- load valuePtr 0
      valuePtr''    <- unbox env symbolTable (getQualType pat) valuePtr'
      symbolTable'  <- generateSymbolTableForPattern ctx env symbolTable valuePtr'' pat
      nextNodePtr   <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
      nextNodePtr'  <- load nextNodePtr 0
      nextNodePtr'' <- ctxAddrSpaceCast ctx nextNodePtr' listType
      generateSymbolTableForList ctx env symbolTable' nextNodePtr'' next

  [] ->
    return symbolTable


generateSymbolTableForPattern :: (MonadIO m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
                              => PatternCtx m -> Env -> SymbolTable -> Operand -> Core.Pattern -> m SymbolTable
generateSymbolTableForPattern ctx env symbolTable baseExp pat = case pat of
  Core.Typed _ _ _ (Core.PVar n) -> do
    return $ Map.insert n (Symbol VariableSymbol baseExp) symbolTable

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
    Monad.foldM (generateSymbolTableForIndexedData ctx env baseExp) symbolTable patsWithIds

  Core.Typed _ _ _ (Core.PList pats) ->
    generateSymbolTableForList ctx env symbolTable baseExp pats

  Core.Typed _ _ _ (Core.PCon _ pats) -> do
    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- ctxSafeBitcast ctx baseExp constructorType
    let patsWithIds = List.zip pats [1..]
    Monad.foldM (generateSymbolTableForIndexedData ctx env constructor') symbolTable patsWithIds

  Core.Typed qt _ _ (Core.PRecord fieldPatterns restName) -> do
    subPatterns <- mapM (getFieldPattern ctx env symbolTable baseExp) $ Map.toList fieldPatterns
    symbolTable' <- Monad.foldM (\previousTable (field, pat') -> generateSymbolTableForPattern ctx env previousTable field pat') symbolTable subPatterns

    -- Handle rest pattern: create a record with all fields except the matched ones
    case restName of
      Just restVarName -> do
        let recordType' = Core.getType pat
        case recordType' of
          IT.TRecord allFields _ _ -> do
            let matchedFieldNames = Map.keys fieldPatterns
            let restFieldNames = List.filter (`List.notElem` matchedFieldNames) (Map.keys allFields)

            -- Build a new record with only the rest fields
            restFields <- mapM (\fieldName -> do
                let area = Core.getArea pat
                nameOperand <- ctxBuildStr ctx env area fieldName
                fieldValue <- callWithMetadata (makeDILocation env area) selectField [(nameOperand, []), (baseExp, [])]

                let fieldType = Type.StructureType False [stringType, boxType]
                fieldPtr <- callMallocWithMetadata (makeDILocation env area) gcMalloc [(Operand.ConstantOperand $ sizeof' fieldType, [])]
                fieldPtr' <- ctxSafeBitcast ctx fieldPtr (Type.ptr fieldType)
                ctxStoreItem ctx fieldPtr' () (nameOperand, 0)
                ctxStoreItem ctx fieldPtr' () (fieldValue, 1)
                return fieldPtr'
              ) restFieldNames

            let fieldCount = i32ConstOp (fromIntegral $ List.length restFields)
            let base = Operand.ConstantOperand (Constant.Null boxType)
            restRecord <- callWithMetadata (makeDILocation env (Core.getArea pat)) buildRecord $ [(fieldCount, []), (base, [])] ++ ((,[]) <$> restFields)
            return $ Map.insert restVarName (Symbol VariableSymbol restRecord) symbolTable'

          _ ->
            return $ Map.insert restVarName (Symbol VariableSymbol baseExp) symbolTable'

      Nothing ->
        return symbolTable'

  _ ->
    error "Unreachable: generateSymbolTableForPattern called with invalid pattern"


generateSubPatternTest :: (MonadIO m, MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m)
                       => PatternCtx m -> Env -> SymbolTable -> Operand -> (Core.Pattern, Operand) -> m Operand
generateSubPatternTest ctx env symbolTable prev (pat', ptr) = do
  v <- load ptr 0
  v' <- unbox env symbolTable (getQualType pat') v
  curr <- generateBranchTest ctx env symbolTable pat' v'
  prev `Instruction.and` curr


generateListSubPatternTest :: (MonadIO m, MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m)
                           => PatternCtx m -> Env -> SymbolTable -> Operand -> [Core.Pattern] -> m Operand
generateListSubPatternTest ctx env symbolTable basePtr pats = case pats of
  (pat : next) -> case pat of
    Core.Typed _ _ _ (Core.PSpread _) ->
      return true

    _ -> do
      valuePtr      <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 0)]
      valuePtr'     <- load valuePtr 0
      valuePtr''    <- unbox env symbolTable (getQualType pat) valuePtr'
      test          <- generateBranchTest ctx env symbolTable pat valuePtr''
      nextNodePtr   <- gep basePtr [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
      nextNodePtr'  <- load nextNodePtr 0
      nextNodePtr'' <- ctxAddrSpaceCast ctx nextNodePtr' listType
      nextTest      <- generateListSubPatternTest ctx env symbolTable nextNodePtr'' next
      test `Instruction.and` nextTest

  [] ->
    return true


generateBranchTest :: (MonadIO m, MonadFix.MonadFix m, MonadIRBuilder m, MonadModuleBuilder m)
                   => PatternCtx m -> Env -> SymbolTable -> Core.Pattern -> Operand -> m Operand
generateBranchTest ctx env symbolTable pat value = case pat of
  Core.Typed (_ IT.:=> t) _ _ (Core.PNum n) -> case t of
    IT.TCon (IT.TC "Byte" IT.Star) "prelude" _ ->
      icmp IntegerPredicate.EQ (C.int8 (read n)) value

    IT.TCon (IT.TC "Short" IT.Star) "prelude" _ ->
      icmp IntegerPredicate.EQ (C.int32 (read n)) value

    IT.TCon (IT.TC "Integer" IT.Star) "prelude" _ ->
      icmp IntegerPredicate.EQ (C.int64 (read n)) value

    IT.TCon (IT.TC "Float" IT.Star) "prelude" _ ->
      fcmp FloatingPointPredicate.OEQ (C.double (read n)) value

    _ ->
      icmp IntegerPredicate.EQ (C.int64 (read n)) value

  Core.Typed _ _ _ (Core.PBool "true") ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 1) value

  Core.Typed _ _ _ (Core.PBool _) ->
    icmp IntegerPredicate.EQ (Operand.ConstantOperand $ Constant.Int 1 0) value

  Core.Typed _ area _ (Core.PStr s) -> do
    s' <- ctxBuildStr ctx env area s
    callWithMetadata (makeDILocation env area) areStringsEqual [(s', []), (value, [])]

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
    Monad.foldM (generateSubPatternTest ctx env symbolTable) true patsWithPtrs

  Core.Typed _ area _ (Core.PList pats) -> mdo
    let hasSpread = List.any isSpread pats

    currentBlock
    lengthTest <-
      if hasSpread then do
        let minLengthToFind = List.length pats - 1
        if minLengthToFind == 1 then do
          nextPtr <- gep value [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          nextPtr' <- load nextPtr 0
          icmp IntegerPredicate.NE nextPtr' (Operand.ConstantOperand (Constant.Null boxType))
        else if minLengthToFind == 2 then mdo
          br length1Block
          length1Block <- block `named` "length1Block"
          nextPtr <- gep value [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          nextPtr' <- load nextPtr 0
          hasLength1 <- icmp IntegerPredicate.NE nextPtr' (Operand.ConstantOperand (Constant.Null boxType))
          condBr hasLength1 length2Block lengthResultBlock

          length2Block <- block `named` "length2Block"
          nextPtr'' <- ctxSafeBitcast ctx nextPtr' listType
          secondItemPtr <- gep nextPtr'' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          secondItemPtr' <- load secondItemPtr 0
          hasLength2 <- icmp IntegerPredicate.NE secondItemPtr' (Operand.ConstantOperand (Constant.Null boxType))
          br lengthResultBlock

          lengthResultBlock <- block `named` "lengthResultBlock"
          phi [(hasLength1, length1Block), (hasLength2, length2Block)]
        else
          callWithMetadata (makeDILocation env area) madlistHasMinLength [(C.int64 (fromIntegral $ List.length pats - 1), []), (value, [])]
      else
        if List.null pats then do
          nextPtr <- gep value [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          nextPtr' <- load nextPtr 0
          icmp IntegerPredicate.EQ nextPtr' (Operand.ConstantOperand (Constant.Null boxType))
        else if List.length pats == 1 then mdo
          br length1Block
          length1Block <- block `named` "length1Block"
          nextPtr <- gep value [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          nextPtr' <- load nextPtr 0
          hasLength1 <- icmp IntegerPredicate.NE nextPtr' (Operand.ConstantOperand (Constant.Null boxType))
          condBr hasLength1 notLength2Block lengthResultBlock

          notLength2Block <- block `named` "notLength2Block"
          nextPtr'' <- ctxSafeBitcast ctx nextPtr' listType
          secondItemPtr <- gep nextPtr'' [Operand.ConstantOperand (Constant.Int 32 0), Operand.ConstantOperand (Constant.Int 32 1)]
          secondItemPtr' <- load secondItemPtr 0
          hasNotLength2 <- icmp IntegerPredicate.EQ secondItemPtr' (Operand.ConstantOperand (Constant.Null boxType))
          br lengthResultBlock

          lengthResultBlock <- block `named` "lengthResultBlock"
          phi [(hasLength1, length1Block), (hasNotLength2, notLength2Block)]
        else
          callWithMetadata (makeDILocation env area) madlistHasLength [(C.int64 (fromIntegral $ List.length pats), []), (value, [])]

    lengthTestBlock'' <- currentBlock
    condBr lengthTest subPatternTestBlock testResultBlock

    subPatternTestBlock <- block `named` "subPatternTestBlock"
    subPatternsTest <- generateListSubPatternTest ctx env symbolTable value pats
    subPatternTestBlock' <- currentBlock
    br testResultBlock

    testResultBlock <- block `named` "testResultBlock"
    phi [(subPatternsTest, subPatternTestBlock'), (lengthTest, lengthTestBlock'')]

    where
      isSpread :: Core.Pattern -> Bool
      isSpread p = case p of
        Core.Typed _ _ _ Core.PSpread{} ->
          True

        _ ->
          False

  Core.Typed _ _ _ (Core.PRecord fieldPatterns restName) -> do
    subPatterns <- mapM (getFieldPattern ctx env symbolTable value) $ Map.toList fieldPatterns
    subTests    <- mapM (uncurry (generateBranchTest ctx env symbolTable) . Tuple.swap) subPatterns
    Monad.foldM Instruction.and (List.head subTests) (List.tail subTests)

  Core.Typed _ _ _ (Core.PCon name pats) -> mdo
    currentBlock
    br idTestBlock
    idTestBlock <- block `named` "idTestBlock"
    let constructorId = case Map.lookup name symbolTable of
          Just (Symbol (ConstructorSymbol id _) _) ->
            i64ConstOp $ fromIntegral id

          _ ->
            error $ "Core.Constructor '" <> name <> "' not found!"

    let constructorType = Type.ptr $ Type.StructureType False (Type.IntegerType 64 : (boxType <$ List.take (List.length pats) [0..]))
    constructor' <- ctxSafeBitcast ctx value constructorType

    id  <- gep constructor' [i32ConstOp 0, i32ConstOp 0]
    id' <- load id 0
    idTest <- icmp IntegerPredicate.EQ constructorId id'
    condBr idTest subPatternsTestBlock testResultBlock

    subPatternsTestBlock <- block `named` "subPatternsTestBlock"
    let argIds = fromIntegral <$> List.take (List.length pats) [1..]
    constructorArgPtrs <- getStructPointers argIds constructor'
    let patsWithPtrs = List.zip pats constructorArgPtrs
    subPatternsTest <- Monad.foldM (generateSubPatternTest ctx env symbolTable) true patsWithPtrs
    subPatternsTestBlock' <- currentBlock
    br testResultBlock

    testResultBlock <- block `named` "testResultBlock"
    phi [(idTest, idTestBlock), (subPatternsTest, subPatternsTestBlock')]

  _ ->
    error "Unreachable: generateBranchTest called with invalid pattern"


getFieldPattern :: (MonadIO m, MonadIRBuilder m, MonadFix.MonadFix m, MonadModuleBuilder m)
               => PatternCtx m -> Env -> SymbolTable -> Operand -> (String, Core.Pattern) -> m (Operand, Core.Pattern)
getFieldPattern ctx env symbolTable record (fieldName, fieldPattern) = do
  let area = Core.getArea fieldPattern
  nameOperand <- ctxBuildStr ctx env area fieldName
  field       <- callWithMetadata (makeDILocation env area) selectField [(nameOperand, []), (record, [])]
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
