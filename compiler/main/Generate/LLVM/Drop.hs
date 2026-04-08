{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE LambdaCase        #-}
{-|
Module      : Generate.LLVM.Drop
Description : Perceus drop specialization — per-type LLVM drop functions.

= Background

When an RC-managed value's refcount reaches zero, @rc_dec_with_drop@ calls a
per-type @drop@ function to recursively decrement child pointers before freeing
the struct.  Without drop specialization, children would leak.

= What this module generates

For each user-defined ADT in the module, it emits an LLVM function:

@
  define void @drop__<MangledTypeName>(i8* %ptr) { ... }
@

The function:

1. Casts @%ptr@ to the ADT struct type.
2. For multi-constructor ADTs, reads the tag (field 0) and switches.
3. For each constructor branch, reads each field.
4. If a field's Madlib type is RC-managed (not a primitive), calls
   @rc_dec(field)@.  Primitives (Integer, Float, Boolean, etc.) are encoded
   via @inttoptr@ and must NEVER be passed to @rc_dec@.
5. Returns void.

= Safety

- The drop function is only CALLED by @rc_dec_with_drop@ when the refcount
  reaches zero, which only happens under @MADLIB_USE_RC@.  Under GC mode
  @rc_dec_with_drop@ is a no-op macro so these functions are dead code.
- Primitive-boxed values (Integer, Float, Boolean, Byte, Short, Char, Unit,
  enum ADTs) are explicitly excluded from RC ops.
-}
module Generate.LLVM.Drop
  ( generateDropFunctions
  , dropFnForType
  , runtimeDropFnForType
  , isRCManagedType
  , nullDropFn
  ) where

import qualified Data.Map                     as Map
import qualified Data.List                    as List
import           Data.ByteString.Short        as SBS (ShortByteString, fromShort, toShort)
import           Data.ByteString.Char8        as BS8 (pack)
import           System.Random                (randomIO)
import qualified Control.Monad.Fix            as MonadFix
import           Control.Monad.IO.Class       (MonadIO, liftIO)

import           LLVM.AST                     as AST hiding (function)
import           LLVM.AST.Type                as Type
import qualified LLVM.AST.Constant            as Constant
import qualified LLVM.AST.Operand             as Operand
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction   as Instruction hiding (gep)

import qualified Infer.Type                   as IT
import           Generate.LLVM.SymbolTable
import           Generate.LLVM.Types          (boxType)
import           Generate.LLVM.Emit           (emitGEP)
import           Generate.LLVM.Builtins       (rcDec, rcDecWithDrop)
import qualified AST.Core                     as Core
import           Generate.LLVM.Function       (findMaximumConstructorArity)


-- | The LLVM function type for a drop function: @void(i8*)@
dropFnType :: Type.Type
dropFnType = Type.FunctionType Type.void [boxType] False

-- | A null drop function pointer (passed to rc_dec_with_drop when there are
-- no children to recurse into — e.g. for String, which is an atomic leaf).
nullDropFn :: Operand.Operand
nullDropFn =
  Operand.ConstantOperand $
    Constant.Null (Type.ptr dropFnType)


-- | Is this Madlib type RC-managed (heap-allocated with an RC header)?
-- Returns False for types that are encoded via @inttoptr@ (primitives and
-- enum ADTs), which must NEVER be passed to @rc_dec@.
isRCManagedType :: SymbolTable -> IT.Type -> Bool
isRCManagedType symbolTable t = case t of
  -- Primitive types: encoded as inttoptr, NOT heap-allocated
  IT.TCon (IT.TC "Integer"  _) _ _ -> False
  IT.TCon (IT.TC "Float"    _) _ _ -> False
  IT.TCon (IT.TC "Boolean"  _) _ _ -> False
  IT.TCon (IT.TC "Byte"     _) _ _ -> False
  IT.TCon (IT.TC "Short"    _) _ _ -> False
  IT.TCon (IT.TC "Char"     _) _ _ -> False
  IT.TCon (IT.TC "Unit"     _) _ _ -> False
  -- String, Array, ByteArray: heap-allocated but leaf/special — rc_dec handles them
  IT.TCon (IT.TC "String"   _) _ _ -> True
  IT.TCon (IT.TC "Array"    _) _ _ -> True
  IT.TCon (IT.TC "ByteArray"_) _ _ -> True
  -- Enum ADTs (all constructors zero-arity) are stored as plain i64
  IT.TCon (IT.TC name _) path _    ->
    let key = path <> "_" <> name
    in  case Map.lookup key symbolTable of
          Just (Symbol (ADTSymbol 0 _) _) -> False   -- enum
          _                               -> True    -- heap-allocated ADT
  -- TApp: parameterised ADT or List — always heap-allocated
  IT.TApp{} -> True
  -- Everything else: heap-allocated
  _         -> True


-- | Generate a reference to the drop function for a given type.
-- Returns Nothing if the type is an atomic leaf (String, primitive, enum) —
-- in that case @rc_dec@ with null drop is used.
-- Returns Just op for compound types whose children need recursive decrement.
dropFnForType :: SymbolTable -> IT.Type -> Maybe Operand.Operand
dropFnForType symbolTable t = case t of
  -- Primitives: no drop needed (inttoptr encoded)
  IT.TCon (IT.TC "Integer"  _) _ _ -> Nothing
  IT.TCon (IT.TC "Float"    _) _ _ -> Nothing
  IT.TCon (IT.TC "Boolean"  _) _ _ -> Nothing
  IT.TCon (IT.TC "Byte"     _) _ _ -> Nothing
  IT.TCon (IT.TC "Short"    _) _ _ -> Nothing
  IT.TCon (IT.TC "Char"     _) _ _ -> Nothing
  IT.TCon (IT.TC "Unit"     _) _ _ -> Nothing
  -- Atomic heap types: rc_dec with null drop suffices (no child pointers)
  IT.TCon (IT.TC "String"   _) _ _ -> Nothing
  -- Array: has a child items buffer — use runtime rc_drop_array
  IT.TCon (IT.TC "Array"    _) _ _ -> Just (externalDropFn "rc_drop_array")
  -- ByteArray: has a child bytes buffer — use runtime rc_drop_bytearray
  IT.TCon (IT.TC "ByteArray"_) _ _ -> Just (externalDropFn "rc_drop_bytearray")
  -- List (TApp): handled by runtime rc_drop_list
  IT.TApp (IT.TCon (IT.TC "List" _) _ _) _ ->
    Just (externalDropFn "rc_drop_list")
  -- Array (TApp form)
  IT.TApp (IT.TCon (IT.TC "Array" _) _ _) _ -> Just (externalDropFn "rc_drop_array")
  -- Function / PAP: handled by runtime rc_drop_pap
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" _) _ _) _) _ ->
    Just (externalDropFn "rc_drop_pap")
  -- General enum ADT check (before the user-ADT fallthrough)
  IT.TCon (IT.TC name _) path _ ->
    let key = path <> "_" <> name
    in  case Map.lookup key symbolTable of
          Just (Symbol (ADTSymbol 0 _) _) -> Nothing  -- enum, no drop
          _                               -> Just (externalDropFn (dropFnName path name))
  -- Parameterised user ADT — unwrap to get the base type name
  IT.TApp f _ -> dropFnForType symbolTable f
  -- Everything else: no children known, use rc_dec with null drop
  _ -> Nothing


-- | Like 'dropFnForType' but ONLY returns runtime-defined drop functions
-- (rc_drop_list, rc_drop_array, rc_drop_bytearray, rc_drop_pap).
-- Returns Nothing for user-defined ADTs to avoid referencing symbols that
-- may not be declared in the current module's LLVM IR.
-- Use this when generating ad-hoc rc_dec_with_drop calls (e.g. scope drops)
-- rather than inside generated drop functions (which have full linkage).
runtimeDropFnForType :: IT.Type -> Maybe Operand.Operand
runtimeDropFnForType t = case t of
  IT.TCon (IT.TC "Array"    _) _ _ -> Just (externalDropFn "rc_drop_array")
  IT.TCon (IT.TC "ByteArray"_) _ _ -> Just (externalDropFn "rc_drop_bytearray")
  IT.TApp (IT.TCon (IT.TC "List"  _) _ _) _ -> Just (externalDropFn "rc_drop_list")
  IT.TApp (IT.TCon (IT.TC "Array" _) _ _) _ -> Just (externalDropFn "rc_drop_array")
  IT.TApp (IT.TApp (IT.TCon (IT.TC "(->)" _) _ _) _) _ -> Just (externalDropFn "rc_drop_pap")
  IT.TApp f _  -> runtimeDropFnForType f
  _            -> Nothing


-- | Make a reference to an external (runtime or previously emitted) drop fn.
externalDropFn :: String -> Operand.Operand
externalDropFn name =
  Operand.ConstantOperand $
    Constant.GlobalReference
      (Type.ptr dropFnType)
      (AST.mkName name)


-- | Mangle a type path+name into a valid LLVM symbol name for a drop function.
dropFnName :: String -> String -> String
dropFnName path name =
  "drop__" <> mangleStr path <> "__" <> mangleStr name
  where
    mangleStr = map (\c -> if c `elem` ("./\\-" :: String) then '_' else c)


-- ---------------------------------------------------------------------------
-- Drop function generation for user-defined ADTs
-- ---------------------------------------------------------------------------

-- | Generate LLVM drop functions for all ADT type declarations in the module.
-- Called once per module after constructor generation.
generateDropFunctions
  :: (MonadIO m, MonadFix.MonadFix m, MonadModuleBuilder m)
  => SymbolTable         -- ^ Full module symbol table (with ADT symbols)
  -> [Core.TypeDecl]     -- ^ All type declarations in this module
  -> m ()
generateDropFunctions symbolTable typedecls =
  mapM_ (generateDropForADT symbolTable) typedecls


generateDropForADT
  :: (MonadIO m, MonadFix.MonadFix m, MonadModuleBuilder m)
  => SymbolTable
  -> Core.TypeDecl
  -> m ()
generateDropForADT symbolTable td = case td of
  Core.Untyped _ _ Core.ADT{ Core.adtname, Core.adtconstructors } -> do
    let maxArity  = findMaximumConstructorArity adtconstructors
        ctorCount = List.length adtconstructors

    -- Enum ADTs (maxArity == 0) are stored as i64 — no heap allocation, no drop
    if maxArity == 0
      then return ()
      else do
        let sortedCtors = List.sortBy (\a b -> compare (Core.getConstructorName a) (Core.getConstructorName b)) adtconstructors
        -- Extract the module path from the first constructor's return type
        let path = case sortedCtors of
              (Core.Untyped _ _ (Core.Constructor _ _ ctorT) : _) ->
                IT.getTConPath (IT.getReturnType ctorT)
              _ -> ""
        let fnName = dropFnName path adtname
        emitDropFunction symbolTable fnName maxArity ctorCount sortedCtors

  _ -> return ()


-- | Emit the LLVM drop function body.
emitDropFunction
  :: (MonadIO m, MonadFix.MonadFix m, MonadModuleBuilder m)
  => SymbolTable
  -> String              -- ^ LLVM function name
  -> Int                 -- ^ maxArity
  -> Int                 -- ^ ctorCount
  -> [Core.Constructor]  -- ^ constructors, sorted by name (= tag order)
  -> m ()
emitDropFunction symbolTable fnName maxArity ctorCount sortedCtors = do
  let isSingleCtor = ctorCount == 1
  let structType =
        if isSingleCtor
          then Type.StructureType False (List.replicate maxArity boxType)
          else Type.StructureType False (Type.i64 : List.replicate maxArity boxType)

  -- Declare any referenced cross-module drop functions as externals.
  -- We collect names of drop functions referenced in field types and emit
  -- `extern` declarations so the LLVM module can resolve them at link time.
  let allParamTypes = concatMap (\ctor -> case ctor of
        Core.Untyped _ _ (Core.Constructor _ _ ct) -> IT.getParamTypes ct
        _ -> []) sortedCtors
  let referencedExterns = List.nub
        [ nm
        | t   <- allParamTypes
        , Just (Operand.ConstantOperand (Constant.GlobalReference _ (AST.Name nm)))
                <- [dropFnForType symbolTable t]
        ]
  mapM_ (\nm ->
    extern (AST.Name nm) [Type.ptr Type.i8] Type.void
    ) referencedExterns

  _ <- function (AST.mkName fnName) [(boxType, NoParameterName)] Type.void $ \[ptr] ->
    if isSingleCtor
      then do
        _ <- block `named` "entry"
        let (Core.Untyped _ _ (Core.Constructor _ _ ctorType)) = List.head sortedCtors
            paramTypes = IT.getParamTypes ctorType
        structPtr <- bitcast ptr (Type.ptr structType)
        emitFieldDrops symbolTable structPtr 0 paramTypes
        retVoid
      else mdo
        -- Entry block: read tag, switch
        _ <- block `named` "entry"
        structPtr <- bitcast ptr (Type.ptr structType)
        tagPtr    <- emitGEP structPtr [int32c 0, int32c 0]
        tag       <- load tagPtr 0

        let indexedCtors = List.zip sortedCtors [0..]

        -- switch uses forward block references — resolved by mdo
        switch tag defaultBlock
          [ (Constant.Int 64 (fromIntegral i), blk)
          | ((_, i), blk) <- List.zip indexedCtors ctorBlockNames
          ]

        -- Emit each constructor's drop block; collect the block names
        ctorBlockNames <- mapM (\(ctor, _idx) -> do
          blk <- block `named` "dropCtor"
          let (Core.Untyped _ _ (Core.Constructor _ _ ctorType)) = ctor
              paramTypes = IT.getParamTypes ctorType
          -- Fields start at index 1 (index 0 is the tag)
          emitFieldDrops symbolTable structPtr 1 paramTypes
          retVoid
          return blk
          ) indexedCtors

        -- Default block (unreachable — all tags are covered)
        defaultBlock <- block `named` "dropDefault"
        unreachable

  return ()
  where
    int32c n = Operand.ConstantOperand (Constant.Int 32 n)


-- | Emit rc_dec calls for struct fields starting at field index @startIdx@.
emitFieldDrops
  :: (MonadIRBuilder m, MonadModuleBuilder m)
  => SymbolTable
  -> Operand.Operand   -- ^ Struct pointer (already cast)
  -> Integer           -- ^ Starting field index
  -> [IT.Type]         -- ^ Field types (in order)
  -> m ()
emitFieldDrops symbolTable structPtr startIdx fieldTypes =
  mapM_ dropField (List.zip fieldTypes [startIdx..])
  where
    int32c n = Operand.ConstantOperand (Constant.Int 32 n)
    dropField (fieldType, idx) =
      if isRCManagedType symbolTable fieldType
        then do
          fieldPtr <- emitGEP structPtr [int32c 0, int32c (fromIntegral idx)]
          fieldVal <- load fieldPtr 0
          case dropFnForType symbolTable fieldType of
            Nothing    -> call rcDec [(fieldVal, [])] >> return ()
            Just dropF -> call rcDecWithDrop [(fieldVal, []), (dropF, [])] >> return ()
        else
          return ()
