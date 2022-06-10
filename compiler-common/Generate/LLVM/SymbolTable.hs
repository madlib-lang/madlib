module Generate.LLVM.SymbolTable where
import qualified LLVM.AST.Operand                as Operand
import qualified Data.Map                        as Map



data SymbolType
  = VariableSymbol
  | LocalVariableSymbol Operand.Operand
  -- ^ operand is a ptr to the value for mutation
  | TCOParamSymbol Operand.Operand
  -- ^ operand is a ptr to the value for mutation
  | FunctionSymbol Int
  -- ^ arity
  | MethodSymbol Int
  -- ^ arity
  | ConstructorSymbol Int Int
  -- ^ unique id ( index ) | arity
  | ADTSymbol Int
  -- ^ maximum amount of params that a constructor of that type needs
  -- this will be used to allocate or dereference structs for any
  -- constructor of that type
  | TopLevelAssignment
  -- ^ amount of items in the env
  | DictionarySymbol (Map.Map String Int) -- <- index of the method for each name in the dict
  deriving(Eq, Show)


data Symbol
  = Symbol SymbolType Operand.Operand
  deriving(Eq, Show)


type SymbolTable
  = Map.Map String Symbol
