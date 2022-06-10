module Generate.LLVM.Env where

import qualified Data.Map                        as Map
import qualified LLVM.AST.Operand                as Operand
import           LLVM.AST                        as AST hiding (function)


data RecursionData
  = PlainRecursionData
      { entryBlockName :: AST.Name
      , continueRef :: Operand.Operand
      , boxedParams :: [Operand.Operand]
      }
  | RightListRecursionData
      { entryBlockName :: AST.Name
      , continueRef :: Operand.Operand
      , boxedParams :: [Operand.Operand]
      , start :: Operand.Operand
      , end :: Operand.Operand
      }
  | ConstructorRecursionData
      { entryBlockName :: AST.Name
      , continueRef :: Operand.Operand
      , boxedParams :: [Operand.Operand]
      , start :: Operand.Operand
      , end :: Operand.Operand
      , holePtr :: Operand.Operand
      }
  deriving(Eq, Show)

data Env
  = Env
  { dictionaryIndices :: Map.Map String (Map.Map String (Int, Int))
    -- ^ Map InterfaceName (Map MethodName (index, arity))
  , isLast :: Bool
  , isTopLevel :: Bool
  , recursionData :: Maybe RecursionData
  , envASTPath :: String
  }
  deriving(Eq, Show)

initialEnv :: Env
initialEnv =
  Env { dictionaryIndices = mempty
      , isLast = True
      , isTopLevel = True
      , recursionData = Nothing
      , envASTPath = ""
      }
