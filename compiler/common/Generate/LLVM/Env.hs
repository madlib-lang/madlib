{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Generate.LLVM.Env where

import qualified Data.Map                        as Map
import qualified LLVM.AST.Operand                as Operand
import           LLVM.AST                        as AST hiding (function)
import           Data.Hashable
import           GHC.Generics hiding(Constructor)
import           Generate.LLVM.Hashable


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
  | ArithmeticRecursionData
      { entryBlockName :: AST.Name
      , continueRef :: Operand.Operand
      , boxedParams :: [Operand.Operand]
      , holePtr :: Operand.Operand
      }
  deriving(Eq, Show, Generic, Hashable)

data Env
  = Env
  { isLast :: Bool
  , isTopLevel :: Bool
  , recursionData :: Maybe RecursionData
  , envASTPath :: String
  , envCurrentCompilationUnitSymbolIndex :: Word
  , envCurrentFileSymbolIndex :: Word
  , envCurrentSubProgramSymbolIndex :: Word
  }
  deriving(Eq, Show, Generic, Hashable)

initialEnv :: Env
initialEnv =
  Env { isLast = True
      , isTopLevel = True
      , recursionData = Nothing
      , envASTPath = ""
      , envCurrentCompilationUnitSymbolIndex = 0
      , envCurrentFileSymbolIndex = 0
      , envCurrentSubProgramSymbolIndex = 0
      }
