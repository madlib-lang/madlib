{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Generate.LLVM.Hashable where

import qualified LLVM.AST.Operand                as Operand
import qualified LLVM.AST.Type
import qualified LLVM.AST
import qualified LLVM.AST.DataLayout
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.FunctionAttribute
import qualified LLVM.AST.Visibility
import qualified LLVM.AST.COMDAT
import qualified LLVM.AST.DLL
import qualified LLVM.AST.ThreadLocalStorage
import qualified LLVM.AST.CallingConvention
import qualified LLVM.AST.ParameterAttribute
import qualified LLVM.AST.InlineAssembly
import qualified LLVM.AST.RMWOperation
import qualified LLVM.AST.Name
import qualified LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant
import qualified LLVM.AST.Float
import qualified LLVM.AST.IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate
import           Data.Hashable
import           GHC.Generics hiding(Constructor)


deriving instance Hashable LLVM.AST.Type.Type
deriving instance Hashable LLVM.AST.Name.Name
deriving instance Hashable LLVM.AST.AddrSpace.AddrSpace
deriving instance Hashable LLVM.AST.Type.FloatingPointType
deriving instance Hashable LLVM.AST.Constant.Constant
deriving instance Hashable LLVM.AST.Float.SomeFloat
deriving instance Hashable LLVM.AST.IntegerPredicate.IntegerPredicate
deriving instance Hashable LLVM.AST.FloatingPointPredicate.FloatingPointPredicate
deriving instance Hashable Operand.Metadata
deriving instance Hashable Operand.MetadataNodeID
deriving instance Hashable Operand.MDNode
deriving instance Hashable Operand.DIExpression
deriving instance Hashable Operand.DWOp
deriving instance Hashable Operand.DIGlobalVariableExpression
deriving instance Hashable Operand.DWOpFragment
deriving instance Hashable Operand.DILocation
deriving instance Hashable Operand.DIGlobalVariable
deriving instance Hashable Operand.DIMacroNode
deriving instance Hashable Operand.DILocalScope
deriving instance Hashable Operand.DINode
deriving instance Hashable Operand.DIFile
deriving instance Hashable Operand.DIScope
deriving instance Hashable Operand.DIEnumerator
deriving instance Hashable Operand.DILexicalBlockBase
deriving instance Hashable Operand.DIType
deriving instance Hashable Operand.DICompileUnit
deriving instance Hashable Operand.ChecksumInfo
deriving instance Hashable Operand.DIImportedEntity
deriving instance Hashable Operand.DISubprogram
deriving instance Hashable Operand.DIMacroInfo
deriving instance Hashable Operand.DIDerivedType
deriving instance Hashable Operand.ChecksumKind
deriving instance Hashable Operand.DIBasicType
deriving instance Hashable Operand.DIModule
deriving instance Hashable Operand.DISubroutineType
deriving instance Hashable Operand.Encoding
deriving instance Hashable Operand.DerivedTypeTag
deriving instance Hashable Operand.ImportedEntityTag
deriving instance Hashable Operand.DICompositeType
deriving instance Hashable Operand.DINamespace
deriving instance Hashable Operand.DIObjCProperty
deriving instance Hashable Operand.DITemplateParameter
deriving instance Hashable Operand.DIFlag
deriving instance Hashable Operand.BasicTypeTag
deriving instance Hashable Operand.DISubrange
deriving instance Hashable Operand.DICount
deriving instance Hashable Operand.DIAccessibility
deriving instance Hashable Operand.TemplateValueParameterTag
deriving instance Hashable Operand.DIVariable
deriving instance Hashable Operand.DILocalVariable
deriving instance Hashable Operand.DIInheritance
deriving instance Hashable Operand.Virtuality
deriving instance Hashable Operand.DebugEmissionKind
deriving instance Hashable Operand.DebugNameTableKind
deriving instance Hashable (Operand.MDRef Operand.DIVariable)
deriving instance Hashable (Operand.MDRef Operand.DIImportedEntity)
deriving instance Hashable (Operand.MDRef Operand.DILocalVariable)
deriving instance Hashable (Operand.MDRef Operand.DIGlobalVariableExpression)
deriving instance Hashable (Operand.MDRef Operand.DISubprogram)
deriving instance Hashable (Operand.MDRef (Either Operand.DIDerivedType Operand.DISubprogram))
deriving instance Hashable (Operand.MDRef Operand.DITemplateParameter)
deriving instance Hashable (Operand.MDRef Operand.DICompositeType)
deriving instance Hashable (Operand.MDRef Operand.DICompileUnit)
deriving instance Hashable (Operand.MDRef (Either Operand.DIType Operand.DISubprogram))
deriving instance Hashable (Operand.MDRef Operand.DINode)
deriving instance Hashable (Operand.MDRef Operand.DISubroutineType)
deriving instance Hashable (Operand.MDRef Operand.DIDerivedType)
deriving instance Hashable (Operand.MDRef Operand.DIType)
deriving instance Hashable (Operand.MDRef Operand.DIMacroNode)
deriving instance Hashable (Operand.MDRef Operand.DIScope)
deriving instance Hashable (Operand.MDRef Operand.DIFile)
deriving instance Hashable (Operand.MDRef Operand.DIExpression)
deriving instance Hashable (Operand.MDRef Operand.DILocalScope)
deriving instance Hashable (Operand.MDRef Operand.DIGlobalVariable)
deriving instance Hashable (Operand.MDRef Operand.MDNode)
deriving instance Hashable Operand.Operand
-- deriving instance Hashable LLVM.AST.Module
-- deriving instance Hashable LLVM.AST.DataLayout.DataLayout
-- deriving instance Hashable LLVM.AST.DataLayout.Endianness
-- deriving instance Hashable LLVM.AST.DataLayout.AlignmentInfo
-- deriving instance Hashable LLVM.AST.DataLayout.Mangling
-- deriving instance Hashable LLVM.AST.DataLayout.AlignType
-- deriving instance Hashable LLVM.AST.Linkage.Linkage
-- deriving instance Hashable LLVM.AST.FunctionAttribute.GroupID
-- deriving instance Hashable LLVM.AST.FunctionAttribute.FunctionAttribute
-- deriving instance Hashable LLVM.AST.Visibility.Visibility
-- deriving instance Hashable LLVM.AST.COMDAT.SelectionKind
-- deriving instance Hashable LLVM.AST.DLL.StorageClass
-- deriving instance Hashable LLVM.AST.ThreadLocalStorage.Model
-- deriving instance Hashable LLVM.AST.CallingConvention.CallingConvention
-- deriving instance Hashable LLVM.AST.ParameterAttribute.ParameterAttribute
-- deriving instance Hashable LLVM.AST.Parameter
-- deriving instance Hashable LLVM.AST.BasicBlock
-- deriving instance Hashable LLVM.AST.Terminator
-- deriving instance Hashable (LLVM.AST.Named LLVM.AST.Terminator)
-- deriving instance Hashable LLVM.AST.InlineAssembly.InlineAssembly
-- deriving instance Hashable LLVM.AST.Instruction
-- deriving instance Hashable (LLVM.AST.Named LLVM.AST.Instruction)
-- deriving instance Hashable LLVM.AST.FastMathFlags
-- deriving instance Hashable LLVM.AST.InlineAssembly.Dialect
-- deriving instance Hashable LLVM.AST.SynchronizationScope
-- deriving instance Hashable LLVM.AST.MemoryOrdering
-- deriving instance Hashable LLVM.AST.RMWOperation.RMWOperation
-- deriving instance Hashable LLVM.AST.UnnamedAddr
-- deriving instance Hashable LLVM.AST.Definition
-- deriving instance Hashable LLVM.AST.Global
-- deriving instance Hashable LLVM.AST.TailCallKind
-- deriving instance Hashable LLVM.AST.LandingPadClause
