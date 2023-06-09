{-# LANGUAGE DisambiguateRecordFields #-}
module Generate.LLVM.Debug where
import LLVM.AST
import qualified LLVM.AST.Operand as Operand
import qualified System.FilePath as FilePath
import Generate.LLVM.Helper
import Generate.LLVM.Env
import Explain.Location
import Data.ByteString.Short (ShortByteString)


makeCompileUnitMetadata :: Word -> Word -> Definition
makeCompileUnitMetadata diFileIndex index =
  MetadataNodeDefinition (MetadataNodeID index) $
    DINode . Operand.DIScope . Operand.DICompileUnit $
    Operand.CompileUnit
      { Operand.language = 12
      , Operand.file = MDRef (MetadataNodeID diFileIndex)
      , Operand.producer = stringToShortByteString "clang version 6.0.0 (tags/RELEASE_600/final)"
      , Operand.optimized = False
      , Operand.flags = stringToShortByteString ""
      , Operand.runtimeVersion = 0
      , Operand.splitDebugFileName = stringToShortByteString ""
      , Operand.emissionKind = Operand.FullDebug
      , Operand.enums = []
      , Operand.retainedTypes = []
      , Operand.globals = []
      , Operand.imports = []
      , Operand.macros = []
      , Operand.dWOId = 0
      , Operand.splitDebugInlining = False
      , Operand.debugInfoForProfiling = False
      , Operand.nameTableKind = Operand.NameTableKindDefault
      , Operand.debugBaseAddress = False
      }


makeFileMetadata :: Word -> FilePath -> Definition
makeFileMetadata index astPath =
  MetadataNodeDefinition
    (MetadataNodeID index)
    (DINode
      (Operand.DIScope
        (Operand.DIFile
          (Operand.File
            (stringToShortByteString $ FilePath.takeFileName astPath)
            (stringToShortByteString $ FilePath.takeDirectory astPath)
            Nothing)
        )
      )
    )


makeDILocation :: Env -> Area -> (ShortByteString, MDRef MDNode)
makeDILocation env area =
  let diSubprogramIndex = envCurrentSubProgramSymbolIndex env
      metadata = MDInline $
        DILocation Operand.Location
          { Operand.line = fromIntegral $ getLineFromStart area
          , Operand.column = fromIntegral $ getColumnFromStart area
          , scope = MDRef (MetadataNodeID diSubprogramIndex)
          }
  in  (stringToShortByteString "dbg", metadata)
