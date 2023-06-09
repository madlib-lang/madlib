{-# LANGUAGE DisambiguateRecordFields #-}
module Generate.LLVM.Debug where
import LLVM.AST
import qualified LLVM.AST.Operand as Operand
import qualified System.FilePath as FilePath
import Generate.LLVM.Helper
import Generate.LLVM.Env
import Explain.Location
import Data.ByteString.Short (ShortByteString)
import qualified Data.List as List


makeCompileUnitMetadata :: Word -> Word -> Definition
makeCompileUnitMetadata diFileIndex index =
  MetadataNodeDefinition (MetadataNodeID index) $
    DINode . Operand.DIScope . Operand.DICompileUnit $
    Operand.CompileUnit
      { Operand.language = 12
      , Operand.file = MDRef (MetadataNodeID diFileIndex)
      , Operand.producer = stringToShortByteString "Homebrew clang version 12.0.1"
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
      , Operand.nameTableKind = Operand.NameTableKindNone
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


makeDILocation :: Env -> Area -> [(ShortByteString, MDRef MDNode)]
makeDILocation env area =
  if envIsDebugBuild env then
    let diSubprogramIndex = envCurrentSubProgramSymbolIndex env
        metadata = MDInline $
          DILocation Operand.Location
            { Operand.line = fromIntegral $ getLineFromStart area
            , Operand.column = fromIntegral $ getColumnFromStart area
            , scope = MDRef (MetadataNodeID diSubprogramIndex)
            }
    in  [(stringToShortByteString "dbg", metadata)]
  else
    []


makeOriginalName :: String -> String
makeOriginalName globalName =
  let withoutHash = List.drop 36 globalName
  in  if '_' `List.notElem` withoutHash then
        withoutHash
      else
        (List.init . List.init . List.dropWhileEnd (/= '_')) withoutHash


makeDISubprogram :: Env -> Word -> String -> Definition
makeDISubprogram env index functionName =
  let diCUIndex = envCurrentCompilationUnitSymbolIndex env
      diFileIndex = envCurrentFileSymbolIndex env
      diSubprogram = MetadataNodeDefinition (MetadataNodeID index) $
        DINode . Operand.DIScope . Operand.DILocalScope . Operand.DISubprogram $
          Operand.Subprogram
            { scope = Nothing
            , name = stringToShortByteString (makeOriginalName functionName)
            , linkageName = stringToShortByteString ""
            , file = Just (MDRef (MetadataNodeID diFileIndex))
            , line = 0
            , type' = Just $ Operand.MDInline $ Operand.SubroutineType { flags = [], cc = 0, typeArray = [] }
            , localToUnit = True
            , definition = True
            , scopeLine = 0
            , containingType = Nothing
            , virtuality = Operand.NoVirtuality
            , virtualityIndex = 0
            , thisAdjustment = 0
            , flags = []
            , optimized = False
            , unit = Just (MDRef (MetadataNodeID diCUIndex))
            , Operand.templateParams = []
            , declaration = Nothing
            , retainedNodes = []
            , thrownTypes = []
            }
  in  diSubprogram
