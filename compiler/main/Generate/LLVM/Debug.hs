{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.LLVM.Debug where
import LLVM.AST
import qualified LLVM.AST.Operand as Operand
import qualified System.FilePath as FilePath
import Generate.LLVM.Helper
import Generate.LLVM.Env
import Explain.Location
import Data.ByteString.Short (ShortByteString)
import qualified Data.List as List
import Generate.LLVM.WithMetadata (callWithMetadata)
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST as AST
import qualified Control.Monad as Monad
import LLVM.IRBuilder (MonadIRBuilder, MonadModuleBuilder)
import qualified Data.Maybe as Maybe


llvmDbgDeclare :: Operand
llvmDbgDeclare =
  Operand.ConstantOperand (Constant.GlobalReference (Type.ptr $ Type.FunctionType Type.void [Type.MetadataType, Type.MetadataType, Type.MetadataType] False) (AST.mkName "llvm.dbg.declare"))


makeCompileUnitMetadata :: Word -> Word -> Definition
makeCompileUnitMetadata diFileIndex index =
  MetadataNodeDefinition (MetadataNodeID index) $
    DINode . Operand.DIScope . Operand.DICompileUnit $
    Operand.CompileUnit
      { language = 12
      , file = MDRef (MetadataNodeID diFileIndex)
      , producer = "clang version 6.0.0 (tags/RELEASE_600/final)"
      , optimized = True
      , flags = ""
      , runtimeVersion = 0
      , splitDebugFileName = ""
      , emissionKind = Operand.FullDebug
      , enums = []
      , retainedTypes = []
      , globals = []
      , imports = []
      , macros = []
      , dWOId = 0
      , splitDebugInlining = False
      , debugInfoForProfiling = False
      , nameTableKind = Operand.NameTableKindDefault
      , rangesBaseAddress = False
      , sysRoot = ""
      , sdk = ""
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
            (Just (Operand.ChecksumInfo Operand.MD5 "")))
        )
      )
    )


makeDILocation :: Env -> Area -> [(ShortByteString, MDRef MDNode)]
makeDILocation env area =
  if envIsDebugBuild env then
    case envCurrentSubProgramSymbolIndex env of
      Just diSubprogramIndex ->
        let metadata = MDInline $
              DILocation Operand.Location
                { Operand.line = fromIntegral $ getLineFromStart area
                , Operand.column = fromIntegral $ getColumnFromStart area
                , scope = MDRef (MetadataNodeID diSubprogramIndex)
                }
        in  [("dbg", metadata)]

      Nothing ->
        []
  else
    []


makeOriginalName :: String -> String
makeOriginalName globalName =
  let withoutHash = List.drop 8 globalName
  in  if '_' `List.notElem` withoutHash then
        withoutHash
      else
        (List.init . List.init . List.dropWhileEnd (/= '_')) withoutHash


makeDISubprogram :: Env -> Area -> Word -> String -> Definition
makeDISubprogram env area index functionName =
  let diCUIndex = envCurrentCompilationUnitSymbolIndex env
      diFileIndex = envCurrentFileSymbolIndex env
      diSubprogram = MetadataNodeDefinition (MetadataNodeID index) $
        DINode . Operand.DIScope . Operand.DILocalScope . Operand.DISubprogram $
          Operand.Subprogram
            { scope = Just (MDRef (MetadataNodeID diFileIndex))
            , name = stringToShortByteString (makeOriginalName functionName)
            , linkageName = ""
            , file = Just (MDRef (MetadataNodeID diFileIndex))
            , line = fromIntegral $ getLineFromStart area
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


declareVariable :: (MonadIRBuilder f, MonadModuleBuilder f) => Env -> Area -> Bool -> String -> Operand -> f ()
declareVariable env area isArg varName var =
  Monad.when (envIsDebugBuild env && Maybe.isJust (envCurrentSubProgramSymbolIndex env)) $ do
    callWithMetadata
      (makeDILocation env area)
      llvmDbgDeclare
      [ (Operand.MetadataOperand (MDValue var), [])
      , (Operand.MetadataOperand
          (MDNode (Operand.MDInline (Operand.DINode (Operand.DIVariable (Operand.DILocalVariable
            Operand.LocalVariable
              { name = stringToShortByteString varName
              , scope = MDRef $ Operand.MetadataNodeID (Maybe.fromMaybe undefined (envCurrentSubProgramSymbolIndex env))
              , file = Just $ MDRef $ Operand.MetadataNodeID (envCurrentFileSymbolIndex env)
              , line = fromIntegral $ getLineFromStart area
              , type' = Just $ Operand.MDInline $ Operand.DIBasicType Operand.BasicType { flags = [], name = "int", sizeInBits = 64, alignInBits = 0, encoding = Just Operand.SignedEncoding, tag = Operand.BaseType }
              , flags = []
              , arg = if isArg then 1 else 0
              , alignInBits = 0
              }
          ))))), [])
      , (Operand.MetadataOperand (MDNode (Operand.MDInline (DIExpression (Operand.Expression [])))), [])
      ]
    return ()
