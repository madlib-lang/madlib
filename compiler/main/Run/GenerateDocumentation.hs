{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Run.GenerateDocumentation where

import qualified Data.Map                      as M
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( dropFileName
                                                , takeDirectory
                                                , takeBaseName
                                                , takeExtension
                                                , takeFileName
                                                , dropExtension
                                                , joinPath
                                                , splitDirectories
                                                , makeRelative
                                                )
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                , getDirectoryContents
                                                , doesDirectoryExist
                                                , getCurrentDirectory
                                                )
import           Data.List                      ( isSuffixOf, isInfixOf, isPrefixOf )
import qualified Parse.DocString.Grammar       as DocString
import qualified Parse.DocString.DocString     as DocString
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import qualified AST.Solved                    as Slv
import           Generate.Documentation
import           Run.Target
import           Run.Utils
import qualified Utils.PathUtils as PathUtils
import qualified Rock
import qualified Driver.Query    as Query
import qualified Driver.Rules    as Rules
import           Data.Dependent.HashMap (DHashMap)
import           Data.IORef.Lifted
import           Run.Options
import           Control.Concurrent.MVar
import           Utils.PathUtils (defaultPathUtils)
import           Control.Monad
import qualified Driver
import Run.OptimizationLevel


getFilesForDoc :: FilePath -> IO [FilePath]
getFilesForDoc fp = do
  allFiles <- getFilesToCompile False fp
  let withoutSpecFiles = filter (not . isSuffixOf ".spec.mad") allFiles
      withoutPreludePrivateFiles =
        filter
          (\path ->
            not ("prelude/__internal__" `isInfixOf` path) ||
            not ("__" `isPrefixOf` takeBaseName path && "__" `isSuffixOf` takeBaseName path)
          )
          withoutSpecFiles
  return withoutPreludePrivateFiles


runTask :: Options -> IORef (DHashMap Query.Query MVar) -> Rock.Task Query.Query a -> IO a
runTask options ioRef task =
  Rock.runTask (Rock.memoise ioRef (Driver.ignoreTaskKind (Rock.writer (\_ _ -> return ()) $ Rules.rules options))) task


generateDocData :: FilePath -> [FilePath] -> IO [(Slv.AST, Slv.AST, String, [DocString.DocString])]
generateDocData rootFolder paths = do
  rootPath <- canonicalizePath "./"

  let jsOptions =
        Options
          { optPathUtils = defaultPathUtils
          , optEntrypoint = ""
          , optRootPath = rootPath
          , optOutputPath = "./"
          , optTarget = TNode
          , optOptimized = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = False
          , optInsertInstancePlaholders = False
          , optMustHaveMain = False
          , optParseOnly = False
          , optOptimizationLevel = O1
          , optDebug = False
          }
  let llvmOptions =
        Options
          { optPathUtils = defaultPathUtils
          , optEntrypoint = ""
          , optRootPath = rootPath
          , optOutputPath = "./"
          , optTarget = TLLVM
          , optOptimized = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = False
          , optInsertInstancePlaholders = False
          , optMustHaveMain = False
          , optParseOnly = False
          , optOptimizationLevel = O1
          , optDebug = False
          }
  jsMemoVar <- newIORef mempty
  llvmMemoVar <- newIORef mempty

  forM paths $ \path -> do
    canonicalEntrypoint <- canonicalizePath path
    let jsOptions' = jsOptions { optEntrypoint = canonicalEntrypoint }
    let llvmOptions' = llvmOptions { optEntrypoint = canonicalEntrypoint }

    (jsAst, _)          <- runTask jsOptions' jsMemoVar (Rock.fetch $ Query.SolvedASTWithEnv canonicalEntrypoint)
    (llvmAst, _)        <- runTask llvmOptions' llvmMemoVar (Rock.fetch $ Query.SolvedASTWithEnv canonicalEntrypoint)
    docStrings          <- runTask llvmOptions' llvmMemoVar (Rock.fetch $ Query.DocStrings canonicalEntrypoint)
    let moduleName = dropExtension $ makeRelative rootFolder canonicalEntrypoint 
    return (jsAst, llvmAst, moduleName, docStrings)


runDocumentationGenerator :: FilePath -> IO ()
runDocumentationGenerator fp = do
  let ext      = takeExtension fp
      rootPath = case ext of
        ".mad" ->
          takeDirectory fp

        '.' : _ ->
          ""

        _ ->
          fp

  filepaths         <- getFilesForDoc fp
  canonicalRootPath <- canonicalizePath rootPath
  docInfos          <- generateDocData canonicalRootPath filepaths
  putStrLn $ generateASTsDoc docInfos
