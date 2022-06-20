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
import           Data.List                      ( isSuffixOf )
import           Text.Show.Pretty

import           Error.Error
import           Error.Context
import qualified Parse.DocString.Grammar       as DocString
import qualified Parse.DocString.DocString     as DocString
import           Parse.Madlib.AST
import qualified Canonicalize.AST              as Can
import qualified Canonicalize.Env              as Can
import           Infer.Infer
import           Infer.AST
import qualified AST.Solved                    as Slv
import           Generate.Documentation
import           Utils.Path
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


getFilesForDoc :: FilePath -> IO [FilePath]
getFilesForDoc fp = do
  allFiles <- getFilesToCompile False fp
  return $ filter (not . isSuffixOf ".spec.mad") allFiles


runTask :: Options -> IORef (DHashMap Query.Query MVar) -> Rock.Task Query.Query a -> IO a
runTask options ioRef task =
  Rock.runTask (Rock.memoise ioRef (Driver.ignoreTaskKind (Rock.writer (\_ _ -> return ()) $ Rules.rules options))) task


generateDocData :: FilePath -> [FilePath] -> IO [(Slv.AST, Slv.AST, String, [DocString.DocString])]
generateDocData rootFolder paths = do
  let jsOptions =
        Options
          { optPathUtils = defaultPathUtils
          , optEntrypoint = ""
          , optRootPath = "./"
          , optOutputPath = "./"
          , optTarget = TNode
          , optOptimized = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = False
          , optInsertInstancePlaholders = False
          }
  let llvmOptions =
        Options
          { optPathUtils = defaultPathUtils
          , optEntrypoint = ""
          , optRootPath = "./"
          , optOutputPath = "./"
          , optTarget = TLLVM
          , optOptimized = False
          , optBundle = False
          , optCoverage = False
          , optGenerateDerivedInstances = False
          , optInsertInstancePlaholders = False
          }
  memoVar <- newIORef mempty

  forM paths $ \path -> do
    canonicalEntrypoint <- canonicalizePath path
    (jsAst, _)          <- runTask jsOptions memoVar (Rock.fetch $ Query.SolvedASTWithEnv path)
    (llvmAst, _)        <- runTask llvmOptions memoVar (Rock.fetch $ Query.SolvedASTWithEnv path)
    docStrings          <- runTask llvmOptions memoVar (Rock.fetch $ Query.DocStrings path)
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
