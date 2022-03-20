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


solveASTsForDoc :: FilePath -> [FilePath] -> IO (Either CompilationError [(Slv.AST, String, [DocString.DocString])])
solveASTsForDoc _          []         = return $ Right []
solveASTsForDoc rootFolder (fp : fps) = do
  canonicalEntrypoint       <- canonicalizePath fp
  astTable                  <- buildASTTable TNode mempty canonicalEntrypoint
  Just dictionaryModulePath <- resolveAbsoluteSrcPath PathUtils.defaultPathUtils (dropFileName canonicalEntrypoint) "Dictionary"
  let (canTable, _) = case astTable of
        Right table -> Can.runCanonicalization mempty dictionaryModulePath TNode Can.initialEnv table canonicalEntrypoint
        Left  e     -> (Left e, [])

  rootPath <- canonicalizePath $ computeRootPath fp
  let moduleName = dropExtension $ makeRelative rootFolder canonicalEntrypoint

  let entryAST         = canTable >>= flip Can.findAST canonicalEntrypoint . fst
      resolvedASTTable = case (entryAST, canTable) of
        (Right ast, Right (table, _)) -> do
          runExcept (runStateT (solveTable table ast) InferState { count = 0, errors = [] })
        (_     , Left e) -> Left e
        (Left e, _     ) -> Left $ CompilationError (ImportNotFound rootPath) NoContext

  case resolvedASTTable of
    Left  e          -> return $ Left e

    Right (table, _) -> case M.lookup canonicalEntrypoint table of
      Just ast -> do
        fileContent <- readFile fp
        let docStrings = DocString.parse fileContent
        case docStrings of
          Right ds -> do
            next <- solveASTsForDoc rootFolder fps
            return $ ([(ast, moduleName, ds)] ++) <$> next
          Left _ -> do
            next <- solveASTsForDoc rootFolder fps
            return $ ([(ast, moduleName, [])] ++) <$> next

      Nothing -> solveASTsForDoc rootFolder fps


getFilesForDoc :: FilePath -> IO [FilePath]
getFilesForDoc fp = do
  allFiles <- getFilesToCompile False fp
  return $ filter (not . isSuffixOf ".spec.mad") allFiles

runDocumentationGenerator :: FilePath -> IO ()
runDocumentationGenerator fp = do
  let ext = takeExtension fp
  filepaths <- getFilesForDoc fp
  let rootPath = case ext of
        ".mad"     -> takeDirectory fp
        '.' : rest -> ""
        _          -> fp

  canonicalRootPath <- canonicalizePath rootPath

  asts              <- solveASTsForDoc canonicalRootPath filepaths
  case asts of
    Right asts' -> putStrLn $ generateASTsDoc asts'

    Left  e     -> putStrLn $ ppShow e
