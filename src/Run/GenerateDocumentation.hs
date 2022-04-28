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


solveASTsForDoc :: FilePath -> [FilePath] -> IO (Either CompilationError [(Slv.AST, Slv.AST, String, [DocString.DocString])])
solveASTsForDoc _          []         = return $ Right []
solveASTsForDoc rootFolder (fp : fps) = do
  canonicalEntrypoint       <- canonicalizePath fp

  Just dictionaryModulePath <- resolveAbsoluteSrcPath PathUtils.defaultPathUtils (dropFileName canonicalEntrypoint) "Dictionary"
  rootPath <- canonicalizePath $ computeRootPath fp
  let moduleName = dropExtension $ makeRelative rootFolder canonicalEntrypoint

  -- TNode build target
  jsAstTable                  <- buildASTTable TNode mempty canonicalEntrypoint
  let (jsCanTable, _) = case jsAstTable of
        Right table -> Can.runCanonicalization mempty dictionaryModulePath TNode Can.initialEnv table canonicalEntrypoint
        Left  e     -> (Left e, [])


  let jsEntryAST         = jsCanTable >>= flip Can.findAST canonicalEntrypoint . fst
      jsResolvedASTTable = case (jsEntryAST, jsCanTable) of
        (Right ast, Right (table, _)) -> do
          runExcept (runStateT (solveTable table ast) InferState { count = 0, errors = [] })
        (_     , Left e) -> Left e
        (Left e, _     ) -> Left $ CompilationError (ImportNotFound rootPath) NoContext
  -- End TNode

  -- TLLVM build target
  llvmAstTable <- buildASTTable TLLVM mempty canonicalEntrypoint
  let (llvmCanTable, _) = case llvmAstTable of
        Right table -> Can.runCanonicalization mempty dictionaryModulePath TLLVM Can.initialEnv table canonicalEntrypoint
        Left  e     -> (Left e, [])

  let llvmEntryAST     = llvmCanTable >>= flip Can.findAST canonicalEntrypoint . fst
      llvmResolvedASTTable = case (llvmEntryAST, llvmCanTable) of
        (Right ast, Right (table, _)) -> do
          runExcept (runStateT (solveTable table ast) InferState { count = 0, errors = [] })
        (_     , Left e) -> Left e
        (Left e, _     ) -> Left $ CompilationError (ImportNotFound rootPath) NoContext
  -- End TLLVM

  case (jsResolvedASTTable, llvmResolvedASTTable) of
    (Left  e, _) ->
      return $ Left e

    (_, Left e) ->
      return $ Left e

    (Right (jsTable, _), Right (llvmTable, _)) ->
      case (M.lookup canonicalEntrypoint jsTable, M.lookup canonicalEntrypoint llvmTable) of
        (Just jsAst, Just llvmAst) -> do
          fileContent <- readFile fp
          case DocString.parse fileContent of
            Right ds -> do
              next <- solveASTsForDoc rootFolder fps
              return $ ([(jsAst, llvmAst, moduleName, ds)] ++) <$> next
            Left _ -> do
              next <- solveASTsForDoc rootFolder fps
              return $ ([(jsAst, llvmAst, moduleName, [])] ++) <$> next

        _ ->
          solveASTsForDoc rootFolder fps


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
    Right asts' ->
      putStrLn $ generateASTsDoc asts'

    Left  e     ->
      putStrLn $ ppShow e
