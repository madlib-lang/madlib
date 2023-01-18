module Run.Format where
import           System.Exit
import           System.FilePath                ( takeExtension )
import qualified System.Directory               as Directory
import           Prelude                 hiding ( readFile )
import qualified Prelude
import           Control.Exception              ( IOException
                                                , try
                                                )

import           AST.Source
import           Parse.Madlib.AST
import           Parse.Comments.Lexer
import           Run.Utils
import           Utils.PathUtils
import           Error.Context
import           Error.Error
import           Format.Format

import           Text.Show.Pretty
import           Parse.Madlib.Grammar (parse)
import           Data.Either.Combinators (mapLeft)
import           Run.Options
import           Run.Target
import Explain.Format (formatError)




parseASTsToFormat :: [FilePath] -> IO (Either CompilationError [(AST, [Comment])])
parseASTsToFormat        []         = return $ Right []
parseASTsToFormat  (fp : fps)   = do
  canonicalEntrypoint <- Directory.canonicalizePath fp
  code <- try $ readFile defaultPathUtils canonicalEntrypoint :: IO (Either IOException String)

  let source = case code of
          Right a -> Right a
          Left  _ -> Left $ CompilationError (ImportNotFound canonicalEntrypoint) NoContext

  case source of
      Left _ ->
        return $ Left $ CompilationError (ImportNotFound canonicalEntrypoint) NoContext

      Right code -> do
        next <- parseASTsToFormat fps
        let options = Options { optPathUtils = defaultPathUtils
                              , optEntrypoint = canonicalEntrypoint
                              , optRootPath = "./"
                              , optOutputPath = "./build"
                              , optTarget = TAny
                              , optOptimized = False
                              , optBundle = False
                              , optCoverage = False
                              , optGenerateDerivedInstances = True
                              , optInsertInstancePlaholders = True
                              , optMustHaveMain = True
                              }
        ast <- case parse code of
          Right a ->
            computeAbsoluteImportPathsForAST (optPathUtils options) (optRootPath options) (setPath a fp)

          Left _ ->
            return $ Left $ CompilationError Error NoContext
        let ast' = mapLeft (const $ CompilationError (GrammarError "" "") NoContext) ast
        let comments =
              case parseComments code of
                Right a ->
                  Right a

                Left _ ->
                  Left $ CompilationError Error NoContext

        let astWithComments = (,) <$> ((\a -> a { apath = Just canonicalEntrypoint }) <$> ast') <*> comments

        return $ (:) <$> astWithComments <*> next


parseCodeToFormat :: String -> IO (Either CompilationError [(AST, [Comment])])
parseCodeToFormat code = do
  cwd <- Directory.getCurrentDirectory
  let options = Options { optPathUtils = defaultPathUtils
                        , optEntrypoint = "./Module.mad"
                        , optRootPath = cwd
                        , optOutputPath = "./build"
                        , optTarget = TAny
                        , optOptimized = False
                        , optBundle = False
                        , optCoverage = False
                        , optGenerateDerivedInstances = True
                        , optInsertInstancePlaholders = True
                        , optMustHaveMain = True
                        }
  ast <- case parse code of
        Right a ->
          computeAbsoluteImportPathsForAST (optPathUtils options) (optRootPath options) (setPath a "./Module.mad")

        Left _ ->
          return $ Left $ CompilationError Error NoContext
  let ast' = mapLeft (const $ CompilationError (GrammarError "" "") NoContext) ast
  let comments =
        case parseComments code of
          Right a ->
            Right a

          Left _ ->
            Left $ CompilationError Error NoContext
  return $ (\a cs -> [(a, cs)]) <$> ((\a -> a { apath = Just "" }) <$> ast') <*> comments


processAST :: Int -> Bool -> AST -> [Comment] -> IO String
processAST width fix ast comments = do
  let formatted = astToSource width ast comments

  case apath ast of
    Just path -> do
      if fix then
        writeFile path formatted
      else
        putStr formatted

    Nothing ->
      return ()

  return formatted

runFormatter :: Int -> Bool -> FilePath -> String -> IO ()
runFormatter width fix path code = do
  astsWithComments <-
    if code == "--EMPTY--" then do
      filesToFormat <- getFilesToCompile False path
      if null filesToFormat then do
        putStrLn $ "No file to format found, verify that the directory or file '" <> path <> "' exists!"
        exitFailure
      else
        parseASTsToFormat filesToFormat
    else
      parseCodeToFormat code

  case astsWithComments of
    Right asts' -> do
      mapM_ (uncurry (processAST width fix)) asts'
    Left err -> do
      formattedErr <- formatError Prelude.readFile False err
      putStrLn formattedErr
      exitFailure

  return ()
