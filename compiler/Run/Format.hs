module Run.Format where
import           System.Exit
import           System.FilePath                ( takeExtension )
import qualified System.Directory               as Directory
import           Prelude                 hiding ( readFile )
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

import Text.Show.Pretty
import qualified Rock
import Driver.Query
import Run.Options
import Parse.Madlib.Grammar (parse)
import Data.Either.Combinators (mapLeft)




parseASTsToFormat :: [FilePath] -> IO (Either CompilationError [(AST, [Comment])])
parseASTsToFormat        []         = return $ Right []
parseASTsToFormat  (fp : fps)   = do
  canonicalEntrypoint <- Directory.canonicalizePath fp
  code <- try $ readFile defaultPathUtils canonicalEntrypoint :: IO (Either IOException String)

  let source = case code of
          Right a -> Right a
          Left  _ -> Left $ CompilationError (ImportNotFound canonicalEntrypoint) NoContext

  case source of
      Left e ->
        return $ Left $ CompilationError (ImportNotFound canonicalEntrypoint) NoContext

      Right code -> do
        next <- parseASTsToFormat fps
        let ast = mapLeft (const $ CompilationError (GrammarError "" "") NoContext) (parse code)
        let comments =
              case parseComments code of
                Right a ->
                  Right a

                Left _ ->
                  Left $ CompilationError Error NoContext

        let astWithComments = (,) <$> ((\a -> a {apath = Just ""}) <$> ast) <*> comments

        return $ (:) <$> astWithComments <*> next


parseCodeToFormat :: String -> IO (Either CompilationError [(AST, [Comment])])
parseCodeToFormat code = do
  let ast = mapLeft (const $ CompilationError (GrammarError "" "") NoContext) (parse code)
  let comments =
        case parseComments code of
          Right a ->
            Right a

          Left _ ->
            Left $ CompilationError Error NoContext
  return $ (\a cs -> [(a, cs)]) <$> ((\a -> a {apath = Just ""}) <$> ast) <*> comments


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
      parseASTsToFormat filesToFormat
    else
      parseCodeToFormat code

  case astsWithComments of
    Right asts' -> do
      mapM_ (uncurry (processAST width fix)) asts'
    Left err -> do
      putStrLn $ ppShow err
      exitFailure

  return ()
