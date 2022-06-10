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
import Test.Hspec.Discover (writeFile)



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
        let ast = buildAST canonicalEntrypoint code
        let comments =
              case parseComments code of
                Right a ->
                  Right a

                Left _ ->
                  Left $ CompilationError Error NoContext

        let astWithComments = (,) <$> ast <*> comments

        return $ (:) <$> astWithComments <*> next


parseCodeToFormat :: String -> Either CompilationError [(AST, [Comment])]
parseCodeToFormat code =
  let ast = buildAST "" code
      comments =
        case parseComments code of
          Right a ->
            Right a

          Left _ ->
            Left $ CompilationError Error NoContext
  in (\a cs -> [(a, cs)]) <$> ast <*> comments


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
      return $ parseCodeToFormat code

  case astsWithComments of
    Right asts' -> do
      mapM_ (uncurry (processAST width fix)) asts'
    Left err -> do
      putStrLn $ ppShow err
      exitFailure

  return ()
