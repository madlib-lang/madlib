{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Run.Repl where
import Prelude hiding(read, print)
import System.IO (hFlush, stdout, stderr)
import Parse.Madlib.Grammar (parse)
import Text.Show.Pretty
import Data.IORef
import qualified AST.Source as Src
import qualified Driver
import Error.Error
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Run.Options as Options
import qualified Rock
import qualified Driver.Query as Query
import Error.Warning
import Control.Monad.IO.Class (liftIO)
import qualified Utils.PathUtils as PathUtils
import Run.Target
import Run.OptimizationLevel
import Control.Monad (forM_, forM)
import Explain.Format
import qualified Data.List as List
import Explain.Location
import qualified Format.Format as Format
import qualified AST.Solved as Slv
import           System.IO.Silently
import           System.Exit (ExitCode)
import           Control.Exception (try)
import           System.Process


-- TODO: consider this lib
-- https://hackage.haskell.org/package/haskeline-0.8.1.1/docs/System-Console-Haskeline.html

type State = Driver.State CompilationError


replModulePath :: String
replModulePath = "__REPL__.mad"


runTask :: State -> Options.Options -> Driver.Prune -> Map.Map FilePath String -> Rock.Task Query.Query a -> IO (a, [CompilationWarning], [CompilationError])
runTask state options prune fileUpdates task =
  Driver.runIncrementalTask
    state
    options
    (Map.keys fileUpdates)
    fileUpdates
    prune
    task


options :: Options.Options
options =
  Options.Options
    { Options.optEntrypoint = replModulePath
    , Options.optTarget = TLLVM
    , Options.optRootPath = "./"
    , Options.optOutputPath = ".repl/run"
    , Options.optOptimized = False
    , Options.optPathUtils = PathUtils.defaultPathUtils
    , Options.optBundle = False
    , Options.optCoverage = False
    , Options.optGenerateDerivedInstances = True
    , Options.optInsertInstancePlaholders = False
    , Options.optMustHaveMain = False
    , Options.optOptimizationLevel = O1
    , Options.optDebug = False
    }


state :: IO (IORef Src.AST)
state =
  newIORef Src.emptyAST


startCode :: String
startCode =
  unlines
    [ "import __IO__ from \"IO\""
    , ""
    , "main = () => {"
    , "}"
    ]


src :: a -> Src.Source a
src a =
  Src.Source emptyArea Src.TargetAll a


start :: IO ()
start = do
  state <- Driver.initialState

  -- load initial module and fill caches for external modules like IO
  hSilence [stdout, stderr] $ runTask state options Driver.Don'tPrune (Map.singleton replModulePath startCode) $ do
    Rock.fetch $ Query.BuiltTarget replModulePath
  loop state
  return ()


addNewCode :: String -> String -> String
addNewCode newCode previousCode =
  let previousLines = List.init $ lines previousCode
  in  unlines $ previousLines ++ [newCode, "}"]


read :: IO String
read = liftIO Prelude.getLine


addExpToAST :: Src.AST -> Src.Exp -> Src.AST
addExpToAST ast exp =
  if Src.isTopLevelFunction exp then
    ast { Src.aexps = init (Src.aexps ast) ++ [exp, last $ Src.aexps ast] }
  else
    let (Src.Source area target (Src.Assignment n (
          Src.Source absArea absTarget (Src.AbsWithMultilineBody params exps)
          ))) = last $ Src.aexps ast
        mainWithNewExp =
          Src.Source area target (Src.Assignment n (
            Src.Source absArea absTarget (Src.AbsWithMultilineBody params (init exps ++ [exp, Src.Source (Area (Loc 0 0 0) (Loc 0 0 0)) Src.TargetAll Src.LUnit]))
          ))
    in  ast { Src.aexps = init (Src.aexps ast) ++ [mainWithNewExp] }


addImportToAST :: Src.AST -> Src.Import -> Src.AST
addImportToAST ast imp =
  ast { Src.aimports = Src.aimports ast ++ [imp] }

addTypeDeclToAST :: Src.AST -> Src.TypeDecl -> Src.AST
addTypeDeclToAST ast td =
  ast { Src.atypedecls = Src.atypedecls ast ++ [td] }


addLogToLastExp :: Src.AST -> Src.AST
addLogToLastExp ast =
  let (Src.Source area target (Src.Assignment n (
          Src.Source absArea absTarget (Src.AbsWithMultilineBody params body)
          ))) = last $ Src.aexps ast
  in  if length body > 1 then
        case last $ init body of
          Src.Source _ _ (Src.Assignment _ _) ->
            ast

          Src.Source _ _ (Src.TypedExp (Src.Source _ _ (Src.Assignment _ _)) _) ->
            ast

          _ ->
            let lastBodyExp = last $ init body
                wrapped = src (Src.App (src (Src.Var "__IO__.log")) [lastBodyExp])
                updatedBody = Src.Source area target (Src.Assignment n (
                    Src.Source absArea absTarget (Src.AbsWithMultilineBody params (init (init body) ++ [wrapped, Src.Source (Area (Loc 0 0 0) (Loc 0 0 0)) Src.TargetAll Src.LUnit]))
                  ))
            in  ast { Src.aexps = init (Src.aexps ast) ++ [updatedBody] }
      else
        ast


shouldRun :: Src.AST -> Bool
shouldRun ast =
  not (null $ Src.aexps ast) && not (Src.isTopLevelFunction (last (Src.aexps ast)))


evalMadlibExp :: State -> String -> IO String
evalMadlibExp state code = case parse code of
  Right parsedNewCode -> do
    (currentAST, _, _) <- runTask state options Driver.Don'tPrune mempty $ do
      Rock.fetch $ Query.ParsedAST replModulePath
    let newAST = foldl addExpToAST currentAST (Src.aexps parsedNewCode)
    let newASTWithImports = foldl addImportToAST newAST (Src.aimports parsedNewCode)
    let newASTWithTypeDecls = foldl addTypeDeclToAST newASTWithImports (Src.atypedecls parsedNewCode)
    let newASTWithLogAdded = addLogToLastExp newASTWithTypeDecls
    let newCodeWithLogAdded = Format.astToSource 80 newASTWithLogAdded []
    ((typed, _), _, errs) <- runTask state options Driver.Don'tPrune (Map.singleton replModulePath newCodeWithLogAdded) $ do
      Rock.fetch $ Query.SolvedASTWithEnv replModulePath

    formattedErrs <- forM errs $ simpleFormatError False
    if null formattedErrs then do
      -- TODO: compile & run the AST with the newly added __IO__.log call for the last exp of main

      hSilence [stdout, stderr] $ runTask state options Driver.Don'tPrune mempty $ do
        Rock.fetch $ Query.BuiltTarget replModulePath

      runResult <- try $ readProcessWithExitCode ".repl/run" [] ""
      let output = case (runResult :: Either IOError (ExitCode, String, String)) of
            Right (_, result, _) | shouldRun parsedNewCode ->
              result

            _ ->
              ""

      -- then reset to newAST
      let newValidCode = Format.astToSource 80 newASTWithTypeDecls []
      runTask state options Driver.Don'tPrune (Map.singleton replModulePath newValidCode) $ do
        Rock.fetch $ Query.SolvedASTWithEnv replModulePath

      let output' = List.dropWhileEnd Char.isSpace output
      return output'
    else do
      let previousCode = Format.astToSource 80 currentAST []
      -- We need this to reset the code to the previous state
      runTask state options Driver.Don'tPrune (Map.singleton replModulePath previousCode) $ do
        Rock.fetch $ Query.ParsedAST replModulePath
      return $ List.intercalate "\n" formattedErrs

  _ ->
    return "Grammar error"


-- should return a type other than String
-- something like
-- data EvalResult
--   = TypeCheckError CompilationError
--   | EvaluatedCode String
--   | ImportedModule FilePath String
--   | TypeDefined ...
eval :: State -> String -> IO String
eval state code = do
  evalMadlibExp state code


print :: String -> IO ()
print "" = return ()
print result = do
  putStrLn result


loop :: State -> IO ()
loop state = do
  putStr "> "
  hFlush stdout

  code <- read
  evaluated <- eval state code
  print evaluated

  loop state
