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
import qualified System.Console.Haskeline         as Haskeline
import Control.Monad.Catch (handle)
import GHC.IO.Exception (IOException(IOError))
import Infer.Type
import           Paths_madlib                   ( version )
import           Data.Version                   ( showVersion )



-- TODO: handle export & simply discard it after parsing
-- TODO: if assignment, show assignmentName :: Type

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


addExpToAST :: Src.AST -> Src.Exp -> Src.AST
addExpToAST ast exp = case exp of
  Src.Source _ _ (Src.Export e) ->
    addExpToAST ast e

  _ ->
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

          lastBodyExp ->
            let wrapped = src (Src.App (src (Src.Var "__IO__.log")) [lastBodyExp])
                updatedBody = Src.Source area target (Src.Assignment n (
                    Src.Source absArea absTarget (Src.AbsWithMultilineBody params (init (init body) ++ [wrapped, Src.Source (Area (Loc 0 0 0) (Loc 0 0 0)) Src.TargetAll Src.LUnit]))
                  ))
            in  ast { Src.aexps = init (Src.aexps ast) ++ [updatedBody] }
      else
        ast


findType :: Slv.AST -> Maybe (Qual Type)
findType ast =
  let (Slv.Typed _ _ (Slv.Assignment _ (
          Slv.Typed _ _ (Slv.Abs _ body)
          ))) = last $ Slv.aexps ast
  in  if length body > 1 then
        case last $ init body of
          Slv.Typed _ _ (Slv.App _ (Slv.Typed (_ :=> t) _ _) _) ->
            Just ([] :=> t)

          _ ->
            Nothing
      else
        Nothing


shouldRun :: Src.AST -> Bool
shouldRun ast =
  not (null $ Src.aexps ast) && not (Src.isTopLevelFunction (last (Src.aexps ast)))


getNewExpName :: [Src.Exp] -> Maybe String
getNewExpName exps = case exps of
  [Src.Source _ _ (Src.NamedTypedExp n _ _)] ->
    Just n

  [Src.Source _ _ (Src.TypedExp e _)] ->
    getNewExpName [e]

  [Src.Source _ _ (Src.Assignment n _)] ->
    Just n

  [Src.Source _ _ (Src.Export e)] ->
    getNewExpName [e]

  _ ->
    Nothing


findTypeOfName :: Slv.AST -> String -> Maybe (Qual Type)
findTypeOfName ast name =
  let topLevel = List.find ((== Just name) . Slv.getExpName) $ Slv.aexps ast
  in  case topLevel of
        Just (Slv.Typed qt _ _) ->
          Just qt

        Nothing ->
          -- if not top level we look in Main
          let (Slv.Typed _ _ (Slv.Assignment n (Slv.Typed _ _ (Slv.Abs _ body)))) = last $ Slv.aexps ast
              localDecl = List.find ((== Just name) . Slv.getExpName) $ reverse body
          in  case localDecl of
                Just (Slv.Typed (_ :=> t) _ _) ->
                  Just ([] :=> t)

                _ ->
                  Nothing



evalMadlibCode :: Options.Options -> State -> String -> Haskeline.InputT IO CommandResult
evalMadlibCode options state code = case parse code of
  Right parsedNewCode -> do
    (currentAST, _, _) <- liftIO $ runTask state options Driver.Don'tPrune mempty $ do
      Rock.fetch $ Query.ParsedAST replModulePath
    let newAST = foldl addExpToAST currentAST (Src.aexps parsedNewCode)
    let newASTWithImports = foldl addImportToAST newAST (Src.aimports parsedNewCode)
    let newASTWithTypeDecls = foldl addTypeDeclToAST newASTWithImports (Src.atypedecls parsedNewCode)
    let newASTWithLogAdded = addLogToLastExp newASTWithTypeDecls
    let newCodeWithLogAdded = Format.astToSource 80 newASTWithLogAdded []
    ((typed, _), _, errs) <- liftIO $ runTask state options Driver.Don'tPrune (Map.singleton replModulePath newCodeWithLogAdded) $ do
      Rock.fetch $ Query.SolvedASTWithEnv replModulePath

    formattedErrs <- liftIO $ forM errs $ simpleFormatError False
    if null formattedErrs then do
      let newExpName = getNewExpName $ Src.aexps parsedNewCode
      let maybeType = findType typed
      liftIO $ hSilence [stdout, stderr] $ runTask state options Driver.Don'tPrune mempty $ do
        Rock.fetch $ Query.BuiltTarget replModulePath

      runResult <-
        if Options.optTarget options == TLLVM then
          liftIO $ try $ readProcessWithExitCode ".repl/run" [] ""
        else
          liftIO $ try $ readProcessWithExitCode "node" [".repl/__REPL__.mjs"] ""
      let output = case (runResult :: Either IOError (ExitCode, String, String)) of
            Right (_, result, _) | shouldRun parsedNewCode ->
              result

            _ ->
              ""

      -- then reset to newAST
      let newValidCode = Format.astToSource 80 newASTWithTypeDecls []
      let resetValidCode = do
            runTask state options Driver.Don'tPrune (Map.singleton replModulePath newValidCode) $ do
              Rock.fetch $ Query.ParsedAST replModulePath
            return ()

      let output' = List.dropWhileEnd Char.isSpace output

      case newExpName of
        Just n ->
          case findTypeOfName typed n of
            Nothing ->
              return $ Continue resetValidCode

            Just t ->
              return $ Output n (Just t) resetValidCode



        Nothing ->
          -- We just typed something evaluated that should be printed
          if null output' then
            return $ Continue resetValidCode
          else
            return $ Output output' maybeType resetValidCode
    else do
      let previousCode = Format.astToSource 80 currentAST []
      -- We need this to reset the code to the previous state
      liftIO $ runTask state options Driver.Don'tPrune (Map.singleton replModulePath previousCode) $ do
        Rock.fetch $ Query.ParsedAST replModulePath
      return $ ErrorResult $ List.intercalate "\n" formattedErrs

  _ ->
    return $ ErrorResult "Grammar error"


data CommandResult
  = Exit
  | CommandNotFound String
  | Output String (Maybe (Qual Type)) (IO ())
  | CommandResult String
  | ErrorResult String
  | Continue (IO ())


evalCmd :: String -> Haskeline.InputT IO CommandResult
evalCmd cmd = case cmd of
  "exit" ->
    return Exit

  "help" ->
    return $ CommandResult "help text TBD"

  _ ->
    return $ CommandNotFound cmd



read :: Haskeline.InputT IO (Maybe String)
read = Haskeline.getInputLine "> "


-- should return a type other than String
-- something like
-- data EvalResult
--   = TypeCheckError CompilationError
--   | EvaluatedCode String
--   | ImportedModule FilePath String
--   | TypeDefined ...
eval :: Options.Options -> State -> String -> Haskeline.InputT IO CommandResult
eval options state code =
  case code of
    ':' : cmd ->
      evalCmd cmd

    _ ->
      evalMadlibCode options state code


print :: CommandResult -> Haskeline.InputT IO ()
print result = case result of
  Output output maybeType _ -> do
    let output' = color Yellow output
    let output'' = case maybeType of
          Just qt ->
            output' <> color Grey (" :: " <> prettyPrintQualType qt)

          _ ->
            output'
    liftIO $ putStrLn output''

  ErrorResult err ->
    liftIO $ putStrLn err

  CommandResult toShow ->
    liftIO $ putStrLn toShow

  _ ->
    return ()


loop :: Options.Options -> State -> Haskeline.InputT IO ()
loop options state = do
  -- code <- Haskeline.handleInterrupt (return (Just "")) (Haskeline.withInterrupt read)
  code <- read
  case code of
    Just "" ->
      loop options state

    Just c -> do
      evaluated <- eval options state c
      print evaluated
      case evaluated of
        Exit ->
          return ()

        Continue postAction -> do
          liftIO postAction
          loop options state

        Output _ _ postAction -> do
          liftIO postAction
          loop options state

        _ -> do
          loop options state

    _ ->
      return ()


introduction :: String
introduction =
  unlines
    [ color Grey "------ " <> (color Yellow ("Madlib@" <> showVersion version)) <> color Grey " -----------------------------------"
    , "Welcome to the repl!"
    , color Grey "The command :help will assist you"
    , color Grey "The command :exit will exit the REPL"
    , color Grey "--------------------------------------------------------"
    ]


start :: Target -> IO ()
start target = do
  state <- Driver.initialState

  let options =
        Options.Options
          { Options.optEntrypoint = replModulePath
          , Options.optTarget = target
          , Options.optRootPath = "./"
          -- , Options.optOutputPath = ".repl/"
          , Options.optOutputPath = if target == TLLVM then ".repl/run" else ".repl/"
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

  putStrLn introduction
  -- load initial module and fill caches for external modules like IO
  hSilence [stdout, stderr] $ liftIO $ runTask state options Driver.Don'tPrune (Map.singleton replModulePath startCode) $ do
    Rock.fetch $ Query.BuiltTarget replModulePath

  Haskeline.runInputT Haskeline.defaultSettings $ loop options state
  -- Haskeline.runInputT Haskeline.defaultSettings $ Haskeline.withInterrupt $ loop state
  return ()
