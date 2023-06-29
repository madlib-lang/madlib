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
import qualified Data.Maybe as Maybe
import System.Environment (lookupEnv)



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

addInstanceToAST :: Src.AST -> Src.Instance -> Src.AST
addInstanceToAST ast inst =
  ast { Src.ainstances = Src.ainstances ast ++ [inst] }

addInterfaceToAST :: Src.AST -> Src.Interface -> Src.AST
addInterfaceToAST ast interface =
  ast { Src.ainterfaces = Src.ainterfaces ast ++ [interface] }


addLogToLastExp :: Bool -> Src.AST -> Src.AST
addLogToLastExp isColorful ast =
  let (Src.Source area target (Src.Assignment n (
          Src.Source absArea absTarget (Src.AbsWithMultilineBody params body)
          ))) = last $ Src.aexps ast
  in  if length body > 1 then
        case last $ init body of
          Src.Source _ _ (Src.Assignment _ _) ->
            ast

          Src.Source _ _ (Src.TypedExp (Src.Source _ _ (Src.Assignment _ _)) _) ->
            ast

          Src.Source _ _ (Src.NamedTypedExp _ _ _) ->
            ast

          lastBodyExp ->
            let logVarName = if isColorful then "__IO__.cLog" else "__IO__.log"
                wrapped = src (Src.App (src (Src.Var logVarName)) [lastBodyExp])
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



evalMadlibCode :: Bool -> Options.Options -> State -> String -> Haskeline.InputT IO CommandResult
evalMadlibCode isColorful options state code = case parse code of
  Right parsedNewCode -> do
    (currentAST, _, _) <- liftIO $ runTask state options Driver.Don'tPrune mempty $ do
      Rock.fetch $ Query.ParsedAST replModulePath
    let newAST = foldl addExpToAST currentAST (Src.aexps parsedNewCode)
    let newASTWithImports = foldl addImportToAST newAST (Src.aimports parsedNewCode)
    let newASTWithTypeDecls = foldl addTypeDeclToAST newASTWithImports (Src.atypedecls parsedNewCode)
    let newASTWithInterfaces = foldl addInterfaceToAST newASTWithTypeDecls (Src.ainterfaces parsedNewCode)
    let newASTWithInstances = foldl addInstanceToAST newASTWithInterfaces (Src.ainstances parsedNewCode)
    let newASTWithLogAdded = addLogToLastExp isColorful newASTWithInstances
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
      let newValidCode = Format.astToSource 80 newASTWithInstances []
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


findTypeCommand :: Options.Options -> State -> String -> Haskeline.InputT IO CommandResult
findTypeCommand options state name = do
  ((typed, _), _, _) <- liftIO $ runTask state options Driver.Don'tPrune mempty $ do
      Rock.fetch $ Query.SolvedASTWithEnv replModulePath
  let maybeType = findTypeOfName typed name
  case maybeType of
    Just _ ->
      return $ Output name maybeType (return ())

    Nothing ->
      return $ ErrorResult $ "No type found for '" <> name <> "'"


data CommandResult
  = Exit
  | CommandNotFound String
  | Output String (Maybe (Qual Type)) (IO ())
  | CommandResult String
  | ErrorResult String
  | Continue (IO ())



resetCommand :: Options.Options -> State -> Haskeline.InputT IO CommandResult
resetCommand options state = do
  liftIO $ hSilence [stdout, stderr] $ runTask state options Driver.Don'tPrune (Map.singleton replModulePath startCode) $ do
    Rock.fetch $ Query.ParsedAST replModulePath
  return $ CommandResult "Cleared, you can now start fresh again!"

evalCmd :: Bool -> Options.Options -> State -> String -> Haskeline.InputT IO CommandResult
evalCmd isColorful options state cmd = case cmd of
  "exit" ->
    return Exit

  "e" ->
    return Exit

  "help" ->
    return $ CommandResult $ introduction isColorful

  "h" ->
    return $ CommandResult $ introduction isColorful

  "reset" ->
    resetCommand options state

  "r" ->
    resetCommand options state

  't':'y':'p':'e':' ':args -> do
    let args' = List.dropWhile Char.isSpace args
        args'' = words args'
    if length args'' /= 1 then
      return $ ErrorResult $ "Invalid command ':" <> cmd <> "'"
    else
      findTypeCommand options state $ List.head args''

  't':' ':args -> do
    let args' = List.dropWhile Char.isSpace args
        args'' = words args'
    if length args'' /= 1 then
      return $ ErrorResult $ "Invalid command ':" <> cmd <> "'"
    else
      findTypeCommand options state $ List.head args''

  _ ->
    return $ CommandNotFound $ ':' : cmd



read :: Bool -> Haskeline.InputT IO (Maybe String)
read isColorful = read' isColorful False []


read' :: Bool -> Bool -> [String] -> Haskeline.InputT IO (Maybe String)
read' isColorful multi acc = do
  let start =
        if multi then
          "| "
        else
          "> "
  maybeLine <- Haskeline.getInputLine start
  case maybeLine of
    Nothing ->
      return Nothing

    Just line ->
      if not multi && null acc && (line == ":multi" || line == ":m") then do
        -- we start multiline mode
        liftIO $ putStrLn $ colorWhen isColorful Grey "-------- " <> colorWhen isColorful Yellow "Multiline Mode enabled" <> colorWhen isColorful Grey " ------------------------"
        liftIO $ putStrLn $ colorWhen isColorful Grey "You are now in multiline mode, to validate it enter a"
        liftIO $ putStrLn $ colorWhen isColorful Grey "dot ( '.' ) on an empty line"
        liftIO $ putStrLn $ colorWhen isColorful Grey "--------------------------------------------------------"
        read' isColorful True []
      else if multi then
        if line == "." then
          return $ Just $ unlines acc
        else
          read' isColorful multi (acc ++ [line])
      else
        return $ Just line


eval :: Bool -> Options.Options -> State -> String -> Haskeline.InputT IO CommandResult
eval isColorful options state code =
  case code of
    ':' : cmd ->
      evalCmd isColorful options state cmd

    _ ->
      evalMadlibCode isColorful options state code


print :: Bool -> CommandResult -> Haskeline.InputT IO ()
print isColorful result = case result of
  Output output maybeType _ -> do
    -- let output' = colorWhen isColorful Yellow output
    let output'' = case maybeType of
          Just qt ->
            output <> colorWhen isColorful Grey (" :: " <> prettyPrintQualType qt)

          _ ->
            output
    liftIO $ putStrLn output''

  ErrorResult err ->
    liftIO $ putStrLn err

  CommandResult toShow ->
    liftIO $ putStrLn toShow

  CommandNotFound cmd ->
    liftIO $ putStrLn $ "Command not found '" <> cmd <> "'"

  _ ->
    return ()


loop :: Bool -> Options.Options -> State -> Haskeline.InputT IO ()
loop isColorful options state = do
  -- code <- Haskeline.handleInterrupt (return (Just "")) (Haskeline.withInterrupt read)
  code <- read isColorful
  case code of
    Just "" ->
      loop isColorful options state

    Just c -> do
      evaluated <- eval isColorful options state c
      print isColorful evaluated
      case evaluated of
        Exit ->
          return ()

        Continue postAction -> do
          liftIO postAction
          loop isColorful options state

        Output _ _ postAction -> do
          liftIO postAction
          loop isColorful options state

        _ -> do
          loop isColorful options state

    _ ->
      return ()


introduction :: Bool -> String
introduction isColorful =
  unlines
    [ colorWhen isColorful Grey "------ " <> colorWhen isColorful Yellow ("REPL - Madlib@" <> showVersion version) <> colorWhen isColorful Grey " -------------------------------"
    , colorWhen isColorful Grey "Available commands:"
    , colorWhen isColorful Grey "  :help           show help (alias :h)"
    , colorWhen isColorful Grey "  :exit           exit the REPL (alias :e)"
    , colorWhen isColorful Grey "  :multi          start multiline mode (alias :m)"
    , colorWhen isColorful Grey "  :type NAME      show the type of an assignment (alias :t)"
    , colorWhen isColorful Grey "  :reset          reset the state of the repl (alias :r)"
    , colorWhen isColorful Grey "-----------------------------------------------------------"
    ]


start :: Target -> IO ()
start target = do
  noColor <- lookupEnv "NO_COLOR"
  let isColorful = noColor == Just "" || Maybe.isNothing noColor
  state <- Driver.initialState

  let options =
        Options.Options
          { Options.optEntrypoint = replModulePath
          , Options.optTarget = target
          , Options.optRootPath = "./"
          , Options.optOutputPath = if target == TLLVM then ".repl/run" else ".repl/"
          , Options.optOptimized = False
          , Options.optPathUtils = PathUtils.defaultPathUtils
          , Options.optBundle = False
          , Options.optCoverage = False
          , Options.optGenerateDerivedInstances = True
          , Options.optInsertInstancePlaholders = False
          , Options.optMustHaveMain = False
          , Options.optParseOnly = False
          , Options.optOptimizationLevel = O1
          , Options.optDebug = False
          }

  putStrLn $ introduction isColorful

  -- load initial module and fill caches for external modules like IO
  hSilence [stdout, stderr] $ liftIO $ runTask state options Driver.Don'tPrune (Map.singleton replModulePath startCode) $ do
    Rock.fetch $ Query.BuiltTarget replModulePath

  Haskeline.runInputT Haskeline.defaultSettings $ loop isColorful options state
  -- Haskeline.runInputT Haskeline.defaultSettings $ Haskeline.withInterrupt $ loop state
  return ()
