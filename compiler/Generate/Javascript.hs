{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Generate.Javascript where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( sort
                                                , find
                                                , intercalate
                                                , foldl', stripPrefix, isInfixOf, isPrefixOf
                                                )
import           Data.List.GroupBy              ( groupBy )

import           AST.Core                      as Core
import           Utils.Path                     ( cleanRelativePath
                                                , computeTargetPath
                                                , makeRelativeEx
                                                , convertWindowsSeparators
                                                )
import           System.FilePath                ( replaceExtension
                                                , dropFileName
                                                , joinPath, splitDirectories, takeDirectory, pathSeparator
                                                )
import           Explain.Location
import           Infer.Type
import           Generate.JSInternals
import           Run.Target
import           Generate.Utils
import           Text.Show.Pretty               ( ppShow )
import Distribution.Types.Lens (_Impl)
import qualified Data.Maybe as Maybe
import Run.CommandLine
import Control.Exception
import GHC.IO.Exception
import System.Process
import System.Directory
import System.Environment (getEnv)
import Run.Options
import qualified Data.List as List



data RecursionData
  = PlainRecursionData { rdParams :: [String] }
  | RightListRecursionData { rdParams :: [String] }
  | ConstructorRecursionData { rdParams :: [String] }
  deriving(Eq, Show)

data Env = Env { varsInScope :: S.Set String, recursionData :: Maybe RecursionData, varsRewritten :: M.Map String String } deriving(Eq, Show)

initialEnv :: Env
initialEnv = Env { varsInScope = S.empty, recursionData = Nothing, varsRewritten = M.empty }

allowedJSNames :: [String]
allowedJSNames = ["delete", "class", "while", "for", "case", "switch", "try", "length", "var", "default", "break"]

generateSafeName :: String -> String
generateSafeName n =
  if '.' `elem` n then
    let namespace = takeWhile (/= '.') n
        name      = tail $ dropWhile (/= '.') n
    in  if name `elem` allowedJSNames then
          namespace <> "." <> "_$_" <> name <> "_$_"
        else
          n
  else if n `elem` allowedJSNames then
    "_$_" <> n <> "_$_"
  else
    n

hpWrapLine :: Bool -> FilePath -> Int -> String -> String
hpWrapLine coverage astPath line compiled =
  if coverage then "__hpLineWrap('" <> astPath <> "', " <> show line <> ", " <> compiled <> ")" else compiled


escapeStringLiteral :: String -> String
escapeStringLiteral s = case s of
  '`' : next ->
    '\\' : '`' : escapeStringLiteral next

  c : next ->
    c : escapeStringLiteral next

  "" ->
    ""

data CompilationConfig
  = CompilationConfig
      { ccrootPath       :: FilePath
      , ccastPath        :: FilePath
      , ccentrypointPath :: FilePath
      , ccoutputPath     :: FilePath
      , cccoverage       :: Bool
      , ccoptimize       :: Bool
      , cctarget         :: Target
      , ccinternalsPath  :: FilePath
      }

class Compilable a where
  compile :: Env -> CompilationConfig -> a -> String

instance Compilable Exp where
  compile _ _ Untyped{} = ""
  compile env config e@(Typed qt@(_ :=> expType) area@(Area (Loc _ l _) _) metadata exp) =
    let
      astPath   = ccastPath config
      coverage  = cccoverage config
      optimized = ccoptimize config
    in
      case exp of
        _ | isPlainRecursionEnd metadata ->
          let compiledExp = compile env config (Typed qt area [] exp)
          in  "($_result_ = " <> compiledExp <> ")"

        _ | isRightListRecursionEnd metadata ->
          let compiledExp = compile env config (Typed qt area [] exp)
          in  "($_end_.n = " <> compiledExp <> ", $_result_ = $_start_.n)"

        _ | isConstructorRecursionEnd metadata ->
          let compiledExp = compile env config (Typed qt area [] exp)
          in  "($args[$index] = "<>compiledExp<>", $_result_ = $_start_.__args[0])"

        Call fn@(Typed _ _ _ (Var constructorName True)) args | isConstructorRecursiveCall metadata ->
          case getConstructorRecursionInfo metadata of
            Just (ConstructorRecursionInfo _ position) ->
              let Just params   = rdParams <$> recursionData env
                  newValue = "$newValue = { __constructor: \""<>constructorName<>"\", __args: [] }"
                  compiledArgs =
                    (\(index, arg) ->
                      if index == position then
                        "$newValue.__args.push(null)"
                      else
                        "$newValue.__args.push(" <> compile env config arg <> ")"
                    ) <$> zip [0..] args
                  updateParams  = case args!!position of
                    Core.Typed _ _ _ (Core.Call _ recArgs) ->
                      let compiledRecArgs  = compile env config <$> recArgs
                      in  (\(param, arg) -> param <> " = " <> arg <> "") <$> zip params compiledRecArgs

                    _ ->
                      undefined
              in  "("<>newValue<>", " <> intercalate ", " compiledArgs <> ", $args[$index] = $newValue, $args = $newValue.__args, $index = "<>show position<>", "<>intercalate ", " updateParams<>", $_continue_ = true)"

            _ ->
              undefined

        Literal (LNum v) ->
          hpWrapLine coverage astPath l v

        Literal (LFloat v) ->
          hpWrapLine coverage astPath l v

        Literal (LStr (leading : v)) | leading == '"' || leading == '\'' ->
          if null v then
            hpWrapLine coverage astPath l "``"
          else
            hpWrapLine coverage astPath l ("`" <> init (escapeStringLiteral v) <> "`")

        Literal (LStr v) ->
          hpWrapLine coverage astPath l ("`" <> escapeStringLiteral v <> "`")

        Literal (LChar v) ->
          -- String is aliased to __String as people may use String as a default import
          hpWrapLine coverage astPath l ("__String.fromCharCode(" <> (show . fromEnum) v <> ")")

        Literal (LBool v) ->
          hpWrapLine coverage astPath l v

        Literal LUnit ->
          hpWrapLine coverage astPath l "({ __constructor: \"Unit\", __args: [] })"

        Call (Typed _ _ _ (Var "++" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " + "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "unary-minus" _)) [arg] ->
          "-" <> hpWrapLine coverage astPath (getStartLine arg) (compile env config arg)

        Call (Typed _ _ _ (Var "+" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " + "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "-" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " - "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "*" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " * "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "/" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " / "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "%" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " % "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "==" _)) [left, right] ->
          eqFnName optimized
          <> "("
          <> hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> ", "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)
          <> ")"

        Call (Typed _ _ _ (Var "!=" _)) [left, right] ->
          "!"
          <> eqFnName optimized
          <> "("
          <> hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> ", "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)
          <> ")"

        Call (Typed _ _ _ (Var "&&" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " && "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "||" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " || "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "|" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " | "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "&" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " & "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "^" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " ^ "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "~" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " ~ "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "<<" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " << "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">>" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " >> "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">>>" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " >>> "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " > "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "<" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " < "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var ">=" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " >= "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call (Typed _ _ _ (Var "<=" _)) [left, right] ->
          hpWrapLine coverage astPath (getStartLine left) (compile env config left)
          <> " <= "
          <> hpWrapLine coverage astPath (getStartLine right) (compile env config right)

        Call fn args | isPlainRecursiveCall metadata ->
          let params  = case recursionData env of
                Just PlainRecursionData { rdParams } ->
                  rdParams

                Just RightListRecursionData { rdParams } ->
                  rdParams

                Just ConstructorRecursionData { rdParams } ->
                  rdParams

                _ ->
                  []

              compiledArgs = compile env config <$> args
              updateParams  = (\(param, arg) -> param <> " = " <> arg <> "") <$> zip params compiledArgs
          in  if null updateParams then
                "($_continue_ = true)"
              else
                "(" <> intercalate ", " updateParams <> ", $_continue_ = true)"

        Call fn args ->
          let compiledArgs = (<> ")") . ("(" <>) . compile env config <$> args
          in  compile env config fn <> concat compiledArgs

        If cond truthy falsy ->
          "("
            <> compile env config cond
            <> " ? "
            <> compile env config truthy
            <> " : "
            <> compile env config falsy
            <> ")"

        Definition params body -> compileAbs Nothing (getValue <$> params) body
         where
          compileAbs :: Maybe [Exp] -> [Name] -> [Exp] -> String
          compileAbs parent params body =
            let params' = intercalate " => " params
            in  "("
                <> params'
                <> " => "
                <> compileBody env{ varsInScope = varsInScope env <> S.fromList params} body
                <> ")"

          compileBody :: Env -> [Exp] -> String
          compileBody env body = case body of
            [exp] | isPlainRecursiveDefinition metadata ->
              let tcoParams = (\param -> "    let $$" <> param <> " = " <> param <> ";") <$> (getValue <$> params)
                  updateParams =  (\param -> "      let $" <> param <> " = $$" <> param <> ";") <$> (getValue <$> params)
                  rewrite     = M.fromList ((\param -> (param, "$"<>param)) <$> (getValue <$> params))
              in  "{\n"
                  <> "    let $_result_;\n"
                  <> "    let $_continue_ = true;\n"
                  <> unlines tcoParams
                  <> "\n"
                  <> "    while($_continue_) {\n"
                  <> unlines updateParams
                  <> "\n"
                  <> "        $_continue_ = false;\n"
                  <> "        "<> compile env { recursionData = Just PlainRecursionData { rdParams = ("$$"<>) <$> (getValue <$> params) }, varsRewritten = varsRewritten env <> rewrite } config exp
                  <> "\n    }\n"
                  <> "    return $_result_;\n"
                  <> "}"

            [exp] | isRightListRecursiveDefinition metadata ->
              let tcoParams = (\param -> "    let $$" <> param <> " = " <> param <> ";") <$> (getValue <$> params)
                  updateParams =  (\param -> "      let $" <> param <> " = $$" <> param <> ";") <$> (getValue <$> params)
                  rewrite     = M.fromList ((\param -> (param, "$"<>param)) <$> (getValue <$> params))
              in  "{\n"
                  <> "    let $_result_;\n"
                  <> "    let $_continue_ = true;\n"
                  <> "    let $_start_ = {};\n"
                  <> "    let $_end_ = $_start_;\n"
                  <> unlines tcoParams
                  <> "\n"
                  <> "    while($_continue_) {\n"
                  <> unlines updateParams
                  <> "\n"
                  <> "        $_continue_ = false;\n"
                  <> "        "<> compile env { recursionData = Just RightListRecursionData { rdParams = ("$$"<>) <$> (getValue <$> params) }, varsRewritten = varsRewritten env <> rewrite } config exp
                  <> "\n    }\n"
                  <> "    return $_result_;\n"
                  <> "}"

            [exp] | isConstructorRecursiveDefinition metadata ->
              let tcoParams = (\param -> "    let $$" <> param <> " = " <> param <> ";") <$> (getValue <$> params)
                  updateParams =  (\param -> "      let $" <> param <> " = $$" <> param <> ";") <$> (getValue <$> params)
                  rewrite     = M.fromList ((\param -> (param, "$"<>param)) <$> (getValue <$> params))
              in  "{\n"
                  <> "    let $_result_;\n"
                  <> "    let $_continue_ = true;\n"
                  <> "    let $_start_ = { __args: [] };\n"
                  <> "    let $args = $_start_.__args;\n"
                  <> "    let $index = 0;\n"
                  <> "    let $newValue;\n"
                  <> unlines tcoParams
                  <> "\n"
                  <> "    while($_continue_) {\n"
                  <> unlines updateParams
                  <> "\n"
                  <> "        $_continue_ = false;\n"
                  <> "        "<> compile env { recursionData = Just ConstructorRecursionData { rdParams = ("$$"<>) <$> (getValue <$> params) }, varsRewritten = varsRewritten env <> rewrite } config exp
                  <> "\n    }\n"
                  <> "    return $_result_;\n"
                  <> "}"

            [exp] ->
              compile env config exp

            exps ->
              "{\n"
              <> compileBody' env exps
              <> "}"


          compileBody' :: Env -> [Exp] -> String
          compileBody' env [exp] = case exp of
            (Typed _ _ _ (JSExp _)) -> compile env config exp
            _                         -> "    return " <> compile env config exp <> ";\n"
          compileBody' e (exp : es) = case exp of
            Core.Typed _ _ _ (Core.Assignment name _) ->
              let nextEnv = e { varsInScope = S.insert name (varsInScope e) }
              in  "    " <> compile e config exp <> ";\n" <> compileBody' nextEnv es

            _ -> "    " <> compile e config exp <> ";\n" <> compileBody' e es

        Var name _ ->
          let safeName  = generateSafeName name
              rewritten = varsRewritten env
          in  case M.lookup safeName rewritten of
                Just safeName' ->
                  if safeName' == "!" || not coverage then
                    safeName'
                  else
                    hpWrapLine coverage astPath l safeName'

                Nothing ->
                  if safeName == "!" || not coverage then
                    safeName
                  else
                    hpWrapLine coverage astPath l safeName

        Placeholder (ClassRef cls _ call var, ts) exp' -> insertPlaceholderArgs "" e

         where
          insertPlaceholderArgs :: String -> Exp -> String
          insertPlaceholderArgs prev exp'' = case exp'' of
            Core.Typed _ _ _ (Placeholder (ClassRef cls ps call var, ts) exp''') ->
              let dict  = generateRecordName optimized cls ts var
                  dict' = partiallyApplySubDicts dict ps
              in  insertPlaceholderArgs (prev <> "(" <> dict' <> ")") exp'''

            _ -> compile env config exp'' <> prev

          partiallyApplySubDicts :: String -> [ClassRefPred] -> String
          partiallyApplySubDicts dict ps = if not (null ps)
            then
              let
                dicts =
                  "["
                    <> intercalate
                         ", "
                         (   (\(CRPNode cls' ts var subdicts) ->
                               partiallyApplySubDicts (generateRecordName optimized cls' ts var) subdicts
                             )
                         <$> ps
                         )
                    <> "]"
              in  applyDictsFnName optimized <> "(" <> dict <> ", " <> dicts <> ")"
            else dict


        Placeholder (MethodRef cls method var, ts) (Core.Typed _ _ _ (Var name _)) ->
          let compiled = generateRecordName optimized cls ts var <> "." <> method <> "()"
          in  if not coverage then compiled else hpWrapLine coverage astPath l compiled

        Assignment name exp ->
          let safeName                       = generateSafeName name
              (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
              content'                       = if coverage && isFunctionType expType
                then "__hpFnWrap('" <> astPath <> "', " <> show l <> ", '" <> safeName <> "')(" <> content <> ")"
                else content
              needsModifier = notElem safeName $ varsInScope env
          in  if not (null dicts)
                then
                  (if needsModifier then "let " else "")
                  <> safeName
                  <> " = "
                  <> placeholders
                  <> content'

                  -- NB: this commented solution enables memoizing for overloaded functions
                  -- but also it does weird things wrt concurrency and Wish, which led tests
                  -- to use the wrong instance of Inspect to display diffs.
                  -- (if needsModifier then "let " else "")
                  -- <> safeName
                  -- <> " = "
                  -- <> placeholders
                  -- <> "{"
                  -- <> "\n  "
                  -- <> intercalate
                  --      "\n  "
                  --      ((\dict -> getGlobalForTarget (cctarget config) <> "." <> dict <> " = " <> dict) <$> dicts)
                  -- <> "\n\n"
                  -- <> "  return "
                  -- <> safeName
                  -- <> "__ND__()"
                  -- <> "\n};\n"
                  -- <> "let "
                  -- <> safeName
                  -- <> "__ND__"
                  -- <> " = "
                  -- <> onceFnName optimized
                  -- <> "(() => "
                  -- <> content'
                  -- <> ")"
                else (if needsModifier then "let " else "") <> safeName <> " = " <> content'

        Export     ass    -> "export " <> compile env config ass

        NameExport name   -> "export { " <> generateSafeName name <> " }"

        Record     fields -> let fs = intercalate "," $ compileField <$> fields in "({" <> fs <> " })"
         where
          compileField :: Field -> String
          compileField (Typed _ _ _ field) = case field of
            Field (name, exp) ->
              " " <> name <> ": " <> hpWrapLine coverage astPath (getStartLine exp) (compile env config exp)
            FieldSpread exp -> " ..." <> hpWrapLine coverage astPath (getStartLine exp) (compile env config exp)

        Access record (Typed _ _ _ (Var name _)) ->
          hpWrapLine coverage astPath (getStartLine record) $ compile env config record <> name

        JSExp           content -> content

        Core.ListConstructor [
            Core.Typed _ _ _ (Core.ListItem li),
            Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
          ] | Core.isRightListRecursiveCall metadata ->
          let compiledLi    = compile env config li
              Just params   = rdParams <$> recursionData env
              compiledArgs  = compile env config <$> args
              updateParams  = (\(param, arg) -> param <> " = " <> arg <> "") <$> zip params compiledArgs
          in  "($_end_ = $_end_.n = { v: "<> compiledLi <>" }, " <> intercalate ", " updateParams <> ", $_continue_ = true)"

        ListConstructor elems   -> "(" <> compileListElements elems <> ")"
          where
            compileListElements :: [ListItem] -> String
            compileListElements items = case items of
              [Typed _ _ _ (ListItem last)] ->
                "{ v: " <> compile env config last <> ", n: null }"

              [Typed _ _ _ (ListSpread last)] ->
                compile env config last

              (Typed _ _ _ (ListSpread spread) : next) ->
                "__listCtorSpread__(" <> compile env config spread <> ", " <> compileListElements next <> ")"

              [] ->
                "null"

              (Typed _ _ _ (ListItem item) : next) ->
                "{ v: " <> compile env config item <> ", n: " <> compileListElements next <> " }"

              _ ->
                "null"

        TupleConstructor elems -> "([" <> intercalate ", " (compile env config <$> elems) <> "])"

        Do exps ->
          let compiledExps = compile env config <$> exps
              allExceptLast = init compiledExps
              l = last compiledExps
          in "(() => {\n  "
              <> intercalate "\n  " allExceptLast
              <> "\n  return " <> l
              <> "\n})()"

        Where exp (first : cs) ->
          "((__x__) => {\n  "
            <> compileIs first
            <> concat (("  else " ++) . compileIs <$> cs)
            <> "  else {\n    console.log('non exhaustive patterns for value: ', __x__.toString()); \n    console.trace(); \n    throw 'non exhaustive patterns!';\n  }\n"
            <> "})("
            <> compile env config exp
            <> ")"
         where
          compileListPattern :: String -> [Pattern] -> String
          compileListPattern scope pats = case pats of
            [] ->
              scope <> " === null"

            items ->
              buildListLengthCheck scope items

          buildListLengthCheck :: String -> [Pattern] -> String
          buildListLengthCheck scope pats = case pats of
            [pat@(Typed _ _ _ (PSpread _))] ->
                compilePattern scope pat

            [] ->
              scope <> " === null"

            (pat : more) ->
              scope <> " !== null && " <> compilePattern (scope <> ".v") pat <> " && " <> buildListLengthCheck (scope <> ".n") more


          compileTuplePattern :: String -> [Pattern] -> String
          compileTuplePattern scope [] = scope <> ".length === 0"
          compileTuplePattern scope items =
            scope
              <> ".length "
              <> lengthComparator items
              <> " "
              <> show (if containsSpread items then length items - 1 else length items)
              <> " && "
              <> intercalate
                   " && "
                   ((\(item, i) -> compilePattern (scope <> "[" <> show i <> "]") item) <$> zip items [0 ..])

          lengthComparator :: [Pattern] -> String
          lengthComparator pats = if containsSpread pats then ">=" else "==="

          containsSpread :: [Pattern] -> Bool
          containsSpread pats =
            let isSpread = \case
                  Typed _ _ _ (PSpread _) -> True
                  _                       -> False
            in  case find isSpread pats of
                  Just _  -> True
                  Nothing -> False

          compilePattern :: String -> Pattern -> String
          compilePattern scope (Typed _ _ _ pat) = case pat of
            PVar _  -> "true"
            PAny    -> "true"
            PNum  n -> scope <> " === " <> n
            PStr  n -> scope <> " === " <> n
            PChar n -> scope <> " === " <> "__String.fromCharCode(" <> (show . fromEnum) n <> ")"
            PBool n -> scope <> " === " <> n
            PCon n [] -> scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\""
            PCon n ps ->
              let args = intercalate " && " $ filter (not . null) $ compileCtorArg scope n <$> zip [0 ..] ps
              in  scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\"" <> if not (null args)
                    then " && " <> args
                    else ""
            PRecord m     -> intercalate " && " $ filter (not . null) $ M.elems $ M.mapWithKey (compileRecord scope) m

            PSpread pat   -> compilePattern scope pat
            PList   items -> compileListPattern scope items
            PTuple  items -> compileTuplePattern scope items


          compileIs :: Is -> String
          compileIs (Typed _ (Area (Loc _ l _) _) _ (Is pat exp)) =
            "if ("
              <> (if coverage then "__hp('" <> astPath <> "', 'line', " <> show l <> ", " <> show l <> ") || " else "")
              <> compilePattern "__x__" pat
              <> ") {\n"
              <> buildVars "__x__" pat
              <> (if Maybe.isJust (recursionData env) then "    " else "    return ")
              <> compile env config exp
              <> ";\n  }\n"

          buildVars :: String -> Pattern -> String
          buildVars _ Untyped{} = undefined
          buildVars v (Typed _ _ _ pat) = case pat of
            PRecord fields ->
              "    let { "
                <> intercalate
                     ", "
                     (filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields)
                <> " } = "
                <> v
                <> ";\n"

            PList  items -> buildListVars v items
            PTuple items -> buildTupleVars v items

            PCon _ ps   -> concat $ (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "]") p) <$> zip [0 ..] ps
            PVar n       -> "    let " <> generateSafeName n <> " = " <> v <> ";\n"

            _            -> ""

          buildFieldVar :: String -> Pattern -> String
          buildFieldVar _ Untyped{} = undefined
          buildFieldVar name (Typed _ _ _ pat) = case pat of
            PSpread (Typed _ _ _ (PVar n)) ->
              "..." <> generateSafeName n

            PVar n ->
              if null name then
                generateSafeName n
              else
                name <> ": " <> generateSafeName n

            PRecord fields ->
              name
                <> ": { "
                <> intercalate
                     ", "
                     (filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields)
                <> " }"

            PCon _ args -> if null name
              then "{ __args: [" <> intercalate ", " (buildFieldVar "" <$> args) <> "] }"
              else
                name
                <> ": "
                <> "{ __args: ["
                <> intercalate ", " ((\(i, arg) -> buildFieldVar "" arg) <$> zip [0 ..] args)
                <> "] }"

            PList  pats ->
              name <> ": " <> packListVars (buildListItemVar <$> pats)

            PTuple pats ->
              name <> ": [" <> intercalate ", " (buildTupleItemVar <$> pats) <> "]"

            _           ->
              ""


          buildListVars :: String -> [Pattern] -> String
          buildListVars scope items =
            if null items then
              ""
            else
              let lastItem = case last items of
                    item@(Typed _ _ _ (PSpread _)) ->
                      buildListItemVar item

                    item ->
                      let built = buildListItemVar item
                      in  if null built then
                        ""
                      else
                        "{ v: " <> buildListItemVar item <> " }"
                  itemsStr = (buildListItemVar <$> init items) ++ [lastItem]
                  packed = packListVars itemsStr
              in  if null packed then
                    ""
                  else
                    "    let " <> packListVars' itemsStr <> " = " <> scope <> ";\n"

          buildListItemVar :: Pattern -> String
          buildListItemVar Untyped{} = undefined
          buildListItemVar (Typed _ _ _ pat) = case pat of
            PSpread (Typed _ _ _ (PVar n)) ->
              generateSafeName n

            PVar n ->
              generateSafeName n

            PCon _ args ->
              let built = intercalate ", " $ buildListItemVar <$> args
              in "{ __args: [" <> built <> "]}"

            PList pats ->
              packListVars $ buildListItemVar <$> pats

            PTuple pats ->
              "[" <> intercalate ", " (buildListItemVar <$> pats) <> "]"

            PRecord fields ->
              "{ " <> intercalate ", " (M.elems $ M.mapWithKey buildFieldVar fields) <> " }"

            _              ->
              ""


          packListVars :: [String] -> String
          packListVars items = case items of
            [last] ->
              "{ v: " <> last <> " }"

            _ ->
              packListVars' items

          packListVars' :: [String] -> String
          packListVars' items = case items of
            [last] ->
              last

            (item : more) ->
              let next = packListVars' more
                  item' =
                    if null item then
                      ""
                    else
                      "v: " <> item <> ", "
                  next' =
                    if null next then
                      ""
                    else
                      "n: " <> next
              in  if null item' && null next' then
                    ""
                  else
                    "{ " <> item' <> next' <> " }"

            [] ->
              ""


          buildTupleVars :: String -> [Pattern] -> String
          buildTupleVars scope items =
            let itemsStr = buildTupleItemVar <$> items
            in  "    let [" <> intercalate "," itemsStr <> "] = " <> scope <> ";\n"

          buildTupleItemVar :: Pattern -> String
          buildTupleItemVar (Typed _ _ _ pat) = case pat of
            PSpread (Typed _ _ _ (PVar n)) -> "..." <> generateSafeName n
            PVar    n      -> generateSafeName n
            PCon _ args   -> let built = intercalate ", " $ buildTupleItemVar <$> args in "{ __args: [" <> built <> "]}"
            PList   pats   -> packListVars $ buildListItemVar <$> pats
            PTuple  pats   -> "[" <> intercalate ", " (buildTupleItemVar <$> pats) <> "]"
            PRecord fields -> "{ " <> intercalate ", " (M.elems $ M.mapWithKey buildFieldVar fields) <> " }"
            _              -> ""

          compileRecord :: String -> Name -> Pattern -> String
          compileRecord scope n p = compilePattern (scope <> "." <> n) p

          compileCtorArg :: String -> String -> (Int, Pattern) -> String
          compileCtorArg scope _ (x, p) = compilePattern (scope <> ".__args[" <> show x <> "]") p

        Extern (ps :=> t) name foreignName ->
          let paramTypes = getParamTypes t
              paramCount = length ps + length paramTypes
              params = take paramCount ((:"")<$> ['a' ..])
              compiledParams = concat $ (<> " => ") <$> params
              body = foreignName <> concat ((\arg -> "(" <> arg <> ")") <$> params)
          in  "let " <> name <> " = " <> compiledParams <> body

        _ -> "/* Not implemented: " <> ppShow exp <> "*/\n"


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable TypeDecl where
  compile _ _ (Untyped _ _ Alias{}                     ) = ""
  compile _ _ (Untyped _ _ ADT { adtconstructors = [] }) = ""
  compile env config (Untyped _ _ adt) =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>) "" (addExport exported . compile env config <$> ctors)
   where
    addExport :: Bool -> String -> String
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  compile _ config (Untyped _ _ (Constructor cname cparams _)) =
    let coverage  = cccoverage config
        optimized = ccoptimize config
    in  case cparams of
          [] -> "let " <> cname <> " = " <> compileBody cname cparams <> ";\n"
          _ ->
            "let " <> cname <> " = " <> "(" <> compileParams cparams <> " => " <> compileBody cname cparams <> ");\n"
   where
    compileParams n = let argNames = (: []) <$> take (length n) ['a' ..] in intercalate " => " argNames

    compileBody n a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          args     = argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"


compileImport :: CompilationConfig -> Import -> String
compileImport config (Untyped _ _ imp) = case imp of
  NamedImport names _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import { " <> compileNames (generateSafeName . Core.getValue <$> names) <> " } from \"" <> importPath <> "\""
    where compileNames names = if null names then "" else (init . init . concat) $ (++ ", ") <$> names
  DefaultImport alias _ absPath ->
    let importPath = buildImportPath config absPath
    in  "import " <> Core.getValue alias <> " from \"" <> importPath <> "\""


buildImportPath :: CompilationConfig -> FilePath -> FilePath
buildImportPath config absPath =
  let outputPath = ccoutputPath config
      rootPath   = ccrootPath config
      astPath    = ccastPath config
      destPath   = dropFileName $ computeTargetPath outputPath rootPath astPath
      depPath    = computeTargetPath outputPath rootPath absPath
  in  convertWindowsSeparators $ cleanRelativePath $ replaceExtension (joinPath ["./", makeRelativeEx destPath depPath]) ".mjs"


updateASTPath :: FilePath -> CompilationConfig -> CompilationConfig
updateASTPath astPath config = config { ccastPath = astPath }

instance Compilable Core.Interface where
  compile _ config (Untyped _ _ interface) = case interface of
    Core.Interface name _ _ _ _ -> getGlobalForTarget (cctarget config) <> "." <> name <> " = {};\n"

instance Compilable Core.Instance where
  compile env config (Untyped _ _ inst) = case inst of
    Core.Instance "Eq" _ _ _ -> ""

    Core.Instance interface _ typings dict -> interface <> "['" <> typings <> "'] = {};\n" <> concat
      (uncurry compileMethod <$> M.toList (M.map fst dict))
     where
      compileMethod :: Name -> Exp -> String
      compileMethod n (Core.Typed (_ :=> t) (Area (Loc _ line _) _) _ (Core.Assignment _ exp)) =
        let
          (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
          content'                       = if cccoverage config && isFunctionType t
            then "__hpFnWrap('" <> ccastPath config <> "', " <> show line <> ", '" <> n <> "')(" <> content <> ")"
            else content
          instRoot = interface <> "['" <> typings <> "']"
          compiledMethod =
            instRoot
              <> "['"
              <> n
              <> "'] = () => "
              <> placeholders
              <> content'
              <> ";\n"
        in
          if not (null dicts)
            then compiledMethod
            else instRoot <> "['" <> n <> "'] = () => " <> content' <> ";\n"
      -- compileMethod :: Name -> Exp -> String
      -- compileMethod n (Core.Typed t (Area (Loc _ line _) _) (Core.Assignment _ exp)) =
      --   let
      --     (placeholders, dicts, content) = compileAssignmentWithPlaceholder env config exp
      --     content'                       = if cccoverage config && isFunctionType t
      --       then "__hpFnWrap('" <> ccastPath config <> "', " <> show line <> ", '" <> n <> "')(" <> content <> ")"
      --       else content
      --     instRoot = interface <> "['" <> typings <> "']"
      --     compiledNDMethod =
      --       "let __"
      --         <> interface
      --         <> typings
      --         <> n
      --         <> " = "
      --         <> onceFnName (ccoptimize config)
      --         <> "(() => "
      --         <> content'
      --         <> ");\n"
      --     compiledMethod =
      --       instRoot
      --         <> "['"
      --         <> n
      --         <> "'] = () => "
      --         <> placeholders
      --         <> "{\n  "
      --         <> intercalate
      --              "\n  "
      --              ((\dict -> getGlobalForTarget (cctarget config) <> "." <> dict <> " = " <> dict) <$> dicts)
      --         <> "\n  return __"
      --         <> interface
      --         <> typings
      --         <> n
      --         <> "();\n};\n"
      --   in
      --     if not (null dicts)
      --       then compiledNDMethod <> compiledMethod
      --       else instRoot <> "['" <> n <> "'] = () => " <> content' <> ";\n"


compileAssignmentWithPlaceholder :: Env -> CompilationConfig -> Exp -> (String, [String], String)
compileAssignmentWithPlaceholder env config fullExp@(Core.Typed _ _ _ exp) = case exp of
  Placeholder (ClassRef cls _ call var, ts) e -> if not call
    then
      let dict                      = generateRecordName (ccoptimize config) cls ts var
          (phs, dicts, compiledExp) = compileAssignmentWithPlaceholder env config e
      in  ("(" <> dict <> ") => " <> phs, dicts <> [dict], compiledExp)
    else ("", [], compile env config fullExp)

  _ -> ("", [], compile env config fullExp)

generateRecordName :: Bool -> String -> String -> Bool -> String
generateRecordName optimized cls ts var =
  if var then if optimized then cls <> ts else cls <> "_" <> ts else cls <> "." <> ts

instance Compilable AST where
  compile env config ast =
    let entrypointPath     = ccentrypointPath config
        coverage           = cccoverage config
        internalsPath      = ccinternalsPath config
        exps               = aexps ast
        typeDecls          = atypedecls ast
        path               = apath ast
        imports            = aimports ast
        interfaces         = ainterfaces ast
        instances          = ainstances ast


        astPath            = fromMaybe "Unknown" path

        configWithASTPath  = updateASTPath astPath config

        infoComment        = "// file: " <> astPath <> "\n"

        compiledInterfaces = case interfaces of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledInstances = case instances of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledAdts = case typeDecls of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledExps = case exps of
          [] -> ""
          x  -> compileExps env configWithASTPath exps
          -- x  -> foldr1 (<>) (terminate . compile env configWithASTPath <$> x)
        compiledImports = case imports of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compileImport configWithASTPath <$> x) <> "\n"
        defaultExport = buildDefaultExport typeDecls exps
    in  infoComment
          <> (if entrypointPath == astPath then "import {} from \"" <> internalsPath <> "\"\n" else "")
          <> compiledImports
          <> compiledAdts
          <> compiledInterfaces
          <> compiledInstances
          <> compiledExps
          <> defaultExport
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"


compileExps :: Env -> CompilationConfig -> [Exp] -> String
compileExps env config [exp     ] = compile env config exp <> ";\n"
compileExps env config (exp : es) = case exp of
  Core.Typed _ _ _ (Core.Assignment name _) ->
    let nextEnv = env { varsInScope = S.insert name (varsInScope env) }
    in  compile env config exp <> ";\n" <> compileExps nextEnv config es

  _ -> compile env config exp <> ";\n" <> compileExps env config es


buildDefaultExport :: [TypeDecl] -> [Exp] -> String
buildDefaultExport as es =
  let expExportNames    = generateSafeName . getExportName <$> filter isExport es
      adtExportNames    = getConstructorName <$> concat (adtconstructors . getValue <$> filter isADTExport as)
      allDefaultExports = expExportNames <> adtExportNames
  in  case allDefaultExports of
        []      -> "export default {};\n"
        exports -> "export default { " <> intercalate ", " exports <> " };\n"

 where
  isADTExport :: TypeDecl -> Bool
  isADTExport (Untyped _ _ ADT { adtexported }) = adtexported
  isADTExport _                               = False

  isExport :: Exp -> Bool
  isExport a = case a of
    Typed _ _ _ (Export _) -> True

    _ -> False

  getExportName :: Exp -> String
  getExportName (Typed _ _ _ (Export (Typed _ _ _ (Assignment n _)))) = n
  getExportName (Typed _ _ _ (Export (Typed _ _ _ (Extern _ n _))))   = n


  getConstructorName :: Constructor -> String
  getConstructorName (Untyped _ _ (Constructor cname _ _)) = cname





rollupNotFoundMessage :: String
rollupNotFoundMessage = unlines
  [ "Compilation error:"
  , "Rollup was not found."
  , "You must have rollup installed in order to use the bundling option. Please visit this page in order to install it: https://rollupjs.org/guide/en/#installation"
  ]

runBundle :: FilePath -> IO (Either String (String, String))
runBundle entrypointCompiledPath = do
  putStrLn "Bundling.."
  rollupPath        <- try $ getEnv "ROLLUP_PATH"
  rollupPathChecked <- case (rollupPath :: Either IOError String) of
    Left _ -> do
      r <-
        try (readProcessWithExitCode "rollup" ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left  err -> do
          putStrLn $ ppShow err
          return $ Left rollupNotFoundMessage
        Right _ -> return $ Right "rollup"
    Right p -> do
      r <- try (readProcessWithExitCode p ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left err -> do
          putStrLn $ ppShow err
          r <-
            try (readProcessWithExitCode "rollup" ["--version"] "") :: IO
              (Either SomeException (ExitCode, String, String))
          case r of
            Left  err -> do
              putStrLn $ ppShow err
              return $ Left rollupNotFoundMessage
            Right _ -> return $ Right "rollup"
        Right _ -> return $ Right p

  case rollupPathChecked of
    Right rollup -> do
      r <-
        try
          (readProcessWithExitCode
            rollup
            [ entrypointCompiledPath
            , "--format"
            , "umd"
            , "--name"
            , "exe"
            , "-p"
            , "@rollup/plugin-node-resolve"
            , "--silent"
            ]
            ""
          ) :: IO (Either SomeException (ExitCode, String, String))

      case r of
        Left  e ->
          return $ Left (ppShow e)

        Right (_, stdout, stderr) -> do
          return $ Right (stdout, stderr)

    Left e ->
      return $ Left e


generateInternalsModule :: Options -> IO ()
generateInternalsModule options = do
  writeFile (takeDirectory (optOutputPath options) <> (pathSeparator : "__internals__.mjs"))
    $ generateInternalsModuleContent (optTarget options) (optOptimized options) (optCoverage options)


computeInternalsPath :: FilePath -> FilePath -> FilePath
computeInternalsPath rootPath astPath = case stripPrefix rootPath astPath of
  Just s ->
    let dirs = splitDirectories (takeDirectory s)
        minus
          | joinPath ["prelude", "__internal__"] `isInfixOf` astPath = if joinPath ["prelude", "__internal__"] `isInfixOf` rootPath then 0 else 2
          | "madlib_modules" `isInfixOf` astPath && not (rootPath `isPrefixOf` astPath) = -2
          | otherwise = 1
        dirLength = length dirs - minus
    in  joinPath $ ["./"] <> replicate dirLength ".." <> ["__internals__.mjs"]

  Nothing ->
    if joinPath ["prelude", "__internal__"] `isInfixOf` astPath then
      let dirLength = length $ dropWhile (/= "__internal__") $ splitDirectories (takeDirectory astPath)
      in  joinPath $ ["./"] <> replicate dirLength ".." <> ["__internals__.mjs"]
    else
      "./__internals__.mjs"


generateJSModule :: Options -> [FilePath] -> Core.AST -> IO String
generateJSModule options pathsToBuild ast@Core.AST { Core.apath = Nothing } = return ""
generateJSModule options pathsToBuild ast@Core.AST { Core.apath = Just path }
  = do
    let rootPath           = optRootPath options
        internalsPath      = convertWindowsSeparators $ computeInternalsPath rootPath path
        entrypointPath     = if path `elem` pathsToBuild then path else optEntrypoint options
        computedOutputPath = computeTargetPath (takeDirectory (optOutputPath options)) rootPath path

    let moduleContent = compile
          initialEnv
          (
            CompilationConfig
              rootPath
              path
              entrypointPath
              computedOutputPath
              (optCoverage options)
              (optOptimized options)
              (optTarget options)
              internalsPath
          )
          ast

    let rest = List.dropWhile (/= path) pathsToBuild
    let total = List.length pathsToBuild
    let curr = total - List.length rest + 1
    let currStr = if curr < 10 then " " <> show curr else show curr
    Prelude.putStrLn $ "[" <> currStr <> " of "<> show total<>"] Compiled '" <> path <> "'"

    return moduleContent
