{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
                                                , takeDirectoryIfFile
                                                )
import           System.FilePath                ( replaceExtension
                                                , dropFileName
                                                , joinPath, splitDirectories, takeDirectory, pathSeparator
                                                , splitPath, takeFileName
                                                )
import           Explain.Location
import           Infer.Type
import           Generate.JSInternals
import           Run.Target
import           Text.Show.Pretty               ( ppShow )
import Distribution.Types.Lens (_Impl)
import qualified Data.Maybe as Maybe
import Control.Exception
import GHC.IO.Exception
import System.Process
import System.Environment (getEnv)
import Run.Options
import qualified Data.List as List



data RecursionData
  = PlainRecursionData { rdParams :: [String] }
  | RightListRecursionData { rdParams :: [String] }
  | ConstructorRecursionData { rdParams :: [String] }
  deriving(Eq, Show)

data Env
  = Env
  { varsInScope :: S.Set String
  , recursionData :: Maybe RecursionData
  , varsRewritten :: M.Map String String
  }
  deriving(Eq, Show)

initialEnv :: Env
initialEnv =
  Env
    { varsInScope = S.empty
    , recursionData = Nothing
    , varsRewritten = M.empty
    }

allowedJSNames :: [String]
allowedJSNames = ["delete", "class", "while", "for", "case", "switch", "try", "length", "var", "default", "break", "null"]

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
      , ccoptimize       :: Bool
      , cctarget         :: Target
      , ccinternalsPath  :: FilePath
      }

class Compilable a where
  compile :: Env -> CompilationConfig -> a -> String

instance Compilable Exp where
  compile _ _ Untyped{} = ""
  compile env config e@(Typed qt area metadata exp) =
    let optimized = ccoptimize config
    in case exp of
        _ | isPlainRecursionEnd metadata ->
          let compiledExp = compile env config (Typed qt area [] exp)
          in  "($_result_ = " <> compiledExp <> ")"

        _ | isRightListRecursionEnd metadata ->
          let compiledExp = compile env config (Typed qt area [] exp)
          in  "($_end_.n = " <> compiledExp <> ", $_result_ = $_start_.n)"

        _ | isConstructorRecursionEnd metadata ->
          let compiledExp = compile env config (Typed qt area [] exp)
          in  "($args[$index] = "<>compiledExp<>", $_result_ = $_start_.__args[0])"

        Call (Typed _ _ _ (Var constructorName True)) args | isConstructorRecursiveCall metadata ->
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
          v

        Literal (LFloat v) ->
          v

        Literal (LStr (leading : v)) | leading == '"' || leading == '\'' ->
          if null v then
            "``"
          else
            "`" <> init (escapeStringLiteral v) <> "`"

        Literal (LStr v) ->
          "`" <> escapeStringLiteral v <> "`"

        Literal (LChar v) ->
          "String.fromCharCode(" <> (show . fromEnum) v <> ")"

        Literal (LBool v) ->
          v

        Literal LUnit ->
          "({ __constructor: \"Unit\", __args: [] })"

        TypedHole ->
          "(() => { throw 'Typed hole reached, exiting.' })()"

        Call (Typed _ _ _ (Var "++" _)) [left, right] ->
          compile env config left <> " + " <> compile env config right

        Call (Typed _ _ _ (Var "unary-minus" _)) [arg] ->
          "-" <> compile env config arg

        Call (Typed _ _ _ (Var "+" _)) [left, right] ->
          "(" <> compile env config left <> " + " <> compile env config right <> ")"

        Call (Typed _ _ _ (Var "-" _)) [left, right] ->
          "(" <> compile env config left <> " - " <> compile env config right <> ")"

        Call (Typed _ _ _ (Var "*" _)) [left, right] ->
          "(" <> compile env config left <> " * " <> compile env config right <> ")"

        Call (Typed _ _ _ (Var "/" _)) [left, right] ->
          "(" <> compile env config left <> " / " <> compile env config right <> ")"

        Call (Typed _ _ _ (Var "%" _)) [left, right] ->
          compile env config left <> " % " <> compile env config right

        Call (Typed _ _ _ (Var "==" _)) [left, right] ->
          eqFnName optimized
          <> "("
          <> compile env config left
          <> ", "
          <> compile env config right
          <> ")"

        Call (Typed _ _ _ (Var "!=" _)) [left, right] ->
          "!"
          <> eqFnName optimized
          <> "("
          <> compile env config left
          <> ", "
          <> compile env config right
          <> ")"

        Call (Typed _ _ _ (Var "&&" _)) [left, right] ->
          compile env config left <> " && " <> compile env config right

        Call (Typed _ _ _ (Var "||" _)) [left, right] ->
          compile env config left <> " || " <> compile env config right

        Call (Typed _ _ _ (Var "|" _)) [left, right] ->
          compile env config left <> " | " <> compile env config right

        Call (Typed _ _ _ (Var "&" _)) [left, right] ->
          compile env config left <> " & " <> compile env config right

        Call (Typed _ _ _ (Var "^" _)) [left, right] ->
          compile env config left <> " ^ " <> compile env config right

        Call (Typed _ _ _ (Var "~" _)) [left, right] ->
          compile env config left <> " ~ " <> compile env config right

        Call (Typed _ _ _ (Var "<<" _)) [left, right] ->
          compile env config left <> " << " <> compile env config right

        Call (Typed _ _ _ (Var ">>" _)) [left, right] ->
          compile env config left <> " >> " <> compile env config right

        Call (Typed _ _ _ (Var ">>>" _)) [left, right] ->
          compile env config left <> " >>> " <> compile env config right

        Call (Typed _ _ _ (Var ">" _)) [left, right] ->
          compile env config left <> " > " <> compile env config right

        Call (Typed _ _ _ (Var "<" _)) [left, right] ->
          compile env config left <> " < " <> compile env config right

        Call (Typed _ _ _ (Var ">=" _)) [left, right] ->
          compile env config left <> " >= " <> compile env config right

        Call (Typed _ _ _ (Var "<=" _)) [left, right] ->
          compile env config left <> " <= " <> compile env config right

        Call _ args | isPlainRecursiveCall metadata ->
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

        Call fn [Typed _ _ _ (Literal LUnit)] ->
          compile env config fn <> "()"

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
          compileAbs _ params body =
            let params' = intercalate " => " params
            in  "("
                <> params'
                <> " => "
                <> compileBody env{ varsInScope = varsInScope env <> S.fromList params} body
                <> ")"

          compileBody :: Env -> [Exp] -> String
          compileBody env body = case body of
            es | isPlainRecursiveDefinition metadata ->
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
                  <> "        "<> compileBody' env { recursionData = Just PlainRecursionData { rdParams = ("$$"<>) <$> (getValue <$> params) }, varsRewritten = varsRewritten env <> rewrite } es
                  <> "\n    }\n"
                  <> "    return $_result_;\n"
                  <> "}"

            es | isRightListRecursiveDefinition metadata ->
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
                  <> "        " <> compileBody' env { recursionData = Just RightListRecursionData { rdParams = ("$$"<>) <$> (getValue <$> params) }, varsRewritten = varsRewritten env <> rewrite } es
                  <> "\n    }\n"
                  <> "    return $_result_;\n"
                  <> "}"

            es | isConstructorRecursiveDefinition metadata ->
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
                  <> "        "<> compileBody' env { recursionData = Just ConstructorRecursionData { rdParams = ("$$"<>) <$> (getValue <$> params) }, varsRewritten = varsRewritten env <> rewrite } es
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
            (Typed _ _ _ (JSExp _)) ->
              compile env config exp

            _ | isPlainRecursiveDefinition metadata || isRightListRecursiveDefinition metadata || isConstructorRecursiveDefinition metadata ->
              compile env config exp

            _ ->
              "    return " <> compile env config exp <> ";\n"
          compileBody' e (exp : es) = case exp of
            Core.Typed _ _ _ (Core.Assignment name _) ->
              let nextEnv = e { varsInScope = S.insert name (varsInScope e) }
              in  "    " <> compile e config exp <> ";\n" <> compileBody' nextEnv es

            _ -> "    " <> compile e config exp <> ";\n" <> compileBody' e es

        Var name _ ->
          let safeName  = generateSafeName name
              rewritten = varsRewritten env
          in  fromMaybe safeName (M.lookup safeName rewritten)

        Assignment name exp ->
          let safeName = generateSafeName name
              content = compile env config exp
              needsModifier = notElem safeName $ varsInScope env
          in  (if needsModifier then "let " else "") <> safeName <> " = " <> content

        Export e ->
          "export " <> compile env config e

        NameExport name   -> "export { " <> generateSafeName name <> " }"

        Record     fields -> let fs = intercalate "," $ compileField <$> fields in "({" <> fs <> " })"
         where
          compileField :: Field -> String
          compileField (Typed _ _ _ field) = case field of
            Field (name, exp) ->
              " " <> name <> ": " <> compile env config exp

            FieldSpread exp ->
              " ..." <> compile env config exp

        Access record (Typed _ _ _ (Var name _)) ->
          compile env config record <> name

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
              <> intercalate ";\n  " allExceptLast
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
            PChar n -> scope <> " === " <> "String.fromCharCode(" <> (show . fromEnum) n <> ")"
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
          compileIs (Typed _ _ _ (Is pat exp)) =
            "if ("
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
                <> intercalate ", " ((\(_, arg) -> buildFieldVar "" arg) <$> zip [0 ..] args)
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

        Extern (_ :=> t) name foreignName ->
          let paramTypes = getParamTypes t
              paramCount = length paramTypes
              params = take paramCount ((:"")<$> ['a' ..])
              compiledParams = concat $ (<> " => ") <$> params
              body = foreignName <> concat ((\arg -> "(" <> arg <> ")") <$> params)
          in  "let " <> name <> " = " <> compiledParams <> body

        _ -> "/* Not implemented: " <> ppShow exp <> "*/\n"


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable TypeDecl where
  compile _ _ (Untyped _ _ ADT { adtconstructors = [] }) = ""
  compile env config (Untyped _ _ adt) =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>) "" (addExport exported . compile env config <$> ctors)
   where
    addExport :: Bool -> String -> String
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  compile _ _ (Untyped _ _ (Constructor cname cparams _)) =
    case cparams of
      [] ->
        "let " <> cname <> " = " <> compileBody cname cparams <> ";\n"

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
    in  "import { " <> compileNames (generateSafeName . Core.getImportName <$> names) <> " } from \"" <> importPath <> "\""
    where compileNames names = if null names then "" else (init . init . concat) $ (++ ", ") <$> names


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


generateRecordName :: Bool -> String -> String -> Bool -> String
generateRecordName optimized cls ts var =
  if var then if optimized then cls <> ts else cls <> "_" <> ts else cls <> "." <> ts

instance Compilable AST where
  compile env config ast =
    let entrypointPath     = ccentrypointPath config
        internalsPath      = ccinternalsPath config
        exps               = aexps ast
        typeDecls          = atypedecls ast
        path               = apath ast
        imports            = aimports ast

        astPath            = fromMaybe "Unknown" path
        configWithASTPath  = updateASTPath astPath config
        infoComment        = "// file: " <> astPath <> "\n"

        compiledAdts = case typeDecls of
          [] -> ""
          x  -> foldr1 (<>) (compile env configWithASTPath <$> x)
        compiledExps = case exps of
          [] -> ""
          _  -> compileExps env configWithASTPath exps
          -- x  -> foldr1 (<>) (terminate . compile env configWithASTPath <$> x)
        compiledImports = case imports of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compileImport configWithASTPath <$> x) <> "\n"
        defaultExport = buildDefaultExport typeDecls exps
    in  infoComment
          <> (if entrypointPath == astPath then "import {} from \"" <> internalsPath <> "\"\n" else "")
          <> compiledImports
          <> compiledAdts
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
  putStrLn $ "Bundling with entrypoint '" <> entrypointCompiledPath <> "'"
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


makeInternalsModuleTargetPath :: FilePath -> FilePath
makeInternalsModuleTargetPath outputPath =
  takeDirectoryIfFile outputPath <> (pathSeparator : "__internals__.mjs")


generateInternalsModule :: Options -> IO ()
generateInternalsModule options = do
  writeFile (makeInternalsModuleTargetPath (optOutputPath options))
    $ generateInternalsModuleContent (optTarget options) (optOptimized options) (optCoverage options)


computeInternalsRelativePath :: FilePath -> FilePath -> FilePath
computeInternalsRelativePath outputPath astTargetPath =
  let internalsPath = makeInternalsModuleTargetPath outputPath
      parts = splitPath $ fromMaybe "" $ stripPrefix (dropFileName internalsPath) (dropFileName astTargetPath)
  in  joinPath $ "./" : (".." <$ parts) ++ [takeFileName internalsPath]


generateMainCall :: Options -> String -> String
generateMainCall options astPath =
  if astPath == optEntrypoint options then
    if optTarget options == TNode then
      unlines
        [ "const makeArgs = () => {"
        , "  let list = {}"
        , "  let start = list"
        , "  Object.keys(process.argv.slice(0)).forEach((key) => {"
        , "    list = list.n = { v: process.argv[key], n: null }"
        , "  }, {})"
        , "  return {"
        , "    n: start.n.n.n,"
        , "    v: start.n.n.v"
        , "  }"
        , "}"
        , "main(makeArgs())"
        ]
    else
      "\nmain(null)\n"
  else
    ""


generateJSModule :: Options -> [FilePath] -> Core.AST -> IO String
generateJSModule _ _ Core.AST { Core.apath = Nothing } = return ""
generateJSModule options pathsToBuild ast@Core.AST { Core.apath = Just path }
  = do
    let rootPath           = optRootPath options
        computedOutputPath = computeTargetPath (optOutputPath options) rootPath path
        internalsPath      = convertWindowsSeparators $ computeInternalsRelativePath (optOutputPath options) computedOutputPath
        entrypointPath     = if path `elem` pathsToBuild then path else optEntrypoint options

    let moduleContent = compile
          initialEnv
          (
            CompilationConfig
              rootPath
              path
              entrypointPath
              computedOutputPath
              (optOptimized options)
              (optTarget options)
              internalsPath
          )
          ast
    let moduleContent' = moduleContent <> generateMainCall options path

    let rest = List.dropWhile (/= path) pathsToBuild
    let total = List.length pathsToBuild
    let curr = total - List.length rest + 1
    let currStr = if curr < 10 then " " <> show curr else show curr
    Prelude.putStrLn $ "[" <> currStr <> " of "<> show total<>"] Compiled '" <> path <> "'"

    return moduleContent'
