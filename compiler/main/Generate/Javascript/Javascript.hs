{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Generate.Javascript.Javascript where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( sort
                                                , find
                                                , intercalate
                                                , foldl', stripPrefix, isInfixOf, isPrefixOf
                                                )
import           Data.Char                      ( isSpace )
import           Data.Char                      ( ord )
import           Numeric                        ( showHex )
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
import           Infer.Type
import           Generate.Javascript.Internals
import           Generate.Javascript.Doc
import           Generate.Javascript.ModuleDoc
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
import Data.IORef
import Infer.MonomorphizationState
import qualified Data.ByteString.Lazy.Char8 as BLChar8
import Utils.Hash (generateHashFromPath, addHashToName)



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
  , methodNames :: S.Set String
  , inBody :: Bool
  }
  deriving(Eq, Show)

initialEnv :: Env
initialEnv =
  Env
    { varsInScope = S.empty
    , recursionData = Nothing
    , varsRewritten = M.empty
    , methodNames = S.empty
    , inBody = False
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


escapeJSString :: String -> String
escapeJSString s = case s of
  '\n' : next ->
    '\\' : 'n' : escapeJSString next

  '\r' : next ->
    '\\' : 'r' : escapeJSString next

  '\t' : next ->
    '\\' : 't' : escapeJSString next

  '\b' : next ->
    '\\' : 'b' : escapeJSString next

  '\f' : next ->
    '\\' : 'f' : escapeJSString next

  '"' : next ->
    '\\' : '"' : escapeJSString next

  '\\' : next ->
    '\\' : '\\' : escapeJSString next

  c : next | ord c < 32 ->
    "\\u" <> leftPad4 (showHex (ord c) "") <> escapeJSString next

  c : next ->
    c : escapeJSString next

  "" ->
    ""
 where
  leftPad4 hexDigits = replicate (4 - length hexDigits) '0' <> hexDigits

renderJSStringLiteral :: String -> String
renderJSStringLiteral value = "\"" <> escapeJSString value <> "\""

replaceAll :: String -> String -> String -> String
replaceAll needle replacement haystack
  | null needle = haystack
  | otherwise = go haystack
 where
  go xs
    | needle `isPrefixOf` xs =
        replacement <> go (drop (length needle) xs)
    | otherwise = case xs of
        c : rest ->
          c : go rest

        [] ->
          []

normalizeJSRawBlock :: String -> String
normalizeJSRawBlock raw =
  let trimmed = reverse (dropWhile isSpace (reverse (dropWhile isSpace raw)))
      shouldNormalize =
        not (null trimmed)
          && head trimmed == '{'
          && last trimmed == '}'
          && not (";" `isInfixOf` trimmed)
  in  if not (null trimmed) && head trimmed == '{' && last trimmed == '}' then
        if shouldNormalize then
          replaceAll " return " "; return "
            $ replaceAll " throw " "; throw "
            $ replaceAll " let " "; let "
            $ replaceAll " const " "; const "
            $ replaceAll " var " "; var "
            $ trimmed
        else
          trimmed
      else
        raw

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

codegenPanic :: String -> a
codegenPanic msg =
  error ("Generate.Javascript: " <> msg)

docJoin :: JSDoc -> [JSDoc] -> JSDoc
docJoin _ [] = mempty
docJoin sep (first : rest) =
  foldl' (\acc next -> acc <> sep <> next) first rest

docJoinText :: String -> [JSDoc] -> JSDoc
docJoinText sep = docJoin (docText sep)

docParens :: JSDoc -> JSDoc
docParens content = docText "(" <> content <> docText ")"

docCommaSeparatedLines :: [JSDoc] -> JSDoc
docCommaSeparatedLines docs = case docs of
  [] ->
    mempty

  first : rest ->
    foldl' (\acc next -> acc <> docText "," <> docLine' <> next) first rest

docApplyChain :: JSDoc -> [JSDoc] -> JSDoc
docApplyChain fnDoc argDocs =
  foldl'
    (\acc argDoc -> docGroup (acc <> docLine' <> docParens (docNest 2 (docLine' <> argDoc) <> docLine')))
    fnDoc
    argDocs

docCurriedLambda :: [String] -> JSDoc -> JSDoc
docCurriedLambda names bodyDoc =
  foldr
    (\name acc -> docText name <> docText " =>" <> docLine <> acc)
    bodyDoc
    names

class Compilable a where
  emit :: Env -> CompilationConfig -> a -> JSDoc

instance Compilable Exp where
  emit = emitExp

emitExp :: Env -> CompilationConfig -> Exp -> JSDoc
emitExp _ _ Untyped{} = mempty
emitExp env config (Typed qt area metadata exp) =
    let optimized = ccoptimize config
    in case exp of
        _ | isPlainRecursionEnd metadata ->
          let compiledExp = emitExp env config (Typed qt area [] exp)
          in  "($_result_ = " <> compiledExp <> ")"

        _ | isRightListRecursionEnd metadata ->
          let compiledExp = emitExp env config (Typed qt area [] exp)
          in  "($_end_.n = " <> compiledExp <> ", $_result_ = $_start_.n)"

        _ | isConstructorRecursionEnd metadata ->
          let compiledExp = emitExp env config (Typed qt area [] exp)
          in  "($args[$index] = "<>compiledExp<>", $_result_ = $_start_.__args[0])"

        Call (Typed _ _ _ (Var constructorName True)) args | isConstructorRecursiveCall metadata ->
          case getConstructorRecursionInfo metadata of
            Just (ConstructorRecursionInfo _ position) ->
              let params = case recursionData env of
                    Just rd ->
                      rdParams rd

                    Nothing ->
                      codegenPanic
                        "constructor recursion call without recursion context"
                  -- Based on the following input, we need to only use ConstructorName
                  -- __6bb57939a0e365f381d7e05ce50bfeb1__ConstructorName
                  -- therefore we need to drop the first 36 characters
                  newValue = "$newValue = { __constructor: \""<> drop 7 constructorName <>"\", __args: [] }"
                  compiledArgs =
                    (\(index, arg) ->
                      if index == position then
                        "$newValue.__args.push(null)"
                      else
                        "$newValue.__args.push(" <> emitExp env config arg <> ")"
                    ) <$> zip [0..] args
                  updateParams = case drop position args of
                    Core.Typed _ _ _ (Core.Call _ recArgs) : _ ->
                      let compiledRecArgs  = emitExp env config <$> recArgs
                      in  (\(param, arg) -> docText param <> " = " <> arg) <$> zip params compiledRecArgs

                    _ ->
                      codegenPanic
                        ("invalid constructor recursion argument layout at position " <> show position)
              in  "("
                  <> docText newValue
                  <> ", "
                  <> docJoinText ", " compiledArgs
                  <> ", $args[$index] = $newValue, $args = $newValue.__args, $index = "
                  <> docText (show position)
                  <> ", "
                  <> docJoinText ", " updateParams
                  <> ", $_continue_ = true)"

            _ ->
              codegenPanic "missing constructor recursion info"

        Literal (LNum v) ->
          docText v

        Literal (LFloat v) ->
          docText v

        Literal (LStr v) ->
          docText (renderJSStringLiteral v)

        Literal (LChar v) ->
          "String.fromCodePoint(" <> docText ((show . fromEnum) v) <> ")"

        Literal (LBool v) ->
          docText v

        Literal LUnit ->
          "({ __constructor: \"Unit\", __args: [] })"

        TypedHole ->
          "(() => { throw 'Typed hole reached, exiting.' })()"

        Call (Typed _ _ _ (Var "++" _)) [left, right] ->
          "(" <> emitExp env config left <> " + " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "unary-minus" _)) [arg] ->
          "(-" <> emitExp env config arg <> ")"

        Call (Typed _ _ _ (Var "+" _)) [left, right] ->
          "(" <> emitExp env config left <> " + " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "-" _)) [left, right] ->
          "(" <> emitExp env config left <> " - " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "*" _)) [left, right] ->
          "(" <> emitExp env config left <> " * " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "/" _)) [left, right] ->
          "(" <> emitExp env config left <> " / " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "%" _)) [left, right] ->
          "(" <> emitExp env config left <> " % " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "==" _)) [left, right] ->
          docText (eqFnName optimized)
          <> "("
          <> emitExp env config left
          <> ", "
          <> emitExp env config right
          <> ")"

        Call (Typed _ _ _ (Var "!=" _)) [left, right] ->
          "!"
          <> docText (eqFnName optimized)
          <> "("
          <> emitExp env config left
          <> ", "
          <> emitExp env config right
          <> ")"

        Call (Typed _ _ _ (Var "&&" _)) [left, right] ->
          "(" <> emitExp env config left <> " && " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "||" _)) [left, right] ->
          "(" <> emitExp env config left <> " || " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "|" _)) [left, right] ->
          "(" <> emitExp env config left <> " | " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "&" _)) [left, right] ->
          "(" <> emitExp env config left <> " & " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "^" _)) [left, right] ->
          "(" <> emitExp env config left <> " ^ " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "~" _)) [left, right] ->
          "(" <> emitExp env config left <> " ~ " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "<<" _)) [left, right] ->
          "(" <> emitExp env config left <> " << " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var ">>" _)) [left, right] ->
          "(" <> emitExp env config left <> " >> " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var ">>>" _)) [left, right] ->
          "(" <> emitExp env config left <> " >>> " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var ">" _)) [left, right] ->
          "(" <> emitExp env config left <> " > " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "<" _)) [left, right] ->
          "(" <> emitExp env config left <> " < " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var ">=" _)) [left, right] ->
          "(" <> emitExp env config left <> " >= " <> emitExp env config right <> ")"

        Call (Typed _ _ _ (Var "<=" _)) [left, right] ->
          "(" <> emitExp env config left <> " <= " <> emitExp env config right <> ")"

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

              compiledArgs = emitExp env config <$> args
              updateParams  = (\(param, arg) -> docText param <> " = " <> arg) <$> zip params compiledArgs
          in  if null updateParams then
                "($_continue_ = true)"
              else
                "(" <> docJoinText ", " updateParams <> ", $_continue_ = true)"

        Call fn [Typed _ _ _ (Literal LUnit)] ->
          emitExp env config fn <> "()"

        Call fn args ->
          let compiledFn = emitExp env config fn
              compiledArgs = emitExp env config <$> args
          in  docApplyChain compiledFn compiledArgs

        If cond truthy falsy ->
          "("
            <> emitExp env config cond
            <> " ? "
            <> emitExp env config truthy
            <> " : "
            <> emitExp env config falsy
            <> ")"

        While cond body ->
          "while("
            <> emitExp env config cond
            <> ") "
            <> docBlock [emitExp env config body]

        Definition params body -> compileAbs Nothing (getValue <$> params) body
         where
          compileAbs :: Maybe [Exp] -> [Name] -> [Exp] -> JSDoc
          compileAbs _ params body =
            let safeParams = generateSafeName <$> params
                safeParamNames = S.fromList safeParams
                innerEnv = env
                  { varsInScope = varsInScope env <> S.fromList params
                  , varsRewritten = M.filterWithKey (\k _ -> k `S.notMember` safeParamNames) (varsRewritten env)
                  , inBody = True
                  }
            in  "("
                <> docGroup (docCurriedLambda safeParams (compileBody innerEnv body))
                <> ")"

          compileBody :: Env -> [Exp] -> JSDoc
          compileBody env body = case body of
            es | isPlainRecursiveDefinition metadata ->
              let safeParams  = generateSafeName <$> (getValue <$> params)
                  tcoParams   = (\p -> "let $$" <> docText p <> " = " <> docText p <> ";") <$> safeParams
                  updateParams = (\p -> "let $" <> docText p <> " = $$" <> docText p <> ";") <$> safeParams
                  rewrite     = M.fromList ((\p -> (p, "$"<>p)) <$> safeParams)
              in  docBlock
                    $ [ "let $_result_;"
                      , "let $_continue_ = true;"
                      ]
                   <> tcoParams
                   <> [ "while($_continue_) "
                        <> docBlock
                             (updateParams
                             <> [ "$_continue_ = false;"
                                , compileBody' env { recursionData = Just PlainRecursionData { rdParams = ("$$"<>) <$> safeParams }, varsRewritten = varsRewritten env <> rewrite } es
                                ]
                             )
                      , "return $_result_;"
                      ]

            es | isRightListRecursiveDefinition metadata ->
              let safeParams  = generateSafeName <$> (getValue <$> params)
                  tcoParams   = (\p -> "let $$" <> docText p <> " = " <> docText p <> ";") <$> safeParams
                  updateParams = (\p -> "let $" <> docText p <> " = $$" <> docText p <> ";") <$> safeParams
                  rewrite     = M.fromList ((\p -> (p, "$"<>p)) <$> safeParams)
              in  docBlock
                    $ [ "let $_result_;"
                      , "let $_continue_ = true;"
                      , "let $_start_ = {};"
                      , "let $_end_ = $_start_;"
                      ]
                   <> tcoParams
                   <> [ "while($_continue_) "
                        <> docBlock
                             (updateParams
                             <> [ "$_continue_ = false;"
                                , compileBody' env { recursionData = Just RightListRecursionData { rdParams = ("$$"<>) <$> safeParams }, varsRewritten = varsRewritten env <> rewrite } es
                                ]
                             )
                      , "return $_result_;"
                      ]

            es | isConstructorRecursiveDefinition metadata ->
              let safeParams  = generateSafeName <$> (getValue <$> params)
                  tcoParams   = (\p -> "let $$" <> docText p <> " = " <> docText p <> ";") <$> safeParams
                  updateParams = (\p -> "let $" <> docText p <> " = $$" <> docText p <> ";") <$> safeParams
                  rewrite     = M.fromList ((\p -> (p, "$"<>p)) <$> safeParams)
              in  docBlock
                    $ [ "let $_result_;"
                      , "let $_continue_ = true;"
                      , "let $_start_ = { __args: [] };"
                      , "let $args = $_start_.__args;"
                      , "let $index = 0;"
                      , "let $newValue;"
                      ]
                   <> tcoParams
                   <> [ "while($_continue_) "
                        <> docBlock
                             (updateParams
                             <> [ "$_continue_ = false;"
                                , compileBody' env { recursionData = Just ConstructorRecursionData { rdParams = ("$$"<>) <$> safeParams }, varsRewritten = varsRewritten env <> rewrite } es
                                ]
                             )
                      , "return $_result_;"
                      ]

            [exp] ->
              emitExp env config exp

            exps ->
              docBlock [compileBody' env exps]


          isJSBlock :: String -> Bool
          isJSBlock raw = case dropWhile isSpace raw of
            '{' : _ ->
              True

            _ ->
              False

          compileBody' :: Env -> [Exp] -> JSDoc
          compileBody' env [exp] = case exp of
            (Typed _ _ _ (JSExp raw)) ->
              if isJSBlock raw then
                docText (normalizeJSRawBlock raw)
              else
                "return (" <> docText raw <> ");"

            _ | isPlainRecursiveDefinition metadata || isRightListRecursiveDefinition metadata || isConstructorRecursiveDefinition metadata ->
              emitExp env config exp

            _ ->
              "return " <> emitExp env config exp <> ";"
          compileBody' e (exp : es) = case exp of
            Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _) ->
              let safeName = generateSafeName name
                  nextEnv = e
                    { varsInScope = S.insert name (varsInScope e)
                    , varsRewritten = M.delete safeName (varsRewritten e)
                    }
              in  emitExp e config exp <> ";" <> docHardline <> compileBody' nextEnv es

            _ -> emitExp e config exp <> ";" <> docHardline <> compileBody' e es

        Var name _ ->
          let safeName  = generateSafeName name
              rewritten = varsRewritten env
          in  docText $ fromMaybe safeName (M.lookup safeName rewritten)

        Assignment (Typed _ _ _ (Var name _)) exp ->
          let safeName = generateSafeName name
              content = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config exp
              needsModifier = notElem safeName $ varsInScope env
              assignment = (if needsModifier then "let " else "") <> docText safeName <> " = " <> content
              methodGlobal =
                if name `S.member` methodNames env then
                  docHardline <> "global." <> docText name <> " = " <> docText name
                else
                  ""
          in  assignment <> methodGlobal

        Assignment lhs exp ->
          let lhs' = emitExp env config lhs
              content = emitExp env config exp
          in  lhs' <> " = " <> content

        Export e ->
          "export " <> emitExp env config e

        NameExport _ ->
          ""

        Record fields ->
          let fs = compileField <$> fields
          in  case fs of
                [] ->
                  "({})"

                _ ->
                  docGroup
                    $ "({"
                    <> docNest 2 (docLine' <> docCommaSeparatedLines fs)
                    <> docLine'
                    <> "})"
         where
          compileField :: Field -> JSDoc
          compileField (Typed _ _ _ field) = case field of
            Field (name, exp) ->
              docText name <> ": " <> emitExp env config exp

            FieldSpread exp ->
              "..." <> emitExp env config exp

        Access record (Typed _ _ _ (Var name _)) ->
          emitExp env config record <> docText name

        -- TODO: we need to emit the throw again somehow
        ArrayAccess arr index ->
          let arr' = emitExp env config arr
              index' = emitExp env config index
          in  arr' <> "[" <> index' <> "]"
          -- in "(function() {"
          --       <> index'
          --       <> " >= "
          --       <> arr'
          --       <> ".length ? "
          --       <> "(function() { throw \"Array out of bounds access\\nYou accessed the index '"
          --       <> index' 
          --       <> "' but the array currently has length '\" + "
          --       <> arr'
          --       <> ".length"
          --       <> " + \"'.\\n\\n\" })() : "
          --       <> arr'
          --       <> "["
          --       <> index'
          --       <> "]"
          --       <> "})())"

        JSExp           content -> docText (normalizeJSRawBlock content)

        Core.ListConstructor [
            Core.Typed _ _ _ (Core.ListItem li),
            Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
          ] | Core.isRightListRecursiveCall metadata ->
          let compiledLi    = emitExp env config li
              Just params   = rdParams <$> recursionData env
              compiledArgs  = emitExp env config <$> args
              updateParams  = (\(param, arg) -> docText param <> " = " <> arg) <$> zip params compiledArgs
          in  "($_end_ = $_end_.n = { v: "<> compiledLi <>" }, " <> docJoinText ", " updateParams <> ", $_continue_ = true)"

        Core.ListConstructor [
            Core.Typed _ _ _ (Core.ListItem li1),
            Core.Typed _ _ _ (Core.ListItem li2),
            Core.Typed _ _ _ (Core.ListSpread (Core.Typed _ _ _ (Core.Call _ args)))
          ] | Core.isRightListRecursiveCall metadata ->
          let compiledLi1    = emitExp env config li1
              compiledLi2    = emitExp env config li2
              Just params   = rdParams <$> recursionData env
              compiledArgs  = emitExp env config <$> args
              updateParams  = (\(param, arg) -> docText param <> " = " <> arg) <$> zip params compiledArgs
          in  "($_end_.n = { v: "<> compiledLi1 <>", n: { v: "<> compiledLi2 <>" }}, $_end_ = $_end_.n.n, " <> docJoinText ", " updateParams <> ", $_continue_ = true)"

        ListConstructor elems   -> "(" <> compileListElements elems <> ")"
          where
            listNode :: JSDoc -> JSDoc -> JSDoc
            listNode valueDoc nextDoc =
              docGroup
                $ "{"
                <> docNest 2
                     (docLine'
                     <> "v: "
                     <> valueDoc
                     <> ","
                     <> docLine'
                     <> "n: "
                     <> nextDoc
                     )
                <> docLine'
                <> "}"

            compileListElements :: [ListItem] -> JSDoc
            compileListElements items = case items of
              [Typed _ _ _ (ListItem last)] ->
                listNode (emitExp env config last) "null"

              [Typed _ _ _ (ListSpread last)] ->
                emitExp env config last

              (Typed _ _ _ (ListSpread spread) : next) ->
                "__listCtorSpread__(" <> emitExp env config spread <> ", " <> compileListElements next <> ")"

              [] ->
                "null"

              (Typed _ _ _ (ListItem item) : next) ->
                listNode (emitExp env config item) (compileListElements next)

              _ ->
                "null"

        TupleConstructor elems -> "([" <> docJoinText ", " (emitExp env config <$> elems) <> "])"

        Do exps ->
          let compiledExps = compileBody' env { inBody = True } exps
          in "(() => " <> docBlock [compiledExps] <> ")()"
          where
            compileBody' :: Env -> [Exp] -> JSDoc
            compileBody' env [exp] =
              "return " <> emitExp env config exp <> ";"
            compileBody' e (exp : es) = case exp of
              Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _) ->
                let safeName = generateSafeName name
                    nextEnv = e
                      { varsInScope = S.insert name (varsInScope e)
                      , varsRewritten = M.delete safeName (varsRewritten e)
                      }
                in  emitExp e config exp <> ";" <> docHardline <> compileBody' nextEnv es

              _ -> emitExp e config exp <> ";" <> docHardline <> compileBody' e es


        Where exp (first : cs) ->
          "((__x__) => "
            <> docBlock [compileIfChain first cs]
            <> ")("
            <> emitExp env config exp
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
            PVar _ ->
              "true"

            PAny ->
              "true"

            PNum  n ->
              scope <> " === " <> n

            PStr  n ->
              scope <> " === " <> renderJSStringLiteral n

            PChar n ->
              scope <> " === " <> "String.fromCodePoint(" <> (show . fromEnum) n <> ")"

            PBool n ->
              scope <> " === " <> n

            PCon n [] ->
              scope <> ".__constructor === " <> "\"" <> drop 7 (removeNamespace n) <> "\""

            PCon n ps ->
              let args = intercalate " && " $ filter (not . null) $ compileCtorArg scope n <$> zip [0 ..] ps
              in  scope <> ".__constructor === " <> "\"" <> drop 7 (removeNamespace n) <> "\"" <> if not (null args)
                    then " && " <> args
                    else ""

            PRecord m _restName ->
              let fieldChecks = intercalate " && " $ filter (not . null) $ M.elems $ M.mapWithKey (compileRecord scope) m
              in if null fieldChecks then "true" else fieldChecks

            PSpread pat ->
              compilePattern scope pat

            PList items ->
              compileListPattern scope items

            PTuple items ->
              compileTuplePattern scope items

          compileIsBranch :: Is -> JSDoc
          compileIsBranch (Typed _ _ _ (Is pat exp)) =
            let patVarNames = S.fromList (generateSafeName <$> getPatternVars pat)
                branchEnv = env { varsRewritten = M.filterWithKey (\k _ -> k `S.notMember` patVarNames) (varsRewritten env) }
                varDecls = filter (not . null) (dropWhile isSpace <$> lines (buildVars "__x__" pat))
                varDocs = docText <$> varDecls
                returnStmt =
                  if Maybe.isJust (recursionData env) then
                    emitExp branchEnv config exp <> ";"
                  else
                    "return " <> emitExp branchEnv config exp <> ";"
            in "("
              <> docText (compilePattern "__x__" pat)
              <> ") "
              <> docBlock (varDocs <> [returnStmt])

          compileIfChain :: Is -> [Is] -> JSDoc
          compileIfChain firstCase restCases =
            "if "
              <> compileIsBranch firstCase
              <> mconcat ((" else if " <>) . compileIsBranch <$> restCases)
              <> " else "
              <> compileElse

          compileElse :: JSDoc
          compileElse =
            docBlock
              [ "console.log('non exhaustive patterns for value: ', __x__.toString());"
              , "console.trace();"
              , "throw 'non exhaustive patterns!';"
              ]

          buildVars :: String -> Pattern -> String
          buildVars _ Untyped{} = codegenPanic "unexpected untyped pattern in buildVars"
          buildVars v (Typed _ _ _ pat) = case pat of
            PRecord fields restName ->
              let fieldVars = filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields
                  restVar = maybe [] (\name -> ["..." <> generateSafeName name]) restName
                  allVars = fieldVars <> restVar
              in "    let { "
                <> intercalate ", " allVars
                <> " } = "
                <> v
                <> ";\n"

            PList  items ->
              buildListVars v items

            PTuple items ->
              buildTupleVars v items

            PCon _ ps ->
              concat $ (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "]") p) <$> zip [0 ..] ps

            PVar n ->
              "    let " <> generateSafeName n <> " = " <> v <> ";\n"

            _ ->
              ""

          buildFieldVar :: String -> Pattern -> String
          buildFieldVar _ Untyped{} = codegenPanic "unexpected untyped pattern in buildFieldVar"
          buildFieldVar name (Typed _ _ _ pat) = case pat of
            PSpread (Typed _ _ _ (PVar n)) ->
              "..." <> generateSafeName n

            PVar n ->
              if null name then
                generateSafeName n
              else
                name <> ": " <> generateSafeName n

            PRecord fields restName ->
              let fieldVars = filter (not . null) . ((snd <$>) . reverse . sort . M.toList) $ M.mapWithKey buildFieldVar fields
                  restVar = maybe [] (\n -> ["..." <> generateSafeName n]) restName
              in name
                <> ": { "
                <> intercalate ", " (fieldVars <> restVar)
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

            _ ->
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
                    let varName = packListVars' itemsStr
                    in  if null varName then
                          ""
                        else
                          "    let " <> varName <> " = " <> scope <> ";\n"

          buildListItemVar :: Pattern -> String
          buildListItemVar Untyped{} = codegenPanic "unexpected untyped pattern in buildListItemVar"
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

            PRecord fields restName ->
              let fieldVars = M.elems $ M.mapWithKey buildFieldVar fields
                  restVar = maybe [] (\name -> ["..." <> generateSafeName name]) restName
              in "{ " <> intercalate ", " (fieldVars <> restVar) <> " }"

            _ ->
              ""


          packListVars :: [String] -> String
          packListVars items = case items of
            [""] ->
              ""

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
            PRecord fields restName -> 
              let fieldVars = M.elems $ M.mapWithKey buildFieldVar fields
                  restVar = maybe [] (\n -> ["..." <> generateSafeName n]) restName
              in "{ " <> intercalate ", " (fieldVars <> restVar) <> " }"
            _              -> ""

          compileRecord :: String -> Name -> Pattern -> String
          compileRecord scope n p = compilePattern (scope <> "." <> n) p

          compileCtorArg :: String -> String -> (Int, Pattern) -> String
          compileCtorArg scope _ (x, p) = compilePattern (scope <> ".__args[" <> show x <> "]") p

        Extern (_ :=> t) name foreignName ->
          let paramTypes = getParamTypes t
              paramCount = length paramTypes
              params = take paramCount ((:"")<$> ['a' ..])
              body = docApplyChain (docText foreignName) (docText <$> params)
              lambdaDoc = docCurriedLambda params body
          in  "let " <> docText name <> " = " <> docGroup lambdaDoc

        _ -> codegenPanic ("unsupported expression node: " <> ppShow exp)


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable TypeDecl where
  emit = emitTypeDeclDoc

emitTypeDeclDoc :: Env -> CompilationConfig -> TypeDecl -> JSDoc
emitTypeDeclDoc _ _ (Untyped _ _ ADT { adtconstructors = [] }) = ""
emitTypeDeclDoc _ _ (Untyped _ _ adt) =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>) "" (addExport exported . emitConstructorDoc <$> ctors)
   where
    addExport :: Bool -> JSDoc -> JSDoc
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  emit _ _ = emitConstructorDoc

emitConstructorDoc :: Constructor -> JSDoc
emitConstructorDoc (Untyped _ _ (Constructor cname cparams _)) =
    case cparams of
      [] ->
        "let " <> docText cname <> " = " <> compileBody cname cparams <> ";" <> docHardline

      _ ->
        "let "
          <> docText cname
          <> " = ("
          <> docCurriedLambda (argNames cparams) (compileBody cname cparams)
          <> ");"
          <> docHardline
   where
    argNames n = (: []) <$> take (length n) ['a' ..]

    compileBody n a =
      let args = argNames a
          argDocs = docText <$> args
      -- Based on the following input, we need to only use ConstructorName
      -- __6bb57939a0e365f381d7e05ce50bfeb1__ConstructorName
      -- therefore we need to drop the first 36 characters
      in  docGroup
            $ "({"
            <> docNest 2
                 (docLine'
                 <> "__constructor: \""
                 <> docText (drop 7 n)
                 <> "\","
                 <> docLine'
                 <> "__args: ["
                 <> docNest 2 (docLine' <> docCommaSeparatedLines argDocs)
                 <> docLine'
                 <> "]"
                 )
            <> docLine'
            <> "})"


compileImport :: CompilationConfig -> Import -> JSImport
compileImport config (Untyped _ _ (NamedImport names _ absPath)) =
  let importPath = buildImportPath config absPath
  in  JSImportNamed (generateSafeName . Core.getImportName <$> names) importPath


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
  emit env config ast =
    let entrypointPath     = ccentrypointPath config
        internalsPath      = ccinternalsPath config
        exps               = aexps ast
        typeDecls          = atypedecls ast
        path               = apath ast
        imports            = aimports ast

        astPath            = fromMaybe "Unknown" path
        configWithASTPath  = updateASTPath astPath config
        infoComment        = JSCommentLine (docText ("// file: " <> astPath))
        preludeImport =
          if entrypointPath == astPath then
            [JSImportStmt (JSImportBare internalsPath)]
          else
            []
        compiledImports = JSImportStmt . compileImport configWithASTPath <$> imports
        compiledAdts = JSRawBlock . emit env configWithASTPath <$> typeDecls
        compiledExps = emitExps env configWithASTPath exps
        defaultExport = [buildDefaultExport typeDecls exps]
        moduleDoc = emptyModuleDoc
          { mdHeaderComments = [infoComment]
          , mdPreludeImports = preludeImport
          , mdImports = compiledImports
          , mdTypeDecls = compiledAdts
          , mdDefinitions = compiledExps
          , mdFooter = defaultExport
          }
    in  renderModuleDoc moduleDoc


emitExps :: Env -> CompilationConfig -> [Exp] -> [JSStmt]
emitExps _ _ [] = []
emitExps env config [exp] = [JSLineStmt (emit env config exp)]
emitExps env config (exp : es) = case exp of
  Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _) ->
    let nextEnv = env { varsInScope = S.insert name (varsInScope env) }
    in  JSLineStmt (emit env config exp) : emitExps nextEnv config es

  _ -> JSLineStmt (emit env config exp) : emitExps env config es


buildDefaultExport :: [TypeDecl] -> [Exp] -> JSStmt
buildDefaultExport as es =
  let expExportNames    = generateSafeName . getExportName <$> filter isExport es
      adtExportNames    = getConstructorName <$> concat (adtconstructors . getValue <$> filter isADTExport as)
      allDefaultExports = expExportNames <> adtExportNames
  in  case allDefaultExports of
        []      -> JSExportDefault (JSObjectExpr [])
        exports -> JSExportDefault (JSObjectExpr (docText <$> exports))

 where
  isADTExport :: TypeDecl -> Bool
  isADTExport (Untyped _ _ ADT { adtexported }) = adtexported
  isADTExport _                               = False

  isExport :: Exp -> Bool
  isExport a = case a of
    Typed _ _ _ (Export _) -> True

    _ -> False

  getExportName :: Exp -> String
  getExportName (Typed _ _ _ (Export (Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) _)))) = n
  getExportName (Typed _ _ _ (Export (Typed _ _ _ (Extern _ n _))))   = n


  getConstructorName :: Constructor -> String
  getConstructorName (Untyped _ _ (Constructor cname _ _)) = cname


rollupNotFoundMessage :: String
rollupNotFoundMessage = unlines
  [ "Compilation error:"
  , "Rollup was not found."
  , "You must have rollup installed in order to use the bundling option. Please visit this page in order to install it: https://rollupjs.org/guide/en/#installation"
  ]

-- TODO: rework this
runBundle :: FilePath -> Target -> IO (Either String (String, String))
runBundle entrypointCompiledPath target = do
  putStrLn $ "Bundling with entrypoint '" <> entrypointCompiledPath <> "'"
  esbuildPath        <- try $ getEnv "ESBUILD_PATH"
  esbuildPathChecked <- case (esbuildPath :: Either IOError String) of
    Left _ -> do
      r <-
        try (readProcessWithExitCode "esbuild" ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left  err -> do
          putStrLn $ ppShow err
          return $ Left rollupNotFoundMessage
        Right _ -> return $ Right "esbuild"
    Right p -> do
      r <- try (readProcessWithExitCode p ["--version"] "") :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left err -> do
          putStrLn $ ppShow err
          r <-
            try (readProcessWithExitCode "esbuild" ["--version"] "") :: IO
              (Either SomeException (ExitCode, String, String))
          case r of
            Left  err -> do
              putStrLn $ ppShow err
              return $ Left rollupNotFoundMessage
            Right _ -> return $ Right "esbuild"
        Right _ -> return $ Right p

  let platform = if target == TNode then "node" else "browser"
  case esbuildPathChecked of
    Right esbuild -> do
      r <-
        try
          (readProcessWithExitCode
            esbuild
            [ entrypointCompiledPath
            , "--platform=" ++ platform
            , "--bundle"
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


hashModulePath :: AST -> String
hashModulePath ast =
  generateHashFromPath $ Maybe.fromMaybe "" (apath ast)


generateMainCall :: Options -> String -> String
generateMainCall options astPath =
  let moduleHash = generateHashFromPath astPath
      mainName = addHashToName moduleHash "main"
  in  if astPath == optEntrypoint options then
        if optTarget options == TNode then
          "\n" <> unlines
            [ "const __makeArgs = () => {"
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
            , mainName <> "(__makeArgs())"
            ]
        else
          "\n" <> mainName <> "(null)\n"
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

    -- TODO: move this to a Query as well?
    monomorphicMethodNames <- readIORef monomorphicMethods

    let moduleContent = docRender $ emit
          initialEnv { methodNames = monomorphicMethodNames }
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

    Prelude.putStrLn $ "[1 of 1] Compiled '" <> path <> "'"

    return moduleContent'
