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
                                                , foldl', stripPrefix, isInfixOf, isPrefixOf, isSuffixOf
                                                )
import           Data.Char                      ( isSpace, isUpper, isHexDigit )
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
import           Generate.Javascript.SourceMap  (Mapping(..), buildSourceMapJSON, base64Encode)
import           Explain.Location               (Area)
import           Run.Target
import           Run.SourceMapMode
import           Text.Show.Pretty               ( ppShow )
import Distribution.Types.Lens (_Impl)
import qualified Data.Maybe as Maybe
import Control.Exception
import Control.Monad (forM)
import GHC.IO.Exception
import System.Process
import System.Environment (getEnv)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
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
  , constructorsByName :: M.Map String (String, Int)
  , topLevelNames :: S.Set String
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
    , constructorsByName = M.empty
    , topLevelNames = S.empty
    , methodNames = S.empty
    , inBody = False
    }

collectConstructorsByName :: Core.AST -> M.Map String (String, Int)
collectConstructorsByName ast =
  let getCtorName ctor = case ctor of
        Untyped _ _ (Constructor cname _ _) ->
          cname

        Typed _ _ _ (Constructor cname _ _) ->
          cname

      collectTypeDecl typeDecl = case typeDecl of
        Untyped _ _ ADT { adtconstructors = ctors } ->
          [ (generateSafeName name, (drop 7 name, Core.getConstructorArity ctor))
          | ctor <- ctors
          , let name = getCtorName ctor
          ]

        _ ->
          []
  in M.fromList (atypedecls ast >>= collectTypeDecl)

emitConstructorValueDoc :: String -> Int -> JSDoc
emitConstructorValueDoc ctorName arity =
  let args = (: []) <$> take arity ['a' ..]
      argDocs = docText <$> args
      body =
        docGroup
          $ "({"
          <> docNest 2
               (docLine'
               <> "__constructor: \""
               <> docText ctorName
               <> "\","
               <> docLine'
               <> "__args: ["
               <> docNest 2 (docLine' <> docCommaSeparatedLines argDocs)
               <> docLine'
               <> "]"
               )
          <> docLine'
          <> "})"
  in  if arity == 0 then body else "(" <> docCurriedLambda args body <> ")"

allowedJSNames :: S.Set String
allowedJSNames = S.fromList ["delete", "class", "while", "for", "case", "switch", "try", "length", "var", "default", "break", "null"]

isGeneratedTestsName :: String -> Bool
isGeneratedTestsName n = "__tests__" `isInfixOf` n


looksLikeGeneratedConstructorName :: String -> Bool
looksLikeGeneratedConstructorName n = case n of
  '_' : next ->
    let (hashPart, rest) = span isHexDigit next
    in  not (null hashPart) && case rest of
          '_' : first : more ->
            isUpper first && not ("__" `isInfixOf` more)

          _ ->
            False

  _ ->
    False


collectTopLevelNames :: Core.AST -> S.Set String
collectTopLevelNames ast =
  let names = aexps ast >>= \case
        Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) _) ->
          [generateSafeName n]
        Typed _ _ _ (Export (Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) _))) ->
          [generateSafeName n]
        _ ->
          []
  in S.fromList names

generateSafeName :: String -> String
generateSafeName n =
  if '.' `elem` n then
    let namespace = takeWhile (/= '.') n
        name      = tail $ dropWhile (/= '.') n
    in  if name `S.member` allowedJSNames then
          namespace <> "." <> "_$_" <> name <> "_$_"
        else
          n
  else if n `S.member` allowedJSNames then
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

trimJS :: String -> String
trimJS = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isJSIdentifierChar :: Char -> Bool
isJSIdentifierChar c = c == '_' || c == '$' || c == '\'' || c == '#' || c == '.' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

stripBindingKeyword :: String -> Maybe String
stripBindingKeyword s =
  Maybe.listToMaybe
    $ Maybe.mapMaybe (`stripPrefix` s) ["const ", "let ", "var "]

rewriteTopLevelArrowBindingToFunction :: String -> String
rewriteTopLevelArrowBindingToFunction raw =
  let trimmed = trimJS raw
      parseWithParams name rhs =
        let rhs' = dropWhile isSpace rhs
        in  case rhs' of
              '(' : rest ->
                let (params, afterParams) = break (== ')') rest
                    afterClose = case afterParams of
                      ')' : more ->
                        dropWhile isSpace more

                      _ ->
                        ""
                in  case stripPrefix "=>" afterClose of
                      Just afterArrow ->
                        let body = trimJS afterArrow
                            fnBody = if not (null body) && head body == '{'
                              then body
                              else "{ return " <> dropTrailingSemicolon body <> "; }"
                        in  Just ("function " <> name <> "(" <> params <> ") " <> fnBody)

                      Nothing ->
                        Nothing

              _ ->
                let (param, afterParam) = span isJSIdentifierChar rhs'
                    afterParam' = dropWhile isSpace afterParam
                in  case stripPrefix "=>" afterParam' of
                      Just afterArrow | not (null param) ->
                        let body = trimJS afterArrow
                            fnBody = if not (null body) && head body == '{'
                              then body
                              else "{ return " <> dropTrailingSemicolon body <> "; }"
                        in  Just ("function " <> name <> "(" <> param <> ") " <> fnBody)

                      _ ->
                        Nothing
      dropTrailingSemicolon s = reverse (dropWhile isSpace (dropWhile (== ';') (dropWhile isSpace (reverse s))))
  in  case stripBindingKeyword trimmed of
        Just rest ->
          let (name, rest') = span isJSIdentifierChar rest
              afterName = dropWhile isSpace rest'
          in  if null name then
                trimmed
              else
                case stripPrefix "=" afterName >>= parseWithParams name of
                  Just rewritten ->
                    rewritten

                  Nothing ->
                    trimmed

        Nothing ->
          trimmed

normalizeTopLevelJSExp :: Env -> String -> String
normalizeTopLevelJSExp env raw =
  let normalized = normalizeJSRawBlock raw
  in  if inBody env then
        normalized
      else
        rewriteTopLevelArrowBindingToFunction normalized

data CompilationConfig
  = CompilationConfig
      { ccrootPath            :: FilePath
      , ccastPath             :: FilePath
      , ccentrypointPath      :: FilePath
      , ccoutputPath          :: FilePath
      , ccoptimize            :: Bool
      , cctarget              :: Target
      , ccinternalsPath       :: FilePath
      -- | True only for the single true entrypoint module (not per-module entrypoints).
      , ccisEntrypoint        :: Bool
      -- | For the entrypoint module only: list of (moduleInitFnName, relativeImportPath)
      -- for all dependency modules, in dependency order. The entrypoint calls each in order.
      , ccmoduleInitImports   :: [(String, FilePath)]
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

-- Returns True if a top-level RHS needs to go into the module init function
-- rather than being evaluated at module load time.
-- Lambdas (Definition), plain literals, and constructor Vars are safe to
-- evaluate eagerly at load time. Everything else may depend on values from
-- other modules that are not yet initialized when this module first loads.
rhsNeedsModuleInit :: Core.Exp -> Bool
rhsNeedsModuleInit (Core.Typed _ _ _ e) = case e of
  Core.Definition _ _ -> False
  Core.Literal _      -> False
  Core.Var _ True     -> False  -- constructor var, emits as a record literal
  _                   -> True
rhsNeedsModuleInit _ = False


class Compilable a where
  emit :: Env -> CompilationConfig -> a -> JSDoc

instance Compilable Exp where
  emit = emitExp

emitExp :: Env -> CompilationConfig -> Exp -> JSDoc
emitExp _ _ Untyped{} = mempty
emitExp env config (Typed qt area metadata exp) =
    docAnnotate area $
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
              resolvedName = fromMaybe safeName (M.lookup safeName rewritten)
              ctorArityFromType = length $ getParamTypes (getQualified qt)
              ctorNameFromVar =
                if length safeName > 7 then drop 7 safeName else safeName
          in  case M.lookup safeName (constructorsByName env) of
                Just (ctorName, arity) | not (safeName `S.member` varsInScope env) ->
                  emitConstructorValueDoc ctorName arity

                _ | looksLikeGeneratedConstructorName safeName && not (safeName `S.member` varsInScope env) ->
                  emitConstructorValueDoc ctorNameFromVar ctorArityFromType

                _ | isGeneratedTestsName safeName && not (safeName `S.member` varsInScope env) ->
                  docText safeName <> "()"

                _ ->
                  docText resolvedName

        Assignment (Typed _ _ _ (Var name _)) exp ->
          let safeName = generateSafeName name
              rhsArity = case exp of
                Typed rhsQt _ _ _ ->
                  length $ getParamTypes (getQualified rhsQt)

                Untyped{} ->
                  0
              methodGlobal =
                if name `S.member` methodNames env then
                  docHardline <> "global." <> docText name <> " = " <> docText name
                else
                  ""
              mkFunctionAlias rhsName =
                let safeRhs = generateSafeName rhsName
                in "function "
                  <> docText safeName
                  <> "(...__args) "
                  <> docBlock
                       [ "return __args.length === 0"
                           <> " ? "
                           <> docText safeRhs
                           <> " : __args.reduce((f, a) => f(a), "
                           <> docText safeRhs
                           <> ");"
                       ]
                  <> methodGlobal

              mkFunctionDecl params =
                let safeParams = generateSafeName <$> (getValue <$> params)
                    firstParam = head safeParams
                    fnExpr = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config exp
                    invoke = docApplyChain fnExpr [docText firstParam]
                in "function "
                  <> docText safeName
                  <> "("
                  <> docText firstParam
                  <> ") "
                  <> docBlock ["return " <> invoke <> ";"]
                  <> methodGlobal
          in case exp of
               Typed _ _ _ (Var rhsName _) | not (inBody env) && rhsArity > 0 ->
                 mkFunctionAlias rhsName

               _ | not (inBody env) && isGeneratedTestsName safeName ->
                 let content = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config exp
                 in  "function "
                      <> docText safeName
                      <> "() "
                      <> docBlock ["return " <> content <> ";"]
                      <> methodGlobal

               Typed _ _ _ (Definition params _) | not (null params) && not (inBody env) ->
                 mkFunctionDecl params

               _ ->
                 let content = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config exp
                     needsModifier = not (safeName `S.member` varsInScope env)
                     declKeyword = if inBody env then "let " else "var "
                     assignment = (if needsModifier then declKeyword else "") <> docText safeName <> " = " <> content
                     needsModuleInit =
                       not (inBody env)
                         && needsModifier
                         && not (isGeneratedTestsName safeName)
                         && rhsNeedsModuleInit exp
                 in  if needsModuleInit then
                       -- Declare uninitialized at top level; actual assignment goes in __moduleInit_<hash>
                       "var " <> docText safeName <> methodGlobal
                     else
                       assignment <> methodGlobal

        Assignment lhs exp ->
          let lhs' = emitExp env config lhs
              content = emitExp env config exp
          in  lhs' <> " = " <> content

        Export e -> case e of
          Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) exp)
            | isGeneratedTestsName (generateSafeName name) ->
              let safeName = generateSafeName name
                  content = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config exp
                  methodGlobal =
                    if name `S.member` methodNames env then
                      docHardline <> "global." <> docText name <> " = " <> docText name
                    else
                      ""
              in  "export function "
                    <> docText safeName
                    <> "() "
                    <> docBlock ["return " <> content <> ";"]
                    <> methodGlobal

          Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) rhs@(Typed rhsQt _ _ (Var rhsName _)))
            | let rhsArity = length $ getParamTypes (getQualified rhsQt)
            , rhsArity > 0 ->
              let safeName = generateSafeName name
                  safeRhs = generateSafeName rhsName
                  methodGlobal =
                    if name `S.member` methodNames env then
                      docHardline <> "global." <> docText name <> " = " <> docText name
                    else
                      ""
              in  "export function "
                    <> docText safeName
                    <> "(...__args) "
                    <> docBlock
                         [ "return __args.length === 0"
                             <> " ? "
                             <> docText safeRhs
                             <> " : __args.reduce((f, a) => f(a), "
                             <> docText safeRhs
                             <> ");"
                         ]
                    <> methodGlobal

          Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) exp@(Typed _ _ _ (Definition params _)))
            | not (null params) ->
              let safeName = generateSafeName name
                  safeParams = generateSafeName <$> (getValue <$> params)
                  firstParam = head safeParams
                  fnExpr = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config exp
                  invoke = docApplyChain fnExpr [docText firstParam]
                  methodGlobal =
                    if name `S.member` methodNames env then
                      docHardline <> "global." <> docText name <> " = " <> docText name
                    else
                      ""
              in  "export function "
                    <> docText safeName
                    <> "("
                    <> docText firstParam
                    <> ") "
                    <> docBlock ["return " <> invoke <> ";"]
                    <> methodGlobal

          _ ->
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

        JSExp           content -> docText (normalizeTopLevelJSExp env content)

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
          in  (if inBody env then "let " else "var ") <> docText name <> " = " <> docGroup lambdaDoc

        _ -> codegenPanic ("unsupported expression node: " <> ppShow exp)
      where
        hasAnyTopLevelRef :: Exp -> Bool
        hasAnyTopLevelRef e = case e of
          Typed _ _ _ inner -> case inner of
            Var n _ ->
              generateSafeName n `S.member` topLevelNames env

            Definition _ body ->
              any hasAnyTopLevelRef body

            Assignment l r ->
              hasAnyTopLevelRef l || hasAnyTopLevelRef r

            Export e' ->
              hasAnyTopLevelRef e'

            Do exps ->
              any hasAnyTopLevelRef exps

            Where e' iss ->
              hasAnyTopLevelRef e' || any (hasAnyTopLevelRef . getIsExpression) iss

            If c t f ->
              hasAnyTopLevelRef c || hasAnyTopLevelRef t || hasAnyTopLevelRef f

            While c b ->
              hasAnyTopLevelRef c || hasAnyTopLevelRef b

            Call fn args ->
              hasAnyTopLevelRef fn || any hasAnyTopLevelRef args

            Record fields ->
              any
                (\case
                  Typed _ _ _ (Field (_, fe)) ->
                    hasAnyTopLevelRef fe

                  Typed _ _ _ (FieldSpread fe) ->
                    hasAnyTopLevelRef fe

                  _ ->
                    False
                )
                fields

            Access rec _ ->
              hasAnyTopLevelRef rec

            ArrayAccess arr idx ->
              hasAnyTopLevelRef arr || hasAnyTopLevelRef idx

            ListConstructor items ->
              any
                (\case
                  Typed _ _ _ (ListItem ie) ->
                    hasAnyTopLevelRef ie

                  Typed _ _ _ (ListSpread ie) ->
                    hasAnyTopLevelRef ie

                  _ ->
                    False
                )
                items

            TupleConstructor items ->
              any hasAnyTopLevelRef items

            _ ->
              False

          Untyped{} ->
            False


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
        "var " <> docText cname <> " = " <> compileBody cname cparams <> ";" <> docHardline

      _ ->
        "var "
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
    let internalsPath    = ccinternalsPath config
        moduleInitImports = ccmoduleInitImports config
        isEntrypoint     = ccisEntrypoint config
        exps             = aexps ast
        typeDecls        = atypedecls ast
        path             = apath ast
        imports          = aimports ast

        astPath          = fromMaybe "Unknown" path
        configWithASTPath = updateASTPath astPath config
        infoComment      = JSCommentLine (docText ("// file: " <> astPath))
        preludeImport    =
          if isEntrypoint then [JSImportStmt (JSImportBare internalsPath)] else []

        -- For the entrypoint: import __moduleInit_<hash> from each dependency
        moduleInitImportStmts =
          if isEntrypoint then
            [ JSImportStmt (JSImportNamed [fnName] relPath)
            | (fnName, relPath) <- moduleInitImports
            ]
          else []

        compiledImports  = JSImportStmt . compileImport configWithASTPath <$> imports
        compiledAdts     = JSRawBlock . emit env configWithASTPath <$> typeDecls
        compiledExps     = emitExps env configWithASTPath exps

        -- Module init function: exported so the entrypoint can call it
        moduleHash       = generateHashFromPath astPath
        moduleInitFnName = "__moduleInit_" <> moduleHash
        moduleInitStmts  = emitModuleInitBody env configWithASTPath exps
        moduleInitFn     =
          if null moduleInitStmts then
            JSLineStmt $ "export function " <> docText moduleInitFnName <> "() {}"
          else
            JSLineStmt $
              "export function " <> docText moduleInitFnName <> "() "
              <> docBlock (moduleInitStmts)

        -- For the entrypoint: call each dependency's module init in order, then our own
        moduleInitCalls =
          if isEntrypoint then
            [ JSLineStmt (docText fnName <> "()")
            | (fnName, _) <- moduleInitImports
            ]
            <> [JSLineStmt (docText moduleInitFnName <> "()")]
          else []

        defaultExport = [buildDefaultExport typeDecls exps]
        moduleDoc = emptyModuleDoc
          { mdHeaderComments = [infoComment]
          , mdPreludeImports = preludeImport
          , mdImports        = moduleInitImportStmts <> compiledImports
          , mdTypeDecls      = compiledAdts
          , mdDefinitions    = compiledExps <> [moduleInitFn]
          , mdFooter         = moduleInitCalls <> defaultExport
          }
    in  renderModuleDoc moduleDoc


emitExps :: Env -> CompilationConfig -> [Exp] -> [JSStmt]
emitExps _ _ [] = []
emitExps env config [exp] = [JSLineStmt (annotatedEmit env config exp)]
emitExps env config (exp : es) = case exp of
  Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _) ->
    let nextEnv = env { varsInScope = S.insert name (varsInScope env) }
    in  JSLineStmt (annotatedEmit env config exp) : emitExps nextEnv config es

  Core.Typed _ _ _ (Core.Export (Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) _))) ->
    let nextEnv = env { varsInScope = S.insert name (varsInScope env) }
    in  JSLineStmt (annotatedEmit env config exp) : emitExps nextEnv config es

  _ -> JSLineStmt (annotatedEmit env config exp) : emitExps env config es

-- | Emit an expression and wrap it with its source annotation so that
-- renderWithMappings can extract a Mapping entry for it.
annotatedEmit :: Env -> CompilationConfig -> Exp -> JSDoc
annotatedEmit env config exp = case exp of
  Core.Typed _ area _ _ -> docAnnotate area (emit env config exp)
  Core.Untyped area _ _ -> docAnnotate area (emit env config exp)


-- | Collect the module init body: for every top-level var that needs module init,
-- emit the assignment statement (varName = expr) that goes inside __moduleInit_<hash>.
emitModuleInitBody :: Env -> CompilationConfig -> [Exp] -> [JSDoc]
emitModuleInitBody _ _ [] = []
emitModuleInitBody env config (exp : es) =
  let rest env' = emitModuleInitBody env' config es
  in  case exp of
    Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) rhs) ->
      let safeName = generateSafeName name
          nextEnv  = env { varsInScope = S.insert name (varsInScope env) }
          needsInit =
            not (safeName `S.member` varsInScope env)
              && not (isGeneratedTestsName safeName)
              && rhsNeedsModuleInit rhs
          content = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config rhs
      in  (if needsInit then (docText safeName <> " = " <> content :) else id) (rest nextEnv)

    Core.Typed _ _ _ (Core.Export (Core.Typed _ _ _ (Core.Assignment (Core.Typed _ _ _ (Core.Var name _)) rhs))) ->
      let safeName = generateSafeName name
          nextEnv  = env { varsInScope = S.insert name (varsInScope env) }
          needsInit =
            not (safeName `S.member` varsInScope env)
              && not (isGeneratedTestsName safeName)
              && rhsNeedsModuleInit rhs
          content = emitExp (env { varsInScope = S.insert name (varsInScope env) }) config rhs
      in  (if needsInit then (docText safeName <> " = " <> content :) else id) (rest nextEnv)

    _ -> rest env


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
runBundle :: FilePath -> FilePath -> Target -> SourceMapMode -> IO (Either String (String, String))
runBundle entrypointCompiledPath bundleOutputPath target sourceMapMode = do
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
    Right esbuild ->
      case sourceMapMode of
        -- External source maps require --outfile so esbuild can write the .map
        -- file alongside the bundle; capturing stdout is not supported in this mode.
        ExternalSourceMap -> do
          r <-
            try
              (readProcessWithExitCode
                esbuild
                [ entrypointCompiledPath
                , "--platform=" ++ platform
                , "--bundle"
                , "--sourcemap=external"
                , "--outfile=" ++ bundleOutputPath
                ]
                ""
              ) :: IO (Either SomeException (ExitCode, String, String))
          case r of
            Left  e ->
              return $ Left (ppShow e)
            -- esbuild wrote the files itself; return empty stdout so the caller
            -- does not overwrite the file.
            Right (_, _, stderr) ->
              return $ Right ("", stderr)

        _ -> do
          let sourcemapFlag = case sourceMapMode of
                InlineSourceMap -> ["--sourcemap=inline"]
                _               -> []
          r <-
            try
              (readProcessWithExitCode
                esbuild
                ([ entrypointCompiledPath
                 , "--platform=" ++ platform
                 , "--bundle"
                 ] ++ sourcemapFlag)
                ""
              ) :: IO (Either SomeException (ExitCode, String, String))
          case r of
            Left  e ->
              return $ Left (ppShow e)
            Right (_, stdout, stderr) ->
              return $ Right (stdout, stderr)

    Left e ->
      return $ Left e


makeInternalsModuleTargetPath :: FilePath -> FilePath
makeInternalsModuleTargetPath outputPath =
  takeDirectoryIfFile outputPath <> (pathSeparator : "__internals__.mjs")


generateInternalsModule :: Options -> IO ()
generateInternalsModule options = do
  let internalsTargetPath = makeInternalsModuleTargetPath (optOutputPath options)
  createDirectoryIfMissing True (takeDirectoryIfFile internalsTargetPath)
  writeFile internalsTargetPath
    $ generateInternalsModuleContent (optTarget options) (optOptimized options) (optCoverage options)


computeInternalsRelativePath :: FilePath -> FilePath -> FilePath
computeInternalsRelativePath outputPath astTargetPath =
  -- outputPath and astTargetPath are already absolute paths
  let outputRoot    = takeDirectoryIfFile outputPath
      internalsPath = outputRoot <> (pathSeparator : "__internals__.mjs")
      astDir        = dropFileName astTargetPath
      relPath       = convertWindowsSeparators $ cleanRelativePath $ makeRelativeEx astDir internalsPath
  in  if "." `isPrefixOf` relPath then relPath else "./" <> relPath


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

findGeneratedMainName :: Core.AST -> Maybe String
findGeneratedMainName ast =
  let names = aexps ast >>= \case
        Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) _) ->
          [n]
        Typed _ _ _ (Export (Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) _))) ->
          [n]
        _ ->
          []
  in  find ("_main" `isSuffixOf`) names


generateMainCallFromAST :: Options -> String -> Core.AST -> String
generateMainCallFromAST options astPath ast =
  let fallbackMainCall = generateMainCall options astPath
  in  case findGeneratedMainName ast of
        Just mainName ->
          if astPath == optEntrypoint options then
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
        Nothing ->
          fallbackMainCall



-- | Return the number of lines in a source file (used to filter mappings).
sourceFileLineCount :: FilePath -> IO Int
sourceFileLineCount path = do
  content <- readFile path
  return $! length (lines content)


-- | Drop mappings whose source line exceeds the source file's line count.
-- Monomorphization and inlining can produce Core nodes with areas from other
-- modules; those produce line numbers that are out-of-range for the current
-- source file and would confuse source map consumers.
filterMappingsBySourceFile :: Int -> [Mapping] -> [Mapping]
filterMappingsBySourceFile lineCount = filter (\m -> mappingSrcLine m < lineCount)


-- | Attach a //# sourceMappingURL comment and optionally write the map JSON.
-- Returns (annotated JS string, map JSON string).
makeExternalSourceMap
  :: FilePath   -- absolute path to .mad source file
  -> FilePath   -- absolute path to output .mjs file
  -> [Mapping]
  -> String     -- JS module content
  -> (String, String)
makeExternalSourceMap sourcePath outputPath mappings jsContent =
  let outFileName    = takeFileName outputPath
      -- relative path from the .mjs directory to the .mad source
      relSource      = makeRelativeEx (dropFileName outputPath) sourcePath
      mapJson        = buildSourceMapJSON outFileName relSource mappings Nothing
      annotated      = jsContent <> "\n//# sourceMappingURL=" <> outFileName <> ".map\n"
  in  (annotated, mapJson)

makeInlineSourceMap
  :: FilePath   -- absolute path to .mad source file
  -> FilePath   -- absolute path to output .mjs file
  -> [Mapping]
  -> String     -- JS module content
  -> String
makeInlineSourceMap sourcePath outputPath mappings jsContent =
  let outFileName = takeFileName outputPath
      relSource   = makeRelativeEx (dropFileName outputPath) sourcePath
      mapJson     = buildSourceMapJSON outFileName relSource mappings Nothing
      encoded     = base64Encode mapJson
      annotated   = jsContent <> "\n//# sourceMappingURL=data:application/json;base64," <> encoded <> "\n"
  in  annotated


generateJSModule :: Options -> [FilePath] -> Core.AST -> IO (String, [Mapping])
generateJSModule _ _ Core.AST { Core.apath = Nothing } = return ("", [])
generateJSModule options pathsToBuild ast@Core.AST { Core.apath = Just path }
  = do
    -- All paths are already canonicalized/absolute from the Rock query system and Options,
    -- so makeAbsolute is redundant here.
    let rootPath           = optRootPath options
        outputPath         = optOutputPath options
        computedOutputPath = computeTargetPath outputPath rootPath path
        entrypointPath     = if path `elem` pathsToBuild then path else optEntrypoint options
        isEntrypoint       = path == optEntrypoint options
        internalsPath      = computeInternalsRelativePath outputPath computedOutputPath

    -- TODO: move this to a Query as well?
    monomorphicMethodNames <- readIORef monomorphicMethods

    -- For the entrypoint, compute import entries for all dependency module init functions.
    -- pathsToBuild is in dependency order with the entrypoint last; exclude the entrypoint itself.
    moduleInitImports <-
      if isEntrypoint then do
        let deps = filter (/= path) pathsToBuild
        forM deps $ \depPath -> do
          let depHash    = generateHashFromPath depPath
              depFnName  = "__moduleInit_" <> depHash
              depOutPath = computeTargetPath outputPath rootPath depPath
              entryDir   = dropFileName computedOutputPath
              relPath    = convertWindowsSeparators $ cleanRelativePath
                             $ replaceExtension (joinPath ["./", makeRelativeEx entryDir depOutPath]) ".mjs"
          return (depFnName, relPath)
      else
        return []

    let env = initialEnv
                { methodNames        = monomorphicMethodNames
                , constructorsByName = collectConstructorsByName ast
                , topLevelNames      = collectTopLevelNames ast
                }
        config = CompilationConfig
                  { ccrootPath          = rootPath
                  , ccastPath           = path
                  , ccentrypointPath    = entrypointPath
                  , ccoutputPath        = outputPath
                  , ccoptimize          = optOptimized options
                  , cctarget            = optTarget options
                  , ccinternalsPath     = internalsPath
                  , ccisEntrypoint      = isEntrypoint
                  , ccmoduleInitImports = moduleInitImports
                  }

    -- Render the module.  When source maps are requested we use the annotated
    -- render path to extract Mapping entries directly from the Doc annotations
    -- that emitExp attaches.  Otherwise we strip annotations for a plain render.
    let moduleDoc = emit env config ast
        mainCallSuffix = generateMainCallFromAST options path ast
    let (moduleContent, mappings) =
          if optSourceMaps options /= NoSourceMap then
            let (rendered, ms) = renderWithMappings 80 moduleDoc
            in  (rendered <> mainCallSuffix, ms)
          else
            (docRender moduleDoc <> mainCallSuffix, [])

    return (moduleContent, mappings)
