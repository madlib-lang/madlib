{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Compile where

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( sort
                                                , find
                                                , intercalate
                                                )

import           AST.Solved
import           Utils.Path                     ( cleanRelativePath
                                                , computeTargetPath
                                                , makeRelativeEx
                                                )
import           System.FilePath                ( replaceExtension
                                                , dropFileName
                                                , joinPath
                                                )


class Compilable a where
  -- first FilePath is the root folder of sources
  -- second FilePath is the output path
  compile :: FilePath -> FilePath -> a -> String

instance Compilable Exp where
  compile astPath outputPath (Solved _ _ exp) = case exp of
    LNum  v     -> v
    LStr  v     -> "`" <> v <> "`"
    LBool v     -> v

    App abs arg -> case abs of
      Solved _ _ (App (Solved _ _ (Var "++")) arg') ->
        compile astPath outputPath arg'
          <> " + "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "+")) arg') ->
        compile astPath outputPath arg'
          <> " + "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "-")) arg') ->
        compile astPath outputPath arg'
          <> " - "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "*")) arg') ->
        compile astPath outputPath arg'
          <> " * "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "/")) arg') ->
        compile astPath outputPath arg'
          <> " / "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "%")) arg') ->
        compile astPath outputPath arg'
          <> " % "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "==")) arg') ->
        compile astPath outputPath arg'
          <> " === "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "&&")) arg') ->
        compile astPath outputPath arg'
          <> " && "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "||")) arg') ->
        compile astPath outputPath arg'
          <> " || "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var ">")) arg') ->
        compile astPath outputPath arg'
          <> " > "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "<")) arg') ->
        compile astPath outputPath arg'
          <> " < "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var ">=")) arg') ->
        compile astPath outputPath arg'
          <> " >= "
          <> compile astPath outputPath arg
      Solved _ _ (App (Solved _ _ (Var "<=")) arg') ->
        compile astPath outputPath arg'
          <> " <= "
          <> compile astPath outputPath arg

      Solved _ _ (App (Solved _ _ (Var "|>")) arg') ->
        compile astPath outputPath arg
          <> "("
          <> compile astPath outputPath arg'
          <> ")"

      _ -> compileApp [] abs arg
     where
      compileApp :: [String] -> Exp -> Exp -> String
      compileApp prevArgs abs arg =
        let
          args = [compile astPath outputPath arg] <> prevArgs
          next = case abs of
            (Solved _ _ (App abs' arg')) -> compileApp args abs' arg'
            _ ->
              compile astPath outputPath abs
                <> "("
                <> intercalate ", " args
                <> ")"
        in
          next

    If cond truthy falsy ->
      "("
        <> compile astPath outputPath cond
        <> " ? "
        <> compile astPath outputPath truthy
        <> " : "
        <> compile astPath outputPath falsy
        <> ")"

    Abs param body -> compileAbs Nothing param body
     where
      compileAbs :: Maybe Exp -> Name -> Exp -> String
      compileAbs parent param body =
        let start = case parent of
              Just _  -> ", " <> param
              Nothing -> "curryPowder((" <> param
            next = case body of
              (Solved _ _ (Abs param' body')) ->
                compileAbs (Just body) param' body'
              _ -> ") => " <> compile astPath outputPath body <> ")"
        in  start <> next

    Var name -> name

    Assignment name exp ->
      "const " <> name <> " = " <> compile astPath outputPath exp <> ""

    TypedExp exp _ -> compile astPath outputPath exp

    Export (Solved _ _ (Assignment name exp)) ->
      "export const " <> name <> " = " <> compile astPath outputPath exp <> ""

    Record fields ->
      -- Maybe just map and intercalate ?
      let fs = intercalate "," $ compileField <$> fields in "({" <> fs <> " })"
     where
      compileField :: Field -> String
      compileField field = case field of
        Field (name, exp) ->
          " " <> name <> ": " <> compile astPath outputPath exp
        FieldSpread exp -> " ..." <> compile astPath outputPath exp

    FieldAccess record field ->
      compile astPath outputPath record <> compile astPath outputPath field

    JSExp content -> content

    ListConstructor elems ->
      "([" <> intercalate ", " (compileListItem <$> elems) <> "])"
     where
      compileListItem :: ListItem -> String
      compileListItem li = case li of
        ListItem   exp -> compile astPath outputPath exp
        ListSpread exp -> " ..." <> compile astPath outputPath exp

    TupleConstructor elems ->
      "([" <> intercalate ", " (compile astPath outputPath <$> elems) <> "])"

    Where exp (first : cs) ->
      "((__x__) => {\n  "
        <> compileIs first
        <> concat (("  else " ++) . compileIs <$> cs)
        -- TODO: Add an else for undefined patterns error and make it throw.
        <> "})("
        <> compile astPath outputPath exp
        <> ")"
     where
      compilePattern :: String -> Pattern -> String
      compilePattern _     (PVar _)  = "true"
      compilePattern _     PAny      = "true"
      compilePattern scope (PNum  n) = scope <> " === " <> n
      compilePattern scope (PStr  n) = scope <> " === \"" <> n <> "\""
      compilePattern scope (PBool n) = scope <> " === " <> n
      compilePattern scope (PCon n)
        | n == "String"  = "typeof " <> scope <> " === \"string\""
        | n == "Boolean" = "typeof " <> scope <> " === \"boolean\""
        | n == "Number"  = "typeof " <> scope <> " === \"number\""
        | otherwise      = ""
      compilePattern scope (PCtor n []) =
        scope <> ".__constructor === " <> "\"" <> removeNamespace n <> "\""
      compilePattern scope (PCtor n ps) =
        scope
          <> ".__constructor === "
          <> "\""
          <> removeNamespace n
          <> "\""
          <> if not (null args) then " && " <> args else ""
       where
        args =
          intercalate " && "
            $   filter (not . null)
            $   compileCtorArg scope n
            <$> zip [0 ..] ps
      compilePattern scope (PRecord m) =
        intercalate " && " $ filter (not . null) $ M.elems $ M.mapWithKey
          (compileRecord scope)
          m

      compilePattern scope (PSpread pat) = compilePattern scope pat
      compilePattern scope (PList   [] ) = scope <> ".length === 0"
      compilePattern scope (PList items) =
        scope
          <> ".length "
          <> lengthComparator items
          <> " "
          <> show (length items)
          <> " && "
          <> intercalate
               " && "
               (   (\(item, i) ->
                     compilePattern (scope <> "[" <> show i <> "]") item
                   )
               <$> zip items [0 ..]
               )
       where
        lengthComparator :: [Pattern] -> String
        lengthComparator pats = if containsSpread pats then ">=" else "==="
        containsSpread :: [Pattern] -> Bool
        containsSpread pats =
          let isSpread = \case
                PSpread _ -> True
                _         -> False
          in  case find isSpread pats of
                Just _  -> True
                Nothing -> False

      compilePattern _ _ = ""


      compileIs :: Is -> String
      compileIs (Solved _ _ (Is pat exp)) =
        "if ("
          <> compilePattern "__x__" pat
          <> ") {\n"
          <> buildVars "__x__" pat
          <> "    return "
          <> compile astPath outputPath exp
          <> ";\n  }\n"

      buildVars :: String -> Pattern -> String
      buildVars v p = case p of
        PRecord fields ->
          "    const { "
            <> intercalate
                 ", "
                 ( filter (not . null)
                 . ((snd <$>) . reverse . sort . M.toList)
                 $ M.mapWithKey buildFieldVar fields
                 )
            <> " } = "
            <> v
            <> ";\n"
         where
          buildFieldVar :: String -> Pattern -> String
          buildFieldVar name pat = case pat of
            PSpread (PVar n) -> "..." <> n
            PVar    n        -> if null name then n else name <> ": " <> n
            PRecord fields ->
              name
                <> ": { "
                <> intercalate
                     ", "
                     ( filter (not . null)
                     . ((snd <$>) . reverse . sort . M.toList)
                     $ M.mapWithKey buildFieldVar fields
                     )
                <> " }"
            PCtor _ args -> if null name
              then
                "{ __args: ["
                <> intercalate ", " (buildFieldVar "" <$> args)
                <> "] }"
              else
                name
                <> ": "
                <> "{ __args: ["
                <> intercalate
                     ", "
                     ((\(i, arg) -> buildFieldVar "" arg) <$> zip [0 ..] args)
                <> "] }"
            _ -> ""
        PList items ->
          let itemsStr = buildListVar <$> items
          in  "    const [" <> intercalate "," itemsStr <> "] = " <> v <> ";\n"
         where
          buildListVar :: Pattern -> String
          buildListVar pat = case pat of
            PSpread (PVar n) -> "..." <> n
            PVar    n        -> n
            _                -> ""
        PCtor _ ps ->
          concat
            $   (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "]") p)
            <$> zip [0 ..] ps
        PVar n -> "    const " <> n <> " = " <> v <> ";\n"

        _      -> ""

      compileRecord :: String -> Name -> Pattern -> String
      compileRecord scope n p = compilePattern (scope <> "." <> n) p

      compileCtorArg :: String -> String -> (Int, Pattern) -> String
      compileCtorArg scope _ (x, p) =
        compilePattern (scope <> ".__args[" <> show x <> "]") p

    _ -> "// Not implemented\n"


removeNamespace :: String -> String
removeNamespace = reverse . takeWhile (/= '.') . reverse


instance Compilable ADT where
  compile _ _ ADT { adtconstructors = [] } = ""
  compile astPath outputPath adt =
    let ctors    = adtconstructors adt
        exported = adtexported adt
    in  foldr (<>)
              ""
              (addExport exported . compile astPath outputPath <$> ctors)
   where
    addExport :: Bool -> String -> String
    addExport exported ctor = if exported then "export " <> ctor else ctor


instance Compilable Constructor where
  compile _ _ (Constructor cname cparams) =
    "const "
      <> cname
      <> " = curryPowder("
      <> compileParams cparams
      <> " => "
      <> compileBody cname cparams
      <> ");\n"
   where
    compileParams n =
      let argNames = (: []) <$> take (length n) ['a' ..]
      in  "(" <> intercalate ", " argNames <> ")"

    compileBody n a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          args     = argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"


compileImport :: FilePath -> FilePath -> FilePath -> Import -> String
compileImport rootPath outputPath astPath (NamedImport names path absPath) =
  let importPath = buildImportPath outputPath rootPath astPath absPath
  in  "import { " <> compileNames names <> " } from \"" <> importPath <> "\""
  where compileNames names = (init . init . concat) $ (++ ", ") <$> names

compileImport rootPath outputPath astPath (DefaultImport alias path absPath) =
  let importPath = buildImportPath outputPath rootPath astPath absPath
  in  "import " <> alias <> " from \"" <> importPath <> "\""

buildImportPath outputPath rootPath astPath absPath =
  let destPath = dropFileName $ computeTargetPath outputPath rootPath astPath
      depPath  = computeTargetPath outputPath rootPath absPath
  in  cleanRelativePath $ replaceExtension
        (joinPath ["./", makeRelativeEx destPath depPath])
        ".mjs"




instance Compilable AST where
  compile rootPath outputPath adt =
    let
      exps         = aexps adt
      adts         = aadts adt
      path         = apath adt
      imports      = aimports adt


      path'        = fromMaybe "Unknown" path

      infoComment  = "// file: " <> path' <> "\n"
      helpers      = curryPowder

      compiledAdts = case adts of
        [] -> ""
        x  -> foldr1 (<>) (compile rootPath outputPath <$> x)
      compiledExps = case exps of
        [] -> ""
        x  -> foldr1 (<>) (terminate . compile rootPath outputPath <$> x)
      compiledImports = case imports of
        [] -> ""
        x ->
          foldr1 (<>)
                 (terminate . compileImport rootPath outputPath path' <$> x)
            <> "\n"
      defaultExport = buildDefaultExport adts exps
    in
      infoComment
      <> compiledImports
      <> helpers
      <> compiledAdts
      <> compiledExps
      <> defaultExport
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"


buildDefaultExport :: [ADT] -> [Exp] -> String
buildDefaultExport as es =
  let expExportNames = getExportName <$> filter isExport es
      adtExportNames = getConstructorName
        <$> concat (adtconstructors <$> filter adtexported as)
      allDefaultExports = expExportNames <> adtExportNames
  in  case allDefaultExports of
        []      -> ""
        exports -> "export default { " <> intercalate ", " exports <> " };\n"

 where
  isExport :: Exp -> Bool
  isExport a = case a of
    (Solved _ _ (Export _)) -> True
    (Solved _ _ (TypedExp (Solved _ _ (Export _)) _)) -> True

    _ -> False

  getExportName :: Exp -> String
  getExportName (Solved _ _ (Export (Solved _ _ (Assignment n _)))) = n
  getExportName (Solved _ _ (TypedExp (Solved _ _ (Export (Solved _ _ (Assignment n _)))) _))
    = n

  getConstructorName :: Constructor -> String
  getConstructorName (Constructor cname _) = cname


curryPowder :: String
curryPowder = unlines
  [ "const toString = (fn, args = []) => () => ("
  , "  `curry(${fn.toString()})${args.length > 0 ? `(${args.join(`,`)})` : ``}`"
  , ")"
  , "const curryPowder = (fn) => {"
  , "  function curried(...args) {"
  , "    const length = args.length"
  , "    function saucy(...args2) {"
  , "      return curried.apply(this, args.concat(args2))"
  , "    }"
  , "    saucy.toString = toString(fn, args)"
  , "    return ("
  , "      length >= fn.length ?"
  , "      fn.apply(this, args) :"
  , "      saucy"
  , "    )"
  , "  }"
  , "  curried.toString = toString(fn)"
  , "  return curried"
  , "};"
  , ""
  ]
