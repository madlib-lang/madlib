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
import           Data.Char                      ( toLower )

import           AST.Solved
import           Explain.Location


class Compilable a where
  -- If the Bool is true it indicates that the expression terminates.
  compile :: a -> String

instance Compilable Exp where
  compile (Solved _ _ exp) = case exp of
    LInt  v     -> v
    LStr  v     -> "\"" <> v <> "\""
    LBool v     -> toLower <$> v

    App abs arg -> case abs of
      Solved _ _ (Var "+" ) -> "(" <> compile arg <> ") + "
      Solved _ _ (Var "-" ) -> "(" <> compile arg <> ") - "
      Solved _ _ (Var "*" ) -> "(" <> compile arg <> ") * "
      Solved _ _ (Var "/" ) -> "(" <> compile arg <> ") / "
      Solved _ _ (Var "==") -> "(" <> compile arg <> ") === "
      Solved _ _ (Var "&&") -> "(" <> compile arg <> ") && "
      Solved _ _ (Var "||") -> "(" <> compile arg <> ") || "
      Solved _ _ (Var ">" ) -> "(" <> compile arg <> ") > "
      Solved _ _ (Var "<" ) -> "(" <> compile arg <> ") < "
      Solved _ _ (Var ">=") -> "(" <> compile arg <> ") >= "
      Solved _ _ (Var "<=") -> "(" <> compile arg <> ") <= "
      Solved _ _ (App (Solved _ _ (Var "|>")) arg') ->
        compile arg <> "(" <> compile arg' <> ")"

      _ -> compile abs <> "(" <> compile arg <> ")"

    If cond truthy falsy ->
      "("
        <> compile cond
        <> " ? "
        <> compile truthy
        <> " : "
        <> compile falsy
        <> ")"

    -- Abs param body      -> "(" <> param <> " => " <> compile body <> ")"
    Abs param body      -> compileAbs Nothing param body
      where
        -- TODO: Check if parent is Nothing we add (
        -- Check if body is Abs we just call it again with a parent
        compileAbs :: Maybe Exp -> Name -> Exp -> String
        compileAbs parent param body =
          let start = case parent of
                Just _  -> ", " <> param
                Nothing -> "curryPowder((" <> param
              next = case body of
                (Solved _ _ (Abs param' body')) -> compileAbs (Just body) param' body'
                _                               -> ") => " <> compile body <> ")"
          in  start <> next

    Var name            -> name

    Assignment name exp -> "const " <> name <> " = " <> compile exp <> ""

    TypedExp   exp  _   -> compile exp

    Export (Solved _ _ (Assignment name exp)) ->
      "export const " <> name <> " = " <> compile exp <> ""

    Record fields ->
      -- Maybe just map and intercalate ?
      let fs = intercalate "," $ compileField <$> fields in "({" <> fs <> " })"
     where
      compileField :: Field -> String
      compileField field = case field of
        Field (name, exp) -> " " <> name <> ": " <> compile exp
        FieldSpread exp -> " ..." <> compile exp

    FieldAccess record field -> compile record <> compile field

    JSExp content            -> content

    ListConstructor elems ->
      "([" <> intercalate ", " (compileListItem <$> elems) <> "])"
     where
      compileListItem :: ListItem -> String
      compileListItem li = case li of
        ListItem   exp -> compile exp
        ListSpread exp -> " ..." <> compile exp

    Where exp (first : cs) ->
      "((__x__) => {\n  "
        <> compileIs first
        <> concat (("  else " ++) . compileIs <$> cs)
        -- TODO: Add an else for undefined patterns error and make it throw.
        <> "})("
        <> compile exp
        <> ")"
     where
      compilePattern :: String -> Pattern -> String
      compilePattern _     (PVar _) = "true"
      compilePattern _     PAny     = "true"
      compilePattern scope (PNum n) = scope <> " === " <> n
      compilePattern scope (PStr n) = scope <> " === \"" <> n <> "\""
      compilePattern scope (PBool n) | n == "true" = scope <> " === true"
                                     | otherwise   = scope <> " === false"
      compilePattern scope (PCon n)
        | n == "String" = "typeof " <> scope <> " === \"string\""
        | n == "Bool"   = "typeof " <> scope <> " === \"boolean\""
        | n == "Num"    = "typeof " <> scope <> " === \"number\""
        | otherwise     = ""
      compilePattern scope (PCtor n []) =
        scope <> ".__constructor === " <> "\"" <> n <> "\""
      compilePattern scope (PCtor n ps) =
        scope
          <> ".__constructor === "
          <> "\""
          <> n
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
          <> compile exp
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
            PVar    n        -> name <> ": " <> n
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
            $ (\(i, p) -> buildVars (v <> ".__args[" <> show i <> "].value") p)
            <$> zip [0 ..] ps
        PVar n -> "    const " <> n <> " = " <> v <> ";\n"
        -- PSpread (PVar n) -> "    const [..." <> n <> "] = " <> v <> ";\n"

        _      -> ""

      compileRecord :: String -> Name -> Pattern -> String
      compileRecord scope n p = compilePattern (scope <> "." <> n) p

      compileCtorArg :: String -> String -> (Int, Pattern) -> String
      compileCtorArg scope _ (x, p) =
        compilePattern (scope <> ".__args[" <> show x <> "].value") p

    _ -> "// Not implemented\n"


instance Compilable ADT where
  compile ADT { adtconstructors = [] } = ""
  compile ADT { adtconstructors } = foldr1 (<>) (compile <$> adtconstructors)


instance Compilable ADTConstructor where
  compile ADTConstructor { adtcname, adtcargs } = case adtcargs of
    Nothing ->
      "const " <> adtcname <> " = { __constructor: \"" <> adtcname <> "\" };\n"
    Just args ->
      "const "
        <> adtcname
        <> " = "
        <> compileArgs args
        <> compileBody adtcname args
        <> ";\n"
   where
    compileArgs n =
      let argNames = (: []) <$> take (length n) ['a' ..]
      in  foldr1 (<>) $ (<> " => ") <$> argNames

    compileBody n a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          args     = buildPCompArg <$> argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"

  compile _ = "// Not implemented\n"

buildPCompArg :: String -> String
buildPCompArg a = "__buildCtorParam(" <> a <> ")"

instance Compilable Import where
  compile (NamedImport names path) =
    "import { " <> compileNames names <> " } from \"./" <> path <> ".mjs\""
    where compileNames names = (init . init . concat) $ (++ ", ") <$> names
  compile (DefaultImport alias path) =
    "import " <> alias <> " from \"./" <> path <> ".mjs\""


instance Compilable AST where
  compile AST { aexps, aadts, apath, aimports } =

    let path        = fromMaybe "Unknown" apath

        infoComment = "// file: " <> path <> "\n"
        helpers     = curryPowder <> buildPCompArgFn

        adts        = case aadts of
          [] -> ""
          x  -> foldr1 (<>) (compile <$> x)
        exps = case aexps of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compile <$> x)
        imports = case aimports of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compile <$> x) <> "\n"
        defaultExport = buildDefaultExport aexps
    in  infoComment <> imports <> helpers <> adts <> exps <> defaultExport
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"


buildDefaultExport :: [Exp] -> String
buildDefaultExport es =
  let exports = filter isExport es
  in  case exports of
        [] -> ""
        exps ->
          "export default { "
            <> intercalate ", " (getExportName <$> exps)
            <> " };\n"

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


buildPCompArgFn :: String
buildPCompArgFn = unlines
  [ ""
  , "const __buildCtorParam = n => {"
  , "  if (typeof n === \"string\") {"
  , "    return { type: \"String\", value: n };"
  , "  } else {"
  , "    return { type: \"\", value: n };"
  , "  }"
  , "};"
  , ""
  ]

curryPowder :: String
curryPowder = unlines
  [ "" 
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
