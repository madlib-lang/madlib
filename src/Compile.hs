{-# LANGUAGE NamedFieldPuns #-}
module Compile where

import           Grammar
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Debug.Trace                    ( trace )
import           Data.List                      ( intercalate )

class Compilable a where
  -- If the Bool is True it indicates that the expression terminates.
  compile :: a -> String

instance Compilable Exp where
  compile LInt { eval } = eval
  compile LStr { eval } = "\"" <> eval <> "\""
  compile LBool { eval } | eval == "False" = "false"
                         | otherwise       = "true"

  compile App { eabs, earg, efieldAccess = False } = case eabs of
    App { earg = ifArg, eabs = App { earg = condArg, eabs = Var { ename = "ifElse" } } }
      -> "("
        <> compile condArg
        <> " ? "
        <> compile ifArg
        <> " : "
        <> compile earg
        <> ")"
    Var { ename = "+" }   -> "(" <> compile earg <> ") + "
    Var { ename = "-" }   -> "(" <> compile earg <> ") - "
    Var { ename = "*" }   -> "(" <> compile earg <> ") * "
    Var { ename = "/" }   -> "(" <> compile earg <> ") / "
    Var { ename = "===" } -> "(" <> compile earg <> ") === "
    App { earg = earg', eabs = Var { ename = "|>" } } ->
      compile earg <> "(" <> compile earg' <> ")"
    _ -> compile eabs <> "(" <> compile earg <> ")"

  compile App { eabs, earg, efieldAccess = True } =
    compile earg <> compile eabs

  compile Abs { ebody, eparam } =
    "(" <> eparam <> " => " <> compile ebody <> ")"

  compile Var { ename } = ename

  compile Assignment { ename, eexp, eexported } =
    let export = if eexported then "export " else ""
    in  export <> "const " <> ename <> " = " <> compile eexp <> ""

  compile TypedExp { eexp } = case eexp of
    Var{} -> ""
    _     -> compile eexp

  compile Record { erfields } =
    let fields = init $ M.foldrWithKey compileField "" erfields
    in  "{" <> fields <> " }"
   where
    compileField name exp res =
      " " <> name <> ": " <> compile exp <> "," <> res

  compile JSExp { econtent } = econtent

  compile ListConstructor { eelems } =
    "[" <> intercalate ", " (compile <$> eelems) <> "]"

  compile Switch { eexp, ecases = (first : cs) } =
    "((__x__) => {\n  "
      <> compileCase first
      <> concat (("  else " ++) . compileCase <$> cs)
      -- TODO: Add an else for undefined patterns error and make it throw.
      <> "})("
      <> compile eexp
      <> ")"
   where
    compilePattern :: String -> Pattern -> String
    compilePattern _     (PVar _) = ""
    compilePattern _     PAny     = "true"
    compilePattern scope (PNum n) = scope <> " === " <> n
    compilePattern scope (PStr n) = scope <> " === \"" <> n <> "\""
    compilePattern scope (PBool n) | n == "True" = scope <> " === true"
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
        <> if not (null (args)) then " && " <> args else ""
     where
      args =
        intercalate " && "
          $   (filter (not . null))
          $   compileCtorArg scope n
          <$> zip [0 ..] ps
    compilePattern scope (PRecord m) =
      intercalate " && " $ (filter (not . null)) $ M.elems $ M.mapWithKey
        (compileRecord scope)
        m
    compilePattern scope (PUserDef n) = scope <> " === \"" <> n <> "\""
    compilePattern _     _            = ""

    compileCase :: Case -> String
    compileCase Case { casepattern, caseexp } =
      "if ("
        <> compilePattern "__x__" casepattern
        <> ") {\n"
        <> buildVars "__x__" casepattern
        <> "    return "
        <> compile caseexp
        <> ";\n  }\n"

    buildVars :: String -> Pattern -> String
    buildVars v p = case p of
      PRecord fields ->
        concat $ M.mapWithKey (\k p' -> buildVars (v <> "." <> k) p') fields
      PCtor _ ps ->
        concat
          $ (\(i, p) -> buildVars (v <> ".__args[" <> (show i) <> "].value") p)
          <$> zip [0 ..] ps
      PVar n -> "    const " <> n <> " = " <> v <> ";\n"
      _      -> ""

    compileRecord :: String -> Name -> Pattern -> String
    compileRecord scope n p = compilePattern (scope <> "." <> n) p

    compileCtorArg :: String -> String -> (Int, Pattern) -> String
    compileCtorArg scope _ (x, p) =
      compilePattern (scope <> ".__args[" <> show x <> "].value") p

  compile _ = "// Not implemented\n"


instance Compilable ADT where
  compile ADT { adtconstructors = [] }                = ""
  compile ADT { adtname, adtparams, adtconstructors } = foldr1
    (<>)
    (compile <$> adtconstructors)
   where
      -- TODO: Remove ?
      -- compileADTType :: Name -> [Name] -> String
      -- compileADTType name params = "const " <> name <> " = { __params: [" <> intercalate "," params <> "] };\n"

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
          -- TODO: Hook a function to each arg in order to define the type of them:
          -- ex:
          --   const Success = a => b => ({ __constructor: "Success", __args: [ { type: inputToType(a), value: a, { type: inputToType(b), value: b } ] });
          args     = buildPCompArg <$> argNames
          argStr   = intercalate ", " args
      in  "({ __constructor: \"" <> n <> "\", __args: [ " <> argStr <> " ] })"

  compile _ = "// Not implemented\n"

buildPCompArg :: String -> String
buildPCompArg a = "__buildCtorParam(" <> a <> ")"

instance Compilable ImportDecl where
  compile ImportDecl { ipath, inames } =
    "import { " <> compileNames inames <> " } from \"./" <> ipath <> ".mjs\""
    where compileNames names = (init . init . concat) $ (++ ", ") <$> names


-- TODO: Add imports compilation
instance Compilable AST where
  compile AST { aexps, aadts, apath, aimports } =


    let path        = fromMaybe "Unknown" apath

        infoComment = "// file: " <> path <> "\n"
        helpers     = buildPCompArgFn

        adts        = case aadts of
          [] -> ""
          x  -> foldr1 (<>) (compile <$> x)
        exps = case aexps of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compile <$> x)
        imports = case aimports of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compile <$> x) <> "\n"
    in  infoComment <> imports <> helpers <> adts <> exps
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"


buildPCompArgFn :: String
buildPCompArgFn = unlines
  [ "const __buildCtorParam = n => {"
  , "  if (typeof n === \"string\") {"
  , "    return { type: \"String\", value: n };"
  , "  } else {"
  , "    return { type: \"\", value: n };"
  , "  }"
  , "};\n"
  ]
