{-# LANGUAGE NamedFieldPuns #-}
module Compile where

import           Grammar
import qualified Data.Map                      as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

class Compilable a where
  -- If the Bool is True it indicates that the expression terminates.
  compile :: a -> String

instance Compilable Exp where
  compile LInt { eval } = eval
  compile LStr { eval } = "\"" <> eval <> "\""
  compile LBool { eval } | eval == "False" = "false"
                         | otherwise       = "true"

  compile App { eabs, earg, efieldAccess = False } = case eabs of
    Var { ename = "+" }   -> "(" <> compile earg <> ") + "
    Var { ename = "===" } -> "(" <> compile earg <> ") === "
    _                     -> compile eabs <> "(" <> compile earg <> ")"

  compile App { eabs, earg, efieldAccess = True } =
    compile earg <> "." <> compile eabs

  compile Abs { ebody, eparam } =
    "(" <> eparam <> " => " <> compile ebody <> ")"

  compile Var { ename } = ename

  compile Assignment { ename, eexp, eexported } =
    let export = if eexported then "export " else ""
    in  export <> "const " <> ename <> " = " <> compile eexp <> ""

  compile TypedExp { eexp } = case eexp of
    Var{} -> ""
    _     -> compile eexp

  compile RecordCall { efields } =
    let fields = init $ M.foldrWithKey compileField "" efields
    in  "{" <> fields <> " }"
   where
    compileField name exp res =
      " " <> name <> ": " <> compile exp <> "," <> res

  compile JSExp { econtent } = econtent

  compile _ = "// Not implemented\n"

instance Compilable ADT where
  compile ADT { adtconstructors = [] } = ""
  compile ADT { adtconstructors }      = foldr1 (<>) $ compile <$> adtconstructors

instance Compilable ADTConstructor where
  compile ADTConstructor { adtcname, adtcargs } = case adtcargs of
    [] -> "const " <> adtcname <> " = []" <> ";\n"
    args ->
      "const "
        <> adtcname
        <> " = "
        <> compileArgs args
        <> compileBody args
        <> ";\n"
   where
    compileArgs n =
      let argNames = (: []) <$> take (length n) ['a' ..]
      in  foldr1 (<>) $ (<> " => ") <$> argNames

    compileBody a =
      let argNames = (: []) <$> take (length a) ['a' ..]
          argStr   = foldr1 (<>) $ (<> ", ") <$> argNames
      in  "([" <> init (init argStr) <> "])"

--   compile ADTRecordConstructor { adtcname, adtcfields } = undefined
  compile _ = "// Not implemented\n"

instance Compilable ImportDecl where
  compile ImportDecl { ipath, inames } =
    "import { " <> compileNames inames <> " } from \"./" <> ipath <> ".mjs\""
    where
        compileNames names = (init . init . concat) $ (++ ", ") <$> names


-- TODO: Add imports compilation
instance Compilable AST where
  compile AST { aexps, aadts, apath, aimports } =


    let path = fromMaybe "Unknown" apath

        infoComment = "// file: " <> path <> "\n"
        adts = case aadts of
          [] -> ""
          x  -> foldr1 (<>) (compile <$> x)
        exps = case aexps of
          [] -> ""
          x  -> foldr1 (<>) (terminate . compile <$> x)
        imports = case aimports of
          [] -> ""
          x -> foldr1 (<>) (terminate . compile <$> x) <> "\n"

    in  infoComment <> imports <> adts <> exps
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"
