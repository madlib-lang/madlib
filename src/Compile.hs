{-# LANGUAGE NamedFieldPuns #-}
module Compile where

import           Grammar

class Compilable a where
  -- If the Bool is True it indicates that the expression terminates.
  compile :: a -> String

instance Compilable Exp where
  compile LInt { eval } = eval
  compile LStr { eval } = "\"" <> eval <> "\""
  compile LBool { eval } | eval == "False" = "false"
                         | otherwise       = "true"

  compile App { eabs, earg } = case eabs of
    Var { ename = "+" }   -> "(" <> compile earg <> ") + "
    Var { ename = "===" } -> "(" <> compile earg <> ") === "
    _                     -> compile eabs <> "(" <> compile earg <> ")"

  compile Abs { ebody, eparam } =
    "(" <> eparam <> " => " <> compile ebody <> ")"

  compile Var { ename } = ename

  compile Assignment { ename, eexp } =
    "const " <> ename <> " = " <> compile eexp <> ""

  compile TypedExp { eexp } = case eexp of
    Var{} -> ""
    _     -> compile eexp

  compile _ = ""

instance Compilable ADT where
  compile ADT { adtconstructors } = foldr1 (<>) $ compile <$> adtconstructors

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

  compile ADTRecordConstructor {} = "// Record - Not Implemented \n"


instance Compilable AST where
  compile AST { aexps, aadts } =
    let adts = foldr1 (<>) (compile <$> aadts)
        exps = foldr1 (<>) (terminate . compile <$> aexps)
    in  adts <> exps
   where
    terminate :: String -> String
    terminate a | null a    = ""
                | otherwise = a <> ";\n"
