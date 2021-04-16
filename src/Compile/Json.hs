module Compile.Json where

import qualified AST.Solved                    as Slv
import qualified Data.Map                      as M
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                )
import           Infer.Type
import           Explain.Location
import           Utils.Tuple                    ( lst )
import           Debug.Trace
import           Text.Show.Pretty
import           Error.Error
import           Explain.Format
import           Compile.Utils


indentSize :: Int
indentSize = 2

indent :: Int -> String
indent depth = concat $ replicate (depth * indentSize) " "




compileASTTable :: [(InferError, String)] -> Slv.Table -> String
compileASTTable errs table =
  let compiledASTs   = (\(path, ast) -> "\"" <> path <> "\": " <> compileAST ast) <$> M.toList table
      compiledErrors = intercalate ",\n    " $ compileError 2 <$> errs
  in  "{\n  \"asts\": {\n    "
        <> intercalate ",\n    " compiledASTs
        <> "\n  },\n  \"errors\": [\n    "
        <> compiledErrors
        <> "\n  ]\n}\n"


getErrorType :: InferError -> String
getErrorType (InferError err _) = case err of
  UnificationError t1 t2 -> "UnificationError"
  _                      -> "Error"


compileError :: Int -> (InferError, String) -> String
compileError depth (err@(InferError typeError ctx), formatted) =
  let area    = getCtxArea ctx
      errPath = getCtxPath ctx
      loc     = case area of
        Just a  -> indent (depth + 1) <> "\"loc\": " <> compileArea (depth + 1) a <> "\n"
        Nothing -> ""
      origin = case errPath of
        Just a  -> indent (depth + 1) <> "\"origin\": \"" <> a <> "\",\n"
        Nothing -> ""
  in  "{\n"
        <> indent (depth + 1)
        <> "\"errorType\": \""
        <> getErrorType err
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"message\": "
        <> escapeString formatted
        <> ",\n"
        <> origin
        <> loc
        <> indent depth
        <> "}"

compileAST :: Slv.AST -> String
compileAST ast =
  let (Just path)              = Slv.apath ast
      exps                     = Slv.aexps ast
      instances                = Slv.ainstances ast
      typeDeclarations         = Slv.atypedecls ast
      compiledExps             = intercalate ",\n        " $ compileExp 4 <$> exps
      compiledInstances        = intercalate ",\n        " $ compileInstance 4 <$> instances
      compiledTypeDeclarations = intercalate ",\n        " $ compileTypeDeclaration 4 <$> typeDeclarations
  in  "{\n      \"path\": \""
        <> path
        <> "\",\n"
        <> "      \"instances\": [\n"
        <> "        "
        <> compiledInstances
        <> "\n      ],\n"
        <> "      \"typeDeclarations\": [\n"
        <> "        "
        <> compiledTypeDeclarations
        <> "\n      ],\n"
        <> "      \"expressions\": [\n"
        <> "        "
        <> compiledExps
        <> "\n      ]\n"
        <> "    }"

compileTypeDeclaration :: Int -> Slv.TypeDecl -> String
compileTypeDeclaration depth (Slv.Untyped area td) = case td of
  Slv.ADT _ _ ctors _ _ ->
    let compiledConstructors =
            intercalate (",\n" <> indent (depth + 2)) $ compileConstructor (depth + 2) <$> Slv.adtconstructors td
    in  "{\n"
          <> indent (depth + 1)
          <> "\"nodeType\": \"ADT\",\n"
          <> indent (depth + 1)
          <> "\"loc\": "
          <> compileArea (depth + 1) area
          <> ",\n"
          <> indent (depth + 1)
          <> "\"name\": \""
          <> Slv.adtname td
          <> "\",\n"
          <> indent (depth + 1)
          <> "\"kind\": \""
          <> prettyPrintKind (buildKind (length $ Slv.adtparams td))
          <> "\",\n"
          <> indent (depth + 1)
          <> "\"constructors\": [\n"
          <> indent (depth + 2)
          <> compiledConstructors
          <> "\n"
          <> indent (depth + 1)
          <> "]\n"
          <> indent depth
          <> "}"

  _ ->
    "{\n"
      <> indent (depth + 1)
      <> "\"nodeType\": \"Alias\",\n"
      <> indent (depth + 1)
      <> "\"loc\": { \"start\": { \"line\": 0, \"col\": 0 }, \"end\": { \"line\": 0, \"col\": 0 }}\n"
      <> indent depth
      <> "}"

compileConstructor :: Int -> Slv.Constructor -> String
compileConstructor depth (Slv.Untyped area (Slv.Constructor name typings t)) =
  "{\n"
    <> indent (depth + 1)
    <> "\"nodeType\": \"Constructor\",\n"
    <> indent (depth + 1)
    <> "\"name\": \""
    <> name
    <> "\",\n"
    <> indent (depth + 1)
    <> "\"loc\": "
    <> compileArea (depth + 1) area
    <> ",\n"
    <> indent (depth + 1)
    <> "\"type\": \""
    <> prettyPrintType True t
    <> "\"\n"
    <> indent depth
    <> "}"

compileInstance :: Int -> Slv.Instance -> String
compileInstance depth (Slv.Untyped area inst@(Slv.Instance _ _ _ methods)) =
  let compiledLoc     = compileArea (depth + 1) area
      compiledMethods = compileInstanceMethods (depth + 2) methods
  in  "{\n"
        <> indent (depth + 1)
        <> "\"loc\": "
        <> compiledLoc
        <> ",\n"
        <> indent (depth + 1)
        <> "\"methods\": [\n"
        <> indent (depth + 2)
        <> compiledMethods
        <> "\n"
        <> indent (depth + 1)
        <> "]\n"
        <> indent depth
        <> "}"

compileInstanceMethods :: Int -> M.Map Name (Slv.Exp, Scheme) -> String
compileInstanceMethods depth methods = intercalate (",\n" <> indent depth) $ M.elems $ M.mapWithKey
  (\name (exp, scheme) -> compileInstanceMethod depth name exp scheme)
  methods

compileInstanceMethod :: Int -> Name -> Slv.Exp -> Scheme -> String
compileInstanceMethod depth name exp scheme =
  "{\n"
    <> indent (depth + 1)
    <> "\"name\": \""
    <> name
    <> "\",\n"
    <> indent (depth + 1)
    <> "\"exp\": "
    <> compileExp (depth + 1) exp
    <> "\n"
    <> indent depth
    <> "}"

compileExp :: Int -> Slv.Exp -> String
compileExp depth (Slv.Solved t area exp) =
  let compiledType      = prettyPrintType True t
      compiledExpFields = compileExpFields (depth + 1) exp
  in  "{\n"
        <> indent (depth + 1)
        <> "\"type\": \""
        <> prettyPrintType True t
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"loc\": "
        <> compileArea (depth + 1) area
        <> ",\n"
        <> compileExpFields (depth + 1) exp
        <> indent depth
        <> "}"

compileExpFields :: Int -> Slv.Exp_ -> String
compileExpFields depth exp = case exp of
  Slv.Var n -> indent depth <> "\"nodeType\": \"Variable\",\n" <> indent depth <> "\"name\": \"" <> n <> "\"\n"

  Slv.LNum val ->
    let compiledVal =
            if isInfixOf "Infinity" val || isInfixOf "-Infinity" val then "" <> escapeString val <> "" else val
    in  indent depth <> "\"nodeType\": \"LiteralNumber\",\n" <> indent depth <> "\"value\": " <> compiledVal <> "\n"

  Slv.LBool val ->
    indent depth <> "\"nodeType\": \"LiteralBoolean\",\n" <> indent depth <> "\"value\": " <> val <> "\n"

  Slv.LStr val ->
    indent depth
      <> "\"nodeType\": \"LiteralString\",\n"
      <> indent depth
      <> "\"value\": "
      <> (sanitizeVal . escapeString) val
      <> "\n"
   where
    sanitizeVal :: String -> String
    sanitizeVal s = if (length s > 3) && ((head s == '\\' && s !! 1 == '"') || (head s == '\\' && s !! 1 == '\''))
      then init . init . tail . tail $ s
      else s

  Slv.LUnit -> indent depth <> "\"nodeType\": \"LiteralUnit\"\n"

  Slv.Abs (Slv.Solved pType pArea pName) body ->
    indent depth
      <> "\"nodeType\": \"Abstraction\",\n"
      <> indent depth
      <> "\"param\": {\n"
      <> indent (depth + 1)
      <> "\"nodeType\": \"AbstractionParameter\",\n"
      <> indent (depth + 1)
      <> "\"name\": \""
      <> pName
      <> "\",\n"
      <> indent (depth + 1)
      <> "\"loc\": "
      <> compileArea (depth + 1) pArea
      <> ",\n"
      <> indent (depth + 1)
      <> "\"type\": \""
      <> prettyPrintType True pType
      <> "\"\n"
      <> indent depth
      <> "},\n"
      <> indent depth
      <> "\"body\": [\n"
      <> indent (depth + 1)
      <> intercalate (",\n" <> indent (depth + 1)) (compileExp (depth + 1) <$> body)
      <> "\n"
      <> indent depth
      <> "]\n"

  Slv.App abs arg _ ->
    indent depth
      <> "\"nodeType\": \"Application\",\n"
      <> indent depth
      <> "\"abstraction\": "
      <> compileExp depth abs
      <> ",\n"
      <> indent depth
      <> "\"argument\": "
      <> compileExp depth arg
      <> "\n"

  Slv.TypedExp exp scheme ->
    indent depth
      <> "\"nodeType\": \"TypedExpression\",\n"
      <> indent depth
      <> "\"expression\": "
      <> compileExp depth exp
      <> ",\n"
      <> indent depth
      <> "\"scheme\": \"TBD\"\n"

  Slv.Export exp ->
    indent depth <> "\"nodeType\": \"Export\",\n" <> indent depth <> "\"expression\": " <> compileExp depth exp <> "\n"

  Slv.NameExport name ->
    indent depth <> "\"nodeType\": \"NameExport\",\n" <> indent depth <> "\"name\": \"" <> name <> "\"\n"

  Slv.Assignment name exp ->
    indent depth
      <> "\"nodeType\": \"Assignment\",\n"
      <> indent depth
      <> "\"name\": \""
      <> name
      <> "\",\n"
      <> indent depth
      <> "\"expression\": "
      <> compileExp depth exp
      <> "\n"

  Slv.Access record field ->
    indent depth
      <> "\"nodeType\": \"Access\",\n"
      <> indent depth
      <> "\"record\": "
      <> compileExp depth record
      <> ",\n"
      <> indent depth
      <> "\"field\": "
      <> compileExp depth field
      <> "\n"

  Slv.If condition truthy falsy ->
    indent depth
      <> "\"nodeType\": \"If\",\n"
      <> indent depth
      <> "\"condition\": "
      <> compileExp depth condition
      <> ",\n"
      <> indent depth
      <> "\"truthy\": "
      <> compileExp depth truthy
      <> ",\n"
      <> indent depth
      <> "\"falsy\": "
      <> compileExp depth falsy
      <> "\n"

  Slv.Where exp iss ->
    indent depth
      <> "\"nodeType\": \"Where\",\n"
      <> indent depth
      <> "\"expression\": "
      <> compileExp depth exp
      <> ",\n"
      <> indent depth
      <> "\"isCases\": [\n"
      <> indent (depth + 1)
      <> intercalate (",\n" <> indent (depth + 1)) (compileIs (depth + 1) <$> iss)
      <> "\n"
      <> indent depth
      <> "]\n"

  Slv.Placeholder _ exp ->
    indent depth
      <> "\"nodeType\": \"Placeholder\",\n"
      <> indent depth
      <> "\"expression\": "
      <> compileExp depth exp
      <> "\n"

  Slv.TemplateString exps ->
    indent depth
      <> "\"nodeType\": \"TemplateString\",\n"
      <> indent depth
      <> "\"expressions\": [\n"
      <> indent (depth + 1)
      <> intercalate (",\n" <> indent (depth + 1)) (compileExp (depth + 1) <$> exps)
      <> indent depth
      <> "]\n"

  Slv.TupleConstructor exps ->
    indent depth
      <> "\"nodeType\": \"TupleConstructor\",\n"
      <> indent depth
      <> "\"expressions\": [\n"
      <> indent (depth + 1)
      <> intercalate (",\n" <> indent (depth + 1)) (compileExp (depth + 1) <$> exps)
      <> indent depth
      <> "]\n"

  Slv.ListConstructor items ->
    indent depth
      <> "\"nodeType\": \"ListConstructor\",\n"
      <> indent depth
      <> "\"items\": [\n"
      <> indent (depth + 1)
      <> intercalate (",\n" <> indent (depth + 1)) (compileListItem (depth + 1) <$> items)
      <> indent depth
      <> "]\n"

  Slv.Record fields ->
    indent depth
      <> "\"nodeType\": \"Record\",\n"
      <> indent depth
      <> "\"fields\": [\n"
      <> indent (depth + 1)
      <> intercalate (",\n" <> indent (depth + 1)) (compileField (depth + 1) <$> fields)
      <> indent depth
      <> "]\n"

  Slv.JSExp _ -> indent depth <> "\"nodeType\": \"JSExpression\",\n" <> indent depth <> "\"js\": \"-\"\n"

compileIs :: Int -> Slv.Is -> String
compileIs depth (Slv.Solved t area (Slv.Is pat exp)) =
  "{\n"
    <> indent (depth + 1)
    <> "\"nodeType\": \"Is\",\n"
    <> indent (depth + 1)
    <> "\"type\": \""
    <> prettyPrintType True t
    <> "\",\n"
    <> indent (depth + 1)
    <> "\"loc\":"
    <> compileArea (depth + 1) area
    <> ",\n"
    <> indent (depth + 1)
    <> "\"pattern\": "
    <> compilePattern (depth + 1) pat
    <> ",\n"
    <> indent (depth + 1)
    <> "\"expression\": "
    <> compileExp (depth + 1) exp
    <> "\n"
    <> indent depth
    <> "}"

compilePattern :: Int -> Slv.Pattern -> String
compilePattern depth (Slv.Solved t area pat) =
  "{\n"
    <> indent (depth + 1)
    <> "\"nodeType\": \"Pattern\",\n"
    <> indent (depth + 1)
    <> "\"loc\":"
    <> compileArea (depth + 1) area
    <> ",\n"
    <> indent (depth + 1)
    <> "\"type\": \""
    <> prettyPrintType True t
    <> "\"\n"
    <> indent depth
    <> "}"


compileListItem :: Int -> Slv.ListItem -> String
compileListItem depth li = case li of
  Slv.Solved t area (Slv.ListSpread exp) ->
    indent depth
      <> "{\n"
      <> indent (depth + 1)
      <> "\"type\": \""
      <> prettyPrintType True t
      <> "\",\n"
      <> indent (depth + 1)
      <> "\"loc\": "
      <> compileArea (depth + 1) area
      <> ",\n"
      <> indent (depth + 1)
      <> "\"itemType\": \"ListSpread\",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"

  Slv.Solved t area (Slv.ListItem exp) ->
    indent depth
      <> "{\n"
      <> indent (depth + 1)
      <> "\"type\": \""
      <> prettyPrintType True t
      <> "\",\n"
      <> indent (depth + 1)
      <> "\"loc\": "
      <> compileArea (depth + 1) area
      <> ",\n"
      <> indent (depth + 1)
      <> "\"itemType\": \"ListItem\",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"

compileField :: Int -> Slv.Field -> String
compileField depth (Slv.Solved t area field) = case field of
  Slv.FieldSpread exp ->
    indent depth
      <> "{\n"
      <> indent (depth + 1)
      <> "\"type\": \""
      <> prettyPrintType True t
      <> "\",\n"
      <> indent (depth + 1)
      <> "\"loc\": "
      <> compileArea (depth + 1) area
      <> ",\n"
      <> indent (depth + 1)
      <> "\"fieldType\": \"FieldSpread\",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"

  Slv.Field (name, exp) ->
    indent depth
      <> "{\n"
      <> indent (depth + 1)
      <> "\"type\": \""
      <> prettyPrintType True t
      <> "\",\n"
      <> indent (depth + 1)
      <> "\"loc\": "
      <> compileArea (depth + 1) area
      <> ",\n"
      <> indent (depth + 1)
      <> "\"itemType\": \"Field\",\n"
      <> indent (depth + 1)
      <> "\"fieldName\": \""
      <> name
      <> "\",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"


compileArea :: Int -> Area -> String
compileArea depth (Area start end) =
  "{\n"
    <> indent (depth + 1)
    <> "\"start\": "
    <> compileLoc start
    <> ",\n"
    <> indent (depth + 1)
    <> "\"end\": "
    <> compileLoc end
    <> "\n"
    <> indent depth
    <> "}"

compileLoc :: Loc -> String
compileLoc (Loc _ line col) = "{ \"line\": " <> show line <> ", \"col\": " <> show col <> " }"
