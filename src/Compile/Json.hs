module Compile.Json where

import qualified AST.Solved                    as Slv
import qualified Data.Map                      as M
import           Data.List                      ( intercalate )
import           Infer.Type
import           Explain.Location
import           Utils.Tuple                    ( lst )
import           Debug.Trace
import           Text.Show.Pretty


indentSize :: Int
indentSize = 2

indent :: Int -> String
indent depth = concat $ replicate (depth * indentSize) " "

compileASTTable :: Slv.Table -> String
compileASTTable table =
  let compiledASTs = (\(path, ast) -> "\"" <> path <> "\": " <> compileAST ast) <$> M.toList table
      wrapped      = "{\n  " <> intercalate ",\n  " compiledASTs <> "\n}\n"
  in  wrapped

compileAST :: Slv.AST -> String
compileAST ast =
  let (Just path)  = Slv.apath ast
      exps         = Slv.aexps ast
      compiledExps = intercalate ",\n      " $ compileExp 3 <$> exps
  in  "{\n    \"path\": \""
        <> path
        <> "\",\n"
        <> "    \"expressions\": [\n"
        <> "      "
        <> compiledExps
        <> "\n    ]"
        <> "\n  }"

compileExp :: Int -> Slv.Exp -> String
compileExp depth (Slv.Solved t area exp) =
  let compiledType      = compileType t
      compiledExpFields = compileExpFields (depth + 1) exp
  in  "{\n"
        <> indent (depth + 1)
        <> "\"type\": \""
        <> compileType t
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
  Slv.Var  n   -> indent depth <> "\"nodeType\": \"Variable\",\n" <> indent depth <> "\"name\": \"" <> n <> "\"\n"

  Slv.LNum val -> indent depth <> "\"nodeType\": \"LiteralNumber\",\n" <> indent depth <> "\"value\": " <> val <> "\n"

  Slv.LBool val ->
    indent depth <> "\"nodeType\": \"LiteralBoolean\",\n" <> indent depth <> "\"value\": " <> val <> "\n"

  Slv.LStr val ->
    indent depth <> "\"nodeType\": \"LiteralString\",\n" <> indent depth <> "\"value\": \"" <> val <> "\"\n"

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
      <> compileType pType
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

  Slv.FieldAccess record field ->
    indent depth
      <> "\"nodeType\": \"FieldAccess\",\n"
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
      <> indent depth
      <> "]\n"

  Slv.NamespaceAccess n ->
    indent depth <> "\"nodeType\": \"NamespaceAccess\",\n" <> indent depth <> "\"accessor\": \"" <> n <> "\"\n"

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
    <> "\"loc\":"
    <> compileArea (depth + 1) area
    <> ",\n"
    <> indent (depth + 1)
    <> "\"pattern\": \"NotImplemented\",\n"
    <> indent (depth + 1)
    <> "\"expression\": "
    <> compileExp (depth + 1) exp
    <> "\n"
    <> indent depth
    <> "}"


compileListItem :: Int -> Slv.ListItem -> String
compileListItem depth li = case li of
  Slv.ListSpread exp ->
    indent depth
      <> "{\n"
      <> indent (depth + 1)
      <> "\"itemType\": \"ListSpread\",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"

  Slv.ListItem exp ->
    indent depth
      <> "{\n"
      <> indent (depth + 1)
      <> "\"itemType\": \"ListItem\",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"

compileField :: Int -> Slv.Field -> String
compileField depth li = case li of
  Slv.FieldSpread exp ->
    indent depth
      <> "{\n"
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
      <> "\"itemType\": \"Field\",\n"
      <> indent (depth + 1)
      <> "\"fieldName\": "
      <> name
      <> ",\n"
      <> indent (depth + 1)
      <> "\"expression\": "
      <> compileExp (depth + 1) exp
      <> "\n"
      <> indent depth
      <> "}"

compileType :: Type -> String
compileType = lst . compileType' (mempty, mempty)

letters :: [Char]
letters = ['a' ..]

hkLetters :: [Char]
hkLetters = ['m' ..]

compileType' :: (M.Map String Int, M.Map String Int) -> Type -> (M.Map String Int, M.Map String Int, String)
compileType' (vars, hkVars) t = case t of
  TCon (TC n _) -> (vars, hkVars, n)

  TVar (TV n k) -> case k of
    Star -> case M.lookup n vars of
      Just x  -> (vars, hkVars, [letters !! x])
      Nothing -> let newIndex = M.size vars in (M.insert n newIndex vars, hkVars, [letters !! newIndex])

    Kfun _ _ -> case M.lookup n hkVars of
      Just x  -> (vars, hkVars, [hkLetters !! x])
      Nothing -> let newIndex = M.size hkVars in (vars, M.insert n newIndex hkVars, [hkLetters !! newIndex])

  TApp (TApp (TCon (TC "(,)" _)) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = compileType' (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = compileType' (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, "<" <> left <> ", " <> right <> ">")

  TApp (TApp (TCon (TC "(->)" _)) (TApp (TApp (TCon (TC "(->)" _)) tl) tr)) trr ->
    let (varsLeft      , hkVarsLeft      , left      ) = compileType' (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = compileType' (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = compileType' (varsRight, hkVarsRight) trr
    in  (varsRightRight, hkVarsRightRight, "(" <> left <> " -> " <> right <> ") -> " <> rightRight)

  TApp (TCon (TC "(->)" _)) (TApp (TApp (TCon (TC "(->)" _)) tl) tr) ->
    let (varsLeft , hkVarsLeft , left ) = compileType' (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = compileType' (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, "(" <> left <> " -> " <> right <> ")")

  TApp (TApp (TCon (TC "(->)" _)) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = compileType' (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = compileType' (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, left <> " -> " <> right)

  TApp tl (TApp (TApp (TCon (TC "(,)" _)) tll) tr) ->
    let (varsLeft    , hkVarsLeft    , left    ) = compileType' (vars, hkVars) tl
        (varsLeftLeft, hkVarsLeftLeft, leftLeft) = compileType' (varsLeft, hkVarsLeft) tll
        (varsRight   , hkVarsRight   , right   ) = compileType' (varsLeftLeft, hkVarsLeftLeft) tr
    in  (varsRight, hkVarsRight, left <> " <" <> leftLeft <> ", " <> right <> ">")

  TApp tl (TApp tll tr) ->
    let (varsLeft    , hkVarsLeft    , left    ) = compileType' (vars, hkVars) tl
        (varsLeftLeft, hkVarsLeftLeft, leftLeft) = compileType' (varsLeft, hkVarsLeft) tll
        (varsRight   , hkVarsRight   , right   ) = compileType' (varsLeftLeft, hkVarsLeftLeft) tr
    in  (varsRight, hkVarsRight, left <> " (" <> leftLeft <> " " <> right <> ")")

  TApp tl tr ->
    let (varsLeft , hkVarsLeft , left ) = compileType' (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = compileType' (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, left <> " " <> right)

  _ -> (vars, hkVars, "")

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
