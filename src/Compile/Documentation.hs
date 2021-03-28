module Compile.Documentation where

import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (unpack)
import qualified AST.Solved as Slv
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Bifunctor
import Explain.Format (prettyPrintType)
import Data.List (intercalate, find)
import Parse.DocString.DocString
import Compile.Utils


indentSize :: Int
indentSize = 2

indent :: Int -> String
indent depth = concat $ replicate (depth * indentSize) " "


prepareExportedExps :: Slv.AST -> [(String, Slv.Exp)]
prepareExportedExps ast =
  let exps                      = Slv.aexps ast
      exports                   = filter Slv.isExport exps
      exportsWithNames          = (\export -> (Slv.getExpName export, export)) <$> exports
      filteredExportedWithNames = filter (isJust . fst) exportsWithNames
  in  Data.Bifunctor.first (fromMaybe "") <$> filteredExportedWithNames

generateASTDoc :: Int -> (Slv.AST, [DocString]) -> String
generateASTDoc depth (ast, docStrings) =
  let astPath     = fromMaybe "unknown" $ Slv.apath ast
      expsForDoc  = prepareExportedExps ast
      description = extractModuleDescription docStrings
  in  "{\n"
    <> indent (depth + 1) <> "\"path\": \"" <> astPath <> "\",\n"
    <> indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"
    <> indent (depth + 1) <> "\"expressions\": [\n"
    <> indent (depth + 2) <> generateExpsDoc (depth + 2) docStrings expsForDoc <> "\n"
    <> indent (depth + 1) <> "]\n"
    <> indent depth <> "}"

generateASTsDoc :: [(Slv.AST, [DocString])] -> String
generateASTsDoc asts =
  let depth      = 0
      modules    = intercalate (",\n" <> indent depth) $ generateASTDoc (depth + 2) <$> asts
  in  indent depth <> "{\n"
    <> indent (depth + 1) <> "\"modules\": [\n"
    <> indent (depth + 2) <> modules <> "\n"
    <> indent (depth + 1) <> "]\n"
    <> indent depth <> "}"

generateExpsDoc :: Int -> [DocString] -> [(String, Slv.Exp)] -> String
generateExpsDoc depth docStrings expInfos = intercalate (",\n" <> indent depth) (generateExpDoc depth docStrings <$> expInfos)

generateExpDoc :: Int -> [DocString] -> (String, Slv.Exp) -> String
generateExpDoc depth docStrings (name, exp) =
  let typing    = prettyPrintType $ Slv.getType exp
      docString = findDocStringForExpName name docStrings
      descriptionField = case docString of
          Just (FunctionDoc _ description) ->
            indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"

          Nothing ->
            indent (depth + 1) <> "\"description\": \"\",\n"

  in  "{\n"
    <> indent (depth + 1) <> "\"name\": \"" <> name <> "\",\n"
    <> descriptionField
    <> indent (depth + 1) <> "\"type\": \"" <> typing <> "\"\n"
    <> indent depth <> "}"


findDocStringForExpName :: String -> [DocString] -> Maybe DocString
findDocStringForExpName name = find $ functionDocNameEquals name

functionDocNameEquals :: String -> DocString -> Bool
functionDocNameEquals name docString = case docString of
  (FunctionDoc n _) -> n == name
  _ -> False

extractModuleDescription :: [DocString] -> String
extractModuleDescription docStrings =
  let filtered = filter isModuleDocString docStrings
      descs    = getModuleDocDescription <$> filtered
  in  intercalate "\n" descs
