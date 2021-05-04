{-# LANGUAGE NamedFieldPuns #-}
module Compile.Documentation where

import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Text.Lazy                 ( unpack )
import qualified AST.Solved                    as Slv
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import qualified Data.Bifunctor
import           Explain.Format                 ( prettyPrintType
                                                , predsToStr
                                                , schemeToStr
                                                , predToStr
                                                )
import           Data.List                      ( intercalate
                                                , find
                                                )
import           Parse.DocString.DocString
import           Compile.Utils
import qualified Data.Map                      as M
import           Infer.Type
import           Text.Regex.TDFA


indentSize :: Int
indentSize = 2

indent :: Int -> String
indent depth = concat $ replicate (depth * indentSize) " "


isAliasExported :: Slv.TypeDecl -> Bool
isAliasExported typeDecl = case typeDecl of
  Slv.Untyped _ Slv.Alias { Slv.aliasexported } -> aliasexported
  _ -> False

prepareExportedADTs :: Slv.AST -> [Slv.TypeDecl]
prepareExportedADTs ast = filter Slv.isADTExported $ Slv.atypedecls ast

prepareExportedAliases :: Slv.AST -> [Slv.TypeDecl]
prepareExportedAliases ast = filter isAliasExported $ Slv.atypedecls ast


prepareInterfacesForDocs :: Slv.AST -> [Slv.Interface]
prepareInterfacesForDocs = Slv.ainterfaces

prepareInstancesForDocs :: Slv.AST -> [Slv.Instance]
prepareInstancesForDocs = Slv.ainstances


prepareExportedExps :: Slv.AST -> [(String, Slv.Exp)]
prepareExportedExps ast =
  let exps                      = Slv.aexps ast
      exports                   = filter Slv.isExport exps
      exportsWithNames          = (\export -> (Slv.getExpName export, export)) <$> exports
      filteredExportedWithNames = filter (isJust . fst) exportsWithNames
  in  Data.Bifunctor.first (fromMaybe "") <$> filteredExportedWithNames

generateASTDoc :: Int -> (Slv.AST, String, [DocString]) -> String
generateASTDoc depth (ast, fullModuleName, docStrings) =
  let astPath          = fromMaybe "unknown" $ Slv.apath ast
      expsForDoc       = prepareExportedExps ast
      adtsForDoc       = prepareExportedADTs ast
      aliasesForDoc    = prepareExportedAliases ast
      interfacesForDoc = prepareInterfacesForDocs ast
      instancesForDoc  = prepareInstancesForDocs ast
      description      = extractModuleDescription docStrings
  in  "{\n"
        <> indent (depth + 1)
        <> "\"path\": \""
        <> astPath
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"moduleName\": \""
        <> fullModuleName
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"description\": "
        <> escapeString description
        <> ",\n"
        <> indent (depth + 1)
        <> "\"typeDeclarations\": [\n"
        <> indent (depth + 2)
        <> generateADTsDoc (depth + 2) docStrings adtsForDoc
        <> "\n"
        <> indent (depth + 1)
        <> "],\n"
        <> indent (depth + 1)
        <> "\"aliases\": [\n"
        <> indent (depth + 2)
        <> generateAliasesDoc (depth + 2) docStrings aliasesForDoc
        <> "\n"
        <> indent (depth + 1)
        <> "],\n"
        <> indent (depth + 1)
        <> "\"interfaces\": [\n"
        <> indent (depth + 2)
        <> generateInterfacesDoc (depth + 2) docStrings interfacesForDoc
        <> "\n"
        <> indent (depth + 1)
        <> "],\n"
        <> indent (depth + 1)
        <> "\"instances\": [\n"
        <> indent (depth + 2)
        <> generateInstancesDoc (depth + 2) docStrings instancesForDoc
        <> "\n"
        <> indent (depth + 1)
        <> "],\n"
        <> indent (depth + 1)
        <> "\"expressions\": [\n"
        <> indent (depth + 2)
        <> generateExpsDoc (depth + 2) docStrings expsForDoc
        <> "\n"
        <> indent (depth + 1)
        <> "]\n"
        <> indent depth
        <> "}"

generateASTsDoc :: [(Slv.AST, String, [DocString])] -> String
generateASTsDoc asts =
  let depth   = 0
      modules = intercalate (",\n" <> indent (depth + 2)) $ generateASTDoc (depth + 2) <$> asts
  in  indent depth
        <> "{\n"
        <> indent (depth + 1)
        <> "\"modules\": [\n"
        <> indent (depth + 2)
        <> modules
        <> "\n"
        <> indent (depth + 1)
        <> "]\n"
        <> indent depth
        <> "}"


generateInstancesDoc :: Int -> [DocString] -> [Slv.Instance] -> String
generateInstancesDoc depth docStrings instances =
  intercalate (",\n" <> indent depth) (generateInstanceDoc depth docStrings <$> instances)

generateInstanceDoc :: Int -> [DocString] -> Slv.Instance -> String
generateInstanceDoc depth docStrings (Slv.Untyped _ (Slv.Instance name constraints declaration _)) =
  let constraints' = case predsToStr False constraints of
        "()" -> ""
        or   -> or
      declaration'     = predToStr False declaration

      docString        = findDocStringForInstanceDeclaration declaration' docStrings

      descriptionField = case docString of
        Just (InstanceDoc _ description _) ->
          indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"

        Nothing -> indent (depth + 1) <> "\"description\": \"\",\n"

      exampleField = case docString of
        Just (InstanceDoc _ _ tags) -> case findExampleTag tags of
          Just example -> indent (depth + 1) <> "\"example\": " <> escapeString example <> ",\n"
          Nothing      -> emptyExample $ depth + 1

        Nothing -> emptyExample $ depth + 1

      sinceField = case docString of
        Just (InstanceDoc _ _ tags) -> case findSinceTag tags of
          Just since -> indent (depth + 1) <> "\"since\": " <> escapeString since <> ",\n"
          Nothing    -> emptySince $ depth + 1

        Nothing -> emptySince $ depth + 1
  in  "{\n"
        <> indent (depth + 1)
        <> "\"name\": \""
        <> name
        <> "\",\n"
        <> descriptionField
        <> exampleField
        <> sinceField
        <> indent (depth + 1)
        <> "\"constraints\": \""
        <> constraints'
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"declaration\": \""
        <> declaration'
        <> "\"\n"
        <> indent depth
        <> "}"



generateInterfacesDoc :: Int -> [DocString] -> [Slv.Interface] -> String
generateInterfacesDoc depth docStrings interfaces =
  intercalate (",\n" <> indent depth) (generateInterfaceDoc depth docStrings <$> interfaces)

generateInterfaceDoc :: Int -> [DocString] -> Slv.Interface -> String
generateInterfaceDoc depth docStrings (Slv.Untyped _ (Slv.Interface name constraints vars _ methodTypings)) =
  let vars'        = unwords $ (\(TV n _) -> n) <$> vars
      constraints' = case predsToStr False constraints of
        "()" -> ""
        or   -> or
      methods'         = M.map (prettyPrintConstructorTyping' False) methodTypings
      methods''        = M.elems $ M.mapWithKey (\n t -> "\"" <> n <> " :: " <> t <> "\"") methods'
      methods'''       = intercalate (",\n" <> indent (depth + 2)) methods''
      docString        = findDocStringForInterfaceName name docStrings

      descriptionField = case docString of
        Just (InterfaceDoc _ description _) ->
          indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"

        Nothing -> indent (depth + 1) <> "\"description\": \"\",\n"

      exampleField = case docString of
        Just (InterfaceDoc _ _ tags) -> case findExampleTag tags of
          Just example -> indent (depth + 1) <> "\"example\": " <> escapeString example <> ",\n"
          Nothing      -> emptyExample $ depth + 1

        Nothing -> emptyExample $ depth + 1

      sinceField = case docString of
        Just (InterfaceDoc _ _ tags) -> case findSinceTag tags of
          Just since -> indent (depth + 1) <> "\"since\": " <> escapeString since <> ",\n"
          Nothing    -> emptySince $ depth + 1

        Nothing -> emptySince $ depth + 1
  in  "{\n"
        <> indent (depth + 1)
        <> "\"name\": \""
        <> name
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"vars\": \""
        <> vars'
        <> "\",\n"
        <> indent (depth + 1)
        <> "\"constraints\": \""
        <> constraints'
        <> "\",\n"
        <> descriptionField
        <> exampleField
        <> sinceField
        <> indent (depth + 1)
        <> "\"methods\": [\n"
        <> indent (depth + 2)
        <> methods'''
        <> "\n"
        <> indent (depth + 1)
        <> "]\n"
        <> indent depth
        <> "}"


generateADTsDoc :: Int -> [DocString] -> [Slv.TypeDecl] -> String
generateADTsDoc depth docStrings typeDecls =
  intercalate (",\n" <> indent depth) (generateADTDoc depth docStrings <$> typeDecls)

generateADTDoc :: Int -> [DocString] -> Slv.TypeDecl -> String
generateADTDoc depth docStrings typeDecl = case typeDecl of
  Slv.Untyped _ (Slv.ADT name params ctors _ _) ->
    let params' = unwords params
        ctors' =
            (\(Slv.Untyped _ (Slv.Constructor n ts _)) ->
                "\"" <> n <> " " <> unwords (prettyPrintConstructorTyping <$> ts) <> "\""
              )
              <$> ctors
        ctors''          = intercalate (",\n" <> indent (depth + 2)) ctors'
        docString        = findDocStringForTypeName name docStrings

        descriptionField = case docString of
          Just (TypeDefDoc _ description _) ->
            indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"

          Nothing -> indent (depth + 1) <> "\"description\": \"\",\n"

        exampleField = case docString of
          Just (TypeDefDoc _ _ tags) -> case findExampleTag tags of
            Just example -> indent (depth + 1) <> "\"example\": " <> escapeString example <> ",\n"
            Nothing      -> emptyExample $ depth + 1

          Nothing -> emptyExample $ depth + 1

        sinceField = case docString of
          Just (TypeDefDoc _ _ tags) -> case findSinceTag tags of
            Just since -> indent (depth + 1) <> "\"since\": " <> escapeString since <> ",\n"
            Nothing    -> emptySince $ depth + 1

          Nothing -> emptySince $ depth + 1
    in  "{\n"
          <> indent (depth + 1)
          <> "\"type\": \"ADT\",\n"
          <> descriptionField
          <> exampleField
          <> sinceField
          <> indent (depth + 1)
          <> "\"name\": \""
          <> name
          <> "\",\n"
          <> indent (depth + 1)
          <> "\"params\": \""
          <> params'
          <> "\",\n"
          <> indent (depth + 1)
          <> "\"constructors\": [\n"
          <> indent (depth + 2)
          <> ctors''
          <> "\n"
          <> indent (depth + 1)
          <> "]\n"
          <> indent depth
          <> "}"

  _ -> ""


generateAliasesDoc :: Int -> [DocString] -> [Slv.TypeDecl] -> String
generateAliasesDoc depth docStrings typeDecls =
  intercalate (",\n" <> indent depth) (generateAliasDoc depth docStrings <$> typeDecls)

generateAliasDoc :: Int -> [DocString] -> Slv.TypeDecl -> String
generateAliasDoc depth docStrings typeDecl = case typeDecl of
  Slv.Untyped _ (Slv.Alias name params tipe _) ->
    let params'          = unwords params
        tipe'            = prettyPrintConstructorTyping tipe
        docString        = findDocStringForTypeName name docStrings

        descriptionField = case docString of
          Just (TypeDefDoc _ description _) ->
            indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"

          Nothing -> indent (depth + 1) <> "\"description\": \"\",\n"

        exampleField = case docString of
          Just (TypeDefDoc _ _ tags) -> case findExampleTag tags of
            Just example -> indent (depth + 1) <> "\"example\": " <> escapeString example <> ",\n"
            Nothing      -> emptyExample $ depth + 1

          Nothing -> emptyExample $ depth + 1

        sinceField = case docString of
          Just (TypeDefDoc _ _ tags) -> case findSinceTag tags of
            Just since -> indent (depth + 1) <> "\"since\": " <> escapeString since <> ",\n"
            Nothing    -> emptySince $ depth + 1

          Nothing -> emptySince $ depth + 1
    in  "{\n"
          <> indent (depth + 1)
          <> "\"type\": \"Alias\",\n"
          <> descriptionField
          <> exampleField
          <> sinceField
          <> indent (depth + 1)
          <> "\"name\": \""
          <> name
          <> "\",\n"
          <> indent (depth + 1)
          <> "\"params\": \""
          <> params'
          <> "\",\n"
          <> indent (depth + 1)
          <> "\"aliasedType\": \""
          <> tipe'
          <> "\"\n"
          <> indent depth
          <> "}"


  _ -> ""

prettyPrintConstructorTyping :: Slv.Typing -> String
prettyPrintConstructorTyping t@(Slv.Untyped _ typing) = case typing of
  Slv.TRComp _ ts ->
    if not (null ts) then "(" <> prettyPrintConstructorTyping' False t <> ")" else prettyPrintConstructorTyping' False t
  Slv.TRArr _ _ -> "(" <> prettyPrintConstructorTyping' False t <> ")"
  _             -> prettyPrintConstructorTyping' True t

prettyPrintConstructorTyping' :: Bool -> Slv.Typing -> String
prettyPrintConstructorTyping' paren (Slv.Untyped _ typing) = case typing of
  Slv.TRSingle n -> n
  Slv.TRComp n typing' ->
    let space = if not (null typing') then " " else ""
    in  if paren
          then
            "("
            <> n
            <> space
            <> unwords ((\t -> prettyPrintConstructorTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')
            <> ")"
          else n <> space <> unwords ((\t -> prettyPrintConstructorTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')
  Slv.TRArr (Slv.Untyped _ (Slv.TRArr l r)) r' ->
    "("
      <> prettyPrintConstructorTyping' False l
      <> " -> "
      <> prettyPrintConstructorTyping' False r
      <> ") -> "
      <> prettyPrintConstructorTyping' False r'
  Slv.TRArr l r -> if paren
    then "(" <> prettyPrintConstructorTyping' False l <> " -> " <> prettyPrintConstructorTyping' False r <> ")"
    else prettyPrintConstructorTyping' False l <> " -> " <> prettyPrintConstructorTyping' False r
  Slv.TRTuple ts -> "<" <> intercalate ", " (prettyPrintConstructorTyping' False <$> ts) <> ">"
  Slv.TRRecord ts ->
    let mapped  = M.mapWithKey (\k v -> k <> " :: " <> prettyPrintConstructorTyping' False v) ts
        fields  = M.elems mapped
        fields' = intercalate ", " fields
    in  "{ " <> fields' <> " }"
  _ -> ""

isTRArrOrTRCompWithArgs :: Slv.Typing -> Bool
isTRArrOrTRCompWithArgs (Slv.Untyped _ typing) = case typing of
  Slv.TRArr  _ _  -> True
  Slv.TRComp _ ts -> not (null ts)
  _               -> False


generateExpsDoc :: Int -> [DocString] -> [(String, Slv.Exp)] -> String
generateExpsDoc depth docStrings expInfos =
  intercalate (",\n" <> indent depth) (generateExpDoc depth docStrings <$> expInfos)

emptyExample :: Int -> String
emptyExample depth = indent depth <> "\"example\": \"\",\n"

emptySince :: Int -> String
emptySince depth = indent depth <> "\"since\": \"\",\n"

generateExpDoc :: Int -> [DocString] -> (String, Slv.Exp) -> String
generateExpDoc depth docStrings (name, exp) =
  let typing           = prettyPrintType True $ Slv.getType exp
      docString        = findDocStringForExpName name docStrings
      descriptionField = case docString of
        Just (FunctionDoc _ description _) ->
          indent (depth + 1) <> "\"description\": " <> escapeString description <> ",\n"

        Nothing -> indent (depth + 1) <> "\"description\": \"\",\n"

      exampleField = case docString of
        Just (FunctionDoc _ _ tags) -> case findExampleTag tags of
          Just example -> indent (depth + 1) <> "\"example\": " <> escapeString example <> ",\n"
          Nothing      -> emptyExample $ depth + 1

        Nothing -> emptyExample $ depth + 1

      sinceField = case docString of
        Just (FunctionDoc _ _ tags) -> case findSinceTag tags of
          Just since -> indent (depth + 1) <> "\"since\": " <> escapeString since <> ",\n"
          Nothing    -> emptySince $ depth + 1

        Nothing -> emptySince $ depth + 1
  in  "{\n"
        <> indent (depth + 1)
        <> "\"name\": \""
        <> name
        <> "\",\n"
        <> descriptionField
        <> exampleField
        <> sinceField
        <> indent (depth + 1)
        <> "\"type\": \""
        <> typing
        <> "\"\n"
        <> indent depth
        <> "}"


findDocStringForExpName :: String -> [DocString] -> Maybe DocString
findDocStringForExpName name = find $ functionDocNameEquals name

functionDocNameEquals :: String -> DocString -> Bool
functionDocNameEquals name docString = case docString of
  (FunctionDoc n _ _) -> n == name
  _                   -> False

findDocStringForTypeName :: String -> [DocString] -> Maybe DocString
findDocStringForTypeName name = find $ typeDefDocNameEquals name

typeDefDocNameEquals :: String -> DocString -> Bool
typeDefDocNameEquals name docString = case docString of
  (TypeDefDoc n _ _) -> n == name
  _                  -> False

findDocStringForInterfaceName :: String -> [DocString] -> Maybe DocString
findDocStringForInterfaceName name = find $ interfaceDocNameEquals name

interfaceDocNameEquals :: String -> DocString -> Bool
interfaceDocNameEquals name docString = case docString of
  (InterfaceDoc n _ _) -> n == name
  _                    -> False

findDocStringForInstanceDeclaration :: String -> [DocString] -> Maybe DocString
findDocStringForInstanceDeclaration decl = find $ instanceDocDeclEquals decl

instanceDocDeclEquals :: String -> DocString -> Bool
instanceDocDeclEquals decl docString = case docString of
  (InstanceDoc d _ _) ->
    let regex                      = "[A-Z]+[a-zA-Z0-9_]*"
        concreteTypesFromDocString = getAllTextMatches (d =~ regex) :: [String]
        concreteTypesFromInstance  = getAllTextMatches (decl =~ regex) :: [String]
    in  concreteTypesFromDocString == concreteTypesFromInstance
  _ -> False

extractModuleDescription :: [DocString] -> String
extractModuleDescription docStrings =
  let filtered = filter isModuleDocString docStrings
      descs    = getModuleDocDescription <$> filtered
  in  intercalate "\n" descs
