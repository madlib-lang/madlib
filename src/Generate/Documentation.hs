{-# LANGUAGE NamedFieldPuns #-}
module Generate.Documentation where

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
                                                , predToStr, prettyPrintQualType
                                                )
import           Data.List                      ( intercalate
                                                , find
                                                )
import           Parse.DocString.DocString
import           Generate.Utils
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Maybe
import           Infer.Type
import           Explain.Format
import           Text.Regex.TDFA
import Utils.Tuple


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
      exports                   = filter Slv.isExportOnly exps
      exportsWithNames = (\export -> (Slv.getExpName export, export)) <$> (exports ++ nameExportTargetExps)
      filteredExportedWithNames = filter (isJust . fst) exportsWithNames
      nameExportNames           = Slv.getNameExportName <$> filter Slv.isNameExport exps
      nameExportTargetExps =
          Maybe.mapMaybe (\name -> find (\export -> Slv.getExpName export == Just name) exps) nameExportNames
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
  let constraints' = case lst $ predsToStr False (mempty, mempty) constraints of
        "()" -> ""
        or   -> or
      declaration'     = lst $ predToStr False (mempty, mempty) declaration

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
      constraints' = case lst $ predsToStr False (mempty, mempty) constraints of
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


generateExpsDoc :: Int -> [DocString] -> [(String, Slv.Exp)] -> String
generateExpsDoc depth docStrings expInfos =
  intercalate (",\n" <> indent depth) (generateExpDoc depth docStrings <$> expInfos)

emptyExample :: Int -> String
emptyExample depth = indent depth <> "\"example\": \"\",\n"

emptySince :: Int -> String
emptySince depth = indent depth <> "\"since\": \"\",\n"

generateExpDoc :: Int -> [DocString] -> (String, Slv.Exp) -> String
generateExpDoc depth docStrings (name, exp) =
  let typing           = prettyPrintQualType True $ Slv.getQualType exp
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
