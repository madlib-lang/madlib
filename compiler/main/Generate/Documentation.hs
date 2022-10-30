{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Generate.Documentation where

import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Text.Lazy                 ( unpack )
import qualified AST.Solved                     as Slv
import qualified AST.Source                     as Src
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import qualified Data.Bifunctor
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
import           Utils.Tuple

data DocItemPair
  = DocItemPair (Maybe DocItem) (Maybe DocItem)
  deriving(Eq, Show)

data DocItem
  = DocItemExpression      { diName :: String, diDescription :: String, diExample :: String, diSince :: String, diType :: String }
  | DocItemTypeDeclaration { diName :: String, diParams :: String, diConstructors :: [String], diDescription :: String, diExample :: String, diSince :: String }
  | DocItemAlias           { diName :: String, diParams :: String, diAliasedType :: String, diDescription :: String, diExample :: String, diSince :: String }
  | DocItemInterface       { diName :: String, diVars :: String, diConstraints :: String, diMethods :: [String], diDescription :: String, diExample :: String, diSince :: String }
  | DocItemInstance        { diName :: String, diDeclaration :: String, diConstraints :: String, diDescription :: String, diExample :: String, diSince :: String }
  deriving(Eq, Show)


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
prepareInstancesForDocs ast = filter ((\name -> name /= "Eq" && name /= "Inspect") . Slv.getInstanceName) (Slv.ainstances ast)


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


isExpressionPair :: DocItemPair -> Bool
isExpressionPair pair = case pair of
  DocItemPair (Just DocItemExpression {}) _ ->
    True

  DocItemPair _ (Just DocItemExpression {}) ->
    True

  _ ->
    False


isTypeDeclarationPair :: DocItemPair -> Bool
isTypeDeclarationPair pair = case pair of
  DocItemPair (Just DocItemTypeDeclaration {}) _ ->
    True

  DocItemPair _ (Just DocItemTypeDeclaration {}) ->
    True

  _ ->
    False


isAliasPair :: DocItemPair -> Bool
isAliasPair pair = case pair of
  DocItemPair (Just DocItemAlias {}) _ ->
    True

  DocItemPair _ (Just DocItemAlias {}) ->
    True

  _ ->
    False


isInterfacePair :: DocItemPair -> Bool
isInterfacePair pair = case pair of
  DocItemPair (Just DocItemInterface {}) _ ->
    True

  DocItemPair _ (Just DocItemInterface {}) ->
    True

  _ ->
    False


isInstancePair :: DocItemPair -> Bool
isInstancePair pair = case pair of
  DocItemPair (Just DocItemInstance {}) _ ->
    True

  DocItemPair _ (Just DocItemInstance {}) ->
    True

  _ ->
    False



getDescriptionForDocItem :: Maybe DocString -> String
getDescriptionForDocItem docString = case docString of
  Just (FunctionDoc _ _ description _) ->
    description

  Just (TypeDefDoc _ _ description _) ->
    description

  Just (InterfaceDoc _ _ description _) ->
    description

  Just (InstanceDoc _ _ description _) ->
    description

  _ ->
    ""

getExampleForDocItem :: Maybe DocString -> String
getExampleForDocItem docString = case docString of
  Just (FunctionDoc _ _ _ tags) ->
    case findExampleTag tags of
      Just example ->
        example

      Nothing ->
        ""

  Just (TypeDefDoc _ _ _ tags) ->
    case findExampleTag tags of
      Just example ->
        example

      Nothing ->
        ""

  Just (InterfaceDoc _ _ _ tags) ->
    case findExampleTag tags of
      Just example ->
        example

      Nothing ->
        ""

  Just (InstanceDoc _ _ _ tags) ->
    case findExampleTag tags of
      Just example ->
        example

      Nothing ->
        ""

  _ ->
    ""

getSinceForDocItem :: Maybe DocString -> String
getSinceForDocItem docString = case docString of
  Just (FunctionDoc _ _ _ tags) ->
    case findSinceTag tags of
      Just since ->
        since

      Nothing ->
        ""

  Just (TypeDefDoc _ _ _ tags) ->
    case findSinceTag tags of
      Just since ->
        since

      Nothing ->
        ""

  Just (InterfaceDoc _ _ _ tags) ->
    case findSinceTag tags of
      Just since ->
        since

      Nothing ->
        ""

  Just (InstanceDoc _ _ _ tags) ->
    case findSinceTag tags of
      Just since ->
        since

      Nothing ->
        ""

  _ ->
    ""

buildExpressionDocItem :: [DocString] -> (String, Slv.Exp) -> DocItem
buildExpressionDocItem docStrings (name, exp) =
  let typing      = formatType exp
      docString   = findDocStringForExpName name docStrings
      description = getDescriptionForDocItem docString
      example     = getExampleForDocItem docString
      since       = getSinceForDocItem docString
  in  DocItemExpression { diName = name, diDescription = description, diExample = example, diSince = since, diType = typing }


buildTypeDeclarationDocItem :: [DocString] -> Slv.TypeDecl -> DocItem
buildTypeDeclarationDocItem docStrings typeDecl = case typeDecl of
  Slv.Untyped _ (Slv.Alias name params tipe _) ->
    let params'     = unwords params
        tipe'       = prettyPrintTyping tipe
        docString   = findDocStringForTypeName name docStrings
        description = getDescriptionForDocItem docString
        example     = getExampleForDocItem docString
        since       = getSinceForDocItem docString
    in  DocItemAlias { diName = name, diAliasedType = tipe', diParams = params', diDescription = description, diExample = example, diSince = since }

  Slv.Untyped _ (Slv.ADT name params ctors _ _) ->
    let params' = unwords params
        ctors' =
          (\(Slv.Untyped _ (Slv.Constructor n ts _)) ->
            if null ts then
              n
            else
              n <> "(" <> intercalate ", " (prettyPrintTyping <$> ts) <> ")"
          ) <$> ctors
        docString   = findDocStringForTypeName name docStrings
        description = getDescriptionForDocItem docString
        example     = getExampleForDocItem docString
        since       = getSinceForDocItem docString
    in  DocItemTypeDeclaration { diName = name, diParams = params', diConstructors = ctors', diDescription = description, diExample = example, diSince = since }


buildInterfaceDocItem :: [DocString] -> Slv.Interface -> DocItem
buildInterfaceDocItem docStrings (Slv.Untyped _ (Slv.Interface name constraints vars _ methodTypings)) =
  let vars'        = unwords $ (\(TV n _) -> n) <$> vars
      constraints' = case lst $ predsToStr False (mempty, mempty) constraints of
        "()" ->
          ""

        or   ->
          or

      methods'    = M.map (prettyPrintTyping' False) methodTypings
      methods''   = M.elems $ M.mapWithKey (\n t -> n <> " :: " <> t) methods'
      docString   = findDocStringForInterfaceName name docStrings
      description = getDescriptionForDocItem docString
      example     = getExampleForDocItem docString
      since       = getSinceForDocItem docString
  in  DocItemInterface { diName = name, diVars = vars', diConstraints = constraints', diMethods = methods'', diDescription = description, diExample = example, diSince = since }


buildInstanceDocItem :: [DocString] -> Slv.Instance -> DocItem
buildInstanceDocItem docStrings (Slv.Untyped _ (Slv.Instance name constraints declaration _)) =
  let constraints' = case lst $ predsToStr False (mempty, mempty) constraints of
        "()" ->
          ""

        or   ->
          or
      declaration' = lst $ predToStr False (mempty, mempty) declaration
      docString    = findDocStringForInstanceDeclaration declaration' docStrings
      description  = getDescriptionForDocItem docString
      example      = getExampleForDocItem docString
      since        = getSinceForDocItem docString
  in  DocItemInstance { diName = name, diDeclaration = declaration', diConstraints = constraints', diDescription = description, diExample = example, diSince = since }


buildJSDocItems :: [DocString] -> Slv.AST -> [DocItem]
buildJSDocItems docStrings ast =
  let docStrings'       = filter ((\target -> target == Src.TargetJS || target == Src.TargetAll) . getDocStringTarget) docStrings
      expDocItems       = buildExpressionDocItem docStrings' <$> prepareExportedExps ast
      typeDeclDocItems  = buildTypeDeclarationDocItem docStrings' <$> (prepareExportedADTs ast ++ prepareExportedAliases ast)
      interfaceDocItems = buildInterfaceDocItem docStrings' <$> prepareInterfacesForDocs ast
      instanceDocItems  = buildInstanceDocItem docStrings' <$> prepareInstancesForDocs ast
  in  expDocItems ++ typeDeclDocItems ++ interfaceDocItems ++ instanceDocItems


buildLLVMDocItems :: [DocString] -> Slv.AST -> [DocItem]
buildLLVMDocItems docStrings ast =
  let docStrings'       = filter ((\target -> target == Src.TargetLLVM || target == Src.TargetAll) . getDocStringTarget) docStrings
      expDocItems       = buildExpressionDocItem docStrings' <$> prepareExportedExps ast
      typeDeclDocItems  = buildTypeDeclarationDocItem docStrings' <$> (prepareExportedADTs ast ++ prepareExportedAliases ast)
      interfaceDocItems = buildInterfaceDocItem docStrings' <$> prepareInterfacesForDocs ast
      instanceDocItems  = buildInstanceDocItem docStrings' <$> prepareInstancesForDocs ast
  in  expDocItems ++ typeDeclDocItems ++ interfaceDocItems ++ instanceDocItems


-- returns a potentially found item and the rest of the elements
findCorrespondingDocItem :: [DocItem] -> DocItem -> [DocItem] -> (Maybe DocItem, [DocItem])
findCorrespondingDocItem skipped docItem docItems = case (docItem, docItems) of
  (_, []) ->
    (Nothing, skipped)

  (DocItemExpression { diName }, found@(DocItemExpression { diName = diName' }) : next) | diName == diName' ->
    (Just found, skipped ++ next)

  (DocItemTypeDeclaration { diName }, found@(DocItemTypeDeclaration { diName = diName' }) : next) | diName == diName' ->
    (Just found, skipped ++ next)

  (DocItemAlias { diName }, found@(DocItemAlias { diName = diName' }) : next) | diName == diName' ->
    (Just found, skipped ++ next)

  (DocItemInterface { diName }, found@(DocItemInterface { diName = diName' }) : next) | diName == diName' ->
    (Just found, skipped ++ next)

  (DocItemInstance { diDeclaration }, found@(DocItemInstance { diDeclaration = diDeclaration' }) : next) | diDeclaration == diDeclaration' ->
    (Just found, skipped ++ next)

  (_, item : next) ->
    findCorrespondingDocItem (item : skipped) docItem next


buildDocItemPairs :: [DocItem] -> [DocItem] -> [DocItemPair]
buildDocItemPairs jsDocItems llvmDocItems = case jsDocItems of
  item : next ->
    case findCorrespondingDocItem [] item llvmDocItems of
      (res, rest) ->
        DocItemPair (Just item) res : buildDocItemPairs next rest

  [] ->
    DocItemPair Nothing . Just <$> llvmDocItems


generateDocItem :: DocItem -> String
generateDocItem docItem = case docItem of
  DocItemExpression { diName, diDescription, diExample, diSince, diType } ->
    "{\n"
    <> indent 6
    <> "\"name\": "
    <> "\"" <> diName <> "\",\n"
    <> indent 6
    <> "\"description\": "
    <> escapeString diDescription <> ",\n"
    <> indent 6
    <> "\"example\": "
    <> escapeString diExample <> ",\n"
    <> indent 6
    <> "\"since\": "
    <> "\"" <> diSince <> "\",\n"
    <> indent 6
    <> "\"type\": "
    <> "\"" <> diType <> "\"\n"
    <> indent 5
    <> "}"

  DocItemTypeDeclaration { diName, diParams, diConstructors, diDescription, diExample, diSince } ->
    "{\n"
    <> indent 6
    <> "\"name\": "
    <> "\"" <> diName <> "\",\n"
    <> indent 6
    <> "\"params\": "
    <> "\"" <> diParams <> "\",\n"
    <> indent 6
    <> "\"constructors\": [\n"
    <> indent 7
    <> intercalate (",\n" <> indent 7) (("\"" <>) . (<> "\"") <$> diConstructors) <> "\n" <> indent 6 <> "],\n"
    <> indent 6
    <> "\"description\": "
    <> escapeString diDescription <> ",\n"
    <> indent 6
    <> "\"example\": "
    <> escapeString diExample <> ",\n"
    <> indent 6
    <> "\"since\": "
    <> "\"" <> diSince <> "\"\n"
    <> indent 5
    <> "}"

  DocItemAlias { diName, diParams, diAliasedType, diDescription, diExample, diSince } ->
    "{\n"
    <> indent 6
    <> "\"name\": "
    <> "\"" <> diName <> "\",\n"
    <> indent 6
    <> "\"params\": "
    <> "\"" <> diParams <> "\",\n"
    <> indent 6
    <> "\"aliasedType\": "
    <> "\"" <> diAliasedType <> "\",\n"
    <> indent 6
    <> "\"description\": "
    <> escapeString diDescription <> ",\n"
    <> indent 6
    <> "\"example\": "
    <> escapeString diExample <> ",\n"
    <> indent 6
    <> "\"since\": "
    <> "\"" <> diSince <> "\"\n"
    <> indent 5
    <> "}"

  DocItemInterface { diName, diVars, diConstraints, diMethods, diDescription, diExample, diSince } ->
    "{\n"
    <> indent 6
    <> "\"name\": "
    <> "\"" <> diName <> "\",\n"
    <> indent 6
    <> "\"vars\": "
    <> "\"" <> diVars <> "\",\n"
    <> indent 6
    <> "\"constraints\": "
    <> "\"" <> diConstraints <> "\",\n"
    <> indent 6
    <> "\"methods\": [\n"
    <> indent 7
    <> intercalate (",\n" <> indent 7) (("\"" <>) . (<> "\"") <$> diMethods) <> "\n" <> indent 6 <> "],\n"
    <> indent 6
    <> "\"description\": "
    <> escapeString diDescription <> ",\n"
    <> indent 6
    <> "\"example\": "
    <> escapeString diExample <> ",\n"
    <> indent 6
    <> "\"since\": "
    <> "\"" <> diSince <> "\"\n"
    <> indent 5
    <> "}"

  DocItemInstance { diName, diDeclaration, diConstraints, diDescription, diExample, diSince } ->
    "{\n"
    <> indent 6
    <> "\"name\": "
    <> "\"" <> diName <> "\",\n"
    <> indent 6
    <> "\"declaration\": "
    <> "\"" <> diDeclaration <> "\",\n"
    <> indent 6
    <> "\"constraints\": "
    <> "\"" <> diConstraints <> "\",\n"
    <> indent 6
    <> "\"description\": "
    <> escapeString diDescription <> ",\n"
    <> indent 6
    <> "\"example\": "
    <> escapeString diExample <> ",\n"
    <> indent 6
    <> "\"since\": "
    <> "\"" <> diSince <> "\"\n"
    <> indent 5
    <> "}"


generatePair :: DocItemPair -> String
generatePair docItemPair = case docItemPair of
  DocItemPair (Just js) (Just llvm) ->
    "{\n"
    <> indent 5
    <> "\"js\": "
    <> generateDocItem js
    <> ",\n"
    <> indent 5
    <> "\"llvm\": "
    <> generateDocItem llvm
    <> "\n"
    <> indent 4
    <> "}"

  DocItemPair (Just js) Nothing ->
    "{\n"
    <> indent 5
    <> "\"js\": "
    <> generateDocItem js
    <> "\n"
    <> indent 4
    <> "}"

  DocItemPair Nothing (Just llvm) ->
    "{\n"
    <> indent 5
    <> "\"llvm\": "
    <> generateDocItem llvm
    <> "\n"
    <> indent 4
    <> "}"


generateASTDoc :: (Slv.AST, Slv.AST, String, [DocString]) -> String
generateASTDoc (jsAST, llvmAST, mouleName, docStrings) =
  let astPath              = fromMaybe "unknown" $ Slv.apath jsAST
      description          = extractModuleDescription docStrings
      jsDocItems           = buildJSDocItems docStrings jsAST
      llvmDocItems         = buildLLVMDocItems docStrings llvmAST
      pairs                = buildDocItemPairs jsDocItems llvmDocItems
      expressionPairs      = filter isExpressionPair pairs
      typeDeclarationPairs = filter isTypeDeclarationPair pairs
      aliasPairs           = filter isAliasPair pairs
      interfacePairs       = filter isInterfacePair pairs
      instancePairs        = filter isInstancePair pairs
  in  "{\n"
      <> indent 3
      <> "\"path\": \""
      <> astPath
      <> "\",\n"
      <> indent 3
      <> "\"moduleName\": \""
      <> mouleName
      <> "\",\n"
      <> indent 3
      <> "\"description\": "
      <> escapeString description
      <> ",\n"
      <> indent 3
      <> "\"typeDeclarations\": [\n"
      <> indent 4
      <> intercalate (",\n" <> indent 4) (generatePair <$> typeDeclarationPairs)
      <> "\n"
      <> indent 3
      <> "],\n"
      <> indent 3
      <> "\"aliases\": [\n"
      <> indent 4
      <> intercalate (",\n" <> indent 4) (generatePair <$> aliasPairs)
      <> "\n"
      <> indent 3
      <> "],\n"
      <> indent 3
      <> "\"interfaces\": [\n"
      <> indent 4
      <> intercalate (",\n" <> indent 4) (generatePair <$> interfacePairs)
      <> "\n"
      <> indent 3
      <> "],\n"
      <> indent 3
      <> "\"instances\": [\n"
      <> indent 4
      <> intercalate (",\n" <> indent 4) (generatePair <$> instancePairs)
      <> "\n"
      <> indent 3
      <> "],\n"
      <> indent 3
      <> "\"expressions\": [\n"
      <> indent 4
      <> intercalate (",\n" <> indent 4) (generatePair <$> expressionPairs)
      <> "\n"
      <> indent 3
      <> "]\n"
      <> indent 2
      <> "}"


generateASTsDoc :: [(Slv.AST, Slv.AST, String, [DocString])] -> String
generateASTsDoc asts =
  let modules = intercalate (",\n" <> indent 2) $ generateASTDoc <$> asts
  in  "{\n"
      <> indent 1
      <> "\"modules\": [\n"
      <> indent 2
      <> modules
      <> "\n"
      <> indent 1
      <> "]\n"
      <> "}"


findDocStringForExpName :: String -> [DocString] -> Maybe DocString
findDocStringForExpName name = find $ functionDocNameEquals name


functionDocNameEquals :: String -> DocString -> Bool
functionDocNameEquals name docString = case docString of
  (FunctionDoc _ n _ _) -> n == name
  _                   -> False


findDocStringForTypeName :: String -> [DocString] -> Maybe DocString
findDocStringForTypeName name = find $ typeDefDocNameEquals name


typeDefDocNameEquals :: String -> DocString -> Bool
typeDefDocNameEquals name docString = case docString of
  (TypeDefDoc _ n _ _) -> n == name
  _                  -> False


findDocStringForInterfaceName :: String -> [DocString] -> Maybe DocString
findDocStringForInterfaceName name = find $ interfaceDocNameEquals name


interfaceDocNameEquals :: String -> DocString -> Bool
interfaceDocNameEquals name docString = case docString of
  (InterfaceDoc _ n _ _) -> n == name
  _                    -> False


findDocStringForInstanceDeclaration :: String -> [DocString] -> Maybe DocString
findDocStringForInstanceDeclaration decl = find $ instanceDocDeclEquals decl


instanceDocDeclEquals :: String -> DocString -> Bool
instanceDocDeclEquals decl docString = case docString of
  (InstanceDoc _ d _ _) ->
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


formatType :: Slv.Exp -> String
formatType (Slv.Typed qt _ exp) = case exp of
  -- Slv.TypedExp _ typing _ ->
  --   prettyPrintTyping' False typing

  _ ->
    prettyPrintQualType qt
