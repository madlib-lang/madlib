{-# LANGUAGE NamedFieldPuns #-}
module Generate.Javascript.ModuleDoc where

import           Data.Char (isSpace)
import qualified Data.List as List
import           Generate.Javascript.Doc
import qualified Prettyprinter as Pretty


data JSImport
  = JSImportNamed [String] FilePath
  | JSImportBare FilePath
  deriving(Eq, Show)


data JSExpr
  = JSRawExpr JSDoc
  | JSObjectExpr [JSDoc]


data JSStmt
  = JSImportStmt JSImport
  | JSLineStmt JSDoc
  | JSRawBlock JSDoc
  | JSExportDefault JSExpr
  | JSCommentLine JSDoc


data JSModuleDoc
  = JSModuleDoc
      { mdHeaderComments :: [JSStmt]
      , mdPreludeImports :: [JSStmt]
      , mdImports        :: [JSStmt]
      , mdTypeDecls      :: [JSStmt]
      , mdDefinitions    :: [JSStmt]
      , mdFooter         :: [JSStmt]
      }


emptyModuleDoc :: JSModuleDoc
emptyModuleDoc =
  JSModuleDoc { mdHeaderComments = [], mdPreludeImports = [], mdImports = [], mdTypeDecls = [], mdDefinitions = [], mdFooter = [] }


renderModuleDoc :: JSModuleDoc -> JSDoc
renderModuleDoc JSModuleDoc { mdHeaderComments, mdPreludeImports, mdImports, mdTypeDecls, mdDefinitions, mdFooter } =
  let sections = [ mdHeaderComments, mdPreludeImports, mdImports, mdTypeDecls, mdDefinitions, mdFooter ]
      nonEmptySections = filter (not . null) sections
      docs = map (docVsep . map renderStmtDoc) nonEmptySections
  in  docVsep $ List.intersperse docHardline docs

renderModule :: JSModuleDoc -> String
renderModule = docRender . renderModuleDoc

renderStmtDoc :: JSStmt -> JSDoc
renderStmtDoc stmt = case stmt of
  JSImportStmt imp ->
    renderImportDoc imp <> docText ";"

  JSLineStmt line ->
    terminateJSLineDoc line

  JSRawBlock raw ->
    raw

  JSExportDefault expr ->
    docText "export default " <> renderExprDoc expr <> docText ";"

  JSCommentLine content ->
    content


renderExprDoc :: JSExpr -> JSDoc
renderExprDoc expr = case expr of
  JSRawExpr raw ->
    raw

  JSObjectExpr [] ->
    docText "{}"

  JSObjectExpr names ->
    docGroup
      $ docText "{"
      <> docNest 2 (docLine' <> commaSepLineDocs names)
      <> docLine'
      <> docText "}"


renderImportDoc :: JSImport -> JSDoc
renderImportDoc imp = case imp of
  JSImportBare path ->
    docText "import {} from \"" <> docText path <> docText "\""

  JSImportNamed names path ->
    Pretty.group
      $ docText "import { "
      <> commaSepDocs (map docText names)
      <> docText " } from \""
      <> docText path
      <> docText "\""


terminateJSLineDoc :: JSDoc -> JSDoc
terminateJSLineDoc lineDoc =
  let rendered = docRender lineDoc
      stripped  = reverse $ dropWhile isSpace $ reverse rendered
  in  if null stripped then
        Pretty.emptyDoc
      else if ";" `List.isSuffixOf` stripped || "}" `List.isSuffixOf` stripped then
        lineDoc
      else
        lineDoc <> docText ";"


commaSepDocs :: [JSDoc] -> JSDoc
commaSepDocs docs = case docs of
  [] ->
    Pretty.emptyDoc

  first : more ->
    foldl (\acc next -> acc <> docText ", " <> next) first more


commaSepLineDocs :: [JSDoc] -> JSDoc
commaSepLineDocs docs = case docs of
  [] ->
    Pretty.emptyDoc

  first : more ->
    foldl (\acc next -> acc <> docText "," <> docLine' <> next) first more
