module Format.Format where

import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Maybe
import           Text.Show.Pretty
import           Debug.Trace

import qualified Text.PrettyPrint.Leijen as PP

import AST.Source as Src
import Explain.Location
import Parse.Comments.Lexer




data Node
  = ImportNode Import
  | ExpNode Exp
  | TypeDeclNode TypeDecl
  | InterfaceNode Interface
  | InstanceNode Instance
  deriving(Eq, Show)


getNodeArea :: Node -> Area
getNodeArea node = case node of
  ImportNode imp ->
    getArea imp

  ExpNode exp ->
    getArea exp

  TypeDeclNode td ->
    getArea td

  InterfaceNode interface ->
    getArea interface

  InstanceNode inst ->
    getArea inst


astToNodeList :: AST -> [Node]
astToNodeList ast =
  let imports    = aimports ast
      exps       = aexps ast
      tds        = atypedecls ast
      interfaces = ainterfaces ast
      instances  = ainstances ast
  in  (ImportNode <$> imports)
      ++ (ExpNode <$> exps)
      ++ (TypeDeclNode <$> tds)
      ++ (InterfaceNode <$> interfaces)
      ++ (InstanceNode <$> instances)


sortASTNodes :: [Node] -> [Node]
sortASTNodes = sortOn getNodeArea




indentSize :: Int
indentSize = 2

argsToDoc :: [Comment] -> [Exp] -> (PP.Doc, [Comment])
argsToDoc comments args = case args of
  [exp] ->
    expToDoc comments exp

  (exp : more) ->
    let (arg, comments')    = expToDoc comments exp
        (more', comments'') = argsToDoc comments' more
    in  (arg <> PP.text "," <> PP.line <> more', comments'')

  [] ->
    (PP.empty, comments)

paramsToDoc :: [Comment] -> [Source String] -> (PP.Doc, [Comment])
paramsToDoc comments nodes = case nodes of
  [Source area name] ->
    let (commentsDoc, comments') = insertComments False area comments
    in  (commentsDoc <> PP.text name, comments')

  (Source area name : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        param                    = PP.text name <> PP.text ","
        (more', comments'')      = paramsToDoc comments' more
    in  (commentsDoc <> param <> PP.line <> more', comments'')

  [] ->
    (PP.empty, comments)

listItemsToDoc :: [Comment] -> [ListItem] -> (PP.Doc, [Comment])
listItemsToDoc comments nodes = case nodes of
  (Source _ (ListItem exp) : more) ->
    let (item, comments')   = expToDoc comments exp
        (more', comments'') = listItemsToDoc comments' more
        line =
          if null more then
            PP.empty
          else
            PP.line
    in  (item <> PP.text "," <> line <> more', comments'')

  (Source _ (ListSpread exp) : more) ->
    let (item, comments')   = expToDoc comments exp
        (more', comments'') = listItemsToDoc comments' more
        line =
          if null more then
            PP.empty
          else
            PP.line
    in  (PP.text "..." <> item <> PP.text "," <> line <> more', comments'')

  [] ->
    (PP.empty, comments)

fieldsToDoc :: [Comment] -> [Field] -> (PP.Doc, [Comment])
fieldsToDoc comments fields = case fields of
  (Source area (Field (name, exp)) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        name'                    = PP.text name
        (value, comments'')      = expToDoc comments' exp
        line  =
          if null more then
            PP.empty
          else
            PP.line
        (more', comments''') = fieldsToDoc comments' more
    in  ( commentsDoc <> name' <> PP.text ": " <> value <> PP.text "," <> line <> more'
        , comments'''
        )

  (Source area (FieldSpread exp) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        (item, comments'')       = expToDoc comments' exp
        line =
          if null more then
            PP.empty
          else
            PP.line
        (more', comments''') = fieldsToDoc comments'' more
    in  ( commentsDoc <> PP.text "..." <> item <> PP.text "," <> line <> more'
        , comments'''
        )

  (Source area (FieldShorthand name) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        item = PP.text name
        line =
          if null more then
            PP.empty
          else
            PP.line
        (more', comments'') = fieldsToDoc comments' more
    in  ( commentsDoc <> item <> PP.text "," <> line <> more'
        , comments''
        )

  [] ->
    (PP.empty, comments)

dictItemsToDoc :: [Comment] -> [DictItem] -> (PP.Doc, [Comment])
dictItemsToDoc comments fields = case fields of
  (Source area (DictItem key value) : more) ->
    let (key', comments')    = expToDoc comments key
        (value', comments'') = expToDoc comments' value
        (more', comments''') = dictItemsToDoc comments'' more
        line =
          if null more then
            PP.empty
          else
            PP.line
    in  ( value' <> PP.text ": " <> key' <> PP.text "," <> line <> more'
        , comments'''
        )

  [] ->
    (PP.empty, comments)


jsxPropsToDoc :: [Comment] -> [JsxProp] -> (PP.Doc, [Comment])
jsxPropsToDoc comments fields = case fields of
  (Source area (JsxProp key value) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        key'                     = PP.text key
        (value', comments'')     = expToDoc comments' value
        value'' = case value of
          Source _ (LStr _) ->
            value'

          _ ->
            PP.lbrace <> value' <> PP.rbrace
        line  =
          if null more then
            PP.empty
          else
            PP.line
        (more', comments''') = jsxPropsToDoc comments' more
    in  ( commentsDoc <> key' <> PP.equals <> value'' <> line <> more'
        , comments'''
        )

  [] ->
    (PP.empty, comments)

jsxChildrenToDoc :: [Comment] -> [JsxChild] -> (PP.Doc, [Comment])
jsxChildrenToDoc comments children = case children of
  (JsxChild (Source area (LStr s)) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        -- We need to remove the leading and trailing double quote
        s'                       = init (tail s)
        exp' =
          if last s' == ' ' then
            commentsDoc <> PP.text (init s') <> PP.line
          else
            commentsDoc <> PP.text s'
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (exp' <> more', comments'')

  (JsxChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (exp' <> more', comments'')

  (JsxSpreadChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (PP.lbrace <> PP.text "..." <> exp' <> PP.rbrace <> more', comments'')

  (JsxExpChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (PP.lbrace <> exp' <> PP.rbrace <> more', comments'')

  [] ->
    (PP.empty, comments)


patternFieldToDoc :: PatternField -> PP.Doc
patternFieldToDoc field = case field of
  PatternField (Source _ n) pat ->
    PP.text n <> PP.text ": " <> patternToDoc pat

  PatternFieldShorthand (Source _ n) ->
    PP.text n

patternToDoc :: Pattern -> PP.Doc
patternToDoc (Source _ pat) = case pat of
  PVar name ->
    PP.text name

  PNum n ->
    PP.text n

  PStr s ->
    PP.text s

  PBool b ->
    PP.text b

  PAny ->
    PP.text "_"

  PNullaryCon (Source _ name) ->
    PP.text name

  PCon (Source _ name) _ args _ ->
    let name' = PP.text name
        args' = PP.hcat $ PP.punctuate (PP.text ", ") (patternToDoc <$> args)
    in  name' <> PP.lparen <> args' <> PP.rparen

  PList _ ps _ ->
    PP.group
      (
        PP.text "["
        <> PP.nest indentSize (PP.linebreak <> PP.hcat (PP.punctuate (PP.text "," <> PP.line) (patternToDoc <$> ps)) <> PP.comma)
        <> PP.linebreak
        <> PP.text "]"
      )

  PTuple _ ps _ ->
    PP.group
      (
        PP.text "<"
        <> PP.nest indentSize (PP.linebreak <> PP.hcat (PP.punctuate (PP.text "," <> PP.line) (patternToDoc <$> ps)))
        <> PP.linebreak
        <> PP.text ">"
      )

  PRecord _ fields _ ->
    PP.group
      (
        PP.text "{"
        <> PP.nest indentSize (PP.line <> PP.hcat (PP.punctuate (PP.text "," <> PP.line) (patternFieldToDoc <$> fields)) <> PP.comma)
        <> PP.line
        <> PP.text "}"
      )

  PSpread _ p ->
    PP.text "..." <> patternToDoc p

issToDoc :: [Comment] -> [Is] -> (PP.Doc, [Comment])
issToDoc comments iss = case iss of
  (Source area (Is pat _ exp) : more) ->
    let pat'                           = patternToDoc pat
        (commentsBeforePat, comments') = insertComments False area comments
        (exp', comments'')             = expToDoc comments' exp
        (next, comments''')            = issToDoc comments'' more
        breaks =
          if null more then
            PP.empty
          else
            PP.nesting (\i -> PP.nest (-i) PP.linebreak) <> PP.linebreak
    in  ( commentsBeforePat
            <> pat'
            <> PP.text " =>"
            <> PP.nest indentSize (PP.line <> exp') <> breaks <> next
        , comments'''
        )

  [] ->
    (PP.empty, comments)


bodyToDoc :: [Comment] -> [Exp] -> (PP.Doc, [Comment])
bodyToDoc comments exps = case exps of
  [exp] ->
    expToDoc comments exp

  (exp : next) ->
    let (exp', comments') = expToDoc comments exp
        (next', comments'') = bodyToDoc comments' next
    in  (exp' <> PP.line <> next', comments'')

  [] ->
    (PP.empty, comments)


binOpToDocs :: [Comment] -> Exp -> ([PP.Doc], [Comment])
binOpToDocs comments exp = case exp of
  Source _ (BinOp operandL operator operandR) ->
    let (operandL', comments')   = binOpToDocs comments operandL
        (operator', comments'')  = expToDoc comments' operator
        (operandR', comments''') = binOpToDocs comments'' operandR
        operandL'' =
          case operandL of
            Source _ BinOp{} ->
              operandL'

            _ ->
              [PP.hcat operandL']
    in  (operandL'' <> [PP.line <> operator' <> PP.space] <> operandR', comments''')

  e ->
    let (e', comments') = expToDoc comments e
    in  ([e'], comments')

accessToDocs :: [Comment] -> Exp -> ([PP.Doc], [Comment])
accessToDocs comments exp = case exp of
  Source _ (Access rec field) ->
    let (rec', comments')    = accessToDocs comments rec
        (field', comments'') = expToDoc comments' field
    in  (rec' <> [PP.linebreak <> field'], comments'')

  Source _ (App fn args) ->
    let (fn', comments')    = accessToDocs comments fn
        (args', comments'') = argsToDoc comments' args
        args'' =
          if shouldNestApp args then
            PP.group (PP.lparen <> PP.nest indentSize (PP.linebreak <> args') <> PP.linebreak <> PP.rparen)
          else
            PP.lparen <> args' <> PP.rparen
        fn'' = case fn of
          Source _ Access{} ->
            fn' <> [args'']

          _ ->
            [PP.hcat $ fn' <> [args'']]
    in  (fn'', comments'')

  e ->
    let (e', comments') = expToDoc comments e
    in  ([e'], comments')

accessAsFNToDoc :: [Comment] -> Exp -> [Exp] -> (PP.Doc, [Comment])
accessAsFNToDoc comments access args =
  let (access', comments') = accessToDocs comments access
      (args', comments'')  = argsToDoc comments' args
      args'' =
        if shouldNestApp args then
          PP.group (PP.lparen <> PP.nest indentSize (PP.linebreak <> args') <> PP.linebreak <> PP.rparen)
        else
          PP.lparen <> args' <> PP.rparen
  in  ( PP.group (PP.nest indentSize (PP.hcat access' <> args''))
      , comments''
      )


typingArgsToDoc :: [Comment] -> [Typing] -> (PP.Doc, [Comment])
typingArgsToDoc comments typings = case typings of
  (typing : more) ->
    let (typing', comments') = typingToDoc comments typing
        typing'' = case typing of
          Source _ TRComp{} ->
            PP.lparen <> typing' <> PP.rparen

          Source _ TRArr{} ->
            PP.lparen <> typing' <> PP.rparen

          _ ->
            typing'
        (more', comments'') = typingArgsToDoc comments' more
        space =
          if null more then
            PP.empty
          else
            PP.space
    in  (typing'' <> space <> more', comments'')

  [] ->
    (PP.empty, comments)

typingListToDoc :: [Comment] -> [Typing] -> (PP.Doc, [Comment])
typingListToDoc comments typings = case typings of
  (typing : more) ->
    let (typing', comments') = typingToDoc comments typing
        (more', comments'')  = typingListToDoc comments' more
        comma =
          if null more then
            PP.empty
          else
            PP.text ", "
    in  (typing' <> comma <> more', comments'')

  [] ->
    (PP.empty, comments)


recordFieldTypingsToDoc :: [Comment] -> [(Src.Name, Typing)] -> (PP.Doc, [Comment])
recordFieldTypingsToDoc comments fields = case fields of
  ((name, typing) : more) ->
    let (typing', comments') = typingToDoc comments typing
        (more', comments'')  = recordFieldTypingsToDoc comments' more
        comma =
          if null more then
            PP.comma
          else
            PP.text ", "
    in  (PP.text name <> PP.text " :: " <> typing' <> comma <> more', comments'')

  [] ->
    (PP.empty, comments)

methodTypingsToDoc :: [Comment] -> [(Src.Name, Typing)] -> (PP.Doc, [Comment])
methodTypingsToDoc comments methods = case methods of
  ((name, typing) : more) ->
    let (typing', comments') = typingToDoc comments typing
        (more', comments'')  = methodTypingsToDoc comments' more
        after =
          if null more then
            PP.empty
          else
            PP.nesting (\i -> PP.nest (-i) PP.linebreak) <> PP.linebreak
    in  (PP.text name <> PP.text " :: " <> typing' <> after <> more', comments'')

  [] ->
    (PP.empty, comments)


typingToDoc :: [Comment] -> Typing -> (PP.Doc, [Comment])
typingToDoc comments typing = case typing of
  Source _ (TRSingle n) ->
    (PP.text n, comments)

  Source _ (TRComp n args) ->
    let (args', comments') = typingArgsToDoc comments args
    in  (PP.text n <> PP.space <> args', comments')

  Source _ (TRArr left right) ->
    let (left', comments')   = typingToDoc comments left
        (right', comments'') = typingToDoc comments' right
        left'' = case left of
          Source _ TRArr{} ->
            PP.lparen <> left' <> PP.rparen

          _ ->
            left'
    in  (left'' <> PP.text " -> " <> right', comments'')

  Source _ (TRTuple typings) ->
    let (typings', comments') = typingListToDoc comments typings
    in  (PP.text "<" <> typings' <> PP.text ">", comments')

  Source _ (TRRecord fields maybeExt) ->
    let (typings', comments')   = recordFieldTypingsToDoc comments (Map.toList fields)
        (maybeExt', comments'') = case maybeExt of
          Just ext ->
            let (ext', comments''') = typingToDoc comments' ext
            in  (PP.text "..." <> ext' <> PP.text ", ", comments''')

          Nothing ->
            (PP.empty, comments')
    in  (PP.text "{ " <> maybeExt' <> typings' <> PP.text " }", comments'')

  Source _ (TRConstrained constraints typing) ->
    let (constraints', comments') = typingListToDoc comments constraints
        constraints'' =
          if length constraints > 1 then
            PP.lparen <> constraints' <> PP.rparen
          else
            constraints'
        (typing', comments'') = typingToDoc comments' typing
    in  (constraints'' <> PP.text " => " <> typing', comments'')


templateStringExpsToDoc :: [Comment] -> [Exp] -> (PP.Doc, [Comment])
templateStringExpsToDoc comments exps = case exps of
  (e@(Source _ LStr{}) : more) ->
    let (e', comments')     = expToDoc comments e
        (more', comments'') = templateStringExpsToDoc comments' more
    in  (e' <> more', comments'')

  (e : more) ->
    let (e', comments')     = expToDoc comments e
        (more', comments'') = templateStringExpsToDoc comments' more
    in  (PP.group (PP.text "${" <> PP.nest indentSize (PP.linebreak <> e') <> PP.linebreak <> PP.text "}") <> more', comments'')

  [] ->
    (PP.empty, comments)


shouldNestApp :: [Exp] -> Bool
shouldNestApp args = case args of
  [Source _ Record{}] ->
    False

  [Source _ Dictionary{}] ->
    False

  [Source _ ListConstructor{}] ->
    False

  [Source _ TupleConstructor{}] ->
    False

  _ ->
    True


formatParams :: Bool -> PP.Doc -> PP.Doc
formatParams isSingle paramsDoc =
  if isSingle then
    PP.lparen <> paramsDoc <> PP.rparen
  else
    PP.group
      (
        PP.lparen
        <> PP.nest indentSize (PP.linebreak <> paramsDoc)
        <> PP.linebreak
      )
    <> PP.rparen

expToDoc :: [Comment] -> Exp -> (PP.Doc, [Comment])
expToDoc comments exp = 
  let (commentsDoc, comments') = insertComments False (getArea exp) comments
      (exp', comments'') = case exp of
        Source _ (App fn@(Source _ Access{}) args) ->
          accessAsFNToDoc comments' fn args

        Source _ (App fn args) ->
          let (fn', comments'')    = expToDoc comments' fn
              (args', comments''') = argsToDoc comments'' args
              args'' =
                if shouldNestApp args then
                  PP.group (PP.lparen <> PP.nest indentSize (PP.linebreak <> args') <> PP.linebreak <> PP.rparen)
                else
                  PP.lparen <> args' <> PP.rparen
          in  ( fn' <> args''
              , comments'''
              )

        Source _ (Abs params body) ->
          let (params', comments'') = paramsToDoc comments' params
              (body', comments''')  = expToDoc comments'' (head body)
              params''              = formatParams (length params == 1) params'
              arrowAndBody          = case body of
                [Source _ BinOp{}] ->
                  PP.text " =>" <> PP.nest indentSize (PP.softline <> body')

                _ ->
                  PP.text " => " <> body'
          in  ( params'' <> arrowAndBody, comments''')

        Source _ (AbsWithMultilineBody params body) ->
          let (params', comments'') = paramsToDoc comments' params
              (body', comments''') = bodyToDoc comments'' body
              params'' = formatParams (length params == 1) params'
          in  ( params''
                <> PP.text " => "
                <> PP.lbrace
                <> PP.nest indentSize (PP.line <> body')
                <> PP.line
                <> PP.rbrace
              , comments'''
              )

        Source _ (Do exps) ->
          let (exps', comments'') = bodyToDoc comments' exps
          in  ( PP.text "do "
                <> PP.lbrace
                <> PP.nest indentSize (PP.line <> exps')
                <> PP.line
                <> PP.rbrace
              , comments''
              )

        Source _ (Return exp) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (PP.text "return " <> exp', comments'')

        Source _ (Assignment name _ exp) ->
          let name'             = PP.text name
              (exp', comments'') = expToDoc comments' exp
          in  (name' <> PP.text " = " <> exp', comments'')

        Source _ (DoAssignment name exp) ->
          let name'             = PP.text name
              (exp', comments'') = expToDoc comments' exp
          in  (name' <> PP.text " <- " <> exp', comments'')

        Source _ (If _ cond truthy _ falsy) ->
          let (cond', comments'')    = expToDoc comments' cond
              (truthy', comments''') = expToDoc comments'' truthy
              (falsy', comments'''') = expToDoc comments''' falsy
          in  ( PP.group
                (
                  PP.text "if ("
                  <> PP.nest indentSize (PP.linebreak <> cond') <> PP.linebreak
                )
              <> PP.group
                  (
                    PP.text ") {"
                    <> PP.nest indentSize (PP.line <> truthy') <> PP.line <> PP.rbrace
                  )
              <> PP.group
                  (
                    PP.text " else {"
                    <> PP.nest indentSize (PP.line <> falsy') <> PP.line <> PP.rbrace
                  )
              , comments''''
              )

        Source _ (Ternary cond _ truthy _ falsy) ->
          let (cond', comments'')    = expToDoc comments' cond
              (truthy', comments''') = expToDoc comments'' truthy
              (falsy', comments'''') = expToDoc comments''' falsy
          in  ( PP.group
                (
                  cond' <>
                  PP.nest indentSize
                    (
                      PP.line
                      <> PP.text "? " <> truthy'
                      <> PP.line <> PP.text ": " <> falsy'
                    )
                )
              , comments''''
              )

        Source _ (Record fields) ->
          let (fields', comments'') = fieldsToDoc comments' fields
          in  ( PP.group
                  (
                    PP.lbrace
                    <> PP.nest indentSize (PP.line <> fields')
                    <> PP.line
                  )
                <> PP.rbrace
              , comments''
              )

        Source _ (Dictionary items) ->
          let (fields', comments'') = dictItemsToDoc comments' items
          in  ( PP.group
                  (
                    PP.text "{{"
                    <> PP.nest indentSize (PP.line <> fields')
                    <> PP.line
                  )
                <> PP.text "}}"
              , comments''
              )

        Source area (ListConstructor items) ->
          let (items', comments'')       = listItemsToDoc comments' items
              (commentsDoc, comments''') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments''
          in  ( PP.group
                  (
                    PP.lbracket
                    <> PP.nest indentSize (PP.linebreak <> items')
                    <> commentsDoc
                    <> PP.linebreak
                  )
                <> PP.rbracket
              , comments'''
              )

        Source area (TupleConstructor items) ->
          let (items', comments')       = argsToDoc comments items
              (commentsDoc, comments'') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments'
          in  ( PP.group
                  (
                    PP.text "<"
                    <> PP.nest indentSize (PP.linebreak <> items')
                    <> commentsDoc
                    <> PP.linebreak
                  )
                <> PP.text ">"
              , comments''
              )

        Source _ (Pipe exps) ->
          let (exps', comments'') = argsToDoc comments' exps
          in  ( PP.text "pipe("
                  <> PP.nest indentSize (PP.linebreak <> exps')
                  <> PP.linebreak
                  <> PP.text ")"
              , comments''
              )

        Source _ (Where _ exp _ iss _) ->
          let (exp', comments'')  = expToDoc comments' exp
              (iss', comments''') = issToDoc comments'' iss
              exp'' =
                if shouldNestApp [exp] then
                  PP.lparen
                  <> PP.nest indentSize (PP.linebreak <> exp')
                  <> PP.linebreak
                else
                  PP.lparen <> exp'
          in  ( PP.group (PP.text "where " <> exp'')
                <> PP.text ") {"
                <> PP.nest indentSize (PP.line <> iss')
                <> PP.line <> PP.rbrace
              , comments'''
              )

        Source _ (WhereAbs _ _ iss _) ->
          let (iss', comments'') = issToDoc comments' iss
          in  ( PP.text "where"
                <> PP.text " {"
                <> PP.nest indentSize (PP.line <> iss')
                <> PP.line
                <> PP.rbrace
              , comments''
              )

        Source _ (LNum n) ->
          (PP.text n, comments')

        Source _ (LStr s) ->
          (PP.text s, comments')

        Source _ (LBool b) ->
          (PP.text b, comments')

        Source _ LUnit ->
          (PP.text "()", comments')

        Source _ (Var n) ->
          let n' =
                case n of
                  "unary-minus" ->
                    "-"

                  a ->
                    a
          in (PP.text n', comments')

        Source _ (Parenthesized _ exp _) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (PP.lparen <> exp' <> PP.rparen, comments'')

        Source _ (UnOp operator operand) ->
          let (operator', comments'') = expToDoc comments' operator
              (operand', comments''') = expToDoc comments'' operand
          in  (operator' <> operand', comments''')

        binOp@(Source _ BinOp{}) ->
          let (parts, comments'') = binOpToDocs comments' binOp
              first               = head parts
              rest                = tail parts
          in  (PP.group (first <> PP.nest indentSize (PP.hcat rest)), comments'')

        access@(Source _ Access{}) ->
          let (parts, comments'') = accessToDocs comments' access
              first               = head parts
              rest                = tail parts
          in  (PP.group (first <> PP.nest indentSize (PP.hcat rest)), comments'')

        Source _ (TemplateString exps) ->
          let (content, comments'') = templateStringExpsToDoc comments' exps
          in  (PP.text "`" <> content <> PP.text "`", comments'')

        Source _ (Export exp) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (PP.text "export " <> exp', comments'')

        Source _ (NameExport n) ->
          (PP.text "export " <> PP.text n, comments')

        Source _ (TypeExport n) ->
          (PP.text "export type " <> PP.text n, comments')

        Source _ (JSExp js) ->
          let lines' = lines js
              -- js' = PP.vcat (PP.text . Text.unpack . Text.strip . Text.pack <$> lines')
              js' = PP.text js
          in
            if length lines' > 1 then
              (PP.text "#-" <> js' <> PP.text "-#", comments')
            else
              (PP.group (PP.text "#-" <> js' <> PP.text "-#"), comments')
              -- (PP.group (PP.text "#-" <> PP.line <> js' <> PP.line <> PP.text "-#"), comments')

        Source _ (JsxTag name props children) ->
          let (props', comments'')     = jsxPropsToDoc comments' props
              (children', comments''') = jsxChildrenToDoc comments'' children
              lineAfterName           =
                if null props then
                  PP.linebreak
                else
                  PP.line
          in  ( PP.group
                  (
                    PP.group
                      (
                        PP.text "<" <> PP.text name
                        <> PP.nest indentSize (lineAfterName <> props')
                        <> PP.linebreak
                        <> PP.text ">"
                      )
                    <> PP.nest indentSize (PP.linebreak <> children')
                    <> PP.linebreak
                    <> PP.text "</" <> PP.text name <> PP.text ">"
                  )
              , comments'''
              )

        Source _ (JsxAutoClosedTag name props) ->
          let (props', comments'')     = jsxPropsToDoc comments' props
          in  ( PP.group
                  (
                    PP.text "<" <> PP.text name
                    <> PP.nest indentSize (PP.line <> props')
                    <> PP.line
                    <> PP.text "/>"
                  )
              , comments''
              )

        Source _ (TypedExp exp typing) ->
          let (exp', comments'')     = expToDoc comments' exp
              (typing', comments''') = typingToDoc comments'' typing
          in  (PP.lparen <> exp' <> PP.text " :: " <> typing' <> PP.rparen, comments''')

        Source _ (NamedTypedExp name exp typing) ->
          let (exp', comments'')     = expToDoc comments' exp
              (typing', comments''') = typingToDoc comments'' typing
          in  (PP.text name <> PP.text " :: " <> typing' <> PP.linebreak <> exp', comments''')
  in  (commentsDoc <> exp', comments'')


importToDoc :: [Comment] -> Import -> (PP.Doc, [Comment])
importToDoc comments imp = case imp of
  Source _ (NamedImport names path _) ->
    let nameDocs = PP.text . getSourceContent <$> names
        namesDoc = PP.vsep (PP.punctuate PP.comma nameDocs)
        lineDoc  =
          if null names then
            PP.empty
          else
            PP.line
    in  ( PP.group
            (
              PP.text "import " <> PP.lbrace
              <> PP.nest indentSize (lineDoc <> namesDoc)
              <> lineDoc <> PP.rbrace
              <> PP.text " from " <> PP.text ("\"" ++ path ++ "\"")
            )
        , comments
        )

  Source _ (TypeImport names path _) ->
    let nameDocs = PP.text . getSourceContent <$> names
        namesDoc = PP.vsep (PP.punctuate PP.comma nameDocs)
    in  ( PP.group
            (
              PP.text "import type " <> PP.lbrace
              <> PP.nest indentSize (PP.line <> namesDoc)
              <> PP.line <> PP.rbrace
              <> PP.text " from " <> PP.text ("\"" ++ path ++ "\"")
            )
        , comments
        )

  Source _ (DefaultImport name path _) ->
    let nameDoc = (PP.text . getSourceContent) name
    in  ( PP.text "import "
          <> nameDoc
          <> PP.text " from "
          <> PP.text ("\"" ++ path ++ "\"")
        , comments
        )

  Source _ (ImportAll path _) ->
    (PP.text "import " <> PP.text ("\"" ++ path ++ "\""), comments)


constructorsToDoc :: [Comment] -> [Constructor] -> (PP.Doc, [Comment])
constructorsToDoc comments ctors = case ctors of
  (Source _ (Constructor name args) : more) ->
    let name' = PP.text name
        (args', comments')  = typingListToDoc comments args
        args''              =
          if null args then
            PP.empty
          else
            PP.lparen <> args' <> PP.rparen
        (more', comments'') = constructorsToDoc comments' more
        end =
          if null more then
            PP.empty
          else
            PP.line <> PP.text "|" <> PP.space
    in  (name' <> args'' <> end <> more', comments'')

  [] ->
    (PP.empty, comments)


typeDeclToDoc :: [Comment] -> TypeDecl -> (PP.Doc, [Comment])
typeDeclToDoc comments td = case td of
  Source _ adt@ADT{} ->
    let name   = PP.text (adtname adt)
        export =
          if adtexported adt then
            PP.text "export "
          else
            PP.empty
        params = PP.hcat (PP.punctuate PP.space (PP.text <$> adtparams adt))
        equals =
          if null (adtparams adt) then
            PP.linebreak <> PP.text "= "
          else
            PP.line <> PP.text "= "
        (constructors, comments') = constructorsToDoc comments (adtconstructors adt)
    in  ( export <> PP.text "type " <> name <> PP.space <> params
          <> PP.group (PP.nest indentSize (equals <> constructors))
        , comments'
        )

  Source _ alias@Alias{} ->
    let name = PP.text (aliasname alias)
        export =
          if aliasexported alias then
            PP.text "export "
          else
            PP.empty
        params =
          if null (aliasparams alias) then
            PP.empty
          else
            PP.hcat (PP.punctuate PP.space (PP.text <$> aliasparams alias)) <> PP.space
        (typing, comments') = typingToDoc comments (aliastype alias)
    in  ( export <> PP.text "alias " <> name <> PP.space <> params <> PP.text "= " <> typing
        , comments'
        )


interfaceToDoc :: [Comment] -> Interface -> (PP.Doc, [Comment])
interfaceToDoc comments interface = case interface of
  Source _ (Interface constraints name vars methods) ->
    let (constraints', comments') = typingListToDoc comments constraints
        constraints''
          | null constraints       = PP.empty
          | length constraints > 1 = PP.lparen <> constraints' <> PP.rparen <> PP.text " => "
          | otherwise              = constraints' <> PP.text " => "

        vars' = PP.hcat (PP.punctuate PP.space (PP.text <$> vars))
        (methods', comments'') = methodTypingsToDoc comments' (Map.toList methods)
    in  ( PP.text "interface " <> constraints'' <> PP.text name <> PP.space <> vars' <> PP.text " {" <> PP.nest indentSize (PP.line <> methods') <> PP.line <> PP.rbrace
        ,comments''
        )


methodsToDoc :: [Comment] -> [Exp] -> (PP.Doc, [Comment])
methodsToDoc comments methods = case methods of
  (exp : more) ->
    let (exp', comments') = expToDoc comments exp
        (more', comments'')  = methodsToDoc comments' more
        after =
          if null more then
            PP.empty
          else
            PP.nesting (\i -> PP.nest (-i) PP.linebreak) <> PP.linebreak
    in  (exp' <> after <> more', comments'')

  [] ->
    (PP.empty, comments)


instanceToDoc :: [Comment] -> Instance -> (PP.Doc, [Comment])
instanceToDoc comments inst = case inst of
  Source _ (Instance constraints name typings methods) ->
    let (constraints', comments') = typingListToDoc comments constraints
        constraints''
          | null constraints       = PP.empty
          | length constraints > 1 = PP.lparen <> constraints' <> PP.rparen <> PP.text " => "
          | otherwise              = constraints' <> PP.text " => "
        (typings', comments'')  = typingArgsToDoc comments typings
        (methods', comments''') = methodsToDoc comments'' (Map.elems methods)
    in  ( PP.text "instance " <> constraints'' <> PP.text name <> PP.space <> typings' <> PP.text " {" <> PP.nest indentSize (PP.line <> methods') <> PP.line <> PP.rbrace
        , comments'''
    )


commentToDoc :: Bool -> Comment -> PP.Doc
commentToDoc topLevel comment = case comment of
  Comment _ c ->
    let spaces =
          if length c > 200 then
            PP.empty
          else
            PP.text (concat $ replicate (200 - length c) " ")
    in PP.text c <> spaces <> PP.line

  MultilineComment _ c ->
    let break =
          if topLevel then
            PP.linebreak
          else
            PP.softbreak
    in PP.text c <> break


isInlineComment :: Comment -> Bool
isInlineComment comment = case comment of
  Comment _ c ->
    True

  MultilineComment _ c ->
    False


insertComments :: Bool -> Area -> [Comment] -> (PP.Doc, [Comment])
insertComments topLevel area@(Area (Loc _ nodeStartLine _) _) comments = case comments of
  (comment : more) ->
    let commentArea@(Area _ (Loc _ commentEndLine _)) = getCommentArea comment
        after                                         = area `isAfter` commentArea
        afterOrSameLine                               = after || isSameLine area commentArea && isInlineComment comment
    in
      if afterOrSameLine then
        let (next, comments') = insertComments topLevel area (tail comments)
            comment'          = commentToDoc topLevel comment
            comment''         =
              if nodeStartLine - commentEndLine > 1 then
                comment' <> PP.linebreak <> PP.linebreak
              else
                comment'
        in  (comment'' <> next, comments')
      else
        let (next, comments') = insertComments topLevel area (tail comments)
        in  (next, comment : comments')

  [] ->
    (PP.empty, comments)

insertRemainingComments :: [Comment] -> (PP.Doc, [Comment])
insertRemainingComments comments = case comments of
  (comment : more) ->
    let (next, comments') = insertRemainingComments (tail comments)
        comment'          = commentToDoc True comment
    in  (comment' <> next, comments')

  [] ->
    (PP.empty, comments)


nodesToDocs :: [Comment] -> [Node] -> (PP.Doc, [Comment])
nodesToDocs comments nodes = case nodes of
  (node : more) ->
    let (commentsDoc, comments') = insertComments True (getNodeArea node) comments
        (node', comments'')      =
          case node of
            ExpNode exp ->
              let (exp', comments'') = expToDoc comments' exp
                  emptyLines =
                    if null more then
                      PP.line
                    else
                      PP.line <> PP.line <> PP.line
              in  (exp' <> emptyLines, comments'')

            ImportNode imp ->
              let (imp', comments'') = importToDoc comments' imp
                  emptyLines = case more of
                    (ImportNode _ : _) ->
                      PP.line

                    _ ->
                      PP.line <> PP.line <> PP.line <> PP.line
              in  (imp' <> emptyLines, comments'')

            TypeDeclNode td ->
              let (td', comments'') = typeDeclToDoc comments' td
                  emptyLines =
                    if null more then
                      PP.line
                    else
                      PP.line <> PP.line <> PP.line
              in  (td' <> emptyLines, comments'')

            InterfaceNode interface ->
              let (interface', comments'') = interfaceToDoc comments' interface
                  emptyLines =
                    if null more then
                      PP.line
                    else
                      PP.line <> PP.line <> PP.line
              in  (interface' <> emptyLines, comments'')

            InstanceNode inst ->
              let (inst', comments'') = instanceToDoc comments' inst
                  emptyLines =
                    if null more then
                      PP.line
                    else
                      PP.line <> PP.line <> PP.line
              in  (inst' <> emptyLines, comments'')
        (more', comments''') = nodesToDocs comments'' more
    in  (commentsDoc <> node' <> more', comments''')

  [] ->
    let (commentsDoc, _) = insertRemainingComments comments
        commentsDoc' =
          if null comments then
            commentsDoc
          else
            PP.line <> commentsDoc
    in  (commentsDoc', comments)


removeCommentSpaces :: String -> String
removeCommentSpaces line =
  if "//" `isInfixOf` line then
    dropWhileEnd (== ' ') line
  else
    line

astToSource :: Int -> AST -> [Comment] -> String
astToSource width ast comments =
  let nodeList = (sortASTNodes . astToNodeList) ast
      (doc, _) = nodesToDocs comments nodeList
      rendered = PP.displayS (PP.renderPretty 1.0 width doc) ""
  in  unlines $ removeCommentSpaces <$> lines rendered
