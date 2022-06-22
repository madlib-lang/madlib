{-# OPTIONS_GHC -Wno-deprecations #-}
module Format.Format where

import           Data.List
import qualified Data.Map                                as Map
import           Data.Text.Prettyprint.Doc               as Pretty
import           Data.Text.Prettyprint.Doc.Render.String as Pretty

import           AST.Source as Src
import           Explain.Location
import           Parse.Comments.Lexer




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


emptyLine :: Pretty.Doc ann
emptyLine =
  Pretty.nesting (\i -> Pretty.nest (-i) Pretty.line')


argsToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
argsToDoc comments args = case args of
  [exp] ->
    expToDoc comments exp

  (exp : more) ->
    let (arg, comments')    = expToDoc comments exp
        (more', comments'') = argsToDoc comments' more
    in  (arg <> Pretty.pretty "," <> Pretty.line <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


paramsToDoc :: [Comment] -> [Source String] -> (Pretty.Doc ann, [Comment])
paramsToDoc comments nodes = case nodes of
  [Source area _ name] ->
    let (commentsDoc, comments') = insertComments False area comments
    in  (commentsDoc <> Pretty.pretty name, comments')

  (Source area _ name : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        param                    = Pretty.pretty name <> Pretty.pretty ","
        (more', comments'')      = paramsToDoc comments' more
    in  (commentsDoc <> param <> Pretty.line <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


listItemsToDoc :: [Comment] -> [ListItem] -> (Pretty.Doc ann, [Comment])
listItemsToDoc comments nodes = case nodes of
  (Source _ _ (ListItem exp) : more) ->
    let (item, comments')   = expToDoc comments exp
        (more', comments'') = listItemsToDoc comments' more
        line =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
    in  (item <> Pretty.pretty "," <> line <> more', comments'')

  (Source _ _ (ListSpread exp) : more) ->
    let (item, comments')   = expToDoc comments exp
        (more', comments'') = listItemsToDoc comments' more
        line =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
    in  (Pretty.pretty "..." <> item <> Pretty.pretty "," <> line <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


fieldsToDoc :: [Comment] -> [Field] -> (Pretty.Doc ann, [Comment])
fieldsToDoc comments fields = case fields of
  (Source area _ (Field (name, exp)) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        name'                    = Pretty.pretty name
        (value, _)      = expToDoc comments' exp
        line  =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
        (more', comments''') = fieldsToDoc comments' more
    in  ( commentsDoc <> name' <> Pretty.pretty ": " <> value <> Pretty.pretty "," <> line <> more'
        , comments'''
        )

  (Source area _ (FieldSpread exp) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        (item, comments'')       = expToDoc comments' exp
        line =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
        (more', comments''') = fieldsToDoc comments'' more
    in  ( commentsDoc <> Pretty.pretty "..." <> item <> Pretty.pretty "," <> line <> more'
        , comments'''
        )

  (Source area _ (FieldShorthand name) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        item = Pretty.pretty name
        line =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
        (more', comments'') = fieldsToDoc comments' more
    in  ( commentsDoc <> item <> Pretty.pretty "," <> line <> more'
        , comments''
        )

  [] ->
    (Pretty.emptyDoc, comments)


dictItemsToDoc :: [Comment] -> [DictItem] -> (Pretty.Doc ann, [Comment])
dictItemsToDoc comments fields = case fields of
  (Source _ _ (DictItem key value) : more) ->
    let (key', comments')    = expToDoc comments key
        (value', comments'') = expToDoc comments' value
        (more', comments''') = dictItemsToDoc comments'' more
        line =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
    in  ( value' <> Pretty.pretty ": " <> key' <> Pretty.pretty "," <> line <> more'
        , comments'''
        )

  [] ->
    (Pretty.emptyDoc, comments)


jsxPropsToDoc :: [Comment] -> [JsxProp] -> (Pretty.Doc ann, [Comment])
jsxPropsToDoc comments fields = case fields of
  (Source area _ (JsxProp key value) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        key'                     = Pretty.pretty key
        (value', _)     = expToDoc comments' value
        value'' = case value of
          Source _ _ (LStr _) ->
            value'

          _ ->
            Pretty.lbrace <> value' <> Pretty.rbrace
        line  =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line
        (more', comments''') = jsxPropsToDoc comments' more
    in  ( commentsDoc <> key' <> Pretty.equals <> value'' <> line <> more'
        , comments'''
        )

  [] ->
    (Pretty.emptyDoc, comments)


jsxChildrenToDoc :: [Comment] -> [JsxChild] -> (Pretty.Doc ann, [Comment])
jsxChildrenToDoc comments children = case children of
  (JsxChild (Source area _ (LStr s)) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        -- We need to remove the leading and trailing double quote
        s'                       = init (tail s)
        exp' =
          if last s' == ' ' then
            commentsDoc <> Pretty.pretty (init s') <> Pretty.line
          else
            commentsDoc <> Pretty.pretty s'
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (exp' <> more', comments'')

  (JsxChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (exp' <> more', comments'')

  (JsxSpreadChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (Pretty.lbrace <> Pretty.pretty "..." <> exp' <> Pretty.rbrace <> more', comments'')

  (JsxExpChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (Pretty.lbrace <> exp' <> Pretty.rbrace <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


patternFieldToDoc :: PatternField -> Pretty.Doc ann
patternFieldToDoc field = case field of
  PatternField (Source _ _ n) pat ->
    Pretty.pretty n <> Pretty.pretty ": " <> patternToDoc pat

  PatternFieldShorthand (Source _ _ n) ->
    Pretty.pretty n


patternToDoc :: Pattern -> Pretty.Doc ann
patternToDoc (Source _ _ pat) = case pat of
  PVar name ->
    Pretty.pretty name

  PNum n ->
    Pretty.pretty n

  PFloat n ->
    Pretty.pretty n

  PChar c ->
    Pretty.pretty $ '\'':c:'\'':""

  PStr s ->
    Pretty.pretty s

  PBool b ->
    Pretty.pretty b

  PAny ->
    Pretty.pretty "_"

  PNullaryCon (Source _ _ name) ->
    Pretty.pretty name

  PCon (Source _ _ name) args ->
    let name' = Pretty.pretty name
        args' = Pretty.hcat $ Pretty.punctuate (Pretty.pretty ", ") (patternToDoc <$> args)
    in  name' <> Pretty.lparen <> args' <> Pretty.rparen

  PList ps ->
    Pretty.group
      (
        Pretty.pretty "["
        <> Pretty.nest indentSize (Pretty.line' <> Pretty.hcat (Pretty.punctuate (Pretty.pretty "," <> Pretty.line) (patternToDoc <$> ps)) <> Pretty.comma)
        <> Pretty.line'
        <> Pretty.pretty "]"
      )

  PTuple ps ->
    Pretty.group
      (
        Pretty.pretty "#["
        <> Pretty.nest indentSize (Pretty.line' <> Pretty.hcat (Pretty.punctuate (Pretty.pretty "," <> Pretty.line) (patternToDoc <$> ps)))
        <> Pretty.line'
        <> Pretty.pretty "]"
      )

  PRecord fields ->
    Pretty.group
      (
        Pretty.pretty "{"
        <> Pretty.nest indentSize (Pretty.line <> Pretty.hcat (Pretty.punctuate (Pretty.pretty "," <> Pretty.line) (patternFieldToDoc <$> fields)) <> Pretty.comma)
        <> Pretty.line
        <> Pretty.pretty "}"
      )

  PSpread p ->
    Pretty.pretty "..." <> patternToDoc p


issToDoc :: [Comment] -> [Is] -> (Pretty.Doc ann, [Comment])
issToDoc comments iss = case iss of
  (Source area _ (Is pat exp) : more) ->
    let pat'                           = patternToDoc pat
        (commentsBeforePat, comments') = insertComments False area comments
        (exp', comments'')             = expToDoc comments' exp
        (next, comments''')            = issToDoc comments'' more
        breaks =
          if null more then
            Pretty.emptyDoc
          else
            emptyLine <> Pretty.line'
    in  ( commentsBeforePat
            <> pat'
            <> Pretty.pretty " =>"
            <> Pretty.nest indentSize (Pretty.line <> exp') <> breaks <> next
        , comments'''
        )

  [] ->
    (Pretty.emptyDoc, comments)


bodyToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
bodyToDoc comments exps = case exps of
  [exp] ->
    expToDoc comments exp

  (exp : next) ->
    let (exp', comments')   = expToDoc comments exp
        (next', comments'') = bodyToDoc comments' next
        breaks              = case next of
          [Source _ _ Return{}] ->
            emptyLine <> Pretty.hardline

          _ ->
            Pretty.hardline
    in  (exp' <> breaks <> next', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


binOpToDocs :: [Comment] -> Exp -> ([Pretty.Doc ann], [Comment])
binOpToDocs comments exp = case exp of
  Source _ _ (BinOp operandL operator operandR) ->
    let (operandL', comments')   = binOpToDocs comments operandL
        (operator', comments'')  = expToDoc comments' operator
        (operandR', comments''') = binOpToDocs comments'' operandR
        operandL'' =
          case operandL of
            Source _ _ BinOp{} ->
              operandL'

            _ ->
              [Pretty.hcat operandL']
    in  (operandL'' <> [Pretty.line <> operator' <> Pretty.space] <> operandR', comments''')

  e ->
    let (e', comments') = expToDoc comments e
    in  ([e'], comments')


accessToDocs :: [Comment] -> Exp -> ([Pretty.Doc ann], [Comment])
accessToDocs comments exp = case exp of
  Source _ _ (Access rec field) ->
    let (rec', comments')    = accessToDocs comments rec
        (field', comments'') = expToDoc comments' field
    in  (rec' <> [Pretty.line' <> field'], comments'')

  Source _ _ (App fn args) ->
    let (fn', comments')    = accessToDocs comments fn
        (args', comments'') = argsToDoc comments' args
        args'' =
          if shouldNestApp args then
            Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> Pretty.line' <> Pretty.rparen)
          else
            Pretty.lparen <> args' <> Pretty.rparen
        fn'' = case fn of
          Source _ _ Access{} ->
            fn' <> [args'']

          _ ->
            [Pretty.hcat $ fn' <> [args'']]
    in  (fn'', comments'')

  e ->
    let (e', comments') = expToDoc comments e
    in  ([e'], comments')


accessAsFNToDoc :: [Comment] -> Exp -> [Exp] -> (Pretty.Doc ann, [Comment])
accessAsFNToDoc comments access args =
  let (access', comments') = accessToDocs comments access
      (args', comments'')  = argsToDoc comments' args
      args'' =
        if shouldNestApp args then
          Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> Pretty.line' <> Pretty.rparen)
        else
          Pretty.lparen <> args' <> Pretty.rparen
  in  ( Pretty.group (Pretty.nest indentSize (Pretty.hcat access' <> args''))
      , comments''
      )


typingArgsToDoc :: [Comment] -> [Typing] -> (Pretty.Doc ann, [Comment])
typingArgsToDoc comments typings = case typings of
  (typing : more) ->
    let (typing', comments') = typingToDoc comments typing
        typing'' = case typing of
          Source _ _ TRComp{} ->
            Pretty.lparen <> typing' <> Pretty.rparen

          Source _ _ TRArr{} ->
            Pretty.lparen <> typing' <> Pretty.rparen

          _ ->
            typing'
        (more', comments'') = typingArgsToDoc comments' more
        space =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.space
    in  (typing'' <> space <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


typingListToDoc :: [Comment] -> [Typing] -> (Pretty.Doc ann, [Comment])
typingListToDoc comments typings = case typings of
  (typing : more) ->
    let (typing', comments') = typingToDoc comments typing
        (more', comments'')  = typingListToDoc comments' more
        comma =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.pretty ", "
    in  (typing' <> comma <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


recordFieldTypingsToDoc :: [Comment] -> [(Src.Name, Typing)] -> (Pretty.Doc ann, [Comment])
recordFieldTypingsToDoc comments fields = case fields of
  ((name, typing) : more) ->
    let (typing', comments') = typingToDoc comments typing
        (more', comments'')  = recordFieldTypingsToDoc comments' more
        comma =
          if null more then
            Pretty.comma
          else
            Pretty.pretty ", "
    in  (Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> comma <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)

methodTypingsToDoc :: [Comment] -> [(Src.Name, Typing)] -> (Pretty.Doc ann, [Comment])
methodTypingsToDoc comments methods = case methods of
  ((name, typing) : more) ->
    let (typing', comments') = typingToDoc comments typing
        (more', comments'')  = methodTypingsToDoc comments' more
        after =
          if null more then
            Pretty.emptyDoc
          else
            emptyLine <> Pretty.line'
    in  (Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> after <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


typingToDoc :: [Comment] -> Typing -> (Pretty.Doc ann, [Comment])
typingToDoc comments typing = case typing of
  Source _ _ (TRSingle n) ->
    (Pretty.pretty n, comments)

  Source _ _ (TRComp n args) ->
    let (args', comments') = typingArgsToDoc comments args
    in  (Pretty.pretty n <> Pretty.space <> args', comments')

  Source _ _ (TRArr left right) ->
    let (left', comments')   = typingToDoc comments left
        (right', comments'') = typingToDoc comments' right
        left'' = case left of
          Source _ _ TRArr{} ->
            Pretty.lparen <> left' <> Pretty.rparen

          _ ->
            left'
    in  (left'' <> Pretty.pretty " -> " <> right', comments'')

  Source _ _ (TRTuple typings) ->
    let (typings', comments') = typingListToDoc comments typings
    in  (Pretty.pretty "#[" <> typings' <> Pretty.pretty "]", comments')

  Source _ _ (TRRecord fields maybeExt) ->
    let (typings', comments')   = recordFieldTypingsToDoc comments (Map.toList (snd <$> fields))
        (maybeExt', comments'') = case maybeExt of
          Just ext ->
            let (ext', comments''') = typingToDoc comments' ext
            in  (Pretty.pretty "..." <> ext' <> Pretty.pretty ", ", comments''')

          Nothing ->
            (Pretty.emptyDoc, comments')
    in  (Pretty.pretty "{ " <> maybeExt' <> typings' <> Pretty.pretty " }", comments'')

  Source _ _ (TRConstrained constraints typing) ->
    let (constraints', comments') = typingListToDoc comments constraints
        constraints'' =
          if length constraints > 1 then
            Pretty.lparen <> constraints' <> Pretty.rparen
          else
            constraints'
        (typing', comments'') = typingToDoc comments' typing
    in  (constraints'' <> Pretty.pretty " => " <> typing', comments'')


templateStringExpsToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
templateStringExpsToDoc comments exps = case exps of
  (e@(Source _ _ LStr{}) : more) ->
    let (e', comments')     = expToDoc comments e
        (more', comments'') = templateStringExpsToDoc comments' more
    in  (e' <> more', comments'')

  (e : more) ->
    let (e', comments')     = expToDoc comments e
        (more', comments'') = templateStringExpsToDoc comments' more
    in  ( Pretty.group
            (
              Pretty.pretty "${"
              <> Pretty.nest indentSize (Pretty.line' <> e')
              <> Pretty.line'
              <> Pretty.pretty "}"
            )
          <> more'
        , comments''
        )

  [] ->
    (Pretty.emptyDoc, comments)


shouldNestApp :: [Exp] -> Bool
shouldNestApp args = case args of
  [Source _ _ Record{}] ->
    False

  [Source _ _ Dictionary{}] ->
    False

  [Source _ _ ListConstructor{}] ->
    False

  [Source _ _ TupleConstructor{}] ->
    False

  _ ->
    True


formatParams :: Bool -> Pretty.Doc ann -> Pretty.Doc ann
formatParams isSingle paramsDoc =
  if isSingle then
    Pretty.lparen <> paramsDoc <> Pretty.rparen
  else
    Pretty.group
      (
        Pretty.lparen
        <> Pretty.nest indentSize (Pretty.line' <> paramsDoc)
        <> Pretty.line'
      )
    <> Pretty.rparen


expToDoc :: [Comment] -> Exp -> (Pretty.Doc ann, [Comment])
expToDoc comments exp = 
  let (commentsDoc, comments') = insertComments False (getArea exp) comments
      (exp', comments'') = case exp of
        Source _ _ (App fn@(Source _ _ Access{}) args) ->
          accessAsFNToDoc comments' fn args

        Source _ _ (App fn args) ->
          let (fn', comments'')    = expToDoc comments' fn
              (args', comments''') = argsToDoc comments'' args
              args'' =
                if shouldNestApp args then
                  Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> Pretty.line' <> Pretty.rparen)
                else
                  Pretty.lparen <> args' <> Pretty.rparen
          in  ( fn' <> args''
              , comments'''
              )

        Source _ _ (Abs params body) ->
          let (params', comments'') = paramsToDoc comments' params
              (body', comments''')  = expToDoc comments'' (head body)
              params''              = formatParams (length params == 1) params'
              arrowAndBody          = case body of
                [Source _ _ BinOp{}] ->
                  Pretty.pretty " =>" <> Pretty.nest indentSize (Pretty.softline <> body')

                _ ->
                  Pretty.pretty " => " <> body'
          in  ( params'' <> arrowAndBody, comments''')

        Source _ _ (AbsWithMultilineBody params body) ->
          let (params', comments'') = paramsToDoc comments' params
              (body', comments''') = bodyToDoc comments'' body
              params'' = formatParams (length params == 1) params'
          in  ( params''
                <> Pretty.pretty " => "
                <> Pretty.lbrace
                <> Pretty.nest indentSize (Pretty.line <> body')
                <> Pretty.line
                <> Pretty.rbrace
              , comments'''
              )

        Source _ _ (Do exps) ->
          let (exps', comments'') = bodyToDoc comments' exps
          in  ( Pretty.pretty "do "
                <> Pretty.lbrace
                <> Pretty.nest indentSize (Pretty.line <> exps')
                <> Pretty.line
                <> Pretty.rbrace
              , comments''
              )

        Source _ _ (Return exp) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (Pretty.pretty "return " <> exp', comments'')

        Source _ _ (Assignment name exp) ->
          let name'             = Pretty.pretty name
              (exp', comments'') = expToDoc comments' exp
          in  (name' <> Pretty.pretty " = " <> exp', comments'')

        Source _ _ (DoAssignment name exp) ->
          let name'             = Pretty.pretty name
              (exp', comments'') = expToDoc comments' exp
          in  (name' <> Pretty.pretty " <- " <> exp', comments'')

        Source _ _ (If cond truthy falsy) ->
          let (cond', comments'')    = expToDoc comments' cond
              (truthy', comments''') = expToDoc comments'' truthy
              (falsy', comments'''') = expToDoc comments''' falsy
          in  ( Pretty.group
                (
                  Pretty.pretty "if ("
                  <> Pretty.nest indentSize (Pretty.line' <> cond') <> Pretty.line'
                )
              <> Pretty.group
                  (
                    Pretty.pretty ") {"
                    <> Pretty.nest indentSize (Pretty.line <> truthy') <> Pretty.line <> Pretty.rbrace
                  )
              <> Pretty.group
                  (
                    Pretty.pretty " else {"
                    <> Pretty.nest indentSize (Pretty.line <> falsy') <> Pretty.line <> Pretty.rbrace
                  )
              , comments''''
              )

        Source _ _ (Ternary cond truthy falsy) ->
          let (cond', comments'')    = expToDoc comments' cond
              (truthy', comments''') = expToDoc comments'' truthy
              (falsy', comments'''') = expToDoc comments''' falsy
          in  ( Pretty.group
                (
                  cond' <>
                  Pretty.nest indentSize
                    (
                      Pretty.line
                      <> Pretty.pretty "? " <> truthy'
                      <> Pretty.line <> Pretty.pretty ": " <> falsy'
                    )
                )
              , comments''''
              )

        Source _ _ (Record fields) ->
          let (fields', comments'') = fieldsToDoc comments' fields
          in  ( Pretty.group
                  (
                    Pretty.lbrace
                    <> Pretty.nest indentSize (Pretty.line <> fields')
                    <> Pretty.line
                  )
                <> Pretty.rbrace
              , comments''
              )

        Source _ _ (Dictionary items) ->
          let (fields', comments'') = dictItemsToDoc comments' items
          in  ( Pretty.group
                  (
                    Pretty.pretty "{{"
                    <> Pretty.nest indentSize (Pretty.line <> fields')
                    <> Pretty.line
                  )
                <> Pretty.pretty "}}"
              , comments''
              )

        Source area _ (ListConstructor items) ->
          let (items', comments'')       = listItemsToDoc comments' items
              (commentsDoc, comments''') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments''
          in  ( Pretty.group
                  (
                    Pretty.lbracket
                    <> Pretty.nest indentSize (Pretty.line' <> items')
                    <> commentsDoc
                    <> Pretty.line'
                  )
                <> Pretty.rbracket
              , comments'''
              )

        Source area _ (TupleConstructor items) ->
          let (items', comments')       = argsToDoc comments items
              (commentsDoc, comments'') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments'
          in  ( Pretty.group
                  (
                    Pretty.pretty "#["
                    <> Pretty.nest indentSize (Pretty.line' <> items')
                    <> commentsDoc
                    <> Pretty.line'
                  )
                <> Pretty.pretty "]"
              , comments''
              )

        Source _ _ (Pipe exps) ->
          let (exps', comments'') = argsToDoc comments' exps
          in  ( Pretty.pretty "pipe("
                  <> Pretty.nest indentSize (Pretty.hardline <> exps')
                  <> Pretty.hardline
                  <> Pretty.pretty ")"
              , comments''
              )

        Source _ _ (Where exp iss) ->
          let (exp', comments'')  = expToDoc comments' exp
              (iss', comments''') = issToDoc comments'' iss
              exp'' =
                if shouldNestApp [exp] then
                  Pretty.lparen
                  <> Pretty.nest indentSize (Pretty.line' <> exp')
                  <> Pretty.line'
                else
                  Pretty.lparen <> exp'
          in  ( Pretty.group (Pretty.pretty "where " <> exp'')
                <> Pretty.pretty ") {"
                <> Pretty.nest indentSize (Pretty.line <> iss')
                <> Pretty.line <> Pretty.rbrace
              , comments'''
              )

        Source _ _ (WhereAbs iss) ->
          let (iss', comments'') = issToDoc comments' iss
          in  ( Pretty.pretty "where"
                <> Pretty.pretty " {"
                <> Pretty.nest indentSize (Pretty.line <> iss')
                <> Pretty.line
                <> Pretty.rbrace
              , comments''
              )

        Source _ _ (LNum n) ->
          (Pretty.pretty n, comments')

        Source _ _ (LFloat n) ->
          (Pretty.pretty n, comments')

        Source _ _ (LChar c) ->
          (Pretty.pretty $ '\'':c:'\'':"", comments')

        Source _ _ (LStr s) ->
          (Pretty.pretty s, comments')

        Source _ _ (LBool b) ->
          (Pretty.pretty b, comments')

        Source _ _ LUnit ->
          (Pretty.pretty "{}", comments')

        Source _ _ (Var n) ->
          let n' =
                case n of
                  "unary-minus" ->
                    "-"

                  a ->
                    a
          in (Pretty.pretty n', comments')

        Source _ _ (Parenthesized _ exp _) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (Pretty.lparen <> exp' <> Pretty.rparen, comments'')

        Source _ _ (UnOp operator operand) ->
          let (operator', comments'') = expToDoc comments' operator
              (operand', comments''') = expToDoc comments'' operand
          in  (operator' <> operand', comments''')

        binOp@(Source _ _ BinOp{}) ->
          let (parts, comments'') = binOpToDocs comments' binOp
              first               = head parts
              rest                = tail parts
          in  (Pretty.group (first <> Pretty.nest indentSize (Pretty.hcat rest)), comments'')

        access@(Source _ _ Access{}) ->
          let (parts, comments'') = accessToDocs comments' access
              first               = head parts
              rest                = tail parts
          in  (Pretty.group (first <> Pretty.nest indentSize (Pretty.hcat rest)), comments'')

        Source _ _ (TemplateString exps) ->
          let (content, comments'') = templateStringExpsToDoc comments' exps
          in  (Pretty.pretty "`" <> content <> Pretty.pretty "`", comments'')

        Source _ _ (Export (Source _ _ (Extern typing name name'))) ->
          let (typing', comments'') = typingToDoc comments' typing
          in  (
                Pretty.pretty name <> Pretty.pretty " :: " <> typing'<> Pretty.hardline
                <> Pretty.pretty "export " <> Pretty.pretty name <> Pretty.pretty " = extern \"" <> Pretty.pretty name' <> Pretty.pretty "\""
              , comments''
              )

        Source _ _ (Export exp) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (Pretty.pretty "export " <> exp', comments'')

        Source _ _ (NameExport n) ->
          (Pretty.pretty "export " <> Pretty.pretty n, comments')

        Source _ _ (TypeExport n) ->
          (Pretty.pretty "export type " <> Pretty.pretty n, comments')

        Source _ _ (JSExp js) ->
          let lines' = lines js
              js' = Pretty.pretty js
          in
            if length lines' > 1 then
              (Pretty.pretty "#-" <> js' <> Pretty.pretty "-#", comments')
            else
              (Pretty.group (Pretty.pretty "#-" <> js' <> Pretty.pretty "-#"), comments')
              -- (Pretty.group (Pretty.pretty "#-" <> Pretty.line <> js' <> Pretty.line <> Pretty.pretty "-#"), comments')

        Source _ _ (JsxTag name props children) ->
          let (props', comments'')     = jsxPropsToDoc comments' props
              (children', comments''') = jsxChildrenToDoc comments'' children
              lineAfterName           =
                if null props then
                  Pretty.line'
                else
                  Pretty.line
          in  ( Pretty.group
                  (
                    Pretty.group
                      (
                        Pretty.pretty "<" <> Pretty.pretty name
                        <> Pretty.nest indentSize (lineAfterName <> props')
                        <> Pretty.line'
                        <> Pretty.pretty ">"
                      )
                    <> Pretty.nest indentSize (Pretty.line' <> children')
                    <> Pretty.line'
                    <> Pretty.pretty "</" <> Pretty.pretty name <> Pretty.pretty ">"
                  )
              , comments'''
              )

        Source _ _ (JsxAutoClosedTag name props) ->
          let (props', comments'')     = jsxPropsToDoc comments' props
          in  ( Pretty.group
                  (
                    Pretty.pretty "<" <> Pretty.pretty name
                    <> Pretty.nest indentSize (Pretty.line <> props')
                    <> Pretty.line
                    <> Pretty.pretty "/>"
                  )
              , comments''
              )

        Source _ _ (Extern typing name name') ->
          let (typing', comments'') = typingToDoc comments' typing
          in  (
                Pretty.pretty name <> Pretty.pretty " :: " <> typing'<> Pretty.hardline
                <> Pretty.pretty name <> Pretty.pretty " = extern \"" <> Pretty.pretty name' <> Pretty.pretty "\""
              , comments''
              )

        Source _ _ (TypedExp exp typing) ->
          let (exp', comments'')     = expToDoc comments' exp
              (typing', comments''') = typingToDoc comments'' typing
          in  (Pretty.lparen <> exp' <> Pretty.pretty " :: " <> typing' <> Pretty.rparen, comments''')

        Source _ _ (NamedTypedExp name exp typing) ->
          let (exp', comments'')     = expToDoc comments' exp
              (typing', comments''') = typingToDoc comments'' typing
          in  (Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> Pretty.line' <> exp', comments''')

        Source _ _ (IfTarget sourceTarget) ->
          (Pretty.pretty ("#iftarget " <> targetToString sourceTarget), comments')

        Source _ _ (ElseIfTarget sourceTarget) ->
          (Pretty.pretty ("#elseif " <> targetToString sourceTarget), comments')

        Source _ _ EndIfTarget ->
          (Pretty.pretty "#endif", comments')
  in  (commentsDoc <> exp', comments'')


targetToString :: SourceTarget -> String
targetToString sourceTarget = case sourceTarget of
  TargetLLVM ->
    "llvm"

  TargetJS ->
    "js"

  TargetAll ->
    "all"


importToDoc :: [Comment] -> Import -> (Pretty.Doc ann, [Comment])
importToDoc comments imp = case imp of
  Source _ _ (NamedImport names path _) ->
    let nameDocs = Pretty.pretty . getSourceContent <$> names
        namesDoc = Pretty.vsep (Pretty.punctuate Pretty.comma nameDocs)
        lineDoc  =
          if null names then
            Pretty.emptyDoc
          else
            Pretty.line
    in  ( Pretty.group
            (
              Pretty.pretty "import " <> Pretty.lbrace
              <> Pretty.nest indentSize (lineDoc <> namesDoc)
              <> lineDoc <> Pretty.rbrace
              <> Pretty.pretty " from " <> Pretty.pretty ("\"" ++ path ++ "\"")
            )
        , comments
        )

  Source _ _ (TypeImport names path _) ->
    let nameDocs = Pretty.pretty . getSourceContent <$> names
        namesDoc = Pretty.vsep (Pretty.punctuate Pretty.comma nameDocs)
    in  ( Pretty.group
            (
              Pretty.pretty "import type " <> Pretty.lbrace
              <> Pretty.nest indentSize (Pretty.line <> namesDoc)
              <> Pretty.line <> Pretty.rbrace
              <> Pretty.pretty " from " <> Pretty.pretty ("\"" ++ path ++ "\"")
            )
        , comments
        )

  Source _ _ (DefaultImport name path _) ->
    let nameDoc = (Pretty.pretty . getSourceContent) name
    in  ( Pretty.pretty "import "
          <> nameDoc
          <> Pretty.pretty " from "
          <> Pretty.pretty ("\"" ++ path ++ "\"")
        , comments
        )


constructorsToDoc :: [Comment] -> [Constructor] -> (Pretty.Doc ann, [Comment])
constructorsToDoc comments ctors = case ctors of
  (Source _ _ (Constructor name args) : more) ->
    let name' = Pretty.pretty name
        (args', comments')  = typingListToDoc comments args
        args''              =
          if null args then
            Pretty.emptyDoc
          else
            Pretty.lparen <> args' <> Pretty.rparen
        (more', comments'') = constructorsToDoc comments' more
        end =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.line <> Pretty.pretty "|" <> Pretty.space
    in  (name' <> args'' <> end <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


typeDeclToDoc :: [Comment] -> TypeDecl -> (Pretty.Doc ann, [Comment])
typeDeclToDoc comments td = case td of
  Source _ _ adt@ADT{} ->
    let name   = Pretty.pretty (adtname adt)
        export =
          if adtexported adt then
            Pretty.pretty "export "
          else
            Pretty.emptyDoc
        params = Pretty.hcat (Pretty.punctuate Pretty.space (Pretty.pretty <$> adtparams adt))
        equals =
          if null (adtparams adt) then
            Pretty.line' <> Pretty.pretty "= "
          else
            Pretty.line <> Pretty.pretty "= "
        (constructors, comments') = constructorsToDoc comments (adtconstructors adt)
    in  ( export <> Pretty.pretty "type " <> name <> Pretty.space <> params
          <> Pretty.group (Pretty.nest indentSize (equals <> constructors))
        , comments'
        )

  Source _ _ alias@Alias{} ->
    let name = Pretty.pretty (aliasname alias)
        export =
          if aliasexported alias then
            Pretty.pretty "export "
          else
            Pretty.emptyDoc
        params =
          if null (aliasparams alias) then
            Pretty.emptyDoc
          else
            Pretty.hcat (Pretty.punctuate Pretty.space (Pretty.pretty <$> aliasparams alias)) <> Pretty.space
        (typing, comments') = typingToDoc comments (aliastype alias)
    in  ( export <> Pretty.pretty "alias " <> name <> Pretty.space <> params <> Pretty.pretty "= " <> typing
        , comments'
        )


interfaceToDoc :: [Comment] -> Interface -> (Pretty.Doc ann, [Comment])
interfaceToDoc comments interface = case interface of
  Source _ _ (Interface constraints name vars methods) ->
    let (constraints', comments') = typingListToDoc comments constraints
        constraints''
          | null constraints       = Pretty.emptyDoc
          | length constraints > 1 = Pretty.lparen <> constraints' <> Pretty.rparen <> Pretty.pretty " => "
          | otherwise              = constraints' <> Pretty.pretty " => "

        vars' = Pretty.hcat (Pretty.punctuate Pretty.space (Pretty.pretty <$> vars))
        (methods', comments'') = methodTypingsToDoc comments' (Map.toList methods)
    in  ( Pretty.pretty "interface " <> constraints'' <> Pretty.pretty name <> Pretty.space <> vars' <> Pretty.pretty " {" <> Pretty.nest indentSize (Pretty.line <> methods') <> Pretty.line <> Pretty.rbrace
        ,comments''
        )


methodsToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
methodsToDoc comments methods = case methods of
  (exp : more) ->
    let (exp', comments') = expToDoc comments exp
        (more', comments'')  = methodsToDoc comments' more
        after =
          if null more then
            Pretty.emptyDoc
          else
            emptyLine <> Pretty.line'
    in  (exp' <> after <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


instanceToDoc :: [Comment] -> Instance -> (Pretty.Doc ann, [Comment])
instanceToDoc comments inst = case inst of
  Source _ _ (Instance constraints name typings methods) ->
    let (constraints', _) = typingListToDoc comments constraints
        constraints''
          | null constraints       = Pretty.emptyDoc
          | length constraints > 1 = Pretty.lparen <> constraints' <> Pretty.rparen <> Pretty.pretty " => "
          | otherwise              = constraints' <> Pretty.pretty " => "
        (typings', comments'')  = typingArgsToDoc comments typings
        (methods', comments''') = methodsToDoc comments'' (Map.elems methods)
    in  ( Pretty.pretty "instance " <> constraints'' <> Pretty.pretty name <> Pretty.space <> typings' <> Pretty.pretty " {" <> Pretty.nest indentSize (Pretty.line <> methods') <> Pretty.line <> Pretty.rbrace
        , comments'''
    )


commentToDoc :: Bool -> Comment -> Pretty.Doc ann
commentToDoc topLevel comment = case comment of
  Comment _ c ->
    Pretty.pretty c <> Pretty.hardline

  MultilineComment _ c ->
    let break =
          if topLevel then
            Pretty.line'
          else
            Pretty.softline'
    in Pretty.pretty c <> break


isInlineComment :: Comment -> Bool
isInlineComment comment = case comment of
  Comment _ _ ->
    True

  MultilineComment _ _ ->
    False


insertComments :: Bool -> Area -> [Comment] -> (Pretty.Doc ann, [Comment])
insertComments topLevel area@(Area (Loc _ nodeStartLine _) _) comments = case comments of
  (comment : _) ->
    let commentArea@(Area _ (Loc _ commentEndLine _)) = getCommentArea comment
        after                                         = area `isAfter` commentArea
        afterOrSameLine                               = after || isSameLine area commentArea && isInlineComment comment
    in
      if afterOrSameLine then
        let (next, comments') = insertComments topLevel area (tail comments)
            comment'          = commentToDoc topLevel comment
            comment''         =
              if nodeStartLine - commentEndLine > 1 then
                comment' <> Pretty.line' <> Pretty.line'
              else
                comment'
        in  (comment'' <> next, comments')
      else
        let (next, comments') = insertComments topLevel area (tail comments)
        in  (next, comment : comments')

  [] ->
    (Pretty.emptyDoc, comments)


insertRemainingComments :: [Comment] -> (Pretty.Doc ann, [Comment])
insertRemainingComments comments = case comments of
  (comment : _) ->
    let (next, comments') = insertRemainingComments (tail comments)
        comment'          = commentToDoc True comment
    in  (comment' <> next, comments')

  [] ->
    (Pretty.emptyDoc, comments)


nodesToDocs :: [Comment] -> [Node] -> (Pretty.Doc ann, [Comment])
nodesToDocs comments nodes = case nodes of
  (node : more) ->
    let (commentsDoc, comments') = insertComments True (getNodeArea node) comments
        (node', comments'')      =
          case node of
            ExpNode exp ->
              let (exp', comments'') = expToDoc comments' exp
                  emptyLines =
                    if null more then
                      Pretty.line
                    else
                      Pretty.line <> Pretty.line <> Pretty.line
              in  (exp' <> emptyLines, comments'')

            ImportNode imp ->
              let (imp', comments'') = importToDoc comments' imp
                  emptyLines = case more of
                    (ImportNode _ : _) ->
                      Pretty.line

                    _ ->
                      Pretty.line <> Pretty.line <> Pretty.line <> Pretty.line
              in  (imp' <> emptyLines, comments'')

            TypeDeclNode td ->
              let (td', comments'') = typeDeclToDoc comments' td
                  emptyLines =
                    if null more then
                      Pretty.line
                    else
                      Pretty.line <> Pretty.line <> Pretty.line
              in  (td' <> emptyLines, comments'')

            InterfaceNode interface ->
              let (interface', comments'') = interfaceToDoc comments' interface
                  emptyLines =
                    if null more then
                      Pretty.line
                    else
                      Pretty.line <> Pretty.line <> Pretty.line
              in  (interface' <> emptyLines, comments'')

            InstanceNode inst ->
              let (inst', comments'') = instanceToDoc comments' inst
                  emptyLines =
                    if null more then
                      Pretty.line
                    else
                      Pretty.line <> Pretty.line <> Pretty.line
              in  (inst' <> emptyLines, comments'')
        (more', comments''') = nodesToDocs comments'' more
    in  (commentsDoc <> node' <> more', comments''')

  [] ->
    let (commentsDoc, _) = insertRemainingComments comments
        commentsDoc' =
          if null comments then
            commentsDoc
          else
            Pretty.line <> commentsDoc
    in  (commentsDoc', comments)


astToSource :: Int -> AST -> [Comment] -> String
astToSource width ast comments =
  let nodeList = (sortASTNodes . astToNodeList) ast
      (doc, _) = nodesToDocs comments nodeList
      layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine width 1.0}
  in  renderString (layoutPretty layoutOptions doc)
