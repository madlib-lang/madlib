{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE LambdaCase #-}
module Format.Format where

import           Data.List
import qualified Data.Map                                as Map
import           Data.Text.Prettyprint.Doc               as Pretty
import           Data.Text.Prettyprint.Doc.Render.String as Pretty

import           AST.Source as Src
import           Explain.Location
import           Parse.Comments.Lexer
import qualified Explain.Location as Location
import Utils.Path (getPathType, ModulePath (FileSystemPath, PackagePath), isPreludePath)
import Data.Functor (($>))
import Debug.Trace
import Text.Show.Pretty (ppShow)
import Data.Char (showLitChar)




data Node
  = ImportNode Import
  | ExpNode Exp
  | TypeDeclNode TypeDecl
  | InterfaceNode Interface
  | InstanceNode Instance
  | DerivedDecl Derived
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

  DerivedDecl decl ->
    getArea decl


nodesLineDiff :: [Comment] -> Node -> Node -> Int
nodesLineDiff comments n1 n2 =
  computeLineDiff comments (getNodeArea n1) (getNodeArea n2)


expLineDiff :: [Comment] -> Exp -> Exp -> Int
expLineDiff comments e1 e2 =
  computeLineDiff comments (getArea e1) (getArea e2)


computeLineDiff :: [Comment] -> Area -> Area -> Int
computeLineDiff comments  (Area _ (Loc _ l1 _)) (Area (Loc _ l2 _) _) =
  let l2' = case comments of
        (c : _) ->
          min (Location.getLineFromStart $ getCommentArea c) l2

        _ ->
          l2
  in  l2' - l1


astToNodeList :: AST -> [Node]
astToNodeList ast =
  let imports    = aimports ast
      exps       = aexps ast
      tds        = atypedecls ast
      interfaces = ainterfaces ast
      instances  = ainstances ast
      derived    = aderived ast
  in  (ImportNode <$> imports)
      ++ (ExpNode <$> exps)
      ++ (TypeDeclNode <$> tds)
      ++ (InterfaceNode <$> interfaces)
      ++ (InstanceNode <$> instances)
      ++ (DerivedDecl <$> derived)


sortASTNodes :: [Node] -> [Node]
sortASTNodes = sortOn getNodeArea




indentSize :: Int
indentSize = 2


emptyLine :: Pretty.Doc ann
emptyLine =
  Pretty.nesting (\i -> Pretty.nest (-i) Pretty.line')


trailingCommaOrEmpty :: Pretty.Doc ann
trailingCommaOrEmpty = Pretty.flatAlt (Pretty.comma <> Pretty.hardline) Pretty.emptyDoc

trailingCommaOrSpace :: Pretty.Doc ann
trailingCommaOrSpace = Pretty.flatAlt (Pretty.comma <> Pretty.hardline) Pretty.space


argsToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
argsToDoc comments args = case args of
  [exp] ->
    expToDoc comments exp

  (exp : more) ->
    let (arg, comments')    = expToDoc comments exp
        (more', comments'') = argsToDoc comments' more
    in  (arg <> Pretty.comma <> Pretty.line <> more', comments'')

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
        after =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> Pretty.line
    in  (item <> after <> more', comments'')

  (Source _ _ (ListSpread exp) : more) ->
    let (item, comments')   = expToDoc comments exp
        (more', comments'') = listItemsToDoc comments' more
        after =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> Pretty.line
    in  (Pretty.pretty "..." <> item <> after <> more', comments'')

  [] ->
    (Pretty.emptyDoc, comments)


fieldsToDoc :: [Comment] -> [Field] -> (Pretty.Doc ann, [Comment])
fieldsToDoc comments fields = case fields of
  (Source area _ (Field (name, exp)) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        name'                    = Pretty.pretty name
        (value, _)      = expToDoc comments' exp
        after =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> Pretty.line
        (more', comments''') = fieldsToDoc comments' more
    in  ( commentsDoc <> name' <> Pretty.pretty ": " <> value <> after <> more'
        , comments'''
        )

  (Source area _ (FieldSpread exp) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        (item, comments'')       = expToDoc comments' exp
        after =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> Pretty.line
        (more', comments''') = fieldsToDoc comments'' more
    in  ( commentsDoc <> Pretty.pretty "..." <> item <> after <> more'
        , comments'''
        )

  (Source area _ (FieldShorthand name) : more) ->
    let (commentsDoc, comments') = insertComments False area comments
        item = Pretty.pretty name
        after =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> Pretty.line
        (more', comments'') = fieldsToDoc comments' more
    in  ( commentsDoc <> item <> after <> more'
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
        after =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> Pretty.line
    in  ( key' <> Pretty.pretty ": " <> value' <> after <> more'
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
        exp'                     = commentsDoc <> Pretty.pretty s'
        (more', comments'')      = jsxChildrenToDoc comments' more
    in  (exp' <> (if null more then emptyDoc else Pretty.line') <> more', comments'')

  (JsxChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (exp' <> (if null more then emptyDoc else Pretty.line') <> more', comments'')

  (JsxSpreadChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (Pretty.lbrace <> Pretty.pretty "..." <> exp' <> Pretty.rbrace <> (if null more then emptyDoc else Pretty.line') <> more', comments'')

  (JsxExpChild exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = jsxChildrenToDoc comments' more
    in  (Pretty.lbrace <> exp' <> Pretty.rbrace <> (if null more then emptyDoc else Pretty.line') <> more', comments'')

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
    Pretty.pretty $ renderChar c

  PStr s ->
    Pretty.pretty $ "\"" <> s <> "\""

  PBool b ->
    Pretty.pretty b

  PAny ->
    Pretty.pretty "_"

  PNullaryCon (Source _ _ name) ->
    Pretty.pretty name

  PCon (Source _ _ name) args ->
    let name' = Pretty.pretty name
        args' = Pretty.group (Pretty.nest indentSize (Pretty.line' <> Pretty.hcat (Pretty.punctuate (Pretty.comma <> Pretty.line) (patternToDoc <$> args))) <> trailingCommaOrEmpty)
    in  name' <> Pretty.lparen <> args' <> Pretty.rparen

  PList ps ->
    Pretty.group
      (
        Pretty.pretty "["
        <> Pretty.nest indentSize (Pretty.line' <> Pretty.hcat (Pretty.punctuate (Pretty.pretty "," <> Pretty.line) (patternToDoc <$> ps)))
        <> trailingCommaOrEmpty
        <> Pretty.pretty "]"
      )

  PTuple ps ->
    Pretty.group
      (
        Pretty.pretty "#["
        <> Pretty.nest indentSize (Pretty.line' <> Pretty.hcat (Pretty.punctuate (Pretty.pretty "," <> Pretty.line) (patternToDoc <$> ps)))
        <> trailingCommaOrEmpty
        <> Pretty.pretty "]"
      )

  PRecord fields ->
    Pretty.group
      (
        Pretty.pretty "{"
        <> Pretty.nest indentSize (Pretty.line <> Pretty.hcat (Pretty.punctuate (Pretty.pretty "," <> Pretty.line) (patternFieldToDoc <$> fields)))
        <> trailingCommaOrSpace
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
  [] ->
    (Pretty.emptyDoc, comments)

  [Source _ _ LUnit] ->
    (Pretty.emptyDoc, comments)

  [exp] ->
    expToDoc comments exp

  (exp : next) ->
    let (exp', comments')   = expToDoc comments exp
        (next', comments'') = bodyToDoc comments' next
        breaks              = Pretty.hcat $ replicate (max 1 $ expLineDiff comments' exp (head next)) Pretty.hardline
        breaks' = case next of
          [Source _ _ LUnit] ->
            Pretty.emptyDoc

          _ ->
            breaks
    in  (exp' <> breaks' <> next', comments'')


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
            Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> trailingCommaOrEmpty <> Pretty.rparen)
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
          Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> trailingCommaOrEmpty <> Pretty.rparen)
        else
          Pretty.lparen <> args' <> Pretty.rparen
  in  ( Pretty.group (Pretty.nest indentSize (Pretty.hcat access' <> args''))
      , comments''
      )


typingArgsToDoc :: [Comment] -> [Typing] -> (Pretty.Doc ann, [Comment])
typingArgsToDoc comments typings = case typings of
  (typing : more) ->
    let (typing', comments') = typingToDoc False comments typing
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


typingListToDoc :: Bool -> [Comment] -> [Typing] -> (Pretty.Doc ann, [Comment])
typingListToDoc canBreak comments typings = case typings of
  (typing : more) ->
    let (commentDoc, comments') = insertComments False (getArea typing) comments
        (typing', comments'') = typingToDoc canBreak comments' typing
        (more', comments''')  = typingListToDoc canBreak comments'' more
        comma =
          if null more then
            Pretty.emptyDoc
          else
            Pretty.comma <> (if canBreak then Pretty.line else Pretty.space)
    in  (commentDoc <> typing' <> comma <> more', comments''')

  [] ->
    (Pretty.emptyDoc, comments)


recordFieldTypingsToDoc :: Bool -> [Comment] -> [(Src.Name, Typing)] -> (Pretty.Doc ann, [Comment])
recordFieldTypingsToDoc canBreak comments fields = case fields of
  ((name, typing) : more) ->
    let (commentsDoc, comments') = insertComments False (getArea typing) comments
        (typing', comments'') = typingToDoc canBreak comments' typing
        (more', comments''')  = recordFieldTypingsToDoc canBreak comments'' more
        comma
          | null more = Pretty.emptyDoc
          | not canBreak = Pretty.pretty ", "
          | otherwise = Pretty.comma <> Pretty.line
    in  (commentsDoc <> Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> comma <> more', comments''')

  [] ->
    (Pretty.emptyDoc, comments)

methodTypingsToDoc :: Bool -> [Comment] -> [(Src.Name, Typing)] -> (Pretty.Doc ann, [Comment])
methodTypingsToDoc canBreak comments methods = case methods of
  ((name, typing) : more) ->
    let (commentDoc, comments') = insertComments True (getArea typing) comments
        (typing', comments'') = typingToDoc canBreak comments' typing
        (more', comments''')  = methodTypingsToDoc canBreak comments'' more
        after =
          if null more then
            Pretty.emptyDoc
          else
            emptyLine <> Pretty.line'
    in  (commentDoc <> Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> after <> more', comments''')

  [] ->
    (Pretty.emptyDoc, comments)


flattenTRArrs :: Typing -> [Typing]
flattenTRArrs typing = case typing of
  Source _ _ (TRArr left right) ->
    left : flattenTRArrs right

  _ ->
    [typing]


trArrPartsToDocs :: Bool -> [Comment] -> [Typing] -> ([Pretty.Doc ann], [Comment])
trArrPartsToDocs canBreak comments typings = case typings of
  [] ->
    ([], comments)

  (typing@(Source _ _ TRArr{}) : more) ->
    let (typing', comments') = typingToDoc canBreak comments typing
        (commentDocs, comments'') =
          if null more then
            ([], comments')
          else
            insertCommentsAsDocList False (getArea $ head more) comments'
        (more', comments''')  = trArrPartsToDocs canBreak comments'' more
        sep = if canBreak then Pretty.line else Pretty.space
        sep' = if canBreak then Pretty.hardline else Pretty.space
        commentDoc = Pretty.hcat $ map (sep'<>) commentDocs
        arrow =
          if null more then
            []
          else
            [sep <> Pretty.pretty "-> "]
    in  (Pretty.lparen <> typing' <> Pretty.rparen : commentDoc : arrow ++ more', comments''')

  (typing : more) ->
    let (typing', comments') = typingToDoc canBreak comments typing
        (commentDocs, comments'') =
          if null more then
            ([], comments')
          else
            insertCommentsAsDocList False (getArea $ head more) comments'
        (more', comments''')  = trArrPartsToDocs canBreak comments'' more
        sep = if canBreak then Pretty.line else Pretty.space
        sep' = if canBreak then Pretty.hardline else Pretty.space
        commentDoc = Pretty.hcat $ map (sep'<>) commentDocs
        arrow =
          if null more then
            []
          else
            [sep <> Pretty.pretty "-> "]
    in  (typing' : commentDoc : arrow ++ more', comments''')


typingToDoc :: Bool -> [Comment] -> Typing -> (Pretty.Doc ann, [Comment])
typingToDoc canBreak comments typing = case typing of
  Source _ _ (TRSingle n) ->
    (Pretty.pretty n, comments)

  Source _ _ (TRComp n args) ->
    let (args', comments') = typingArgsToDoc comments args
        space = if null args then Pretty.emptyDoc else Pretty.space
    in  (Pretty.pretty n <> space <> args', comments')

  Source _ _ TRArr{} ->
    let parts = flattenTRArrs typing
        (parts', comments') = trArrPartsToDocs canBreak comments parts
    in  ( Pretty.group $ Pretty.nest indentSize (Pretty.hcat parts')
        , comments'
        )

  Source _ _ (TRTuple typings) ->
    let (typings', comments') = typingListToDoc canBreak comments typings
        sep = if canBreak then Pretty.line' else Pretty.emptyDoc
        sepEnd = if canBreak then trailingCommaOrEmpty else Pretty.emptyDoc
    in  ( Pretty.group
            (
              Pretty.pretty "#["
              <> Pretty.nest indentSize (sep <> typings')
              <> sepEnd
            )
            <> Pretty.pretty "]"
        , comments'
        )

  Source _ _ (TRRecord fields maybeExt) ->
    let (typings', comments')   = recordFieldTypingsToDoc canBreak comments (Map.toList (snd <$> fields))
        sep = if canBreak then Pretty.line else Pretty.space
        afterSpread =
          if Map.null fields && canBreak then
            trailingCommaOrEmpty
          else if Map.null fields then
            Pretty.emptyDoc
          else
            Pretty.comma <> sep
        after =
          if canBreak then
            trailingCommaOrSpace
          else
            sep
        (maybeExt', comments'') = case maybeExt of
          Just ext ->
            let (ext', comments''') = typingToDoc canBreak comments' ext
            in  (Pretty.pretty "..." <> ext' <> afterSpread, comments''')

          Nothing ->
            (Pretty.emptyDoc, comments')
    in  ( Pretty.group
            (
              Pretty.lbrace
              <> Pretty.nest indentSize (sep <> maybeExt' <> typings')
              <> after
            )
            <> Pretty.rbrace
        , comments''
        )

  Source _ _ (TRConstrained constraints typing) ->
    let (constraints', comments') = typingListToDoc canBreak comments constraints
        constraints'' =
          if length constraints > 1 then
            Pretty.group (
              Pretty.lparen
              <> Pretty.nest indentSize (Pretty.line' <> constraints')
              <> Pretty.line'
              <> Pretty.rparen
            )
          else
            constraints'
        (typing', comments'') = typingToDoc canBreak comments' typing
    in  (constraints'' <> Pretty.pretty " => " <> typing', comments'')


templateStringExpsToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
templateStringExpsToDoc comments exps = case exps of
  (Source _ _ (LStr s) : more) ->
    let e' = Pretty.hcat $ Pretty.punctuate Pretty.hardline (map Pretty.pretty $ lines s)
        (more', comments') = templateStringExpsToDoc comments more
    in  (Pretty.nesting (\x -> Pretty.nest (-x) e') <> more', comments')

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

  [] ->
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


-- renderChar :: Char -> String
-- renderChar c = case c of
--   '\a' ->
--     "'\\a'"

--   '\b' ->
--     "'\\b'"

--   '\f' ->
--     "'\\f'"

--   '\n' ->
--     "'\\n'"

--   '\r' ->
--     "'\\r'"

--   '\t' ->
--     "'\\t'"

--   '\v' ->
--     "'\\v'"

--   '\\' ->
--     "'\\\\'"

--   '\'' ->
--     "'\\''"

--   c ->
--     ['\'', c, '\'']

renderChar :: String -> String
renderChar c = "'" ++ c ++ "'"


escapeBackticks :: String -> String
escapeBackticks s = s >>= \case
  '`' ->
    "\\`"

  c ->
    [c]


expToDoc :: [Comment] -> Exp -> (Pretty.Doc ann, [Comment])
expToDoc comments exp =
  let (commentsDoc, comments') = insertComments False (getArea exp) comments
      (exp', comments'') = case exp of
        Source _ _ (App fn@(Source _ _ (Access rec field)) args) ->
          case rec of
            Source _ _ Access{} ->
              accessAsFNToDoc comments' fn args

            Source _ _ (App (Source _ _ Access{}) _) ->
              accessAsFNToDoc comments' fn args

            _ ->
              let (rec', comments'')    = expToDoc comments' rec
                  (field', comments''') = expToDoc comments'' field
                  (args', comments'''') = argsToDoc comments''' args
                  args'' =
                    if shouldNestApp args then
                      Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> trailingCommaOrEmpty <> Pretty.rparen)
                    else
                      Pretty.lparen <> args' <> Pretty.rparen
              in  (rec' <> field' <> args'', comments'''')

        Source _ _ (App fn args) ->
          let (fn', comments'')    = expToDoc comments' fn
              (args', comments''') = argsToDoc comments'' args
              args'' =
                if shouldNestApp args then
                  Pretty.group (Pretty.lparen <> Pretty.nest indentSize (Pretty.line' <> args') <> trailingCommaOrEmpty <> Pretty.rparen)
                else
                  Pretty.lparen <> args' <> Pretty.rparen
          in  ( fn' <> args''
              , comments'''
              )

        Source _ _ (Abs params body) ->
          let (params', comments'') = paramsToDoc comments' params
              (body', comments''')  =
                if null body then
                  (Pretty.emptyDoc, comments'')
                else
                  expToDoc comments'' (head body)
              params''              = formatParams (length params == 1) params'
              arrowAndBody          = case body of
                [Source _ _ BinOp{}] ->
                  Pretty.pretty " =>" <> Pretty.nest indentSize (Pretty.softline <> body')

                [Source _ _ JsxTag{}] ->
                  Pretty.pretty " =>" <> Pretty.nest indentSize (Pretty.group (Pretty.line <> body'))

                _ ->
                  Pretty.pretty " => " <> body'
          in  ( params'' <> arrowAndBody, comments''')

        Source area _ (AbsWithMultilineBody params body) ->
          let (params', comments'') = paramsToDoc comments' params
              (body', comments''')  = bodyToDoc comments'' body
              (commentsAfterBody, comments'''') = insertCommentsAsDocList False (Area (getEndLoc area) (getEndLoc area)) comments'''
              commentsAfterBody' = case body of
                [Source _ _ LUnit] ->
                  hcat $ intersperse Pretty.hardline commentsAfterBody

                _ ->
                  if length comments'''' == length comments''' then
                    Pretty.emptyDoc
                  else
                    Pretty.hardline <> hcat (intersperse Pretty.hardline commentsAfterBody)
              params'' = formatParams (length params <= 1) params'
              doc = case body of
                [Source _ _ LUnit] | length comments'''' == length comments'' ->
                  params''
                  <> Pretty.pretty " => "
                  <> Pretty.lbrace
                  <> Pretty.rbrace

                _ ->
                  params''
                  <> Pretty.pretty " => "
                  <> Pretty.lbrace
                  <> Pretty.nest indentSize (Pretty.line <> body' <> commentsAfterBody')
                  <> Pretty.line
                  <> Pretty.rbrace
          in  ( doc
              , comments''''
              )

        Source area _ (Do exps) ->
          let (exps', comments'') = bodyToDoc comments' exps
              (commentsAfterBody, comments''') = insertCommentsAsDocList False (Area (getEndLoc area) (getEndLoc area)) comments''
              commentsAfterBody' = case exps of
                [Source _ _ LUnit] ->
                  hcat $ intersperse Pretty.hardline commentsAfterBody

                _ ->
                  if length comments''' == length comments'' then
                    Pretty.emptyDoc
                  else
                    Pretty.hardline <> hcat (intersperse Pretty.hardline commentsAfterBody)
              doc = case exps of
                [Source _ _ LUnit] | length comments''' == length comments'' ->
                  Pretty.pretty "do "
                  <> Pretty.lbrace
                  <> Pretty.rbrace

                _ ->
                    Pretty.pretty "do "
                  <> Pretty.lbrace
                  <> Pretty.nest indentSize (Pretty.hardline <> exps' <> commentsAfterBody')
                  <> Pretty.hardline
                  <> Pretty.rbrace
          in  ( doc
              , comments'''
              )

        Source _ _ (Return exp) ->
          let (exp', comments'') = expToDoc comments' exp
          in  (Pretty.pretty "return " <> exp', comments'')

        Source _ _ (Assignment name exp) ->
          let name'              = Pretty.pretty name
              (exp', comments'') = expToDoc comments' exp
          in  (name' <> Pretty.pretty " = " <> exp', comments'')

        Source _ _ (Mutate lhs exp) ->
          let (lhs', comments'') = expToDoc comments' lhs
              (exp', comments''') = expToDoc comments'' exp
          in  (lhs' <> Pretty.pretty " := " <> exp', comments''')

        Source _ _ (DoAssignment name exp) ->
          let name'              = Pretty.pretty name
              (exp', comments'') = expToDoc comments' exp
          in  (name' <> Pretty.pretty " <- " <> exp', comments'')

        Source _ _ (While cond body) ->
          let (cond', comments'')    = expToDoc comments' cond
              (body', comments''') = expToDoc comments'' body
              isWhileDo = case body of
                Source _ _ (Do _) ->
                  True

                _ ->
                  False
          in  ( Pretty.group
                (
                  Pretty.pretty "while("
                  <> Pretty.nest indentSize (Pretty.line' <> cond') <> Pretty.line'
                )
              <> Pretty.group
                  (
                    if isWhileDo then
                      Pretty.rparen <> Pretty.space <> body'
                    else
                      Pretty.pretty ") {"
                      <> Pretty.nest indentSize (Pretty.hardline <> body') <> Pretty.hardline <> Pretty.rbrace
                  )
              , comments'''
              )

        Source _ _ (If cond truthy falsy) ->
          let (cond', comments'')    = expToDoc comments' cond
              (truthy', comments''') = expToDoc comments'' truthy
              (falsy', comments'''') = expToDoc comments''' falsy
              showElse = case falsy of
                Source _ _ LUnit ->
                  False

                _ ->
                  True
              isIfDo = case truthy of
                Source _ _ (Do _) ->
                  True

                _ ->
                  False
              isElseDo = case falsy of
                Source _ _ (Do _) ->
                  True

                _ ->
                  False

              isElseIf = case falsy of
                Source _ _ (If _ _ _) ->
                  True

                _ ->
                  False
          in  ( Pretty.group
                (
                  Pretty.pretty "if ("
                  <> Pretty.nest indentSize (Pretty.line' <> cond') <> Pretty.line'
                )
              <> Pretty.group
                  (
                    if isIfDo then
                      Pretty.rparen <> Pretty.space <> truthy'
                    else
                      Pretty.pretty ") {"
                      <> Pretty.nest indentSize (Pretty.hardline <> truthy') <> Pretty.hardline <> Pretty.rbrace
                  )
              <>  (
                    if isElseDo || isElseIf then
                      Pretty.group (Pretty.pretty " else "  <> falsy')
                    else if showElse then
                      Pretty.group
                        (
                          Pretty.pretty " else {"
                          <> Pretty.nest indentSize (Pretty.hardline <> falsy') <> Pretty.hardline <> Pretty.rbrace
                        )
                    else
                      Pretty.emptyDoc
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
                    <> trailingCommaOrSpace
                  )
                <> Pretty.rbrace
              , comments''
              )

        Source _ _ (Dictionary items) ->
          let (fields', comments'') = dictItemsToDoc comments' items
          in  if null items then
                (Pretty.pretty "{{}}", comments'')
              else
                ( Pretty.group
                    (
                      Pretty.pretty "{{"
                      <> Pretty.nest indentSize (Pretty.line <> fields')
                      <> trailingCommaOrSpace
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
                    <> trailingCommaOrEmpty
                    <> commentsDoc
                  )
                <> Pretty.rbracket
              , comments'''
              )

        Source area _ (TupleConstructor items) ->
          let (items', comments'')     = argsToDoc comments' items
              (commentsDoc, comments''') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments''
          in  ( Pretty.group
                  (
                    Pretty.pretty "#["
                    <> Pretty.nest indentSize (Pretty.line' <> items')
                    <> trailingCommaOrEmpty
                    <> commentsDoc
                  )
                <> Pretty.pretty "]"
              , comments'''
              )

        Source area _ (ArrayAccess arr index) ->
          let (arr', comments')       = expToDoc comments arr
              (index', comments'')       = expToDoc comments' index
              (commentsDoc, comments''') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments''
          in  ( Pretty.group
                  (
                    arr' <> Pretty.pretty "["
                    <> Pretty.line'
                    <> index'
                    <> Pretty.line'
                    <> commentsDoc
                  )
                <> Pretty.pretty "]"
              , comments'''
              )

        Source area _ (Pipe exps) ->
          let (exps', comments'') = argsToDoc comments' exps
              (commentsDoc, comments''') = insertComments False (Area (getEndLoc area) (getEndLoc area)) comments''
              commentsDoc' =
                if length comments'' == length comments''' || null comments'' then
                  mempty
                else
                  hcat (replicate (computeLineDiff [] (getArea $ last exps) (getCommentArea $ head comments'')) Pretty.line') <> commentsDoc
          in  ( Pretty.pretty "pipe("
                  <> Pretty.nest indentSize (Pretty.hardline <> exps' <> Pretty.comma <> commentsDoc')
                  <> Pretty.hardline
                  <> Pretty.rparen
              , comments'''
              )

        Source _ _ (Where exp iss) ->
          let (exp', comments'')  = expToDoc comments' exp
              (iss', comments''') = issToDoc comments'' iss
              lineBreakForIndentation =
                if length iss > 1 then
                  -- if there's more than one branch we force multiline layout
                  Pretty.hardline
                else
                  Pretty.line
              exp'' =
                if shouldNestApp [exp] then
                  Pretty.lparen
                  <> Pretty.nest indentSize (Pretty.line' <> exp')
                  <> Pretty.line'
                else
                  Pretty.lparen <> exp'
          in  ( Pretty.group (Pretty.pretty "where" <> exp'')
                <> Pretty.pretty ") {"
                <> Pretty.nest indentSize (lineBreakForIndentation <> iss')
                <> Pretty.line <> Pretty.rbrace
              , comments'''
              )

        Source _ _ (WhereAbs iss) ->
          let (iss', comments'') = issToDoc comments' iss
              lineBreakForIndentation =
                if length iss > 1 then
                  -- if there's more than one branch we force multiline layout
                  Pretty.hardline
                else
                  Pretty.line
          in  ( Pretty.pretty "where"
                <> Pretty.pretty " {"
                <> Pretty.nest indentSize (lineBreakForIndentation <> iss')
                <> Pretty.line
                <> Pretty.rbrace
              , comments''
              )

        Source _ _ (LNum n) ->
          (Pretty.pretty n, comments')

        Source _ _ (LByte n) ->
          (Pretty.pretty n <> Pretty.pretty "_b", comments')

        Source _ _ (LShort n) ->
          (Pretty.pretty n <> Pretty.pretty "_s", comments')

        Source _ _ (LInt n) ->
          (Pretty.pretty n <> Pretty.pretty "_i", comments')

        Source _ _ (LFloat n) ->
          (Pretty.pretty n, comments')

        Source _ _ (LChar c) ->
          (Pretty.pretty $ renderChar c, comments')

        Source _ _ (LStr s) ->
          let s' = "\"" <> s <> "\""
          in (Pretty.pretty s', comments')

        Source _ _ (LBool b) ->
          (Pretty.pretty b, comments')

        Source _ _ LUnit ->
          (Pretty.pretty "{}", comments')

        Source _ _ TypedHole ->
          (Pretty.pretty "???", comments')

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
          in  (Pretty.group (Pretty.nest indentSize (Pretty.lparen <> line' <> exp') <> Pretty.line' <> Pretty.rparen), comments'')

        Source _ _ (UnOp operator operand) ->
          let (operator', comments'') = expToDoc comments' operator
              (operand', comments''') = expToDoc comments'' operand
          in  (operator' <> operand', comments''')

        binOp@(Source _ _ BinOp{}) ->
          let (parts, comments'') = binOpToDocs comments' binOp
              first               = head parts
              rest                = tail parts
          in  (Pretty.group (first <> Pretty.nest indentSize (Pretty.hcat rest)), comments'')

        access@(Source _ _ (Access rec field)) ->
          case rec of
            Source _ _ Access{} ->
              let (parts, comments'') = accessToDocs comments' access
                  first               = head parts
                  rest                = tail parts
              in  (Pretty.group (first <> Pretty.nest indentSize (Pretty.hcat rest)), comments'')

            Source _ _ (App (Source _ _ Access{}) _) ->
              let (parts, comments'') = accessToDocs comments' access
                  first               = head parts
                  rest                = tail parts
              in  (Pretty.group (first <> Pretty.nest indentSize (Pretty.hcat rest)), comments'')

            _ ->
              let (rec', comments'')    = expToDoc comments' rec
                  (field', comments''') = expToDoc comments'' field
              in  (rec' <> field', comments''')

        Source _ _ (TemplateString exps) ->
          let (content, comments'') = templateStringExpsToDoc comments' exps
          in  (Pretty.pretty "`" <> content <> Pretty.pretty "`", comments'')

        Source _ _ (Export (Source _ _ (Extern typing name name'))) ->
          let (typing', comments'') = typingToDoc True comments' typing
          in  (
                Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> Pretty.hardline
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
              -- TODO: this logic is still not correct and thus commented for now.
              -- The issue is that if the fence starts with #- {\n  ...,
              -- the space between #- and { will create an extra indentation level
              -- and this will happen every time it is formatted, thus shifting the whole
              -- fenced code to the right each application.
              -- leadingSpaces = length $ takeWhile (== ' ') js
              -- js' = Pretty.nesting $ \n -> Pretty.nest (leadingSpaces - n) $ Pretty.pretty js

              js' = Pretty.nesting $ \x -> Pretty.nest (-x) $ Pretty.pretty js
          in
            if length lines' > 1 then
              -- (Pretty.pretty "#-" <> Pretty.pretty js <> Pretty.pretty "-#", comments')
              (Pretty.pretty "#-" <> js' <> Pretty.pretty "-#", comments')
            else
              -- (Pretty.group (Pretty.pretty "#-" <> Pretty.pretty js <> Pretty.pretty "-#"), comments')
              (Pretty.group (Pretty.pretty "#-" <> js' <> Pretty.pretty "-#"), comments')

        Source _ _ (JsxTag name props children) ->
          let (props', comments'')     = jsxPropsToDoc comments' props
              (children', comments''') = jsxChildrenToDoc comments'' children
              lineAfterName            =
                if null props then
                  Pretty.emptyDoc
                else
                  Pretty.line
              lineBeforeRightChevron   =
                if null props then
                  Pretty.emptyDoc
                else
                  Pretty.line'

          in  ( Pretty.group
                  (
                    Pretty.pretty "<" <> Pretty.pretty name
                    <> Pretty.nest indentSize (lineAfterName <> props')
                    <> lineBeforeRightChevron
                    <> Pretty.pretty ">"
                  )
                <>  (
                      if null children then
                        Pretty.emptyDoc
                      else
                        Pretty.nest indentSize (Pretty.line' <> children')
                    )
                <> Pretty.line'
                <> Pretty.pretty "</" <> Pretty.pretty name <> Pretty.pretty ">"
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
          let (typing', comments'') = typingToDoc True comments' typing
          in  (
                Pretty.pretty name <> Pretty.pretty " :: " <> typing'<> Pretty.hardline
                <> Pretty.pretty name <> Pretty.pretty " = extern \"" <> Pretty.pretty name' <> Pretty.pretty "\""
              , comments''
              )

        Source _ _ (TypedExp exp typing) ->
          let (exp', comments'')     = expToDoc comments' exp
              (typing', comments''') = typingToDoc True comments'' typing
          in  (Pretty.lparen <> exp' <> Pretty.pretty " :: " <> typing' <> Pretty.rparen, comments''')

        Source _ _ (NamedTypedExp name exp typing) ->
          let (exp', comments'')     = expToDoc comments' exp
              (typing', comments''') = typingToDoc True comments'' typing
          in  (Pretty.pretty name <> Pretty.pretty " :: " <> typing' <> Pretty.line' <> exp', comments''')

        Source _ _ (IfTarget sourceTarget) ->
          (Pretty.pretty ("#iftarget " <> targetToString sourceTarget), comments')

        Source _ _ (ElseIfTarget sourceTarget) ->
          (Pretty.pretty ("#elseif " <> targetToString sourceTarget), comments')

        Source _ _ EndIfTarget ->
          (Pretty.pretty "#endif", comments')

  in  (commentsDoc <> exp', comments'')


sanitizeConstructorAccessTypeName :: String -> String
sanitizeConstructorAccessTypeName typeName = case typeName of
  "(,)" ->
    "#[,]"

  "(,,)" ->
    "#[,,]"

  "(,,,)" ->
    "#[,,,]"

  "(,,,,)" ->
    "#[,,,,]"

  "(,,,,,)" ->
    "#[,,,,,]"

  "(,,,,,,)" ->
    "#[,,,,,,]"

  "(,,,,,,,)" ->
    "#[,,,,,,,]"

  "(,,,,,,,,)" ->
    "#[,,,,,,,,]"

  "(,,,,,,,,,)" ->
    "#[,,,,,,,,,]"

  or ->
    or


targetToString :: SourceTarget -> String
targetToString sourceTarget = case sourceTarget of
  TargetLLVM ->
    "llvm"

  TargetJS ->
    "js"

  TargetAll ->
    "all"



derivedToDoc :: [Comment] -> Derived -> (Pretty.Doc ann, [Comment])
derivedToDoc comments derived = case derived of
  (Source area _ (DerivedADT interfaceName adtName)) ->
    let (commentDoc, comments') = insertComments False area comments
    in  (commentDoc <> Pretty.pretty "derive " <> Pretty.pretty interfaceName <> Pretty.space <> Pretty.pretty adtName, comments')

  (Source area _ (DerivedRecord interfaceName fields)) ->
    let (commentDoc, comments') = insertComments False area comments
    in  (commentDoc <> Pretty.pretty "derive " <> Pretty.pretty interfaceName <> Pretty.space <> Pretty.encloseSep Pretty.lbrace Pretty.rbrace (Pretty.pretty ", ") (map Pretty.pretty fields), comments')


importNamesToDoc :: [Comment] -> [Source Name] -> ([Pretty.Doc ann], [Comment])
importNamesToDoc comments names = case names of
  (Source area _ name : more) ->
    let (commentDoc, comments') = insertComments False area comments
        nameDoc                 = Pretty.pretty name
        (more', comments'')     = importNamesToDoc comments' more
    in  (commentDoc <> nameDoc : more', comments'')

  [] ->
    ([], comments)


importToDoc :: [Comment] -> Import -> (Pretty.Doc ann, [Comment])
importToDoc comments imp = case imp of
  Source area _ (NamedImport names path _) ->
    let (commentDoc, comments') = insertComments False area comments
        (nameDocs, comments'')  = importNamesToDoc comments' (sortBy (\a b -> compare (getSourceContent a) (getSourceContent b)) names)
        namesDoc = Pretty.vsep (Pretty.punctuate Pretty.comma nameDocs)
        lineDoc  =
          if null names then
            Pretty.emptyDoc
          else
            Pretty.line
        trailing =
          if null names then
            Pretty.emptyDoc
          else
            trailingCommaOrSpace
    in  ( Pretty.group
            (
              commentDoc
              <> Pretty.pretty "import " <> Pretty.lbrace
              <> Pretty.nest indentSize (lineDoc <> namesDoc)
              <> trailing <> Pretty.rbrace
              <> Pretty.pretty " from " <> Pretty.pretty ("\"" ++ path ++ "\"")
            )
        , comments''
        )

  Source area _ (TypeImport names path _) ->
    let (commentDoc, comments') = insertComments False area comments
        (nameDocs, comments'')  = importNamesToDoc comments' (sortBy (\a b -> compare (getSourceContent a) (getSourceContent b)) names)
        namesDoc                = Pretty.vsep (Pretty.punctuate Pretty.comma nameDocs)
        lineDoc  =
          if null names then
            Pretty.emptyDoc
          else
            Pretty.line
        trailing =
          if null names then
            Pretty.emptyDoc
          else
            trailingCommaOrSpace
    in  ( Pretty.group
            (
              commentDoc
              <> Pretty.pretty "import type " <> Pretty.lbrace
              <> Pretty.nest indentSize (lineDoc <> namesDoc)
              <> trailing <> Pretty.rbrace
              <> Pretty.pretty " from " <> Pretty.pretty ("\"" ++ path ++ "\"")
            )
        , comments''
        )

  Source area _ (DefaultImport name path _) ->
    let nameDoc = (Pretty.pretty . getSourceContent) name
        (commentDoc, comments')         = insertComments False area comments
        (commentDocForName, comments'') = insertComments False (getArea name) comments'
    in  ( commentDoc
          <> Pretty.pretty "import "
          <> (commentDocForName <> nameDoc)
          <> Pretty.pretty " from "
          <> Pretty.pretty ("\"" ++ path ++ "\"")
        , comments''
        )


constructorsToDoc :: [Comment] -> [(Constructor, Doc ann)] -> (Pretty.Doc ann, [Comment])
constructorsToDoc comments ctors = case ctors of
  ((Source area _ (Constructor name args), separator) : more) ->
    let name' = Pretty.pretty name
        (commentsDoc, comments') = insertCommentsAsDocList False area comments
        (args', comments'')  = typingListToDoc True comments' args
        args''              =
          if null args then
            Pretty.emptyDoc
          else if length args == 1 then
            Pretty.lparen <> args' <> Pretty.rparen
          else
            Pretty.group
              (
                Pretty.lparen
                <> Pretty.nest indentSize (Pretty.line' <> args')
                <> trailingCommaOrEmpty
              )
              <> Pretty.rparen
        (more', comments''') = constructorsToDoc comments'' more
        possibleNewLineBeforeComment =
          if length comments' /= length comments then
            Pretty.hardline
          else
            Pretty.emptyDoc
    in  (possibleNewLineBeforeComment <> Pretty.hcat (intersperse Pretty.hardline commentsDoc) <> separator <> name' <> args'' <> more', comments''')
    -- in  (possibleNewLineBeforeComment <> Pretty.hcat commentsDoc <> separator <> name' <> args'' <> more', comments''')

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
        equals = Pretty.line <> Pretty.pretty "= "
        space =
          if null (adtparams adt) then
            Pretty.emptyDoc
          else
            Pretty.space
        prefixOperators = equals : ([0..] $> Pretty.line <> Pretty.pretty "| ")
        (constructors, comments') = constructorsToDoc comments (zip (adtconstructors adt) prefixOperators)
    in  ( export <> Pretty.pretty "type " <> name <> space <> params
          <> Pretty.group (Pretty.nest indentSize constructors)
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
        (typing, comments') = typingToDoc True comments (aliastype alias)
    in  ( export <> Pretty.pretty "alias " <> name <> Pretty.space <> params <> Pretty.pretty "= " <> typing
        , comments'
        )


interfaceToDoc :: [Comment] -> Interface -> (Pretty.Doc ann, [Comment])
interfaceToDoc comments interface = case interface of
  Source _ _ (Interface constraints name vars methods) ->
    let (constraints', comments') = typingListToDoc True comments constraints
        constraints''
          | null constraints       = Pretty.emptyDoc
          | length constraints > 1 = Pretty.lparen <> constraints' <> Pretty.rparen <> Pretty.pretty " => "
          | otherwise              = constraints' <> Pretty.pretty " => "

        vars' = Pretty.hcat (Pretty.punctuate Pretty.space (Pretty.pretty <$> vars))
        (methods', comments'') = methodTypingsToDoc True comments' (Map.toList methods)
    in  ( Pretty.pretty "interface " <> constraints'' <> Pretty.pretty name <> Pretty.space <> vars' <> Pretty.pretty " {" <> Pretty.nest indentSize (Pretty.line <> methods') <> Pretty.line <> Pretty.rbrace
        ,comments''
        )


methodsToDoc :: [Comment] -> [Exp] -> (Pretty.Doc ann, [Comment])
methodsToDoc comments methods = case methods of
  (exp : more) ->
    let (exp', comments')   = expToDoc comments exp
        (more', comments'') = methodsToDoc comments' more
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
    let (constraints', _) = typingListToDoc True comments constraints
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
            Pretty.hardline
          else
            Pretty.line
    in Pretty.pretty c <> break


commentToDocWithoutBreak :: Comment -> Pretty.Doc ann
commentToDocWithoutBreak comment = case comment of
  Comment _ c ->
    Pretty.pretty c

  MultilineComment _ c ->
    Pretty.pretty c


isInlineComment :: Comment -> Bool
isInlineComment comment = case comment of
  Comment _ _ ->
    True

  MultilineComment _ _ ->
    False


insertComments :: Bool -> Area -> [Comment] -> (Pretty.Doc ann, [Comment])
insertComments topLevel area comments = case comments of
  (comment : _) ->
    let commentArea = getCommentArea comment
        after = area `isAfter` commentArea
        afterOrSameLine = after || isSameLine area commentArea && isInlineComment comment
    in
      if afterOrSameLine then
        let (next, comments') = insertComments topLevel area (tail comments)
            comment'          = commentToDoc topLevel comment
            comment''         = comment' <> hcat (replicate (computeLineDiff (tail comments) commentArea area - 1) Pretty.line')
        in  (comment'' <> next, comments')
      else
        let (next, comments') = insertComments topLevel area (tail comments)
        in  (next, comment : comments')

  [] ->
    (Pretty.emptyDoc, comments)


insertCommentsAsDocList :: Bool -> Area -> [Comment] -> ([Pretty.Doc ann], [Comment])
insertCommentsAsDocList topLevel area comments = case comments of
  (comment : _) ->
    let commentArea = getCommentArea comment
        after                                         = area `startsBeforeEnd` commentArea
        afterOrSameLine                               = after || isSameLine area commentArea && isInlineComment comment
    in
      if afterOrSameLine then
        let (next, comments') = insertCommentsAsDocList topLevel area (tail comments)
            comment'          = commentToDocWithoutBreak comment
        in  (comment' : next, comments')
      else
        let (next, comments') = insertCommentsAsDocList topLevel area (tail comments)
        in  (next, comment : comments')

  [] ->
    ([], comments)


insertRemainingComments :: [Comment] -> (Pretty.Doc ann, [Comment])
insertRemainingComments comments = case comments of
  (comment : _) ->
    let (next, comments') = insertRemainingComments (tail comments)
        comment'          = commentToDoc True comment
    in  (comment' <> next, comments')

  [] ->
    (Pretty.emptyDoc, comments)



data ImportType
  = APreludeTypeImport
  | BPackageTypeImport
  | CLocalTypeImport
  | DPreludeImport
  | EPackageImport
  | FLocalImport
  deriving(Eq, Ord)

gatherImportNodes :: [Node] -> ([(Import, (ImportType, FilePath))], [Node])
gatherImportNodes nodes = gatherImportNodes' nodes []


gatherImportNodes' :: [Node] -> [(Import, (ImportType, FilePath))] -> ([(Import, (ImportType, FilePath))], [Node])
gatherImportNodes' nodes acc = case nodes of
  (ImportNode imp : next) ->
    let importPath         = snd $ getImportPath imp
        absoluteImportPath = getImportAbsolutePath imp
        pathType           = getPathType importPath
    in  case pathType of
          FileSystemPath | isTypeImport imp ->
            gatherImportNodes' next (acc ++ [(imp, (CLocalTypeImport, importPath))])

          FileSystemPath ->
            gatherImportNodes' next (acc ++ [(imp, (FLocalImport, importPath))])

          PackagePath | isPreludePath absoluteImportPath && isTypeImport imp ->
            gatherImportNodes' next (acc ++ [(imp, (APreludeTypeImport, importPath))])

          PackagePath | isPreludePath absoluteImportPath ->
            gatherImportNodes' next (acc ++ [(imp, (DPreludeImport, importPath))])

          _ | isTypeImport imp ->
            gatherImportNodes' next (acc ++ [(imp, (BPackageTypeImport, importPath))])

          _ ->
            gatherImportNodes' next (acc ++ [(imp, (EPackageImport, importPath))])

  _ ->
    (acc, nodes)


renderImports :: [Comment] -> [Import] -> ([Pretty.Doc ann], [Comment])
renderImports comments imports = case imports of
  (imp : more) ->
    let (imp', comments')   = importToDoc comments imp
        (more', comments'') = renderImports comments' more
    in  (imp' <> Pretty.hardline : more', comments'')

  [] ->
    ([], comments)


nodesToDocs :: [Comment] -> [Node] -> (Pretty.Doc ann, [Comment])
nodesToDocs comments nodes = case nodes of
  (node : more) ->
    let (commentsDoc, comments') = insertComments True (getNodeArea node) comments
        (node', comments'', newMore) =
          case node of
            ExpNode exp ->
              let (exp', comments'') = expToDoc comments' exp
                  emptyLinesToAdd    =
                    if null more then
                      Pretty.hardline
                    else
                      Pretty.hcat $ replicate (max 1 $ nodesLineDiff comments'' node (head more)) Pretty.hardline
              in  (exp' <> emptyLinesToAdd, comments'', more)

            ImportNode _ ->
              let (importData, more')    = gatherImportNodes nodes
                  (imports, importInfo)  = unzip importData
                  (imports', comments'') = renderImports comments' imports
                  sorter = \(_, ordA) (_, ordB) -> compare ordA ordB
                  grouper = \(_, (importTypeA, _)) (_, (importTypeB, _)) -> importTypeA == importTypeB
                  sorted = intercalate [Pretty.hardline] $ map (fst <$>) $ groupBy grouper $ sortBy sorter (zip imports' importInfo)
              in  (Pretty.hcat sorted <> Pretty.hardline <> Pretty.hardline <> Pretty.hardline, comments'', more')

            DerivedDecl decl ->
              let (exp', comments'') = derivedToDoc comments' decl
                  emptyLinesToAdd    =
                    if null more then
                      Pretty.hardline
                    else
                      Pretty.hcat $ replicate (max 1 $ nodesLineDiff comments'' node (head more)) Pretty.hardline
              in  (exp' <> emptyLinesToAdd, comments'', more)

            TypeDeclNode td ->
              let (td', comments'') = typeDeclToDoc comments' td
                  emptyLinesToAdd   =
                    if null more then
                      Pretty.hardline
                    else
                      Pretty.hcat $ replicate (max 1 $ nodesLineDiff comments'' node (head more)) Pretty.hardline
              in  (td' <> emptyLinesToAdd, comments'', more)

            InterfaceNode interface ->
              let (interface', comments'') = interfaceToDoc comments' interface
                  emptyLinesToAdd          =
                    if null more then
                      Pretty.hardline
                    else
                      Pretty.hcat $ replicate (max 1 $ nodesLineDiff comments'' node (head more)) Pretty.hardline
              in  (interface' <> emptyLinesToAdd, comments'', more)

            InstanceNode inst ->
              let (inst', comments'') = instanceToDoc comments' inst
                  emptyLinesToAdd     =
                    if null more then
                      Pretty.hardline
                    else
                      Pretty.hcat $ replicate (max 1 $ nodesLineDiff comments'' node (head more)) Pretty.hardline
              in  (inst' <> emptyLinesToAdd, comments'', more)
        (more', comments''') = nodesToDocs comments'' newMore
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
