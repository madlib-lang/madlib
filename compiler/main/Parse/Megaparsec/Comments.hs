{-# LANGUAGE OverloadedStrings #-}
module Parse.Megaparsec.Comments
  ( Comment(..)
  , getCommentArea
  , parseComments
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import           Data.Void
import           Control.Monad                  ( void )

import           Text.Megaparsec
import qualified Text.Megaparsec.Char           as C

import           Explain.Location


-- | A comment extracted from source code
data Comment
  = Comment Area String           -- Single-line comment
  | MultilineComment Area String  -- Multi-line block comment
  deriving (Eq, Show)


-- | Get the area of a comment
getCommentArea :: Comment -> Area
getCommentArea (Comment area _) = area
getCommentArea (MultilineComment area _) = area


-- | Simple parser type for comment extraction (no custom state needed)
type CommentParser = Parsec Void Text


-- | Parse all comments from a source string
parseComments :: String -> Either String [Comment]
parseComments str = case runParser pComments "<input>" (T.pack str) of
  Right comments -> Right comments
  Left err       -> Left (errorBundlePretty err)


-- | Top-level parser: extract all comments, skipping non-comment content
pComments :: CommentParser [Comment]
pComments = do
  comments <- many $ choice
    [ Just <$> try pMultilineComment
    , Just <$> try pSingleLineComment
    , Nothing <$ try pStringLiteral  -- skip string literals
    , Nothing <$ try pCharLiteral    -- skip char literals
    , Nothing <$ try pJSBlock        -- skip JS blocks
    , Nothing <$ try pTemplateString -- skip template strings
    , Nothing <$ anySingle           -- skip any other character
    ]
  eof
  return [c | Just c <- comments]


-- | Parse a single-line comment: // ...
pSingleLineComment :: CommentParser Comment
pSingleLineComment = do
  offset <- getOffset
  pos <- getSourcePos
  let startLoc = Loc offset (unPos $ sourceLine pos) (unPos $ sourceColumn pos)
  content <- C.string "//" *> many (anySingleBut '\n')
  let fullContent = "//" ++ content
  offset' <- getOffset
  pos' <- getSourcePos
  let endLoc = Loc offset' (unPos $ sourceLine pos') (unPos $ sourceColumn pos')
  return $ Comment (Area startLoc endLoc) fullContent


-- | Parse a multiline comment: /* ... */ (with nesting support)
pMultilineComment :: CommentParser Comment
pMultilineComment = do
  offset <- getOffset
  pos <- getSourcePos
  let startLoc = Loc offset (unPos $ sourceLine pos) (unPos $ sourceColumn pos)
  content <- pBlockComment
  offset' <- getOffset
  pos' <- getSourcePos
  let endLoc = Loc offset' (unPos $ sourceLine pos') (unPos $ sourceColumn pos')
  return $ MultilineComment (Area startLoc endLoc) content


-- | Parse a (possibly nested) block comment, returning its full text including delimiters
pBlockComment :: CommentParser String
pBlockComment = do
  void $ C.string "/*"
  content <- manyTill pBlockCommentContent (C.string "*/")
  return $ "/*" ++ concat content ++ "*/"
  where
    pBlockCommentContent = choice
      [ -- Nested block comment
        pBlockComment
      , -- Any single character
        (:[]) <$> anySingle
      ]


-- | Skip a double-quoted string literal to avoid finding "comments" inside strings
pStringLiteral :: CommentParser ()
pStringLiteral = do
  void $ C.char '"'
  void $ manyTill (try (C.string "\\\"" >> return '"') <|> anySingle) (C.char '"')


-- | Skip a single-quoted character literal
pCharLiteral :: CommentParser ()
pCharLiteral = do
  void $ C.char '\''
  void $ manyTill (try (C.string "\\'" >> return '\'') <|> anySingle) (C.char '\'')


-- | Skip a JS block #- ... -#
pJSBlock :: CommentParser ()
pJSBlock = do
  void $ C.string "#-"
  void $ manyTill anySingle (C.string "-#")


-- | Skip a template string (backtick-delimited)
pTemplateString :: CommentParser ()
pTemplateString = do
  void $ C.char '`'
  void $ manyTill (try (C.string "\\`" >> return '`') <|> anySingle) (C.char '`')
