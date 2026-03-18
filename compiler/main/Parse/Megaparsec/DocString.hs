{-# LANGUAGE OverloadedStrings #-}
module Parse.Megaparsec.DocString
  ( parse
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import           Data.Char                      ( isSpace )
import           Data.List                      ( isSuffixOf, foldl' )
import           Data.Void
import           Control.Monad                  ( void )

import           Text.Megaparsec                hiding ( State, match, parse )
import qualified Text.Megaparsec.Char           as C

import           AST.Source                     ( SourceTarget(..) )
import           Parse.DocString.DocString
import           Text.Regex.TDFA


-- | Simple parser type for doc string extraction
type DocParser = Parsec Void Text


-- | Parse doc strings from a source string
parse :: String -> Either String [DocString]
parse s = case runParser pDocStrings "<input>" (T.pack s) of
  Right docs -> Right docs
  Left err   -> Left (errorBundlePretty err)


-- | Top-level parser: extract all doc strings
pDocStrings :: DocParser [DocString]
pDocStrings = do
  docs <- many $ choice
    [ Just <$> try pDocString
    , Nothing <$ try pMacroIfTarget
    , Nothing <$ try pMacroElseIf
    , Nothing <$ try pMacroEndIf
    , Nothing <$ anySingle
    ]
  eof
  return [d | Just d <- docs]


-- | State for tracking source target during doc string parsing
-- Since we can't use StateT easily with Parsec Void, we'll use a simpler approach
-- and track target macros by looking at context

-- | Parse a single doc string: /** ... */
pDocString :: DocParser DocString
pDocString = do
  void $ C.string "/**"
  -- Collect all content until */
  content <- manyTill anySingle (try $ C.string "*/")
  -- Look ahead to determine what this documents
  afterContent <- getInput
  let afterStr = T.unpack $ T.take 1000 afterContent
  let charContent = content
  let assembled = charContent
  let description = processCharacters [assembled]

  -- Parse tags from the content
  let (desc, tags) = extractTags assembled

  -- Determine the type of doc string based on what follows
  let target = TargetAll  -- TODO: track target macros properly

  -- Try to match what follows
  let matchedFn       = match (toRegex regexFunctionDocString) afterStr :: (String, String, String, [String])
  let matchedTyping   = match (toRegex regexTypingDocString) afterStr :: (String, String, String, [String])
  let matchedTypeDec  = match (toRegex regexTypeDefDocString) afterStr :: (String, String, String, [String])
  let matchedIface    = match (toRegex regexInterfaceDocString) afterStr :: (String, String, String, [String])
  let matchedInst     = match (toRegex regexInstanceDocString) afterStr :: (String, String, String, [String])

  case (matchedFn, matchedTyping, matchedTypeDec, matchedIface, matchedInst) of
    ((_, _, _, [fnName]), _, _, _, _) ->
      return $ FunctionDoc target fnName (processCharacters [desc]) tags

    (_, (_, _, _, [fnName]), _, _, _) ->
      return $ FunctionDoc target fnName (processCharacters [desc]) tags

    (_, _, (_, _, _, [_, _, typeName]), _, _) ->
      return $ TypeDefDoc target typeName (processCharacters [desc]) tags

    (_, _, _, (_, _, _, [_, typeName]), _) ->
      return $ InterfaceDoc target typeName (processCharacters [desc]) tags

    (_, _, _, _, (_, _, _, [_, typeName])) ->
      return $ InstanceDoc target (trim typeName) (processCharacters [desc]) tags

    _ ->
      return $ ModuleDoc target (processCharacters [desc])


-- | Extract tags (@example, @since) from doc string content
extractTags :: String -> (String, [DocStringTag])
extractTags input = go input "" []
  where
    go [] desc tags = (desc, reverse tags)
    go ('@':'e':'x':'a':'m':'p':'l':'e':rest) desc tags =
      let (tagContent, remaining) = extractTagContent rest
      in  go remaining desc (ExampleTag TargetAll (processCharacters [tagContent]) : tags)
    go ('@':'s':'i':'n':'c':'e':rest) desc tags =
      let (tagContent, remaining) = extractTagContent rest
      in  go remaining desc (SinceTag TargetAll (processCharacters [tagContent]) : tags)
    go (c:rest) desc tags = go rest (desc ++ [c]) tags

    extractTagContent :: String -> (String, String)
    extractTagContent input = go' input ""
      where
        go' [] acc = (acc, [])
        go' ('@':rest) acc = (acc, '@':rest)  -- Next tag starts
        go' (c:rest) acc = go' rest (acc ++ [c])


-- Regex patterns (same as the original DocString lexer)

regexFunctionDocString :: String
regexFunctionDocString = "\\`\n[ ]*export[\n\\ ]*([a-zA-Z0-9_]*)[\n\\ ]*=.*"

regexTypingDocString :: String
regexTypingDocString = "\\`\n[ ]*([a-zA-Z0-9_]*)[ ]*\\:\\:.*"

regexTypeDefDocString :: String
regexTypeDefDocString = "\\`\n[ ]*(export )?(type|alias)[ ]*([a-zA-Z0-9_]*)[a-zA-Z0-9_ \n]*.*"

regexInterfaceDocString :: String
regexInterfaceDocString = "\\`\n[ ]*interface([^=]*=>)?[ ]*([a-zA-Z0-9_]*)[a-zA-Z0-9_ \n]*\\{.*"

regexInstanceDocString :: String
regexInstanceDocString = "\\`\n[ ]*instance([^=]*=>)?[ ]*([a-zA-Z0-9_ ()<>]*)\\{.*"

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False, lastStarGreedy = False, rightAssoc = False } defaultExecOpt


-- Helper functions (same as original Grammar.y)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

regex :: String
regex = "\n[ ]*\\*[ ]?"

sanitizeDescription :: String -> String
sanitizeDescription desc =
  let matched = match (toRegex regex) desc :: (String, String, String)
  in  case matched of
    (before, "", "") ->
      let trimmed = trim desc
      in
        if isSuffixOf "\n" trimmed then
          sanitizeDescription (init $ init trimmed)
        else trimmed
    (before, _, after) ->
      if before == "" then
        sanitizeDescription after
      else if after == "" then
        sanitizeDescription before
      else
        sanitizeDescription $ before <> "\n" <> after


processCharacters :: [String] -> String
processCharacters chars =
  let assembled = foldl' (<>) "" chars
  in  sanitizeDescription assembled


-- | Skip target macro directives
pMacroIfTarget :: DocParser ()
pMacroIfTarget = void $ C.string "#iftarget" *> many (C.alphaNumChar <|> C.char ' ')

pMacroElseIf :: DocParser ()
pMacroElseIf = void $ C.string "#elseif" *> many (C.alphaNumChar <|> C.char ' ')

pMacroEndIf :: DocParser ()
pMacroEndIf = void $ C.string "#endif"
