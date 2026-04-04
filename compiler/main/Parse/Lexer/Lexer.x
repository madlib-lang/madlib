{
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-tabs #-}
-- Alex-generated lexer for Madlib.
-- Uses monadUserState-bytestring wrapper.
-- Template strings: emits TkTemplateInterpolOpen/$Close markers and tracks
-- curly brace nesting so the lexer can pre-lex the entire file without
-- needing to interleave with the parser.
module Parse.Lexer.Lexer
  ( scanMany
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Explain.Location (Loc(..))
import Parse.Lexer.Token (Token(..), RangedToken(..))
}

%wrapper "monadUserState-bytestring"

$digit     = [0-9]
$hexdigit  = [0-9a-fA-F]
$lower     = [a-z]
$upper     = [A-Z]
$identchar = [a-zA-Z0-9_\']

@ident     = ($lower | "_") $identchar*
@typeName  = $upper $identchar*
@decimal   = $digit+
@hexnum    = "0x" $hexdigit+
@float     = $digit+ "." $digit+ ([eE] [\+\-]? $digit+)?
@escape    = \\ ([\"\\\/bfnrt\`\'] | "u" "{" $hexdigit+ "}" | "u" $hexdigit{4} | "x" $hexdigit{2} | $digit)

tokens :-

-- -- Normal mode (code)

-- Skip whitespace (no newlines)
<0,interp> [\ \t\f\v\r]+ ;
<0,interp> "//" [^\n]*   ;
-- Block comments: use a start code so they can span multiple lines
<0,interp>     "/*"  { beginBlockComment }
<blkcmt>        "*/" { endBlockComment }
<blkcmt>        \n   ;
<blkcmt>        .    ;

-- Newlines (significant in Madlib grammar)
<0> \n  { \inp _ -> do { recordStart inp; return TkNewline } }

-- Keywords come before @ident rules because Alex uses longest-match;
-- if "ifx" is in input, @ident wins. If just "if" is next, "if" wins.
<0,interp> "if"        { kwOrIdent TkIf }
<0,interp> "else"      { kwOrIdent TkElse }
<0,interp> "while"     { kwOrIdent TkWhile }
<0,interp> "where"     { kwOrIdent TkWhere }
<0,interp> "do"        { kwOrIdent TkDo }
<0,interp> "return"    { kwOrIdent TkReturn }
<0,interp> "pipe"      { kwOrIdent TkPipe }
<0,interp> "import"    { kwOrIdent TkImport }
<0,interp> "export"    { kwOrIdent TkExport }
<0,interp> "from"      { kwOrIdent TkFrom }
<0,interp> "type"      { kwOrIdent TkType }
<0,interp> "alias"     { kwOrIdent TkAlias }
<0,interp> "extern"    { kwOrIdent TkExtern }
<0,interp> "interface" { kwOrIdent TkInterface }
<0,interp> "instance"  { kwOrIdent TkInstance }
<0,interp> "derive"    { kwOrIdent TkDerive }
<0,interp> "when"      { kwOrIdent TkWhen }
<0,interp> "is"        { kwOrIdent TkIs }
<0,interp> "not"       { kwOrIdent TkNot }
<0,interp> "true"      { \inp _ -> do { recordStart inp; return TkTrue } }
<0,interp> "false"     { \inp _ -> do { recordStart inp; return TkFalse } }

<0,interp> @ident    { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkName    (BSLC.unpack (BSL.take len bs))) } }
<0,interp> @typeName { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkTypeName (BSLC.unpack (BSL.take len bs))) } }

<0,interp> @hexnum "_b" { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkHexByte   (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @hexnum "_s" { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkHexShort  (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @hexnum "_i" { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkHexInt    (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @hexnum      { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkHexNumber (BSLC.unpack (BSL.take len bs))) } }
<0,interp> @decimal "_b" { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkByte  (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @decimal "_s" { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkShort (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @float "_f"  { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkFloat (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @float       { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkFloat (BSLC.unpack (BSL.take len bs))) } }
<0,interp> @decimal "_f" { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkFloat (BSLC.unpack (BSL.take (len-2) bs))) } }
<0,interp> @decimal     { \inp@(_,_,bs,_) len -> do { recordStart inp; return (TkInt   (BSLC.unpack (BSL.take len bs))) } }

-- String literals
<0,interp> \"        { beginLit string }
<string>   @escape   { accumLit }
<string>   [^\"\\\n]+ { accumLit }
<string>   \n        { accumLit }
<string>   \"        { endLit TkString }

-- Char literals
<0,interp> \'        { beginLit char_lit }
<char_lit> \\ .      { accumLit }
<char_lit> [^\'\\\n]+ { accumLit }
<char_lit> \n        { accumLit }
<char_lit> \'        { endLit TkChar }

-- Template strings
<0,interp> \`        { beginTemplate }
<tmpl>     @escape   { accumLit }
<tmpl>     [^\`\\\$\n]+ { accumLit }
<tmpl>     \n        { accumLit }
<tmpl>     "${"      { emitTmplInterpolOpen }
<tmpl>     "$"       { \_ _ -> do { modLitVal (++ "$"); alexMonadScan } }
<tmpl>     \`        { emitTmplFull }

-- Template continuation (after interpolation closed by })
<tmpl_cont> @escape      { accumLit }
<tmpl_cont> [^\`\\\$\n]+ { accumLit }
<tmpl_cont> \n           { accumLit }
<tmpl_cont> "${"         { emitTmplInterpolOpenCont }
<tmpl_cont> "$"          { \_ _ -> do { modLitVal (++ "$"); alexMonadScan } }
<tmpl_cont> \`           { emitTmplEnd }

-- Interpolation context: normal code but track { } nesting.
-- When brace depth hits 0 via }, that } closes the interpolation.
<interp> "{{" { interpDoubleLCurly }
<interp> "{"  { interpLCurly }
<interp> "}"  { interpRCurly }
<interp> \n   { \inp _ -> do { recordStart inp; return TkNewline } }
<interp> \`   { beginTemplate }

-- JS blocks
<0,interp> "#-"     { beginJSBlock }
<jsbl>     "-#"     { endJSBlock }
<jsbl>     [^\-\n]+ { accumJSBlock }
<jsbl>     "-"      { accumJSBlock }
<jsbl>     \n       { accumJSBlock }

-- Multi-char operators (longest first)
<0,interp> "???"   { \inp _ -> do { recordStart inp; return TkTypedHole } }
<0,interp> "..."   { \inp _ -> do { recordStart inp; return TkSpread } }
<0,interp> ">>>"   { \inp _ -> do { recordStart inp; return TkTripleRightChevron } }
<0,interp> "<|>"   { \inp _ -> do { recordStart inp; return TkAlternativeOp } }
<0,interp> "??"    { \inp _ -> do { recordStart inp; return TkDoubleQuestionMark } }
<0,interp> "?."    { \inp _ -> do { recordStart inp; return TkQuestionDot } }
<0,interp> "::"    { \inp _ -> do { recordStart inp; return TkDoubleColon } }
<0,interp> ":="    { \inp _ -> do { recordStart inp; return TkMutateEq } }
<0,interp> "=="    { \inp _ -> do { recordStart inp; return TkDoubleEq } }
<0,interp> "!="    { \inp _ -> do { recordStart inp; return TkNotEq } }
<0,interp> "<="    { \inp _ -> do { recordStart inp; return TkLeftChevronEq } }
<0,interp> ">="    { \inp _ -> do { recordStart inp; return TkRightChevronEq } }
<0,interp> "&&"    { \inp _ -> do { recordStart inp; return TkDoubleAmpersand } }
<0,interp> "||"    { \inp _ -> do { recordStart inp; return TkDoublePipe } }
<0,interp> "++"    { \inp _ -> do { recordStart inp; return TkDoublePlus } }
<0,interp> "->"    { \inp _ -> do { recordStart inp; return TkRightArrow } }
<0,interp> "<-"    { \inp _ -> do { recordStart inp; return TkLeftArrow } }
<0,interp> "=>"    { \inp _ -> do { recordStart inp; return TkFatArrow } }
<0,interp> "|>"    { \inp _ -> do { recordStart inp; return TkPipeOp } }
<0,interp> "<<"    { \inp _ -> do { recordStart inp; return TkDoubleLeftChevron } }
<0,interp> ">>"    { \inp _ -> do { recordStart inp; return TkDoubleRightChevron } }
<0,interp> "#["    { \inp _ -> do { recordStart inp; return TkTupleStart } }
<0,interp> "{{"    { \inp _ -> do { recordStart inp; return TkLeftDoubleCurly } }

-- Single-char operators (for interp: { and } handled specially above)
<0,interp> "="  { \inp _ -> do { recordStart inp; return TkEq } }
<0,interp> ":"  { \inp _ -> do { recordStart inp; return TkColon } }
<0,interp> ","  { \inp _ -> do { recordStart inp; return TkComma } }
<0,interp> "."  { \inp _ -> do { recordStart inp; return TkDot } }
<0,interp> "("  { \inp _ -> do { recordStart inp; return TkLeftParen } }
<0,interp> ")"  { \inp _ -> do { recordStart inp; return TkRightParen } }
<0>        "{"  { \inp _ -> do { recordStart inp; return TkLeftCurly } }
<0>        "}"  { \inp _ -> do { recordStart inp; return TkRightCurly } }
<0,interp> "["  { \inp _ -> do { recordStart inp; return TkLeftSquare } }
<0,interp> "]"  { \inp _ -> do { recordStart inp; return TkRightSquare } }
<0,interp> "|"  { \inp _ -> do { recordStart inp; return TkPipeChar } }
<0,interp> ";"  { \inp _ -> do { recordStart inp; return TkSemicolon } }
<0,interp> "#"  { \inp _ -> do { recordStart inp; return TkSharp } }
<0,interp> "$"  { \inp _ -> do { recordStart inp; return TkDollar } }
<0,interp> "?"  { \inp _ -> do { recordStart inp; return TkQuestionMark } }
<0,interp> "+"  { \inp _ -> do { recordStart inp; return TkPlus } }
<0,interp> "-"  { \inp _ -> do { recordStart inp; return TkDash } }
<0,interp> "*"  { \inp _ -> do { recordStart inp; return TkStar } }
<0,interp> "/"  { \inp _ -> do { recordStart inp; return TkSlash } }
<0,interp> "%"  { \inp _ -> do { recordStart inp; return TkPercent } }
<0,interp> "<"  { \inp _ -> do { recordStart inp; return TkLeftChevron } }
<0,interp> ">"  { \inp _ -> do { recordStart inp; return TkRightChevron } }
<0,interp> "!"  { \inp _ -> do { recordStart inp; return TkExclamation } }
<0,interp> "&"  { \inp _ -> do { recordStart inp; return TkAmpersand } }
<0,interp> "^"  { \inp _ -> do { recordStart inp; return TkXor } }
<0,interp> "~"  { \inp _ -> do { recordStart inp; return TkTilde } }


{
-- | Lexer user state
-- usTmplStack: stack of saved interp depths for nested template strings.
-- When we start a template inside an interpolation, we save the current depth.
-- When that inner template ends, we restore the saved depth and go back to interp.
data AlexUserState = AlexUserState
  { usLitVal     :: !String    -- accumulated literal (string/char/template) content
  , usJSBVal     :: !String    -- accumulated JS block content
  , usTmplDepth  :: !Int       -- nesting depth inside current template interpolation
  , usTmplStack  :: ![Int]     -- saved depths for nested templates
  , usTokenStart :: !AlexPosn  -- start position of current token (set by each token action)
  , usLitStart   :: !AlexPosn  -- start position of the current literal (string/char/template/jsblock)
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { usLitVal     = []
  , usJSBVal     = []
  , usTmplDepth  = 0
  , usTmplStack  = []
  , usTokenStart = AlexPn 0 1 1
  , usLitStart   = AlexPn 0 1 1
  }

-- | Get the user state (not provided by monadUserState-bytestring template)
alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{ alex_ust = ust } -> Right (s, ust)

-- | Set the user state
alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ust = Alex $ \s -> Right (s{ alex_ust = ust }, ())

-- | Convert AlexPosn to Loc
toLoc :: AlexPosn -> Loc
toLoc (AlexPn a l c) = Loc a l c

-- | Modify accumulated literal value
modLitVal :: (String -> String) -> Alex ()
modLitVal f = do
  st <- alexGetUserState
  alexSetUserState st { usLitVal = f (usLitVal st) }

-- | Record the start position of the current token from AlexInput.
-- Call this at the top of every token-producing action.
recordStart :: AlexInput -> Alex ()
recordStart (pn, _, _, _) = do
  st <- alexGetUserState
  alexSetUserState st { usTokenStart = pn }

-- | kwOrIdent: In Alex, if "ifx" is in input, @ident matches all 3 chars
-- and "if" cannot match because @ident is longer. So "if" only matches
-- when it IS the longest match (e.g. "if "). This action just returns the keyword.
kwOrIdent :: Token -> AlexAction Token
kwOrIdent kwTok inp _ = do
  recordStart inp
  return kwTok

-- | Begin a string or char literal: clear buffer, switch start code
beginLit :: Int -> AlexAction Token
beginLit mode inp@(pn, _, _, _) _ = do
  recordStart inp
  st <- alexGetUserState
  alexSetUserState st { usLitVal = [], usLitStart = pn }
  alexSetStartCode mode
  alexMonadScan

-- | Accumulate literal content (recursive – does NOT produce a token itself,
-- so we do NOT record a start here; the start was recorded when the literal began)
-- Uses UTF-8 decoding to correctly handle multi-byte characters.
accumLit :: AlexAction Token
accumLit (_, _, bs, _) len = do
  modLitVal (++ T.unpack (TE.decodeUtf8 (BSL.toStrict (BSL.take len bs))))
  alexMonadScan

-- | End a literal: emit token, return to start code 0 or interp if inside interpolation
-- Restores usTokenStart to usLitStart so scanMany sees the opening delimiter position.
endLit :: (String -> Token) -> AlexAction Token
endLit mkTk _ _ = do
  st <- alexGetUserState
  let val = usLitVal st
  alexSetUserState st { usLitVal = [], usTokenStart = usLitStart st }
  -- If we are inside a template interpolation, restore to interp mode
  if usTmplDepth st > 0
    then alexSetStartCode interp
    else alexSetStartCode 0
  return (mkTk val)

-- | Begin a template string.
-- If we're currently inside an interpolation (start code interp), push the current
-- depth onto the stack so we can restore it when this inner template ends.
beginTemplate :: AlexAction Token
beginTemplate inp@(pn, _, _, _) _ = do
  recordStart inp
  st <- alexGetUserState
  startCode <- alexGetStartCode
  let newStack = if startCode == interp then usTmplDepth st : usTmplStack st else usTmplStack st
  alexSetUserState st { usLitVal = [], usTmplStack = newStack, usLitStart = pn }
  alexSetStartCode tmpl
  alexMonadScan

-- | Template: hit dollar-brace -> emit the segment, push to interp mode
emitTmplInterpolOpen :: AlexAction Token
emitTmplInterpolOpen _ _ = do
  st <- alexGetUserState
  let val = usLitVal st
  -- Restore usTokenStart to the opening backtick position
  alexSetUserState st { usLitVal = [], usTmplDepth = 1, usTokenStart = usLitStart st }
  alexSetStartCode interp
  return (TkTemplateStringStart val)

-- | Template continuation: hit dollar-brace -> emit mid segment
emitTmplInterpolOpenCont :: AlexAction Token
emitTmplInterpolOpenCont inp@(pn, _, _, _) _ = do
  recordStart inp
  st <- alexGetUserState
  let val = usLitVal st
  alexSetUserState st { usLitVal = [], usTmplDepth = 1, usLitStart = pn }
  alexSetStartCode interp
  return (TkTemplateStringMid val)

-- | Template: hit ` -> full template (no interpolation)
-- If stack non-empty, we're nested inside an outer interpolation; restore it.
emitTmplFull :: AlexAction Token
emitTmplFull _ _ = do
  st <- alexGetUserState
  let val = usLitVal st
  case usTmplStack st of
    (savedDepth : rest) -> do
      alexSetUserState st { usLitVal = [], usTmplDepth = savedDepth, usTmplStack = rest
                          , usTokenStart = usLitStart st }
      alexSetStartCode interp
    [] -> do
      alexSetUserState st { usLitVal = [], usTokenStart = usLitStart st }
      alexSetStartCode 0
  return (TkTemplateStringFull val)

-- | Template continuation: hit ` -> end segment
emitTmplEnd :: AlexAction Token
emitTmplEnd _ _ = do
  st <- alexGetUserState
  let val = usLitVal st
  case usTmplStack st of
    (savedDepth : rest) -> do
      alexSetUserState st { usLitVal = [], usTmplDepth = savedDepth, usTmplStack = rest
                          , usTokenStart = usLitStart st }
      alexSetStartCode interp
    [] -> do
      alexSetUserState st { usLitVal = [], usTokenStart = usLitStart st }
      alexSetStartCode 0
  return (TkTemplateStringEnd val)

-- | In interp mode: double-curly increases nesting depth by 2
interpDoubleLCurly :: AlexAction Token
interpDoubleLCurly inp _ = do
  recordStart inp
  st <- alexGetUserState
  alexSetUserState st { usTmplDepth = usTmplDepth st + 2 }
  return TkLeftDoubleCurly

-- | In interp mode: single-curly increases nesting depth
interpLCurly :: AlexAction Token
interpLCurly inp _ = do
  recordStart inp
  st <- alexGetUserState
  alexSetUserState st { usTmplDepth = usTmplDepth st + 1 }
  return TkLeftCurly

-- | In interp mode: right-curly decreases nesting; if depth reaches 0, close interpolation
interpRCurly :: AlexAction Token
interpRCurly inp _ = do
  recordStart inp
  st <- alexGetUserState
  let depth = usTmplDepth st - 1
  alexSetUserState st { usTmplDepth = depth }
  if depth <= 0
    then do
      -- Back to template continuation mode
      alexSetStartCode tmpl_cont
      return TkTemplateInterpolClose
    else
      return TkRightCurly

-- | JS block: begin
beginJSBlock :: AlexAction Token
beginJSBlock inp@(pn, _, _, _) _ = do
  recordStart inp
  st <- alexGetUserState
  alexSetUserState st { usJSBVal = [], usLitStart = pn }
  alexSetStartCode jsbl
  alexMonadScan

-- | JS block: accumulate
accumJSBlock :: AlexAction Token
accumJSBlock (_, _, bs, _) len = do
  st <- alexGetUserState
  alexSetUserState st { usJSBVal = usJSBVal st ++ T.unpack (TE.decodeUtf8 (BSL.toStrict (BSL.take len bs))) }
  alexMonadScan

-- | JS block: end
endJSBlock :: AlexAction Token
endJSBlock _ _ = do
  st <- alexGetUserState
  let val = usJSBVal st
  alexSetUserState st { usJSBVal = [], usTokenStart = usLitStart st }
  alexSetStartCode 0
  return (TkJSBlock val)

-- | Block comment: begin
beginBlockComment :: AlexAction Token
beginBlockComment _ _ = do
  alexSetStartCode blkcmt
  alexMonadScan

-- | Block comment: end, restore outer start code
endBlockComment :: AlexAction Token
endBlockComment _ _ = do
  st <- alexGetUserState
  if usTmplDepth st > 0
    then alexSetStartCode interp
    else alexSetStartCode 0
  alexMonadScan

-- Required by monadUserState wrapper
alexEOF :: Alex Token
alexEOF = return TkEOF

-- | Scan entire input into a list of RangedTokens.
-- alexMonadScan :: Alex Token (in monadUserState wrapper).
-- We record position before/after each scan call.
-- The monadUserState-bytestring wrapper uses lazy ByteString, so we convert.
scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex (BSL.fromStrict input) loop
  where
    loop :: Alex [RangedToken]
    loop = do
      tok <- alexMonadScan
      (endPn, _, _, _) <- alexGetInput
      startPn <- fmap usTokenStart alexGetUserState
      let rt = RangedToken tok (toLoc startPn) (toLoc endPn)
      if tok == TkEOF
        then return [rt]
        else do
          rest <- loop
          return (rt : rest)
}
