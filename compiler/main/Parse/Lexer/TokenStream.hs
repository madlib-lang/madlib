{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -O3 #-}
-- | Megaparsec Stream instance for a list of RangedToken.
-- Enables using a pre-lexed token list as the input to Megaparsec parsers.
module Parse.Lexer.TokenStream
  ( TokenStream(..)
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))

import Text.Megaparsec (Stream(..), VisualStream(..), TraversableStream(..), PosState(..), SourcePos(..))
import Text.Megaparsec.Pos (mkPos, unPos)

import Explain.Location (Loc(..))
import Parse.Lexer.Token (Token(..), RangedToken(..))


-- | Wrapper around a list of RangedToken, providing Megaparsec's Stream interface.
-- We use a newtype to give it a distinct Stream instance.
newtype TokenStream = TokenStream { unTokenStream :: [RangedToken] }
  deriving (Show)


-- Megaparsec requires Ord for Token s and Tokens s.
-- Token s = RangedToken, Tokens s = [RangedToken]
-- We need Ord on RangedToken. Let's derive it.
-- (Token.hs already derives Eq and Ord on Token.)


-- | Stream instance: token = RangedToken, tokens = [RangedToken]
instance Stream TokenStream where
  type Token  TokenStream = RangedToken
  type Tokens TokenStream = [RangedToken]

  tokenToChunk  _ t  = [t]
  tokensToChunk _ ts = ts
  chunkToTokens _ ts = ts
  chunkLength   _ ts = length ts
  chunkEmpty    _ ts = null ts

  take1_ (TokenStream [])     = Nothing
  take1_ (TokenStream (t:ts)) = Just (t, TokenStream ts)

  takeN_ n (TokenStream ts)
    | n <= 0   = Just ([], TokenStream ts)
    | null ts  = Nothing
    | otherwise = let (pre, post) = splitAt n ts in Just (pre, TokenStream post)

  takeWhile_ f (TokenStream ts) =
    let (pre, post) = span f ts in (pre, TokenStream post)


-- | VisualStream: for error messages
instance VisualStream TokenStream where
  showTokens _ ts = intercalate " " (map (showToken . rtToken) (NE.toList ts))
    where
      showToken TkEOF                     = "<EOF>"
      showToken TkNewline                 = "<newline>"
      showToken (TkName s)                = s
      showToken (TkTypeName s)            = s
      showToken (TkInt s)                 = s
      showToken (TkFloat s)               = s
      showToken (TkByte s)                = s ++ "b"
      showToken (TkShort s)               = s ++ "s"
      showToken (TkHexNumber s)           = s
      showToken (TkHexByte s)             = s
      showToken (TkHexShort s)            = s
      showToken (TkHexInt s)              = s
      showToken (TkString s)              = "\"" ++ s ++ "\""
      showToken (TkChar s)                = "'" ++ s ++ "'"
      showToken (TkTemplateStringFull s)  = "`" ++ s ++ "`"
      showToken (TkTemplateStringStart s) = "`" ++ s ++ "${"
      showToken (TkTemplateStringMid s)   = "}" ++ s ++ "${"
      showToken (TkTemplateStringEnd s)   = "}" ++ s ++ "`"
      showToken TkTemplateInterpolClose   = "} (interp close)"
      showToken (TkJSBlock _)             = "#- ... -#"
      showToken TkIf                      = "if"
      showToken TkElse                    = "else"
      showToken TkWhile                   = "while"
      showToken TkWhere                   = "where"
      showToken TkDo                      = "do"
      showToken TkReturn                  = "return"
      showToken TkPipe                    = "pipe"
      showToken TkImport                  = "import"
      showToken TkExport                  = "export"
      showToken TkFrom                    = "from"
      showToken TkType                    = "type"
      showToken TkAlias                   = "alias"
      showToken TkExtern                  = "extern"
      showToken TkInterface               = "interface"
      showToken TkInstance                = "instance"
      showToken TkDerive                  = "derive"
      showToken TkWhen                    = "when"
      showToken TkIs                      = "is"
      showToken TkNot                     = "not"
      showToken TkTrue                    = "true"
      showToken TkFalse                   = "false"
      showToken TkEq                      = "="
      showToken TkMutateEq                = ":="
      showToken TkDoubleColon             = "::"
      showToken TkColon                   = ":"
      showToken TkComma                   = ","
      showToken TkSpread                  = "..."
      showToken TkDot                     = "."
      showToken TkLeftParen               = "("
      showToken TkRightParen              = ")"
      showToken TkLeftCurly               = "{"
      showToken TkRightCurly              = "}"
      showToken TkLeftDoubleCurly         = "{{"
      showToken TkLeftSquare              = "["
      showToken TkRightSquare             = "]"
      showToken TkTupleStart              = "#["
      showToken TkLeftArrow               = "<-"
      showToken TkRightArrow              = "->"
      showToken TkFatArrow                = "=>"
      showToken TkPipeOp                  = "|>"
      showToken TkPipeChar                = "|"
      showToken TkSemicolon               = ";"
      showToken TkSharp                   = "#"
      showToken TkDollar                  = "$"
      showToken TkQuestionMark            = "?"
      showToken TkDoubleQuestionMark      = "??"
      showToken TkQuestionDot             = "?."
      showToken TkTypedHole               = "???"
      showToken TkPlus                    = "+"
      showToken TkDoublePlus              = "++"
      showToken TkDash                    = "-"
      showToken TkStar                    = "*"
      showToken TkSlash                   = "/"
      showToken TkPercent                 = "%"
      showToken TkDoubleEq                = "=="
      showToken TkNotEq                   = "!="
      showToken TkLeftChevron             = "<"
      showToken TkRightChevron            = ">"
      showToken TkLeftChevronEq           = "<="
      showToken TkRightChevronEq          = ">="
      showToken TkDoubleAmpersand         = "&&"
      showToken TkDoublePipe              = "||"
      showToken TkExclamation             = "!"
      showToken TkAmpersand               = "&"
      showToken TkXor                     = "^"
      showToken TkTilde                   = "~"
      showToken TkDoubleLeftChevron       = "<<"
      showToken TkDoubleRightChevron      = ">>"
      showToken TkTripleRightChevron      = ">>>"
      showToken TkAlternativeOp           = "<|>"


-- | TraversableStream: for computing source positions from offsets.
-- Offset = index into the token list.
-- We use the position stored in each RangedToken for accurate error messages.
instance TraversableStream TokenStream where
  reachOffset o PosState{..} =
    ( Just $ showCurrentToken (drop (o - pstateOffset) (unTokenStream pstateInput))
    , PosState
        { pstateInput      = TokenStream $ drop (o - pstateOffset) (unTokenStream pstateInput)
        , pstateOffset     = max pstateOffset o
        , pstateSourcePos  = newPos
        , pstateTabWidth   = pstateTabWidth
        , pstateLinePrefix = pstateLinePrefix
        }
    )
    where
      newPos = case drop (o - pstateOffset) (unTokenStream pstateInput) of
        []    -> pstateSourcePos
        (t:_) -> locToSourcePos pstateSourcePos (rtStart t)

      showCurrentToken []    = "<EOF>"
      showCurrentToken (t:_) = showTokForError (rtToken t)

      showTokForError TkEOF      = "<EOF>"
      showTokForError TkNewline  = "<newline>"
      showTokForError (TkName s) = s
      showTokForError TkIf       = "if"
      showTokForError TkElse     = "else"
      showTokForError t          = show t


-- | Convert a Loc to a SourcePos, using the file name from an existing SourcePos.
locToSourcePos :: SourcePos -> Loc -> SourcePos
locToSourcePos ref (Loc _ l c) = SourcePos
  { sourceName   = sourceName ref
  , sourceLine   = mkPos (max 1 l)
  , sourceColumn = mkPos (max 1 c)
  }
