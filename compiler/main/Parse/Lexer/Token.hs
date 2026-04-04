module Parse.Lexer.Token where

import Explain.Location (Loc(..))


-- | A token with its source range
data RangedToken = RangedToken
  { rtToken :: !Token
  , rtStart :: !Loc
  , rtEnd   :: !Loc
  } deriving (Show, Eq, Ord)


-- | All Madlib tokens
data Token
  -- Literals
  = TkInt       !String
  | TkFloat     !String
  | TkByte      !String
  | TkShort     !String
  | TkHexInt    !String   -- hex integer (0x...)
  | TkHexByte   !String   -- hex byte (0x...b)
  | TkHexShort  !String   -- hex short (0x...s)
  | TkHexNumber !String   -- hex number (0x...)
  | TkString    !String
  | TkChar      !String
  | TkTrue
  | TkFalse
  -- Identifiers
  | TkName      !String   -- lower-case identifier
  | TkTypeName  !String   -- upper-case identifier (type/constructor names)
  -- Template strings (split at interpolation boundaries)
  | TkTemplateStringStart !String  -- backtick ... ${ (content before first interpolation)
  | TkTemplateStringMid   !String  -- } ... ${ (content between interpolations)
  | TkTemplateStringEnd   !String  -- } ... backtick (content after last interpolation)
  | TkTemplateStringFull  !String  -- backtick ... backtick (no interpolation)
  -- Template interpolation close marker
  | TkTemplateInterpolClose  -- } that closes a ${...} interpolation
  -- JS block
  | TkJSBlock   !String
  -- Keywords
  | TkIf
  | TkElse
  | TkWhile
  | TkWhere
  | TkDo
  | TkReturn
  | TkPipe
  | TkImport
  | TkExport
  | TkFrom
  | TkType
  | TkAlias
  | TkExtern
  | TkInterface
  | TkInstance
  | TkDerive
  | TkWhen
  | TkIs
  | TkNot
  -- Symbols
  | TkEq               -- =  (not == or =>)
  | TkMutateEq         -- :=
  | TkDoubleColon      -- ::
  | TkColon            -- :
  | TkComma            -- ,
  | TkSpread           -- ...
  | TkDot              -- .
  | TkLeftParen        -- (
  | TkRightParen       -- )
  | TkLeftCurly        -- {
  | TkRightCurly       -- }
  | TkLeftDoubleCurly  -- {{
  | TkLeftSquare       -- [
  | TkRightSquare      -- ]
  | TkTupleStart       -- #[
  | TkLeftArrow        -- <-
  | TkRightArrow       -- ->
  | TkFatArrow         -- =>
  | TkPipeOp           -- |>
  | TkPipeChar         -- |
  | TkSemicolon        -- ;
  | TkSharp            -- #  (not #[ or #-)
  | TkDollar           -- $
  | TkQuestionMark     -- ?  (not ??)
  | TkDoubleQuestionMark -- ??
  | TkQuestionDot      -- ?.
  | TkTypedHole        -- ???
  -- Arithmetic
  | TkPlus             -- +
  | TkDoublePlus       -- ++
  | TkDash             -- -
  | TkStar             -- *
  | TkSlash            -- /
  | TkPercent          -- %
  -- Comparison
  | TkDoubleEq         -- ==
  | TkNotEq            -- !=
  | TkLeftChevron      -- <
  | TkRightChevron     -- >
  | TkLeftChevronEq    -- <=
  | TkRightChevronEq   -- >=
  -- Logical
  | TkDoubleAmpersand  -- &&
  | TkDoublePipe       -- ||
  | TkExclamation      -- !
  -- Bitwise
  | TkAmpersand        -- &
  | TkXor              -- ^
  | TkTilde            -- ~
  | TkDoubleLeftChevron  -- <<
  | TkDoubleRightChevron -- >>
  | TkTripleRightChevron -- >>>
  -- Alternative op
  | TkAlternativeOp    -- <|>
  -- Whitespace / structure
  | TkNewline
  | TkEOF
  deriving (Show, Eq, Ord)
