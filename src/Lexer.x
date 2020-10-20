{
{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Lexer
  ( Alex(..)
  , AlexState(..)
  , alexEOF
  , Token(..)
  , Pos(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPos
  , strV
  )
where

import           System.Exit
import           Debug.Trace
import qualified Data.Text as T
}

%wrapper "monad"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$empty =  [\ \t\f\v\r]          -- equivalent to $white but without line return

tokens :-
  [\n]                                  { mapToken (\_ -> TokenReturn) }
  \"($printable # \")+\"                { mapToken (\s -> TokenStr (sanitizeStr s)) }
  "--".*                                ;
  import                                { mapToken (\_ -> TokenImport) }
  export                                { mapToken (\_ -> TokenExport) }
  from                                  { mapToken (\_ -> TokenFrom) }
  data                                  { mapToken (\_ -> TokenData) }
  const                                 { mapToken (\_ -> TokenConst) }
  [=]                                   { mapToken (\_ -> TokenEq) }
  \+                                    { mapToken (\_ -> TokenPlus) }
  \-                                    { mapToken (\_ -> TokenDash) }
  \*                                    { mapToken (\_ -> TokenStar) }
  \/                                    { mapToken (\_ -> TokenSlash) }
  if                                    { mapToken (\_ -> TokenIf) }
  $digit+                               { mapToken (\s -> TokenInt s) }
  "True"                                { mapToken (\_ -> (TokenBool "True")) }
  "False"                               { mapToken (\_ -> (TokenBool "False")) }
  "==="                                 { mapToken (\_ -> TokenTripleEq) }
  \.                                    { mapToken (\_ -> TokenDot) }
  \,                                    { mapToken (\_ -> TokenComa) }
  \{                                    { mapToken (\_ -> TokenLeftCurly) }
  \}                                    { mapToken (\_ -> TokenRightCurly) }
  \(                                    { mapToken (\_ -> TokenLeftParen) }
  \)                                    { mapToken (\_ -> TokenRightParen) }
  \:\:                                  { mapToken (\_ -> TokenDoubleColon) }
  \:                                    { mapToken (\_ -> TokenColon) }
  \-\>                                  { mapToken (\_ -> TokenArrow) }
  \=\>                                  { mapToken (\_ -> TokenFatArrow) }
  \|                                    { mapToken (\_ -> TokenPipe) }
  \;                                    { mapToken (\_ -> TokenSemiColon) }
  -- \#\-                                  { mapToken (\_ -> TokenJSBlockLeft) }
  -- \-\#                                  { mapToken (\_ -> TokenJSBlockRight) }
  $alpha [$alpha $digit \_ \']*         { mapToken (\s -> TokenName s) }
  \#\- [$alpha $digit \_ \' \  \. \( \)]* \-\#   { mapToken (\s -> TokenJSBlock (sanitizeJSBlock s)) }
  $empty+                               ;

{
sanitizeStr :: String -> String
sanitizeStr = tail . init

sanitizeJSBlock :: String -> String
sanitizeJSBlock = strip . tail . tail . init . init

strip  = T.unpack . T.strip . T.pack

--type AlexAction result = AlexInput -> Int -> Alex result
mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (posn, prevChar, pending, input) len = return $ Token (makePos posn) token
  where token = trace (show $ tokenizer (take len input)) (tokenizer (take len input))

makePos :: AlexPosn -> Pos
makePos (AlexPn a l c) = Pos a l c

tokenToPos :: Token -> Pos
tokenToPos (Token x _) = x

data Token = Token Pos TokenClass deriving (Eq, Show)

data Pos = Pos Int Int Int deriving (Eq, Show)

data TokenClass
 = TokenConst
 | TokenInt  String
 | TokenStr  String
 | TokenName String
 | TokenJSBlock String
 | TokenBool String
 | TokenIf
 | TokenEq
 | TokenPlus
 | TokenDash
 | TokenStar
 | TokenSlash
 | TokenTripleEq
 | TokenComa
 | TokenLeftCurly
 | TokenRightCurly
 | TokenLeftParen
 | TokenRightParen
 | TokenDoubleColon
 | TokenColon
 | TokenDot
 | TokenArrow
 | TokenFatArrow
 | TokenEOF
 | TokenImport
 | TokenExport
 | TokenFrom
 | TokenPipe
 | TokenData
 | TokenSemiColon
 | TokenReturn
 | TokenJSBlockLeft
 | TokenJSBlockRight
 deriving (Eq, Show)


strV :: Token -> String
strV (Token _ (TokenStr x))  = x
strV (Token _ (TokenInt x)) = x
strV (Token _ (TokenBool x)) = x
strV (Token _ (TokenName x)) = x
strV (Token _ (TokenJSBlock x)) = x


-- intV :: Token -> Int
-- intV (Token _ (TokenInt x)) = x

-- boolV :: Token -> Bool
-- boolV (Token _ (TokenBool x)) = x

alexEOF :: Alex Token
alexEOF = return (Token (Pos 1 1 1) TokenEOF)
}
