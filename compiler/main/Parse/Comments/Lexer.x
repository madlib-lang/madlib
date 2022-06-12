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

module Parse.Comments.Lexer
  ( Alex(..)
  , AlexState(..)
  , alexEOF
  , Token(..)
  , Loc(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToArea
  , parseComments
  , Comment(..)
  , getCommentArea
  )
where

import           Control.Monad.State
import           System.Exit
import qualified Data.Text.Lazy     as T
import           Data.Char
import           Explain.Location
import           Text.Regex.TDFA
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
}

%wrapper "monadUserState"

$alpha    = [a-zA-Z]
$digit    = 0-9
$head     = [\ \n]
$tail     = [\n]


@head = [\ ]*[\n]?[\ ]*

tokens :-
  <0>              \/\*                          { beginComment }
  <comment>        [.\n]                         { pushCommentChar }
  <comment>        \*\/                          { endComment }
  <0>              \#\- ([$alpha $digit \" \_ \' \` \$ \ \+ \- \* \. \, \( \) \; \: \{ \} \[ \] \! \? \| \& \n \= \< \> \\ \/\^]|\\\#)* \-\# ;
  <0>              \"(($printable # \")|\\\")*\" ;
  <0>              '(($printable # ')|\\')*'     ;
  <0>              `                             { beginStringTemplate }
  <stringTemplate> `                             { endStringTemplate }
  <stringTemplate> [.\n]                         ;
  <0>              "//"[^\n]*                    { mapToken (\s -> TokenComment s) }
  <0>              (.|\n)                        ;
{

data Comment
  = Comment Area String
  | MultilineComment Area String
  deriving(Eq, Show)


getCommentArea :: Comment -> Area
getCommentArea comment = case comment of
  Comment area _ ->
    area

  MultilineComment area _ ->
    area


tokenToComment :: Token -> Comment
tokenToComment token = case token of
  Token area (TokenCommentMultiline s) ->
    MultilineComment area s

  Token area (TokenComment s) ->
    Comment area s

parseComments :: String -> Either String [Comment]
parseComments str = runAlex str runNext


runNext :: Alex [Comment]
runNext = do
  token@(Token _ cls) <- alexMonadScan
  if cls == TokenEOF
    then return []
    else (tokenToComment token :) <$> runNext


data AlexUserState = AlexUserState Int String Loc deriving(Eq, Show)


alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 "" (Loc 0 0 0)


setStartCode :: Int -> Alex ()
setStartCode code = do
  sc <- getStartCode
  Alex $ \s -> Right (s{ alex_scd = code }, ())


getStartCode :: Alex Int
getStartCode = Alex $ \s -> do
  let sc = alex_scd s
  Right (s, sc)


getState :: Alex AlexUserState
getState = Alex $ \s@AlexState{alex_ust = state} -> Right (s, state)

setState :: AlexUserState -> Alex ()
setState state = Alex $ \s -> Right (s{ alex_ust = state }, ())


beginStringTemplate :: AlexInput -> Int -> Alex Token
beginStringTemplate input len = do
  setStartCode stringTemplate
  skip input len


endStringTemplate :: AlexInput -> Int -> Alex Token
endStringTemplate input len = do
  setStartCode 0
  skip input len


beginComment :: AlexInput -> Int -> Alex Token
beginComment input@((AlexPn a l c), _, _, _) len = do
  setStartCode comment
  (AlexUserState commentDepth commentBuffer loc) <- getState
  let loc' =
        if commentDepth > 0 then
          loc
        else
          Loc a l c
  setState (AlexUserState (commentDepth + 1) (commentBuffer ++ "/*") loc')
  skip input len


endComment :: AlexInput -> Int -> Alex Token
endComment input@((AlexPn a l c), _, _, _) len = do
  (AlexUserState commentDepth commentBuffer loc) <- getState
  let newDepth = commentDepth - 1
  if newDepth == 0 then do
    setState (AlexUserState newDepth "" (Loc 0 0 0))
    setStartCode 0
    return $ Token (Area loc (Loc (a + 2) l (c + 2))) (TokenCommentMultiline (commentBuffer ++ "*/"))
  else do
    setState (AlexUserState newDepth (commentBuffer ++ "*/") loc)
    skip input len

pushCommentChar :: AlexInput -> Int -> Alex Token
pushCommentChar (posn, prevChar, pending, input) len = do
  (AlexUserState commentDepth commentBuffer loc) <- getState
  setState (AlexUserState commentDepth (commentBuffer ++ take len input) loc)
  skip input len

mapToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mapToken tokenizer (pos, _, _, input) len =
  let chars = take len input
      area  = makeArea pos chars
  in  return $ Token area (tokenizer chars)

computeTokenOffsetFromLeadingWhiteChars :: (Int, Int, Int) -> String -> (Int, Int, Int)
computeTokenOffsetFromLeadingWhiteChars initialLoc input =
  let leadingWhites = takeWhile isSpace input
  in  computeTokenOffsetFromLeadingWhiteChars' initialLoc leadingWhites

computeTokenOffsetFromLeadingWhiteChars' :: (Int, Int, Int) -> [Char] -> (Int, Int, Int)
computeTokenOffsetFromLeadingWhiteChars' prev@(prevAbs, prevL, prevC) whites =
  case whites of
    ('\n':cs) ->
      let (computedAbs, computedL, computedC) = (prevAbs + 1, prevL + 1, 1)
      in  computeTokenOffsetFromLeadingWhiteChars' (computedAbs, computedL, computedC) cs
    (_:cs)    ->
      let (computedAbs, computedL, computedC) = (prevAbs + 1, prevL, prevC + 1)
      in  computeTokenOffsetFromLeadingWhiteChars' (computedAbs, computedL, computedC) cs
    _         -> prev


makeArea :: AlexPosn -> String -> Area
makeArea (AlexPn a l c) tokenContent =
  let (a', l', c')  = computeTokenOffsetFromLeadingWhiteChars (a, l, c) tokenContent
      start         = Loc a' l' c'
      contentLines  = lines tokenContent
      lastLine      = last contentLines
      numberOfLines = length contentLines
      end           = if numberOfLines > 1
                      then Loc (a + length tokenContent) (l + numberOfLines - 1) (length lastLine + 1)
                      else Loc (a + length tokenContent) l (c + length tokenContent)
  in  Area start end

tokenToArea :: Token -> Area
tokenToArea (Token area _) = area

data Token = Token Area TokenClass deriving (Eq, Show)

data TokenClass
 = TokenCommentMultiline String
 | TokenComment String
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return (Token (Area (Loc 1 1 1) (Loc 1 1 1)) TokenEOF)
}
