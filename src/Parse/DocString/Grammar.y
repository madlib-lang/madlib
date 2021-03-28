{
module Parse.DocString.Grammar where

import           Text.Printf
import           Control.Monad.Except
import           Data.Char(isSpace)

import           Parse.DocString.Lexer
import           Infer.Type
import           Parse.DocString.DocString
import           Explain.Location
import           Explain.Meta
import           Target
import           Text.Regex.TDFA
import           Debug.Trace (trace)
import           Text.Show.Pretty (ppShow)
}

%name parseDocStrings docs
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexerWrap } { TokenEOF }

%token
  'moduleStart'    { TokenModuleDocStringStart }
  'functionStart'  { TokenFunctionDocStringStart _ }
  'end'            { TokenDocStringEnd }
  'moduleDescPart' { TokenModuleDescriptionCharacter _ }

%%

docs :: { [DocString] }
  : doc { [$1] }
  | doc docs { $1 : $2 }

doc :: { DocString }
  : 'moduleStart' parts 'end'   { ModuleDoc $ processDescriptionCharacters $2 }
  | 'functionStart' parts 'end' { FunctionDoc (getFunctionName $1) (processDescriptionCharacters $2) }

parts :: { [String] }
  : 'moduleDescPart'       { [getModuleDescCharacter $1] }
  | 'moduleDescPart' parts { getModuleDescCharacter $1 : $2 }


{
toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

regex :: String
regex = "[\n\\ ]*\\*[ ]*"

sanitizeDescription :: String -> String
sanitizeDescription desc =
  let matched = match (toRegex regex) desc :: (String, String, String)
  in  case matched of
    (before, "", "") -> trim desc
    (before, _, after) -> sanitizeDescription $ before <> "\n" <> after


processDescriptionCharacters :: [String] -> String
processDescriptionCharacters chars =
  let assembled = foldl (<>) "" chars
  in  sanitizeDescription assembled


lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap f = alexMonadScan >>= f

parseError :: Token -> Alex a
parseError token =
  alexError $ "invalid token: " <> ppShow token

parse :: String -> Either String [DocString]
parse s = runAlex s parseDocStrings
}
