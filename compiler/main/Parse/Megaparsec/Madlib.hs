{-# LANGUAGE OverloadedStrings #-}
module Parse.Megaparsec.Madlib
  ( parse
  , parseForFormatter
  , ParseError(..)
  , parseWithStructuredError
  , parseForFormatterWithStructuredError
  , parseWithStructuredErrorBS
  , parseForFormatterWithStructuredErrorBS
  , parseWithRecoveryBS
  ) where

import qualified Data.ByteString               as BS
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Data.List.NonEmpty             ( toList, NonEmpty(..) )
import qualified Data.Set                       as Set

import           Text.Megaparsec                ( errorBundlePretty, ParseErrorBundle, bundleErrors, parseErrorTextPretty )
import qualified Text.Megaparsec               as MP

import qualified AST.Source                      as Src
import           Explain.Location

import           Parse.Megaparsec.Common
import           Parse.Megaparsec.Error
import           Parse.Megaparsec.Declaration
import           Parse.Lexer.Lexer              ( scanMany )
import           Parse.Lexer.Token              ( Token(..), RangedToken(..) )
import           Parse.Lexer.TokenStream        ( TokenStream(..) )


-- | Structured parse error for clean integration with Error.Error
data ParseError
  = ParseBadEscape Area
  | ParseEmptyChar Area
  | ParseSyntaxError Int Int String  -- line, col, message
  | ParseLexError String             -- lexer error
  deriving (Show)


-- | Lex a ByteString, returning the token stream or an error
-- scanMany already excludes TkEOF tokens.
lexBS :: BS.ByteString -> Either ParseError TokenStream
lexBS bs = case scanMany bs of
  Left err -> Left (ParseLexError err)
  Right ts -> Right (TokenStream ts)


-- | Parse a Madlib source string, returning either an error string or the AST
parse :: String -> Either String Src.AST
parse s = case lexBS (TE.encodeUtf8 (T.pack s)) of
  Left (ParseLexError msg) -> Left ("Lex error: " ++ msg)
  Left err                 -> Left (show err)
  Right ts -> case runMadlibParser pAST "<input>" ts of
    Right ast -> Right ast
    Left err  -> Left (formatError err)


-- | Parse a Madlib source string in formatter mode
parseForFormatter :: String -> Either String Src.AST
parseForFormatter s = case lexBS (TE.encodeUtf8 (T.pack s)) of
  Left (ParseLexError msg) -> Left ("Lex error: " ++ msg)
  Left err                 -> Left (show err)
  Right ts -> case runMadlibParserForFormatter pAST "<input>" ts of
    Right ast -> Right ast
    Left err  -> Left (formatError err)


-- | Parse with structured error
parseWithStructuredError :: String -> Either ParseError Src.AST
parseWithStructuredError s = do
  ts <- lexBS (TE.encodeUtf8 (T.pack s))
  case runMadlibParser pAST "<input>" ts of
    Right ast -> Right ast
    Left err  -> Left (extractStructuredError err)


-- | Parse for formatter with structured error
parseForFormatterWithStructuredError :: String -> Either ParseError Src.AST
parseForFormatterWithStructuredError s = do
  ts <- lexBS (TE.encodeUtf8 (T.pack s))
  case runMadlibParserForFormatter pAST "<input>" ts of
    Right ast -> Right ast
    Left err  -> Left (extractStructuredError err)


-- | Parse directly from ByteString
parseWithStructuredErrorBS :: BS.ByteString -> Either ParseError Src.AST
parseWithStructuredErrorBS bs = do
  ts <- lexBS bs
  case runMadlibParser pAST "<input>" ts of
    Right ast -> Right ast
    Left err  -> Left (extractStructuredError err)


-- | Parse for formatter directly from ByteString
parseForFormatterWithStructuredErrorBS :: BS.ByteString -> Either ParseError Src.AST
parseForFormatterWithStructuredErrorBS bs = do
  ts <- lexBS bs
  case runMadlibParserForFormatter pAST "<input>" ts of
    Right ast -> Right ast
    Left err  -> Left (extractStructuredError err)


-- | Extract structured error from a parse error bundle
extractStructuredError :: ParseErrorBundle TokenStream CustomError -> ParseError
extractStructuredError bundle =
  let errs = toList (bundleErrors bundle)
  in  case errs of
    (err : _) -> case err of
      MP.FancyError _ errSet ->
        case [ e | MP.ErrorCustom e <- Set.toList errSet ] of
          (BadEscapeSequence area : _) -> ParseBadEscape area
          (EmptyCharLiteral area : _)  -> ParseEmptyChar area
          _ -> toSyntaxError bundle err
      _ -> toSyntaxError bundle err
    [] -> ParseSyntaxError 1 1 "Syntax error"
  where
    toSyntaxError bndl err =
      let (posErrs, _) = MP.attachSourcePos MP.errorOffset (err :| []) (MP.bundlePosState bndl)
      in  case toList posErrs of
            ((_, sp) : _) ->
              ParseSyntaxError
                (MP.unPos $ MP.sourceLine sp)
                (MP.unPos $ MP.sourceColumn sp)
                (parseErrorTextPretty err)
            [] ->
              ParseSyntaxError 1 1 (parseErrorTextPretty err)


-- | Parse with error recovery
parseWithRecoveryBS :: BS.ByteString -> (Either ParseError Src.AST, [ParseRecoveryError])
parseWithRecoveryBS bs =
  case lexBS bs of
    Left err -> (Left err, [])
    Right ts ->
      let (result, finalState) = runMadlibParserWithState pASTWithRecovery "<input>" ts
      in  case result of
            Right ast -> (Right ast, psRecoveryErrors finalState)
            Left err  -> (Left (extractStructuredError err), psRecoveryErrors finalState)


-- | Format a parse error bundle into a human-readable string
formatError :: ParseErrorBundle TokenStream CustomError -> String
formatError = errorBundlePretty
