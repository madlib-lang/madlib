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


-- | Structured parse error for clean integration with Error.Error
data ParseError
  = ParseBadEscape Area
  | ParseEmptyChar Area
  | ParseSyntaxError Int Int String  -- line, col, message
  deriving (Show)


-- | Parse a Madlib source string, returning either an error string or the AST
-- This matches the API of Parse.Madlib.Grammar.parse
parse :: String -> Either String Src.AST
parse s = case runMadlibParser pAST "<input>" (TE.encodeUtf8 (T.pack s)) of
  Right ast -> Right ast
  Left err  -> Left (formatError err)


-- | Parse a Madlib source string in formatter mode
-- This matches the API of Parse.Madlib.Grammar.parseForFormatter
parseForFormatter :: String -> Either String Src.AST
parseForFormatter s = case runMadlibParserForFormatter pAST "<input>" (TE.encodeUtf8 (T.pack s)) of
  Right ast -> Right ast
  Left err  -> Left (formatError err)


-- | Parse with structured error, eliminating the string-parsing hack in AST.hs
parseWithStructuredError :: String -> Either ParseError Src.AST
parseWithStructuredError s = case runMadlibParser pAST "<input>" (TE.encodeUtf8 (T.pack s)) of
  Right ast -> Right ast
  Left err  -> Left (extractStructuredError err)


-- | Parse for formatter with structured error
parseForFormatterWithStructuredError :: String -> Either ParseError Src.AST
parseForFormatterWithStructuredError s = case runMadlibParserForFormatter pAST "<input>" (TE.encodeUtf8 (T.pack s)) of
  Right ast -> Right ast
  Left err  -> Left (extractStructuredError err)


-- | Parse directly from ByteString (avoids String->ByteString conversion)
parseWithStructuredErrorBS :: BS.ByteString -> Either ParseError Src.AST
parseWithStructuredErrorBS bs = case runMadlibParser pAST "<input>" bs of
  Right ast -> Right ast
  Left err  -> Left (extractStructuredError err)


-- | Parse for formatter directly from ByteString
parseForFormatterWithStructuredErrorBS :: BS.ByteString -> Either ParseError Src.AST
parseForFormatterWithStructuredErrorBS bs = case runMadlibParserForFormatter pAST "<input>" bs of
  Right ast -> Right ast
  Left err  -> Left (extractStructuredError err)


-- | Extract structured error from a parse error bundle
extractStructuredError :: ParseErrorBundle BS.ByteString CustomError -> ParseError
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


-- | Parse with error recovery, returning a partial AST and accumulated recovery errors.
-- Used only in LSP mode to provide tooling even when the file has syntax errors.
parseWithRecoveryBS :: BS.ByteString -> (Either ParseError Src.AST, [ParseRecoveryError])
parseWithRecoveryBS bs =
  let (result, finalState) = runMadlibParserWithState pASTWithRecovery "<input>" bs
  in  case result of
        Right ast -> (Right ast, psRecoveryErrors finalState)
        Left err  -> (Left (extractStructuredError err), psRecoveryErrors finalState)


-- | Format a parse error bundle into a human-readable string
formatError :: ParseErrorBundle BS.ByteString CustomError -> String
formatError = errorBundlePretty
