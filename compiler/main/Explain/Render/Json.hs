-- | JSON projection of the diagnostic IR: a newline-delimited JSON object
-- suitable for machine consumption (e.g. CI tools, editor integrations).
module Explain.Render.Json
  ( renderJson
  , jsonString
  ) where

import qualified Data.List                     as List
import           Data.Char                      ( ord )
import           Numeric                        ( showHex )

import           Error.Error
import           Error.Context
import           Explain.Location
import           Explain.Diagnostic.Build       ( errorDiagnostic )
import           Explain.Render.Text            ( renderSimple )


-- | Format a 'CompilationError' as a newline-delimited JSON object.
renderJson :: CompilationError -> String
renderJson (CompilationError typeErr ctx) =
  let path    = getCtxPath' ctx
      (line, col) = case ctx of
        Context _ (Area (Loc _ l c) _) -> (l, c)
        _                              -> (0, 0)
      msg = renderSimple False (errorDiagnostic ctx typeErr)
      -- Remove newlines from message for JSON single-line embedding
      msgOneLine = List.intercalate "\\n" (lines msg)
  in  "{\"type\":\"error\""
      <> ",\"file\":" <> jsonString path
      <> ",\"line\":" <> show line
      <> ",\"col\":" <> show col
      <> ",\"message\":" <> jsonString msgOneLine
      <> "}"


-- | Escape a string for JSON embedding.
jsonString :: String -> String
jsonString s = "\"" <> concatMap escapeChar s <> "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | ord c < 0x20 = let h = showHex (ord c) ""
                           padded = replicate (4 - length h) '0' <> h
                       in  "\\u" <> padded
      | otherwise    = [c]
