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
import           Explain.Diagnostic
import           Explain.Diagnostic.Build       ( errorDiagnostic )
import           Explain.Render.Text            ( renderSimple
                                                , renderSections
                                                )


-- | Format a 'CompilationError' as a newline-delimited JSON object. Keeps
-- the original flat fields (type/file/line/col/message) for backward
-- compatibility with existing consumers, and adds structured fields
-- (title, spans with style/label, notes with kind, code) that let a richer
-- consumer (an editor extension, a lint aggregator) show more than the
-- flattened message string does — e.g. a secondary span on the other side
-- of an if/else mismatch, rendered as its own marker instead of buried in
-- prose.
renderJson :: CompilationError -> String
renderJson (CompilationError typeErr ctx) =
  let path    = getCtxPath' ctx
      (line, col) = case ctx of
        Context _ (Area (Loc _ l c) _) -> (l, c)
        _                              -> (0, 0)
      diagnostic = errorDiagnostic ctx typeErr
      msg = renderSimple False diagnostic
      -- Remove newlines from message for JSON single-line embedding
      msgOneLine = List.intercalate "\\n" (lines msg)
      codeField = case dCode diagnostic of
        Just code -> ",\"code\":" <> jsonString code
        Nothing   -> ",\"code\":null"
  in  "{\"type\":\"error\""
      <> ",\"file\":" <> jsonString path
      <> ",\"line\":" <> show line
      <> ",\"col\":" <> show col
      <> ",\"message\":" <> jsonString msgOneLine
      <> codeField
      <> ",\"title\":" <> jsonString (dTitle diagnostic)
      <> ",\"spans\":[" <> List.intercalate "," (map markerToJson (dMarkers diagnostic)) <> "]"
      <> ",\"notes\":[" <> List.intercalate "," (map noteToJson (dNotes diagnostic)) <> "]"
      <> "}"


markerToJson :: Marker -> String
markerToJson (Marker (Span path (Area (Loc _ sl sc) (Loc _ el ec))) style label) =
  "{\"file\":" <> jsonString path
  <> ",\"startLine\":" <> show sl
  <> ",\"startCol\":" <> show sc
  <> ",\"endLine\":" <> show el
  <> ",\"endCol\":" <> show ec
  <> ",\"style\":" <> jsonString (case style of { Primary -> "primary"; Secondary -> "secondary" })
  <> ",\"label\":" <> jsonString (renderSections False label)
  <> "}"


noteToJson :: Note -> String
noteToJson n =
  let (kind, sections) = case n of
        Hint ss -> ("hint", ss)
        Note ss -> ("note", ss)
  in  "{\"kind\":" <> jsonString kind
      <> ",\"text\":" <> jsonString (renderSections False sections)
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
