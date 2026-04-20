module Generate.Javascript.SourceMap
  ( Mapping(..)
  , buildSourceMapJSON
  , base64Encode
  ) where

import Data.Bits  (shiftL, shiftR, (.&.), (.|.))
import Data.List  (intercalate, sortOn)
import Data.Char  (ord, chr)


-- | A single source mapping entry.
data Mapping = Mapping
  { mappingGenLine  :: Int  -- 0-based output line
  , mappingGenCol   :: Int  -- 0-based output column (0 for line-start mappings)
  , mappingSrcLine  :: Int  -- 0-based source line
  , mappingSrcCol   :: Int  -- 0-based source column
  } deriving (Eq, Show)


-- ---------------------------------------------------------------------------
-- VLQ encoding (Base64 VLQ, Source Map v3)
-- ---------------------------------------------------------------------------

base64Chars :: String
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | Encode a single integer as Base64-VLQ.
encodeVLQ :: Int -> String
encodeVLQ n =
  let signed = if n < 0 then ((-n) `shiftL` 1) .|. 1 else n `shiftL` 1
  in  go signed
  where
    go v =
      let chunk = v .&. 0x1F
          rest  = v `shiftR` 5
          digit = if rest > 0 then chunk .|. 0x20 else chunk
      in  [base64Chars !! digit] <> if rest > 0 then go rest else ""


-- ---------------------------------------------------------------------------
-- Mappings string builder
-- ---------------------------------------------------------------------------

-- | Encode a list of Mapping entries into the Source Map v3 'mappings' string.
--
-- Source map v3 delta-encoding rules:
--   - Generated column resets to 0 at the start of each output line.
--   - Source file index, source line, and source column are cumulative
--     across ALL segments in ALL lines (state is NOT reset per line).
encodeMappings :: [Mapping] -> String
encodeMappings [] = ""
encodeMappings mappings =
  let sorted  = sortOn (\m -> (mappingGenLine m, mappingGenCol m)) mappings
      maxLine = mappingGenLine (last sorted)
      byLine  = [ filter (\m -> mappingGenLine m == l) sorted | l <- [0..maxLine] ]
  in  go 0 0 0 byLine
  where
    -- prevSrcLine and prevSrcCol persist across output lines.
    go :: Int -> Int -> Int -> [[Mapping]] -> String
    go _prevSrcLine _prevSrcCol _prevSrcFile [] = ""
    go prevSrcLine prevSrcCol prevSrcFile (line : rest) =
      let (lineStr, prevSrcLine', prevSrcCol') =
            encodeLineSegments prevSrcLine prevSrcCol line
      in  lineStr <> (if null rest then "" else ";") <> go prevSrcLine' prevSrcCol' prevSrcFile rest

encodeLineSegments :: Int -> Int -> [Mapping] -> (String, Int, Int)
encodeLineSegments prevSrcLine prevSrcCol ms =
  let go _prevGenCol psl psc [] = ("", psl, psc)
      go prevGenCol psl psc (m : rest) =
        let dGenCol  = mappingGenCol m  - prevGenCol
            dSrcLine = mappingSrcLine m - psl
            dSrcCol  = mappingSrcCol m  - psc
            seg      = encodeVLQ dGenCol
                    <> encodeVLQ 0          -- source file index (always 0)
                    <> encodeVLQ dSrcLine
                    <> encodeVLQ dSrcCol
            (restStr, psl', psc') = go (mappingGenCol m) (mappingSrcLine m) (mappingSrcCol m) rest
            sep      = if null rest then "" else ","
        in  (seg <> sep <> restStr, psl', psc')
  in  go 0 prevSrcLine prevSrcCol ms


-- ---------------------------------------------------------------------------
-- JSON serialization
-- ---------------------------------------------------------------------------

jsonStr :: String -> String
jsonStr s = "\"" <> concatMap escape s <> "\""
  where
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c    = [c]

-- | Build a Source Map v3 JSON string.
--
-- outputFile    — basename of the generated .mjs file (e.g. "Foo.mjs")
-- sourceFile    — path to the original .mad source, relative to the .mjs output dir
-- mappings      — list of Mapping entries
-- mSourceContent — optional original source text to embed as sourcesContent
buildSourceMapJSON :: String -> String -> [Mapping] -> Maybe String -> String
buildSourceMapJSON outputFile sourceFile mappings mSourceContent =
  let mappingsStr    = encodeMappings mappings
      sourcesContent = case mSourceContent of
        Nothing  -> ""
        Just src -> ",\n  \"sourcesContent\": [" <> jsonStr src <> "]"
  in  "{\n"
   <> "  \"version\": 3,\n"
   <> "  \"file\": " <> jsonStr outputFile <> ",\n"
   <> "  \"sources\": [" <> jsonStr sourceFile <> "]"
   <> sourcesContent <> ",\n"
   <> "  \"names\": [],\n"
   <> "  \"mappings\": " <> jsonStr mappingsStr <> "\n"
   <> "}\n"


-- ---------------------------------------------------------------------------
-- Pure Base64 encoding (RFC 4648, no line wrapping)
-- ---------------------------------------------------------------------------

base64Table :: String
base64Table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | Base64-encode a String (treating each Char as a byte).
base64Encode :: String -> String
base64Encode = encode . map ord
  where
    encode [] = ""
    encode [a] =
      let v = a `shiftL` 16
      in  [base64Table !! ((v `shiftR` 18) .&. 63)
          ,base64Table !! ((v `shiftR` 12) .&. 63)
          ,'='
          ,'=']
    encode [a, b] =
      let v = (a `shiftL` 16) .|. (b `shiftL` 8)
      in  [base64Table !! ((v `shiftR` 18) .&. 63)
          ,base64Table !! ((v `shiftR` 12) .&. 63)
          ,base64Table !! ((v `shiftR`  6) .&. 63)
          ,'=']
    encode (a : b : c : rest) =
      let v = (a `shiftL` 16) .|. (b `shiftL` 8) .|. c
      in  [base64Table !! ((v `shiftR` 18) .&. 63)
          ,base64Table !! ((v `shiftR` 12) .&. 63)
          ,base64Table !! ((v `shiftR`  6) .&. 63)
          ,base64Table !! ( v              .&. 63)]
          <> encode rest
