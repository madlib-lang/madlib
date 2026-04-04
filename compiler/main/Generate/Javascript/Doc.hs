module Generate.Javascript.Doc where

import qualified Prettyprinter                 as Pretty
import qualified Prettyprinter.Render.String   as PrettyString
import qualified Data.Text                     as T
import           Explain.Location               (Area(..), Loc(..), emptyArea,
                                                 getLineFromStart, getColumnFromStart)
import           Generate.Javascript.SourceMap  (Mapping(..))


-- JSDoc carries source-location annotations so we can build source maps
-- without a separate pass.  Unannotated nodes carry emptyArea.
type JSDoc = Pretty.Doc Area


-- | Attach a source location to a doc node.
docAnnotate :: Area -> JSDoc -> JSDoc
docAnnotate = Pretty.annotate


docText :: String -> JSDoc
docText = Pretty.pretty


docVsep :: [JSDoc] -> JSDoc
docVsep = Pretty.vsep


-- | Render to a plain String, discarding all source-map annotations.
-- Note: PrettyString.renderString ignores annotations, so unAnnotate is not needed.
docRenderWithWidth :: Int -> JSDoc -> String
docRenderWithWidth width doc =
  let layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine width 1.0 }
      stream = Pretty.layoutPretty layoutOptions doc
  in  PrettyString.renderString stream


docRender :: JSDoc -> String
docRender = docRenderWithWidth 80


-- | Render to a String AND collect source-map Mapping entries.
--
-- Strategy: walk the SimpleDocStream tracking a stack of active Area
-- annotations.  Whenever we are about to emit text and the innermost
-- non-empty area has changed since the last mapping was emitted, we
-- record a new Mapping at the current (line, col) position pointing
-- to that area's source location.
--
-- This gives us the most-specific (innermost) annotation per token,
-- which is what source-map consumers expect.
renderWithMappings :: Int -> JSDoc -> (String, [Mapping])
renderWithMappings width doc =
  let layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine width 1.0 }
      stream = Pretty.layoutPretty layoutOptions doc
      (str, revMappings) = go 0 0 [] Nothing stream id []
  in  (str, reverse revMappings)
  where
    -- State:
    --   line, col        : current output position (0-based)
    --   annStack         : stack of active Area annotations (head = innermost)
    --   lastEmittedArea  : the area we last emitted a Mapping for
    --   stream           : remaining SimpleDocStream
    --   accStr           : accumulated output string (ShowS)
    --   accMap           : accumulated mappings (reversed)
    go :: Int -> Int
       -> [Area]          -- annotation stack (innermost first)
       -> Maybe Area      -- last area we emitted a mapping for
       -> Pretty.SimpleDocStream Area
       -> ShowS
       -> [Mapping]
       -> (String, [Mapping])
    go line col stack lastArea stream accStr accMap = case stream of
      Pretty.SFail ->
        (accStr "", accMap)

      Pretty.SEmpty ->
        (accStr "", accMap)

      Pretty.SChar c rest ->
        let (accStr', accMap') = emitTextMapping line col stack lastArea accStr accMap
            (line', col') = advanceChar line col c
        in  go line' col' stack (currentArea stack) rest (accStr' . (c :)) accMap'

      Pretty.SText _len t rest ->
        let s = T.unpack t
            (accStr', accMap') = emitTextMapping line col stack lastArea accStr accMap
            (line', col') = advanceStr line col s
        in  go line' col' stack (currentArea stack) rest (accStr' . (s ++)) accMap'

      Pretty.SLine indent rest ->
        let spaces = replicate indent ' '
        in  go (line + 1) indent stack lastArea rest
               (accStr . ('\n' :) . (spaces ++))
               accMap

      Pretty.SAnnPush area rest ->
        go line col (area : stack) lastArea rest accStr accMap

      Pretty.SAnnPop rest ->
        go line col (drop 1 stack) lastArea rest accStr accMap

    -- Get the innermost non-empty area from the stack.
    currentArea :: [Area] -> Maybe Area
    currentArea [] = Nothing
    currentArea (a : rest)
      | a == emptyArea = currentArea rest
      | otherwise      = Just a

    -- Before emitting text: if the current innermost area differs from
    -- the last one we emitted a mapping for, record a new mapping.
    emitTextMapping :: Int -> Int -> [Area] -> Maybe Area -> ShowS -> [Mapping] -> (ShowS, [Mapping])
    emitTextMapping line col stack lastArea accStr accMap =
      let cur = currentArea stack
      in  if cur == lastArea
          then (accStr, accMap)
          else case cur >>= mkMapping line col of
                 Nothing -> (accStr, accMap)
                 Just m  -> (accStr, m : accMap)

    mkMapping :: Int -> Int -> Area -> Maybe Mapping
    mkMapping genLine genCol area
      | area == emptyArea = Nothing
      | otherwise =
          let srcLine = max 0 (getLineFromStart  area - 1)
              srcCol  = max 0 (getColumnFromStart area - 1)
          in  Just Mapping
                { mappingGenLine = genLine
                , mappingGenCol  = genCol
                , mappingSrcLine = srcLine
                , mappingSrcCol  = srcCol
                }

    advanceChar :: Int -> Int -> Char -> (Int, Int)
    advanceChar line _   '\n' = (line + 1, 0)
    advanceChar line col _    = (line, col + 1)

    advanceStr :: Int -> Int -> String -> (Int, Int)
    advanceStr line col []       = (line, col)
    advanceStr line col (c : cs) = advanceStr line' col' cs
      where (line', col') = advanceChar line col c


docHardline :: JSDoc
docHardline = Pretty.hardline

docNest :: Int -> JSDoc -> JSDoc
docNest = Pretty.nest

docGroup :: JSDoc -> JSDoc
docGroup = Pretty.group

docSoftline :: JSDoc
docSoftline = Pretty.softline

docLine :: JSDoc
docLine = Pretty.line

-- Nothing in flat mode, newline in break mode (no trailing space)
docLine' :: JSDoc
docLine' = Pretty.line'

docHang :: Int -> JSDoc -> JSDoc
docHang = Pretty.hang

docBlock :: [JSDoc] -> JSDoc
docBlock items =
  docText "{"
    <> docHardline
    <> Pretty.indent 2 (docVsep items)
    <> docHardline
    <> docText "}"


docFromLines :: [String] -> JSDoc
docFromLines = docVsep . fmap docText


rawDoc :: String -> JSDoc
rawDoc = docFromLines . lines
