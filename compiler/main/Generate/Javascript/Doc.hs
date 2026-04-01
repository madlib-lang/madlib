module Generate.Javascript.Doc where

import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.String as PrettyString


type JSDoc = Pretty.Doc ()


docText :: String -> JSDoc
docText = Pretty.pretty


docVsep :: [JSDoc] -> JSDoc
docVsep = Pretty.vsep


docRenderWithWidth :: Int -> JSDoc -> String
docRenderWithWidth width =
  let layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine width 1.0 }
  in  PrettyString.renderString . Pretty.layoutPretty layoutOptions


docRender :: JSDoc -> String
docRender = docRenderWithWidth 80


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
