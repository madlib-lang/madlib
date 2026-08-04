-- | Terminal projection of the diagnostic IR: converts a 'Diagnostic' to an
-- Error.Diagnose report and pretty-prints it with source excerpts, underlines
-- and (optionally) ANSI colors.
module Explain.Render.Terminal
  ( toReport
  , renderDiagnostic
  ) where

import qualified Error.Diagnose                as Diagnose
import qualified Prettyprinter                 as Pretty
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified Data.Text                     as Text

import           Explain.Diagnostic
import           Explain.Location
import           Explain.Render.Text            ( renderSections )


-- | Projects a 'Diagnostic' onto a Diagnose report. The Bool enables ANSI
-- colors inside the rendered messages (type diffs, kind highlights).
toReport :: Bool -> Diagnostic -> Diagnose.Report String
toReport color d =
  let mkReport = case dSeverity d of
        SevError   -> Diagnose.Err
        SevWarning -> Diagnose.Warn
      -- A diagnostic without any usable source location renders its body as
      -- part of the title, as there is no marker to attach it to.
      title =
        if null (dMarkers d) && not (null (dBody d)) then
          dTitle d <> "\n\n" <> renderSections color (dBody d)
        else
          dTitle d
      toMarker (Marker (Span path (Area (Loc _ startL startC) (Loc _ endL endC))) style label) =
        ( Diagnose.Position (startL, startC) (endL, endC) path
        , case style of
            Primary   -> Diagnose.This (renderSections color label)
            Secondary -> Diagnose.Where (renderSections color label)
        )
      toNote n = case n of
        Hint sections -> Diagnose.Hint (renderSections color sections)
        Note sections -> Diagnose.Note (renderSections color sections)
  in  mkReport Nothing title (toMarker <$> dMarkers d) (toNote <$> dNotes d)


-- | Full terminal rendering of a diagnostic: attaches the module source so
-- Diagnose can show code excerpts, lays the report out at 80 columns and
-- renders it to a string.
renderDiagnostic :: Bool -> FilePath -> String -> Diagnostic -> String
renderDiagnostic color modulePath moduleContent d =
  let report        = toReport color d
      diagnostic    = Diagnose.addFile Diagnose.def modulePath moduleContent
      diagnostic'   = Diagnose.addReport diagnostic report
      diagnosticDoc =
        if color then
          Diagnose.defaultStyle $ Diagnose.prettyDiagnostic True 2 diagnostic'
        else
          Pretty.unAnnotate $ Diagnose.prettyDiagnostic True 2 diagnostic'
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
  in  Text.unpack $ Terminal.renderStrict (Pretty.layoutPretty layoutOptions diagnosticDoc)
