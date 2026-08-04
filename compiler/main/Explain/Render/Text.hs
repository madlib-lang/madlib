-- | Plain-text projections of the diagnostic IR: the "simple" single-string
-- rendering used by the JSON output and simpleFormatError/simpleFormatWarning,
-- and the LSP rendering (title, marker labels and notes joined by newlines).
module Explain.Render.Text
  ( renderSections
  , renderSection
  , renderNoteText
  , renderSimple
  , renderLsp
  ) where

import           Data.List                      ( intercalate )
import           Explain.Diagnostic
import           Explain.Format.TypeDiff        ( renderTypesWithDiff
                                                , renderSchemesWithDiff
                                                , renderType
                                                , kindToStr
                                                , colorWhen
                                                , Color(Green, Red)
                                                )


-- | Renders the sections of a diagnostic body, marker label or note into a
-- single string. Sections are concatenated without separators: prose sections
-- carry their own spacing. The Bool enables ANSI colors for type diffs.
renderSections :: Bool -> [Section] -> String
renderSections color = concatMap (renderSection color)


renderSection :: Bool -> Section -> String
renderSection color section = case section of
  P s ->
    s

  Verbatim s ->
    s

  ShowType t ->
    intercalate "\n" $ ("  "<>) <$> lines (renderType t)

  ExpectedFound expected found ->
    let (foundPretty, expectedPretty) = renderTypesWithDiff color found expected
        found''     = intercalate "\n" $ ("  "<>) <$> lines foundPretty
        expected''  = intercalate "\n" $ ("  "<>) <$> lines expectedPretty
        expectedStr = if color then "\x1b[0mexpected:\n" else "expected:\n  "
        foundStr    = if color then "\n\x1b[0mbut found:\n" else "\nbut found:\n  "
    in  expectedStr <> expected'' <> foundStr <> found''

  GivenInferred scGiven scInferred ->
    let (scInferred', scGiven') = renderSchemesWithDiff color scInferred scGiven
        scGiven''    = unlines $ ("  "<>) <$> lines scGiven'
        scInferred'' = unlines $ ("  "<>) <$> lines scInferred'
        givenStr     = if color then "\x1b[0mType signature given:\n" else "Type signature given:\n  "
        inferredStr  = if color then "\n\x1b[0mType inferred:\n" else "\nType inferred:\n  "
    in  givenStr <> scGiven'' <> inferredStr <> scInferred''

  ExpectedFoundKind expectedKind actualKind ->
    let expectedStr = if color then "\x1b[0mexpected:\n  " else "expected:\n  "
        foundStr    = if color then "\n\x1b[0mbut found:\n  " else "\nbut found:\n  "
    in  expectedStr
        <> colorWhen color Green (kindToStr expectedKind)
        <> foundStr
        <> colorWhen color Red (kindToStr actualKind)


-- | A note as a single "Hint: ..." / "Note: ..." line block.
renderNoteText :: Bool -> Note -> String
renderNoteText color n = case n of
  Hint sections -> "Hint: " <> renderSections color sections
  Note sections -> "Note: " <> renderSections color sections


-- | Single-string rendering: title, main message and notes separated by blank
-- lines. Used for the JSON `message` field and the simple format functions.
renderSimple :: Bool -> Diagnostic -> String
renderSimple color d =
  let body = case dMarkers d of
        Marker _ _ label : _ ->
          renderSections color label

        [] ->
          renderSections color (dBody d)
      notes = intercalate "\n" (renderNoteText color <$> dNotes d)
  in  intercalate "\n\n" (filter (not . null) [dTitle d, body, notes])


-- | LSP rendering: title, every marker label and every note joined by
-- newlines, with empty parts filtered out. Always colorless.
renderLsp :: Diagnostic -> String
renderLsp d =
  let titlePart =
        if null (dMarkers d) && not (null (dBody d)) then
          dTitle d <> "\n\n" <> renderSections False (dBody d)
        else
          dTitle d
      markerTexts = renderSections False . mLabel <$> dMarkers d
      noteTexts   = renderNoteText False <$> dNotes d
  in  intercalate "\n" (filter (not . null) (titlePart : markerTexts <> noteTexts))
