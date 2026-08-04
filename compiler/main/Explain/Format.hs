{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
-- | Facade over the diagnostic pipeline: every compilation error or warning
-- is built once into a 'Explain.Diagnostic.Diagnostic' (see
-- "Explain.Diagnostic.Build") and projected into the requested output mode by
-- the renderers under @Explain.Render@:
--
--   * terminal (source excerpts, colors)  -> "Explain.Render.Terminal"
--   * plain text (simple \/ LSP)          -> "Explain.Render.Text"
--   * JSON                                -> "Explain.Render.Json"
module Explain.Format
  ( module Explain.Format
  , module Explain.Format.Hints
  , module Explain.Format.TypeDiff
  , jsonString
  ) where

import           Explain.Format.Hints
import           Explain.Format.TypeDiff
import           Explain.Diagnostic             ( Marker(..)
                                                , MarkerStyle(..)
                                                , dMarkers
                                                )
import           Explain.Diagnostic.Build       ( errorDiagnostic
                                                , warningDiagnostic
                                                )
import           Explain.Render.Json            ( renderJson
                                                , jsonString
                                                )
import           Explain.Render.Terminal        ( renderDiagnostic )
import           Explain.Render.Text            ( renderSimple
                                                , renderLsp
                                                , renderSections
                                                )
import           Error.Error
import           Error.Warning
import           Error.Context
import           Explain.Location
import qualified AST.Solved                    as Slv
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                )
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Maybe
import           System.Environment             ( lookupEnv )


underlineWhen :: Bool -> String -> String
underlineWhen when s | when      = "\x1b[4m" <> s <> "\x1b[0m"
                     | otherwise = s


getModuleContent :: (FilePath -> IO String) -> Context -> IO String
getModuleContent _ (Context "" _) =
  return ""
getModuleContent rf (Context modulePath _) =
  rf modulePath
getModuleContent _  _                        =
  return ""


isColorEnabledWhen :: Bool -> IO Bool
isColorEnabledWhen notJson = do
  noColor <- lookupEnv "NO_COLOR"
  return $ notJson && not (noColor /= Just "" && Maybe.isJust noColor)


formatWarning :: (FilePath -> IO String) -> Bool -> CompilationWarning -> IO String
formatWarning rf json (CompilationWarning warning ctx) = do
  isColorEnabled <- isColorEnabledWhen (not json)
  moduleContent  <- getModuleContent rf ctx
  return $ renderDiagnostic isColorEnabled (getCtxPath' ctx) moduleContent (warningDiagnostic ctx warning)


simpleFormatWarning :: Bool -> CompilationWarning -> IO String
simpleFormatWarning json (CompilationWarning warning ctx) = do
  isColorEnabled <- isColorEnabledWhen (not json)
  return $ renderSimple isColorEnabled (warningDiagnostic ctx warning)


-- | Format a warning with hints and notes included, for LSP display.
simpleFormatWarningWithHints :: Bool -> CompilationWarning -> IO String
simpleFormatWarningWithHints _ (CompilationWarning warning ctx) =
  return $ renderLsp (warningDiagnostic ctx warning)


formatError :: (FilePath -> IO String) -> Bool -> CompilationError -> IO String
formatError rf json err@(CompilationError typeErr ctx)
  | json = return $ formatErrorJson err
  | otherwise = do
      isColorEnabled <- isColorEnabledWhen True
      moduleContent  <- getModuleContent rf ctx
      return $ renderDiagnostic isColorEnabled (getCtxPath' ctx) moduleContent (errorDiagnostic ctx typeErr)


-- | Format a 'CompilationError' as a newline-delimited JSON object.
-- Suitable for machine consumption (e.g. CI tools, editor integrations).
formatErrorJson :: CompilationError -> String
formatErrorJson = renderJson


simpleFormatError :: Bool -> CompilationError -> IO String
simpleFormatError json (CompilationError err ctx) = do
  isColorEnabled <- isColorEnabledWhen (not json)
  return $ renderSimple isColorEnabled (errorDiagnostic ctx err)


-- | Format an error with hints and notes included, for LSP display.
-- Extracts the title, the marker messages, and all notes into a single string.
simpleFormatErrorWithHints :: Bool -> CompilationError -> IO String
simpleFormatErrorWithHints _ (CompilationError err ctx) =
  return $ renderLsp (errorDiagnostic ctx err)


-- | The markers (primary and secondary) of an error's diagnostic. Used by
-- the LSP layer to surface secondary markers (the other if-branch, an
-- annotation's location, an instance-chain declaration site, ...) as
-- relatedInformation entries pointing at their own span, instead of only
-- as prose flattened into the message.
diagnosticMarkers :: Context -> TypeError -> [Marker]
diagnosticMarkers ctx err = dMarkers (errorDiagnostic ctx err)


-- | Plain-text rendering of a single marker's label.
renderMarkerLabel :: Marker -> String
renderMarkerLabel = renderSections False . mLabel


-- computeLinesToShow : returns the first line and the last line to show
computeLinesToShow :: Area -> Area -> (Int, Int)
computeLinesToShow (Area (Loc _ l _) _) (Area (Loc _ l' _) _) = (l - 1, l' - 1)


formatHighlightArea :: Area -> String
formatHighlightArea (Area (Loc _ _ c) (Loc _ _ c')) =
  concat [ " " | _ <- [1 .. (c - 1)] ] <> concat [ "^" | _ <- [c .. (c' - 1)] ]


showAreaInSource :: Bool -> Area -> Area -> [String] -> String
showAreaInSource = showAreaInSource' 2 3

showAreaInSource' :: Int -> Int -> Bool -> Area -> Area -> [String] -> String
showAreaInSource' before after json start end code =
  let lines                    = [1 ..]
      (firstLine, lastLine)    = computeLinesToShow start end
      firstLineToShow          = max 0 (firstLine - before)
      lastLineToShow           = lastLine + after
      amountCharsForLineNumber = length $ show lastLineToShow
      prettyPrintedLineNumbers =
          (\n ->
              let asStr       = show n
                  spacesToAdd = amountCharsForLineNumber - length asStr
              in  replicate spacesToAdd ' ' <> asStr <> "|"
            )
            <$> lines
      before' = (\(lNum, line) -> colorWhen (not json) Grey $ lNum <> line)
        <$> slice firstLineToShow (firstLine - 1) (zip prettyPrintedLineNumbers code)
      expContent = uncurry (<>) <$> slice firstLine lastLine (zip prettyPrintedLineNumbers code)
      after'      = (\(lNum, line) -> colorWhen (not json) Grey $ lNum <> line)
        <$> slice (lastLine + 1) lastLineToShow (zip prettyPrintedLineNumbers code)
      (Area (Loc x line col) (Loc _ line' col')) = end
      endCol                = if line == line' then col' else col + 1
      highlightArea         = Area (Loc x line col) (Loc x line endCol)
      spacesBeforeHighlight = " " <> concat (" " <$ show lastLineToShow)
      formattedArea         = spacesBeforeHighlight <> formatHighlightArea highlightArea
  in  unlines $ before' ++ expContent ++ [formattedArea] ++ after'



removeNamespace :: String -> String
removeNamespace name =
  if "." `isInfixOf` name then
    reverse . takeWhile (/= '.') . reverse $ name
  else
    name


prettyPrintTyping :: Slv.Typing -> String
prettyPrintTyping t@(Slv.Untyped _ typing) = case typing of
  Slv.TRComp _ ts ->
    if not (null ts) then
      "(" <> prettyPrintTyping' False t <> ")"
    else
      prettyPrintTyping' False t

  Slv.TRArr _ _ ->
    "(" <> prettyPrintTyping' False t <> ")"

  _ -> prettyPrintTyping' True t
prettyPrintTyping Slv.Typed{} = undefined


prettyPrintTyping' :: Bool -> Slv.Typing -> String
prettyPrintTyping' _ Slv.Typed{} = undefined
prettyPrintTyping' paren (Slv.Untyped _ typing) = case typing of
  Slv.TRSingle n ->
    removeNamespace n

  Slv.TRComp n typing' ->
    let space = if not (null typing') then " " else ""
    in  if paren then
      "("
      <> removeNamespace n
      <> space
      <> unwords ((\t -> prettyPrintTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')
      <> ")"
    else
      removeNamespace n
      <> space
      <> unwords ((\t -> prettyPrintTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')

  Slv.TRArr (Slv.Untyped _ (Slv.TRArr l r)) r' ->
    "("
      <> prettyPrintTyping' False l
      <> " -> "
      <> prettyPrintTyping' False r
      <> ") -> "
      <> prettyPrintTyping' False r'

  Slv.TRArr l r ->
    if paren then
      "(" <> prettyPrintTyping' False l <> " -> " <> prettyPrintTyping' False r <> ")"
    else
      prettyPrintTyping' False l <> " -> " <> prettyPrintTyping' False r

  Slv.TRTuple ts ->
    "#[" <> intercalate ", " (prettyPrintTyping' False <$> ts) <> "]"

  Slv.TRRecord ts _ ->
    let mapped  = M.mapWithKey (\k v -> k <> " :: " <> prettyPrintTyping' False v) (snd <$> ts)
        fields  = M.elems mapped
        fields' = intercalate ", " fields
    in  "{ " <> fields' <> " }"


isTRArrOrTRCompWithArgs :: Slv.Typing -> Bool
isTRArrOrTRCompWithArgs (Slv.Typed _ _ _) = undefined
isTRArrOrTRCompWithArgs (Slv.Untyped _ typing) = case typing of
  Slv.TRArr  _ _  ->
    True

  Slv.TRComp _ ts ->
    not (null ts)

  _               ->
    False


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
