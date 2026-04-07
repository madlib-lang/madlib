{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Explain.Format (module Explain.Format, module Explain.Format.Hints, module Explain.Format.TypeDiff) where

import           Explain.Format.Hints
import           Explain.Format.TypeDiff
import           Error.Error
import           Error.Warning
import           Error.Context
import           Explain.Location
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Type
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                )
import qualified Data.Map                      as M
import           Text.Show.Pretty               ( ppShow )
import           Utils.Tuple
import qualified Data.Maybe                    as Maybe
import           System.Environment (lookupEnv)
import qualified Error.Diagnose                as Diagnose
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified Data.Text as Text
import qualified Prettyprinter as Pretty
import qualified Data.List as List
import Debug.Trace
import           Utils.EditDistance (findSimilar)
import           Data.Char (ord, toUpper, toLower)
import           Numeric (showHex)


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


formatWarning :: (FilePath -> IO String) -> Bool -> CompilationWarning -> IO String
formatWarning rf json (CompilationWarning warning ctx) = do
  noColor       <- lookupEnv "NO_COLOR"
  moduleContent <- getModuleContent rf ctx
  let isColorEnabled =  not json && not (noColor /= Just "" && Maybe.isJust noColor)
      modulePath     = getCtxPath' ctx
      report         = createWarningDiagnostic isColorEnabled ctx warning
      diagnostic     = Diagnose.addFile Diagnose.def modulePath moduleContent
      diagnostic'    = Diagnose.addReport diagnostic report
      diagnosticDoc  =
        if isColorEnabled then
          Diagnose.defaultStyle $ Diagnose.prettyDiagnostic True 2 diagnostic'
        else
          Pretty.unAnnotate $ Diagnose.prettyDiagnostic True 2 diagnostic'
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
      s = Terminal.renderStrict (Pretty.layoutPretty layoutOptions diagnosticDoc)

  return $ Text.unpack s


simpleFormatWarning :: Bool -> CompilationWarning -> IO String
simpleFormatWarning json (CompilationWarning warning ctx) = do
  noColor       <- lookupEnv "NO_COLOR"
  let isColorEnabled =  not json && not (noColor /= Just "" && Maybe.isJust noColor)
  return $ createSimpleWarningDiagnostic isColorEnabled ctx warning


createSimpleWarningDiagnostic :: Bool -> Context -> WarningKind -> String
createSimpleWarningDiagnostic _ _ warning = case warning of
  UnusedImport name path ->
    "Unused import\n\n"
    <> "You imported '" <> name <> "' from the module located at\n"
    <> "'" <> path <> "'\n"
    <> "but it seems that you never use it.\n\n"
    <> "Hint: Remove the import if you don't use it."

  MissingMethods missingMethods ->
    "Missing methods\n\n"
    <> "The instance does not implement all methods. The missing\n"
    <> "methods are the following:\n"
    <> intercalate "\n" (map ("  - "<>) missingMethods)
    <> "\n\n"
    <> "Hint: Implement the missing methods."

  UnusedParameter name ->
    "Unused parameter\n\n"
    <> "You declared a parameter named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove this parameter if you don't need it\n"
    <> "Hint: Use the skip parameter '_' to suppress this warning."

  UnusedDeclaration name ->
    "Unused variable\n\n"
    <> "You declared a variable named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  UnusedTopLevelDeclaration name ->
    "Unused top level binding\n\n"
    <> "You declared a top level binding named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  UnusedConstructor name ->
    "Unused constructor\n\n"
    <> "You declared a constructor named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  UnusedType name ->
    "Unused type\n\n"
    <> "You declared a type named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  MadlibVersionMinorTooLow pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  "Minor Madlib version is too low\n\n"
        <> start
        <> " requires the minimum version '"
        <> minVersion
        <> "' but you currently use madlib\n"
        <> "version '"
        <> versionUsed
        <> "'\n\n"
        <> "Hint: Update your version of madlib."

  MadlibVersionMajorDiffer pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  "Minor Madlib version is too low\n\n"
        <> start
        <> " requires the minimum version '"
        <> minVersion
        <> "' but you currently use madlib\n"
        <> "version '"
        <> versionUsed
        <> "'. Because major versions differ it means there is a breaking\n"
        <> "change and you may not be able to run the project\n\n"
        <> "Hint: Update your version of madlib."

  IncompletePattern missingPatterns ->
    "Incomplete pattern\n\n"
    <> "Examples of missing patterns:\n"
    <> intercalate "\n" (map ("  - "++) missingPatterns) <> "\n\n"
    <> "Note: If the input of where is not handled by a branch, it will most likely crash at\nruntime.\n"
    <> "Hint: Pattern match the missing constructors or add a catch all branch with '_ => ...'."

  RedundantPattern ->
    "Redundant pattern\n\n"
    <> "Note: This pattern will never be reached.\n"
    <> "Hint: Remove it or move it higher up so that it might be useful."

  TypedHoleFound t suggestions ->
    let (renderedType, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t))
        indentedType = unlines $ ("  "<>) <$> lines renderedType
        suggestionsSection =
          if null suggestions then ""
          else
            "\nSuggestions (in scope):\n"
            <> unlines (map renderSuggestion suggestions)
        renderSuggestion (name, sc) =
          "  " <> name <> " :: " <> renderSchemeOneLine sc
    in  "Typed hole\n\n"
        <> "I found a typed hole with type:\n" <> indentedType
        <> suggestionsSection
        <> "\nNote: This will crash at runtime if reached.\n"
        <> "Hint: Replace it with a valid expression of that type."


createWarningDiagnostic :: Bool -> Context -> WarningKind -> Diagnose.Report String
createWarningDiagnostic _ context warning = case warning of
  UnusedImport name path ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused import"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You imported '" <> name <> "' from the module located at\n"
                <> "'" <> path <> "'\n"
                <> "but it seems that you never use it."
            )
          ]
          [Diagnose.Hint "Remove the import if you don't use it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused import"
          []
          [Diagnose.Hint "Remove the import if you don't use it."]

  MissingMethods missingMethods ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Missing methods"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The instance does not implement all methods. The missing\n"
                <> "methods are the following:\n"
                <> intercalate "\n" (map ("  - "<>) missingMethods)
            )
          ]
          [Diagnose.Hint "Implement the missing methods."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Missing methods"
          []
          [Diagnose.Hint "Implement the missing methods."]

  UnusedParameter name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused parameter"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a parameter named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [ Diagnose.Hint "Remove this parameter if you don't need it"
          , Diagnose.Hint "Use the skip parameter '_' to suppress this warning."
          ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused parameter"
          []
          [ Diagnose.Hint "Remove this parameter if you don't need it"
          , Diagnose.Hint "Use the skip parameter '_' to suppress this warning."
          ]

  UnusedDeclaration name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused variable"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a variable named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused variable"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  UnusedTopLevelDeclaration name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused top level binding"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a top level binding named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused top level binding"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  UnusedConstructor name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused constructor"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a constructor named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused constructor"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  UnusedType name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused type"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a type named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused type"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  MadlibVersionMinorTooLow pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                start
                <> " requires the minimum version '"
                <> minVersion
                <> "' but you currently use madlib\n"
                <> "version '"
                <> versionUsed
                <> "'"
            )
          ]
          [Diagnose.Hint "Update your version of madlib."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          []
          [Diagnose.Hint "Update your version of madlib."]

  MadlibVersionMajorDiffer pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                start
                <> " requires the minimum version '"
                <> minVersion
                <> "' but you currently use madlib\n"
                <> "version '"
                <> versionUsed
                <> "'. Because major versions differ it means there is a breaking\n"
                <> "change and you may not be able to run the project"
            )
          ]
          [Diagnose.Hint "Update your version of madlib."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          []
          [Diagnose.Hint "Update your version of madlib."]

  IncompletePattern missingPatterns ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Incomplete pattern"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The branches do not cover all cases\n"
                <> "Examples of missing patterns:\n"
                <> intercalate "\n" (map ("  - "++) missingPatterns)
            )
          ]
          [ Diagnose.Note "If the input of where is not handled by a branch, it will most likely crash at\nruntime."
          , Diagnose.Hint "Pattern match the missing constructors or add a catch all branch with '_ => ...'."
          ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Incomplete pattern"
          []
          [ Diagnose.Note "If the input of where is not handled by a branch, it will most likely crash at\nruntime."
          , Diagnose.Hint "Pattern match the missing constructors or add a catch all branch with '_ => ...'."
          ]

  RedundantPattern ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Redundant pattern"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This "Unreachable pattern"
            )
          ]
          [ Diagnose.Note "This pattern will never be reached."
          , Diagnose.Hint "Remove it or move it higher up so that it might be useful."
          ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Redundant pattern"
          []
          [ Diagnose.Note "This pattern will never be reached."
          , Diagnose.Hint "Remove it or move it higher up so that it might be useful."
          ]

  TypedHoleFound t suggestions ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        let (renderedType, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t))
            indentedType = unlines $ ("  "<>) <$> lines renderedType
            renderSuggestion (name, sc) =
              name <> " :: " <> renderSchemeOneLine sc
            suggestionsNote =
              if null suggestions then []
              else [Diagnose.Note $ "Suggestions (in scope):\n" <> unlines (map (("  " <>) . renderSuggestion) suggestions)]
        in  Diagnose.Warn
              Nothing
              "Typed hole"
              [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                , Diagnose.This $ "I found a typed hole with type:\n  " <> indentedType
                )
              ]
              ( suggestionsNote <>
                [ Diagnose.Note "This will crash at runtime if reached."
                , Diagnose.Hint "Replace it with a valid expression of that type."
                ]
              )

      NoContext ->
        Diagnose.Warn
          Nothing
          "Typed hole"
          []
          [ Diagnose.Note "This will crash at runtime if reached."
          , Diagnose.Hint "Replace it with a valid expression of that type."
          ]


formatError :: (FilePath -> IO String) -> Bool -> CompilationError -> IO String
formatError rf json err@(CompilationError typeErr ctx)
  | json = return $ formatErrorJson err
  | otherwise = do
      noColor       <- lookupEnv "NO_COLOR"
      moduleContent <- getModuleContent rf ctx
      let isColorEnabled =  not (noColor /= Just "" && Maybe.isJust noColor)
          modulePath     = getCtxPath' ctx
          report         = createErrorDiagnostic isColorEnabled ctx typeErr
          diagnostic     = Diagnose.addFile Diagnose.def modulePath moduleContent
          diagnostic'    = Diagnose.addReport diagnostic report
          diagnosticDoc  =
            if isColorEnabled then
              Diagnose.defaultStyle $ Diagnose.prettyDiagnostic True 2 diagnostic'
            else
              Pretty.unAnnotate $ Diagnose.prettyDiagnostic True 2 diagnostic'
          layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
          s = Terminal.renderStrict (Pretty.layoutPretty layoutOptions diagnosticDoc)
      return $ Text.unpack s


-- | Format a 'CompilationError' as a newline-delimited JSON object.
-- Suitable for machine consumption (e.g. CI tools, editor integrations).
formatErrorJson :: CompilationError -> String
formatErrorJson (CompilationError typeErr ctx) =
  let path    = getCtxPath' ctx
      (line, col) = case ctx of
        Context _ (Area (Loc _ l c) _) -> (l, c)
        _                              -> (0, 0)
      msg = createSimpleErrorDiagnostic False ctx typeErr
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


simpleFormatError :: Bool -> CompilationError -> IO String
simpleFormatError json (CompilationError err ctx) = do
  noColor <- lookupEnv "NO_COLOR"
  let isColorEnabled = not json && not (noColor /= Just "" && Maybe.isJust noColor)
  return $ createSimpleErrorDiagnostic isColorEnabled ctx err


-- | Format an error with hints and notes included, for LSP display.
-- Builds the message from the Diagnose.Report to avoid duplication:
-- extracts the title, the marker messages, and all notes into a single string.
simpleFormatErrorWithHints :: Bool -> CompilationError -> IO String
simpleFormatErrorWithHints json (CompilationError err ctx) = do
  noColor <- lookupEnv "NO_COLOR"
  let isColorEnabled = not json && not (noColor /= Just "" && Maybe.isJust noColor)
      report = createErrorDiagnostic isColorEnabled ctx err
  return $ reportToPlainText report


-- | Format a warning with hints and notes included, for LSP display.
simpleFormatWarningWithHints :: Bool -> CompilationWarning -> IO String
simpleFormatWarningWithHints json (CompilationWarning warning ctx) = do
  noColor <- lookupEnv "NO_COLOR"
  let isColorEnabled = not json && not (noColor /= Just "" && Maybe.isJust noColor)
      report = createWarningDiagnostic isColorEnabled ctx warning
  return $ reportToPlainText report


-- | Convert a Diagnose.Report to a plain text string for LSP display.
-- Extracts: title, marker messages (This/Where), and all hints/notes.
-- This is the single source of truth for LSP error messages — no duplication.
reportToPlainText :: Diagnose.Report String -> String
reportToPlainText report =
  let (title, markers, notes) = case report of
        Diagnose.Err _ t ms ns   -> (t, ms, ns)
        Diagnose.Warn _ t ms ns  -> (t, ms, ns)
      markerTexts = concatMap (\(_, marker) -> case marker of
        Diagnose.This msg  -> [msg]
        Diagnose.Where msg -> [msg]
        Diagnose.Maybe msg -> [msg]
        ) markers
      noteTexts = map noteToString notes
      body = title : (markerTexts ++ noteTexts)
  in  intercalate "\n" (filter (not . null) body)

-- | Convert a Diagnose.Note to a plain string.
noteToString :: Diagnose.Note String -> String
noteToString (Diagnose.Hint msg) = "Hint: " <> msg
noteToString (Diagnose.Note msg) = "Note: " <> msg


createSimpleErrorDiagnostic :: Bool -> Context -> TypeError -> String
createSimpleErrorDiagnostic color _ typeError = case typeError of
  UnificationError t1 t2 origin _ ->
    let (pretty1', pretty2') = renderTypesWithDiff color t1 t2
        pretty1'' = intercalate "\n" $ ("  "<>) <$> lines pretty1'
        pretty2'' = intercalate "\n" $ ("  "<>) <$> lines pretty2'
        expectedStr = if color then "\x1b[0mexpected:\n" else "expected:\n"
        foundStr = if color then "\n\x1b[0mbut found:\n" else "\nbut found:\n"
        title = mkUnificationTitle t1 t2 origin
        originContext = case origin of
          FromFunctionArgument fn n (Just (FunctionContext expectedType fullSig _)) ->
            "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type.\n"
            <> "'" <> fn <> "' expects " <> prettyPrintType True expectedType <> " as its " <> toOrdinal n <> " argument.\n"
            <> "Full signature: " <> fn <> " :: " <> prettyPrintType True fullSig <> "\n"
          FromFunctionArgument fn n Nothing ->
            "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type.\n"
          FromFunctionReturn fn ->
            "The return value of '" <> fn <> "' does not match its type annotation.\n"
          FromOperator op ->
            "The operands of '" <> op <> "' must have compatible types.\n"
          FromIfCondition ->
            "The condition of an 'if' must be Boolean.\n"
          FromIfBranches ThenBranch ->
            "The 'then' branch has a different type than the 'else' branch.\n"
          FromIfBranches ElseBranch ->
            "The 'else' branch has a different type than the 'then' branch.\n"
          FromWhileCondition ->
            "The condition of a 'while' must be Boolean.\n"
          FromListElement n | n > 0 ->
            "The " <> toOrdinal n <> " element has a different type than the previous elements.\n"
          FromListElement _ ->
            "All elements in a list must have the same type.\n"
          FromTypeAnnotation ->
            "The expression does not match its type annotation.\n"
          FromPatternMatch n | n > 0 ->
            "The " <> toOrdinal n <> " branch returns a different type than the other branches.\n"
          FromPatternMatch _ ->
            "All branches of a 'where' must return the same type.\n"
          FromAssignment name ->
            "The value does not match the declared type of '" <> name <> "'.\n"
          NoOrigin -> ""
    in  title <> "\n\n" <> originContext <> expectedStr <> pretty2'' <> foundStr <> pretty1''

  TestNotValid t ->
    let prettyType = renderType t
        prettyType' = intercalate "\n" $ ("  "<>) <$> lines prettyType
    in  "Invalid test type\n\n" <> "This test expression has type:\n" <> prettyType'
              <> "\nbut tests must return one of:\n"
              <> "  Wish TestResult TestResult\n"
              <> "  List (Wish TestResult TestResult)"

  TypingHasWrongKind t expectedKind actualKind ->
    let expectedStr = if color then "\x1b[0mexpected:\n  " else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n  " else "\nbut found:\n  "
    in  "Kind mismatch in type annotation\n\n"
        <> "The type '" <> prettyPrintType False t <> "' has kind " <> kindToStr actualKind
        <> ", but kind " <> kindToStr expectedKind <> " was expected.\n"
        <> expectedStr
        <> colorWhen color Green (kindToStr expectedKind)
        <> foundStr
        <> colorWhen color Red (kindToStr actualKind)

  BadEscapeSequence ->
    "This escape sequence is not valid\n\n"
    <> "Hint: Valid escape sequences are either a byte: \\xAB or a unicode: \\uABCD or \\u{ABCDEF} up to 10FFFF"

  EmptyChar ->
    "Empty char\n\n"
    <> "Note: Characters can't be empty"

  NoMain ->
    "Missing 'main' function\n\n"
    <> "Your entry module must define a 'main' function.\n\n"
    <> "Hint: Add a 'main' function, for example:\n"
    <> "  main = () => { IO.log(\"Hello!\") }"

  MainInvalidTyping ->
    "Invalid 'main' signature\n\n"
    <> "The 'main' function must accept 'List String' and return '{}'.\n\n"
    <> "Hint: Use:  main :: List String -> {}\n"
    <> "Note: You can also omit the type annotation and let it be inferred."

  MutationRestriction ->
    "Mutation restriction\n\n"
    <> "This binding depends on a closure that performs mutation and cannot be made polymorphic.\n\n"
    <> "Hint: Add an explicit type annotation with concrete (non-polymorphic) types.\n"
    <> "Note: Mutation requires a fixed memory location; polymorphic types would create separate copies."

  MutatingFunction n ->
    "Cannot mutate function\n\n"
    <> "'" <> n <> "' is a function and cannot be reassigned with ':='.\n\n"
    <> "Hint: To make it swappable, wrap it in a mutable record:\n"
    <> "  ref = { fn: " <> n <> " }\n"
    <> "  ref.fn := newFunction"

  NotAConstructor n ->
    "Not a constructor\n\n"
    <> "'" <> n <> "' is used in a pattern but is not a constructor.\n\n"
    <> "Hint: Only data constructors (capitalized names) can appear in patterns.\n"
    <> "Note: Example pattern:  where | Just x => ...   | Nothing => ...\n"

  MethodNameAlreadyDefined ->
    "Method name already defined\n\n"
    <> "This method name is already used by another interface.\n\n"
    <> "Note: Two interfaces cannot define methods with the same name.\n"
    <> "Hint: Choose a different name, or prefix it to avoid collisions."

  NotADefinition ->
    "Not a definition\n\n"
    <> "This expression appears at the top level, but only definitions are allowed here.\n\n"
    <> "Note: A module's top level can only contain: type definitions, assignments, imports, exports, and interface/instance declarations.\n"
    <> "Hint: Assign it to a name:  myValue = <expression>\n"

  InfiniteType tv t ->
    let (vars, hkVars, printedT) = prettyPrintType' True (mempty, mempty) t
        (_, _, printedN)         = prettyPrintType' True (vars, hkVars) (TVar tv)
    in  "Infinite type\n\n"
        <> "I can't construct this type because it would be infinite.\n"
        <> "The type variable '" <> printedN <> "' appears in its own definition: "
        <> printedN <> " = " <> printedT <> "\n\n"
        <> "Hint: This usually happens when a function is applied to itself\n"
        <> "or when a recursive type needs an explicit type alias."

  IllegalSkipAccess ->
    "Cannot use '_' as a value\n\n"
    <> "The skip symbol '_' is a placeholder for values you don't need. It cannot be read.\n\n"
    <> "Hint: Give it a name if you need to use the value: (x) => x  instead of  (_) => _\n"
    <> "Note: '_' means 'I don't care about this value'. To use it, replace '_' with a named variable."

  UnboundVariable n suggestions ->
    let typoStr = case suggestions of
                    []  -> "Hint: Check for a typo in the name.\n"
                    [s] -> "Hint: Did you mean '" <> s <> "'?\n"
                    _   -> "Hint: Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?\n"
        stdlibStr = case List.lookup n stdlibMap of
                      Just modName -> "Note: '" <> n <> "' is defined in the '" <> modName <> "' module. Add: import " <> modName <> " from \"" <> modName <> "\""
                      Nothing      -> "Note: If '" <> n <> "' is from another module, make sure you have imported it."
    in  "Unbound variable\n\n"
        <> "'" <> n <> "' is not defined in this scope.\n\n"
        <> typoStr
        <> stdlibStr

  UnboundUnknownTypeVariable ->
    "Unbound type variable\n\n"
    <> "A type variable has not been declared\n\n"
    <> "Type variables must be declared in the enclosing function's type signature.\n"
    <> "Hint: For example: myFn :: a -> a (here 'a' is declared in the signature)"

  UnboundVariableFromNamespace namespace name ->
    "Name not in module\n\n"
    <> "'" <> name <> "' was not found in '" <> namespace <> "'.\n\n"
    <> "Hint: Check the exports of '" <> namespace <> "' or use the full import form."

  CapitalizedADTTVar adtname param ->
    "Capitalized ADT variable\n\n"
    <> "The type parameter '" <> param <> "' in the type declaration\n"
    <> "'" <> adtname <> "' is capitalized. Type parameters can't be capitalized."
    <> "Hint: Either remove it if you don't need the type variable, or\nmake its first letter lowercase."

  UnboundType n suggestions ->
    "Unknown type\n\n"
    <> "The type '" <> n <> "' is not defined in this scope.\n\n"
    <> case suggestions of
         []  -> "Hint: Check for a typo in the type name.\nNote: If '" <> n <> "' is defined in another module, import it."
         [s] -> "Hint: Did you mean '" <> s <> "'?"
         _   -> "Hint: Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"

  DerivingAliasNotAllowed n ->
    "Cannot derive for type alias\n\n"
    <> "'" <> n <> "' is a type alias, not a concrete type. Cannot derive instances for aliases.\n\n"
    <> "Hint: Derive the instance on the underlying type that the alias refers to.\n"
    <> "Note: Type aliases are transparent -- instances on the underlying type apply automatically."

  InvalidInterfaceDerived n ->
    "Cannot derive '" <> n <> "'\n\n"
    <> "The interface '" <> n <> "' does not support automatic derivation.\n\n"
    <> "Note: Only these interfaces can be derived: Eq, Show, Comparable.\n"
    <> "Hint: Write a manual instance instead:\n"
    <> "  instance " <> n <> " YourType { ... }"

  SignatureTooGeneral scGiven scInferred ->
    let (scInferred', scGiven') = renderSchemesWithDiff color scInferred scGiven
        scGiven''    = unlines $ ("  "<>) <$> lines scGiven'
        scInferred'' = unlines $ ("  "<>) <$> lines scInferred'
        givenStr     = if color then "\x1b[0mType signature given:\n" else "Type signature given:\n  "
        inferredStr  = if color then "\n\x1b[0mType inferred:\n" else "\nType inferred:\n  "
    in  "Signature too general\n\n"
        <> "The annotation claims the function is more polymorphic than the\n"
        <> "implementation allows. The inferred type is more specific.\n\n"
        <> givenStr <> scGiven'' <> inferredStr <> scInferred''
        <> "\nHint: Update the type annotation to match the inferred type shown above."

  NoInstanceFound cls ts ->
    let typeStr = unwords (prettyPrintType True <$> ts)
        predStr = lst (predToStr True (mempty, mempty) (IsIn cls ts Nothing))
    in  "No instance found\n\n"
        <> "'" <> predStr <> "' is required here, but no instance was found for '" <> typeStr <> "'.\n\n"
        <> "Hint: Make sure '" <> typeStr <> "' implements the '" <> cls <> "' interface.\n"
        <> "Note: Instance methods are automatically in scope when their module is imported,\ndirectly or transitively."

  AmbiguousType (TV _ _, IsIn cls _ _ : _) ->
    "Ambiguous type\n\n"
    <> "An instance of '" <> cls <> "' could not be found\n\n"
    <> "Hint: You can add a type annotation to make it resolvable."

  AmbiguousType (TV n _, []) ->
    "Ambiguous type\n\n"
    <> "An ambiguity for the type variable '" <> (renderTVar n) <> "' could not be resolved\n\n"
    <> "Hint: You can add a type annotation to make it resolvable."

  InterfaceNotExisting cls ->
    "Interface not found\n\n"
    <> "The interface '" <> cls <> "' is not defined.\n\n"
    <> "Hint: Make sure you imported the module defining it,\nor a module that imports it."

  KindError (t, k) (t', k') ->
    "Kind mismatch\n\n"
    <> "'" <> prettyPrintType True t <> "' has kind " <> kindToStr k
    <> ", but '" <> prettyPrintType True t' <> "' has kind " <> kindToStr k' <> ".\n\n"
    <> "Note: Kinds describe how many type arguments a type constructor takes.\n"
    <> "  '*' means a fully-applied type (like 'Int' or 'String').\n"
    <> "  '* -> *' means it takes one argument (like 'List' or 'Maybe').\n"
    <> "Hint: Check whether you applied too many or too few type arguments."

  InstancePredicateError pInstance pWrong pCorrect ->
    let instStr    = lst (predToStr True (mempty, mempty) pInstance)
        wrongStr   = lst (predToStr True (mempty, mempty) pWrong)
        correctStr = lst (predToStr True (mempty, mempty) pCorrect)
    in  "Instance constraint error\n\n"
        <> "The instance '" <> instStr <> "' has an incorrect constraint.\n"
        <> "  Given:    " <> wrongStr <> "\n"
        <> "  Expected: " <> correctStr <> "\n\n"
        <> "Hint: Replace '" <> wrongStr <> "' with '" <> correctStr <> "' in the instance declaration."

  ImportCycle paths ->
    let loop [] = ""
        loop [p]    = "  " <> p <> "\n  ↓  (loops back to start)"
        loop (p:rest) = "  " <> p <> "\n  ↓\n" <> loop rest
    in  "Circular import\n\n"
        <> "This import creates a cycle:\n\n"
        <> loop paths <> "\n\n"
        <> "Note: Circular imports are not allowed because there is no valid initialization order.\n"
        <> "Hint: Extract the shared types or functions into a third module that both can import.\n"
        <> "Hint: Alternatively, move the code that causes the cycle into one of the two modules."

  TypeAnnotationNameMismatch typingName expName ->
    "Type annotation name mismatch\n\n"
    <> "The type annotation is for '" <> typingName <> "' but the following definition is for '" <> expName <> "'.\n\n"
    <> "Hint: Rename the annotation to match: '" <> expName <> " :: <type>'\n"
    <> "Note: A type annotation must immediately precede the definition it annotates and share the same name."

  GrammarError _ msg ->
    let trimmed = List.dropWhileEnd (\c -> c == '\n' || c == '\r') msg
        detail  = if null trimmed then "Unexpected token" else trimmed
    in  "Syntax error\n\n"
        <> detail <> "\n\n"
        <> "Hint: Check for a missing bracket, parenthesis, or operator near this location.\n"
        <> "Note: Common causes: unclosed '(', '{', or '['; a missing '->' in a function; or a typo in a keyword."

  ByteOutOfBounds x ->
    "Byte out of bounds\n\n"
    <> "The literal '" <> x <> "_b' is too big, the maximum value for bytes is 255\n\n"
    <> "Hint: Use a value between 0 and 255, or use 'Integer' for larger numbers."

  ShortOutOfBounds x ->
    "Short out of bounds\n\n"
    <> "The literal '" <> x <> "_s' is too big, the maximum value for shorts is "<> show (2^31 - 1 :: Integer) <> "\n\n"
    <> "Hint: Use a value within the short range, or use 'Integer' for larger numbers."

  IntOutOfBounds x ->
    "Integer out of bounds\n\n"
    <> "The literal '" <> x <> "_i' is too big, the maximum value for integers is "<> show (2^63 - 1 :: Integer) <> "\n\n"
    <> "Hint: Use a Float for very large numbers, or restructure the computation."

  NegatedByte ->
    "Negated byte\n\n"
    <> "Bytes can't be negated\n\n"
    <> "Note: Bytes are unsigned (0-255) and cannot be negated.\n"
    <> "Hint: Use 'Integer' or 'Short' if you need negative numbers."

  UnknownType t suggestions ->
    "Unknown type\n\n"
    <> "The type '" <> t <> "' was not found\n\n"
    <> case suggestions of
         []  -> "Hint: Verify that you imported it"
         [s] -> "Hint: Did you mean '" <> s <> "'?"
         _   -> "Hint: Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"

  NameAlreadyDefined name ->
    "Illegal shadowing\n\n"
    <> "The variable '" <> name <> "' is already defined\n\n"
    <> "Hint: Change the name of the variable.\n"
    <> "Note: The variable might be defined further down. All top level\n"
    <> "assignments share the scope and using a local name that is\n"
    <> "defined in the global scope of a module is not allowed."

  TypeAlreadyDefined name ->
    "Type already defined\n\n"
    <> "The type '" <> name <> "' is already defined\n\n"
    <> "Hint: Change the name of the type.\n"

  ImportCollision name ->
    "Import collision\n\n"
    <> "The imported name '" <> name <> "' is already used\n\n"
    <> "Hint: Use a selective import to rename one: import { " <> name <> " as " <> name <> "2 } from \"./Module\"\n"
    <> "Hint: Or use a qualified import to disambiguate."

  NameAlreadyExported name ->
    "Already exported\n\n"
    <> "'" <> name <> "' appears more than once in the export list.\n\n"
    <> "Hint: Remove the duplicate export."

  NotExported name path suggestions ->
    "Not exported\n\n"
    <> "'" <> name <> "' is not exported by the module at:\n"
    <> "'" <> path <> "'\n\n"
    <> case suggestions of
         []  -> "Hint: Check the spelling, or add 'export { " <> name <> " }' to the module if you own it."
         [s] -> "Hint: Did you mean '" <> s <> "'?"
         _   -> "Hint: Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"

  RecursiveVarAccess _ ->
    "Recursive variable access\n\n"
    <> "This variable refers to itself directly in its own definition,\n"
    <> "so it cannot be initialized.\n\n"
    <> "Note: A direct self-reference is not allowed. Wrap the recursive\n"
    <> "access in a function to defer evaluation:\n"
    <> "  // Not allowed -- direct self-reference:\n"
    <> "  parser = J.map(Title, J.field(\"title\", parser))\n"
    <> "  // Works -- recursive access is wrapped in a function:\n"
    <> "  parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser)))"

  NotInScope name (Loc _ line _) ->
    "Not in scope\n\n"
    <> "'" <> name <> "' (at line " <> ppShow line <> ") is not yet defined at this point.\n\n"
    <> "Note: All variables must be defined before they are used.\n"
    <> "Hint: Move the definition of '" <> name <> "' above this usage, or add a type\n"
    <> "annotation to allow forward references."

  TypesHaveDifferentOrigin adtName origin1 origin2 ->
    "Types have different origins\n\n"
    <> "The type '" <> adtName <> "' is imported from two different locations:\n"
    <> "  - '" <> origin1 <> "'\n"
    <> "  - '" <> origin2 <> "'\n"
    <> "These are treated as distinct types even though they share a name.\n\n"
    <> "Hint: Import '" <> adtName <> "' from only one location, or convert between the two explicitly."

  ShouldBeTypedOrAbove name ->
    "Must be typed or above\n\n"
    <> "'" <> name <> "' is used before it is defined.\n\n"
    <> "Hint: Move the definition of '" <> name <> "' above this usage, or add a type annotation.\n"
    <> "Note: Forward references are allowed only when the referenced name has a type annotation."

  NotCapitalizedADTName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  "ADT name not capitalized\n\n"
        <> "The name '" <> name <> "' of this type is not capitalized\n\n"
        <> "Note: This is incorrect and all types in madlib should start with\n"
        <> "an uppercased letter.\n"
        <> "Hint: Change it to '" <> capitalized <> "'"

  NotCapitalizedAliasName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  "Alias name not capitalized\n\n"
        <> "The name '" <> name <> "' of this type alias is not capitalized\n\n"
        <> "Note: This is incorrect and all types in madlib should start with\n"
        <> "an uppercased letter.\n"
        <> "Hint: Change it to '" <> capitalized <> "'"

  NotCapitalizedConstructorName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  "Constructor name not capitalized\n\n"
        <> "The name '" <> name <> "' of this type constructor is not capitalized\n\n"
        <> "Note: This is incorrect and all types in madlib should start with\n"
        <> "an uppercased letter.\n"
        <> "Hint: Change it to '" <> capitalized <> "'"

  ContextTooWeak preds ->
    "Context too weak\n\n"
    <> "The type annotation is missing required interface constraints.\n"
    <> "The implementation needs: " <> intercalate ", " (predClass <$> preds)
    <> "\n\n"
    <> "Hint: Add the missing constraints to the type annotation.\n"
    <> "Note: Example: if 'Eq a' is missing, change\n"
    <> "  'myFn :: a -> Boolean'  to  'myFn :: Eq a => a -> Boolean'"

  OverloadedMutation n _ ->
    "Mutation in overloaded context\n\n"
    <> "Cannot mutate '" <> n <> "' in a function with type class constraints.\n\n"
    <> "Note: Each specialisation creates a separate closure copy, so mutations\n"
    <> "to '" <> n <> "' would be invisible to callers.\n"
    <> "Hint: Add a concrete type annotation to remove the polymorphism, or avoid mutation here."

  WrongAliasArgCount aliasName expected actual ->
    "Wrong alias argument count\n\n"
    <> "The alias '" <> aliasName <> "' was expected to have " <> show expected <> " argument" <> (if expected > 1 then "s" else "") <> ",\nbut "
    <> show actual <> " "<> (if actual > 1 then "were" else "was") <>" given\n\n"
    <>  if actual > expected then
          "Hint: Remove " <> show (actual - expected) <> " argument(s)"
        else
          "Hint: Add the missing '" <> show (expected - actual) <> "' argument(s)"

  ImportNotFound importName ->
    let isRelative = List.isPrefixOf "./" importName || List.isPrefixOf "../" importName
    in  "Import not found\n\n"
        <> "The module '" <> importName <> "' could not be found.\n\n"
        <> if isRelative then
             "Hint: The file '" <> importName <> ".mad' was not found relative to this module.\n"
             <> "Note: Check the file path and make sure the file exists."
           else
             "Hint: The package '" <> importName <> "' could not be found.\n"
             <> "Note: Run 'madlib install' to install missing dependencies, or check 'madlib.json'."

  InterfaceAlreadyDefined interfaceName ->
    "Interface already defined\n\n"
    <> "The interface '" <> interfaceName <> "' is already defined.\n\n"
    <> "Hint: Choose a different name, or remove the duplicate definition."

  ADTAlreadyDefined adtType ->
    let adtName = renderType adtType
    in  "Type already defined\n\n"
        <> "The type '" <> adtName <> "' is already defined.\n\n"
        <> "Hint: Choose a different name, or remove the duplicate definition."

  WrongSpreadType t ->
    "Type error\n\n" <> t <> "\n\n"
    <> "Note: The spread operator '...' is only valid on record types.\n"
    <> "Hint: Check that the value you are spreading is a record, or remove the spread."

  RecordDuplicateFields fs ->
    "Record duplicate fields\n\n"
    <> "The following fields appear more than once in the record constructor: " <> (concatMap ("\n - " ++) fs) <> "\n\n"
    <> "Hint: Define each field only once."

  RecordMissingFields fs ->
    "Record missing fields\n\n"
    <> "The record is missing the following fields: " <> intercalate ", " (map (\f -> "'" <> f <> "'") fs) <> "\n\n"
    <> "Hint: Add the missing fields to the record."

  RecordExtraFields fs _ ->
    "Record extra fields\n\n"
    <> "The record has unexpected fields: " <> intercalate ", " (map (\f -> "'" <> f <> "'") fs) <> "\n\n"
    <> "Hint: Remove the extra fields or check for a typo."

  InvalidLhs ->
    "Invalid left hand side\n\n"
    <> "The left-hand side of an assignment must be a variable name, a record pattern, or a list pattern.\n\n"
    <> "Hint: Valid examples: x = 5, { name } = person, [first] = list"

  BadMutation ->
    "Bad mutation\n\n"
    <> "Cannot reassign a variable with '='. Use ':=' to mutate an existing binding.\n\n"
    <> "Hint: Use the mutation operator ':='\n"
    <> "Note: Example:  x = 0         // initial binding\n"
    <> "         x := x + 1    // mutation"

  MutatingNotInScope name ->
    "Not in scope\n\n"
    <> "Cannot mutate '" <> name <> "' because it is not in scope.\n\n"
    <> "Hint: Declare '" <> name <> "' before mutating it, or check for a typo in the name."

  MutatingPatternBoundVariable name ->
    "Cannot mutate pattern-bound variable\n\n"
    <> "'" <> name <> "' is bound by pattern matching and cannot be mutated with ':='.\n\n"
    <> "Hint: Introduce a local let binding first: " <> name <> " = <patternVar>, then use ':=' on that."

  FatalError ->
    "Internal compiler error\n\n"
    <> "The compiler encountered an unexpected internal state and could not continue.\n\n"
    <> "Hint: This is likely a compiler bug. Please report it with the code that triggered it.\n"
    <> "Note: Try adding a type annotation or reorganizing the problematic expression."

  Error ->
    "Compilation error\n\n"
    <> "An error occurred during compilation.\n\n"
    <> "Hint: Check the surrounding code for type mismatches or missing imports."

  ASTHasNoPath ->
    "Module not found\n\n"
    <> "A required module could not be located or loaded.\n\n"
    <> "Hint: Verify that all imports resolve to existing files.\n"
    <> "Note: If this is a package dependency, run 'madlib install' to fetch it."

  ConstructorAccessBadIndex typeName constructorName arity index ->
    "Constructor index out of range\n\n"
    <> "The constructor '" <> constructorName <> "' from '" <> typeName <> "' has "
    <> show arity <> " parameter" <> (if arity == 1 then "" else "s")
    <> ", but index " <> show index <> " was accessed.\n\n"
    <> "Hint: Valid indices for '" <> constructorName <> "' are 0 to " <> show (arity - 1) <> "."

  ConstructorAccessNoConstructorFound typeName ->
    "Constructor not found\n\n"
    <> "The type '" <> typeName <> "' has no constructors that can be accessed this way.\n\n"
    <> "Hint: Use pattern matching to destructure the value instead."

  ConstructorAccessTooManyConstructors typeName _ ->
    "Ambiguous constructor access\n\n"
    <> "Cannot access a constructor parameter of '" <> typeName <> "' because it has more than one constructor.\n\n"
    <> "Hint: Use pattern matching to safely handle each constructor case.\n"
    <> "Note: Constructor parameter access only works on types with a single constructor."

  RecordDuplicateRestPattern ->
    "Duplicate rest pattern\n\n"
    <> "A record pattern can only have one rest/spread pattern ('...').\n\n"
    <> "Hint: Remove the extra '...' and keep only one.\n"
    <> "Note: Example:  { x, ...rest } = myRecord  -- only one spread allowed"





mkError :: String -> Context -> String -> [Diagnose.Note String] -> Diagnose.Report String
mkError title context message notes = case context of
  Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
    Diagnose.Err
      Nothing
      title
      [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
        , Diagnose.This message
        )
      ]
      notes
  NoContext ->
    Diagnose.Err Nothing title [] notes


createErrorDiagnostic :: Bool -> Context -> TypeError -> Diagnose.Report String
createErrorDiagnostic color context typeError = case typeError of
  UnificationError t1 t2 origin maybeSecondary ->
    let (pretty1', pretty2') = renderTypesWithDiff color t1 t2
        pretty1'' = intercalate "\n" $ ("  "<>) <$> lines pretty1'
        pretty2'' = intercalate "\n" $ ("  "<>) <$> lines pretty2'
        expectedStr = if color then "\x1b[0mexpected:\n" else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n" else "\nbut found:\n  "
        secondaryPositions = case maybeSecondary of
          Just (SecondaryLocation path (Area (Loc _ sl sc) (Loc _ el ec)) msg) ->
            [(Diagnose.Position (sl, sc) (el, ec) path, Diagnose.Where msg)]
          Nothing -> []
        originHint = case origin of
          FromFunctionArgument fn n (Just (FunctionContext expectedType fullSig _)) ->
            [ Diagnose.Hint $ "'" <> fn <> "' expects " <> prettyPrintType True expectedType <> " as its " <> toOrdinal n <> " argument."
            , Diagnose.Note $ "Full signature: " <> fn <> " :: " <> prettyPrintType True fullSig
            ]
          FromFunctionArgument fn n Nothing ->
            [ Diagnose.Hint $ "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type." ]
          FromFunctionReturn fn ->
            [ Diagnose.Hint $ "The return value of '" <> fn <> "' doesn't match its type annotation."
            , Diagnose.Note "Check that all branches of the function body return the same type."
            ]
          FromOperator op -> operatorHints op t1 t2
          FromIfCondition ->
            [ Diagnose.Hint "The condition of an 'if' expression must be Boolean."
            , Diagnose.Note "Boolean values are 'true' and 'false'. Did you forget a comparison?"
            ]
          FromIfBranches ThenBranch ->
            [ Diagnose.Hint "The 'then' branch has a different type than the 'else' branch."
            , Diagnose.Note "Both branches of an 'if' must return the same type."
            ]
          FromIfBranches ElseBranch ->
            [ Diagnose.Hint "The 'else' branch has a different type than the 'then' branch."
            , Diagnose.Note "Both branches of an 'if' must return the same type."
            ]
          FromWhileCondition ->
            [ Diagnose.Hint "The condition of a 'while' loop must be Boolean." ]
          FromListElement n | n > 0 ->
            [ Diagnose.Hint $ "The " <> toOrdinal n <> " element has a different type than the previous elements."
            , Diagnose.Note "All elements in a list must have the same type."
            ]
          FromListElement _ ->
            [ Diagnose.Hint "All elements in a list literal must have the same type."
            , Diagnose.Note "If you need a heterogeneous collection, consider a custom type or a tuple."
            ]
          FromTypeAnnotation ->
            [ Diagnose.Hint "The expression's type doesn't match its annotation."
            , Diagnose.Note "Check whether the annotation is too specific, or the expression is wrong."
            ]
          FromPatternMatch n | n > 0 ->
            [ Diagnose.Hint $ "The " <> toOrdinal n <> " branch returns a different type than the other branches."
            , Diagnose.Note "All branches of 'where' must return the same type."
            ]
          FromPatternMatch _ ->
            [ Diagnose.Hint "All branches of a 'where' expression must return the same type."
            , Diagnose.Note "Make sure every branch has the same return type."
            ]
          FromAssignment name ->
            [ Diagnose.Hint $ "The right-hand side doesn't match the declared type of '" <> name <> "'." ]
          NoOrigin -> []
        title = mkUnificationTitle t1 t2 origin
        originPrefix = case origin of
          FromFunctionArgument fn n (Just (FunctionContext expectedType _ _)) ->
            "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type.\n"
            <> "Expected: " <> prettyPrintType True expectedType <> "\n"
          FromFunctionArgument fn n Nothing ->
            "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type.\n"
          FromFunctionReturn fn ->
            "The return value of '" <> fn <> "' does not match its annotation.\n"
          FromOperator op ->
            "The operands of '" <> op <> "' have incompatible types.\n"
          FromIfCondition ->
            "The condition of an 'if' must be Boolean.\n"
          FromIfBranches ThenBranch ->
            "The 'then' branch has a different type than the 'else' branch.\n"
          FromIfBranches ElseBranch ->
            "The 'else' branch has a different type than the 'then' branch.\n"
          FromWhileCondition ->
            "The condition of a 'while' must be Boolean.\n"
          FromListElement n | n > 0 ->
            "The " <> toOrdinal n <> " element has a different type than the previous elements.\n"
          FromListElement _ ->
            "All list elements must have the same type.\n"
          FromTypeAnnotation ->
            "The expression does not match its type annotation.\n"
          FromPatternMatch n | n > 0 ->
            "The " <> toOrdinal n <> " branch returns a different type than the other branches.\n"
          FromPatternMatch _ ->
            "All branches of a 'where' must return the same type.\n"
          FromAssignment name ->
            "The value does not match the declared type of '" <> name <> "'.\n"
          NoOrigin -> ""
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          title
          ([ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ originPrefix <> expectedStr <> pretty2'' <> foundStr <> pretty1''
            )
          ] ++ secondaryPositions)
          originHint

      NoContext ->
        Diagnose.Err
          Nothing
          (title <> "\n\n" <> originPrefix <> expectedStr <> pretty2'' <> foundStr <> pretty1'')
          []
          originHint

  TestNotValid t ->
    let prettyType = renderType t
        prettyType' = intercalate "\n" $ ("  "<>) <$> lines prettyType
    in  mkError "Invalid test type" context
          (    "This test expression has type:\n" <> prettyType'
            <> "\nbut tests must return one of:\n"
            <> "  Wish TestResult TestResult\n"
            <> "  List (Wish TestResult TestResult)"
          )
          [ Diagnose.Hint "A test must evaluate to a Wish that resolves to a TestResult."
          , Diagnose.Note "Use 'assertEqual' or 'test' from the Test module to build test values."
          ]

  TypingHasWrongKind t expectedKind actualKind ->
    let expectedStr = if color then "\x1b[0mexpected:\n  " else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n  " else "\nbut found:\n  "
        prettyT = prettyPrintType False t
    in  mkError "Kind mismatch in type annotation" context
          (    "The type '" <> prettyT <> "' has kind " <> kindToStr actualKind
            <> ", but kind " <> kindToStr expectedKind <> " was expected.\n"
            <> expectedStr
            <> colorWhen color Green (kindToStr expectedKind)
            <> foundStr
            <> colorWhen color Red (kindToStr actualKind)
          )
          [ Diagnose.Note $
              "Kinds describe how many type arguments a type constructor takes.\n"
              <> "'*' means a concrete type (like 'Int').\n"
              <> "'* -> *' means it takes one argument (like 'List' or 'Maybe')."
          , Diagnose.Hint $ "'" <> prettyT <> "' needs "
              <> (if actualKind == Star then "no" else "fewer")
              <> " type arguments, but "
              <> kindToStr expectedKind <> " was expected here."
          ]

  NoMain ->
    Diagnose.Err
      Nothing
      "Missing 'main' function"
      []
      [ Diagnose.Hint "Add a 'main' function to your entry module."
      , Diagnose.Note "The simplest valid main:\n  main = () => { IO.log(\"Hello!\") }"
      ]

  MainInvalidTyping ->
    mkError "Invalid 'main' signature" context
      "The 'main' function has the wrong type. It must accept 'List String' and return '{}'."
      [ Diagnose.Hint "Change the signature to:  main :: List String -> {}"
      , Diagnose.Note "You can also omit the type annotation entirely and let it be inferred."
      ]

  MutationRestriction ->
    mkError "Mutation restriction" context
      "This binding depends on a closure that performs mutation and cannot be made polymorphic."
      [ Diagnose.Hint "Add an explicit type annotation with concrete (non-polymorphic) types."
      , Diagnose.Note "Mutation requires a fixed memory location; polymorphic types would create separate copies."
      ]

  MutatingFunction n ->
    mkError "Cannot mutate function" context
      ("'" <> n <> "' is a function and cannot be mutated with ':='.")
      [ Diagnose.Note "Functions are immutable values. You cannot reassign them."
      , Diagnose.Hint $ "To make '" <> n <> "' swappable, wrap it in a mutable record:\n"
          <> "  ref = { fn: " <> n <> " }\n"
          <> "  ref.fn := newFunction    // mutate the record field"
      ]

  NotAConstructor n ->
    let capitalHint =
          if not (null n) && (head n >= 'a' && head n <= 'z') then
            [Diagnose.Note $ "'" <> n <> "' starts with a lowercase letter. Constructors must be capitalized (e.g. 'Just', 'Nothing', 'Left')."]
          else
            []
    in  mkError "Not a constructor" context
          ("'" <> n <> "' is used in a pattern but is not a constructor.")
          ( capitalHint ++
            [ Diagnose.Hint "Only data constructors (capitalized names) can appear in patterns."
            , Diagnose.Note "Example pattern:  where | Just x => ...   | Nothing => ..."
            ]
          )

  MethodNameAlreadyDefined ->
    mkError "Method name already defined" context
      "This method name is already used by another interface."
      [ Diagnose.Note "Two interfaces cannot define methods with the same name."
      , Diagnose.Hint "Choose a different name, or prefix it with the interface name to avoid collisions."
      ]

  NotADefinition ->
    mkError "Not a definition" context
      "This expression appears at the top level, but only definitions are allowed here."
      [ Diagnose.Note "A module's top level can only contain: type definitions, assignments, imports, exports, and interface/instance declarations."
      , Diagnose.Hint "Assign it to a name:  myValue = <expression>"
      ]

  ConstructorAccessBadIndex typeName constructorName arity index ->
    mkError "Constructor index out of range" context
      (    "The constructor '" <> constructorName <> "' from '" <> typeName <> "' has "
        <> show arity <> " parameter" <> (if arity == 1 then "" else "s")
        <> ", but you are trying to access index " <> show index <> "."
      )
      [ Diagnose.Hint $ "Valid indices for '" <> constructorName <> "' are 0 to " <> show (arity - 1) <> "."
      , Diagnose.Note "Constructor parameter access uses zero-based indexing."
      ]

  ConstructorAccessNoConstructorFound typeName ->
    mkError "Constructor not found" context
      ("The type '" <> typeName <> "' has no constructors that can be accessed this way.")
      [ Diagnose.Hint "Use pattern matching to destructure the value instead." ]

  ConstructorAccessTooManyConstructors typeName _ ->
    mkError "Ambiguous constructor access" context
      ("Cannot access a constructor parameter of '" <> typeName <> "' because it has more than one constructor.")
      [ Diagnose.Hint "Use pattern matching to safely handle each constructor case."
      , Diagnose.Note "Constructor parameter access only works on types with a single constructor."
      ]

  InfiniteType tv t ->
    let (vars, hkVars, printedT) = prettyPrintType' True (mempty, mempty) t
        (_, _, printedN)         = prettyPrintType' True (vars, hkVars) (TVar tv)
    in  mkError "Infinite type" context
          (    "I can't construct the type '" <> printedN <> "' because it would need to contain itself:\n"
            <> "  " <> printedN <> " = " <> printedT
          )
          [ Diagnose.Note $
              "This happens when the type checker tries to unify a type variable with\n"
              <> "a type that contains that same variable, creating an infinite loop."
          , Diagnose.Hint "Common causes:"
          , Diagnose.Note $
              "  1. A function applied to itself: f(f) — wrap it in a lambda: f((_) => f)\n"
              <> "  2. A recursive data structure without a type alias\n"
              <> "  3. A missing type annotation on a recursive function"
          ]

  IllegalSkipAccess ->
    mkError "Cannot use '_' as a value" context
      "The skip symbol '_' is a placeholder for values you don't need. It cannot be read."
      [ Diagnose.Hint "Give it a name if you need to use the value:  (x) => x  instead of  (_) => _"
      , Diagnose.Note "'_' means 'I don't care about this value'. To use it, replace '_' with a named variable."
      ]

  UnboundVariable n suggestions ->
    let typoHint = case suggestions of
                     []  -> [Diagnose.Hint "Check for a typo in the name."]
                     [s] -> [Diagnose.Hint $ "Did you mean '" <> s <> "'?"]
                     _   -> [Diagnose.Hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
        stdlibHint = case List.lookup n stdlibMap of
                       Just modName -> [Diagnose.Note $ "'" <> n <> "' is defined in the '" <> modName <> "' module. Add: import " <> modName <> " from \"" <> modName <> "\""]
                       Nothing      -> [Diagnose.Note $ "If '" <> n <> "' is from another module, make sure you have imported it."]
    in  mkError "Unbound variable" context
          ("'" <> n <> "' is not defined in this scope.")
          (typoHint ++ stdlibHint)

  UnboundUnknownTypeVariable ->
    mkError "Unbound type variable" context "A type variable has not been declared"
      [Diagnose.Hint "Verify that you don't have a typo"]

  UnboundVariableFromNamespace namespace name ->
    mkError "Name not in module" context
      ("'" <> name <> "' was not found in '" <> namespace <> "'.")
      [ Diagnose.Hint $ "Check that '" <> name <> "' is exported from the module you imported as '" <> namespace <> "'."
      , Diagnose.Note $ "With a default import 'import List from \"List\"', use 'List.map', 'List.length', etc."
      ]

  CapitalizedADTTVar adtname param ->
    let lowered = if null param then param else map toLower param
    in  mkError "Capitalized ADT variable" context
          (    "The type parameter '" <> param <> "' in the type declaration\n"
            <> "'" <> adtname <> "' is capitalized. Type parameters can't be capitalized."
          )
          [ Diagnose.Hint "Either remove it if you don't need the type variable, or\nmake its first letter lowercase."
          , Diagnose.Hint $ "Change '" <> param <> "' to '" <> lowered <> "'"
          ]

  UnboundType n suggestions ->
    let typoHint = case suggestions of
                     []  -> [ Diagnose.Hint "Check for a typo in the type name."
                             , Diagnose.Note $ "If '" <> n <> "' is defined in another module, import it: 'import Type from \"./Module\"'"
                             ]
                     [s] -> [Diagnose.Hint $ "Did you mean '" <> s <> "'?"]
                     _   -> [Diagnose.Hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkError "Unknown type" context
          ("The type '" <> n <> "' is not defined in this scope.")
          typoHint

  ByteOutOfBounds n ->
    mkError "Byte out of bounds" context
      ("The literal '" <> n <> "_b' is too big, the maximum value for bytes is 255")
      [ Diagnose.Hint "Use a value between 0 and 255, or use 'Integer' for larger numbers." ]

  ShortOutOfBounds n ->
    mkError "Short out of bounds" context
      ("The literal '" <> n <> "_s' is too big, the maximum value for shorts is "<> show (2^31 - 1 :: Integer) <>".")
      [ Diagnose.Hint "Use a value within the short range, or use 'Integer' for larger numbers." ]

  IntOutOfBounds n ->
    mkError "Integer out of bounds" context
      ("The literal '" <> n <> "_i' is too big, the maximum value for integers is "<> show (2^63 - 1 :: Integer) <>".")
      [ Diagnose.Hint "Use a Float for very large numbers, or restructure the computation." ]

  NegatedByte ->
    mkError "Negated byte" context "Bytes can't be negated"
      [ Diagnose.Note "Bytes are unsigned integers in range 0-255 and cannot be negative."
      , Diagnose.Hint "Use 'Integer' or 'Short' if you need signed numbers."
      ]

  DerivingAliasNotAllowed n ->
    mkError "Cannot derive for type alias" context
      ("'" <> n <> "' is a type alias, not a concrete type. You cannot derive instances for aliases.")
      [ Diagnose.Hint "Derive the instance on the underlying type that the alias refers to."
      , Diagnose.Note "Type aliases are transparent — instances on the underlying type apply automatically."
      ]

  InvalidInterfaceDerived n ->
    mkError ("Cannot derive '" <> n <> "'") context
      ("The interface '" <> n <> "' does not support automatic derivation.")
      [ Diagnose.Note "Only these interfaces can be derived: Eq, Show, Comparable."
      , Diagnose.Hint $ "Write a manual instance instead:\n  instance " <> n <> " YourType { ... }"
      ]

  SignatureTooGeneral scGiven scInferred ->
    let (scInferred', scGiven') = renderSchemesWithDiff color scInferred scGiven
        scGiven''    = unlines $ ("  "<>) <$> lines scGiven'
        scInferred'' = unlines $ ("  "<>) <$> lines scInferred'
        givenStr     = if color then "\x1b[0mType signature given:\n" else "Type signature given:\n  "
        inferredStr  = if color then "\n\x1b[0mType inferred:\n" else "\nType inferred:\n  "
    in  mkError "Signature too general" context
          (givenStr <> scGiven'' <> inferredStr <> scInferred'')
          [ Diagnose.Note $
              "The annotation claims the function is more polymorphic than the implementation allows.\n"
              <> "The inferred type is more specific — it uses concrete types or fewer type variables."
          , Diagnose.Hint "Update the type annotation to match the inferred type shown above."
          ]

  NoInstanceFound cls ts ->
    let typeStr    = unwords (prettyPrintType True <$> ts)
        predStr    = lst (predToStr True (mempty, mempty) (IsIn cls ts Nothing))
        smartHints = noInstanceSmartHints cls ts
        stdHints   =
          [ Diagnose.Hint $ "Make sure '" <> typeStr <> "' implements the '" <> cls <> "' interface."
          , Diagnose.Note $ "Instance methods are automatically in scope when their module is imported,\ndirectly or transitively."
          ]
    in  mkError "No instance found" context
          ("'" <> predStr <> "' is required here, but no instance was found for '" <> typeStr <> "'.")
          (smartHints ++ stdHints)

  AmbiguousType (TV _ _, IsIn cls _ maybeArea : _) ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Ambiguous type"
          (
            ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "An instance of '" <> cls <> "' could not be found"
            ) : case maybeArea of
                  Just (Area (Loc _ startL' startC') (Loc _ endL' endC')) ->
                    [ ( Diagnose.Position (startL', startC') (endL', endC') modulePath
                      , Diagnose.Where "The constraint originates from here"
                      )
                    ]

                  Nothing ->
                    []
          )
          [Diagnose.Hint "You can add a type annotation to make it resolvable."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Ambiguous type"
          []
          [Diagnose.Hint "You can add a type annotation to make it resolvable."]

  AmbiguousType (TV n _, []) ->
    mkError "Ambiguous type" context
      ("An ambiguity for the type variable '" <> renderTVar n <> "' could not be resolved")
      [Diagnose.Hint "You can add a type annotation to make it resolvable."]

  InterfaceNotExisting cls ->
    mkError "Interface not found" context ("The interface '" <> cls <> "' is not defined.\n")
      [Diagnose.Hint "Make sure you imported the module defining it,\nor a module that imports it."]

  KindError (t, k) (t', k') ->
    mkError "Kind mismatch" context
      (    "'" <> prettyPrintType True t <> "' has kind " <> kindToStr k
        <> ",\nbut '" <> prettyPrintType True t' <> "' has kind " <> kindToStr k' <> "."
      )
      [ Diagnose.Note $
          "Kinds describe how many type arguments a type constructor takes.\n"
          <> "'*' means a fully-applied type (like 'Int' or 'String').\n"
          <> "'* -> *' means a type that takes one argument (like 'List' or 'Maybe')."
      , Diagnose.Hint "Check whether you applied too many or too few type arguments."
      ]

  InstancePredicateError pInstance pWrong pCorrect ->
    let instStr    = lst (predToStr True (mempty, mempty) pInstance)
        wrongStr   = lst (predToStr True (mempty, mempty) pWrong)
        correctStr = lst (predToStr True (mempty, mempty) pCorrect)
    in  mkError "Instance constraint error" context
          (    "The instance '" <> instStr <> "' has an incorrect constraint.\n"
            <> "  Given:    " <> wrongStr <> "\n"
            <> "  Expected: " <> correctStr
          )
          [ Diagnose.Hint $ "Replace '" <> wrongStr <> "' with '" <> correctStr <> "' in the instance declaration."
          , Diagnose.Note $
              "Instance constraints must use the same type variables as the instance head,\n"
              <> "and must match the shape required by the interface definition."
          ]

  ImportCycle paths ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Circular import"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "This import creates a cycle:\n\n"
                <> buildCycleOutput paths
            )
          ]
          [ Diagnose.Note "Circular imports are not allowed because there is no valid initialization order."
          , Diagnose.Hint "Extract the shared types or functions into a third module that both can import."
          , Diagnose.Hint "Alternatively, move the code that causes the cycle into one of the two modules."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Circular import"
          []
          [ Diagnose.Note "Circular imports are not allowed because there is no valid initialization order."
          , Diagnose.Hint "Extract the shared types or functions into a third module that both can import."
          ]
   where
    buildCycleOutput :: [FilePath] -> String
    buildCycleOutput [] = ""
    buildCycleOutput ps =
      let loop [] = ""
          loop [p]    = "  " <> p <> "\n  ↓  (loops back to start)"
          loop (p:rest) = "  " <> p <> "\n  ↓\n" <> loop rest
      in  loop ps

  TypeAnnotationNameMismatch typingName expName ->
    mkError "Type annotation name mismatch" context
      (    "The type annotation is for '" <> typingName <> "' but the following definition is for '" <> expName <> "'."
      )
      [ Diagnose.Hint $ "Rename the annotation to match: '" <> expName <> " :: <type>'"
      , Diagnose.Note "A type annotation must immediately precede the definition it annotates and share the same name."
      ]

  GrammarError _ msg ->
    let cleanMsg = if null msg then "Unexpected token" else msg
        trimmed  = List.dropWhileEnd (\c -> c == '\n' || c == '\r') cleanMsg
        smartHints = grammarSmartHints trimmed
        stdHints =
          [ Diagnose.Hint "Check for a missing bracket, parenthesis, or operator near this location."
          , Diagnose.Note "Common causes: unclosed '(', '{', or '['; a missing '->' in a function; or a typo in a keyword."
          ]
        -- Only include generic hints when no smart hints are available
        hints = if null smartHints then stdHints else smartHints
    in  mkError "Syntax error" context trimmed hints

  BadEscapeSequence ->
    mkError "Bad escape sequence" context "This escape sequence is not valid"
      [ Diagnose.Hint "Valid escape sequences are either a byte: \\xAB or a unicode: \\uABCD or \\u{ABCDEF} up to 10FFFF"
      ]

  EmptyChar ->
    mkError "Empty Char" context "This character is empty"
      [ Diagnose.Note "Characters can't be empty"
      ]

  UnknownType t suggestions ->
    let hint = case suggestions of
                 []  -> [Diagnose.Hint "Verify that you imported it"]
                 [s] -> [Diagnose.Hint $ "Did you mean '" <> s <> "'?"]
                 _   -> [Diagnose.Hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkError "Unknown type" context ("The type '" <> t <> "' was not found") hint

  NameAlreadyDefined name ->
    mkError "Illegal shadowing" context ("The variable '" <> name <> "' is already defined")
      [ Diagnose.Hint "Change the name of the variable"
      , Diagnose.Note $
          "The variable might be defined further down. All top level\n"
          <> "assignments share the scope and using a local name that is\n"
          <> "defined in the global scope of a module is not allowed."
      ]

  ImportCollision name ->
    mkError "Import collision" context
      ("The imported name '" <> name <> "' is already used")
      [Diagnose.Hint "Use a qualified import or rename one of the conflicting names."]

  TypeAlreadyDefined name ->
    mkError "Type already defined" context ("The type '" <> name <> "' is already defined")
      [Diagnose.Hint "Change the name of the type"]

  NameAlreadyExported name ->
    mkError "Already exported" context
      ("'" <> name <> "' appears more than once in the export list.")
      [ Diagnose.Hint $ "Remove the duplicate export of '" <> name <> "'."
      , Diagnose.Note "Each name can only be exported once from a module."
      ]

  NotExported name path suggestions ->
    let hint = case suggestions of
                 []  -> [ Diagnose.Hint $ "Add 'export { " <> name <> " }' to the module at '" <> path <> "' if you own it."
                        , Diagnose.Note "Or check the module's documentation to find the correct exported name."
                        ]
                 [s] -> [Diagnose.Hint $ "Did you mean '" <> s <> "'?"]
                 _   -> [Diagnose.Hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkError "Not exported" context
          (    "'" <> name <> "' is not exported by the module at:\n"
            <> "'" <> path <> "'"
          )
          hint

  RecursiveVarAccess _ ->
    mkError "Recursive variable access" context
      (    "This variable refers to itself directly in its own definition\n"
        <> "and cannot be initialized."
      )
      [ Diagnose.Note $
          "A direct self-reference is not allowed. Wrap the recursive\n"
          <> "access in a function to defer evaluation:\n"
          <> "  // Not allowed -- direct self-reference:\n"
          <> "  parser = J.map(Title, J.field(\"title\", parser))\n"
          <> "  // Works -- recursive access is wrapped in a function:\n"
          <> "  parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser)))"
      ]

  NotInScope name (Loc _ line _) ->
    mkError "Not in scope" context
      (    "'" <> name <> "' (at line " <> ppShow line <> ") is not yet defined at this point."
      )
      [ Diagnose.Note "All variables must be defined before they are used."
      , Diagnose.Hint $
          "Move the definition of '" <> name <> "' above this usage, or add a type\n"
          <> "annotation to allow forward references."
      ]

  TypesHaveDifferentOrigin adtName origin1 origin2 ->
    mkError "Types have different origins" context
      (    "The type '" <> adtName <> "' is imported from two different locations:\n"
        <> "  - '" <> origin1 <> "'\n"
        <> "  - '" <> origin2 <> "'\n"
        <> "These are treated as distinct types even though they share a name."
      )
      [ Diagnose.Hint $ "Import '" <> adtName <> "' from only one location, or convert between the two explicitly."
      ]

  ShouldBeTypedOrAbove name ->
    mkError "Must be typed or above" context
      ("'" <> name <> "' is used before it is defined.")
      [ Diagnose.Note $
          "This is fine, but in that case you must give it a type\n"
          <> "annotation."
      , Diagnose.Hint $
          "Place that declaration above the place you use it, or give\n"
          <> "it a type annotation."
      ]

  NotCapitalizedADTName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  mkError "ADT name not capitalized" context
          ("The name '" <> name <> "' of this type is not capitalized")
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          , Diagnose.Hint $ "Change it to '" <> capitalized <> "'"
          ]

  NotCapitalizedAliasName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  mkError "Alias name not capitalized" context
          ("The name '" <> name <> "' of this type alias is not capitalized")
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          , Diagnose.Hint $ "Change it to '" <> capitalized <> "'"
          ]

  NotCapitalizedConstructorName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  mkError "Constructor name not capitalized" context
          ("The name '" <> name <> "' of this type constructor is not capitalized")
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          , Diagnose.Hint $ "Change it to '" <> capitalized <> "'"
          ]

  ContextTooWeak preds ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        let positionInfos =
              Maybe.mapMaybe
                (\p@(IsIn _ _ maybeArea) ->
                  case maybeArea of
                    Just (Area (Loc _ startL startC) (Loc _ endL endC)) ->
                      let (_, _, rendererPred) = predToStr True (mempty, mempty) p
                      in  Just
                            ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                            , Diagnose.This $ "The constraint '" <> rendererPred <> "' originates from here"
                            )

                    Nothing ->
                      Nothing
                )
                preds
        in  Diagnose.Err
              Nothing
              "Context too weak"
              (
                [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                  , Diagnose.This $
                      "The context of the type annotation is too weak. The type\n"
                      <> "inferred for the implementation has constraints\n"
                      <> "for the following instances: " <> intercalate ", " (predClass <$> preds)
                  )
                ] ++ positionInfos
              )
          [ Diagnose.Hint  "Add the missing interface constraints to the type annotation."
          , Diagnose.Note $
              "Example: if the constraint 'Eq a' is missing, change\n"
              <> "  'myFn :: a -> Boolean'  to  'myFn :: Eq a => a -> Boolean'"
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Context too weak"
          []
          [ Diagnose.Hint "Add the missing interface constraints to the type annotation."
          , Diagnose.Note $
              "Example: if the constraint 'Eq a' is missing, change\n"
              <> "  'myFn :: a -> Boolean'  to  'myFn :: Eq a => a -> Boolean'"
          ]

  OverloadedMutation n preds ->
    let predNames = intercalate ", " (predClass <$> preds)
    in  mkError "Mutation in overloaded context" context
          ("Cannot mutate '" <> n <> "' because this function has type class constraints (" <> predNames <> ").")
          [ Diagnose.Note $
              "In an overloaded function, each specialisation creates a new closure copy,\n"
              <> "so mutations to '" <> n <> "' would be invisible to callers."
          , Diagnose.Hint "Add a concrete type annotation to remove the polymorphism, or refactor to avoid mutation here."
          ]

  WrongAliasArgCount aliasName expected actual ->
    let exampleArgs = unwords (map (\i -> "Type" <> show i) [1..expected])
        exampleNote = "Example: if '" <> aliasName <> "' takes " <> show expected <> " argument(s), write: " <> aliasName <> " " <> exampleArgs
    in  mkError "Wrong alias argument count" context
          (    "The alias '" <> aliasName <> "' was expected to have " <> show expected <> " argument" <> (if expected > 1 then "s" else "") <> ",\nbut "
            <> show actual <> " "<> (if actual > 1 then "were" else "was") <>" given"
          )
          [ Diagnose.Hint $
              if actual > expected then
                "Remove " <> show (actual - expected) <> " argument(s)"
              else
                "Add the missing '" <> show (expected - actual) <> "' argument(s)"
          , Diagnose.Note exampleNote
          ]

  ImportNotFound importName ->
    let isRelative = List.isPrefixOf "./" importName || List.isPrefixOf "../" importName
        hints =
          if isRelative then
            [ Diagnose.Hint $ "The file '" <> importName <> ".mad' could not be found relative to this module."
            , Diagnose.Note "Check the file path and make sure the file exists."
            ]
          else
            [ Diagnose.Hint $ "The package '" <> importName <> "' could not be found."
            , Diagnose.Note "Run 'madlib install' to install missing dependencies, or check 'madlib.json'."
            ]
    in  mkError "Import not found" context
          ("The module '" <> importName <> "' could not be found.")
          hints

  InterfaceAlreadyDefined interfaceName ->
    mkError "Interface already defined" context
      ("You defined the interface '" <> interfaceName <> "',\nbut it already exists")
      [Diagnose.Hint "Verify that you don't have a typo."]

  InvalidLhs ->
    mkError "Invalid left hand side" context "It is not a valid left hand side expression."
      [ Diagnose.Note "The left-hand side of '=' must be a variable name, a record pattern, or a list pattern."
      , Diagnose.Hint "Valid forms: x = expr, { field } = record, [first] = list"
      ]

  BadMutation ->
    mkError "Bad mutation" context
      "Cannot reassign a variable with '='. Use ':=' to mutate an existing binding."
      [ Diagnose.Hint "Use the mutation operator ':=' to change an existing value."
      , Diagnose.Note "Example:  x = 0         // initial binding\n         x := x + 1    // mutation"
      ]

  MutatingNotInScope name ->
    mkError "Not in scope" context
      ("Cannot mutate '" <> name <> "' because it is not in scope.")
      [ Diagnose.Hint $ "Declare '" <> name <> "' before mutating it, or check for a typo."
      , Diagnose.Note "The variable must be in scope (declared earlier in the same block or outer scope)."
      ]

  MutatingPatternBoundVariable name ->
    mkError "Cannot mutate pattern-bound variable" context
      ("'" <> name <> "' is bound by pattern matching and cannot be mutated.")
      [Diagnose.Hint $ "Introduce a local let binding first: " <> name <> " = <patternVar>, then use ':=' on that."]

  ADTAlreadyDefined adtType ->
    let adtName = renderType adtType
    in  mkError "Type already defined" context
          ("You defined the type '" <> adtName <> "',\nbut it already exists")
          [Diagnose.Hint "Verify that you don't have a typo."]

  RecordDuplicateFields fs ->
    let fs' = concatMap ("\n - " ++) fs
    in  mkError "Record duplicate fields" context
          ("The following fields appear more than once in the record constructor:" <> fs')
          [Diagnose.Hint "Define each field only once."]

  RecordMissingFields fs ->
    let fieldList  = intercalate ", " (map (\f -> "'" <> f <> "'") fs)
        (one, were, them) = if length fs == 1 then ("field", "was", "it") else ("fields", "were", "them")
    in  mkError "Record missing fields" context
          ("The record " <> were <> " missing the " <> one <> ": " <> fieldList)
          [ Diagnose.Hint $ "Add " <> them <> " to the record literal, for example: { " <> List.intercalate ", " (map (\f -> f <> ": <value>") fs) <> " }"
          ]

  RecordExtraFields fs availableFields ->
    let fieldList = intercalate ", " (map (\f -> "'" <> f <> "'") fs)
        suggestions = concatMap (\f ->
          let similar = findSimilar f availableFields
          in  case similar of
                []  -> []
                [s] -> [f <> " -> " <> s]
                _   -> [f <> " -> one of: " <> intercalate ", " similar]
          ) fs
        hint = case suggestions of
                 [] -> [Diagnose.Hint "Remove the extra fields or check for a typo in a field name."]
                 _  -> [Diagnose.Hint $ "Did you mean: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkError "Record extra fields" context
          ("The record has unexpected fields: " <> fieldList)
          hint

  RecordDuplicateRestPattern ->
    mkError "Duplicate rest pattern" context
      "A record pattern can only have one rest/spread pattern ('...')."
      [ Diagnose.Hint "Remove the extra '...' and keep only one."
      , Diagnose.Note "Example:  { x, ...rest } = myRecord  -- only one spread allowed"
      ]

  WrongSpreadType t ->
    mkError "Type error" context t
      [ Diagnose.Note "The spread operator '...' is only valid on record types."
      , Diagnose.Hint "Check that the value you are spreading is a record, or remove the spread."
      ]

  FatalError ->
    mkError "Internal compiler error" context
      "The compiler encountered an unexpected internal state and could not continue."
      [ Diagnose.Hint "This is likely a compiler bug. Please report it with the code that triggered it."
      , Diagnose.Note "You can try reorganizing the problematic expression or adding a type annotation."
      ]

  Error ->
    mkError "Error" context "An error occurred during compilation."
      [ Diagnose.Hint "Check the surrounding code for type mismatches or missing imports." ]

  ASTHasNoPath ->
    mkError "Module not found" context
      "A required module could not be located or loaded."
      [ Diagnose.Hint "Verify that all imports resolve to existing files."
      , Diagnose.Note "If this is a package dependency, run 'madlib install' to fetch it."
      ]


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
