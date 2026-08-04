-- | The diagnostic intermediate representation: every compilation error and
-- warning is built into a 'Diagnostic' exactly once (see
-- "Explain.Diagnostic.Build") and then projected into the three output modes
-- (terminal, plain text / LSP, JSON) by the renderers under "Explain.Render".
--
-- The critical property of this IR is that types are carried *unrendered*
-- ('Type' / 'Scheme' values, not strings): color, diff highlighting and
-- layout decisions belong to the projections, never to message construction.
module Explain.Diagnostic where

import           Explain.Location               ( Area )
import           Infer.Type                     ( Kind
                                                , Scheme
                                                , Type
                                                )


data Severity
  = SevError
  | SevWarning
  deriving (Eq, Show)

-- | A source location a diagnostic points at.
data Span = Span
  { spanPath :: FilePath
  , spanArea :: Area
  }
  deriving (Eq, Show)

-- | Primary is the offending location ("This"); Secondary is supporting
-- context at another location ("Where"), e.g. the other if-branch or the
-- definition a usage conflicts with.
data MarkerStyle
  = Primary
  | Secondary
  deriving (Eq, Show)

data Marker = Marker
  { mSpan  :: Span
  , mStyle :: MarkerStyle
  , mLabel :: [Section]
  }
  deriving (Eq, Show)

-- | A piece of diagnostic prose. Types and kinds stay structured so each
-- renderer can decide how to print them (with or without color/diff).
data Section
  = P String                    -- ^ a paragraph of plain prose
  | ExpectedFound Type Type     -- ^ expected/found pair, rendered with a diff
  | ExpectedFoundKind Kind Kind -- ^ expected/found kinds, rendered green/red
  | GivenInferred Scheme Scheme -- ^ declared/inferred schemes (signatures)
  | ShowType Type               -- ^ a single type, pretty-printed
  | Verbatim String             -- ^ preformatted block (code examples, tables)
  deriving (Eq, Show)

data Note
  = Hint [Section]
  | Note [Section]
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { dSeverity :: Severity
  , dCode     :: Maybe String   -- ^ reserved for stable error codes; unused
  , dTitle    :: String
  , dMarkers  :: [Marker]       -- ^ empty when the error has no location
  , dBody     :: [Section]      -- ^ prose shown when there is no marker to label
  , dNotes    :: [Note]
  }
  deriving (Eq, Show)


-- Convenience builders ------------------------------------------------------

-- | An error diagnostic with a single primary marker.
errorAt :: Span -> String -> [Section] -> [Note] -> Diagnostic
errorAt span' title label notes = Diagnostic
  { dSeverity = SevError
  , dCode     = Nothing
  , dTitle    = title
  , dMarkers  = [Marker span' Primary label]
  , dBody     = []
  , dNotes    = notes
  }

-- | An error diagnostic with no usable source location.
errorNowhere :: String -> [Section] -> [Note] -> Diagnostic
errorNowhere title body notes = Diagnostic
  { dSeverity = SevError
  , dCode     = Nothing
  , dTitle    = title
  , dMarkers  = []
  , dBody     = body
  , dNotes    = notes
  }

-- | A single-sentence hint.
hint :: String -> Note
hint = Hint . pure . P

-- | A single-sentence note.
note :: String -> Note
note = Note . pure . P
