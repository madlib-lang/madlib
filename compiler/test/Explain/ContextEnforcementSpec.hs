module Explain.ContextEnforcementSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldSatisfy
                                                )
import           Control.Monad                  ( forM_ )
import           Error.Error
import           Error.Context
import           Infer.SolveSpec                ( inferModule )


-- | Every reported error must carry a source location. The inference monad
-- tracks the nearest enclosing expression's span (see withCurrentSpan) so
-- even errors thrown deep inside unification or instance resolution get
-- stamped before reaching the user. This spec compiles a corpus of broken
-- programs covering the historical NoContext throw sites and asserts that
-- no locationless error escapes.
brokenPrograms :: [(String, String)]
brokenPrograms =
  [ ( "operator instance error"
    , unlines
        [ "main = () => {"
        , "  1 + \"a\""
        , "}"
        ]
    )
  , ( "annotation mismatch"
    , unlines
        [ "f :: Integer -> Integer"
        , "f = (x) => \"s\""
        , "main = () => { f(1) }"
        ]
    )
  , ( "non-boolean if condition"
    , unlines
        [ "main = () => {"
        , "  if (1) { 2 } else { 3 }"
        , "}"
        ]
    )
  , ( "if branches disagree"
    , unlines
        [ "main = () => {"
        , "  if (true) { 2 } else { \"a\" }"
        , "}"
        ]
    )
  , ( "mixed list element types"
    , unlines
        [ "main = () => {"
        , "  [1, \"a\"]"
        , "}"
        ]
    )
  , ( "missing record field"
    , unlines
        [ "f = (r) => r.name ++ \"!\""
        , "main = () => { f({ age: 3 }) }"
        ]
    )
  , ( "mutation with wrong type"
    , unlines
        [ "main = () => {"
        , "  x = 1"
        , "  x := \"hello\""
        , "}"
        ]
    )
  , ( "wrong constructor in pattern"
    , unlines
        [ "type T = A | B"
        , "main = () => {"
        , "  where(1) {"
        , "    A =>"
        , "      2"
        , ""
        , "    _ =>"
        , "      3"
        , "  }"
        , "}"
        ]
    )
  ]


spec :: Spec
spec =
  describe "Error context enforcement" $ do
    forM_ brokenPrograms $ \(name, code) ->
      it (name <> " reports located errors only") $ do
        (_, _, errors) <- inferModule code
        errors `shouldSatisfy` (not . null)
        forM_ errors $ \err ->
          err `shouldSatisfy` ((/= NoContext) . getContext)
