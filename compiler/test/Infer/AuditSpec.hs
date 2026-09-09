module Infer.AuditSpec where

import Test.Hspec
import Infer.SolveSpec (inferModuleWithoutMain)
import Error.Error
import Error.Context
import Explain.Location (getLineFromStart)
import Control.Monad (forM_)

spec :: Spec
spec = describe "typechecker audit" $ do
  forM_ ["rest.x", "((saved) => saved.x)(rest)"] $ \body ->
    it ("rejects removed field through " ++ body) $ do
      (_, _, errors) <- inferModuleWithoutMain $ unlines
        [ "bad = (r) => where(r) {"
        , "  { x, ...rest } => " ++ body
        , "}"
        , "value :: String"
        , "value = bad({x: true})"
        ]
      errors `shouldSatisfy` any missingX
  it "rejects a removed field after returning and generalizing the rest" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "remove = (r) => where(r) { {x, ...rest} => rest }"
      , "bad = (r) => remove(r).x"
      , "value :: String"
      , "value = bad({x: true})"
      ]
    errors `shouldSatisfy` any missingX
  it "preserves unrelated fields in record rest" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "remove = (r) => where(r) { {x, ...rest} => rest }"
      , "value :: String"
      , "value = remove({x: true, y: \"ok\"}).y"
      ]
    errors `shouldBe` []
  forM_ [False, True] $ \reverseBranches ->
    it ("unifies shared row tails, reversed=" ++ show reverseBranches) $ do
      let branches = ["{...r, x: \"s\"}", "{...r, y: true}"]
          [a,b] = if reverseBranches then reverse branches else branches
      (_, _, errors) <- inferModuleWithoutMain $ unlines
        [ "f = (r) => if (true) { " ++ a ++ " } else { " ++ b ++ " }"
        , "value :: {x :: String, y :: Boolean}"
        , "value = f({x: \"old\", y: true})"
        ]
      errors `shouldBe` []
  it "rejects captured monomorphic variables escaping a local signature" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "f = (captured) => {"
      , "  bad :: a -> a"
      , "  bad = (_) => captured"
      , "  return bad(false)"
      , "}"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      LocalSignatureCapturesOuterType _ -> True
      _ -> False)
  it "preserves a valid polymorphic local identity" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "f = () => {"
      , "  identity :: a -> a"
      , "  identity = (x) => x"
      , "  first = identity(false)"
      , "  return identity(\"ok\")"
      , "}"
      ]
    errors `shouldBe` []
  it "accepts a polymorphic local helper when its outer dependency is explicit" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "f = (combine, values) => {"
      , "  pairs :: (a -> a -> a) -> List a -> List a"
      , "  pairs = (combineFn, xs) => where(xs) {"
      , "    [x, y, ...rest] => [combineFn(x, y), ...pairs(combineFn, rest)]"
      , "    remaining => remaining"
      , "  }"
      , "  return pairs(combine, values)"
      , "}"
      ]
    errors `shouldBe` []
  forM_ ["(a -> a)", "(#[a, a])", "({x :: a})"] $ \argument ->
    it ("canonicalizes constraint argument " ++ argument) $ do
      (_, _, errors) <- inferModuleWithoutMain $ unlines
        [ "f :: Eq " ++ argument ++ " => a -> a"
        , "f = (x) => x"
        ]
      errors `shouldBe` []
  it "resolves the intrinsic Number interface during canonicalization" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "increment :: Number a => a -> a"
      , "increment = (x) => x + 1"
      ]
    errors `shouldBe` []
  it "reports constraint arity without crashing" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "f :: Eq a a => a -> a"
      , "f = (x) => x"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      WrongInterfaceArgCount "Eq" 1 2 -> True
      _ -> False)
  it "reports constraint kind errors without crashing" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "interface Higher f { apply :: f a -> a }"
      , "bad :: Higher Integer => Integer"
      , "bad = 1"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      TypingHasWrongKind _ _ _ -> True
      _ -> False)
  it "checks multiparameter superclass evidence" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "interface Relation a b { relation :: a -> b -> Boolean }"
      , "interface Relation a a => Thing a { thing :: a -> Boolean }"
      , "f :: Thing a => a -> Boolean"
      , "f = (x) => relation(x, x)"
      ]
    errors `shouldBe` []
  it "reports superclass arity at the declaration" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "interface Relation a b { relation :: a -> b -> Boolean }"
      , "interface Relation a => Thing a { thing :: a -> Boolean }"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      WrongInterfaceArgCount "Relation" 2 1 -> True
      _ -> False)
  it "reports a superclass cycle" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "interface B a => A a { aMethod :: a -> Boolean }"
      , "interface A a => B a { bMethod :: a -> Boolean }"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      SuperclassCycle _ -> True
      _ -> False)
  it "rejects an ill-kinded superclass argument" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "interface Parent a { parent :: a -> Boolean }"
      , "interface Parent List => Child a { child :: a -> Boolean }"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      TypingHasWrongKind _ _ _ -> True
      _ -> False)
  it "rejects an insufficient local constraint context" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "f = () => {"
      , "  equal :: a -> Boolean"
      , "  equal = (x) => x == x"
      , "  return equal(false)"
      , "}"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      ContextTooWeak _ -> True
      _ -> False)
  it "rejects incompatible visible fields in shared rows" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "f = (r) => if (true) { {...r, x: \"s\"} } else { {...r, x: true} }"
      , "value = f({x: \"old\", y: true})"
      ]
    errors `shouldSatisfy` any (\(CompilationError e _) -> case e of
      UnificationError _ -> True
      _ -> False)
  it "defaults only absent optional JSX props" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "import type { Maybe } from \"Maybe\""
      , "component :: { name :: String, optional :: Maybe String, children :: List String } -> String"
      , "component = (props) => props.name"
      , "value = <component name=\"ok\" />"
      ]
    errors `shouldBe` []
  it "rejects missing required JSX props alongside optional props" $ do
    (_, _, errors) <- inferModuleWithoutMain $ unlines
      [ "import type { Maybe } from \"Maybe\""
      , "component :: { name :: String, optional :: Maybe String, children :: List String } -> String"
      , "component = (props) => props.name"
      , "value = <component />"
      ]
    errors `shouldSatisfy` any (\(CompilationError e context) -> case (e, context) of
      (RecordMissingFields ["name"] _, Context "Module.mad" area) -> getLineFromStart area == 4
      _ -> False)
  where
    missingX (CompilationError (RecordMissingFields labels _) (Context "Module.mad" area)) =
      "x" `elem` labels && getLineFromStart area > 0
    missingX _ = False
