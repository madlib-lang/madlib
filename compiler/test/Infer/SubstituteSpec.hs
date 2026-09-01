module Infer.SubstituteSpec where

import qualified Data.Map        as M
import qualified Data.Set        as S
import           Test.Hspec       (Spec, describe, it, shouldBe)

import           Infer.Env        (Env(..))
import           Infer.EnvUtils   (extendVars)
import           Infer.Substitute (Substitutable(apply), compose)
import           Infer.Type


spec :: Spec
spec = do
  describe "substitution composition" $ do
    it "applies the old substitution before the new substitution" $ do
      let a = TV 0 Star
          b = TV 1 Star
          old = M.singleton a (TVar b)
          new = M.singleton b tInteger
          source = tTuple2Of (TVar a) (TVar b)
      apply (compose new old) source `shouldBe` apply new (apply old source)

    it "obeys the composition law when domains overlap" $ do
      let a = TV 0 Star
          b = TV 1 Star
          old = M.singleton a (TVar b)
          new = M.fromList [(a, tStr), (b, tInteger)]
          source = tTuple2Of (TVar a) (TVar b)
      apply (compose new old) source `shouldBe` apply new (apply old source)

    it "produces an idempotent normalized substitution" $ do
      let a = TV 0 Star
          b = TV 1 Star
          old = M.singleton a (TVar b)
          new = M.singleton b tInteger
          normalized = compose new old
          source = tTuple2Of (TVar a) (TVar b)
      apply normalized (apply normalized source) `shouldBe` apply normalized source

  describe "row substitution" $ do
    it "preserves an outer label that shadows the same label in its tail" $ do
      let r = TV 2 Row
          substitution = M.singleton r (TRowExtend "x" tStr TRowEmpty)
          source = recordRow (TRowExtend "x" tInteger (TVar r))
          expected = recordRow
            (TRowExtend "x" tInteger (TRowExtend "x" tStr TRowEmpty))
      apply substitution source `shouldBe` expected

    it "removes labels structurally while preserving the tail and shadowing" $ do
      let tailVar = TV 3 Row
          source = recordRow
            (TRowExtend "x" tInteger
              (TRowExtend "y" tBool
                (TRowExtend "x" tStr (TVar tailVar))))
          expected = recordRow (TRowExtend "y" tBool (TVar tailVar))
      removeRecordLabels (S.singleton "x") source `shouldBe` expected

  describe "environment free-variable caches" $ do
    it "removes stale free variables when an open binding is replaced" $ do
      let a = TV 3 Star
          openEnv = extendVars emptyEnv ("value", Forall [] ([] :=> TVar a))
          closedEnv = extendVars openEnv ("value", Forall [] ([] :=> tInteger))
      envFreeTVars closedEnv `shouldBe` S.empty
      envOpenVarNames closedEnv `shouldBe` S.empty

    it "tracks free variables introduced by a substitution range" $ do
      let a = TV 4 Star
          b = TV 5 Star
          env = extendVars emptyEnv ("value", Forall [] ([] :=> TVar a))
          substituted = apply (M.singleton a (tListOf $ TVar b)) env
      envFreeTVars substituted `shouldBe` S.singleton b
      envOpenVarNames substituted `shouldBe` S.singleton "value"


emptyEnv :: Env
emptyEnv = Env
  { envVars = mempty
  , envInterfaces = mempty
  , envConstructors = mempty
  , envMethods = mempty
  , envCurrentPath = ""
  , envInBody = False
  , envDeferBodyAmbiguity = False
  , envNamesInScope = mempty
  , envNamespacesInScope = mempty
  , envImportInfo = mempty
  , envPlaceholdersInScope = mempty
  , envPlaceholdersToDelete = mempty
  , envPatternBoundNames = mempty
  , envFreeTVars = mempty
  , envOpenVarNames = mempty
  , envBuiltinsModulePath = "prelude"
  }
