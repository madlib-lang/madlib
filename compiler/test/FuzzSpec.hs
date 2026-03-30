module FuzzSpec where

import Test.Hspec
import Fuzz.Generator
import Fuzz.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B


spec :: Spec
spec = do
  describe "Fuzz generator determinism" $ do
    it "generates identical model and source for the same seed" $ do
      let a = generateProgramModel 1337 Balanced 10
      let b = generateProgramModel 1337 Balanced 10
      a `shouldBe` b
      renderProgram a `shouldBe` renderProgram b

  describe "Fuzz shrinker" $ do
    it "returns candidates that are not larger than the original list size" $ do
      let m = generateProgramModel 7 AllocationHeavy 12
      let cs = shrinkProgramModel m
      all (\c -> length (pmListVals c) <= length (pmListVals m)) cs `shouldBe` True

  describe "Fuzz artifact JSON" $ do
    it "round-trips run results through JSON" $ do
      let outcome backend =
            BackendOutcome
              { boBackend = backend
              , boCompileOk = True
              , boRunOk = True
              , boTimedOut = False
              , boExitCode = Just 0
              , boStdout = "OK"
              , boStderr = ""
              , boCompileOutput = ""
              }
      let rr =
            RunResult
              { rrSeed = 1
              , rrRunIndex = 0
              , rrProfile = Balanced
              , rrSource = "main = () => { IO.putLine(\"x\") }"
              , rrReducedSource = Nothing
              , rrLLVM = outcome "llvm"
              , rrNode = outcome "node"
              , rrClassification = Match
              , rrShrinkSteps = []
              }
      let encoded = Aeson.encode rr
      Aeson.decode encoded `shouldBe` Just rr
      B.length encoded `shouldSatisfy` (> 0)
