module FuzzSpec where

import Test.Hspec
import Fuzz.Generator
import Fuzz.Engine (classifyResult)
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

  describe "Fuzz renderer" $ do
    it "emits only the enabled feature blocks" $ do
      let model =
            ProgramModel
              { pmSeed = 1
              , pmProfile = Balanced
              , pmConsts = [1, 2, 3, 4, 5, 6]
              , pmListVals = [7, 8, 9]
              , pmUseClosure = True
              , pmUsePap3 = False
              , pmUsePap4 = True
              , pmUseRecursion = True
              , pmUseMap = False
              , pmUseFilter = True
              }
      let src = renderProgram model
      src `shouldContain` "FUZZ_BLOCK_BEGIN:closure"
      src `shouldContain` "FUZZ_BLOCK_BEGIN:pap4"
      src `shouldContain` "FUZZ_BLOCK_BEGIN:filter"
      src `shouldNotContain` "FUZZ_BLOCK_BEGIN:pap3"
      src `shouldNotContain` "FUZZ_BLOCK_BEGIN:map"
      src `shouldNotContain` "FUZZ_BLOCK_BEGIN:recursion"

  describe "Fuzz shrinker" $ do
    it "returns candidates that are not larger than the original list size" $ do
      let m = generateProgramModel 7 AllocationHeavy 12
      let cs = shrinkProgramModel m
      all (\c -> length (pmListVals c) <= length (pmListVals m)) cs `shouldBe` True

    it "disables each enabled feature in at least one candidate" $ do
      let m =
            ProgramModel
              { pmSeed = 7
              , pmProfile = AllocationHeavy
              , pmConsts = [1, 2, 3, 4, 5, 6]
              , pmListVals = [9, 8, 7, 6]
              , pmUseClosure = True
              , pmUsePap3 = True
              , pmUsePap4 = True
              , pmUseRecursion = True
              , pmUseMap = True
              , pmUseFilter = True
              }
      let cs = shrinkProgramModel m
      any (not . pmUseClosure) cs `shouldBe` True
      any (not . pmUsePap3) cs `shouldBe` True
      any (not . pmUsePap4) cs `shouldBe` True
      any (not . pmUseRecursion) cs `shouldBe` True
      any (not . pmUseMap) cs `shouldBe` True
      any (not . pmUseFilter) cs `shouldBe` True

  describe "Fuzz classification" $ do
    let outcome backend compileOk runOk timedOut exitCode stdout stderr =
          BackendOutcome
            { boBackend = backend
            , boCompileOk = compileOk
            , boRunOk = runOk
            , boTimedOut = timedOut
            , boExitCode = exitCode
            , boStdout = stdout
            , boStderr = stderr
            , boCompileOutput = ""
            }

    it "treats equal backend output as a match" $ do
      let llvm = outcome "llvm" True True False (Just 0) "OK\nDONE\n" ""
      let node = outcome "node" True True False (Just 0) "OK\nDONE\n" ""
      classifyResult llvm node `shouldBe` Match

    it "flags different successful output as a mismatch" $ do
      let llvm = outcome "llvm" True True False (Just 0) "OK\nDONE\n" ""
      let node = outcome "node" True True False (Just 0) "OK\nOTHER\n" ""
      classifyResult llvm node `shouldBe` DifferentialMismatch

    it "prefers assertion failures over otherwise matching output" $ do
      let llvm = outcome "llvm" True True False (Just 0) "ASSERT_FAIL: foo\n" ""
      let node = outcome "node" True True False (Just 0) "ASSERT_FAIL: foo\n" ""
      classifyResult llvm node `shouldBe` AssertionFailure

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
