module Error.FilterSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldBe
                                                )
import           Error.Error
import           Error.Context
import           Error.Filter                    ( filterErrors )
import           Explain.Location


ctxAt :: Int -> Int -> Int -> Int -> Context
ctxAt sl sc el ec = Context "Module.mad" (Area (Loc 0 sl sc) (Loc 0 el ec))


spec :: Spec
spec = describe "filterErrors" $ do
  it "removes exact duplicates" $ do
    let err = CompilationError (UnboundVariable "x" []) (ctxAt 1 1 1 2)
    filterErrors [err, err] `shouldBe` [err]

  it "keeps errors of different kinds at the same span" $ do
    let err1 = CompilationError (UnboundVariable "x" []) (ctxAt 1 1 1 2)
        err2 = CompilationError BadMutation (ctxAt 1 1 1 2)
    filterErrors [err1, err2] `shouldBe` [err1, err2]

  it "collapses same-kind errors at the identical span, keeping the first" $ do
    let err1 = CompilationError (UnboundVariable "x" []) (ctxAt 1 1 1 2)
        err2 = CompilationError (UnboundVariable "y" []) (ctxAt 1 1 1 2)
    filterErrors [err1, err2] `shouldBe` [err1]

  it "drops a same-kind error whose span is contained by another's" $ do
    let outer = CompilationError (UnboundVariable "x" []) (ctxAt 1 1 3 1)
        inner = CompilationError (UnboundVariable "x" []) (ctxAt 2 1 2 5)
    filterErrors [outer, inner] `shouldBe` [outer]

  it "does not drop a different-kind error contained by another's span" $ do
    let outer = CompilationError (UnboundVariable "x" []) (ctxAt 1 1 3 1)
        inner = CompilationError BadMutation (ctxAt 2 1 2 5)
    filterErrors [outer, inner] `shouldBe` [outer, inner]

  it "sorts the surviving errors by file position" $ do
    let err1 = CompilationError (UnboundVariable "x" []) (ctxAt 5 1 5 2)
        err2 = CompilationError (UnboundVariable "y" []) (ctxAt 1 1 1 2)
    filterErrors [err1, err2] `shouldBe` [err2, err1]

  it "keeps unrelated errors from independent bindings" $ do
    let err1 = CompilationError (UnboundVariable "x" []) (ctxAt 1 1 1 2)
        err2 = CompilationError (UnboundVariable "y" []) (ctxAt 10 1 10 2)
        err3 = CompilationError (UnboundVariable "z" []) (ctxAt 20 1 20 2)
    filterErrors [err1, err2, err3] `shouldBe` [err1, err2, err3]

  it "handles NoContext errors without crashing" $ do
    let err = CompilationError FatalError NoContext
    filterErrors [err, err] `shouldBe` [err]
