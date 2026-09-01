module Infer.PlaceholderSpec where

import qualified AST.Solved                    as Slv
import           Explain.Location               ( emptyArea )
import           Infer.Placeholder              ( lowerTypeToRuntimeValue )
import           Infer.Type                     ( Kind(..)
                                                , TCon(..)
                                                , TVar(..)
                                                , Type(..)
                                                , mkTCon
                                                )
import           Test.Hspec


builtinsPath :: FilePath
builtinsPath = "/project/prelude/__internal__/__BUILTINS__.mad"


collectCall :: Slv.Exp -> Maybe (String, [Slv.Exp])
collectCall = go []
  where
    go args (Slv.Typed _ _ (Slv.App fn arg _)) =
      go (arg : args) fn

    go args (Slv.Typed _ _ (Slv.Var name False)) =
      Just (name, args)

    go _ _ =
      Nothing


expectCtor :: Slv.Exp -> String -> ([Slv.Exp] -> Expectation) -> Expectation
expectCtor exp_ expectedName checkArgs =
  case collectCall exp_ of
    Just (actualName, args) -> do
      actualName `shouldBe` expectedName
      checkArgs args

    Nothing ->
      expectationFailure $ "expected constructor call, got: " <> show exp_


expectString :: Slv.Exp -> String -> Expectation
expectString (Slv.Typed _ _ (Slv.LStr actual)) expected =
  actual `shouldBe` expected

expectString exp_ _ =
  expectationFailure $ "expected string literal, got: " <> show exp_


expectKind :: Slv.Exp -> Kind -> Expectation
expectKind exp_ Star =
  expectCtor exp_ "__BUILTINS__.typeStar" (`shouldBe` [])

expectKind exp_ Row =
  expectCtor exp_ "__BUILTINS__.typeRow" (`shouldBe` [])

expectKind exp_ (Kfun left right) =
  expectCtor exp_ "__BUILTINS__.typeKfun" $ \args ->
    case args of
      [leftExp, rightExp] -> do
        expectKind leftExp left
        expectKind rightExp right

      _ ->
        expectationFailure $ "expected Kfun to have two args, got: " <> show args


expectRuntimeTVar :: Slv.Exp -> String -> Kind -> Expectation
expectRuntimeTVar exp_ expectedName expectedKind =
  expectCtor exp_ "__BUILTINS__.typeTVar" $ \args ->
    case args of
      [nameExp, kindExp] -> do
        expectString nameExp expectedName
        expectKind kindExp expectedKind

      _ ->
        expectationFailure $ "expected TVar to have two args, got: " <> show args


expectRuntimeTCon :: Slv.Exp -> String -> Kind -> FilePath -> Expectation
expectRuntimeTCon exp_ expectedName expectedKind expectedPath =
  expectCtor exp_ "__BUILTINS__.typeTCon" $ \args ->
    case args of
      [nameExp, kindExp, pathExp] -> do
        expectString nameExp expectedName
        expectKind kindExp expectedKind
        expectString pathExp expectedPath

      _ ->
        expectationFailure $ "expected TCon to have three args, got: " <> show args


spec :: Spec
spec = do
  describe "lowerTypeToRuntimeValue" $ do
    it "lowers TVar with its kind" $ do
      let tvKind = Kfun Star Star
          exp_ = lowerTypeToRuntimeValue builtinsPath emptyArea (TVar (TV 42 tvKind))

      expectRuntimeTVar exp_ "a" tvKind

    it "lowers row-kinded variables" $ do
      let exp_ = lowerTypeToRuntimeValue builtinsPath emptyArea (TVar (TV 42 Row))

      expectRuntimeTVar exp_ "a" Row

    it "lowers TCon with normalized name, kind, and origin path" $ do
      let arrowKind = Kfun Star (Kfun Star Star)
          exp_ = lowerTypeToRuntimeValue builtinsPath emptyArea (mkTCon (TC "(->)" arrowKind) "prelude")

      expectRuntimeTCon exp_ "->" arrowKind "prelude"

    it "lowers TApp using enriched constructor and variable nodes" $ do
      let listKind = Kfun Star Star
          listType = mkTCon (TC "List" listKind) "prelude"
          itemType = TVar (TV 99 Star)
          exp_ = lowerTypeToRuntimeValue builtinsPath emptyArea (TApp listType itemType)

      expectCtor exp_ "__BUILTINS__.typeTApp" $ \args ->
        case args of
          [headExp, argExp] -> do
            expectRuntimeTCon headExp "List" listKind "prelude"
            expectRuntimeTVar argExp "a" Star

          _ ->
            expectationFailure $ "expected TApp to have two args, got: " <> show args

    it "uses the compiler Type origin path for user-defined constructors" $ do
      let path = "/project/src/Domain/User.mad"
          exp_ = lowerTypeToRuntimeValue builtinsPath emptyArea (mkTCon (TC "User" Star) path)

      expectRuntimeTCon exp_ "User" Star path
