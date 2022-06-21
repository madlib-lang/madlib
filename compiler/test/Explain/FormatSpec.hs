module Explain.FormatSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldBe
                                                )
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , unpack
                                                )
import Explain.Format
import Error.Error
import Explain.Location
import GHC.IO (unsafePerformIO)
import Infer.Type
import Error.Context
import qualified Data.Map as Map


makeReadFile :: String -> (FilePath -> IO String)
makeReadFile content =
  const $ return content


sourceFile :: String
sourceFile = "// here is the error\n"

mockedReadFile :: FilePath -> IO String
mockedReadFile = makeReadFile sourceFile


errorArea :: Area
errorArea = Area (Loc 1 1 1) (Loc 21 1 21)


spec :: Spec
spec = do
  describe "format error" $ do
    it "should format InfiniteType errors" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (InfiniteType (TV "a" Star) (TVar (TV "a" Star))) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "Infinite type a -> a"
              ]
      actual `shouldBe` expected

    it "should format illegal skip access" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError IllegalSkipAccess (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "You accessed the skip symbol '_'. This is not permitted as it does not hold any value"
              , "and only serves to indicate that you are not interested in whatever it may contain."
              ]
      actual `shouldBe` expected

    it "should format unbound variable" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnboundVariable "name") (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The variable 'name' has not been declared, you might have a typo."
              ]
      actual `shouldBe` expected

    it "should format unbound variable from namespace" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnboundVariableFromNamespace "Namespace" "name") (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The default import 'Namespace' does not export the function 'name'."
              , ""
              , "Hint: Verify that it is exported or that you spelled it correctly."
              ]
      actual `shouldBe` expected

    it "should format capitalized adt type variable" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (CapitalizedADTTVar "Maybe" "Var") (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The type parameter 'Var' in the type declaration 'Maybe' is capitalized."
              , "Type parameters can't be capitalized."
              , ""
              , "Hint: Either remove it if you don't need the type variable, or make its first letter lowercase."
              ]
      actual `shouldBe` expected

    it "should format unbound types" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnboundType "Maybe") (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The type 'Maybe' has not been declared, you might have a typo!"
              , ""
              , "Hint: Maybe you forgot to import it?"
              ]
      actual `shouldBe` expected

    it "should format signature too general" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (SignatureTooGeneral (Forall [] ([] :=> TVar (TV "a" Star))) (Forall [] ([] :=> tFloat))) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The signature given is too general"
              , "Type signature given:"
              , "    a"
              , ""
              , "Type inferred:"
              , "    Float"
              ]
      actual `shouldBe` expected

    it "should format unification error" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnificationError tStr tFloat) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "Type error, expected:"
              , "    \ESC[32mFloat\ESC[0m"
              , "But found:"
              , "    \ESC[31mString\ESC[0m"
              ]
      actual `shouldBe` expected

    it "should format no instance found" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NoInstanceFound "Number" [tStr]) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "I could not find any instance for 'Number String'. Verify that you imported the module"
              , "where the Number instance for 'String' is defined."
              , ""
              , "NB: remember that instance methods are automatically imported when the module"
              , "is imported, directly, or indirectly."
              ]
      actual `shouldBe` expected

    it "should format ambiguous type" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (AmbiguousType (TV "a" Star, [])) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "An ambiguity for the type variable 'a' could not be resolved!"
              ]
      actual `shouldBe` expected

    it "should format ambiguous type - known interface" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (AmbiguousType (TV "a" Star, [IsIn "Monoid" [TVar (TV "a" Star)] Nothing])) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "An ambiguity could not be resolved! I am"
              , "looking for an instance of 'Monoid' but could not resolve it. You"
              , "might want to add a type annotation to make it resolvable."
              ]
      actual `shouldBe` expected

    it "should format interface not existing" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (InterfaceNotExisting "Comonad") (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The interface 'Comonad' is not defined. Make sure you imported the module"
              , "defining it, or a module that imports it."
              ]
      actual `shouldBe` expected

    it "should format kind error" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (KindError (tListOf tBool, Star) (tListOf (tTuple2Of tUnit tChar), Kfun Star Star)) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "The kind of types don't match, 'List Boolean' has kind * and List #[{}, Char] has kind * -> *."
              ]
      actual `shouldBe` expected

    it "should format instance predicate error" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (InstancePredicateError (IsIn "Monoid" [TVar (TV "a" Star)] Nothing) (IsIn "Monoid" [TVar (TV "a" Star), TVar (TV "a" Star)] Nothing) (IsIn "Monoid" [TVar (TV "a" Star)] Nothing)) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "A constraint in the instance declaration 'Monoid a is not correct."
              , "You gave the constraint 'Monoid a a' but a constraint of the form 'Monoid a'"
              , "was expected."
              ]
      actual `shouldBe` expected

    it "should format import cycle" $ do
      let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (ImportCycle ["path1", "path2", "path1"]) (Context "path" errorArea))
          expected =
            init $ unlines
              [ "\ESC[41mError\ESC[0m in module 'path' at line 1:"
              , "1|// here is the error"
              , "  ^^^^^^^^^^^^^^^^^^^^"
              , ""
              , "I found an import cycle:"
              , ""
              , "path1"
              , "  -> path2"
              , "    -> path1"
              , ""
              , "Hint: Import cycles are not allowed and usually show a design issue. Consider splitting things in more"
              , "modules in order to have both modules import a common dependency instead of having them being co-dependent."
              , "Another solution would be to move things that depend on the other module from the cycle into the other in"
              , "order to collocate things that depend on each other."
              ]
      actual `shouldBe` expected

  describe "prettyPrintQualType" $ do
    it "should pretty print a qualified type with multiple constraints" $ do
      let qt   = ([IsIn "Monad" [TVar $ TV "m" (Kfun Star Star)] Nothing, IsIn "Monoid" [TVar $ TV "rec" Star] Nothing] :=> (TVar (TV "m" (Kfun Star Star)) `fn` TRecord (Map.fromList [("x", tInteger)]) (Just (TVar $ TV "rec" Star)) `fn` tTuple2Of tBool tStr))
          actual   = prettyPrintQualType qt
          expected = "(Monad m, Monoid a) => m -> { ...base, x :: Integer } -> #[Boolean, String]"
      actual `shouldBe` expected

    it "should pretty print a qualified type with one constraint" $ do
      let scheme   = Forall [] ([IsIn "Monad" [TVar $ TV "m" (Kfun Star Star)] Nothing] :=> (TVar (TV "m" (Kfun Star Star)) `fn` TRecord (Map.fromList [("x", tInteger)]) (Just (TVar $ TV "rec" Star)) `fn` (tStr `fn` tTuple4Of tByte tBool tBool tBool) `fn` tTuple3Of tBool tStr (TApp (TApp (TCon (TC "Either" (Kfun (Kfun Star Star) Star)) "Either.mad") tByteArray) (tListOf tStr))))
          actual   = schemeToStr scheme
          expected = "Monad m => m -> { ...base, x :: Integer } -> (String -> #[Byte, Boolean, Boolean, Boolean]) -> #[Boolean, String, Either ByteArray (List String)]"
      actual `shouldBe` expected
