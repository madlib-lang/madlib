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
import qualified AST.Solved as Slv


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
  -- describe "format error" $ do
  --   it "should format InfiniteType errors" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (InfiniteType (TV "a" Star) (TVar (TV "a" Star))) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ Infinite type a -> a"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format illegal skip access" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError IllegalSkipAccess (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ You accessed the skip symbol '_'. This is not permitted as it does not hold any value"
  --             , "│ and only serves to indicate that you are not interested in whatever it may contain."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format unbound variable" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnboundVariable "name") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The variable 'name' has not been declared, you might have a typo."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format unbound variable from namespace" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnboundVariableFromNamespace "Namespace" "name") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The default import 'Namespace' does not export the function 'name'."
  --             , "│ "
  --             , "│ Hint: Verify that it is exported or that you spelled it correctly."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format capitalized adt type variable" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (CapitalizedADTTVar "Maybe" "Var") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The type parameter 'Var' in the type declaration 'Maybe' is capitalized."
  --             , "│ Type parameters can't be capitalized."
  --             , "│ "
  --             , "│ Hint: Either remove it if you don't need the type variable, or make its first letter lowercase."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format unbound types" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnboundType "Maybe") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The type 'Maybe' has not been declared, you might have a typo!"
  --             , "│ "
  --             , "│ Hint: Maybe you forgot to import it?"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format signature too general" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (SignatureTooGeneral (Forall [] ([] :=> TVar (TV "a" Star))) (Forall [] ([] :=> tFloat))) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The signature given is too general"
  --             , "│ Type signature given:"
  --             , "│     a"
  --             , "│ "
  --             , "│ Type inferred:"
  --             , "│     Float"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format unification error" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnificationError tStr tFloat) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ I found a type error!"
  --             , "│ expected:"
  --             , "│     \ESC[32mFloat\ESC[0m"
  --             , "│ but found:"
  --             , "│     \ESC[31mString\ESC[0m"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format no instance found" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NoInstanceFound "Number" [tStr]) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ I could not find any instance for 'Number String'. Verify that you imported the module"
  --             , "│ where the Number instance for 'String' is defined."
  --             , "│ "
  --             , "│ NB: remember that instance methods are automatically imported when the module"
  --             , "│ is imported, directly, or indirectly."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format ambiguous type" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (AmbiguousType (TV "a" Star, [])) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ An ambiguity for the type variable 'a' could not be resolved!"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format ambiguous type - known interface" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (AmbiguousType (TV "a" Star, [IsIn "Monoid" [TVar (TV "a" Star)] Nothing])) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ I am looking for an instance of 'Monoid' but could not resolve it."
  --             , "│ "
  --             , "│ "
  --             , "│ Hint: You might want to add a type annotation to make it resolvable."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format interface not existing" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (InterfaceNotExisting "Comonad") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The interface 'Comonad' is not defined. Make sure you imported the module"
  --             , "│ defining it, or a module that imports it."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format kind error" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (KindError (tListOf tBool, Star) (tListOf (tTuple2Of tUnit tChar), Kfun Star Star)) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The kind of types don't match, 'List Boolean' has kind * and List #[{}, Char] has kind * -> *."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format instance predicate error" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (InstancePredicateError (IsIn "Monoid" [TVar (TV "a" Star)] Nothing) (IsIn "Monoid" [TVar (TV "a" Star), TVar (TV "a" Star)] Nothing) (IsIn "Monoid" [TVar (TV "a" Star)] Nothing)) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ A constraint in the instance declaration 'Monoid a is not correct."
  --             , "│ You gave the constraint 'Monoid a a' but a constraint of the form 'Monoid a'"
  --             , "│ was expected."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format import cycle" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (ImportCycle ["path1", "path2", "path1"]) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ I found an import cycle:"
  --             , "│ "
  --             , "│ path1"
  --             , "│   -> path2"
  --             , "│     -> path1"
  --             , "│ "
  --             , "│ Hint: Import cycles are not allowed and usually show a design issue. Consider splitting things in more"
  --             , "│ modules in order to have both modules import a common dependency instead of having them being co-dependent."
  --             , "│ Another solution would be to move things that depend on the other module from the cycle into the other in"
  --             , "│ order to collocate things that depend on each other."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format grammar error" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (GrammarError "path" "Error at token xyz") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ Error at token xyz"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format unknown type" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (UnknownType "Optional") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ Type Error, the type 'Optional' is not found."
  --             , "│ "
  --             , "│ Hint: Verify that you imported it!"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format name already defined" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NameAlreadyDefined "x") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ Illegal shadowing, the variable 'x' is already defined. Shadowing is not permitted in madlib."
  --             , "│ "
  --             , "│ Hint: Change the name of the variable."
  --             , "│ Also note that the variable might be defined further down. All top level assignments share the scope and using a local name"
  --             , "│ that is defined in the global scope of a module is not allowed."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format name already exported" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NameAlreadyExported "filter") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ Export already defined. You are trying to export the name 'filter' but it"
  --             , "│ appears that you have already exported it."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format not exported" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NotExported "List.mad" "filter") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ You are trying to import 'List.mad' from the module located here:"
  --             , "│ 'filter'"
  --             , "│ Unfortunately, that module does not export 'List.mad'!"
  --             , "│ "
  --             , "│ Hint: Verify that you spelled it correctly or add the export to the module if you can."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format recursive access" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (RecursiveVarAccess "filter") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ You are using a variable that is recursively accessing itself and is thus not yet initialized."
  --             , "│ This is not allowed and can only work if there exists a function in between, let me show you"
  --             , "│ some examples that should make this clearer:"
  --             , "│ parser = J.map(Title, J.field(\"title\", parser)) // this is not allowed because parser is directly refering to itself"
  --             , "│ parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser))) // this works because now the recursive accessed is wrapped in a function"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format not in scope" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NotInScope "filter" (Loc 0 0 0)) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ This expression relies on an expression that accesses the variable 'filter' at line 0."
  --             , "│ All variables need to have been defined by the time they are accessed and this access is thus not allowed."
  --             , "│ "
  --             , "│ Hint: Move that call further down in the module so that the name is defined when you access it."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format types have different origin" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (TypesHaveDifferentOrigin "Maybe" "prelude/Maybe.mad" "lib/Maybe.mad") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ Types do not match. You try to use a type that seems similar but comes from two different locations."
  --             , "│ The type 'Maybe' is used from:"
  --             , "│   - 'prelude/Maybe.mad'"
  --             , "│   - 'lib/Maybe.mad'"
  --             , "│ "
  --             , "│ Hint: Import it only from one place, or if you meant to use both, make sure to convert from one to the other"
  --             , "│ correctly."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format should be typed or above" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (ShouldBeTypedOrAbove "parse") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ You access the name 'parse' before it is defined. This is fine, but in that case you must"
  --             , "│ give it a type annotation."
  --             , "│ "
  --             , "│ Hint: Place that declaration above the place you use it, or give it a type annotation."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format not capitalized adt name" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NotCapitalizedADTName "maybe") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The name 'maybe' of this type is not capitalized. This is incorrect and all types in madlib should start with"
  --             , "│ an uppercased letter."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format not capitalized alias name" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NotCapitalizedAliasName "maybe") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The name 'maybe' of this type alias is not capitalized. This is incorrect and all types in madlib should start with"
  --             , "│ an uppercased letter."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format not capitalized constructor name" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (NotCapitalizedConstructorName "maybe") (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The name 'maybe' of this type constructor is not capitalized. This is incorrect and all types in madlib should start with"
  --             , "│ an uppercased letter."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format typing has wrong kind" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (TypingHasWrongKind (TCon (TC "Maybe" Star) "Maybe.mad") (Kfun Star Star) Star) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The type annotation 'Maybe' has a wrong kind."
  --             , "│ expected:"
  --             , "│     \ESC[32m* -> *\ESC[0m"
  --             , "│ But found:"
  --             , "│     \ESC[31m*\ESC[0m"
  --             ]
  --     actual `shouldBe` expected

  --   it "should format context too weak" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (ContextTooWeak [IsIn "Monad" [TVar (TV "m" (Kfun Star Star))] Nothing]) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The context of the type annotation is too weak. The type inferred for the implementation"
  --             , "│ has the following constraints: Monad."
  --             , "│ "
  --             , "│ Hint: Add the missing interface constraints to the type annotation."
  --             ]
  --     actual `shouldBe` expected

  --   it "should format wrong alias arg count" $ do
  --     let actual   = unsafePerformIO $ format mockedReadFile False (CompilationError (WrongAliasArgCount "User" 2 1) (Context "path" errorArea))
  --         expected =
  --           unlines
  --             [ "│ \ESC[41mError\ESC[0m in module 'path' at line 1:"
  --             , "│ 1|// here is the error"
  --             , "│   ^^^^^^^^^^^^^^^^^^^^"
  --             , "│ "
  --             , "│ The alias 'User' was expected to have 2 arguments, but"
  --             , "│ 1 was given."
  --             , "│ "
  --             , "│ Hint: add the missing '1' argument(s)"
  --             ]
  --     actual `shouldBe` expected

  describe "prettyPrintQualType" $ do
    it "should pretty print a qualified type with multiple constraints" $ do
      let qt   = ([IsIn "Monad" [TVar $ TV "m" (Kfun Star Star)] Nothing, IsIn "Monoid" [TVar $ TV "rec" Star] Nothing] :=> (TVar (TV "m" (Kfun Star Star)) `fn` TRecord (Map.fromList [("x", tInteger)]) (Just (TVar $ TV "rec" Star)) mempty `fn` tTuple2Of tBool tStr))
          actual   = prettyPrintQualType qt
          expected = "(Monad m, Monoid a) => m -> { ...base, x :: Integer } -> #[Boolean, String]"
      actual `shouldBe` expected

    it "should pretty print a qualified type with one constraint" $ do
      let scheme   = Forall [] ([IsIn "Monad" [TVar $ TV "m" (Kfun Star Star)] Nothing] :=> (TVar (TV "m" (Kfun Star Star)) `fn` TRecord (Map.fromList [("x", tInteger)]) (Just (TVar $ TV "rec" Star)) mempty `fn` (tStr `fn` tTuple4Of tByte tBool tBool tBool) `fn` tTuple3Of tBool tStr (TApp (TApp (TCon (TC "Either" (Kfun (Kfun Star Star) Star)) "Either.mad") tByteArray) (tListOf tStr))))
          actual   = schemeToStr scheme
          expected = "Monad m => m -> { ...base, x :: Integer } -> (String -> #[Byte, Boolean, Boolean, Boolean]) -> #[Boolean, String, Either ByteArray (List String)]"
      actual `shouldBe` expected

  describe "prettyPrintTyping" $ do
    it "should pretty print a typing" $ do
      let typing = Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRSingle "a")) (Slv.Untyped emptyArea $ Slv.TRTuple [Slv.Untyped emptyArea (Slv.TRSingle "a"), Slv.Untyped emptyArea (Slv.TRSingle "a")]))
          actual = prettyPrintTyping' True typing
      actual `shouldBe` "(a -> #[a, a])"

    it "should pretty print a constructor typing" $ do
      let typing = Slv.Untyped emptyArea (Slv.TRComp "List" [Slv.Untyped emptyArea (Slv.TRComp "String" [])])
          actual = prettyPrintTyping typing
      actual `shouldBe` "(List String)"

    it "should pretty print a wrapped constructor typing" $ do
      let typing = Slv.Untyped emptyArea (Slv.TRComp "Maybe" [Slv.Untyped emptyArea (Slv.TRComp "List" [Slv.Untyped emptyArea $ Slv.TRComp "Integer" []])])
          actual = prettyPrintTyping typing
      actual `shouldBe` "(Maybe (List Integer))"

    it "should pretty print a var typing" $ do
      let typing = Slv.Untyped emptyArea (Slv.TRSingle "a")
          actual = prettyPrintTyping typing
      actual `shouldBe` "a"

    it "should pretty print a record typing" $ do
      let typing = Slv.Untyped emptyArea (Slv.TRRecord (Map.fromList [("readFile", (emptyArea, Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRSingle "String")) (Slv.Untyped emptyArea (Slv.TRComp "Wish" [Slv.Untyped emptyArea $ Slv.TRSingle "IOError", Slv.Untyped emptyArea $ Slv.TRSingle "String"]))))), ("writeFile", (emptyArea, Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRSingle "String")) (Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRSingle "String")) (Slv.Untyped emptyArea (Slv.TRComp "Wish" [Slv.Untyped emptyArea $ Slv.TRSingle "IOError", Slv.Untyped emptyArea $ Slv.TRSingle "String"])))))))]) Nothing)
          actual = prettyPrintTyping typing
      actual `shouldBe` "{ readFile :: String -> Wish IOError String, writeFile :: String -> String -> Wish IOError String }"

    it "should pretty print a sub function typing" $ do
      let typing = Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRSingle "a")) (Slv.Untyped emptyArea (Slv.TRSingle "b")))) (Slv.Untyped emptyArea (Slv.TRArr (Slv.Untyped emptyArea (Slv.TRComp "m" [Slv.Untyped emptyArea $ Slv.TRSingle "a"])) (Slv.Untyped emptyArea (Slv.TRComp "m" [Slv.Untyped emptyArea $ Slv.TRSingle "b"])))))
          actual = prettyPrintTyping typing
      actual `shouldBe` "((a -> b) -> m a -> m b)"
