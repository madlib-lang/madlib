module Explain.FormatSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldBe
                                                , shouldSatisfy
                                                )
import           Data.List                      ( isInfixOf )
import           Explain.Format
import           Error.Error
import           Explain.Location
import           Infer.Type
import           Error.Context
import qualified Data.Map                      as Map
import qualified AST.Solved                    as Slv


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

makeCtx :: Context
makeCtx = Context "test/Module.mad" (Area (Loc 0 1 1) (Loc 0 1 20))

noCtx :: Context
noCtx = NoContext

-- Run simpleFormatError synchronously (no color, no JSON).
fmt :: TypeError -> Context -> IO String
fmt e ctx = simpleFormatError False (CompilationError e ctx)

-- Infix contains check that works like shouldContain but uses shouldSatisfy.
contains :: String -> String -> IO ()
contains result needle =
  result `shouldSatisfy` isInfixOf needle


-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  -- -------------------------------------------------------------------------
  describe "Type errors" $ do
    it "UnificationError shows expected and found types" $ do
      result <- fmt (UnificationError tStr tFloat NoOrigin) makeCtx
      result `contains` "expected"
      result `contains` "found"

    it "InfiniteType mentions infinite" $ do
      result <- fmt (InfiniteType (TV 1 Star) (TVar (TV 1 Star))) makeCtx
      result `contains` "nfinite"

    it "SignatureTooGeneral shows given and inferred" $ do
      result <- fmt (SignatureTooGeneral (Forall [] ([] :=> TVar (TV 1 Star))) (Forall [] ([] :=> tFloat))) makeCtx
      result `contains` "given"
      result `contains` "inferred"

    it "ContextTooWeak names the missing constraints" $ do
      result <- fmt (ContextTooWeak [IsIn "Eq" [tStr] Nothing]) makeCtx
      result `contains` "Eq"

    it "AmbiguousType with constraint names the interface" $ do
      result <- fmt (AmbiguousType (TV 1 Star, [IsIn "Show" [TVar (TV 1 Star)] Nothing])) makeCtx
      result `contains` "Show"

    it "AmbiguousType with no constraints mentions type variable" $ do
      result <- fmt (AmbiguousType (TV 5 Star, [])) makeCtx
      result `contains` "mbiguous"

    it "KindError shows kind symbols" $ do
      result <- fmt (KindError (tStr, Star) (mkTCon (TC "List" (Kfun Star Star)) "List.mad", Kfun Star Star)) makeCtx
      result `contains` "*"

    it "TypingHasWrongKind mentions kind" $ do
      result <- fmt (TypingHasWrongKind tStr (Kfun Star Star) Star) makeCtx
      result `contains` "kind"

  -- -------------------------------------------------------------------------
  describe "Binding errors" $ do
    it "UnboundVariable with no suggestions prompts for typo check" $ do
      result <- fmt (UnboundVariable "myVar" []) makeCtx
      result `contains` "myVar"

    it "UnboundVariable with suggestions shows them" $ do
      result <- fmt (UnboundVariable "fliter" ["filter"]) makeCtx
      result `contains` "filter"

    it "UnboundType with no suggestions shows type name" $ do
      result <- fmt (UnboundType "MyType" []) makeCtx
      result `contains` "MyType"

    it "UnboundType with suggestion shows it" $ do
      result <- fmt (UnboundType "Mabye" ["Maybe"]) makeCtx
      result `contains` "Maybe"

    it "UnboundUnknownTypeVariable mentions type variable" $ do
      result <- fmt UnboundUnknownTypeVariable makeCtx
      result `contains` "type variable"

    it "UnboundVariableFromNamespace names namespace and function" $ do
      result <- fmt (UnboundVariableFromNamespace "List" "fliter") makeCtx
      result `contains` "fliter"
      result `contains` "List"

    it "NameAlreadyDefined identifies the variable" $ do
      result <- fmt (NameAlreadyDefined "x") makeCtx
      result `contains` "x"

    it "TypeAlreadyDefined identifies the type" $ do
      result <- fmt (TypeAlreadyDefined "User") makeCtx
      result `contains` "User"

    it "NameAlreadyExported identifies the name" $ do
      result <- fmt (NameAlreadyExported "greet") makeCtx
      result `contains` "greet"

    it "TypeAnnotationNameMismatch shows both names" $ do
      result <- fmt (TypeAnnotationNameMismatch "foo" "bar") makeCtx
      result `contains` "foo"
      result `contains` "bar"

    it "ShouldBeTypedOrAbove names the binding" $ do
      result <- fmt (ShouldBeTypedOrAbove "parse") makeCtx
      result `contains` "parse"

    it "NotInScope names the variable" $ do
      result <- fmt (NotInScope "handler" (Loc 0 5 1)) makeCtx
      result `contains` "handler"

    it "RecursiveVarAccess names the variable" $ do
      result <- fmt (RecursiveVarAccess "parser") makeCtx
      result `contains` "parser"

  -- -------------------------------------------------------------------------
  describe "Type definition errors" $ do
    it "NotCapitalizedADTName shows the name" $ do
      result <- fmt (NotCapitalizedADTName "maybe") makeCtx
      result `contains` "maybe"

    it "NotCapitalizedAliasName shows the name" $ do
      result <- fmt (NotCapitalizedAliasName "user") makeCtx
      result `contains` "user"

    it "NotCapitalizedConstructorName shows the name" $ do
      result <- fmt (NotCapitalizedConstructorName "just") makeCtx
      result `contains` "just"

    it "CapitalizedADTTVar shows the type and parameter" $ do
      result <- fmt (CapitalizedADTTVar "Maybe" "Val") makeCtx
      result `contains` "Val"

    it "ADTAlreadyDefined says already" $ do
      result <- fmt (ADTAlreadyDefined tStr) makeCtx
      result `contains` "already"

    it "WrongAliasArgCount shows alias name and counts" $ do
      result <- fmt (WrongAliasArgCount "Pair" 2 1) makeCtx
      result `contains` "Pair"
      result `contains` "2"
      result `contains` "1"

    it "UnknownType names the type" $ do
      result <- fmt (UnknownType "Optional" []) makeCtx
      result `contains` "Optional"

    it "UnknownType with suggestion shows it" $ do
      result <- fmt (UnknownType "Mabye" ["Maybe"]) makeCtx
      result `contains` "Maybe"

    it "TypesHaveDifferentOrigin names the type and origins" $ do
      result <- fmt (TypesHaveDifferentOrigin "User" "a/User.mad" "b/User.mad") makeCtx
      result `contains` "User"
      result `contains` "a/User.mad"

    it "TypingHasWrongKind mentions the kind" $ do
      result <- fmt (TypingHasWrongKind tStr (Kfun Star Star) Star) makeCtx
      result `contains` "kind"

  -- -------------------------------------------------------------------------
  describe "Interface errors" $ do
    it "NoInstanceFound names the interface and type" $ do
      result <- fmt (NoInstanceFound "Eq" [tStr]) makeCtx
      result `contains` "Eq"
      result `contains` "String"

    it "NoInstanceFound Number+String names the interface and type" $ do
      result <- fmt (NoInstanceFound "Number" [tStr]) makeCtx
      result `contains` "Number"
      result `contains` "String"

    it "InterfaceNotExisting names the interface" $ do
      result <- fmt (InterfaceNotExisting "Comonad") makeCtx
      result `contains` "Comonad"

    it "InterfaceAlreadyDefined names the interface" $ do
      result <- fmt (InterfaceAlreadyDefined "Show") makeCtx
      result `contains` "Show"

    it "InstancePredicateError shows the interface name" $ do
      result <- fmt (InstancePredicateError
        (IsIn "Show" [TVar (TV 1 Star)] Nothing)
        (IsIn "Show" [TVar (TV 1 Star), TVar (TV 2 Star)] Nothing)
        (IsIn "Show" [TVar (TV 1 Star)] Nothing)) makeCtx
      result `contains` "Show"

    it "DerivingAliasNotAllowed names the alias" $ do
      result <- fmt (DerivingAliasNotAllowed "Name") makeCtx
      result `contains` "Name"

    it "InvalidInterfaceDerived names the interface" $ do
      result <- fmt (InvalidInterfaceDerived "Functor") makeCtx
      result `contains` "Functor"

    it "MethodNameAlreadyDefined mentions method" $ do
      result <- fmt MethodNameAlreadyDefined makeCtx
      result `contains` "method"

  -- -------------------------------------------------------------------------
  describe "Import errors" $ do
    it "ImportNotFound names the module" $ do
      result <- fmt (ImportNotFound "List") makeCtx
      result `contains` "List"

    it "ImportNotFound relative path" $ do
      result <- fmt (ImportNotFound "./Utils") makeCtx
      result `contains` "Utils"

    it "ImportCollision names the colliding name" $ do
      result <- fmt (ImportCollision "map") makeCtx
      result `contains` "map"

    it "NotExported names the name and path" $ do
      result <- fmt (NotExported "filter" "List.mad" []) makeCtx
      result `contains` "filter"

    it "NotExported with suggestion shows it" $ do
      result <- fmt (NotExported "fliter" "List.mad" ["filter"]) makeCtx
      result `contains` "filter"

    it "ImportCycle shows all modules" $ do
      result <- fmt (ImportCycle ["A.mad", "B.mad", "A.mad"]) makeCtx
      result `contains` "A.mad"
      result `contains` "B.mad"

  -- -------------------------------------------------------------------------
  describe "Mutation errors" $ do
    it "BadMutation mentions ':='" $ do
      result <- fmt BadMutation makeCtx
      result `contains` ":="

    it "MutatingNotInScope names the variable" $ do
      result <- fmt (MutatingNotInScope "counter") makeCtx
      result `contains` "counter"

    it "MutatingPatternBoundVariable names the variable" $ do
      result <- fmt (MutatingPatternBoundVariable "x") makeCtx
      result `contains` "x"

    it "MutationRestriction mentions type" $ do
      result <- fmt MutationRestriction makeCtx
      result `contains` "type"

    it "MutatingFunction names the function" $ do
      result <- fmt (MutatingFunction "myFn") makeCtx
      result `contains` "myFn"

    it "OverloadedMutation names the variable" $ do
      result <- fmt (OverloadedMutation "x" [IsIn "Show" [tStr] Nothing]) makeCtx
      result `contains` "x"

  -- -------------------------------------------------------------------------
  describe "Record errors" $ do
    it "RecordDuplicateFields names the duplicate fields" $ do
      result <- fmt (RecordDuplicateFields ["name", "age"]) makeCtx
      result `contains` "name"

    it "RecordMissingFields names the missing fields" $ do
      result <- fmt (RecordMissingFields ["email", "phone"]) makeCtx
      result `contains` "email"

    it "RecordExtraFields names the extra fields" $ do
      result <- fmt (RecordExtraFields ["extra"] ["name", "age"]) makeCtx
      result `contains` "extra"

    it "RecordDuplicateRestPattern mentions rest/spread" $ do
      result <- fmt RecordDuplicateRestPattern makeCtx
      result `contains` "rest"

    it "InvalidLhs mentions valid forms" $ do
      result <- fmt InvalidLhs makeCtx
      result `contains` "left"

  -- -------------------------------------------------------------------------
  describe "Literal errors" $ do
    it "BadEscapeSequence mentions escape" $ do
      result <- fmt BadEscapeSequence makeCtx
      result `contains` "escape"

    it "EmptyChar mentions empty" $ do
      result <- fmt EmptyChar makeCtx
      result `contains` "mpty"

    it "ByteOutOfBounds shows the literal and limit" $ do
      result <- fmt (ByteOutOfBounds "300") makeCtx
      result `contains` "300"
      result `contains` "255"

    it "ShortOutOfBounds shows the literal" $ do
      result <- fmt (ShortOutOfBounds "99999999999") makeCtx
      result `contains` "99999999999"

    it "IntOutOfBounds shows the literal" $ do
      result <- fmt (IntOutOfBounds "99999999999999999999") makeCtx
      result `contains` "99999999999999999999"

    it "NegatedByte mentions negat" $ do
      result <- fmt NegatedByte makeCtx
      result `contains` "negat"

  -- -------------------------------------------------------------------------
  describe "Other errors" $ do
    it "GrammarError shows the parse message" $ do
      result <- fmt (GrammarError "test.mad" "unexpected token '}'") makeCtx
      result `contains` "unexpected token"

    it "GrammarError with empty message shows Syntax" $ do
      result <- fmt (GrammarError "test.mad" "") makeCtx
      result `contains` "Syntax"

    it "NotADefinition mentions top" $ do
      result <- fmt NotADefinition makeCtx
      result `contains` "top"

    it "NotAConstructor names the identifier" $ do
      result <- fmt (NotAConstructor "myVar") makeCtx
      result `contains` "myVar"

    it "IllegalSkipAccess mentions '_'" $ do
      result <- fmt IllegalSkipAccess makeCtx
      result `contains` "_"

    it "NoMain mentions 'main'" $ do
      result <- fmt NoMain makeCtx
      result `contains` "main"

    it "MainInvalidTyping shows List String" $ do
      result <- fmt MainInvalidTyping makeCtx
      result `contains` "List String"

    it "TestNotValid shows Wish" $ do
      result <- fmt (TestNotValid tStr) makeCtx
      result `contains` "Wish"

    it "WrongSpreadType mentions spread" $ do
      result <- fmt (WrongSpreadType "Cannot spread Integer") makeCtx
      result `contains` "spread"

    it "ConstructorAccessBadIndex shows constructor name and arity" $ do
      result <- fmt (ConstructorAccessBadIndex "Maybe" "Just" 5 10) makeCtx
      result `contains` "Just"
      result `contains` "5"

    it "ConstructorAccessNoConstructorFound names the type" $ do
      result <- fmt (ConstructorAccessNoConstructorFound "EmptyType") makeCtx
      result `contains` "EmptyType"

    it "ConstructorAccessTooManyConstructors names the type" $ do
      result <- fmt (ConstructorAccessTooManyConstructors "Shape" 3) makeCtx
      result `contains` "Shape"

    it "FatalError mentions compiler" $ do
      result <- fmt FatalError makeCtx
      result `contains` "compiler"

    it "ASTHasNoPath mentions module" $ do
      result <- fmt ASTHasNoPath makeCtx
      result `contains` "module"

    it "GrammarError is included in full formatted output" $ do
      let stubReader _ = return "x = 1\n"
      full <- formatError stubReader False (CompilationError (GrammarError "test/Module.mad" "unexpected '}'") makeCtx)
      full `contains` "unexpected"

  -- -------------------------------------------------------------------------
  describe "prettyPrintQualType" $ do
    it "renders multiple constraints" $ do
      let qt = ([IsIn "Monad" [TVar $ TV 11 (Kfun Star Star)] Nothing, IsIn "Monoid" [TVar $ TV 100 Star] Nothing] :=> (TVar (TV 11 (Kfun Star Star)) `fn` TRecord (Map.fromList [("x", tInteger)]) (Just (TVar $ TV 100 Star)) mempty `fn` tTuple2Of tBool tStr))
      prettyPrintQualType qt `shouldBe` "(Monad m, Monoid a) => m -> { ...base, x :: Integer } -> #[Boolean, String]"

    it "renders a single constraint" $ do
      let scheme = Forall [] ([IsIn "Monad" [TVar $ TV 11 (Kfun Star Star)] Nothing] :=> (TVar (TV 11 (Kfun Star Star)) `fn` TRecord (Map.fromList [("x", tInteger)]) (Just (TVar $ TV 100 Star)) mempty `fn` (tStr `fn` tTuple4Of tByte tBool tBool tBool) `fn` tTuple3Of tBool tStr (TApp (TApp (mkTCon (TC "Either" (Kfun (Kfun Star Star) Star)) "Either.mad") tByteArray) (tListOf tStr))))
      schemeToStr scheme `shouldBe` "Monad m => m -> { ...base, x :: Integer } -> (String -> #[Byte, Boolean, Boolean, Boolean]) -> #[Boolean, String, Either ByteArray (List String)]"

    it "renders many type variables without crashing" $ do
      let mkVar i = TVar (TV i Star)
          longFn = foldr1 fn (mkVar <$> [1..30])
      prettyPrintType True longFn `shouldBe` "a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> b1 -> c1 -> d1"

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
