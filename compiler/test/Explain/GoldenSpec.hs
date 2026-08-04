module Explain.GoldenSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , beforeAll_
                                                )
import           Control.Monad                  ( forM_ )
import qualified Data.Map                      as M
import           System.Environment             ( setEnv )
import           System.FilePath                ( (</>) )
import           Explain.Format
import           Error.Error
import           Error.Context
import           Explain.Location
import           Infer.Type
import           TestUtils.Golden               ( shouldMatchGolden )


-- | Golden snapshots of every TypeError constructor rendered through the
-- three output projections: terminal (formatError, NO_COLOR), JSON
-- (formatErrorJson) and LSP text (simpleFormatErrorWithHints).
--
-- These lock the rendered output of the whole error catalogue so that
-- refactors of the rendering pipeline are reviewable as plain-text diffs.
-- Regenerate with: UPDATE_GOLDEN=1 stack test --ta '--match "GoldenSpec"'

goldenDir :: FilePath
goldenDir = "compiler/test/Explain/golden"

makeCtx :: Context
makeCtx = Context "test/Module.mad" (Area (Loc 0 1 1) (Loc 0 1 20))

-- | Stub module source shown in terminal snippets.
stubReader :: FilePath -> IO String
stubReader _ = return $ unlines
  [ "someValue = doStuff(1, 2)"
  , "otherValue = compute(someValue)"
  , "thirdValue = combine(otherValue)"
  , "fourthValue = finish(thirdValue)"
  , "f = (x) => x + 1"
  , "main = () => { f(someValue) }"
  ]

secondaryLoc :: SecondaryLocation
secondaryLoc =
  SecondaryLocation "test/Module.mad" (Area (Loc 0 5 1) (Loc 0 5 16)) "'f' is applied here"

functionContext :: FunctionContext
functionContext = FunctionContext
  { fcExpectedType  = tFloat
  , fcFullSignature = tFloat `fn` tStr
  , fcTotalParams   = 1
  }

tvA :: TVar
tvA = TV 1 Star

goldenCases :: [(String, TypeError, Context)]
goldenCases =
  -- UnificationError through every origin
  [ ("unification-no-origin", UnificationError (TypeMismatch tStr tFloat NoOrigin []), makeCtx)
  , ("unification-annotation-fn-vs-fn", UnificationError (TypeMismatch (tInteger `fn` tStr) (tInteger `fn` tInteger) FromTypeAnnotation []), makeCtx)
  , ("unification-annotation-simple", UnificationError (TypeMismatch tStr tInteger FromTypeAnnotation []), makeCtx)
  , ("unification-operator-and", UnificationError (TypeMismatch tStr tBool (FromOperator "&&") []), makeCtx)
  , ("unification-operator-plus-string", UnificationError (TypeMismatch tStr tFloat (FromOperator "+") []), makeCtx)
  , ("unification-if-condition", UnificationError (TypeMismatch tStr tBool FromIfCondition []), makeCtx)
  , ("unification-while-condition", UnificationError (TypeMismatch tStr tBool FromWhileCondition []), makeCtx)
  , ("unification-if-branches-then", UnificationError (TypeMismatch tStr tFloat (FromIfBranches ThenBranch) []), makeCtx)
  , ("unification-if-branches-else", UnificationError (TypeMismatch tStr tFloat (FromIfBranches ElseBranch) []), makeCtx)
  , ("unification-list-element", UnificationError (TypeMismatch tStr tFloat (FromListElement 3) []), makeCtx)
  , ("unification-list-element-unknown", UnificationError (TypeMismatch tStr tFloat (FromListElement 0) []), makeCtx)
  , ("unification-fn-argument", UnificationError (TypeMismatch tStr tFloat (FromFunctionArgument "map" 2 Nothing) []), makeCtx)
  , ("unification-fn-argument-context", UnificationError (TypeMismatch tStr tFloat (FromFunctionArgument "doStuff" 1 (Just functionContext)) []), makeCtx)
  , ("unification-fn-return", UnificationError (TypeMismatch tStr tFloat (FromFunctionReturn "doStuff") []), makeCtx)
  , ("unification-pattern-match", UnificationError (TypeMismatch tStr tFloat (FromPatternMatch 2) []), makeCtx)
  , ("unification-pattern-match-unknown", UnificationError (TypeMismatch tStr tFloat (FromPatternMatch 0) []), makeCtx)
  , ("unification-assignment", UnificationError (TypeMismatch tStr tFloat (FromAssignment "counter") []), makeCtx)
  , ("unification-secondary-location", UnificationError (TypeMismatch tStr tFloat (FromFunctionArgument "f" 1 Nothing) [secondaryLoc]), makeCtx)
  , ("unification-records", UnificationError (TypeMismatch (TRecord (M.fromList [("name", tStr)]) Nothing mempty) (TRecord (M.fromList [("name", tStr), ("age", tInteger)]) Nothing mempty) NoOrigin []), makeCtx)
  , ("unification-no-context", UnificationError (TypeMismatch tStr tFloat NoOrigin []), NoContext)

  -- Type errors
  , ("infinite-type", InfiniteType tvA (tListOf (TVar tvA)), makeCtx)
  , ("signature-too-general", SignatureTooGeneral (Forall [Star] ([] :=> TGen 0)) (Forall [] ([] :=> tFloat)), makeCtx)
  , ("context-too-weak", ContextTooWeak [IsIn "Eq" [tStr] Nothing], makeCtx)
  , ("ambiguous-type-constrained", AmbiguousType (tvA, [IsIn "Show" [TVar tvA] Nothing]), makeCtx)
  , ("ambiguous-type-bare", AmbiguousType (TV 5 Star, []), makeCtx)
  , ("kind-error", KindError (tStr, Star) (mkTCon (TC "List" (Kfun Star Star)) "List.mad", Kfun Star Star), makeCtx)
  , ("typing-has-wrong-kind", TypingHasWrongKind tStr (Kfun Star Star) Star, makeCtx)

  -- Binding errors
  , ("unbound-variable", UnboundVariable "myCustomFn" [], makeCtx)
  , ("unbound-variable-suggestions", UnboundVariable "fliter" ["filter"], makeCtx)
  , ("unbound-variable-stdlib", UnboundVariable "map" [], makeCtx)
  , ("unbound-type", UnboundType "MyType" [], makeCtx)
  , ("unbound-type-suggestion", UnboundType "Mabye" ["Maybe"], makeCtx)
  , ("unbound-unknown-type-variable", UnboundUnknownTypeVariable, makeCtx)
  , ("unbound-variable-from-namespace", UnboundVariableFromNamespace "List" "fliter", makeCtx)
  , ("name-already-defined", NameAlreadyDefined "x", makeCtx)
  , ("type-already-defined", TypeAlreadyDefined "User", makeCtx)
  , ("name-already-exported", NameAlreadyExported "greet", makeCtx)
  , ("type-annotation-name-mismatch", TypeAnnotationNameMismatch "foo" "bar", makeCtx)
  , ("should-be-typed-or-above", ShouldBeTypedOrAbove "parse", makeCtx)
  , ("not-in-scope", NotInScope "handler" (Loc 0 5 1), makeCtx)
  , ("recursive-var-access", RecursiveVarAccess "parser", makeCtx)

  -- Type definition errors
  , ("not-capitalized-adt-name", NotCapitalizedADTName "maybe", makeCtx)
  , ("not-capitalized-alias-name", NotCapitalizedAliasName "user", makeCtx)
  , ("not-capitalized-constructor-name", NotCapitalizedConstructorName "just", makeCtx)
  , ("capitalized-adt-tvar", CapitalizedADTTVar "Maybe" "Val", makeCtx)
  , ("adt-already-defined", ADTAlreadyDefined tStr, makeCtx)
  , ("wrong-alias-arg-count", WrongAliasArgCount "Pair" 2 1, makeCtx)
  , ("unknown-type", UnknownType "Optional" [], makeCtx)
  , ("unknown-type-suggestion", UnknownType "Mabye" ["Maybe"], makeCtx)
  , ("types-have-different-origin", TypesHaveDifferentOrigin "User" "a/User.mad" "b/User.mad", makeCtx)

  -- Interface errors
  , ("no-instance-found", NoInstanceFound "Eq" [tStr] [], makeCtx)
  , ("no-instance-found-number-string", NoInstanceFound "Number" [tStr] [], makeCtx)
  , ("interface-not-existing", InterfaceNotExisting "Comonad", makeCtx)
  , ("interface-already-defined", InterfaceAlreadyDefined "Show", makeCtx)
  , ("instance-predicate-error", InstancePredicateError (IsIn "Show" [TVar tvA] Nothing) (IsIn "Show" [TVar tvA, TVar (TV 2 Star)] Nothing) (IsIn "Show" [TVar tvA] Nothing), makeCtx)
  , ("overlapping-instances", OverlappingInstances (IsIn "Show" [tStr] Nothing) (IsIn "Show" [TVar tvA] Nothing), makeCtx)
  , ("self-referential-instance", SelfReferentialInstance (IsIn "Show" [TVar tvA] Nothing), makeCtx)
  , ("deriving-alias-not-allowed", DerivingAliasNotAllowed "Name", makeCtx)
  , ("invalid-interface-derived", InvalidInterfaceDerived "Functor", makeCtx)
  , ("method-name-already-defined", MethodNameAlreadyDefined, makeCtx)

  -- Import errors
  , ("import-not-found", ImportNotFound "List", makeCtx)
  , ("import-collision", ImportCollision "map", makeCtx)
  , ("not-exported", NotExported "filter" "List.mad" [], makeCtx)
  , ("not-exported-suggestion", NotExported "fliter" "List.mad" ["filter"], makeCtx)
  , ("import-cycle", ImportCycle ["A.mad", "B.mad", "A.mad"], makeCtx)

  -- Mutation errors
  , ("bad-mutation", BadMutation, makeCtx)
  , ("mutating-not-in-scope", MutatingNotInScope "counter", makeCtx)
  , ("mutating-pattern-bound-variable", MutatingPatternBoundVariable "x", makeCtx)
  , ("mutation-restriction", MutationRestriction, makeCtx)
  , ("mutating-function", MutatingFunction "myFn", makeCtx)
  , ("overloaded-mutation", OverloadedMutation "x" [IsIn "Show" [tStr] Nothing], makeCtx)

  -- Record errors
  , ("record-duplicate-fields", RecordDuplicateFields ["name", "age"], makeCtx)
  , ("record-missing-fields", RecordMissingFields ["email", "phone"], makeCtx)
  , ("record-extra-fields", RecordExtraFields ["extra"] ["name", "age"], makeCtx)
  , ("record-duplicate-rest-pattern", RecordDuplicateRestPattern, makeCtx)
  , ("invalid-lhs", InvalidLhs, makeCtx)

  -- Literal errors
  , ("bad-escape-sequence", BadEscapeSequence, makeCtx)
  , ("empty-char", EmptyChar, makeCtx)
  , ("byte-out-of-bounds", ByteOutOfBounds "300", makeCtx)
  , ("short-out-of-bounds", ShortOutOfBounds "99999999999", makeCtx)
  , ("int-out-of-bounds", IntOutOfBounds "99999999999999999999", makeCtx)
  , ("negated-byte", NegatedByte, makeCtx)

  -- Pattern errors
  , ("refutable-pattern-in-parameter", RefutablePatternInParameter, makeCtx)

  -- Other errors
  , ("grammar-error", GrammarError "test/Module.mad" "unexpected token '}'", makeCtx)
  , ("not-a-definition", NotADefinition, makeCtx)
  , ("not-a-constructor", NotAConstructor "myVar", makeCtx)
  , ("illegal-skip-access", IllegalSkipAccess, makeCtx)
  , ("no-main", NoMain, makeCtx)
  , ("main-invalid-typing", MainInvalidTyping, makeCtx)
  , ("test-not-valid", TestNotValid tStr, makeCtx)
  , ("wrong-spread-type", WrongSpreadType "Cannot spread Integer", makeCtx)
  , ("constructor-access-bad-index", ConstructorAccessBadIndex "Maybe" "Just" 5 10, makeCtx)
  , ("constructor-access-no-constructor-found", ConstructorAccessNoConstructorFound "EmptyType", makeCtx)
  , ("constructor-access-too-many-constructors", ConstructorAccessTooManyConstructors "Shape" 3, makeCtx)
  , ("fatal-error", FatalError, makeCtx)
  , ("ast-has-no-path", ASTHasNoPath, makeCtx)
  , ("generic-error", Error, makeCtx)
  ]


spec :: Spec
spec = beforeAll_ (setEnv "NO_COLOR" "true") $ do
  describe "Error rendering goldens" $ do
    forM_ goldenCases $ \(name, typeError, ctx) -> do
      it name $ do
        let err = CompilationError typeError ctx
        terminal <- formatError stubReader False err
        lsp      <- simpleFormatErrorWithHints False err
        let json = formatErrorJson err
        shouldMatchGolden (goldenDir </> name <> ".terminal.txt") terminal
        shouldMatchGolden (goldenDir </> name <> ".json.txt") json
        shouldMatchGolden (goldenDir </> name <> ".lsp.txt") lsp
