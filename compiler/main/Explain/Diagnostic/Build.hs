{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- | Builds the diagnostic IR for every compilation error and warning.
--
-- This is the single place where diagnostic *content* (titles, prose, hints,
-- notes, marker placement) is decided. It is pure and makes no color or
-- layout decisions: types, schemes and kinds that need projection-specific
-- rendering (diff highlighting, colors) are carried structurally in
-- 'Section' values and rendered by the projections under "Explain.Render".
module Explain.Diagnostic.Build
  ( errorDiagnostic
  , warningDiagnostic
  ) where

import           Explain.Diagnostic
import           Explain.Format.Hints
import           Explain.Format.TypeDiff        ( prettyPrintType
                                                , prettyPrintType'
                                                , predToStr
                                                , kindToStr
                                                , renderType
                                                , renderTVar
                                                , renderSchemesWithDiff
                                                , renderSchemeOneLine
                                                )
import           Error.Error
import           Error.Warning
import           Error.Context
import           Explain.Location
import           Infer.Type
import           Utils.Tuple                    ( lst )
import           Utils.EditDistance             ( findSimilar )
import           Text.Show.Pretty               ( ppShow )
import           Data.List                      ( intercalate )
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import           Data.Char                      ( toUpper
                                                , toLower
                                                )


-- | An error diagnostic anchored at the context's location when there is one.
-- Mirrors the historical behavior of the rich renderer: with no context the
-- marker label is dropped and only title and notes remain.
mkErr :: String -> Context -> [Section] -> [Note] -> Diagnostic
mkErr title context label notes = case context of
  Context modulePath area ->
    Diagnostic
      { dSeverity = SevError
      , dCode     = Nothing
      , dTitle    = title
      , dMarkers  = [Marker (Span modulePath area) Primary label]
      , dBody     = []
      , dNotes    = notes
      }

  NoContext ->
    errorNowhere title [] notes


-- | Same anchoring behavior as 'mkErr', for warnings.
mkWarn :: String -> Context -> [Section] -> [Note] -> Diagnostic
mkWarn title context label notes = case context of
  Context modulePath area ->
    Diagnostic
      { dSeverity = SevWarning
      , dCode     = Nothing
      , dTitle    = title
      , dMarkers  = [Marker (Span modulePath area) Primary label]
      , dBody     = []
      , dNotes    = notes
      }

  NoContext ->
    Diagnostic
      { dSeverity = SevWarning
      , dCode     = Nothing
      , dTitle    = title
      , dMarkers  = []
      , dBody     = []
      , dNotes    = notes
      }


warningDiagnostic :: Context -> WarningKind -> Diagnostic
warningDiagnostic context warning = case warning of
  UnusedImport name path ->
    mkWarn "Unused import" context
      [ P $
          "You imported '" <> name <> "' from the module located at\n"
          <> "'" <> path <> "'\n"
          <> "but it seems that you never use it."
      ]
      [hint "Remove the import if you don't use it."]

  MissingMethods missingMethods ->
    mkWarn "Missing methods" context
      [ P $
          "The instance does not implement all methods. The missing\n"
          <> "methods are the following:\n"
          <> intercalate "\n" (map ("  - "<>) missingMethods)
      ]
      [hint "Implement the missing methods."]

  UnusedParameter name ->
    mkWarn "Unused parameter" context
      [ P $
          "You declared a parameter named '" <> name <> "' but it seems that\n"
          <> "you never use it"
      ]
      [ hint "Remove this parameter if you don't need it"
      , hint "Use the skip parameter '_' to suppress this warning."
      ]

  UnusedDeclaration name ->
    mkWarn "Unused variable" context
      [ P $
          "You declared a variable named '" <> name <> "' but it seems that\n"
          <> "you never use it"
      ]
      [hint "Remove it if you don't need it."]

  UnusedTopLevelDeclaration name ->
    mkWarn "Unused top level binding" context
      [ P $
          "You declared a top level binding named '" <> name <> "' but it seems that\n"
          <> "you never use it"
      ]
      [hint "Remove it if you don't need it."]

  UnusedConstructor name ->
    mkWarn "Unused constructor" context
      [ P $
          "You declared a constructor named '" <> name <> "' but it seems that\n"
          <> "you never use it"
      ]
      [hint "Remove it if you don't need it."]

  UnusedType name ->
    mkWarn "Unused type" context
      [ P $
          "You declared a type named '" <> name <> "' but it seems that\n"
          <> "you never use it"
      ]
      [hint "Remove it if you don't need it."]

  MadlibVersionMinorTooLow pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  mkWarn "Minor Madlib version is too low" context
          [ P $
              start
              <> " requires the minimum version '"
              <> minVersion
              <> "' but you currently use madlib\n"
              <> "version '"
              <> versionUsed
              <> "'"
          ]
          [hint "Update your version of madlib."]

  MadlibVersionMajorDiffer pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  mkWarn "Minor Madlib version is too low" context
          [ P $
              start
              <> " requires the minimum version '"
              <> minVersion
              <> "' but you currently use madlib\n"
              <> "version '"
              <> versionUsed
              <> "'. Because major versions differ it means there is a breaking\n"
              <> "change and you may not be able to run the project"
          ]
          [hint "Update your version of madlib."]

  IncompletePattern missingPatterns ->
    mkWarn "Incomplete pattern" context
      [ P $
          "The branches do not cover all cases\n"
          <> "Examples of missing patterns:\n"
          <> intercalate "\n" (map ("  - "++) missingPatterns)
      ]
      [ note "If the input of where is not handled by a branch, it will most likely crash at\nruntime."
      , hint "Pattern match the missing constructors or add a catch all branch with '_ => ...'."
      ]

  RedundantPattern ->
    mkWarn "Redundant pattern" context
      [P "Unreachable pattern"]
      [ note "This pattern will never be reached."
      , hint "Remove it or move it higher up so that it might be useful."
      ]

  TypedHoleFound t suggestions ->
    let (renderedType, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t))
        indentedType = unlines $ ("  "<>) <$> lines renderedType
        renderSuggestion (name, sc) =
          name <> " :: " <> renderSchemeOneLine sc
        suggestionsNote =
          if null suggestions then []
          else [note $ "Suggestions (in scope):\n" <> unlines (map (("  " <>) . renderSuggestion) suggestions)]
    in  mkWarn "Typed hole" context
          [P $ "I found a typed hole with type:\n  " <> indentedType]
          ( suggestionsNote <>
            [ note "This will crash at runtime if reached."
            , hint "Replace it with a valid expression of that type."
            ]
          )


errorDiagnostic :: Context -> TypeError -> Diagnostic
errorDiagnostic context typeError = case typeError of
  UnificationError TypeMismatch { tmFound = t1, tmExpected = t2, tmOrigin = origin, tmSecondaries = secondaries } ->
    let secondaryMarkers =
          [ Marker (Span path area) Secondary [P msg]
          | SecondaryLocation path area msg <- secondaries
          ]
        originNotes = case origin of
          FromFunctionArgument fn n (Just (FunctionContext expectedType fullSig _)) ->
            [ hint $ "'" <> fn <> "' expects " <> prettyPrintType True expectedType <> " as its " <> toOrdinal n <> " argument."
            , note $ "Full signature: " <> fn <> " :: " <> prettyPrintType True fullSig
            ]
          FromFunctionArgument fn n Nothing ->
            [ hint $ "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type." ]
          FromFunctionReturn fn ->
            [ hint $ "The return value of '" <> fn <> "' doesn't match its type annotation."
            , note "Check that all branches of the function body return the same type."
            ]
          FromOperator op -> operatorHints op t1 t2
          FromIfCondition ->
            [ hint "The condition of an 'if' expression must be Boolean."
            , note "Boolean values are 'true' and 'false'. Did you forget a comparison?"
            ]
          FromIfBranches ThenBranch ->
            [ hint "The 'then' branch has a different type than the 'else' branch."
            , note "Both branches of an 'if' must return the same type."
            ]
          FromIfBranches ElseBranch ->
            [ hint "The 'else' branch has a different type than the 'then' branch."
            , note "Both branches of an 'if' must return the same type."
            ]
          FromWhileCondition ->
            [ hint "The condition of a 'while' loop must be Boolean." ]
          FromListElement n | n > 0 ->
            [ hint $ "The " <> toOrdinal n <> " element has a different type than the previous elements."
            , note "All elements in a list must have the same type."
            ]
          FromListElement _ ->
            [ hint "All elements in a list literal must have the same type."
            , note "If you need a heterogeneous collection, consider a custom type or a tuple."
            ]
          FromTypeAnnotation ->
            [ hint "The expression's type doesn't match its annotation."
            , note "Check whether the annotation is too specific, or the expression is wrong."
            ]
          FromPatternMatch n | n > 0 ->
            [ hint $ "The " <> toOrdinal n <> " branch returns a different type than the other branches."
            , note "All branches of 'where' must return the same type."
            ]
          FromPatternMatch _ ->
            [ hint "All branches of a 'where' expression must return the same type."
            , note "Make sure every branch has the same return type."
            ]
          FromAssignment name ->
            [ hint $ "The right-hand side doesn't match the declared type of '" <> name <> "'." ]
          NoOrigin -> []
        title = mkUnificationTitle t1 t2 origin
        originPrefix = case origin of
          FromFunctionArgument fn n (Just (FunctionContext expectedType _ _)) ->
            "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type.\n"
            <> "Expected: " <> prettyPrintType True expectedType <> "\n"
          FromFunctionArgument fn n Nothing ->
            "The " <> toOrdinal n <> " argument to '" <> fn <> "' has the wrong type.\n"
          FromFunctionReturn fn ->
            "The return value of '" <> fn <> "' does not match its annotation.\n"
          FromOperator op ->
            "The operands of '" <> op <> "' have incompatible types.\n"
          FromIfCondition ->
            "The condition of an 'if' must be Boolean.\n"
          FromIfBranches ThenBranch ->
            "The 'then' branch has a different type than the 'else' branch.\n"
          FromIfBranches ElseBranch ->
            "The 'else' branch has a different type than the 'then' branch.\n"
          FromWhileCondition ->
            "The condition of a 'while' must be Boolean.\n"
          FromListElement n | n > 0 ->
            "The " <> toOrdinal n <> " element has a different type than the previous elements.\n"
          FromListElement _ ->
            "All list elements must have the same type.\n"
          FromTypeAnnotation ->
            "The expression does not match its type annotation.\n"
          FromPatternMatch n | n > 0 ->
            "The " <> toOrdinal n <> " branch returns a different type than the other branches.\n"
          FromPatternMatch _ ->
            "All branches of a 'where' must return the same type.\n"
          FromAssignment name ->
            "The value does not match the declared type of '" <> name <> "'.\n"
          NoOrigin -> ""
        -- expected is t2, found is t1
        body = [P originPrefix, ExpectedFound t2 t1]
    in  case context of
      Context modulePath area ->
        Diagnostic
          { dSeverity = SevError
          , dCode     = Nothing
          , dTitle    = title
          , dMarkers  = Marker (Span modulePath area) Primary body : secondaryMarkers
          , dBody     = []
          , dNotes    = originNotes
          }

      NoContext ->
        Diagnostic
          { dSeverity = SevError
          , dCode     = Nothing
          , dTitle    = title
          , dMarkers  = []
          , dBody     = body
          , dNotes    = originNotes
          }

  TestNotValid t ->
    mkErr "Invalid test type" context
      [ P "This test expression has type:\n"
      , ShowType t
      , P $ "\nbut tests must return one of:\n"
         <> "  Wish TestResult TestResult\n"
         <> "  List (Wish TestResult TestResult)"
      ]
      [ hint "A test must evaluate to a Wish that resolves to a TestResult."
      , note "Use 'assertEqual' or 'test' from the Test module to build test values."
      ]

  TypingHasWrongKind t expectedKind actualKind ->
    let prettyT = prettyPrintType False t
    in  mkErr "Kind mismatch in type annotation" context
          [ P $ "The type '" <> prettyT <> "' has kind " <> kindToStr actualKind
             <> ", but kind " <> kindToStr expectedKind <> " was expected.\n"
          , ExpectedFoundKind expectedKind actualKind
          ]
          [ note $
              "Kinds describe how many type arguments a type constructor takes.\n"
              <> "'*' means a concrete type (like 'Int').\n"
              <> "'* -> *' means it takes one argument (like 'List' or 'Maybe')."
          , hint $ "'" <> prettyT <> "' needs "
              <> (if actualKind == Star then "no" else "fewer")
              <> " type arguments, but "
              <> kindToStr expectedKind <> " was expected here."
          ]

  NoMain ->
    errorNowhere "Missing 'main' function" []
      [ hint "Add a 'main' function to your entry module."
      , note "The simplest valid main:\n  main = () => { IO.log(\"Hello!\") }"
      ]

  MainInvalidTyping ->
    mkErr "Invalid 'main' signature" context
      [P "The 'main' function has the wrong type. It must accept 'List String' and return '{}'."]
      [ hint "Change the signature to:  main :: List String -> {}"
      , note "You can also omit the type annotation entirely and let it be inferred."
      ]

  MutationRestriction ->
    mkErr "Mutation restriction" context
      [P "This binding depends on a closure that performs mutation and cannot be made polymorphic."]
      [ hint "Add an explicit type annotation with concrete (non-polymorphic) types."
      , note "Mutation requires a fixed memory location; polymorphic types would create separate copies."
      ]

  MutatingFunction n ->
    mkErr "Cannot mutate function" context
      [P $ "'" <> n <> "' is a function and cannot be mutated with ':='."]
      [ note "Functions are immutable values. You cannot reassign them."
      , hint $ "To make '" <> n <> "' swappable, wrap it in a mutable record:\n"
          <> "  ref = { fn: " <> n <> " }\n"
          <> "  ref.fn := newFunction    // mutate the record field"
      ]

  NotAConstructor n ->
    let capitalHint =
          if not (null n) && (head n >= 'a' && head n <= 'z') then
            [note $ "'" <> n <> "' starts with a lowercase letter. Constructors must be capitalized (e.g. 'Just', 'Nothing', 'Left')."]
          else
            []
    in  mkErr "Not a constructor" context
          [P $ "'" <> n <> "' is used in a pattern but is not a constructor."]
          ( capitalHint ++
            [ hint "Only data constructors (capitalized names) can appear in patterns."
            , note "Example pattern:  where | Just x => ...   | Nothing => ..."
            ]
          )

  MethodNameAlreadyDefined ->
    mkErr "Method name already defined" context
      [P "This method name is already used by another interface."]
      [ note "Two interfaces cannot define methods with the same name."
      , hint "Choose a different name, or prefix it with the interface name to avoid collisions."
      ]

  NotADefinition ->
    mkErr "Not a definition" context
      [P "This expression appears at the top level, but only definitions are allowed here."]
      [ note "A module's top level can only contain: type definitions, assignments, imports, exports, and interface/instance declarations."
      , hint "Assign it to a name:  myValue = <expression>"
      ]

  ConstructorAccessBadIndex typeName constructorName arity index ->
    mkErr "Constructor index out of range" context
      [ P $ "The constructor '" <> constructorName <> "' from '" <> typeName <> "' has "
         <> show arity <> " parameter" <> (if arity == 1 then "" else "s")
         <> ", but you are trying to access index " <> show index <> "."
      ]
      [ hint $ "Valid indices for '" <> constructorName <> "' are 0 to " <> show (arity - 1) <> "."
      , note "Constructor parameter access uses zero-based indexing."
      ]

  ConstructorAccessNoConstructorFound typeName ->
    mkErr "Constructor not found" context
      [P $ "The type '" <> typeName <> "' has no constructors that can be accessed this way."]
      [ hint "Use pattern matching to destructure the value instead." ]

  ConstructorAccessTooManyConstructors typeName _ ->
    mkErr "Ambiguous constructor access" context
      [P $ "Cannot access a constructor parameter of '" <> typeName <> "' because it has more than one constructor."]
      [ hint "Use pattern matching to safely handle each constructor case."
      , note "Constructor parameter access only works on types with a single constructor."
      ]

  InfiniteType tv t ->
    let (vars, hkVars, printedT) = prettyPrintType' True (mempty, mempty) t
        (_, _, printedN)         = prettyPrintType' True (vars, hkVars) (TVar tv)
    in  mkErr "Infinite type" context
          [ P $ "I can't construct the type '" <> printedN <> "' because it would need to contain itself:\n"
             <> "  " <> printedN <> " = " <> printedT
          ]
          [ note $
              "This happens when the type checker tries to unify a type variable with\n"
              <> "a type that contains that same variable, creating an infinite loop."
          , hint "Common causes:"
          , note $
              "  1. A function applied to itself: f(f) — wrap it in a lambda: f((_) => f)\n"
              <> "  2. A recursive data structure without a type alias\n"
              <> "  3. A missing type annotation on a recursive function"
          ]

  IllegalSkipAccess ->
    mkErr "Cannot use '_' as a value" context
      [P "The skip symbol '_' is a placeholder for values you don't need. It cannot be read."]
      [ hint "Give it a name if you need to use the value:  (x) => x  instead of  (_) => _"
      , note "'_' means 'I don't care about this value'. To use it, replace '_' with a named variable."
      ]

  UnboundVariable n suggestions ->
    let typoHint = case suggestions of
                     []  -> [hint "Check for a typo in the name."]
                     [s] -> [hint $ "Did you mean '" <> s <> "'?"]
                     _   -> [hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
        stdlibHint = case List.lookup n stdlibMap of
                       Just modName -> [note $ "'" <> n <> "' is defined in the '" <> modName <> "' module. Add: import " <> modName <> " from \"" <> modName <> "\""]
                       Nothing      -> [note $ "If '" <> n <> "' is from another module, make sure you have imported it."]
    in  mkErr "Unbound variable" context
          [P $ "'" <> n <> "' is not defined in this scope."]
          (typoHint ++ stdlibHint)

  UnboundUnknownTypeVariable ->
    mkErr "Unbound type variable" context [P "A type variable has not been declared"]
      [hint "Verify that you don't have a typo"]

  UnboundVariableFromNamespace namespace name ->
    mkErr "Name not in module" context
      [P $ "'" <> name <> "' was not found in '" <> namespace <> "'."]
      [ hint $ "Check that '" <> name <> "' is exported from the module you imported as '" <> namespace <> "'."
      , note $ "With a default import 'import List from \"List\"', use 'List.map', 'List.length', etc."
      ]

  CapitalizedADTTVar adtname param ->
    let lowered = if null param then param else map toLower param
    in  mkErr "Capitalized ADT variable" context
          [ P $ "The type parameter '" <> param <> "' in the type declaration\n"
             <> "'" <> adtname <> "' is capitalized. Type parameters can't be capitalized."
          ]
          [ hint "Either remove it if you don't need the type variable, or\nmake its first letter lowercase."
          , hint $ "Change '" <> param <> "' to '" <> lowered <> "'"
          ]

  UnboundType n suggestions ->
    let typoHint = case suggestions of
                     []  -> [ hint "Check for a typo in the type name."
                             , note $ "If '" <> n <> "' is defined in another module, import it: 'import Type from \"./Module\"'"
                             ]
                     [s] -> [hint $ "Did you mean '" <> s <> "'?"]
                     _   -> [hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkErr "Unknown type" context
          [P $ "The type '" <> n <> "' is not defined in this scope."]
          typoHint

  ByteOutOfBounds n ->
    mkErr "Byte out of bounds" context
      [P $ "The literal '" <> n <> "_b' is too big, the maximum value for bytes is 255"]
      [ hint "Use a value between 0 and 255, or use 'Integer' for larger numbers." ]

  ShortOutOfBounds n ->
    mkErr "Short out of bounds" context
      [P $ "The literal '" <> n <> "_s' is too big, the maximum value for shorts is "<> show (2^31 - 1 :: Integer) <>"."]
      [ hint "Use a value within the short range, or use 'Integer' for larger numbers." ]

  IntOutOfBounds n ->
    mkErr "Integer out of bounds" context
      [P $ "The literal '" <> n <> "_i' is too big, the maximum value for integers is "<> show (2^63 - 1 :: Integer) <>"."]
      [ hint "Use a Float for very large numbers, or restructure the computation." ]

  NegatedByte ->
    mkErr "Negated byte" context [P "Bytes can't be negated"]
      [ note "Bytes are unsigned integers in range 0-255 and cannot be negative."
      , hint "Use 'Integer' or 'Short' if you need signed numbers."
      ]

  DerivingAliasNotAllowed n ->
    mkErr "Cannot derive for type alias" context
      [P $ "'" <> n <> "' is a type alias, not a concrete type. You cannot derive instances for aliases."]
      [ hint "Derive the instance on the underlying type that the alias refers to."
      , note "Type aliases are transparent — instances on the underlying type apply automatically."
      ]

  InvalidInterfaceDerived n ->
    mkErr ("Cannot derive '" <> n <> "'") context
      [P $ "The interface '" <> n <> "' does not support automatic derivation."]
      [ note "Only these interfaces can be derived: Eq, Show, Comparable, Json."
      , hint $ "Write a manual instance instead:\n  instance " <> n <> " YourType { ... }"
      ]

  SignatureTooGeneral scGiven scInferred ->
    mkErr "Signature too general" context
      [GivenInferred scGiven scInferred]
      [ note $
          "The annotation claims the function is more polymorphic than the implementation allows.\n"
          <> "The inferred type is more specific — it uses concrete types or fewer type variables."
      , hint "Update the type annotation to match the inferred type shown above."
      ]

  NoInstanceFound cls ts chain ->
    let typeStr    = unwords (prettyPrintType True <$> ts)
        predStr    = lst (predToStr True (mempty, mempty) (IsIn cls ts Nothing))
        smartHints = noInstanceSmartHints cls ts
        -- The required-by chain (innermost first): each entry is an outer
        -- predicate whose instance resolution needed this one.
        chainNotes =
          [ note $ "Required by '" <> lst (predToStr True (mempty, mempty) parent) <> "'."
          | parent <- chain
          ]
        chainMarkers = case context of
          Context modulePath _ ->
            [ Marker (Span modulePath area) Secondary
                [P $ "required by '" <> lst (predToStr True (mempty, mempty) parent) <> "', from here"]
            | parent@(IsIn _ _ (Just area)) <- chain
            ]

          NoContext ->
            []
        stdHints   =
          [ hint $ "Make sure '" <> typeStr <> "' implements the '" <> cls <> "' interface."
          , note $ "Instance methods are automatically in scope when their module is imported,\ndirectly or transitively."
          ]
        base = mkErr "No instance found" context
          [P $ "'" <> predStr <> "' is required here, but no instance was found for '" <> typeStr <> "'."]
          (chainNotes ++ smartHints ++ stdHints)
    in  base { dMarkers = dMarkers base ++ chainMarkers }

  AmbiguousType (TV _ _, IsIn cls _ maybeArea : _) ->
    case context of
      Context modulePath area ->
        Diagnostic
          { dSeverity = SevError
          , dCode     = Nothing
          , dTitle    = "Ambiguous type"
          , dMarkers  =
              Marker (Span modulePath area) Primary
                [P $ "An instance of '" <> cls <> "' could not be found"]
              : case maybeArea of
                  Just area' ->
                    [Marker (Span modulePath area') Secondary [P "The constraint originates from here"]]

                  Nothing ->
                    []
          , dBody     = []
          , dNotes    = [hint "You can add a type annotation to make it resolvable."]
          }

      NoContext ->
        errorNowhere "Ambiguous type" []
          [hint "You can add a type annotation to make it resolvable."]

  AmbiguousType (TV n _, []) ->
    mkErr "Ambiguous type" context
      [P $ "An ambiguity for the type variable '" <> renderTVar n <> "' could not be resolved"]
      [hint "You can add a type annotation to make it resolvable."]

  InterfaceNotExisting cls ->
    mkErr "Interface not found" context [P $ "The interface '" <> cls <> "' is not defined.\n"]
      [hint "Make sure you imported the module defining it,\nor a module that imports it."]

  KindError (t, k) (t', k') ->
    mkErr "Kind mismatch" context
      [ P $ "'" <> prettyPrintType True t <> "' has kind " <> kindToStr k
         <> ",\nbut '" <> prettyPrintType True t' <> "' has kind " <> kindToStr k' <> "."
      ]
      [ note $
          "Kinds describe how many type arguments a type constructor takes.\n"
          <> "'*' means a fully-applied type (like 'Int' or 'String').\n"
          <> "'* -> *' means a type that takes one argument (like 'List' or 'Maybe')."
      , hint "Check whether you applied too many or too few type arguments."
      ]

  InstancePredicateError pInstance pWrong pCorrect ->
    let instStr    = lst (predToStr True (mempty, mempty) pInstance)
        wrongStr   = lst (predToStr True (mempty, mempty) pWrong)
        correctStr = lst (predToStr True (mempty, mempty) pCorrect)
    in  mkErr "Instance constraint error" context
          [ P $ "The instance '" <> instStr <> "' has an incorrect constraint.\n"
             <> "  Given:    " <> wrongStr <> "\n"
             <> "  Expected: " <> correctStr
          ]
          [ hint $ "Replace '" <> wrongStr <> "' with '" <> correctStr <> "' in the instance declaration."
          , note $
              "Instance constraints must use the same type variables as the instance head,\n"
              <> "and must match the shape required by the interface definition."
          ]

  ImportCycle paths ->
    case context of
      Context modulePath area ->
        Diagnostic
          { dSeverity = SevError
          , dCode     = Nothing
          , dTitle    = "Circular import"
          , dMarkers  =
              [ Marker (Span modulePath area) Primary
                  [ P $ "This import creates a cycle:\n\n" <> buildCycleOutput paths ]
              ]
          , dBody     = []
          , dNotes    =
              [ note "Circular imports are not allowed because there is no valid initialization order."
              , hint "Extract the shared types or functions into a third module that both can import."
              , hint "Alternatively, move the code that causes the cycle into one of the two modules."
              ]
          }

      NoContext ->
        errorNowhere "Circular import" []
          [ note "Circular imports are not allowed because there is no valid initialization order."
          , hint "Extract the shared types or functions into a third module that both can import."
          ]
   where
    buildCycleOutput :: [FilePath] -> String
    buildCycleOutput [] = ""
    buildCycleOutput ps =
      let loop [] = ""
          loop [p]    = "  " <> p <> "\n  ↓  (loops back to start)"
          loop (p:rest) = "  " <> p <> "\n  ↓\n" <> loop rest
      in  loop ps

  TypeAnnotationNameMismatch typingName expName ->
    mkErr "Type annotation name mismatch" context
      [P $ "The type annotation is for '" <> typingName <> "' but the following definition is for '" <> expName <> "'."]
      [ hint $ "Rename the annotation to match: '" <> expName <> " :: <type>'"
      , note "A type annotation must immediately precede the definition it annotates and share the same name."
      ]

  GrammarError _ msg ->
    let cleanMsg = if null msg then "Unexpected token" else msg
        trimmed  = List.dropWhileEnd (\c -> c == '\n' || c == '\r') cleanMsg
        smartHints = grammarSmartHints trimmed
        stdHints =
          [ hint "Check for a missing bracket, parenthesis, or operator near this location."
          , note "Common causes: unclosed '(', '{', or '['; a missing '->' in a function; or a typo in a keyword."
          ]
        -- Only include generic hints when no smart hints are available
        notes' = if null smartHints then stdHints else smartHints
    in  mkErr "Syntax error" context [P trimmed] notes'

  BadEscapeSequence ->
    mkErr "Bad escape sequence" context [P "This escape sequence is not valid"]
      [ hint "Valid escape sequences are either a byte: \\xAB or a unicode: \\uABCD or \\u{ABCDEF} up to 10FFFF"
      ]

  EmptyChar ->
    mkErr "Empty Char" context [P "This character is empty"]
      [ note "Characters can't be empty"
      ]

  UnknownType t suggestions ->
    let notes' = case suggestions of
                   []  -> [hint "Verify that you imported it"]
                   [s] -> [hint $ "Did you mean '" <> s <> "'?"]
                   _   -> [hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkErr "Unknown type" context [P $ "The type '" <> t <> "' was not found"] notes'

  NameAlreadyDefined name ->
    mkErr "Illegal shadowing" context [P $ "The variable '" <> name <> "' is already defined"]
      [ hint "Change the name of the variable"
      , note $
          "The variable might be defined further down. All top level\n"
          <> "assignments share the scope and using a local name that is\n"
          <> "defined in the global scope of a module is not allowed."
      ]

  ImportCollision name ->
    mkErr "Import collision" context
      [P $ "The imported name '" <> name <> "' is already used"]
      [hint "Use a qualified import or rename one of the conflicting names."]

  TypeAlreadyDefined name ->
    mkErr "Type already defined" context [P $ "The type '" <> name <> "' is already defined"]
      [hint "Change the name of the type"]

  NameAlreadyExported name ->
    mkErr "Already exported" context
      [P $ "'" <> name <> "' appears more than once in the export list."]
      [ hint $ "Remove the duplicate export of '" <> name <> "'."
      , note "Each name can only be exported once from a module."
      ]

  NotExported name path suggestions ->
    let notes' = case suggestions of
                   []  -> [ hint $ "Add 'export { " <> name <> " }' to the module at '" <> path <> "' if you own it."
                          , note "Or check the module's documentation to find the correct exported name."
                          ]
                   [s] -> [hint $ "Did you mean '" <> s <> "'?"]
                   _   -> [hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkErr "Not exported" context
          [ P $ "'" <> name <> "' is not exported by the module at:\n"
             <> "'" <> path <> "'"
          ]
          notes'

  RecursiveVarAccess _ ->
    mkErr "Recursive variable access" context
      [ P $ "This variable refers to itself directly in its own definition\n"
         <> "and cannot be initialized."
      ]
      [ note $
          "A direct self-reference is not allowed. Wrap the recursive\n"
          <> "access in a function to defer evaluation:\n"
          <> "  // Not allowed -- direct self-reference:\n"
          <> "  parser = J.map(Title, J.field(\"title\", parser))\n"
          <> "  // Works -- recursive access is wrapped in a function:\n"
          <> "  parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser)))"
      ]

  NotInScope name (Loc _ line _) ->
    mkErr "Not in scope" context
      [P $ "'" <> name <> "' (at line " <> ppShow line <> ") is not yet defined at this point."]
      [ note "All variables must be defined before they are used."
      , hint $
          "Move the definition of '" <> name <> "' above this usage, or add a type\n"
          <> "annotation to allow forward references."
      ]

  TypesHaveDifferentOrigin adtName origin1 origin2 ->
    mkErr "Types have different origins" context
      [ P $ "The type '" <> adtName <> "' is imported from two different locations:\n"
         <> "  - '" <> origin1 <> "'\n"
         <> "  - '" <> origin2 <> "'\n"
         <> "These are treated as distinct types even though they share a name."
      ]
      [ hint $ "Import '" <> adtName <> "' from only one location, or convert between the two explicitly."
      ]

  ShouldBeTypedOrAbove name ->
    mkErr "Must be typed or above" context
      [P $ "'" <> name <> "' is used before it is defined."]
      [ note $
          "This is fine, but in that case you must give it a type\n"
          <> "annotation."
      , hint $
          "Place that declaration above the place you use it, or give\n"
          <> "it a type annotation."
      ]

  NotCapitalizedADTName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  mkErr "ADT name not capitalized" context
          [P $ "The name '" <> name <> "' of this type is not capitalized"]
          [ note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          , hint $ "Change it to '" <> capitalized <> "'"
          ]

  NotCapitalizedAliasName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  mkErr "Alias name not capitalized" context
          [P $ "The name '" <> name <> "' of this type alias is not capitalized"]
          [ note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          , hint $ "Change it to '" <> capitalized <> "'"
          ]

  NotCapitalizedConstructorName name ->
    let capitalized = if null name then name else toUpper (head name) : tail name
    in  mkErr "Constructor name not capitalized" context
          [P $ "The name '" <> name <> "' of this type constructor is not capitalized"]
          [ note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          , hint $ "Change it to '" <> capitalized <> "'"
          ]

  ContextTooWeak preds ->
    case context of
      Context modulePath area ->
        let positionInfos =
              Maybe.mapMaybe
                (\p@(IsIn _ _ maybeArea) ->
                  case maybeArea of
                    Just area' ->
                      let (_, _, rendererPred) = predToStr True (mempty, mempty) p
                      in  Just
                            ( Marker (Span modulePath area') Primary
                                [P $ "The constraint '" <> rendererPred <> "' originates from here"]
                            )

                    Nothing ->
                      Nothing
                )
                preds
        in  Diagnostic
              { dSeverity = SevError
              , dCode     = Nothing
              , dTitle    = "Context too weak"
              , dMarkers  =
                  Marker (Span modulePath area) Primary
                    [ P $
                        "The context of the type annotation is too weak. The type\n"
                        <> "inferred for the implementation has constraints\n"
                        <> "for the following instances: " <> intercalate ", " (predClass <$> preds)
                    ]
                  : positionInfos
              , dBody     = []
              , dNotes    =
                  [ hint "Add the missing interface constraints to the type annotation."
                  , note $
                      "Example: if the constraint 'Eq a' is missing, change\n"
                      <> "  'myFn :: a -> Boolean'  to  'myFn :: Eq a => a -> Boolean'"
                  ]
              }

      NoContext ->
        errorNowhere "Context too weak" []
          [ hint "Add the missing interface constraints to the type annotation."
          , note $
              "Example: if the constraint 'Eq a' is missing, change\n"
              <> "  'myFn :: a -> Boolean'  to  'myFn :: Eq a => a -> Boolean'"
          ]

  OverloadedMutation n preds ->
    let predNames = intercalate ", " (predClass <$> preds)
    in  mkErr "Mutation in overloaded context" context
          [P $ "Cannot mutate '" <> n <> "' because this function has type class constraints (" <> predNames <> ")."]
          [ note $
              "In an overloaded function, each specialisation creates a new closure copy,\n"
              <> "so mutations to '" <> n <> "' would be invisible to callers."
          , hint "Add a concrete type annotation to remove the polymorphism, or refactor to avoid mutation here."
          ]

  WrongAliasArgCount aliasName expected actual ->
    let exampleArgs = unwords (map (\i -> "Type" <> show i) [1..expected])
        exampleNote = "Example: if '" <> aliasName <> "' takes " <> show expected <> " argument(s), write: " <> aliasName <> " " <> exampleArgs
    in  mkErr "Wrong alias argument count" context
          [ P $ "The alias '" <> aliasName <> "' was expected to have " <> show expected <> " argument" <> (if expected > 1 then "s" else "") <> ",\nbut "
             <> show actual <> " "<> (if actual > 1 then "were" else "was") <>" given"
          ]
          [ hint $
              if actual > expected then
                "Remove " <> show (actual - expected) <> " argument(s)"
              else
                "Add the missing '" <> show (expected - actual) <> "' argument(s)"
          , note exampleNote
          ]

  ImportNotFound importName ->
    let isRelative = List.isPrefixOf "./" importName || List.isPrefixOf "../" importName
        notes' =
          if isRelative then
            [ hint $ "The file '" <> importName <> ".mad' could not be found relative to this module."
            , note "Check the file path and make sure the file exists."
            ]
          else
            [ hint $ "The package '" <> importName <> "' could not be found."
            , note "Run 'madlib install' to install missing dependencies, or check 'madlib.json'."
            ]
    in  mkErr "Import not found" context
          [P $ "The module '" <> importName <> "' could not be found."]
          notes'

  InterfaceAlreadyDefined interfaceName ->
    mkErr "Interface already defined" context
      [P $ "You defined the interface '" <> interfaceName <> "',\nbut it already exists"]
      [hint "Verify that you don't have a typo."]

  OverlappingInstances (IsIn cls _ _) (IsIn _ _ _) ->
    mkErr "Overlapping instances" context
      [P $ "This instance of '" <> cls <> "' overlaps with an existing one."]
      [ hint "Remove the duplicate or overlapping instance, or make instances non-overlapping." ]

  SelfReferentialInstance (IsIn cls _ _) ->
    mkErr "Self-referential instance" context
      [P $ "The instance of '" <> cls <> "' contains itself in its own constraints."]
      [ note "This would cause infinite recursion during type checking."
      , hint "Remove the recursive constraint from the instance declaration."
      ]

  InvalidLhs ->
    mkErr "Invalid left hand side" context [P "It is not a valid left hand side expression."]
      [ note "The left-hand side of '=' must be a variable name, a record pattern, or a list pattern."
      , hint "Valid forms: x = expr, { field } = record, [first] = list"
      ]

  RefutablePatternInParameter ->
    mkErr "Refutable pattern in function parameter" context
      [P "This pattern may not match all possible values."]
      [ note "Function parameter patterns must be irrefutable (guaranteed to match).\nOnly tuple (#[a, b]) and record ({ name, age }) patterns are allowed."
      , hint "Use a 'where' expression for refutable patterns:\n  (x) => where(x) { Just(val) => ... }"
      ]

  BadMutation ->
    mkErr "Bad mutation" context
      [P "Cannot reassign a variable with '='. Use ':=' to mutate an existing binding."]
      [ hint "Use the mutation operator ':=' to change an existing value."
      , note "Example:  x = 0         // initial binding\n         x := x + 1    // mutation"
      ]

  MutatingNotInScope name ->
    mkErr "Not in scope" context
      [P $ "Cannot mutate '" <> name <> "' because it is not in scope."]
      [ hint $ "Declare '" <> name <> "' before mutating it, or check for a typo."
      , note "The variable must be in scope (declared earlier in the same block or outer scope)."
      ]

  MutatingPatternBoundVariable name ->
    mkErr "Cannot mutate pattern-bound variable" context
      [P $ "'" <> name <> "' is bound by pattern matching and cannot be mutated."]
      [hint $ "Introduce a local let binding first: " <> name <> " = <patternVar>, then use ':=' on that."]

  ADTAlreadyDefined adtType ->
    let adtName = renderType adtType
    in  mkErr "Type already defined" context
          [P $ "You defined the type '" <> adtName <> "',\nbut it already exists"]
          [hint "Verify that you don't have a typo."]

  RecordDuplicateFields fs ->
    let fs' = concatMap ("\n - " ++) fs
    in  mkErr "Record duplicate fields" context
          [P $ "The following fields appear more than once in the record constructor:" <> fs']
          [hint "Define each field only once."]

  RecordMissingFields fs ->
    let fieldList  = intercalate ", " (map (\f -> "'" <> f <> "'") fs)
        (one, were, them) = if length fs == 1 then ("field", "was", "it") else ("fields", "were", "them")
    in  mkErr "Record missing fields" context
          [P $ "The record " <> were <> " missing the " <> one <> ": " <> fieldList]
          [ hint $ "Add " <> them <> " to the record literal, for example: { " <> List.intercalate ", " (map (\f -> f <> ": <value>") fs) <> " }"
          ]

  RecordExtraFields fs availableFields ->
    let fieldList = intercalate ", " (map (\f -> "'" <> f <> "'") fs)
        suggestions = concatMap (\f ->
          let similar = findSimilar f availableFields
          in  case similar of
                []  -> []
                [s] -> [f <> " -> " <> s]
                _   -> [f <> " -> one of: " <> intercalate ", " similar]
          ) fs
        notes' = case suggestions of
                   [] -> [hint "Remove the extra fields or check for a typo in a field name."]
                   _  -> [hint $ "Did you mean: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkErr "Record extra fields" context
          [P $ "The record has unexpected fields: " <> fieldList]
          notes'

  RecordDuplicateRestPattern ->
    mkErr "Duplicate rest pattern" context
      [P "A record pattern can only have one rest/spread pattern ('...')."]
      [ hint "Remove the extra '...' and keep only one."
      , note "Example:  { x, ...rest } = myRecord  -- only one spread allowed"
      ]

  WrongSpreadType t ->
    mkErr "Type error" context [P t]
      [ note "The spread operator '...' is only valid on record types."
      , hint "Check that the value you are spreading is a record, or remove the spread."
      ]

  FatalError ->
    mkErr "Internal compiler error" context
      [P "The compiler encountered an unexpected internal state and could not continue."]
      [ hint "This is likely a compiler bug. Please report it with the code that triggered it."
      , note "You can try reorganizing the problematic expression or adding a type annotation."
      ]

  Error ->
    mkErr "Error" context [P "An error occurred during compilation."]
      [ hint "Check the surrounding code for type mismatches or missing imports." ]

  ASTHasNoPath ->
    mkErr "Module not found" context
      [P "A required module could not be located or loaded."]
      [ hint "Verify that all imports resolve to existing files."
      , note "If this is a package dependency, run 'madlib install' to fetch it."
      ]
