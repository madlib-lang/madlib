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
                                                , firstDifference
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
        detector = detectSpecialCase t1 t2 origin
        title = mkUnificationTitle t1 t2 origin

        -- Concrete, imperative next steps — go in Hints, one idea each.
        detectorHints = case detector of
          Just MaybeVsInner ->
            [hint "Wrap the value: Just(x). Use Nothing where no value exists."]

          Just InnerVsMaybe ->
            [hint "This value is a Maybe. Unwrap it with 'where(x) { Just(v) => ... }' or Maybe.fromMaybe."]

          Just (StringVsChar _) ->
            [hint "Double quotes create a String (\"a\"); single quotes create a Char ('a')."]

          Just MissingApplication ->
            [hint "Is this a function you forgot to call? Try calling it with its arguments."]

          Just FunctionVsCall ->
            [hint "This function needs more arguments before it produces this type."]

          Just ListVsElement ->
            [hint "Wrap it in a list: [x]."]

          Just ElementVsList ->
            [hint "This is a List — did you mean 'List.head' to take one element, or 'map' to transform each one?"]

          Just (NumericMismatch _ _) ->
            [hint "Numbers are never converted implicitly. Convert explicitly, e.g. with 'Integer.toFloat' or 'Number.fromString'."]

          _ ->
            []

        originHints = case origin of
          FromFunctionArgument fn n (Just (FunctionContext expectedType fullSig _)) ->
            [ hint $ "'" <> fn <> "' expects " <> prettyPrintType True expectedType <> " as its " <> toOrdinal n <> " argument."
            , note $ "Signature: " <> fn <> " :: " <> prettyPrintType True fullSig
            ]

          FromFunctionArgument{} ->
            []

          FromFunctionReturn fn ->
            [ note $ "Check that every branch of '" <> fn <> "'s body returns the same type." ]

          FromOperator op ->
            operatorHints op t1 t2

          FromIfCondition ->
            []

          FromWhileCondition ->
            []

          FromIfBranches{} ->
            [ note "An 'if' used as an expression must produce the same type from both branches." ]

          FromListElement{} ->
            [ note "All elements of a list must share one type. For mixed data, define a union type: type Item = A(...) | B(...)." ]

          FromTypeAnnotation ->
            case firstDifference t1 t2 of
              Just place ->
                [note $ "Specifically, " <> place <> " differs."]

              Nothing ->
                []

          FromPatternMatch{} ->
            [ note "All branches of a 'where' must return the same type." ]

          FromAssignment name ->
            [ note $ "':=' cannot change the type of '" <> name <> "'." ]

          TooManyArguments{} ->
            []

          NoOrigin ->
            []

        -- The primary label: what the reader is looking at, and what it should
        -- have been instead. Each origin phrases the two sides in terms of the
        -- construct it belongs to instead of a bare "expected:/but found:".
        body = case origin of
          FromFunctionArgument fn n _ ->
            [ P $ "this argument is" ] ++ [ShowType t1]
              ++ [ P $ "but '" <> fn <> "' expects its " <> toOrdinal n <> " argument to be" ] ++ [ShowType t2]

          FromFunctionReturn fn ->
            [ P "the body produces" ] ++ [ShowType t1]
              ++ [ P $ "but the annotation says '" <> fn <> "' returns" ] ++ [ShowType t2]

          FromIfCondition ->
            [ P "this has type" ] ++ [ShowType t1] ++ [ P "but an 'if' condition must be Boolean" ]

          FromWhileCondition ->
            [ P "this has type" ] ++ [ShowType t1] ++ [ P "but a 'while' condition must be Boolean" ]

          FromIfBranches ThenBranch ->
            [ P "this branch is" ] ++ [ShowType t1] ++ [ P "but the other branch is" ] ++ [ShowType t2]

          FromIfBranches ElseBranch ->
            [ P "this branch is" ] ++ [ShowType t1] ++ [ P "but the other branch is" ] ++ [ShowType t2]

          FromListElement n | n > 0 ->
            [ P $ "the " <> toOrdinal n <> " element is" ] ++ [ShowType t1]
              ++ [ P "but the elements before it are" ] ++ [ShowType t2]

          FromListElement _ ->
            [ExpectedFound t2 t1]

          FromTypeAnnotation ->
            [ P "the annotation says" ] ++ [ShowType t2] ++ [ P "but the implementation produces" ] ++ [ShowType t1]

          FromPatternMatch n | n > 0 ->
            [ P $ "the " <> toOrdinal n <> " branch returns" ] ++ [ShowType t1]
              ++ [ P "but the previous branches return" ] ++ [ShowType t2]

          FromPatternMatch _ ->
            [ExpectedFound t2 t1]

          FromAssignment name ->
            [ P "this value is" ] ++ [ShowType t1]
              ++ [ P $ "but '" <> name <> "' was declared with type" ] ++ [ShowType t2]

          FromOperator _ ->
            [ExpectedFound t2 t1]

          TooManyArguments fn n ->
            [ P $ "this is the " <> toOrdinal (n + 1) <> " argument, but '" <> fn <> "' only accepts " <> countArguments n ]

          NoOrigin ->
            [ExpectedFound t2 t1]

    in  case context of
      Context modulePath area ->
        Diagnostic
          { dSeverity = SevError
          , dCode     = Nothing
          , dTitle    = title
          , dMarkers  = Marker (Span modulePath area) Primary body : secondaryMarkers
          , dBody     = []
          , dNotes    = detectorHints ++ originHints
          }

      NoContext ->
        Diagnostic
          { dSeverity = SevError
          , dCode     = Nothing
          , dTitle    = title
          , dMarkers  = []
          , dBody     = [ExpectedFound t2 t1]
          , dNotes    = detectorHints ++ originHints
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
    mkErr "This type variable isn't declared" context
      [P "I don't recognize this type variable here."]
      [ hint "Type variables in a constructor must also appear in the type's own parameter list."
      , note "Type variables are lowercase; a capitalized name here is treated as a concrete type instead, which is a common source of this error."
      ]

  UnboundVariableFromNamespace namespace name suggestions ->
    let suggestionHint = case suggestions of
          []    -> [hint $ "Check that '" <> name <> "' is exported from the module you imported as '" <> namespace <> "'."]
          s : _ -> [hint $ "Did you mean '" <> namespace <> "." <> s <> "'?"]
    in  mkErr "Name not in module" context
          [P $ "'" <> name <> "' was not found in '" <> namespace <> "'."]
          ( suggestionHint ++
            [ note "With a default import 'import List from \"List\"', use 'List.filter', 'List.length', etc." ]
          )

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
          , dTitle    = "I can't infer a concrete type here"
          , dMarkers  =
              Marker (Span modulePath area) Primary
                [P $ "this needs a '" <> cls <> "' instance, but nothing here tells me which type to pick"]
              : case maybeArea of
                  Just area' ->
                    [Marker (Span modulePath area') Secondary [P "the constraint comes from here"]]

                  Nothing ->
                    []
          , dBody     = []
          , dNotes    = [hint $ "Add a type annotation, e.g. a signature that fixes the type implementing '" <> cls <> "'."]
          }

      NoContext ->
        errorNowhere "I can't infer a concrete type here" []
          [hint "Add a type annotation to make it resolvable."]

  AmbiguousType (TV n _, []) ->
    mkErr "I can't infer a concrete type here" context
      [P $ "nothing determines a concrete type for '" <> renderTVar n <> "' here"]
      [hint "Add a type annotation to make it resolvable."]

  InterfaceNotExisting cls ->
    mkErr "Interface not found" context [P $ "The interface '" <> cls <> "' is not defined.\n"]
      [hint "Make sure you imported the module defining it,\nor a module that imports it."]

  KindError (t, k) (t', k') ->
    let arity k0 = case k0 of { Kfun _ rest -> 1 + arity rest; _ -> 0 :: Int }
        headName = prettyPrintType True t'
        neededMore = arity k > arity k'
    in  mkErr ("'" <> headName <> "' needs " <> (if neededMore then "more" else "fewer") <> " type arguments") context
          [ P $ "'" <> prettyPrintType True t <> "' has kind " <> kindToStr k
             <> ", but '" <> headName <> "' has kind " <> kindToStr k' <> " here."
          ]
          [ hint $ "'" <> headName <> "' takes " <> show (arity k') <> " type argument" <> (if arity k' == 1 then "" else "s")
              <> "; this usage gives it " <> show (arity k) <> "."
          , note $
              "Kinds describe how many type arguments a type constructor takes.\n"
              <> "'*' means a fully-applied type (like 'Integer' or 'String').\n"
              <> "'* -> *' means a type that still takes one argument (like 'List' or 'Maybe')."
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
    mkErr "This Char is empty" context
      [P "There's no character between these quotes."]
      [ hint "Use a String (\"\") for empty text, or put exactly one character between single quotes ('a')."
      ]

  UnknownType t suggestions ->
    let notes' = case suggestions of
                   []  -> [hint "Verify that you imported it"]
                   [s] -> [hint $ "Did you mean '" <> s <> "'?"]
                   _   -> [hint $ "Did you mean one of: " <> intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"]
    in  mkErr "Unknown type" context [P $ "The type '" <> t <> "' was not found"] notes'

  NameAlreadyDefined name ->
    mkErr ("'" <> name <> "' is defined twice") context
      [P $ "'" <> name <> "' is declared again here."]
      [ hint $ "Rename this binding, or remove the earlier '" <> name <> "'."
      , note "All top-level assignments in a module share one scope, so a name can only be bound once — even if the other definition appears later in the file."
      ]

  ImportCollision name ->
    mkErr "Import collision" context
      [P $ "The imported name '" <> name <> "' is already used"]
      [hint "Use a qualified import or rename one of the conflicting names."]

  TypeAlreadyDefined name ->
    mkErr ("'" <> name <> "' is defined twice") context
      [P $ "The type '" <> name <> "' is declared again here."]
      [hint $ "Rename this type, or remove the earlier '" <> name <> "'."]

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

  WrongInterfaceArgCount interfaceName expected actual ->
    mkErr "Wrong interface argument count" context
      [ P $ "The interface '" <> interfaceName <> "' expects " <> show expected <> " argument"
            <> (if expected == 1 then "" else "s") <> ", but " <> show actual <> " were given." ]
      [ hint $ if actual > expected
          then "Remove " <> show (actual - expected) <> " argument(s)."
          else "Add " <> show (expected - actual) <> " argument(s)."
      ]

  InstanceResolutionCycle preds ->
    mkErr "Recursive instance resolution" context
      [P "Resolving this constraint repeats an earlier instance goal."]
      [note $ "Cycle: " <> intercalate " -> " (predClass <$> preds)]

  SuperclassCycle classes ->
    mkErr "Recursive superclass hierarchy" context
      [P "This interface declaration introduces a superclass cycle."]
      [note $ "Cycle: " <> intercalate " -> " classes]

  InvalidInstanceContext pred ->
    mkErr "Invalid instance context" context
      [P $ "The instance context cannot provide required evidence for '" <> predClass pred <> "'."]
      [hint "Add the required constraint to the instance context or make the instance head more specific."]

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
    mkErr ("'" <> interfaceName <> "' is defined twice") context
      [P $ "The interface '" <> interfaceName <> "' is declared again here."]
      [hint $ "Rename this interface, or remove the earlier '" <> interfaceName <> "'."]

  OverlappingInstances (IsIn cls ts1 _) (IsIn _ ts2 _) ->
    let headStr1 = lst (predToStr True (mempty, mempty) (IsIn cls ts1 Nothing))
        headStr2 = lst (predToStr True (mempty, mempty) (IsIn cls ts2 Nothing))
    in  mkErr ("Two instances overlap for '" <> cls <> "'") context
          [ P $ "'" <> headStr1 <> "' overlaps with the existing instance '" <> headStr2 <> "'." ]
          [ hint "Remove one of the two instances, or narrow their types so only one can ever apply to a given value." ]

  SelfReferentialInstance (IsIn cls _ _) ->
    mkErr "Self-referential instance" context
      [P $ "The instance of '" <> cls <> "' contains itself in its own constraints."]
      [ note "This would cause infinite recursion during type checking."
      , hint "Remove the recursive constraint from the instance declaration."
      ]

  InvalidLhs ->
    mkErr "You can't assign to this expression" context
      [P "This isn't a name or pattern that a value can be bound to."]
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
    in  mkErr ("'" <> adtName <> "' is defined twice") context
          [P $ "The type '" <> adtName <> "' is declared again here."]
          [hint $ "Rename this type, or remove the earlier '" <> adtName <> "'."]

  RecordDuplicateFields fs ->
    let fs' = concatMap ("\n - " ++) fs
    in  mkErr "Record duplicate fields" context
          [P $ "The following fields appear more than once in the record constructor:" <> fs']
          [hint "Define each field only once."]

  RecordMissingFields fs available ->
    let fieldList  = intercalate ", " (map (\f -> "'" <> f <> "'") fs)
        (one, them) = if length fs == 1 then ("field", "it") else ("fields", "them")
        -- A missing field is sometimes a misspelling of a field the record
        -- does have (e.g. { nmae: "x" } instead of { name: "x" }) rather than
        -- a genuinely absent one; suggest the closest actual field name.
        typoHints =
          [ hint $ "Did you mean '" <> suggestion <> "' instead of '" <> f <> "'?"
          | f <- fs
          , suggestion : _ <- [findSimilar f available]
          ]
    in  mkErr "Record missing fields" context
          [P $ "The record is missing the " <> one <> ": " <> fieldList]
          ( typoHints ++
            [ hint $ "Add " <> them <> " to the record literal, for example: { " <> List.intercalate ", " (map (\f -> f <> ": <value>") fs) <> " }"
            , note $ "Fields of the expected record: " <> intercalate ", " available <> "."
            ]
          )

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
    mkErr "I can't spread this value" context [P t]
      [ note "The spread operator '...' only works on records and on lists in a rest pattern."
      , hint "Check that the value you are spreading is a record, or remove the spread."
      ]

  FatalError ->
    mkErr "Internal compiler error" context
      [P "I hit an internal state I don't know how to handle, and had to stop."]
      [ hint "This is likely a compiler bug — please report it along with the code that triggered it."
      , note "As a workaround, try adding a type annotation or restructuring the expression at this location."
      ]

  Error ->
    mkErr "Internal compiler error" context
      [P "Something went wrong that I can't describe more precisely."]
      [ hint "This is likely a compiler bug — please report it along with the code that triggered it." ]

  ASTHasNoPath ->
    mkErr "Module not found" context
      [P "A required module could not be located or loaded."]
      [ hint "Verify that all imports resolve to existing files."
      , note "If this is a package dependency, run 'madlib install' to fetch it."
      ]
