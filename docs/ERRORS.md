# Writing error messages

Madlib's compiler errors are built once as a `Diagnostic` (`compiler/main/Explain/Diagnostic.hs`)
in `compiler/main/Explain/Diagnostic/Build.hs`, then projected to terminal, JSON and LSP text by
the renderers under `compiler/main/Explain/Render/`. Any new `TypeError`/`WarningKind` case is
added once, in `Build.hs`, and must follow these rules.

## The ten rules

1. **Never print the same string on both sides of a mismatch.** If two types render identically,
   describe the structural difference (see `firstDifference` in `TypeDiff.hs`) or drop type names
   from the sentence entirely. This is what caused the original "Function is not Function" bug —
   guard against regressing it (see the property test in `FormatSpec.hs`,
   "never renders the same name on both sides of a mismatch").
2. **Titles name the construct and the problem; bodies show the types.** A title must make sense
   read on its own, without the body. A full type appears in a title only if it renders on one
   line and differs from the other side (`inlineOrDescribe` in `Hints.hs`).
3. **Say each thing once.** The title, the primary marker label, the secondary marker label, each
   note and each hint must carry distinct information. Never restate the title as a hint.
4. **Always anchor to source.** Every error should have a primary marker. When two things
   disagree (two if-branches, an annotation vs. its implementation, a mutation vs. a declaration),
   attach a secondary marker at the other location via `SecondaryLocation` /
   `tmSecondaries`.
5. **Label the two sides with meaning, not `expected:`/`found:`,** when a better phrase exists —
   "the annotation says ... but the implementation produces ...", "this branch is ... but the
   other branch is ...". Fall back to the plain `expected:`/`found:` block only when there's no
   more specific relationship to name (see `ExpectedFound` usage for `NoOrigin`).
6. **Hints are imperative and end in a concrete, typeable fragment** (`Just(x)`, `x != 0`,
   `import List from "List"`). Facts belong in Notes; instructions belong in Hints — don't mix the
   two in one line.
7. **Suppress constant expectations.** Don't print `expected: Boolean` for an `if`/`while`
   condition — the prose already says it. Only show a type block when it carries information the
   reader doesn't already have from the sentence around it.
8. **Full types, never silently truncated.** No transformation may replace a sub-part of a type
   with the whole, or vice versa, without saying so in the message. (This is why
   `getParamTypeOrSame`'s narrowing was deleted from `contextualUnify` — it silently swapped the
   reported pair for a fragment.)
9. **Blame the most-recently-written thing** when the blame is otherwise arbitrary — the *n*th
   list element that doesn't match, the *n*th `where` branch that disagrees — not always the
   first one.
10. **First person, no unglossed jargon.** Write "I can't infer a concrete type here", not "An
    ambiguity could not be resolved". Never use "unification", "predicate", "LHS", "refutable",
    etc. without an inline explanation — say what the reader should do instead.

## Building a `UnificationError`

`UnificationError` carries a `TypeMismatch { tmFound, tmExpected, tmOrigin, tmSecondaries }`
(`compiler/common/Error/Error.hs`). `tmOrigin :: ErrorOrigin` says *why* two types were compared
(function argument, if-branches, type annotation, ...) and drives the title, the primary marker's
wording, and which hints/notes apply — see the `ErrorOrigin` case in
`Explain.Diagnostic.Build.errorDiagnostic`.

Before adding a new origin-specific message, check `Explain.Format.Hints.detectSpecialCase`: it
classifies common mismatches (`Maybe` wrapping, `String`/`Char` confusion, missing function
application, `List` vs. element, numeric cross-talk, arity) independently of origin, and its hint
takes priority. Add a new `SpecialCase` there instead of duplicating the check per origin.

## Adding a "did you mean" suggestion

The edit-distance engine (`Utils.EditDistance.findSimilar`) is generic — it takes the misspelled
name and a list of candidates. Adding a suggestion to a new error is a matter of finding the right
candidate list at the throw site (in scope names, a namespace's exports via
`Infer.EnvUtils.namespaceExportNames`, a record's field names, ...) and threading it through the
error's payload, the way `UnboundVariable`, `UnboundType`, `RecordExtraFields`,
`UnboundVariableFromNamespace` and `RecordMissingFields` already do.

## Verifying a new or changed message

1. Add a case to `compiler/test/Explain/GoldenSpec.hs`'s `goldenCases` (or a new blackbox fixture
   under `compiler/test/Blackbox/test-cases/`) that reaches the message.
2. Run `UPDATE_GOLDEN=1 stack test --ta '--match "Error rendering goldens"'` to generate the
   golden files, then read them — the golden diff *is* the review.
3. Run the full suite (`stack test`) and skim any other golden/snapshot diffs it produces; approve
   only the ones that are the intended wording change.
4. If the message can be triggered by a small program, compile it with the freshly built `madlib`
   (after `./scripts/build-runtime` and `./scripts/update-pkg-build`) and read the real terminal
   output — golden files can hide layout issues that only show up in a real 80-column terminal.
