---
name: Type Checker Refactoring
overview: Refactor the type inference codebase to be simpler, more correct, and closer to THIH (Typing Haskell in Haskell) conventions. The changes range from low-risk cleanup (dead code, deduplication) to high-impact structural rewrites (compose, substitution patterns, function consolidation).
todos:
  - id: cleanup-debug-imports
    content: Remove unused Debug.Trace and Text.Show.Pretty imports from 9 files
    status: completed
  - id: gen-tuple-instances
    content: Replace hand-written tuple instances in initialEnv with programmatic generation
    status: completed
  - id: remove-incomplete-patterns
    content: Remove -Wno-incomplete-patterns pragmas and add proper catch-alls
    status: completed
  - id: consolidate-contextual-unify
    content: Merge the three contextualUnify* functions into one with a strategy param
    status: completed
  - id: consolidate-pattern-update
    content: Merge the three pattern-update traversal functions into one
    status: completed
  - id: standardize-compose
    content: Rewrite compose to match the standard THIH definition
    status: completed
  - id: remove-self-compose-hacks
    content: Simplify all self-composition patterns (s compose s compose s, etc.)
    status: completed
    dependencies:
      - standardize-compose
  - id: simplify-postprocess-body
    content: Merge ambiguity defaulting from postProcessBody into split
    status: completed
    dependencies:
      - standardize-compose
      - remove-self-compose-hacks
  - id: factor-generalize
    content: Extract shared generalize logic from inferImplicitly/ExplicitlyTyped
    status: completed
---

# Type Checker Refactoring Plan

The codebase is based on "Typing Haskell in Haskell" (THIH) by Mark P. Jones but has diverged significantly in several areas, introducing complexity, code duplication, and subtle incorrectness. This plan addresses the most impactful issues in order from safest to most invasive.

---

## Phase 1: Low-Risk Cleanup

### 1A. Remove unused debug imports

9 files import `Debug.Trace` and/or `Text.Show.Pretty` but none of them actually use `trace`/`traceShow`/`ppShow` in live code (only in comments). Files:

- [Exp.hs](compiler/main/Infer/Exp.hs), [Interface.hs](compiler/main/Infer/Interface.hs), [Scope.hs](compiler/main/Infer/Scope.hs), [AST.hs](compiler/main/Infer/AST.hs), [Instantiate.hs](compiler/main/Infer/Instantiate.hs), [Substitute.hs](compiler/main/Infer/Substitute.hs), [Placeholder.hs](compiler/main/Infer/Placeholder.hs), [Test.hs](compiler/main/Infer/Test.hs), [ExhaustivePatterns.hs](compiler/main/Infer/ExhaustivePatterns.hs)

### 1B. Generate `initialEnv` tuple instances programmatically

Currently [EnvUtils.hs](compiler/main/Infer/EnvUtils.hs) lines 191-294 (Show) and 327-425 (Eq) hand-write identical tuple instances for arities 2-10. This is ~200 lines of pure boilerplate. Replace with a helper:

```haskell
mkTupleInstance :: String -> Int -> Instance
mkTupleInstance cls n =
  let tvs = [TVar (TV i Star) | i <- [0..n-1]]
      ps  = [IsIn cls [tv] Nothing | tv <- tvs]
      t   = foldl' TApp (getTupleCtor n) tvs
  in  Instance (ps :=> IsIn cls [t] Nothing) M.empty
```

Then: `[mkTupleInstance "Show" n | n <- [2..10]] `and same for `"Eq"`.

### 1C. Remove the disabled `-Wno-incomplete-patterns` pragmas

Several files suppress incomplete pattern warnings:

- [Exp.hs](compiler/main/Infer/Exp.hs), [Interface.hs](compiler/main/Infer/Interface.hs), [AST.hs](compiler/main/Infer/AST.hs), [Pattern.hs](compiler/main/Infer/Pattern.hs), [Placeholder.hs](compiler/main/Infer/Placeholder.hs), [Scope.hs](compiler/main/Infer/Scope.hs)

These hide real bugs. Remove the pragmas and add proper catch-all patterns or `error "unreachable"` where needed.

---

## Phase 2: Deduplicate Functions

### 2A. Consolidate the three `contextualUnify*` functions

[Unify.hs](compiler/main/Infer/Unify.hs) has three nearly identical functions (lines 179-239):

- `contextualUnifyAccess` - used for field access
- `contextualUnify` - standard unify with error context
- `contextualUnify'` - like above but with `discardError` fallback

These differ only in error handling. Consolidate into a single function with an error strategy parameter:

```haskell
data UnifyStrategy = Strict | Discard | AccessStyle

contextualUnify :: UnifyStrategy -> Env -> Can.Canonical a -> Type -> Type -> Infer Substitution
```

This eliminates ~50 lines of near-identical code.

### 2B. Consolidate the three pattern-update functions

Three nearly identical pattern traversals exist:

1. `updatePatternTypes` in [Exp.hs](compiler/main/Infer/Exp.hs) (lines 769-795) - takes a `Vars` map
2. `updateTypes` in [Pattern.hs](compiler/main/Infer/Pattern.hs) (lines 156-177) - simple substitution apply
3. `updatePattern` in [Placeholder.hs](compiler/main/Infer/Placeholder.hs) (lines 193-219) - adds pred selection

Consolidate into a single `applyToPattern :: Substitution -> Maybe Vars -> Maybe [Pred] -> Slv.Pattern -> Slv.Pattern` in a shared location (e.g., Pattern.hs), then call it from the other sites.

---

## Phase 3: Structural Improvements

### 3A. Standardize `compose` to match THIH

This is the most impactful change. The current `compose` in [Substitute.hs](compiler/main/Infer/Substitute.hs) (line 127-148):

```haskell
compose s1 s2 = M.map (apply s1) $ M.unionsWith mergeTypes [s2, apply s1 <$> s1]
```

Diverges from THIH in two ways:

1. **Self-application**: `apply s1 <$> s1` applies s1 to its own values. In standard HM, substitutions produced by `unify` are already idempotent, so this is unnecessary for non-record types.
2. **`mergeTypes`**: A custom merge function that combines Records with TVars during union conflicts. This is needed for extensible records but should be handled in `apply`/`unify`, not in `compose`.

**Replace with the standard definition:**

```haskell
compose s1 s2 = M.map (apply s1) s2 `M.union` s1
```

The record-merging behavior is already handled by `apply` for `TRecord` (lines 51-84 of Substitute.hs), which looks up base TVars and merges record fields. The `unify` function for records should produce substitutions that already encode the full record structure.

### 3B. Remove self-composition hacks (consequence of 3A)

Once `compose` is standard, the following patterns become regular:

| Location | Current (hacky) | Standard |

|---|---|---|

| [Pattern.hs:141](compiler/main/Infer/Pattern.hs) | `s `compose` s `compose` s `| `s` |

| [Exp.hs:239](compiler/main/Infer/Exp.hs) | `s `compose` (sb `compose` s) `| `sb `compose` s` |

| [Exp.hs:992](compiler/main/Infer/Exp.hs) | `s `compose` s' `compose` s `| `s' `compose` s` |

| [Exp.hs:1064-1065](compiler/main/Infer/Exp.hs) | `apply (s `compose` s) t `/ `s `compose` s'' `compose` s'' `| `apply s t` / `s'' `compose` s` |

These all reduce to the standard THIH pattern of chaining substitutions left-to-right.

### 3C. Simplify `inferBody` and `postProcessBody`

The `postProcessBody` function ([Exp.hs](compiler/main/Infer/Exp.hs) lines 244-308) is a second pass over an already-inferred body to resolve ambiguities. The TODO comment says *"find out and comment why we need this"*. In THIH, ambiguity resolution happens during generalization (`split`), not as a post-hoc pass.

The plan:

1. Move the ambiguity defaulting logic from `postProcessBody` into `split` (which already has ambiguity detection at line 856)
2. Have `postProcessBody` only do what it truly needs: apply the final substitution to update solved expressions
3. This eliminates the confusing double-defaulting (once in `postProcessBody`, once in `split`)

### 3D. Factor shared logic out of `inferImplicitlyTyped` / `inferExplicitlyTyped`

These two functions ([Exp.hs](compiler/main/Infer/Exp.hs) lines 971-1126) follow the same pattern:

1. Set up env with fresh type variable or annotation type
2. Run `infer`
3. Unify inferred type with expected type
4. Compute free/generic variables
5. Call `split` for pred partitioning
6. Handle mutation predicates
7. Generalize and return

Extract a shared `generalize` function that takes the inferred result and does steps 4-7, called by both functions.

---

## Phase 4: Correctness Fixes (from the previous audit, still applicable)

### 4A. Ensure `apply` covers optional fields consistently in TRecord

In [Substitute.hs](compiler/main/Infer/Substitute.hs), the `Nothing` case for base (line 65-66) and the catch-all (line 71-72) now correctly apply substitutions to `optionalFields`. This was addressed in the prior fix and should be preserved.

### 4B. Ensure `addInstanceMethod` handles `Nothing` case

In [Interface.hs](compiler/main/Infer/Interface.hs) line 79, the `Nothing` case was added to avoid a runtime crash. This should be preserved.

---

## Summary of Impact

| Phase | Lines removed (est.) | Lines added (est.) | Risk |

|---|---|---|---|

| 1: Cleanup | ~220 | ~30 | Very low |

| 2: Dedup | ~100 | ~40 | Low |

| 3: Structural | ~120 | ~60 | Medium-High |

| 4: Fixes | 0 | 0 | Already done |

**Total net reduction: ~310 lines**, with significantly improved readability, correctness, and alignment with THIH.

## Execution Order

Phase 3A (standardize compose) must come before 3B (remove hacks). Phase 3C should come after 3A/3B because the simplified substitution flow may change what `postProcessBody` needs to do. All Phase 1 and 2 changes are independent and can be done first.

Recommended order: **1A -> 1B -> 1C -> 2A -> 2B -> 3A -> 3B -> 3C -> 3D**

Each step should be compiled and tested (run the compiler's test suite) before proceeding to the next.