---
name: Type Inference Bug Audit
overview: Audit of the `compiler/main/Infer/` directory reveals several bugs ranging from substitution application gaps to dead code and missing substitution threading in record unification.
todos:
  - id: fix-substitute-apply
    content: Fix missing apply s to optionalFields and fields in Substitute.hs TRecord cases
    status: completed
  - id: fix-unify-threading
    content: Thread s1 through subsequent unification steps in Unify.hs record cases
    status: completed
  - id: fix-dead-return
    content: Remove dead/duplicate return statements in Exp.hs postProcessBody
    status: completed
  - id: fix-self-compose
    content: Fix self-composition of substitutions in Pattern.hs and Exp.hs
    status: completed
  - id: fix-incomplete-pattern
    content: Add Nothing case in Interface.hs addInstanceMethod
    status: completed
  - id: fix-inferbody-compose
    content: Review and simplify inferBody substitution composition in Exp.hs
    status: completed
  - id: fix-tv-numbering
    content: Fix tuple instance TVar numbering to not skip index 2 in EnvUtils.hs
    status: completed
---

# Type Inference Bug Audit

After thorough review of all files in `compiler/main/Infer/`, here are the bugs found, ordered from most impactful to least:

---

## Bug 1 (High): Missing substitution application in `apply` for TRecord -- [Substitute.hs](compiler/main/Infer/Substitute.hs)

In the `Substitutable Type` instance (lines 51-72), when the base is a `TVar tv` and that TVar is **not** found in the substitution (`Nothing` case), optional fields are silently left un-substituted:

```65:66:compiler/main/Infer/Substitute.hs
    Nothing ->
      TRecord (apply s <$> fields) (Just (TVar tv)) optionalFields
```

Compare with the `Just newBase@(TVar _)` case just above where optional fields ARE substituted:

```52:53:compiler/main/Infer/Substitute.hs
    Just newBase@(TVar _) ->
      TRecord (apply s <$> fields) (Just newBase) (apply s <$> optionalFields)
```

Similarly, the catch-all case at lines 71-72 fails to substitute **both** fields and optional fields:

```71:72:compiler/main/Infer/Substitute.hs
    _ ->
      TRecord fields (Just (TVar tv)) optionalFields
```

**Impact:** Type variables inside optional fields (and fields in the catch-all) will not be resolved by substitutions, potentially leaving stale type variables in record types. This could cause unsound typing or spurious ambiguity errors when extensible records with optional fields are involved.

**Fix:** Apply `s` to `optionalFields` on line 66, and apply `s` to both `fields` and `optionalFields` on line 72.

---

## Bug 2 (High): Record unification doesn't thread substitutions through stages -- [Unify.hs](compiler/main/Infer/Unify.hs)

In the `(Just tBase, Just tBase')` case of record unification (lines 44-58):

```54:58:compiler/main/Infer/Unify.hs
      s1 <- unifyVars' M.empty (M.elems fieldsToCheck) (M.elems fieldsToCheck')
      s2 <- unify (TRecord fieldsForLeft (Just newBase) mempty) tBase'
      s3 <- unify (TRecord fieldsForRight (Just newBase) mempty) tBase

      return $ s1 `compose` s2 `compose` s3
```

`s1` (from unifying common fields) is never applied before computing `s2` and `s3`. If common fields share type variables with `fieldsForLeft`, `fieldsForRight`, `tBase`, or `tBase'`, the constraints discovered in `s1` are not propagated.

**Example scenario:** Two open records `{ x: a, y: a | b }` and `{ x: Int, z: Bool | c }`. Unifying common field `x` gives `s1 = {a -> Int}`, but `fieldsForLeft = {y: a}` still uses `a` instead of `Int` in the `s2` unification.

The same issue exists in the `(Just tBase, Nothing)` and `(Nothing, Just tBase')` cases at lines 60-84 where `s1` from unifying the base is not applied before unifying common fields in `s2`.

**Fix:** Apply `s1` to the types used in subsequent unification steps:

```haskell
s2 <- unify (apply s1 $ TRecord fieldsForLeft (Just newBase) mempty) (apply s1 tBase')
s3 <- unify (apply s1 $ TRecord fieldsForRight (Just newBase) mempty) (apply s1 tBase)
```

---

## Bug 3 (Medium): Dead/duplicate return in `postProcessBody` -- [Exp.hs](compiler/main/Infer/Exp.hs)

Lines 269-297 contain an inner `if-then-else` whose return value is always discarded because a duplicate `return` on line 297 follows it:

```269:297:compiler/main/Infer/Exp.hs
          if unsolvedPs'' /= [] then do
            forM_ unsolvedPs'' $ \p -> do
              catchError
                (byInst env (apply subst p))
                -- ... error handling ...
            return (unsolvedPs'' ++ solvedPs, subst)
          else
            return (unsolvedPs'' ++ solvedPs, subst)
          return (unsolvedPs'' ++ solvedPs, subst)
```

Both branches AND the final `return` all produce `(unsolvedPs'' ++ solvedPs, subst)`. The if-then-else runs only for side effects (the `forM_`/error throwing), but the `else` branch has no side effects at all, making it truly dead code. The `return` on line 294 is also dead since line 297 supersedes it.

**Impact:** Currently benign since all three return the same value. However, this strongly suggests a past intent where different branches should return different results (e.g., the `else` branch might have been intended to return `(solvedPs, subst)` without `unsolvedPs''`). One of the three `return` statements should be removed, and the logic should be reviewed for correctness.

---

## Bug 4 (Medium): Self-composition of substitutions -- multiple files

Several locations compose a substitution with itself, which is an unusual pattern:

**[Pattern.hs](compiler/main/Infer/Pattern.hs) line 141** -- in `PCon` pattern inference:

```141:compiler/main/Infer/Pattern.hs
    let s' = s `compose` s `compose` s
```

Triple self-composition. For standard idempotent substitutions this is a no-op. However, the custom `compose`/`mergeTypes` in this codebase has special record-merging behavior that could make `s `compose` s `differ from `s` when records are involved. This looks like a mistake; it should likely just be `s`.

**[Exp.hs](compiler/main/Infer/Exp.hs) line 993** -- in `inferImplicitlyTyped`:

```993:compiler/main/Infer/Exp.hs
  let s'' = s `compose` s' `compose` s
```

Here `s` appears twice. The standard approach would be `s' `compose` s`.

**[Exp.hs](compiler/main/Infer/Exp.hs) lines 1065-1066** -- in `inferExplicitlyTyped`:

```1065:1066:compiler/main/Infer/Exp.hs
  s'' <- catchError (contextualUnify' env discardError canExp t' (apply (s `compose` s) t)) (throwError . limitContextArea 2)
  let s' = s `compose` s'' `compose` s''
```

Both `s `compose` s `and `s'' `compose` s''` are self-compositions.

**Impact:** With normal substitutions these are no-ops, but with the custom record-merging `compose` they could produce unexpected results, potentially creating malformed record types. These seem to be accidental from incremental debugging/fixing.

---

## Bug 5 (Medium): Incomplete pattern match in `addInstanceMethod` -- [Interface.hs](compiler/main/Infer/Interface.hs)

Lines 70-78 are missing the `Nothing` case for `findInst`:

```70:78:compiler/main/Infer/Interface.hs
addInstanceMethod env _ p@(IsIn cls _ _) (methodName, methodScheme) = do
  (Interface tvs ps' is) <- lookupInterface env cls
  maybeInstance <- findInst env p
  case maybeInstance of
    Just (Instance qp methods) -> do
      let methods'    = M.insert methodName methodScheme methods
      return env { envInterfaces = M.insert cls (Interface tvs ps' (Instance qp methods' : is)) (envInterfaces env) }
```

If `findInst` returns `Nothing`, this will be a runtime crash (non-exhaustive pattern). The `-Wno-incomplete-patterns` pragma hides this. Note: `addInstanceMethod` appears to be unused (dead code), so this is currently harmless but could become a crash if the function is used in the future.

Additionally, even in the `Just` case, the updated instance is prepended to `is` which still contains the old instance, leading to duplication.

---

## Bug 6 (Low): `inferBody` substitution composition -- [Exp.hs](compiler/main/Infer/Exp.hs)

```239:compiler/main/Infer/Exp.hs
  let finalS = s `compose` (sb `compose` s)
```

In standard HM inference for sequential expressions, the composition should be `sb `compose` s `(apply the first statement's substitution, then the body's). The extra outer `compose s` applies `s` a second time to the already-composed result, which is redundant for normal substitutions but could have unintended effects with the custom record-merging compose.

---

## Bug 7 (Low): Tuple instance TVars skip index 2 -- [EnvUtils.hs](compiler/main/Infer/EnvUtils.hs)

All tuple instances (for `Eq`, `Show`) from 3-tuples onwards use type variable numbering `TV 0, TV 1, TV 3, TV 4, ...`, skipping `TV 2`. This is consistent throughout and instance resolution works via unification (so actual numbers don't matter as long as they're unique), so this is cosmetically wrong but functionally harmless. Example at line 206:

```204:209:compiler/main/Infer/EnvUtils.hs
                      ] :=> IsIn "Show" [
                        TApp (TApp (TApp tTuple3 (TVar (TV 0 Star))) (TVar (TV 1 Star))) (TVar (TV 3 Star))
                      ] Nothing
```

---

## Summary Table

| Bug | File | Severity | Category |

|-----|------|----------|----------|

| 1 | Substitute.hs:65-72 | High | Missing substitution on optional fields |

| 2 | Unify.hs:54-58 | High | Substitution not threaded in record unification |

| 3 | Exp.hs:294-297 | Medium | Dead/duplicate returns |

| 4 | Pattern.hs:141, Exp.hs:993,1065-1066 | Medium | Self-composition of substitutions |

| 5 | Interface.hs:70-78 | Medium | Incomplete pattern match (crash) |

| 6 | Exp.hs:239 | Low | Double application of substitution |

| 7 | EnvUtils.hs | Low | TV index skip (cosmetic) |