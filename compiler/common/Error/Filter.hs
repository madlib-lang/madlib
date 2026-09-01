-- | Suppresses cascading/duplicate compilation errors before they reach the
-- user. A single broken binding can produce several errors as inference
-- retries and downstream expressions see its broken type; this module keeps
-- the errors that explain the problem and drops the ones that are just
-- noise from the same root cause.
module Error.Filter (filterErrors) where

import           Data.List                       ( sortOn )
import qualified Data.List                       as List
import           Error.Context
import           Error.Error
import           Explain.Location


-- | A structural fingerprint of a TypeError's constructor, ignoring its
-- payload. Two errors of different shapes at the same span are never
-- considered duplicates of each other, even if one happens to subsume the
-- other's location.
errorKind :: TypeError -> Int
errorKind e = case e of
  InfiniteType{}                       -> 0
  UnboundVariable{}                    -> 1
  UnboundUnknownTypeVariable{}         -> 2
  UnboundVariableFromNamespace{}       -> 3
  UnboundType{}                        -> 4
  UnificationError{}                   -> 5
  BadEscapeSequence{}                  -> 6
  EmptyChar{}                          -> 7
  TypeAlreadyDefined{}                 -> 8
  ImportCollision{}                    -> 9
  InstancePredicateError{}             -> 10
  KindError{}                          -> 11
  NoInstanceFound{}                    -> 12
  InterfaceAlreadyDefined{}            -> 13
  InterfaceNotExisting{}               -> 14
  OverlappingInstances{}               -> 15
  SelfReferentialInstance{}            -> 16
  AmbiguousType{}                      -> 17
  ADTAlreadyDefined{}                  -> 18
  CapitalizedADTTVar{}                 -> 19
  NotCapitalizedADTName{}              -> 20
  NotCapitalizedAliasName{}            -> 21
  NotCapitalizedConstructorName{}      -> 22
  TypingHasWrongKind{}                 -> 23
  WrongAliasArgCount{}                 -> 24
  WrongInterfaceArgCount{}             -> 71
  InstanceResolutionCycle{}            -> 72
  SuperclassCycle{}                    -> 73
  InvalidInstanceContext{}             -> 74
  UnknownType{}                        -> 25
  WrongSpreadType{}                    -> 26
  ImportNotFound{}                     -> 27
  NotExported{}                        -> 28
  GrammarError{}                       -> 29
  NameAlreadyDefined{}                 -> 30
  TypesHaveDifferentOrigin{}           -> 31
  RecursiveVarAccess{}                 -> 32
  IllegalSkipAccess{}                  -> 33
  NotInScope{}                         -> 34
  SignatureTooGeneral{}                -> 35
  NameAlreadyExported{}                -> 36
  ShouldBeTypedOrAbove{}               -> 37
  ContextTooWeak{}                     -> 38
  DerivingAliasNotAllowed{}            -> 39
  InvalidInterfaceDerived{}            -> 40
  FatalError{}                         -> 41
  ASTHasNoPath{}                       -> 42
  Error{}                              -> 43
  ImportCycle{}                        -> 44
  NoMain{}                             -> 45
  MainInvalidTyping{}                  -> 46
  NotADefinition{}                     -> 47
  ConstructorAccessBadIndex{}          -> 48
  ConstructorAccessNoConstructorFound{}-> 49
  ConstructorAccessTooManyConstructors{} -> 50
  MutationRestriction{}                -> 51
  OverloadedMutation{}                 -> 52
  BadMutation{}                        -> 53
  MutatingNotInScope{}                 -> 54
  MutatingPatternBoundVariable{}       -> 55
  InvalidLhs{}                         -> 56
  RefutablePatternInParameter{}        -> 57
  TypeAnnotationNameMismatch{}         -> 58
  MutatingFunction{}                   -> 59
  MethodNameAlreadyDefined{}           -> 60
  NotAConstructor{}                    -> 61
  RecordDuplicateFields{}              -> 62
  RecordDuplicateRestPattern{}         -> 63
  RecordMissingFields{}                -> 64
  RecordExtraFields{}                  -> 65
  TestNotValid{}                       -> 66
  ByteOutOfBounds{}                    -> 67
  ShortOutOfBounds{}                   -> 68
  IntOutOfBounds{}                     -> 69
  NegatedByte{}                        -> 70


-- | True when the span of `outer` strictly contains the span of `inner`
-- (same file, and inner's range sits within outer's without being equal).
contains :: Context -> Context -> Bool
contains (Context pathOuter (Area (Loc _ lOuterStart cOuterStart) (Loc _ lOuterEnd cOuterEnd)))
         (Context pathInner (Area (Loc _ lInnerStart cInnerStart) (Loc _ lInnerEnd cInnerEnd))) =
  pathOuter == pathInner
    && (lOuterStart, cOuterStart) <= (lInnerStart, cInnerStart)
    && (lInnerEnd, cInnerEnd) <= (lOuterEnd, cOuterEnd)
    && (lOuterStart, cOuterStart, lOuterEnd, cOuterEnd) /= (lInnerStart, cInnerStart, lInnerEnd, cInnerEnd)
contains _ _ = False


sortKey :: CompilationError -> (FilePath, Int, Int)
sortKey err = case getContext err of
  Context path (Area (Loc _ l c) _) -> (path, l, c)
  NoContext                         -> ("", 0, 0)


-- | Removes cascading and duplicate errors, keeping a stable, file-ordered
-- result. Composes three passes:
--
--   1. Exact duplicates (same error, same location) are dropped.
--   2. Same-location, same-kind errors are collapsed to the first one: two
--      'UnificationError's at one span with different type payloads are the
--      strict pass's error and a best-effort retry's error over the same
--      root cause, not two independent problems.
--   3. An error of the same kind whose span is strictly contained by
--      another same-kind error's span is dropped as the inner, re-thrown
--      duplicate of the outer one.
filterErrors :: [CompilationError] -> [CompilationError]
filterErrors =
  sortOn sortKey . dropSubsumed . dropSameSpanSameKind . List.nub
 where
  dropSameSpanSameKind errs =
    let keyOf e = (getContext e, errorKind (typeErrorOf e))
    in  nubOnFirst keyOf errs

  dropSubsumed errs =
    [ e
    | e <- errs
    , not $ any
        (\other ->
          errorKind (typeErrorOf e) == errorKind (typeErrorOf other)
            && getContext other `contains` getContext e
        )
        errs
    ]

  nubOnFirst key = go []
    where
      go _    []       = []
      go seen (x : xs)
        | key x `elem` seen = go seen xs
        | otherwise         = x : go (key x : seen) xs

  typeErrorOf (CompilationError e _) = e
