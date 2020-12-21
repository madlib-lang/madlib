# Type classes

## Type changes

TVar needs to have an extra `[ClassConstraint]` list.

## Unification

There are two operations with ClassContraints:
 - Augmentation
 - Reduction

Augmentation is about doing a union of constraints when we unify a type if the other type is also a TVar.

Reduction is mainly about factoring Constraints.

