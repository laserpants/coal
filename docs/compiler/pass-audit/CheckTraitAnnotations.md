# CheckTraitAnnotations

## Purpose

Verify that user-written trait annotations cover all the trait constraints inferred by the type checker.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/CheckTraitAnnotations.hs
```

---

## Summary

When a function definition has an explicit type annotation (return type or pattern
type annotations) or explicit trait constraints, this pass unifies the annotated type
with the inferred type from the name store, then checks that all inferred traits
are covered by the annotated traits. If any inferred traits reference type variables
from the user's annotation, they must be listed in the annotation's `with` clause.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Type inference completed (name store populated with inferred schemes)

---

## Output

- Same type (no transformation unless errors cause abort)

---

## Detailed Behavior

### Triggering condition

Only checks definitions where the user wrote explicit annotations. A definition is considered explicitly annotated if:
- It has an explicit return type annotation (`isExplicitReturnType`)
- Any pattern has an explicit type annotation (`hasExplicitPatternAnnotations`)
- It has explicit trait constraints (`not (null functionDefinitionConstraints)`)

### `checkTraitCoverage`

1. Instantiates the inferred scheme to get the inferred qualified type
2. Unifies the annotated type with the inferred type via `liftUnifier (unify annQualifiedType inferredType)`
3. Applies the resulting substitution to normalize both types
4. Filters inferred traits to only those referencing the user's explicitly mentioned type variables
5. Checks that every filtered inferred trait has a matching annotated trait
6. Reports `MissingTraitAnnotation` for any uncovered inferred traits

Reported traits are displayed using the names of the user-written type parameters:
`renameMapFromSubstitution` maps substituted (inferred) variable indices back to
the names of the annotated parameters they were unified with, and
`renameTraitWithNames` rewrites the reported traits accordingly. Variables with no
corresponding parameter fall back to their internal rendering.

### Trait matching

Uses structural equality (`typesStructurallyEqual`) which treats type variables as equal regardless of their identity. Two traits are considered matching if:
- They have the same name
- Their type arguments are structurally equal (same shape, variables treated as matching)

### Collecting explicit type variables

- `collectTypeVariables`: collects `TVariable` parameter names from user-written `Type Parameter Kind` annotations
- `collectPatternTypeVariables`: walks pattern annotations to collect type variable names
- `collectTraitTypeVariables` / `collectConstraintTypeVariables`: collect from trait annotations
- All parameter indices are normalized by the substitution to map them to indexed type indices

---

## Compiler Interactions

- **Earlier passes this relies on**: DenormalizeAST, TypeInference (name store)
- **Later passes that rely on this pass**: PhaseLowering (final pass in translation)

---

## Side Effects

- **Generates diagnostics**: `MissingTraitAnnotation` for uncovered inferred traits
- **Modifies compiler state**: No
- **Performs IO**: No