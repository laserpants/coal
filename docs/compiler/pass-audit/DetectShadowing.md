# DetectShadowing

## Purpose

Detect variable shadowing where a binding hides an existing name in an outer scope.
Coal treats shadowing as an error to prevent confusion and bugs.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DetectShadowing.hs
```

---

## Summary

Maintains a `Set Name` of currently bound variables as it traverses the AST.
When a new binding is encountered, if the name (excluding constructors) is
already in the set, a `Shadowing` error is reported.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Do-notation desugared, alias cycles checked

---

## Output

- **Resulting AST**: Same as input (no transformation)
- **Established invariants**: No variable name is shadowed

---

## Detailed Behavior

### `detectShadowing` (Expression instance)

For binding constructs:
- `ELambda`: adds bound pattern variables, checks for shadowing, then recurses
  into the body with extended set
- `ELet`: checks bindings with current names, then body with extended names
- `ERecursiveLet`: both sides use extended names
- Other expressions: recursively descends with `names` unchanged

### `addNames`

Takes a set of new names, filters out constructors (using `isConstructor`), and
for each remaining name, checks if it's already in the current set. If so,
reports `Shadowing`. Returns the union of curated new names and existing names.

---

## Analysis

- **Tree traversals**: Manual recursion through all expression constructors via
  the `ShadowingContext` typeclass
- **Environment handling**: A `Set Name` threaded through traversal
- **Constructor filtering**: Constructors are excluded from shadowing checks
  via `isConstructor`

---

## Compiler Interactions

- **Earlier passes this relies on**: DetectAliasCycles
- **Later passes that rely on this pass**: DetectDuplicateParams

---

## Side Effects

- **Generates diagnostics**: `Shadowing`
- **Modifies compiler state**: No