# DesugarDoNotation

## Purpose

Desugar do-notation syntax into explicit monadic bind (`bind`) and `pure` operations.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DesugarDoNotation.hs
```

---

## Summary

Transforms `do { x <- action1(); y <- action2(x); pure(x + y) }` into
`action1() >>= fn(x) => action2(x) >>= fn(y) => pure(x + y)`. Implements a
`DoNotationContext` typeclass that recursively traverses the AST, transforming
`EDoBlock` nodes.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Builtins inserted

---

## Output

- **Resulting AST**: Same type with all `EDoBlock` nodes eliminated
- **Established invariants**: No do-notation remains in any expression
- **Guarantees made to later passes**: They never need to handle `EDoBlock`

---

## Detailed Behavior

### `normalize`

Splits the last element off the do-block's bindings list. If the last binding
pattern is a wildcard `_`, the final expression is used directly. Otherwise,
`pure(...)` wrapping is inserted.

### `desugarDoNotation` (Expression instance)

For `EDoBlock`:
1. Calls `normalize` to separate the final expression from preceding bindings
2. Folds from the right: each `(pattern, expr)` pair becomes
   `bind(expr, fn(pattern) => rest)`
3. Uses `varE "bind"` and `varE "pure"` — the actual resolution of these names
   to trait methods happens during trait dictionary insertion

For all other expressions, recursively descends via `descendM`.

---

## Transformation Rules

Example:
```
do {
  x <- action1();
  y <- action2(x);
  pure(x + y)
}
```
becomes:
```
bind(action1(), fn(x) => bind(action2(x), fn(y) => pure(x + y)))
```

---

## Analysis

- **Tree traversals**: Bottom-up via `descendM` (uniplate generic traversal)
- **Fresh-name generation**: No
- **Recursion**: The typeclass instances recursively traverse modules → definitions
  → function bodies → expressions

---

## Compiler Interactions

- **Earlier passes this relies on**: InsertBuiltinDefinitions
- **Later passes that rely on this pass**: DetectAliasCycles, DetectShadowing
  (must run after do-notation is flattened)

---

## Side Effects

- **Generates diagnostics**: No
- **Modifies compiler state**: No

---

## Notes

The pass uses `varE "bind"` and `varE "pure"` directly. These are resolved
to trait method instances later in `InsertDictionaries`. The pass uses
`uniplate` generics (`descendM`) for traversal, which means it relies on
the `Data` instances of the AST types.