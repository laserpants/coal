# DesugarDoNotation

## Purpose

Desugar do-notation syntax into explicit monadic bind (`bind`), `pure`, and
`let` operations.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DesugarDoNotation.hs
```

---

## Summary

Transforms `do { x <- action1(); y = action2(x); pure(x + y) }` into
`action1() >>= fn(x) => let y = action2(x) in pure(x + y)`. Implements a
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

Splits the last element off the do-block's statement list. If the last
statement is a bare expression (a `DoBind` whose pattern is a wildcard `_`),
the final expression is used directly. Otherwise, `pure(...)` wrapping is
inserted.

### `desugarDoNotation` (Expression instance)

For `EDoBlock`:
1. Calls `normalize` to separate the final expression from preceding statements
2. Folds from the right, dispatching on the statement kind:
   - `DoBind p e` becomes `bind(e, fn(p) => rest)`
   - `DoLet b` becomes `let b in rest`, i.e. an `ELet` node
3. Uses `varE "bind"` and `varE "pure"` — the actual resolution of these names
   to trait methods happens during trait dictionary insertion

For all other expressions, recursively descends via `descendM`.

---

## Transformation Rules

Example:
```
do {
  a <- action1();
  b = a + 5;
  pure(b)
}
```
becomes:
```
bind(action1(), fn(a) => let b = a + 5 in pure(b))
```

A bare expression statement (`expr;`) is represented in the AST as a bind whose
pattern is a wildcard, so it desugars to `bind(expr, fn(_) => rest)` — the same
treatment it received before let statements were supported.

---

## Analysis

- **Tree traversals**: Bottom-up via `descendM` (uniplate generic traversal)
- **Fresh-name generation**: No
- **Recursion**: The typeclass instances recursively traverse modules → definitions
  → function bodies → expressions

---

## Compiler Interactions

- **Earlier passes this relies on**: InsertBuiltinDefinitions
- **Later passes that rely on this pass**: DetectAliasCycles, DetectShadowing,
  ExpandLetBindings (must run after do-notation is flattened)

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

A `DoLet` desugars into the same `ELet` node the surface `let ... in` syntax
produces, so it inherits that construct's semantics unchanged — in particular
the binding is *not* recursive, and a complex left-hand-side pattern or an
`f(pat, ...) = expr` binding is handled by the later `DesugarPatterns` pass.