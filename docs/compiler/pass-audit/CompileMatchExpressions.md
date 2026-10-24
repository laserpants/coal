# CompileMatchExpressions

## Purpose

Compile high-level match expressions into optimized decision trees for efficient pattern matching.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/CompileMatchExpressions.hs
```

---

## Summary

Transforms `EMatch` expressions into `ECompiledMatch` nodes containing decision trees.
Uses the pattern matching compiler (`CompileEnvelope`, `matchPatterns`) to generate
efficient branching that minimizes redundant constructor tests. Patterns are first
translated into an internal envelope representation, then compiled.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Integer literal patterns expanded, all patterns in simple
  form (constructor patterns with variable sub-patterns)

---

## Output

- Same type with `EMatch` replaced by `ECompiledMatch`

---

## Detailed Behavior

### `passImpl`

Before doing any work, scans the module for a match expression with a pure, lazy
check (`universeBi`, stopping at the first `EMatch`). A module with no match
expressions is returned unchanged and the monadic whole-module traversal is
skipped entirely.

This is not only a fast path. `transformBiM` sequences one monadic action per AST
node, so it builds an `O(nodes)` chain of `RWST` bind closures; that chain is not
forced until the traversal completes, so it stays live for the whole traversal and
dominated the compiler's live heap on large multi-module builds. Skipping the
traversal where it would be the identity removes the chain.

### `compileMatchExprsE`

For `EMatch _ _ e cs`:
1. Generates a fresh name `match.N`
2. Calls `compileClauses` to produce the compiled decision tree
3. Uses `replaceWith` to substitute the scrutinee with the fresh variable

### `compileClauses`

1. Translates each clause into a `([EnvelopePattern], EnvelopeExpression)` pair
2. Passes to `matchPatterns [ll] eqs MFail` which generates the decision tree
3. Calls `compileEnvelope` to convert the internal representation back to
   `Expression`

### `translatePattern`

Converts surface patterns to envelope patterns:
- `PVariable` → `MVariable`
- `PConstructor` → `MConstructor` (recursively)
- `PLiteral` → `MLiteral`
- `PAny` → `MVariable` with `_`
- `PListCons` → `PConstructor "$Cons"` (desugared)
- `PListLiteral` → nested `$Cons`/`$Nil` constructors
- `PTuple` → `PConstructor "$TupleN"`

### `translateListLiteral`

Converts `[p1, p2, p3]` to `$Cons(p1, $Cons(p2, $Cons(p3, $Nil)))`.

---

## Transformation Rules

```coal
match(value) {
  | Some(x) => x + 1
  | None => 0
}
```
becomes an `ECompiledMatch` node containing a decision tree that efficiently
tests the constructor tag of `value` and branches accordingly.

---

## Analysis

- **Algorithm**: Decision tree compilation via `matchPatterns` from
  `Coal.Compiler.PatternMatching.Rule`
- **Fresh-name generation**: `match.N` for each compiled match
- **Pattern translation**: Desugars list and tuple patterns into constructor patterns

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandIntegerLiteralPatterns
- **Later passes that rely on this pass**: InsertDictionaries (works on
  `ECompiledMatch` nodes)

---

## Important Data Structures

- `EnvelopePattern` — internal pattern representation (`MVariable`, `MConstructor`, `MLiteral`)
- `EnvelopeExpression` — internal expression representation (`MExpression`, `MFail`)

---

## Side Effects

- **Creates fresh names**: `match.N` via supply monad
- **Generates diagnostics**: No

## Notes

- The pass is the identity on modules without match expressions; the `passImpl`
  short-circuit above is what keeps it from costing memory on every module.
- The short-circuit triggers on `EMatch` only, matching `compileMatchExprsE`, which
  rewrites `EMatch` and returns every other expression unchanged.