# ExpandExpressionFolds

## Purpose

Expand fold expressions embedded within other expressions into explicit recursive let bindings with lambda expressions and pattern matching.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/ExpandExpressionFolds.hs
```

---

## Summary

Identifies `EFold` nodes within expressions and transforms them into let-bound recursive functions. Validates that `@`-patterns (fold patterns) are only used within fold contexts, not in regular match expressions. Distinct from `ExpandTopLevelFolds` which handles top-level `DFold` definitions.

---

## Input

- **AST representation**: `Module Metadata Kind ()`
- **Required invariants**: Top-level folds expanded

---

## Output

- **Resulting AST**: Same type with all `EFold` nodes eliminated
- **Established invariants**: No fold expressions remain

---

## Detailed Behavior

### `compileFolds` (Expression instance)

Uses `transformM` (bottom-up). For `EFold args clauses`:
1. Generates fresh name `fold.N`
2. Expands clauses via `expandFolds` (same mechanism as `ExpandTopLevelFolds`)
3. Returns `let fold.N = fn(fold.N.expr) => match(fold.N.expr) { ... } in fold.N(args)`

For `EMatch`: runs `expandMatch` which calls `checkPatterns` to ensure no
`@`-patterns appear in regular match expressions (reports `FoldPatternInRegularMatch`).

### `checkPatterns`

Recursively checks that no `PAtVariable` patterns appear outside fold contexts.

### `eliminateAtPatterns`

Converts `PAtVariable` to `PVariable` and reports errors for `PNamedFold` in expression folds (`NamedFoldNotAllowed`).

---

## Transformation Rules

```
fold(list) {
  | [] => 0
  | x :: @rest => 1 + rest
}
```
becomes:
```
let fold$1 = fn(fold$1.expr) => match(fold$1.expr) {
  | [] => 0
  | x :: rest => 1 + fold$1(rest)
} in fold$1(list)
```

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandTopLevelFolds
- **Later passes that rely on this pass**: ExpandLambdaMatchExpressions

---

## Side Effects

- **Generates diagnostics**: `FoldPatternInRegularMatch`, `NamedFoldNotAllowed`, `FoldPatternOutsideConstructor`
- **Creates fresh names**: `fold.N` for each expanded fold