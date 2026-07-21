# ExpandTopLevelFolds

## Purpose

Expand top-level fold definitions into let definitions with explicit lambda expressions
and recursive pattern matching.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/ExpandTopLevelFolds.hs
```

---

## Summary

Transforms `DFold` definitions into `DLet` definitions. The fold's clauses become a
lambda-match expression. `@`-patterns (marking recursive positions) are eliminated and
replaced with explicit recursive calls to the fold function. The pass also validates
that `@`-patterns only appear within constructor patterns.

---

## Input

- **AST representation**: `Module Metadata Kind ()`
- **Required invariants**: Build prepared

---

## Output

- **Resulting AST**: Same type with `DFold` definitions eliminated
- **Established invariants**: No top-level fold definitions remain

---

## Detailed Behavior

### `expandTopLevelFolds`

For `DFold`:
1. Calls `expandClauses` to transform the clauses into a lambda+match expression
2. Creates a `DLet` with the fold name and the expanded expression

### `expandClauses`

1. Generates a fresh name (`fold.N`)
2. Traverses clauses with `expandFolds`, which collects `@`-pattern labels via
   `atLabels`
3. Calls `updateName` to replace each `@`-pattern variable reference with
   `fold(rest)`
4. Eliminates `PNamedFold` and `PAtVariable` patterns via `eliminateAtPatterns`
   (replaces them with `PVariable`)
5. Returns `fn(fold.expr) => match(fold.expr) { ... }`

### `atLabels`

Uses `WriterT` and `transformM` to collect all `(name, label)` pairs from
`PAtVariable` patterns in a pattern tree.

---

## Transformation Rules

```
fold sum : List Nat -> Nat {
  | [] => 0
  | x :: @rest => x + sum(rest)
}
```
becomes:
```
let sum = fn(sum.expr) => match(sum.expr) {
  | [] => 0
  | x :: rest => x + sum(rest)
}
```

---

## Compiler Interactions

- **Earlier passes this relies on**: PrepareBuild
- **Later passes that rely on this pass**: ExpandExpressionFolds

---

## Side Effects

- **Generates diagnostics**: `FoldPatternOutsideConstructor` for misplaced @-patterns
- **Creates fresh names**: `fold.N` for each expanded fold, `fold.expr` for the scrutinee