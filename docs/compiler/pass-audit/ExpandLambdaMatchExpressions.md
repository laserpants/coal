# ExpandLambdaMatchExpressions

## Purpose

Expand lambda-match expressions into standard lambda expressions with embedded match.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/ExpandLambdaMatchExpressions.hs
```

---

## Summary

Transforms `ELambdaMatch` nodes into `ELambda` containing an `EMatch`. The compact syntax
`match { | p1 => e1 | p2 => e2 }` becomes `fn($lambda_match) => match($lambda_match) { | p1 => e1 | p2 => e2 }`.

---

## Input

- **AST representation**: `Module Metadata Kind ()`
- **Required invariants**: Expression folds expanded

---

## Output

- **Resulting AST**: Same type with all `ELambdaMatch` nodes eliminated
- **Established invariants**: No lambda-match expressions remain

---

## Detailed Behavior

### `expandLambdaMatchExpressions` (Expression instance)

Uses `transformM` (bottom-up traversal). For `ELambdaMatch _ _ clauses`:
Returns `lambdaE (varP "$lambda_match" :| []) (matchE (varE "$lambda_match") clauses)`.

---

## Transformation Rules

```
match {
  | [] => 0
  | x :: xs => x + 1
}
```
becomes:
```
fn($lambda_match) => match($lambda_match) {
  | [] => 0
  | x :: xs => x + 1
}
```

---

## Analysis

- **Tree traversals**: Bottom-up via `transformM` (uniplate generic transformation)
- **Fresh-name generation**: No (uses fixed name `$lambda_match`)

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandExpressionFolds
- **Later passes that rely on this pass**: TypeInference

---

## Side Effects

- **Generates diagnostics**: No
- **Modifies compiler state**: No