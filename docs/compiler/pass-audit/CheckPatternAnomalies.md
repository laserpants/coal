# CheckPatternAnomalies

## Purpose

Check pattern exhaustiveness for match expressions.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/CheckPatternAnomalies.hs
```

---

## Summary

Validates that every match expression covers all possible cases (is exhaustive).
Reports `NonExhaustivePatterns` for incomplete matches. Handles patterns in match
expressions, with let-bindings and lambdas already desugared into match by
earlier passes.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Or-patterns expanded

---

## Output

- Same type (no transformation)
- **Established invariants**: All match expressions are exhaustive

---

## Detailed Behavior

### `checkPatternAnomalies` (Expression instance)

Recursively traverses expressions. For `EMatch` and `ELambdaMatch` nodes:
1. Calls `checkExhaustive` with the location and clause list
2. Then recursively checks sub-expressions

### `checkExhaustive`

1. Translates clauses into internal pattern representation via `translatePattern`
2. Calls `exhaustive` from `Coal.Compiler.PatternMatching.AnomalyDetection`
3. If not exhaustive, reports `NonExhaustivePatterns`

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandOrPatterns
- **Later passes that rely on this pass**: ExpandRecordPatterns

---

## Side Effects

- **Generates diagnostics**: `NonExhaustivePatterns`
- **Modifies compiler state**: No