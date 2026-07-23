# ExpandOrPatterns

## Purpose

Expand or-patterns (disjunctive patterns) into separate pattern match clauses.

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/ExpandOrPatterns.hs
```

## Summary

Transforms clauses containing `p1 or p2` patterns into multiple clauses, one per alternative. Validates that both sides of an or-pattern bind the same set of variables. Recursively expands or-patterns nested within constructors, tuples, lists, records, and other pattern forms.

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Guards expanded

## Output

- Same type with all or-patterns expanded into separate clauses

## Detailed Behavior

### `expandOrPatterns` (Clause instance)

Expands the clause pattern via `expandOrPatterns`. For each alternative pattern, duplicates the clause body into a separate clause.

### `expandOrPatterns` (Pattern instance)

For `POr loc _ p1 p2`:
1. Checks that `boundIn p1 == boundIn p2`; if not, reports `OrPatternVariableMismatch`
2. Recursively expands both sides, then concatenates the results

For compound patterns (constructors, tuples, lists, records, `PAs`, annotations): recursively expands sub-patterns and produces the Cartesian product of alternatives.

## Transformation Rules

```
match(x) {
  | Just(y) or Nothing => 0
}
```
becomes:
```
match(x) {
  | Just(y) => 0
  | Nothing => 0
}
```

## Compiler Interactions

- **Earlier passes this relies on**: ExpandGuards
- **Later passes that rely on this pass**: CheckPatternAnomalies

## Side Effects

- **Generates diagnostics**: `OrPatternVariableMismatch`