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
- **Required invariants**: Patterns desugared (`DesugarPatterns`)

## Output

- Same type with all or-patterns expanded into separate clauses

## Detailed Behavior

### `passImpl`

Before doing any work, scans the module for an or-pattern with a pure, lazy check
(`universeBi`, stopping at the first `POr`). A module with no or-patterns is
returned unchanged and the monadic whole-module traversal is skipped entirely.

This is not only a fast path. `transformBiM` sequences one monadic action per AST
node, so it builds an `O(nodes)` chain of `RWST` bind closures; that chain is not
forced until the traversal completes, so it stays live for the whole traversal and
dominated the compiler's live heap on large multi-module builds. Skipping the
traversal where it would be the identity removes the chain.

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

- **Earlier passes this relies on**: DesugarPatterns
- **Later passes that rely on this pass**: CheckPatternAnomalies

## Side Effects

- **Generates diagnostics**: `OrPatternVariableMismatch`

## Notes

- The pass is the identity on modules without or-patterns, which is the common
  case; the `passImpl` short-circuit above is what keeps it from costing memory on
  every module.
- Pass order: this pass runs *before* `ExpandGuards` (see `phaseTranslation` in
  `Coal.Compiler.Pass.PhaseTranslation`), so it cannot rely on guards having been
  expanded.