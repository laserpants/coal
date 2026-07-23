# ExpandRecordPatterns

## Purpose

Desugar record patterns into field select operations (focus expressions).

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/ExpandRecordPatterns.hs
```

## Summary

Transforms record patterns in match clauses into constructor patterns with
explicit field access. A record pattern like `{ x, y }` becomes a `$Record`
constructor pattern followed by `EFocus` operations that extract individual fields.
Shorthand patterns (where the field name doubles as the variable name) are
desugared to explicit variable patterns.

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Pattern anomaly checks completed

## Output

- Same type, with record patterns desugared into focus chains

## Detailed Behavior

### `desugarRecordPatterns` (Pattern instance)

For `PRecord`:
1. Desugars shorthand patterns to explicit variable patterns
2. Generates a fresh row variable name `row.N`
3. Records a `RecordEntry` containing the field dict, original variable name,
   and optional row tail pattern
4. Returns a `PConstructor` for `$Record` containing just the row variable

### `desugarRecordPatterns` (Expression instance)

For `EMatch` with record patterns:
1. Processes clauses in order, for each one:
   - Desugars the pattern, collecting `RecordEntry` data
   - Calls `desugar` for each field to build the `EFocus` chain
2. The `desugar` function builds a nested expression that:
   - Focuses on each field in turn
   - Creates match expressions for interior field patterns
   - Handles fallthrough to remaining clauses when fields don't match

## Transformation Rules

A record pattern:
```
match(record) {
  | { x: 5, y } => body
}
```
becomes approximately:
```
match(record) {
  | $Record(row.0) =>
      match(row.0.field.x) {
        | 5 =>
          match(row.0.tail) {
            | $Record(row.1) =>
              focus(row.1.field.y as y_var) in
              match(y_var) { | y => body }
          }
      }
}
```

## Compiler Interactions

- **Earlier passes this relies on**: CheckPatternAnomalies
- **Later passes that rely on this pass**: ExpandAsPatterns

## Side Effects

- **Creates fresh names**: Row variable names `row.N`, field prefixes
- **Generates diagnostics**: No