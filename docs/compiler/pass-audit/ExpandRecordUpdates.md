# ExpandRecordUpdates

## Purpose

Desugar partial record update expressions into record pattern matches.

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/ExpandRecordUpdates.hs
```

## Summary

A record update `record{ field = value, … }` is rewritten into a match whose
clause observes the updated fields and binds the remaining row via a record
pattern tail:

```
match(record) {
  | { field = $update.n, … | $update.m } =>
      { field = value, … | $update.m }
}
```

The generated match is then handled by the existing record machinery:
`ExpandRecordPatterns` turns the record pattern into a `$Record` constructor
pattern plus `EFocus` operations. A record pattern tail is a row-restricted
*view* of the base record — at runtime its linked list still carries every
field, so re-extending it with the updated fields shadows their previous
values while leaving all other fields reachable. The base expression is
evaluated exactly once. Generated binder names (`$update.N`) cannot be written
in surface syntax, so they can never collide with user bindings.

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Type checking completed (`ERecordUpdate` carries its
  inferred record type)

## Output

- Same type, with record updates replaced by record pattern matches

## Detailed Behavior

1. Extracts the base row from the update's `t` annotation
2. Allocates fresh field binders ($update.N) typed with the base row's field
   types, and a fresh tail binder typed as the base row minus the updated
   fields
3. Builds a single-clause irrefutable `EMatch` whose body is an `ERecord` over
   the tail with the new field values

The update's type equals the base record's type; each new value's type has
already been unified with the corresponding field type by the type checker
(`RuleRecordEquality`-adjacent `RuleRecordUpdate` constraints).

## Transformation Rules

```
record{ field_c = "new value" }
```

becomes approximately:

```
match(record) {
  | { field_c = $update.0 | $update.1 } =>
      { field_c = "new value" | $update.1 }
}
```

## Compiler Interactions

- **Earlier passes this relies on**: TypeInference (concrete record types)
- **Later passes that rely on this pass**: ExpandRecordPatterns (consumes the
  generated `PRecord`)

## Side Effects

- **Creates fresh names**: Field and tail binders `$update.N`
- **Generates diagnostics**: No
