# ExpandIntegerLiteralPatterns

## Purpose

Expand integer literal patterns into equality guards, since the pattern match
compiler operates on constructor discrimination rather than primitive equality.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/ExpandIntegerLiteralPatterns.hs
```

---

## Summary

For each `PInteger` pattern in a match clause, replaces it with a fresh variable
and generates an if-expression that checks equality against the literal value.
Integer literals are converted to expressions using the appropriate `from_int32`,
`from_int64`, or `from_bignum` constructor based on magnitude.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: As-patterns expanded

---

## Output

- Same type with integer literal patterns eliminated

---

## Detailed Behavior

### Match scrutinee binding

Before expanding clauses, every `EMatch` is rewritten to bind its scrutinee once:

```
let $scrut.N = <scrutinee> in
match($scrut.N) { ... }
```

Fallthrough arms rebuild a nested match on that bound variable. Without this
binding, an effectful scrutinee (e.g. `EventSource.select`) would be
re-evaluated for every failed integer arm, dropping events and breaking event
loops.

### `expandClause`

1. Desugars the clause expression recursively
2. Collects all integer literal patterns via `collectIntegerLiteralPatterns`
3. If integer patterns were found:
   - Generates an if-expression: `if (var1 == from_literal(value1) && ...) then body else <fallback>`
   - The fallback is a match on the remaining clauses against the **bound** scrutinee variable
4. If no remaining clauses exist, reports `NonExhaustivePatterns`

### `collectIntegerLiteralPatterns`

Uses `WriterT` to collect `(Label, Integer)` pairs while transforming each
`PInteger a t int` into `PVariable a (Label t "int.[N]")`.

### `numericLiteral`

Creates an equality expression using `(==)` and the appropriate `fromLiteral`
constructor.

### `fromLiteral`

Chooses the integer constructor based on value bounds:
- `≤ maxBound Int32` → `from_int32`
- `≤ maxBound Int64` → `from_int64`
- otherwise → `from_bignum` (via `unsafe_parse_bignum`)

---

## Transformation Rules

```
match(effect()) {
  | 0 => A
  | 1 => B
  | _ => C
}
```
becomes approximately:
```
let $scrut.N = effect() in
match($scrut.N) {
  | int.[1] =>
      if (int.[1] == from_int32(0)) then A
      else match($scrut.N) {
             | int.[2] =>
                 if (int.[2] == from_int32(1)) then B
                 else match($scrut.N) { | _ => C }
           }
}
```

The outer `let` ensures `effect()` runs once even though fallthrough rebuilds
nested matches.

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandAsPatterns
- **Later passes that rely on this pass**: CompileMatchExpressions

---

## Side Effects

- **Creates fresh names**: `$scrut.N` for the bound scrutinee; `int.[N]` for each integer pattern
- **Generates diagnostics**: `NonExhaustivePatterns` if integer patterns lack fallthrough
