# CompileNats

## Purpose

Compile natural number types and constructors into an efficient int32-backed runtime
representation.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/CompileNats.hs
```

---

## Summary

Transforms the `nat` type and its constructors (`Zero`, `Succ`) into an internal
`$Nat` type. The `Nat` type becomes `$Nat`, which is backed by `int32`. `Zero`
becomes `$Zero`, `Succ(n)` becomes `$Succ(unpack(n))`. Pattern matching on nat
constructors is compiled to efficient integer comparisons, reconstructing the
recursive structure only when needed.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Dictionaries inserted

---

## Output

- Same type with nat types/constructors/patterns compiled to internal representation

---

## Detailed Behavior

### Type transformation (`compileNats` Type instance)

`TIntrinsic INat` → `TConstructor KType "$Nat"`. Other types recursively transformed.

### Expression transformation

- `EConstructor "Zero"` → `EConstructor "$Zero" : $Nat`
- `EApplication (EConstructor "Succ") args` →
  `EApplication (EConstructor "$Succ") (EApplication (var "Builtin$.nat$_unpack") args)`
  — unpacks the nat to int32 before wrapping in `$Succ`

### Clause transformation (`compileNats` CompiledClause instance)

For `Succ` patterns:
1. Generates fresh name `nats.N` for the int32 value
2. Reconstructs the recursive nat structure: checks `nats.N == 0`
   (via `Builtin$.comparable.(==)`) — if zero, returns `$Zero`, otherwise
   `$Succ(nats.N - 1)` (via `Builtin$.numeric.(-)`)
3. This ensures the matched variable `s` retains its `$Nat` type for recursive
   use, even though matching tests the underlying int32

For `Zero` patterns: simply changes the label from `Zero` to `$Zero`.

---

## Compiler Interactions

- **Earlier passes this relies on**: InsertDictionaries
- **Later passes that rely on this pass**: DenormalizeAST

---

## Side Effects

- **Creates fresh names**: `nats.N` for reconstructed nat variables
- **Generates diagnostics**: No

---

## Notes

The `nat` → `int32` compilation provides efficient runtime representation while
preserving structural recursion semantics. The `$Nat` type is a kind-indexed
type constructor `KType`, and the unpack/repack pattern ensures stack safety
for deep nat values.