# DetectAliasCycles

## Purpose

Detect cyclic type alias definitions (e.g., `type alias A = B` and `type alias B = A`).

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DetectAliasCycles.hs
```

---

## Summary

Traverses type alias definitions looking for self-references. If a type alias
references itself (directly or indirectly through other aliases), it reports a
`TypeAliasCycle` error. The check runs on the pre-expansion aliases (before
`ExpandAliases` in the type-checking phase).

---

## Input

- **AST representation**: `Module Metadata () ()` — pre-type-checking AST
- **Required invariants**: Do-notation desugared

---

## Output

- **Resulting AST**: Same as input (no transformation)
- **Established invariants**: No cyclic type alias definitions exist

---

## Detailed Behavior

### `detectCycles` (Definition instance)

Only operates on `DTypeAlias` definitions. For each, calls `detectCyclesInType`
on the alias body.

### `detectCyclesInType`

Recursively traverses the type structure:
- `TApplication`, `TArrow`: recurses into both sides
- `TConstructor`: checks if the constructor name matches the alias name
  (direct self-reference)
- `TRecord`, `TRow`, `TAlias`: recurses into components
- Other types: no check needed

---

## Compiler Interactions

- **Earlier passes this relies on**: DesugarDoNotation
- **Later passes that rely on this pass**: ExpandAliases (must not encounter cycles)

---

## Side Effects

- **Generates diagnostics**: `TypeAliasCycle`
- **Modifies compiler state**: No

---

## Notes

Only direct self-references are detected (where the alias name appears as a
`TConstructor`). The pass does not build a full dependency graph of aliases,
so transitive cycles like `A → B → A` may only be caught if the alias expansion
in `ExpandAliases` encounters a loop at runtime. In practice, the recursive
traversal of `TAlias` nodes should catch transitive cycles because it follows
the alias references.