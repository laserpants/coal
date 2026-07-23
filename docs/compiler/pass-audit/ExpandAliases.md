# ExpandAliases

## Purpose

Expand type aliases to their underlying definitions throughout the AST, recursively
resolving nested aliases.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/ExpandAliases.hs
```

---

## Summary

Recursively inlines all type alias references. Uses the `AliasTransform` typeclass
to traverse every type in the AST. When a `TConstructor` is recognized as an alias
(via `buildAliases`), it is replaced with its expanded definition wrapped in a
`TAlias` marker.

---

## Input

- **AST representation**: `Module Metadata Kind ()`
- **Required invariants**: Function groups expanded; aliases registered in build
  (by `KindIndexing`)

---

## Output

- **Resulting AST**: Same type with all alias references expanded
- **Established invariants**: No unresolvable alias references remain in type annotations
- **Guarantees made to later passes**: Type inference works on concrete types only

---

## Detailed Behavior

### `aliasTransform` (Type instance)

For `TConstructor name`: calls `lookupAlias` which checks `buildAliases`.
If found, produces `TAlias name ts expandedType`. If not found, returns unchanged.

For `TApplication (TConstructor name) ts`: calls `aliasTransformTypeApplication`
which propagates the type arguments through the alias expansion via `lookupAlias`.

### `lookupAlias`

Looks up the name in `buildAliases`. If found, calls `transformAliasEntry` to
substitute type parameters with arguments, then wraps the result in `TAlias`.

### `transformAliasEntry`

Two instances:
- `TypeIndex`: Uses `toIndexed` + `Substitution` to instantiate the alias with
  fresh type indexes
- `Parameter`: Uses manual substitution via `substituteAlias`

### Module-level

Also updates `buildNames` (replaces name schemes with expanded versions) and
`buildDataConstructors` (expands types in constructor schemes).

---

## Analysis

- **Tree traversals**: Generic via `descendM` for expressions, manual traversal for types
- **Substitutions**: Type parameter substitution via `Substitution.fromList`
- **Environment handling**: Reads `buildAliases` from current build

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandFunctionGroups, KindIndexing (build aliases)
- **Later passes that rely on this pass**: PrepareBuild, TypeInference

---

## Side Effects

- **Modifies compiler state**: Updates `buildNames` and `buildDataConstructors`
  via `updateCurrentBuildC`
- **Generates diagnostics**: No
- **Creates fresh names**: Fresh type indexes during alias instantiation