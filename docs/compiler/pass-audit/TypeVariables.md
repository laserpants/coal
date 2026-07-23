# TypeVariables

## Purpose

Utility module for collecting type variable names from type alias definitions.
Not a compiler pass itself, but used by `KindIndexing` and `PrepareBuild` to
check for unbound type variables.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/TypeVariables.hs
```

---

## Summary

Provides `collectTypeVarNames` which extracts the set of type variable names
referenced within a `Type Parameter Kind`. Used to detect unbound type variables
in type alias definitions and in data constructor schemes.

---

## Detailed Behavior

### `collectTypeVarNames`

Recursively traverses a `Type Parameter Kind` and collects all `TVariable`
parameter names into a `Set Name`. Handles all type constructors including
`TApplication`, `TArrow`, `TRecord`, `TRow`, `TAlias`.

---

## Usage in Passes

- **KindIndexing**: Used to check that alias definitions don't reference
  undeclared type variables
- **PrepareBuild**: Used to validate constructor schemes against their type
  definition's parameter list