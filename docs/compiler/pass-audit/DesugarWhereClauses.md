# DesugarWhereClauses

## Purpose

Desugar where-clauses in function and constant definitions.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DesugarWhereClauses.hs
```

---

## Summary

The pass is currently a **no-op**: `passImpl = return`. The commented-out code shows
the intended behavior — lifting where-clause definitions out of function/constant
bodies as renamed top-level definitions.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata k t)]`
- **Required invariants**: Builtins inserted

---

## Output

Same as input (unchanged).

---

## Transformation Rules

None currently active. The commented-out implementation shows where-clause
definitions would be:
1. Given manufactured names (e.g., `parentName__$local_origName`)
2. Extracted from `DFunction` / `DConstant` where-clause lists
3. Re-inserted into the module definition list with name references updated

---

## Compiler Interactions

- **Earlier passes this relies on**: InsertBuiltinDefinitions
- **Later passes that rely on this pass**: DesugarDoNotation

---

## Side Effects

None.

---

## Notes

The code is marked as `TODO` and all where-clause handling appears to be deferred.