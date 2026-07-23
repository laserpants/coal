# DetectDuplicateParams

## Purpose

Detect duplicate parameter names in function definitions, type definitions,
lambda expressions, and pattern bindings.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DetectDuplicateParams.hs
```

---

## Summary

Uses a `StateT (Set Name)` to track seen parameter names. In patterns, each
variable binding (`PVariable`, `PAtVariable`, `PShorthand`, `PNamedFold`) checks
against the accumulated set and reports `ConflictingParameter` for duplicates.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Shadowing check completed

---

## Output

- **Resulting AST**: Same as input (no transformation)
- **Established invariants**: No duplicate parameters in any definition

---

## Detailed Behavior

### `checkTypeParameters`

For type and alias definitions, tracks type parameter names in a `StateT` set.
Reports duplicates via `ConflictingParameter`.

### `checkPatterns` / `checkDup`

For each pattern variable, checks against the current set. In `POr` patterns,
resets the state after checking the left side so both sides of an or-pattern
can bind the same variables (this is required).

---

## Analysis

- **Tree traversals**: Explicit recursion through all expression/pattern constructors
- **Environment handling**: `StateT (Set Name)` threaded through `evalStateT`
- **Recursion**: Fully recursive through all pattern and expression forms

---

## Compiler Interactions

- **Earlier passes this relies on**: DetectShadowing
- **Later passes that rely on this pass**: DetectInvalidExports

---

## Side Effects

- **Generates diagnostics**: `ConflictingParameter`
- **Modifies compiler state**: No