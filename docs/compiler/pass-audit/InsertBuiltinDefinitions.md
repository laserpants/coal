# InsertBuiltinDefinitions

## Purpose

Inject compiler-provided builtin definitions into all modules ensuring access to compiler
primitives without requiring explicit imports.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/InsertBuiltinDefinitions.hs
```

---

## Summary

For each module, inserts builtin definitions. For builtin (standard library) modules,
only the core `insertBuiltinDefinitions` set is added. For user modules, both
`insertBuiltinDefinitions` and `insertExtraDefinitions` are added.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Imports at top of module

---

## Output

- **Resulting AST**: Same type, with definitions augmented with builtins
- **Established invariants**: All modules have compiler-provided builtins available

---

## Detailed Behavior

### `insertModuleBuiltins`

Checks if the module is a builtin module (by comparing the path against
`builtinModulesPaths`). If so, only `insertBuiltinDefinitions` is applied.
Otherwise, `insertExtraDefinitions . insertBuiltinDefinitions` is used.

---

## Transformation Rules

Builtin definitions (from `src/Coal/Compiler/Builtin/Definitions.hs`) are prepended
or appended to the definition list. The exact insertion logic is in the
`insertBuiltinDefinitions` / `insertExtraDefinitions` functions.

---

## Compiler Interactions

- **Earlier passes this relies on**: DetectMisplacedImportStatements
- **Later passes that rely on this pass**: All subsequent passes that need
  name resolution for builtins

---

## Side Effects

- **Generates diagnostics**: No
- **Modifies compiler state**: No (only transforms modules)