# DetectMisplacedImportStatements

## Purpose

Detect import statements that appear after non-import definitions in a module.
Enforces the convention that all imports must appear at the top of the module.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DetectMisplacedImportStatements.hs
```

---

## Summary

Scans each module's definitions. After the first non-import definition, any
subsequent import statement is flagged as an error. This ensures a consistent
module structure where imports always precede other definitions.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Modules are parsed and sorted

---

## Output

- **Resulting AST**: Same as input (no transformation)
- **Established invariants**: All import statements are at the top of each module

---

## Detailed Behavior

### `detectMisplacedImportStatements`

For each module, it:
1. Drops the leading import definitions from the definition list (`dropWhile isImport`)
2. Drops subsequent non-import definitions (`dropWhile (not . isImport)`)
3. Any remaining imports (now after definitions) are reported as `MisplacedImportStatement`

### `isImport`

Returns `True` for `DImport` and `DNamespaceImport` definitions.

---

## Transformation Rules

None. This is a purely diagnostic pass.

---

## Compiler Interactions

- **Earlier passes this relies on**: Parsing, SortModules, RefreshCache
- **Later passes that rely on this pass**: InsertBuiltinDefinitions (needs imports at top
  for proper builtin insertion)

---

## Side Effects

- **Generates diagnostics**: `MisplacedImportStatement`
- **Modifies compiler state**: No