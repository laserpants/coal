# DetectInvalidExports

## Purpose

Validate that every name listed in a module's export list refers to a definition
that actually exists within the module.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DetectInvalidExports.hs
```

---

## Summary

For each export in the module's export list, checks whether the exported name
is actually defined. For `NameExport`, checks that the name appears in any
definition's names. For `TypeExport`, checks the type name and each constructor
name.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Duplicate params checked

---

## Output

- **Resulting AST**: Same input (no transformation)
- **Established invariants**: All exported names exist in the module

---

## Detailed Behavior

### `checkExport`

- `NameExport loc name`: Checks `name ∈ definedNames`; if not, reports `ExportNotInModule`
- `TypeExport loc typeName memberNames`: Checks `typeName ∈ definedTypeNames`;
  then for each memberName, checks it's a constructor belonging to that type

### Helper functions

- `definitionNames`: extracts all user-visible names from a definition
- `typeNames`: extracts type and alias names
- `typeConstructorMap`: builds `[(typeName, constructorName)]` mapping

---

## Compiler Interactions

- **Earlier passes this relies on**: DetectDuplicateParams
- **Later passes that rely on this pass**: DetectMainEntrypointMissing

---

## Side Effects

- **Generates diagnostics**: `ExportNotInModule`
- **Modifies compiler state**: No

---

## Notes

The pass works on names only — it does not verify that the exports actually ask
for the right types (that's handled during import resolution in later passes).