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
definition's names. For `TypeExport`, checks the type name and each member
name (data constructors for a data type, interface members for a trait). The
wildcard member list `["*"]` — written as a bare type name, `T` — has no
individual members to validate and is skipped.

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
- `TypeExport loc typeName memberNames`: Checks `typeName ∈ definedTypeNames`
  (type, type-alias, or trait names); then, unless `memberNames` is the wildcard
  `["*"]`, checks that each member is a data constructor of that type or an
  interface member of that trait

### Helper functions

- `definitionNames`: extracts all user-visible names from a definition
- `typeNames`: extracts type, type-alias, and trait names
- `memberMap`: builds a `[(ownerName, memberName)]` mapping for data types
  (their constructors) and traits (their interface members)

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