# PrepareBuild

## Purpose

Populate the `Build` environment by collecting and cataloging all definitions from
a Coal module, serving as the foundation for type inference and later compilation
stages.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/PrepareBuild.hs
```

---

## Summary

Runs in a `ReaderT (ExportList a) (StateT (Build a) (CompilerT a m))` monad stack,
executing 10 strictly ordered steps: type constructors → data constructors → folds →
export expansion → traits → trait interfaces → instances → builtin instances →
imports → placeholders → qualified name resolution. The ordering is critical:
each step depends on data registered by previous steps.

---

## Input

- **AST representation**: `Module Metadata Kind ()`
- **Required invariants**: Aliases expanded

---

## Output

- **Resulting AST**: Same type (no AST change, but build environment populated)
- **Established invariants**: Build environment fully populated with all definitions

---

## Detailed Behavior

The `prepareDefinitions` function runs 10 steps:

1. **Builtin type/data constructors**: Inserts `List`, `Zero`, `Succ` as compiler-provided
2. **Type constructors** (`collectTypeConstructors`): Gathers `DType` definitions with their kinds
3. **Data constructors** (`collectDataConstructors`): Gathers data constructors with schemes
4. **Folds** (`collectFolds`): Registers fold names
5. **Export expansion** (`expandExports`): Converts `Type(*)` to explicit constructor
   lists; keeps `Type(Name)` exports for type aliases unchanged (aliases carry no
   constructors of their own)
6. **Traits** (`collectTraits`): Registers trait definitions and imported traits
7. **Trait interfaces** (`collectTraitsInterface`): Registers trait member signatures
8. **Instances** (`collectInstances`): Registers trait implementations with their methods
9. **Builtin instances**: Adds compiler-provided instances
10. **Imports** (`collectImports`): Processes name/type/namespace imports
11. **Placeholders** (`collectPlaceholders`): Creates entries for to-be-inferred definitions
12. **Qualified names** (`qualifiedImports`): Builds local→qualified name mappings

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandAliases, KindIndexing
- **Later passes that rely on this pass**: ExpandTopLevelFolds, ExpandExpressionFolds, TypeInference

---

## Important Data Structures

- `NameEntry` — `NName`, `NType`, `NTrait`, `NTypeAlias`, `NPlaceholder`
- `TypeConstructorEntry`, `DataConstructorEntry`, `TraitEntry`, `InstanceEntry`, `AliasEntry`
- `Build` — the central environment shared across modules

---

## Side Effects

- **Modifies compiler state**: Populates `compilerBuilds` via `updateCurrentBuildC`
- **Generates diagnostics**: `DuplicateTypeName`, `UnboundTypeVariable`, `ConflictingImports`,
  `ImportNotInModule`, `TraitNotInScope`, `MissingType`, `NoDataConstructorForType`
- **Creates fresh names**: Instance label names