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
executing 10 strictly ordered steps: type constructors → export expansion → data
constructors → folds → traits → trait interfaces → instances → builtin instances →
imports → placeholders → qualified name resolution. The ordering is critical:
each step depends on data registered by previous steps, and export expansion must
run before data constructors are collected so that the export list gates which
constructors are exported.

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
3. **Export expansion** (`expandExports`): Resolves wildcard export member lists to explicit
   ones — a data type `T` becomes `T(A, B, ...)`, a trait `Tr` becomes `Tr(m1, m2, ...)`,
   and a type alias becomes `Alias()`. An empty member list (`T()`) is left untouched, so
   `T` exports every member while `T()` exports none
4. **Data constructors** (`collectDataConstructors`): Gathers data constructors with schemes,
   honouring the expanded export list (a constructor is only exported if the export list
   selects it). Exported members — data constructors and trait interface members — are
   recorded in `buildExportedMembers`, which is kept separate from `buildExportedNames` so
   that a member sharing its name with its owning type (`type Thing = Thing(int32)`) can be
   exported or withheld independently of the type
5. **Folds** (`collectFolds`): Registers fold names and records their expression-level and
   @-pattern (structural recursion) dependencies (`foldExprDeps` / `foldPatternDeps`)
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