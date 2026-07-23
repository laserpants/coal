# KindIndexing

## Purpose

The entry point to the type checking phase. Converts the module to kind-indexed form,
assigning proper `Kind` annotations to type parameters throughout the AST.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/KindIndexing.hs
```

---

## Summary

Performs three main tasks:
1. Kind indexing via `toKindIndexed` — assigns `Kind` annotations to type parameters
2. Environment setup — clears previous state, inserts builtin functions into name store
3. Build preparation — collects type aliases and prepares the `Build` structure

Transforms `Module Metadata () ()` → `Module Metadata Kind ()`.

---

## Input

- **AST representation**: `Module Metadata () ()` — no kind or type annotations
- **Required invariants**: Preflight completed successfully

---

## Output

- **Resulting AST**: `Module Metadata Kind ()` — kind annotations on type parameters
- **Established invariants**: All type parameters have `Kind` annotations;
  builtin functions registered; build aliases collected

---

## Detailed Behavior

### `kindIndexing`

1. Clears assumptions and name store
2. Inserts builtin functions via `insertNameC`
3. Special-cases the `machine` builtin (existentially quantified type `s`)
4. Calls `toKindIndexed` to transform the module
5. Runs `prepareBuildAliases` to populate build alias information
6. Inserts source hash for change detection

### `prepareBuildAliases`

Uses `ReaderT (ExportList a) (StateT (Build a) (CompilerT a m))` to:
1. Fold through definitions collecting type aliases via `collectTypeAliases`
2. For `DTypeAlias`: validates no duplicate names, checks for unbound type variables,
   inserts name entries and alias entries
3. For `DImport TypeImport`: validates the imported type exists and is not conflicting
4. For `DImport (Path ["Builtin$"])`: skips (handled separately)
5. Calls `insertBuildC` to store the build

### Additional functions

- `insertExportedName` — marks a name as exported (unless it's a builtin)
- `insertNameEntry` — adds a name entry to the build
- `insertAlias` — stores an alias entry
- `importedBuild` — retrieves a build for an imported module

---

## Transformation Rules

All type parameters in the AST are annotated with their kinds (e.g., `KType`, `KArrow`).
The exact transformation is handled by `toKindIndexed` from `Coal.Language.Type.Kind.Indexed`.

---

## Analysis

- **Environment handling**: Monad stack `ReaderT (ExportList a) (StateT (Build a) (CompilerT a m))`
- **Dependency analysis**: Resolves imports by looking up imported builds
- **Type variable validation**: Checks for unbound type variables in alias definitions

---

## Compiler Interactions

- **Earlier passes this relies on**: PhasePreflight
- **Later passes that rely on this pass**: All subsequent type checking passes

---

## Important Data Structures

- `ToKindIndexed` typeclass — provides `toKindIndexed`
- `AliasEntry` — stores alias name, parameters, type, and metadata
- `Build` — build information including aliases, names, type constructors

---

## Side Effects

- **Generates diagnostics**: `DuplicateTypeName`, `UnboundTypeVariable`, `ConflictingImports`
- **Modifies compiler state**: Populates name store, assumptions, build cache
- **Creates fresh names**: Supplies fresh type indexes for kind-indexing