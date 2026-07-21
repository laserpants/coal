# SortModules

## Purpose

Sort modules in dependency order via topological sort and detect cyclic dependencies.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/SortModules.hs
```

---

## Summary

Performs a topological sort of modules based on their import dependencies.
Detects strongly connected components (cycles) in the module dependency graph.
Validates that a `Main` module exists and that all imported modules are present.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Assumptions**: All modules have been parsed successfully
- **Required invariants**: Valid parse output

---

## Output

- **Resulting AST**: `[BuildEnvelope (Module Metadata () ())]` — sorted in
  dependency order (dependencies before dependents)
- **Established invariants**:
  - Modules appear in topological order
  - No cyclic import graph exists
  - A `Main` module exists
  - All imported modules are present in the compilation unit
- **Guarantees made to later passes**: Correct processing order for all
  subsequent passes (dependencies are always processed before dependents)

---

## Detailed Behavior

### `passImpl`

1. Checks that `Main` is among the module names; if not, reports `NoModuleMain`
   and throws `PreflightFailure`.
2. Collects import edges for each module via `collectEdges`.
3. Uses `listenErrors` to collect `ModuleNotFound` errors; if any modules are
   missing, aborts.
4. Runs `stronglyConnComp` on the edges to compute SCCs.
5. Filters for `CyclicSCC` components; if any cycles exist, reports
   `ModuleCycle` and aborts.
6. Otherwise, flattens the SCCs into a list (acyclic components contain one
   module, cyclic components are all kept together).

### `collectEdges`

For each module, extracts its import dependencies via `unitDependencies`,
checks each dependency against the set of known module names, and reports
`ModuleNotFound` for any missing imports.

### `unitDependencies`

Extracts dependency paths from a module:
- For `BSource` modules, uses the `dependencies` function on the module's definitions
- For `BCached` modules, uses the `buildDependencies` list from the cached build

### `dependencies`

Collects import paths from `DImport` and `DNamespaceImport` definitions.
For non-builtin modules, also adds `Coal.Applicative` and `Coal.Monad` as
implicit additional dependencies (these are always needed).

---

## Transformation Rules

No AST transformation — this pass only reorders the module list. The module
contents themselves are unchanged.

---

## Analysis

- **Graph algorithms**: Uses `Data.Graph.stronglyConnComp` for cycle detection
  on the import dependency graph
- **Error collection**: Uses `listenErrors` to batch-collect missing module
  errors before deciding to abort

---

## Compiler Interactions

- **Earlier passes this relies on**: Parsing (needs parsed modules with import statements)
- **Later passes that rely on this pass**: All subsequent passes depend on correct
  module ordering; `RefreshCache` specifically needs sorted modules

---

## Important Data Structures

- `SCC` — strongly connected component from `Data.Graph`
- `Set Name` — set of module names for membership testing

---

## Side Effects

- **Generates diagnostics**: `NoModuleMain`, `ModuleNotFound`, `ModuleCycle`
- **Modifies compiler state**: None
- **Creates fresh names**: No

---

## Notes

The implicit dependency on `Coal.Applicative` and `Coal.Monad` for all non-builtin
modules means these modules are always processed before any user module. This
ensures the standard library foundations are available during type checking.