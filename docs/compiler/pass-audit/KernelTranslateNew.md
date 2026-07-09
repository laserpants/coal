# KernelTranslateNew

## Purpose

Translate surface language modules (`Module Metadata Kind IndexedType`) to kernel IR modules (`Module NK.Type`).

---

## Location

```text
src/Coal/Compiler/Pass/PhaseLowering/KernelTranslateNew.hs
```

---

## Summary

Runs the new kernel translation pipeline (as opposed to the legacy pipeline). For each surface module, it translates every definition via `translateDefinition` from `Coal.Compiler.Kernel.Translate.Definition`, producing a list of kernel `Object`s. Qualified names from the build are inserted into the environment to enable cross-module references.

This pass runs per-module via `mapPass` in the lowering phase.

---

## Input

- **AST representation**: `BuildEnvelope (Module Metadata Kind IndexedType)`
- **Required invariants**: Fully type-checked and transformed surface AST

---

## Output

- **Resulting AST**: `BuildEnvelope (Module NK.Type)` — kernel IR modules with:
  - Module name (principal path)
  - Module imports (qualified names from build)
  - Module objects (translated definitions)

---

## Detailed Behavior

### `pass`

For each surface module:
1. Sets the current module path in compiler state (`setCurrentPathC`)
2. Retrieves the build (`getCurrentBuildC`) to get `buildQualifiedNames`
3. Inserts qualified names into the environment (`insertQualifiedNames`)
4. Runs `withModuleName` to set the current module name context
5. Translates each surface definition via `translateDefinition` (using `concatForM`)
6. Assembles a `NKModule.Module` with the module name, imports, and objects

### `translateDefinition`

This function lives in `Coal.Compiler.Kernel.Translate.Definition`. The code
handles translating:
- `DFunction` / `DLet` — function and let definitions
- `DType` — type/data type definitions
- `DInstance` — trait instance implementations
- `DTypeAlias` — type aliases

The translated kernel IR is in administrative normal form (ANF) ready for LLVM codegen.

---

## Compiler Interactions

- **Earlier passes this relies on**: CheckTraitAnnotations (last translation pass)
- **Later passes that rely on this pass**: KernelCodegen

---

## Side Effects

- **Modifies compiler state**: Sets current path, inserts qualified names
- **Generates diagnostics**: No (errors come from `translateDefinition`)
- **Performs IO**: Progress bar tick