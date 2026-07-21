# KernelCodegen

## Purpose

Compile kernel IR modules to LLVM bitcode. Runs the new-kernel compiler on all
source modules together, then assembles each resulting `IRModule` to bitcode via
`llvm-as`.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseLowering/KernelCodegen.hs
```

---

## Summary

Takes all kernel modules (from `KernelTranslateNew`), injects builtin `DData`
constructor definitions into every module, runs the kernel compiler pure pipeline
to produce `IRModule`s, renders LLVM assembly text, and assembles it to bitcode.
Cached modules bypass this process and contribute their stored bitcode directly.

---

## Input

- **AST representation**: `[BuildEnvelope (Module NK.Type)]` — kernel IR modules
- **Required invariants**: All modules translated to kernel IR

---

## Output

- `[(Name, ByteString)]` — list of (module name, LLVM bitcode) pairs

---

## Detailed Behavior

### `pass`

1. Separates `BSource` modules from `BCached` modules (cached modules already
   have bitcode)
2. Optionally dumps debug pretty-printed kernel IR if `configGenerateDebugArtifacts`
   is enabled
3. Injects `builtinDData` constructors into every source module AND the builtin
   module (`Builtin.builtinObjects`)
4. Runs `NK.runCompiler (NK.compileModules (builtinMod : augmented))` to compile
   all modules together through the kernel pipeline
5. For each resulting `IRModule`, calls `assembleOne` to produce bitcode
6. Returns assembled bitcode merged with cached module bitcode

### `assembleOne`

1. Renders the `IRModule` to LLVM text via `renderModule`
2. Writes the text to a temporary `.ll` file
3. Optionally writes a debug `.ll` file if `configGenerateLLVMOutput`
4. Runs `llvm-as file -o -` to produce bitcode (reading from stdout)

### `builtinDData`

A hardcoded list of `DData` objects injected into every module:
- **List**: `$Cons` (tag 0), `$Nil` (tag 1) — ordered lexicographically
- **Record**: `$Record` (tag 0)
- **Nat**: `$Succ` (tag 0), `$Zero` (tag 1) — ordered lexicographically
- **Tuples**: `$Tuple2` through `$Tuple8` (each with a single constructor)

These constructors are injected because their `DData` is never produced by
normal `DType` translation, but the LLVM codegen needs the struct type
declarations and `make_%` functions for them.

The lexicographic ordering is critical because `CaseExpressionCanonicalization`
sorts `ECase` clauses lexicographically, and the LLVM codegen assigns switch
tags by clause position.

---

## Compiler Interactions

- **Earlier passes this relies on**: KernelTranslateNew
- **Later passes that rely on this pass**: Linking

---

## Important Data Structures

- `IRModule` — from `llvm-hs`, the LLVM IR module
- `DData` — kernel IR object representing a data type with constructors

---

## Side Effects

- **Performs IO**: Writes debug files, runs `llvm-as` subprocess, creates temp files
- **Generates diagnostics**: Prints compilation failure messages to stdout
- **Modifies compiler state**: No

---

## Notes

The kernel compilation is done with all source modules together (not per-module)
because the LLVM codegen needs cross-module context. The `Builtin$` module is
always compiled first, and its IR appears at index 0 in the results list.