# PhaseLowering

## Purpose

Translate surface language modules to kernel IR modules, then compile kernel IR to
LLVM bitcode. Consists of two passes: kernel translation (per-module, via `mapPass`)
and kernel code generation (all modules together).

## Passes Executed

1. **KernelTranslateNew** — translate surface AST to kernel IR modules (per-module)
2. **KernelCodegen** — compile all kernel IR modules to LLVM bitcode (together)

## Execution Order

```
mapPass passKernelTranslateNew
  >-> passKernelCodegen
```

## Inputs

- `[BuildEnvelope (Module Metadata Kind IndexedType)]` — fully type-checked and
  transformed surface modules

## Outputs

- `[(Name, ByteString)]` — list of (module name, LLVM bitcode) pairs

## Invariants Established by the Phase

- Surface AST translated to kernel IR (administrative normal form)
- Kernel IR compiled to LLVM IR
- LLVM IR assembled to bitcode via `llvm-as`
- Builtin `DData` constructors injected into every module for LLVM codegen
- Cached modules (from `BCached`) pass through with their stored bitcode

## Invariants Expected by Later Phases

The linking phase expects:
- Bitcode for every module (including `Builtin$`)
- Valid LLVM bitcode that can be assembled to object files