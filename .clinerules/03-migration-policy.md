# Migration policy

## Rule

**The legacy compiler is the behavioural reference.** Any change to the new
compiler or kernel must preserve the behaviour already exhibited by the legacy
compiler. If a Coal program compiled with the old pipeline produces a certain
output, the new pipeline must produce the same output.

## What this means in practice

- **Correctness before optimization.** Do not sacrifice behavioural fidelity for
  performance or code cleanliness. Get it right first.
- **When in doubt, compare.** If a change to the new compiler produces unexpected
  behaviour, run the same input through the legacy compiler and compare results.
  See `workflows/compare-pipelines.md`.
- **Do not delete the legacy code.** It remains as the specification, even after
  the new pipeline fully replaces it at runtime.

## Migration status (high-level)

| Component | Legacy | New | Status |
|-----------|--------|-----|--------|
| Parsing | — | `src/Coal/Parser/` | Shared by both |
| Type checking | — | `src/Coal/Compiler/Pass/PhaseTypeChecking` | Shared |
| Kernel IR AST | `Coal.LegacyKernel.Language` | `Coal.Kernel.Language` | Separate |
| Normalization passes | `Coal.LegacyKernel.Compiler` (inline) | `Coal.Kernel.Pipeline.Passes` (10 structured passes) | Migrated |
| LLVM backend | `Coal.LegacyKernel.LLVM` | `Coal.Kernel.LLVM` | Migrated |
| Runtime | `runtime/` | `runtime-next/` | Migrated |

## Target state

The new pipeline (`src/Coal/Compiler/` + `src/Coal/Kernel/`) and new runtime
(`runtime-next/`) are the active development targets. New features and fixes
should land there. The legacy code exists only as a reference.