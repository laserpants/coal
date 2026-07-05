# Project overview

Coal is a declarative, statically typed, purely functional programming language.
The compiler is written in **Haskell** and targets **LLVM** for native code generation.

For build instructions and language documentation, see `README.md` and [coal-lang.org](https://coal-lang.org/).

## Key facts for working on this project

- **Two compiler pipelines exist side by side:**
  - **Legacy compiler** (`src/Coal/LegacyKernel/`) — the behavioural reference.
  - **New compiler** (`src/Coal/Compiler/` + `src/Coal/Kernel/`) — the active development target.

- **Two runtimes exist:**
  - **Legacy runtime** (`runtime/`) — loose C files, used by the legacy compiler.
  - **New runtime** (`runtime-next/`) — C11, CMake, structured headers and sources. Uses Boehm GC and GMP. This is the development target.

- **The legacy compiler is the ground truth.** Any change to the new compiler or kernel must preserve the behaviour already exhibited by the legacy compiler. Correctness comes before optimization.

- **Do NOT run `stack test`.** The full test suite takes a very long time. After implementing changes, hand control back to the user to evaluate, or run ad-hoc localized tests when it is straightforward to do so.

## Repository layout highlights

| Directory | Purpose |
|-----------|---------|
| `src/Coal/Compiler/` | New high-level pipeline (parsing → linking) |
| `src/Coal/Kernel/` | Kernel IR, normalization passes, LLVM codegen |
| `src/Coal/Language/` | AST definitions, types, expressions, modules |
| `src/Coal/LegacyKernel/` | Legacy kernel pipeline and LLVM backend |
| `src/Coal/Parser/` | Source text → AST |
| `src/Coal/TypeSystem/` | Kinds, constraints, unification |
| `runtime-next/` | New C11 runtime library |
| `test/E2E/` | End-to-end tests |
