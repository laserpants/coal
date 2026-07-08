# Project overview

Coal is a declarative, statically typed, purely functional programming language.
The compiler is written in **Haskell** and targets **LLVM** for native code generation.

For build instructions and language documentation, see `README.md` and [coal-lang.org](https://coal-lang.org/).

## Key facts for working on this project

- **The compiler pipeline** (`src/Coal/Compiler/`) processes source files through six sequential phases: parsing → preflight → type checking → translation → lowering → linking.

- **The kernel** (`src/Coal/Kernel/`) provides the intermediate representation, a normalization pipeline, and the LLVM code generation backend.

- **The runtime** (`runtime/`) is a C11 library built with CMake. It uses Boehm GC for memory management and GMP for arbitrary-precision integer arithmetic.

- **Do NOT run `stack test`.** The full test suite takes a very long time. After implementing changes, hand control back to the user to evaluate, or run ad-hoc localized tests when it is straightforward to do so.

## Repository layout highlights

| Directory | Purpose |
|-----------|---------|
| `src/Coal/Compiler/` | High-level pipeline (parsing → linking) |
| `src/Coal/Kernel/` | Kernel IR, normalization passes, LLVM codegen |
| `src/Coal/Language/` | AST definitions, types, expressions, modules |
| `src/Coal/Parser/` | Source text → AST |
| `src/Coal/TypeSystem/` | Kinds, constraints, unification |
| `runtime/` | C11 runtime library |
| `test/E2E/` | End-to-end tests |