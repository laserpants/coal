# Coding conventions

## Haskell

The project uses **fourmolu** for formatting. Configuration is in `fourmolu.yaml`
at the project root. Run `fourmolu -i` on changed files before committing.

### Module organization

Haskell source under `src/` follows a hierarchical module structure:

| Namespace | Purpose |
|-----------|---------|
| `Coal.Language.*` | AST definitions: expressions, types, patterns, modules |
| `Coal.Compiler.Pass.*` | Compilation phases (one module per phase) |
| `Coal.Compiler.*` | Pipeline orchestration, configuration, error types |
| `Coal.Kernel.Language.*` | Kernel IR AST |
| `Coal.Kernel.Pipeline.Pass.*` | Individual normalization passes |
| `Coal.Kernel.LLVM.*` | LLVM IR code generation |
| `Coal.Kernel.Parser.*` | Kernel IR parser |
| `Coal.TypeSystem.*` | Kinds, constraints, substitution, unification |

### Module structure conventions

- Each `*.hs` file starts with language pragmas, then module declaration, then
  exports, then imports.
- Re-export modules (like `src/Coal/Language.hs`) aggregate sub-modules.
- Compiler phases use a consistent `Pass` monad transformer pattern defined in
  `src/Coal/Compiler/Pass.hs`.

## C (runtime)

The new runtime (`runtime-next/`) follows comprehensive C11 coding standards
documented in `runtime-next/CODING_STYLE.md`. Key highlights:

- Use `rt_` prefix for all public API functions and types
- Use Boehm GC allocation (`rt_alloc`, `rt_alloc_atomic`) — never `malloc`/`free`
- Use fixed-width integer types from `<stdint.h>` (`int32_t`, `int64_t`, etc.)
- Format with `clang-format` (LLVM-based style)
- 4-space indentation, 80-character lines, Linux/K&R brace style

Do not duplicate `CODING_STYLE.md` content here. Read that file when working on
runtime C code.