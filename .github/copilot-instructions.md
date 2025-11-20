# Copilot Instructions for Coal

## Project Overview
- **Coal** is a statically typed, purely functional programming language with a Haskell/ML-inspired type system, implemented in Haskell.
- The compiler targets LLVM for code generation and enforces totality via structural recursion and codata/corecursion distinctions.
- Major directories:
  - `src/Coal/`: Main compiler source, including language, parser, type system, and codegen.
  - `lang/`: Standard library modules in Coal.
  - `runtime/`: C runtime support for the language.
  - `app/`: Entry point for the compiler executable.
  - `test/`: Haskell and E2E tests, plus example Coal programs.

## Key Patterns & Conventions
- **Recursion**: Only structural recursion is allowed for data; codata uses corecursion (see `fold`/`unfold` in README and `src/Coal/Language/Expression.hs`).
- **Type System**: Parametric polymorphism and type inference are central; see `src/Coal/TypeSystem/` and `src/Coal/Language/Type.hs`.
- **Pattern Matching**: Extensively used for both data and codata; see `src/Coal/Language/Pattern.hs`.
- **Compiler Passes**: Organized in `src/Coal/Compiler/Pass/` and related submodules.
- **AST**: Defined and transformed in `src/Coal/AST/`.
- **Standard Library**: Coal source files in `lang/` are used for bootstrapping and user programs.

## Developer Workflows
- **Build**: Use `stack install` (requires GHC, LLVM, libgc, gmp; see README for details).
- **Run**: The compiler executable is `coal`. Example: `coal Main.coal ./lang/IO.coal -o dist`.
- **Test**: Haskell tests in `test/`, run with `stack test`. E2E Coal examples in `test/examples/`.
- **Debug**: Use `stack ghci` for interactive debugging. Compiler errors are usually descriptive.

## Integration & Dependencies
- **LLVM**: Required for codegen; ensure `llc` is available.
- **C Runtime**: See `runtime/` for C code linked with generated binaries.
- **External Libraries**: Boehm GC (`libgc`), GMP (`libgmp`).

## Project-Specific Advice
- Prefer adding new language features as compiler passes or AST transformations.
- Follow the directory/module structure for new components (e.g., new passes in `src/Coal/Compiler/Pass/`).
- Use the `lang/` directory for standard library additions.
- Reference the README for recursion/codata idioms and build/test instructions.

## References
- [README.md](../README.md): Language philosophy, build, and usage.
- [src/Coal/](../src/Coal/): Compiler architecture and key modules.
- [lang/](../lang/): Standard library examples.
- [test/](../test/): Test structure and E2E examples.

---
For more, see https://coal-lang.org/ and in-code documentation.
