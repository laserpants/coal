# Copilot Instructions for Coal

## Project Overview

- **Coal** is a statically typed, purely functional programming language with a Haskell/ML-inspired type system, implemented in Haskell.
- The compiler targets LLVM for code generation and enforces totality via structural recursion and codata/corecursion distinctions.
- Major directories:
  - `src/Coal/`: Main compiler source, including language, parser, type system, and codegen.
  - `src/Coal/Kernel/`: Kernel language and compilation pipeline (separate intermediate representation).
  - `lang/`: Standard library modules in Coal.
  - `runtime/`: C runtime support for the language.
  - `app/`: CLI application with compiler commands and package management.
  - `test/`: Haskell and E2E tests, plus example Coal programs.

## Architecture

- **Two-Stage Compilation**: The compiler uses both a high-level representation (`src/Coal/`) and a kernel representation (`src/Coal/Kernel/`) for intermediate compilation stages.
- **Compiler Pipeline**: Organized in phases (Parsing → Preflight → Main → Lowering → LLVM Codegen); see `src/Coal/Compiler/Pipeline.hs`.
- **CLI System**: `app/Coal/CLI/` contains command definitions (Compile, Build, Clean, Install, Version) and options parsing.
- **Package Management**: `app/Coal/Package/` handles manifest, dependencies, locking, and versioning with support for Git-based dependencies (see `coal.json` and `coal.lock.json`).

## Key Patterns & Conventions

- **Recursion**: Only structural recursion is allowed for data via `fold` expressions with `@`-patterns (see `docs/language-manual.md` and `src/Coal/Language/Expression.hs`). Codata (infinite/lazy structures) use the `Process` type with observation operations (`head`, `tail`, `process` constructor).
- **Type System**: Parametric polymorphism and type inference are central; see `src/Coal/TypeSystem/` and `src/Coal/Language/Type.hs`.
- **Pattern Matching**: Extensively used for both data and codata; see `src/Coal/Language/Pattern.hs`.
- **Compiler Passes**: Organized in phases within `src/Coal/Compiler/Pass/` (ParsingPhase, PreflightPhase, MainPhase, LoweringPhase).
- **AST & Kernel IR**: AST structures in `src/Coal/AST/`; kernel intermediate representation in `src/Coal/Kernel/Language/`.
- **Standard Library**: Coal source files in `lang/` are used for bootstrapping and user programs.
- **LLVM Generation**: Kernel expressions are lowered to LLVM IR; see `src/Coal/Kernel/LLVM/`.

## Import Conventions

The project follows specific conventions for module imports to maintain code clarity and consistency:

### Barrel Modules (API Boundaries)

Barrel modules re-export collections of related modules to provide clean public APIs:

- **`Coal.Compiler`**: Minimal public compiler API
  - Only exposes: `compile`, `compileWithCFiles`, `pipeline`, `prettyError`, `CompilerConfig`, `defaultConfig`
  - Internal compiler modules are NOT re-exported
  - Used by: CLI (`app/`) and E2E tests

- **`Coal.Language`**: Surface language type library
  - Re-exports 19 core language modules (Type, Expression, Pattern, etc.)
  - Does NOT re-export: `Coal.Language.Definition`, `Coal.Language.Module.*`
  - Widely used throughout the compiler (33+ files)

- **`Coal.Parser`**: Minimal parser API
  - Only exposes: `parseSourceFile`, `ParserError`
  - Internal parser implementation is hidden

- **`Coal.Kernel.*`**: Kernel language modules
  - **Keep as-is** - the kernel will become a separate language/package
  - Do not refactor barrel modules under `Coal.Kernel`

- **`Extras`**: Utility library API boundary (keep as-is)

### Import Anti-Patterns to Avoid

**❌ Mixed Imports (Redundant):**

```haskell
-- BAD: Importing both barrel and sub-modules already re-exported
import Coal.Language (Expression (..))
import Coal.Language.Expression (Expression (..))  -- Already in barrel!
```

**✅ Correct Pattern:**

```haskell
-- GOOD: Use barrel for re-exported modules
import Coal.Language (Expression (..), Pattern (..), Choice (..))
import Coal.Language.Definition  -- NOT re-exported, so separate import is OK
```

### Import Style Guidelines

1. **Standard library**: Use qualified imports

   ```haskell
   import qualified Data.Map as Map
   import qualified Data.Text as Text
   ```

2. **Coal modules**: Use explicit imports from barrels when possible

   ```haskell
   import Coal.Language (Type (..), Expression (..), Pattern (..))
   ```

3. **Environment/Name**: Use qualified imports

   ```haskell
   import qualified Coal.Common.Environment as Environment
   ```

4. **Module-specific**: Import only what you need
   ```haskell
   import Coal.Language.Module (Module (..))
   import Coal.Language.Module.Path (principalPath)
   ```

### When to Add Explicit Sub-Module Imports

Only import sub-modules directly when:

- The module is NOT re-exported by the barrel (e.g., `Coal.Language.Definition`)
- You need fine-grained control over specific exports
- The barrel doesn't exist for that namespace

Never import both a barrel and its re-exported sub-modules in the same file.

## Developer Workflows

- **Build**: Use `stack install` (requires GHC, LLVM, libgc, gmp; see README for details).
- **Run**: The compiler executable is `coal`. Major commands:
  - `coal compile [opts] <file>`: Compile a Coal program.
  - `coal build`: Build a project defined in `coal.json`.
  - `coal clean`: Clean build artifacts.
  - `coal install`: Install dependencies (Git-based).
  - `coal --version`: Print compiler version.
- **Test**: Haskell tests in `test/`, run with `stack test`. E2E Coal examples in `test/examples/`.
- **Debug**: Use `stack ghci` for interactive debugging. Compiler errors include detailed messages and source locations.

## Integration & Dependencies

- **LLVM**: Required for codegen; ensure `llc` and `llvm-as` are available.
- **C Runtime**: See `runtime/` for C code linked with generated binaries.
- **External Libraries**: Boehm GC (`libgc`), GMP (`libgmp`).
- **Git Integration**: Package manager uses Git for dependency resolution; see `app/Coal/CLI/Git.hs`.

## Project-Specific Advice

- **New Language Features**: Add as compiler passes or AST transformations; integrate into the pipeline in `src/Coal/Compiler/Pipeline.hs`.
- **New Commands**: Add command definitions in `app/Coal/CLI/Command/` and register in `app/Main.hs` and `app/Coal/CLI/Parser/Command.hs`.
- **Kernel Representation**: When modifying intermediate representations, update both the high-level AST and kernel IR for consistency.
- **Error Handling**: Use descriptive error types with pretty-printing; see `src/Coal/Compiler/Error.hs` for patterns.
- **Standard Library**: Add new Coal modules to `lang/` and reference them in bootstrapping and examples.

## File Structure Details

- `app/Coal/CLI/Command/`: Individual command implementations (Build, Compile, Clean, Install, Version).
- `app/Coal/CLI/Parser/`: Option and command parsers using `optparse-applicative`.
- `app/Coal/Package/`: Package manifest, dependencies, locking, versioning, and errors.
- `src/Coal/Compiler/Pass/`: Compiler passes organized by phase (Parsing, Preflight, Main, Lowering).
- `src/Coal/Compiler/TypeInference/`: Type inference and constraint solving.
- `src/Coal/Kernel/Compiler/`: Kernel compilation, passes, and pipeline.
- `src/Coal/Kernel/LLVM/`: LLVM IR generation and code emission.

## References

- [README.md](../README.md): Language philosophy, build, and usage.
- [src/Coal/](../src/Coal/): Compiler architecture and key modules.
- [src/Coal/Kernel/](../src/Coal/Kernel/): Kernel language and compilation.
- [app/Coal/](../app/Coal/): CLI and package management.
- [lang/](../lang/): Standard library examples.
- [test/](../test/): Test structure and E2E examples.

---

For more, see https://coal-lang.org/ and in-code documentation.
