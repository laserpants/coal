# Contributing to Coal

This is an open and evolving project — contributions are welcome.

## Table of contents

1. [Code of conduct](#code-of-conduct)
2. [How can I contribute?](#how-can-i-contribute)
   - [Bug reports](#bug-reports)
   - [Feature requests](#feature-requests)
   - [Building the ecosystem](#building-the-ecosystem)
   - [Writing documentation](#writing-documentation)
   - [Submitting code](#submitting-code)
3. [Development setup](#development-setup)
4. [Coding style](#coding-style)
   - [Haskell](#haskell)
   - [Runtime library (C)](#runtime-library-c)
5. [Pull request conventions](#pull-request-conventions)
   - [Commit messages](#commit-messages)
   - [Branching](#branching)
   - [Review process](#review-process)
6. [Testing](#testing)
   - [Running tests](#running-tests)
   - [Adding test examples](#adding-test-examples)
7. [AI coding policy](#ai-coding-policy)
8. [Getting help](#getting-help)

## Code of conduct

This project is committed to providing a welcoming and inclusive environment for
everyone. We expect all contributors to:

- Be respectful and considerate of differing perspectives and experiences.
- Give and gracefully accept constructive feedback.
- Focus on what is best for the community and the project.

Unacceptable behaviour includes harassment, trolling, personal attacks, or any
other conduct that creates an unsafe or unwelcoming environment. If you
experience or witness any such behaviour, please contact the maintainer at
[hildenjohannes@gmail.com](mailto:hildenjohannes@gmail.com).

## How can I contribute?

### Bug reports

Report bugs at: [https://codeberg.org/laserpants/coal/issues](https://codeberg.org/laserpants/coal/issues)

Please include:

- A description of the problem.
- Steps to reproduce the bug.
- What you expected to happen.
- What actually happened (including error messages and exit codes).

If possible, include a small, self-contained code example that demonstrates the
issue. A minimal reproduction is invaluable.

When reporting compiler bugs, it helps to identify which pipeline phase produces
the first incorrect result. The compiler supports emitting kernel IR at various
stages — use `coal compile` with appropriate flags to inspect intermediate
representations.

### Feature requests

Feature requests are welcome. Before opening a new one, please search the issue
tracker to see if the idea has already been discussed. When opening a feature
request, include:

- A clear description of the proposed feature and its motivation.
- Examples of how it would be used (syntax, expected behaviour).
- Any relevant background or prior art.

Features that align with the project's design principles (see `.clinerules/`)
and the language's total functional programming philosophy are most likely to
be accepted.

### Building the ecosystem

The Coal language ecosystem extends beyond the compiler itself. Contributions
in the following areas are especially valuable:

- **Standard library**: The standard library lives in `lang/`. Improvements to
  existing modules (e.g., `List`, `String`, `Option`, `Number`) or new standard
  library modules are welcome.

- **Package manager**: The `Package/` namespace in the compiler contains
  dependency management infrastructure. Improvements to package resolution,
  manifest handling, or version management are appreciated.

- **Developer tooling**: IDE support, syntax highlighting (for editors,
  `highlight.js`, etc.), and other tooling improvements help grow the
  community. If you're working on such a tool, open an issue to coordinate.

- **Build and CI**: Improvements to the build system (`stack.yaml`,
  `package.yaml`, Docker setup) or CI pipeline are valuable.

### Writing documentation

The documentation site is [coal-lang.org](https://coal-lang.org/).

It is built with the [Zensical](https://zensical.org/) framework.
The source repository is at: [github.com/laserpants/coal-docs](https://github.com/laserpants/coal-docs)

Contributions to language documentation, tutorials, and guides are highly
welcome. Documentation improvements are one of the most impactful ways to
contribute to an open-source language.

### Submitting code

The repository is at [codeberg.org/laserpants/coal](https://codeberg.org/laserpants/coal).

All code contributions are made via pull requests to the `main` branch. Before
submitting a pull request:

1. Ensure your changes follow the coding style (see [Coding style](#coding-style)).
2. Ensure tests pass (see [Testing](#testing)).
3. Keep the scope focused — a single pull request should address one concern.
4. Write clear commit messages (see [Commit messages](#commit-messages)).

## Development setup

### Prerequisites

- **GHC and Stack**: Install via [GHCup](https://www.haskell.org/ghcup/).
- **LLVM**: An LLVM toolchain providing `llvm-as` and `llc`. On macOS:
  `brew install llvm`. On Debian/Ubuntu: `apt install llvm clang`.
- **Boehm GC** and **GMP**: `brew install bdw-gc gmp` (macOS) or
  `apt install libgc-dev libgmp-dev` (Debian/Ubuntu).
- **fourmolu**: The Haskell formatter. Install with `stack install fourmolu-0.20.0.0`
  or `cabal install fourmolu-0.20.0.0`. The codebase is formatted with
  fourmolu 0.20.0.0; the configuration is pinned in `fourmolu.yaml`.
- **clang-format**: Required for the C runtime. Part of the LLVM/clang
  distribution.
- **CMake**: Required to build the runtime tests: `brew install cmake` (macOS)
  or `apt install cmake` (Debian/Ubuntu).

### Building

```bash
git clone ssh://git@codeberg.org/laserpants/coal.git
cd coal
chmod +x project && ./project install
exec $SHELL -l
coal --version
```

See `README.md` for more detailed installation instructions and troubleshooting.

### Building the runtime separately

The C runtime can be built and tested independently:

```bash
cd runtime
mkdir -p build && cd build
cmake ..
make
ctest
```

## Coding style

### Haskell

Haskell code is formatted with **fourmolu** and linted with **HLint**. Run both before committing:

```bash
# Format
fourmolu -i src/ app/ test/

# Lint
stack build hlint
stack exec hlint -- src/ app/ test/
```

Configuration is in `fourmolu.yaml` at the project root.

Thorough coding conventions for Haskell are documented in `CODING_STYLE.md`
at the project root. Please read that file before submitting Haskell code.
Key points:

- All modules must have explicit export lists.
- Language extensions are declared per file, never in `package.yaml`.
- Use `{-# LANGUAGE StrictData #-}` in data-type modules.
- Prefer `\case` over a named parameter when a function immediately
  pattern-matches its sole argument.
- Passes are composed with the custom `>->` operator (`Coal.Compiler.Pass`).
- Two-space indentation, no hard line-length limit (aim for ~100 characters).

### Runtime library (C)

The runtime (`runtime/`) follows comprehensive C11 coding standards documented
in `runtime/CODING_STYLE.md`. Key highlights:

- C11 standard with `-std=c11 -Wall -Wextra -Wpedantic -Werror`.
- `rt_` prefix for all public API functions and types.
- Use Boehm GC allocation (`rt_alloc`, `rt_alloc_atomic`) — never `malloc`/`free`.
- Use fixed-width integer types from `<stdint.h>` (`int32_t`, `int64_t`, etc.).
- Format with `clang-format` (LLVM-based style): `clang-format -i runtime/src/*.c`.
- 4-space indentation, 80-character lines, Linux/K&R brace style.

## Pull request conventions

### Commit messages

Write commit messages following the [Conventional Commits](https://www.conventionalcommits.org/)
format where appropriate, and keep each commit focused on a single logical change.

```
<type>: <short summary>

[optional body with additional context]

[optional footer(s)]
```

Types include:

| Type | Usage |
|------|-------|
| `feat` | A new language feature or compiler capability |
| `fix` | A bug fix |
| `docs` | Documentation changes (README, CONTRIBUTING, etc.) |
| `style` | Code style changes (formatting, etc.) |
| `refactor` | Code restructuring without behaviour change |
| `test` | Adding or updating tests |
| `chore` | Build, CI, or tooling changes |
| `perf` | Performance improvements |

Examples:

```
fix: resolve panic on division by zero in kernel eval
feat: add foldl to the List standard library
docs: clarify structural recursion constraints in README
```

Prefer imperative present-tense descriptions ("fix", not "fixed" or "fixes").
The first line should not exceed 72 characters.

### Branching

- The `main` branch is the development branch. All pull requests target `main`.
- Feature branches should be named descriptively, e.g.,
  `fix/division-by-zero`, `feat/list-foldl`, `docs/update-readme`.
- Keep branches short-lived. Open a pull request early and mark it as a draft
  if it is not yet ready for review.

### Review process

1. A maintainer will review your pull request. This may take a few days.
2. If changes are requested, address them and push updated commits.
3. Once approved, a maintainer will merge your pull request.

Code review focuses on:

- Correctness: Does the change do what it claims?
- Design: Does it fit the compiler architecture and pipeline model?
- Style: Does it follow the project's coding conventions?
- Tests: Are there sufficient tests for the change?

## Testing

Tests use **hspec**. The full test suite (`stack test`) takes a very long time,
so prefer running individual tests during development.

### Running tests

```bash
# Run all tests (slow)
stack test

# Run a specific test module
stack test --test-arguments="-m E2E"

# Run a single test example
stack test --test-arguments="-m 001"
```

### Test locations

| Directory | Contents |
|---|---|
| `test/Spec.hs` | Top-level test runner |
| `test/E2E/` | End-to-end integration tests |
| `test/Coal/` | Unit tests for compiler modules |
| `test/examples/` | Individual example programs with `.expected` output files |

### Adding test examples

End-to-end tests follow a simple pattern. Each example lives in its own
directory under `test/examples/` (or `test/Coal/examples/`), containing:

- One or more `.coal` source files.
- An `.expected` file containing the expected stdout output.

To add a new example:

1. Create a numbered directory: `test/examples/NNN/`.
2. Add `Main.coal` (and any supporting modules).
3. Add an `.expected` file with the expected output.
4. Register the example in `test/E2E/Spec.hs`.

For compiler error tests, register the test with the expected error variant
(e.g., `TypeError`, `PreflightFailure`, `PatternAnomaly`).

### Runtime tests

The C runtime has its own test suite:

```bash
cd runtime
mkdir -p build && cd build
cmake .. && make && ctest
```

## AI coding policy

Contributions written with the assistance of AI tools (large language models,
code completion systems, etc.) are welcome under the following conditions:

1. **You are responsible for the code you submit.** AI-generated code must be
   reviewed, tested, and understood by you before submission.
2. **Disclose AI assistance.** If significant portions of a contribution were
   AI-generated, please note this in the pull request description.
3. **No wholesale generated contributions.** AI tools may be used to assist
   with implementation, but the contributor must provide meaningful direction,
   design decisions, and quality assurance.
4. **Follow the project's coding conventions.** AI-generated code is held to
   the same standards as human-written code — it must be formatted with
   fourmolu, have explicit export lists, follow naming conventions, etc.

This policy exists to ensure that contributions maintain the project's quality
standards and that every contributor remains accountable for their work.

## Getting help

- **Issue tracker**: [https://codeberg.org/laserpants/coal/issues](https://codeberg.org/laserpants/coal/issues)
- **Documentation**: [https://coal-lang.org](https://coal-lang.org/)
- **Compiler architecture**: See `.clinerules/` and `docs/compiler/` for
  detailed pass documentation and architecture notes.
- **Maintainer**: [hildenjohannes@gmail.com](mailto:hildenjohannes@gmail.com)

For questions or discussion about the design of the language or compiler,
opening an issue on the tracker is the best way to get feedback.