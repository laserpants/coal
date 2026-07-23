# Coding style guide

This document describes the conventions and idioms used in the Coal compiler and
standard library. Follow these when contributing code.

---

## Table of contents

1. [Formatting](#formatting)
2. [Language extensions](#language-extensions)
3. [GHC warnings](#ghc-warnings)
4. [Module structure](#module-structure)
5. [Naming](#naming)
6. [Data types](#data-types)
7. [Typeclasses and idioms](#typeclasses-and-idioms)
8. [Compiler pass pattern](#compiler-pass-pattern)
9. [Kernel normalization pipeline](#kernel-normalization-pipeline)
10. [Documentation](#documentation)
11. [Testing](#testing)

---

## Formatting

Formatting is enforced by **fourmolu**. Run it before committing.

```bash
fourmolu -i src/**/*.hs
```

- **Indentation**: 2 spaces (set in `fourmolu.yaml`).
- **Line length**: No hard limit, but keep lines readable. Aim for ~100
  characters.
- **Record fields**: Each field on its own line, aligned with the opening brace:

```haskell
data CompilerState = CompilerState
  { compilerSources :: Environment Text
  , compilerModules :: Environment (Module Metadata Kind IndexedType)
  , compilerConfig :: CompilerConfig
  }
  deriving (Show, Eq)
```

- **Multi-line expressions**: Break after operators and align continuation
  lines:

```haskell
prettyErrorMessage
  (("\n• " <>) <$> msgs)
  loc
  (Environment.lookup path env)
```

- **Pragmas**: `{-# LANGUAGE #-}` declarations appear first in the file, one per
  line, sorted alphabetically:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
```

Some modules may use additional extensions — only enable what the file actually
uses, not every extension available.

---

## Language extensions

Language extensions are declared **per file**, never in `package.yaml`. Only
enable what the file actually uses.

The following table shows which extensions are used across the codebase and their
typical contexts:

| Extension | Typical context |
|---|---|
| `StrictData` | Data-type modules (AST nodes, state, environments) |
| `LambdaCase` | Pattern matching renderers, evaluators, any `\case` expression |
| `OverloadedStrings` | Modules that construct `Text` values from string literals |
| `NamedFieldPuns` | Destructuring records in builders and compilers |
| `RecordWildCards` | Pattern-matching large records with many fields |
| `GeneralizedNewtypeDeriving` | Newtype wrappers over monad stacks |
| `FlexibleContexts` | Constraints with type families or complex types |
| `FlexibleInstances` | Instances with constraints on type constructors |
| `ScopedTypeVariables` | Type annotations referencing binder-level type variables |
| `RecursiveDo` | LLVM codegen (`mdo` / `rec` blocks for recursive bindings) |
| `TypeApplications` | LLVM codegen, smart constructors |
| `RankNTypes` | Higher-rank type signatures (type inference, pass combinators) |
| `MultiParamTypeClasses` | Type system (constraint classes, unification) |
| `TypeFamilies` | Type inference internals |
| `DeriveFunctor` / `DeriveFoldable` / `DeriveTraversable` | AST types with structural recursion |
| `DeriveGeneric` / `DeriveDataTypeable` | Serialization (Binary, Data) |

Declare extensions at the top of the file, one per line, sorted alphabetically:

```haskell
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
```

---

## GHC warnings

All warnings listed in `package.yaml` are treated as meaningful. Do not suppress
them with `{-# OPTIONS_GHC -Wno-... #-}` unless unavoidable, and leave a comment
explaining why.

The enabled warning set includes:

```
-Wall -Wcompat -Widentities -Wincomplete-record-updates
-Wincomplete-uni-patterns -Wmissing-export-lists
-Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
```

`-Wmissing-export-lists` means **every module must have an explicit export
list**.

---

## Module structure

### Namespace hierarchy

Haskell source under `src/` follows a hierarchical module structure:

| Namespace | Purpose |
|---|---|
| `Coal.Language.*` | AST definitions: expressions, types, patterns, modules |
| `Coal.Compiler.Pass.*` | Compilation phases (one module per phase) |
| `Coal.Compiler.*` | Pipeline orchestration, configuration, error types |
| `Coal.Kernel.Language.*` | Kernel IR AST |
| `Coal.Kernel.Pipeline.Pass.*` | Individual normalization passes |
| `Coal.Kernel.LLVM.*` | LLVM IR code generation |
| `Coal.Kernel.Parser.*` | Kernel IR parser |
| `Coal.TypeSystem.*` | Kinds, constraints, substitution, unification |

Barrel re-export modules (like `src/Coal/Language.hs`) aggregate sub-modules
using `module` re-exports:

```haskell
module Coal.Language (
  module Coal.Language.Type,
  module Coal.Language.Expression,
  module Coal.Language.Pattern,
  -- ...
) where

import Coal.Language.Type
import Coal.Language.Expression
import Coal.Language.Pattern
```

### Export list

Always explicit, grouped by concept with section comments:

```haskell
module Coal.Compiler.State (
  -- * Type aliases
  CompilerConstraint,
  CompilerAssumption,

  -- * Compiler state
  CompilerState (..),
  initialCompilerState,

  -- * Supply operations
  overCompilerSupply,

  -- * Configuration
  overCompilerConfig,

  -- * Module and source management
  overCompilerModules,
  overCompilerSources,
) where
```

Export re-exported types in the section where they are conceptually used, not
at the bottom.

### Module-level documentation

A Haddock block comment immediately follows the language pragmas and precedes
the `module` declaration:

```haskell
{- |
Module: Coal.Compiler.State
Description: Compiler state management and accessor functions

This module defines the CompilerState data structure, which maintains all
mutable state during compilation including type inference state, constraints,
error tracking, and module management.
-}
module Coal.Compiler.State (...) where
```

For shorter documentation, a single-line Haddock is acceptable:

```haskell
{- | Free variable analysis. -}
module Coal.Kernel.FreeVars (freeVars) where
```

### Import order

1. Standard library and `base` packages (`Control.*`, `Data.*`, `System.*`,
   `Text.*`).
2. External packages (third-party libraries).
3. Internal library modules (`Coal.*`, `Extras.*`, `LLVM.*`), sorted
   alphabetically.

Each tier is separated by a blank line. Qualified imports share the tier with
their unqualified siblings:

```haskell
import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Coal.Common.Name (Name)
import Coal.Common.Environment (Environment (..))
import Coal.Kernel.Language.Expr (Binding (..), Expr (..))
import qualified Coal.Kernel.LLVM.Boxing as Boxing
```

Prefer **selective imports** over `(..)` for non-ADT values. Use `(..)` when
importing all constructors of a data type or all methods of a typeclass.

---

## Naming

| Category | Convention | Examples |
|---|---|---|
| Types and type constructors | `PascalCase`, module-prefixed | `CompilerState`, `CompilerConfig`, `InferenceRule` |
| Data constructors | `PascalCase` | `Ok`, `Err`, `Some`, `None`; `EAnnotationKindMismatch` |
| Record fields | `camelCase`, module-prefixed | `compilerSources`, `compilerModules`, `compilerConfig` |
| Functions and smart constructors | `camelCase` | `compile`, `extractModuleName`, `freeVars`, `normalizeTypeIndexes` |
| Type aliases | `PascalCase` | `CompilerConstraint`, `CompilerAssumption`, `IndexedType` |
| Passes | `camelCase` with `pass` prefix | `passDesugarDoNotation`, `passDetectAliasCycles` |
| Phases | `camelCase` with `phase` prefix | `phaseParsing`, `phasePreflight`, `phaseTypeChecking` |
| Module names / paths | `PascalCase` | `Coal.Compiler.Pass.PhaseTranslation` |
| Kernel normalization passes | `camelCase` | `administrativeNormalForm`, `lambdaLifting`, `lambdaFlattening` |

Prefix record fields with an abbreviated module/type name to avoid ambiguity
(`compilerSources`, not `sources`; `moduleName`, not `name`).

---

## Data types

### ADTs

One constructor per line. Constructors are sorted alphabetically inside a data
type where no semantic ordering applies:

```haskell
data CompilerError metadata
  = BadFilename FilePath String
  | BadModuleName FilePath Text
  | CallCycle [Text] (ErrorLocation metadata)
  | ConflictingImports Name Path (ErrorLocation metadata)
  | ConflictingParameter Name (ErrorLocation metadata)
  | ConstraintsError (ConstraintsGenError ()) (ErrorLocation metadata)
  | DuplicateTypeName Name String (ErrorLocation metadata)
  -- ...
```

### Records

Use record syntax for any data type with more than two fields. Do not use
positional syntax for records — always pattern match by field name:

```haskell
renderFunction IRFunction{functionName, functionRetType, functionBlocks} = ...
```

### Deriving

The standard deriving set for data types is `(Show, Eq, Ord)`. Add `Functor`,
`Foldable`, `Traversable`, `Generic`, `Data`, or `Typeable` only when actively
needed. For AST types that need structural recursion, derive `Functor`,
`Foldable`, and `Traversable`:

```haskell
data Expr t
  = EVar (Label t)
  | ECon (Label t)
  | ELit Prim
  | ENil Type
  | EApp Type (Expr t) (NonEmpty (Expr t))
  | ELam (NonEmpty (Label t)) (Expr t)
  | ELet (Binding t) (Expr t)
  | ECase (Expr t) (NonEmpty (Clause t))
  | EIf (Expr t) (Expr t) (Expr t)
  | EOp (Op (Expr t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
```

### StrictData

All data-type modules use `{-# LANGUAGE StrictData #-}`. Fields are strict by
default, which avoids accidental space leaks in the IR graph and compiler
state.

---

## Typeclasses and idioms

### LambdaCase

Use `\case` instead of a named parameter when a function immediately
pattern-matches its sole argument. This is the dominant style in the compiler:

```haskell
prettyError :: Environment Text -> CompilerError Metadata -> Text
prettyError env =
  \case
    ParserError file err ->
      "In file \"" <> Text.pack file <> "\":\n\n" <> Text.pack (errorBundlePretty err)
    BadModuleName file path ->
      "The module name '" <> path <> "' doesn't match the file name '" <> Text.pack file <> "'."
    NameNotInScope name erl ->
      errorMessage ["Name not in scope: '" <> name <> "'"] env erl
    -- ...
```

### OverloadedStrings

Always use `OverloadedStrings` in any module that constructs `Text` values with
string literals. Never call `Text.pack` on a literal.

### Text construction

Prefer `<>` and `foldMap` for building `Text`. Use `Text.intercalate` when
joining a list with a separator:

```haskell
Text.intercalate ", " (map (prettyType . normalizeTypeIndexes) ts)
foldMap (", " <>) (map prettyType params)
```

### Composing with Control.Monad

The compiler composes phases with the custom `>->` operator (Kleisli arrow in
`Coal.Compiler.Pass`), not the standard `>=>`. Write:

```haskell
phasePreflight =
  passSortModules
    >-> passRefreshCache
    >-> passDetectShadowing
```

not:

```haskell
passSortModules >=> passRefreshCache
```

### INLINE pragmas on small functions

Small, frequently used functions carry `{-# INLINE #-}` to eliminate
allocation overhead at call sites:

```haskell
{-# INLINE runUnifier #-}
runUnifier :: Int -> Unifier a -> (Either UnificationError a, Int)

{-# INLINE evalUnifier #-}
evalUnifier :: Int -> Unifier a -> Either UnificationError a
```

Apply the same pattern to zero-argument functions that are purely aliases.

---

## Compiler pass pattern

The compiler defines passes as `newtype Pass a m i o` in
`Coal.Compiler.Pass`, where `i -> CompilerT a m o`:

```haskell
newtype Pass a m i o = Pass {runPass :: i -> CompilerT a m o}
```

Passes are composed with the `>->` operator:

```haskell
(>->) :: (MonadIO m) => Pass a m p q -> Pass a m q r -> Pass a m p r
```

Use `mapPass` to apply a pass element-wise to a list (for `[BuildEnvelope i]`)
and `liftPass` to lift a pass into the `BuildEnvelope` wrapper.

Each phase module exports a single top-level pass value, typically named
`phase<Name>` or `pass<Name>`:

```haskell
module Coal.Compiler.Pass.PhaseTranslation (phaseTranslation) where

phaseTranslation :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
phaseTranslation =
  passNormalizeAST
    >-> passDesugarPatterns
    >-> passExpandGuards
    -- ...
```

The full pipeline is composed in `Coal.Compiler.Pipeline`:

```haskell
pipeline =
  timed "Parsing" phaseParsing
    >-> timed "Preflight" phasePreflight
    >-> phaseMainPasses (phaseTypeChecking >-> phaseTranslation)
    >-> timed "Kernel translate" (mapPass passKernelTranslate)
    >-> timed "Kernel codegen" passKernelCodegen
    >-> timed "Linking" passLinking
```

---

## Kernel normalization pipeline

The kernel normalization pipeline operates within the `PipelineT` monad
(defined in `Coal.Kernel.Pipeline`), which wraps
`StateT PipelineState (ExceptT PipelineError m)`. A `Pass m i o` is
`i -> PipelineT m o`. Passes are composed with `>=>` (Kleisli fish).

The pipeline proceeds through three stages:

### 1. Structural normalization (`structuralNorm`)
1. `caseExpressionCanonicalization` — sort case clauses lexicographically by
   constructor
2. `localNameCanonicalization` — alpha-rename locals to unique names (`x.n`)
3. `lambdaFlattening` — collapse nested lambdas:
   `fn(a) => fn(b) => e` → `fn(a, b) => e`
4. `constructorSaturation` — eta-expand partial constructor applications

### 2. Functional normalization (`functionalNorm`)
5. `lambdaLifting` — lift lambda expressions to top-level definitions
6. `topLevelFunctionNormalization` — merge function-body lambdas; promote
   constant lambdas
7. `functionResultsSaturation` — eta-expand functions whose result type is a
   function type

### 3. Control-flow normalization (`controlFlowNorm`)
8. `logicalOperatorTranslation` — desugar `&&` / `||` into `if` expressions
9. `letBindingSimplification` — eliminate pure-alias `let x = y` bindings
10. `administrativeNormalForm` — extract every non-atomic sub-expression into
    a `let`

The full pipeline is `structuralNorm >=> functionalNorm >=> controlFlowNorm`.

### Key invariants

| Invariant | Established by |
|---|---|
| **Constructor saturation** | `constructorSaturation` |
| **Lambda flattening** | `lambdaFlattening` |
| **Name uniqueness** | `localNameCanonicalization` |
| **ANF** | `administrativeNormalForm` |

After ANF, every non-atomic sub-expression must be a let-binding, and `EIf`
and `ECase` appear only in tail position.

---

## Documentation

Use **Haddock block comments** (`{- | ... -}`) for all exported declarations.
Line comments (`-- |`) are acceptable for short single-line docs on record
fields.

Module-level documentation goes after the language pragmas and before the
`module` declaration (see [Module-level documentation](#module-level-documentation)).

Document record fields in the data-type Haddock:

```haskell
{- | Compiler state — maintained throughout the compilation of a set of
source files.

__Fields:__

* 'compilerSources': Source file contents, keyed by module path.
* 'compilerModules': Parsed and type-checked module ASTs.
* 'compilerConfig': Current compiler configuration (flags, paths, etc.).
-}
data CompilerState = CompilerState { ... }
```

For function documentation, describe what the function does, its parameters,
and its invariants:

```haskell
{- | Compute the set of /free/ variables occurring in an expression.

A variable occurrence is considered free if:

  * it appears in the expression,
  * it is not introduced by an enclosing binder ('ELam', 'ELet', 'ECase'),
  * and it is not a constructor name ('ECon' or clause constructor).

The result contains complete labels, including type annotations.

= Example

@
fn(x : int32) =>
  add(x, y : int32)
@

has free variables @{ y : int32 }@, not just the name @\"y\"@.
-}
freeVars :: (Ord t) => Expr t -> Set (Label t)
```

---

## Testing

Tests use **hspec**. Run individual test modules rather than the full test
suite (`stack test` is very slow).

### Test locations

| Directory | Contents |
|---|---|
| `test/Spec.hs` | Top-level test runner |
| `test/E2E/` | End-to-end tests |
| `test/Coal/` | Unit tests for compiler modules |
| `test/examples/` | Individual example programs with `.expected` output files |

### Test structure

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.AdministrativeNormalFormSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- Helpers
var :: Text -> Expr Type
var n = EVar (lbl n)

spec :: Spec
spec = describe "AdministrativeNormalForm" $ do
  describe "atomic expressions" $ do
    it "leaves variables unchanged" $ do
      runPass (administrativeNormalForm) (modWith (var "x"))
        `shouldBe` Right (modWith (var "x"))

    it "extracts non-atomic application into let" $ do
      let input = modWith (app (var "f") [var "x", var "y"])
      result <- runPass administrativeNormalForm input
      checkAdministrativeNormalForm result `shouldBe` True
```

### Describe hierarchy

Use two levels: the top-level `describe` names the module under test; nested
`describe` blocks group related tests by feature or concept (`"Arithmetic"`,
`"Memory"`, `"Atomics"`).

### Assertions

Prefer `shouldBe` for exact equality. Use `shouldContain` when testing rendered
output (a substring match is sufficient and more robust). Avoid `shouldSatisfy`
unless no equality check is possible.

---

## C runtime

The C runtime follows its own coding standards documented in
`runtime/CODING_STYLE.md`. Key differences from the Haskell conventions:

- C11 standard with `-std=c11 -Wall -Wextra -Wpedantic -Werror`
- `rt_` prefix for all public API functions and types
- 4-space indentation, 80-character lines, Linux/K&R brace style
- Boehm GC for memory management, GMP for arbitrary-precision arithmetic
- Formatted with `clang-format` (LLVM-based style)

Do not duplicate `runtime/CODING_STYLE.md` content here. When working on
runtime C code, refer to that file directly.