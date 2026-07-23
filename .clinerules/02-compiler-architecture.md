# Compiler architecture

## High-level pipeline

The compiler processes source files through six sequential phases, defined in
`src/Coal/Compiler/Pipeline.hs`:

```
phaseParsing → phasePreflight → phaseTypeChecking → phaseTranslation → phaseLowering → passLinking
```

| Phase | Module | Purpose |
|-------|--------|---------|
| Parsing | `Compiler/Pass/PhaseParsing` | Source text → untyped AST |
| Preflight | `Compiler/Pass/PhasePreflight` | Scope resolution, import resolution |
| Type checking | `Compiler/Pass/PhaseTypeChecking` | Kind/type inference, constraint solving |
| Translation | `Compiler/Pass/PhaseTranslation` | AST → kernel IR |
| Lowering | `Compiler/Pass/PhaseLowering` | Kernel IR → LLVM IR → object code |
| Linking | `Compiler/Pass/PhaseLowering/Linking` | Object files → executable |

Phases are composed with `>->` (Kleisli arrow). Each phase is a `Pass Metadata m i o`
(essentially `i -> CompilerT Metadata m o`). The type-checking and translation phases
run in parallel across modules via `mapPass . liftPass` within `phaseMainPasses`.
The pipeline is run via `runPass pipeline files` within the `CompilerT` monad.

Supporting modules in `src/Coal/Compiler/`:

| Module | Purpose |
|--------|---------|
| `Stack.hs` | Compiler error stack with source locations |
| `State.hs` | Global compiler state (sources, config, etc.) |
| `TypeInference.hs` | Top-level type inference orchestration |
| `Build.hs` | Build artifact management |
| `Config.hs` | Compiler configuration |
| `Pass.hs` | Pass monad, composition operators (`>->`, `mapPass`, `liftPass`) |

## Kernel normalization pipeline

After translation, the kernel IR goes through normalization passes defined in
`src/Coal/Kernel/Pipeline/Passes.hs`. The goal is to reach **administrative normal
form (ANF)** — every non-atomic sub-expression is a let-binding.

The normalization pipeline runs within the `PipelineT` monad (defined in
`src/Coal/Kernel/Pipeline.hs`), which wraps `StateT PipelineState (ExceptT PipelineError m)`.
A `Pass m i o` is `i -> PipelineT m o`. Passes are composed with `>=>` (Kleisli fish).

### 1. Structural normalization (`structuralNorm`)
1. `caseExpressionCanonicalization` — sort case clauses lexicographically by constructor
2. `localNameCanonicalization` — alpha-rename locals to unique names (`x.n`)
3. `lambdaFlattening` — collapse nested lambdas: `fn(a) => fn(b) => e` → `fn(a, b) => e`
4. `constructorSaturation` — eta-expand partial constructor applications

### 2. Functional normalization (`functionalNorm`)
5. `lambdaLifting` — lift lambda expressions to top-level definitions
6. `topLevelFunctionNormalization` — merge function-body lambdas; promote constant lambdas
7. `functionResultsSaturation` — eta-expand functions whose result type is a function type

### 3. Control-flow normalization (`controlFlowNorm`)
8. `logicalOperatorTranslation` — desugar `&&` / `||` into `if` expressions
9. `letBindingSimplification` — eliminate pure-alias `let x = y` bindings
10. `administrativeNormalForm` — extract every non-atomic sub-expression into a `let`

The full pipeline is `structuralNorm >=> functionalNorm >=> controlFlowNorm`.

## Kernel compiler

`src/Coal/Kernel/Compiler.hs` orchestrates the full flow from source files to LLVM IR
via the `CompilerT` monad. Key entry points:

- `compileFiles` — parse source files, normalize, and generate LLVM IR
- `compileModules` — given already-parsed modules, run normalization and codegen

Supporting kernel modules:

| Module | Purpose |
|--------|---------|
| `Kernel/Parser.hs` | Kernel IR text format parser |
| `Kernel/Prettyprinter.hs` | Kernel IR pretty-printer |
| `Kernel/Eval.hs` | Kernel IR interpreter / evaluation |
| `Kernel/TypeCheck.hs` | Kernel IR type checker |
| `Kernel/FreeVars.hs` | Free variable analysis |
| `Kernel/Builtin/` | Built-in kernel definitions |
| `Kernel/Graphviz/` | Graphviz DOT output for kernel IR |

## LLVM code generation

`src/Coal/Kernel/LLVM/Codegen.hs` (`irModule`, `irMainModule`) generates one
`IRModule` per kernel module.

Key LLVM modules:
- `Boxing.hs` — value boxing/unboxing
- `Constructor.hs` — data constructor lowering
- `Function.hs` — function/closure codegen
- `Module.hs` — module-level LLVM IR construction
- `Prim.hs` — primitive operation codegen
- `Runtime.hs` / `RuntimeDefs.hs` — runtime function declarations
- `Monad.hs` — codegen monad (`IRCodegen`)