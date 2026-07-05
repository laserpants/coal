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
(essentially `i -> CompilerT Metadata m o`). Modules flow through the pipeline in
parallel via `mapPass . liftPass`.

## Kernel normalization pipeline

After translation, the kernel IR goes through normalization passes defined in
`src/Coal/Kernel/Pipeline/Passes.hs`. The goal is to reach **administrative normal
form (ANF)** — every non-atomic sub-expression is a let-binding.

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

## LLVM code generation

`src/Coal/Kernel/Compiler.hs` orchestrates the full flow: parse → normalize → codegen.
LLVM IR is produced via `src/Coal/Kernel/LLVM/Codegen.hs` (`irModule`), which generates
one `IRModule` per kernel module.

Key LLVM modules:
- `Codegen.hs` — main codegen orchestrator
- `Boxing.hs` — value boxing/unboxing
- `Constructor.hs` — data constructor lowering
- `Function.hs` — function/closure codegen
- `Runtime.hs` / `RuntimeDefs.hs` — runtime function declarations
- `Monad.hs` — codegen monad (`IRCodegen`)