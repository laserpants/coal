# Coal Compiler Pass Audit

This directory documents every compiler pass in `src/Coal/Compiler/Pass/`, describing
what each pass does, its inputs/outputs, invariants, and interactions with other passes.

## Complete Compiler Pipeline

The compiler pipeline is defined in `src/Coal/Compiler/Pipeline.hs` and consists of
five sequential stages:

```
phaseParsing >-> phasePreflight >-> phaseMainPasses (phaseTypeChecking >-> phaseTranslation >-> kernelTranslateModule) >-> passKernelCodegen >-> passLinking
```

The type-checking, translation and kernel-translation passes (`phaseMainPasses`) are
applied **module by module** via `mapPass . liftPass`, so each module's surface AST can
be released as soon as its kernel module has been produced.

All passes are **sequential**: `mapPass` is `traverse`, and the compiler starts no
threads of its own (there is no `forkIO` or `par` anywhere in `src/`); its only
concurrency is waiting on the `llc` and `gcc` subprocesses. This matters for RTS
settings — extra capabilities buy no parallelism while multiplying the per-capability
allocation area — so `-with-rtsopts` in `package.yaml` deliberately leaves the
capability count at one (it does not set `-N`; see the comment there).

## Order of Execution

| # | Phase | Input Type | Output Type |
|---|-------|-----------|-------------|
| 1 | **PhaseParsing** | `[FilePath]` | `[BuildEnvelope (Module Metadata () ())]` |
| 2 | **PhasePreflight** | `[BuildEnvelope (Module Metadata () ())]` | `[BuildEnvelope (Module Metadata () ())]` |
| 3 | **PhaseTypeChecking** | `Module Metadata () ()` | `Module Metadata Kind IndexedType` |
| 4 | **PhaseTranslation** | `Module Metadata Kind IndexedType` | `Module Metadata Kind IndexedType` |
| 5 | **KernelTranslate** | `Module Metadata Kind IndexedType` | `Module Kernel.Type` |
| 6 | **KernelCodegen** | `[BuildEnvelope (Module Kernel.Type)]` | `[(Name, ByteString)]` |
| 7 | **Linking** | `[(Name, ByteString)]` | `()` |

Phases 3-5 are run together, per module, as `phaseMainPasses` (`mapPass . liftPass` over
the module list). `passKernelCodegen` then processes all kernel modules together,
because it needs the whole program to build the cross-module codegen context.

### Progress tracking

The interactive CLI pipeline (`pipelineWithProgress` in `Pipeline.hs`) tracks
progress with per-phase weights from `Coal.Compiler.Pass.Counts`, accumulating
them against a total computed by `updateTotal`.

## Dependency Graph Between Phases

```
PhaseParsing
    |
    v
PhasePreflight
    |
    v
PhaseTypeChecking <-- runs per module, in dependency order
    |
    v
PhaseTranslation <-- depends on type-checked AST
    |
    v
KernelTranslate <-- fused into the same per-module stage
    |
    v
KernelCodegen <-- all modules together
    |
    v
Linking
```

Within `PhasePreflight`, passes are strictly sequential (each depends on the previous).
Within `PhaseTypeChecking` and `PhaseTranslation`, passes are strictly sequential with
optional debug artifact generation between each pass.

## Phase Summaries

### PhaseParsing
Reads source files from disk, parses them into untyped AST modules, and resolves builtin
module embeddings. Produces a list of `BuildEnvelope` values that may be cached or freshly
parsed.

### PhasePreflight
Performs 12 passes that validate module structure, detect errors, insert builtins, and
prepare the module for type checking. Includes topological sort, shadowing detection,
alias cycle detection, let-binding expansion, tuple arity validation, and do-notation
desugaring.

### PhaseTypeChecking
Performs 10 passes that annotate the AST with kind and type information, expand aliases,
prepare the build environment, expand folds, and run type inference. Transforms
`Module Metadata () ()` to `Module Metadata Kind IndexedType`.

### PhaseTranslation
Performs 15 passes that normalize, desugar, and compile patterns and expressions.
Transforms the typed AST into a form suitable for kernel translation. Includes
pattern match compilation, trait dictionary insertion, and nat compilation.

### Kernel translation and codegen
Translates surface language modules to kernel IR modules, then compiles kernel IR to
LLVM bitcode. Kernel translation is fused into the per-module stage
(`kernelTranslateModule` inside `phaseMainPasses`); `passKernelCodegen` then runs over
all modules together, normalizing each kernel module and emitting LLVM bitcode. (The
standalone `phaseLowering` composition in `Coal.Compiler.Pass.PhaseLowering` is
retained.)

### Linking
Assembles LLVM bitcode to object files via `llc`, compiles the runtime C library with
`gcc`, and links everything into a final executable.

## Generated Documents

- [PhaseParsing.md](PhaseParsing.md) — overview of the parsing phase
  - [Parsing.md](Parsing.md) — source file parsing
- [PhasePreflight.md](PhasePreflight.md) — overview of the preflight phase
  - [SortModules.md](SortModules.md)
  - [RefreshCache.md](RefreshCache.md)
  - [DetectMisplacedImportStatements.md](DetectMisplacedImportStatements.md)
  - [InsertBuiltinDefinitions.md](InsertBuiltinDefinitions.md)
  - [DesugarDoNotation.md](DesugarDoNotation.md)
  - [ExpandLetBindings.md](ExpandLetBindings.md)
  - [DetectAliasCycles.md](DetectAliasCycles.md)
  - [DetectShadowing.md](DetectShadowing.md)
  - [DetectDuplicateParams.md](DetectDuplicateParams.md)
  - [CheckTupleArity.md](CheckTupleArity.md)
  - [DetectInvalidExports.md](DetectInvalidExports.md)
  - [DetectMainEntrypointMissing.md](DetectMainEntrypointMissing.md)
- [PhaseTypeChecking.md](PhaseTypeChecking.md) — overview of the type checking phase
  - [KindIndexing.md](KindIndexing.md)
  - [ExpandFunctionGroups.md](ExpandFunctionGroups.md)
  - [ExpandAliases.md](ExpandAliases.md)
  - [PrepareBuild.md](PrepareBuild.md)
  - [ExpandTopLevelFolds.md](ExpandTopLevelFolds.md)
  - [ExpandExpressionFolds.md](ExpandExpressionFolds.md)
  - [ExpandLambdaMatchExpressions.md](ExpandLambdaMatchExpressions.md)
  - [TypeInference.md](TypeInference.md)
  - [ReportTypeErrors.md](ReportTypeErrors.md)
  - [TypeVariables.md](TypeVariables.md)
  - [DebugOutput.md](DebugOutput.md)
- [PhaseTranslation.md](PhaseTranslation.md) — overview of the translation phase
  - [ExpandRecordUpdates.md](ExpandRecordUpdates.md)
  - [NormalizeAST.md](NormalizeAST.md)
  - [DesugarPatterns.md](DesugarPatterns.md)
  - [ExpandOrPatterns.md](ExpandOrPatterns.md)
  - [CheckPatternAnomalies.md](CheckPatternAnomalies.md)
  - [ExpandGuards.md](ExpandGuards.md)
  - [ExpandRecordPatterns.md](ExpandRecordPatterns.md)
  - [ExpandAsPatterns.md](ExpandAsPatterns.md)
  - [ExpandIntegerLiteralPatterns.md](ExpandIntegerLiteralPatterns.md)
  - [CompileMatchExpressions.md](CompileMatchExpressions.md)
  - [InsertDictionaries.md](InsertDictionaries.md)
  - [CompileNats.md](CompileNats.md)
  - [DetectCallCycles.md](DetectCallCycles.md)
  - [DenormalizeAST.md](DenormalizeAST.md)
  - [CheckTraitAnnotations.md](CheckTraitAnnotations.md)
- [PhaseLowering.md](PhaseLowering.md) — overview of the lowering phase
  - [KernelTranslate.md](KernelTranslate.md)
  - [KernelCodegen.md](KernelCodegen.md)
  - [Linking.md](Linking.md)