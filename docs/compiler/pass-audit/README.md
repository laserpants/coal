# Coal Compiler Pass Audit

This directory documents every compiler pass in `src/Coal/Compiler/Pass/`, describing
what each pass does, its inputs/outputs, invariants, and interactions with other passes.

## Complete Compiler Pipeline

The compiler pipeline is defined in `src/Coal/Compiler/Pipeline.hs` and consists of
six sequential phases:

```
phaseParsing >-> phasePreflight >-> phaseMainPasses (phaseTypeChecking >-> phaseTranslation) >-> extraTicks >-> phaseLowering >-> passLinking
```

The type-checking and translation phases (`phaseMainPasses`) run **in parallel across
modules** via `mapPass . liftPass`. All other phases operate on module collections
sequentially.

## Order of Execution

| # | Phase | Input Type | Output Type |
|---|-------|-----------|-------------|
| 1 | **PhaseParsing** | `[FilePath]` | `[BuildEnvelope (Module Metadata () ())]` |
| 2 | **PhasePreflight** | `[BuildEnvelope (Module Metadata () ())]` | `[BuildEnvelope (Module Metadata () ())]` |
| 3 | **PhaseTypeChecking** | `Module Metadata () ()` | `Module Metadata Kind IndexedType` |
| 4 | **PhaseTranslation** | `Module Metadata Kind IndexedType` | `Module Metadata Kind IndexedType` |
| 5 | **PhaseLowering** | `[BuildEnvelope (Module Metadata Kind IndexedType)]` | `[(Name, ByteString)]` |
| 6 | **Linking** | `[(Name, ByteString)]` | `()` |

Phases 3 and 4 are run together as `phaseMainPasses`, mapping each module in parallel.

### Extra Ticks

Between `phaseMainPasses` and `phaseLowering`, the pipeline inserts progress bar ticks
for cached modules via `extraTicks`. This ensures the progress bar reaches 100% even
when all modules are cached.

## Dependency Graph Between Phases

```
PhaseParsing
    |
    v
PhasePreflight
    |
    v
PhaseTypeChecking <-- runs per-module, in parallel
    |
    v
PhaseTranslation <-- depends on type-checked AST
    |
    v
PhaseLowering
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
Performs 11 passes that validate module structure, detect errors, insert builtins, and
prepare the module for type checking. Includes topological sort, shadowing detection,
alias cycle detection, and do-notation desugaring.

### PhaseTypeChecking
Performs 10 passes that annotate the AST with kind and type information, expand aliases,
prepare the build environment, expand folds, and run type inference. Transforms
`Module Metadata () ()` to `Module Metadata Kind IndexedType`.

### PhaseTranslation
Performs 14 passes that normalize, desugar, and compile patterns and expressions.
Transforms the typed AST into a form suitable for kernel translation. Includes
pattern match compilation, trait dictionary insertion, and nat compilation.

### PhaseLowering
Translates surface language modules to kernel IR modules, then compiles kernel IR to
LLVM bitcode. Consists of `passKernelTranslateNew` (per-module, via `mapPass`) and
`passKernelCodegen` (all modules together).

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
  - [DesugarWhereClauses.md](DesugarWhereClauses.md)
  - [DesugarDoNotation.md](DesugarDoNotation.md)
  - [DetectAliasCycles.md](DetectAliasCycles.md)
  - [DetectShadowing.md](DetectShadowing.md)
  - [DetectDuplicateParams.md](DetectDuplicateParams.md)
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
  - [NormalizeAST.md](NormalizeAST.md)
  - [DesugarPatterns.md](DesugarPatterns.md)
  - [ExpandGuards.md](ExpandGuards.md)
  - [ExpandOrPatterns.md](ExpandOrPatterns.md)
  - [CheckPatternAnomalies.md](CheckPatternAnomalies.md)
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
  - [KernelTranslateNew.md](KernelTranslateNew.md)
  - [KernelCodegen.md](KernelCodegen.md)
  - [Linking.md](Linking.md)