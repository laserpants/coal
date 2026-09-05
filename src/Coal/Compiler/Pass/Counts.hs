{- |
Module: Coal.Compiler.Pass.Counts

Centralized progress bar tick counts and weights for compiler phases.

This module provides a single source of truth for progress calculations.
When adding or removing compiler passes, update the corresponding constant here.

The old tick-based system assigned equal weight to each sub-pass, which made
the progress bar reach 100% after type-checking while codegen and linking
(the slowest phases) had barely started. The new weight-based system assigns
realistic proportional weights that match observed wall-clock time distribution.
-}
module Coal.Compiler.Pass.Counts (
  calculateProgressBarTotal,
  cachedModuleTicks,
  calculateWeightTotal,
  weightParsing,
  weightPreflight,
  weightTypeChecking,
  weightTranslation,
  weightKernelTranslate,
  weightKernelCodegen,
  weightLinking,
) where

-- * Phase weights (approximate relative wall-clock cost)

-- | Parsing phase weight (per file).  Fast — mostly I/O + megaparsec.
weightParsing :: Int
weightParsing = 1

-- | Preflight phase weight (total, not per module).  Scoping, imports, etc.
weightPreflight :: Int
weightPreflight = 5

{- | Type checking phase weight (per module).  Kind indexing, fold expand,
constraint generation, type inference — can be heavy for large modules.
-}
weightTypeChecking :: Int
weightTypeChecking = 25

-- | Translation phase weight (per module).  AST desugaring, dictionary insertion.
weightTranslation :: Int
weightTranslation = 15

-- | Kernel translate phase weight (per module).  AST to kernel IR translation.
weightKernelTranslate :: Int
weightKernelTranslate = 10

{- | Kernel codegen phase weight (per module).  LLVM IR generation + llvm-as.
This is proportionally the most expensive phase per module.
-}
weightKernelCodegen :: Int
weightKernelCodegen = 25

-- | Linking phase weight (total).  llc + gcc on all object files.
weightLinking :: Int
weightLinking = 20

-- * Legacy tick counts (kept for backward compatibility with cached modules)

-- | Parsing phase tick count (per file)
ticksParsing :: Int
ticksParsing = 1

{- | Preflight phase tick count (total, not per module)
Includes: passSortModules, passRefreshCache, passDetectMisplacedImportStatements,
passInsertBuiltinDefinitions, passDesugarDoNotation, passDetectAliasCycles,
passDetectShadowing, passDetectMainEntrypointMissing, passDetectDuplicateParams
-}
ticksPreflight :: Int
ticksPreflight = 9

{- | Type checking phase tick count (per module)
Includes: passKindIndexing, passExpandFunctionGroups, passExpandAliases,
passPrepareBuild, passExpandTopLevelFolds, passExpandExpressionFolds,
passExpandLambdaMatchExpressions, passTypeInference, passReportTypeErrors
Plus generateDebugArtifacts after each pass (9 + 9 = 18)
-}
ticksTypeChecking :: Int
ticksTypeChecking = 18

{- | Translation phase tick count (per module)
Includes: passNormalizeAST, passDesugarPatterns, passExpandGuards,
passExpandOrPatterns, passCheckPatternAnomalies, passExpandRecordPatterns,
passExpandAsPatterns, passExpandIntegerLiteralPatterns, passCompileMatchExpressions,
passInsertDictionaries, passCompileNats, passDetectCallCycles, passDenormalizeAST
Plus generateDebugArtifacts after each pass (13 + 13 = 26)
-}
ticksTranslation :: Int
ticksTranslation = 26

{- | Lowering phase tick count (per module)
Includes: passKernelTranslate, passKernelCodegen
-}
ticksLowering :: Int
ticksLowering = 2

-- | Linking phase tick count (total)
ticksLinking :: Int
ticksLinking = 1

{- | Total ticks per module through main compilation passes
Used for simulating progress when loading cached modules
-}
ticksPerModule :: Int
ticksPerModule = ticksTypeChecking + ticksTranslation

-- | Total ticks for cached module (skips type checking and translation)
cachedModuleTicks :: Int
cachedModuleTicks = ticksPerModule

{- | Calculate total progress bar ticks for a compilation session

Takes into account:
- Parsing all files
- Preflight checks (once for all modules)
- Type checking and translation (per module)
- Lowering and code generation (per module)
- Linking (once at the end)
-}
calculateProgressBarTotal ::
  -- | Number of builtin modules
  Int ->
  -- | Number of user source files
  Int ->
  -- | Total progress bar ticks
  Int
calculateProgressBarTotal numBuiltinModules numFiles =
  let numModules = numBuiltinModules + numFiles
      perModuleTicks = numModules * (ticksPerModule + ticksLowering)
      globalTicks = ticksParsing + ticksPreflight + ticksLinking
   in perModuleTicks + globalTicks

-- | Calculate total weight for a compilation session (same formula as ticks but with weights).
calculateWeightTotal ::
  -- | Number of builtin modules
  Int ->
  -- | Number of user source files
  Int ->
  -- | Total progress weight
  Int
calculateWeightTotal _builtinModules numFiles =
  let perModuleWeight = numFiles * (weightTypeChecking + weightTranslation)
      globalWeight = weightParsing + weightPreflight + weightKernelTranslate + weightKernelCodegen + weightLinking
   in perModuleWeight + globalWeight
