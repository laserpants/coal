{- |
Module: Coal.Compiler.Pass.Counts

Centralized progress bar tick counts for compiler phases.

This module provides a single source of truth for progress bar calculations.
When adding or removing compiler passes, update the corresponding constant here.

The tick counts represent the number of progress bar updates that occur when
processing modules through each phase. The `>->` operator in Pass.hs ticks
before running each pass, so the count equals the number of passes in a phase.
-}
module Coal.Compiler.Pass.Counts (
  calculateProgressBarTotal,
  cachedModuleTicks,
) where

-- | Parsing phase tick count (per file)
ticksParsing :: Int
ticksParsing = 1

{- | Preflight phase tick count (total, not per module)
Includes: passSortModules, passRefreshCache, passDetectMisplacedImportStatements,
passInsertBuiltinDefinitions, passDesugarWhereClauses, passDesugarDoNotation,
passDetectAliasCycles, passDetectShadowing, passDetectMainEntrypointMissing,
passDetectDuplicateParams
-}
ticksPreflight :: Int
ticksPreflight = 10

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
passPlaceholders, passCompileNats, passDetectCallCycles, passDenormalizeAST
Plus generateDebugArtifacts after each pass (13 + 13 = 26)
-}
ticksTranslation :: Int
ticksTranslation = 26

{- | Lowering phase tick count (per module)
Includes: passKernelTranslate, generateDebugArtifacts, passKernelCode, passLLVMOutput
-}
ticksLowering :: Int
ticksLowering = 4

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
