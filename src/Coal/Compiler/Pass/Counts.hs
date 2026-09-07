{- |
Module: Coal.Compiler.Pass.Counts

Centralized progress bar weights for compiler phases.

This module is the single source of truth for progress calculations. Each
compiler phase contributes a weight proportional to its observed wall-clock
cost, so the interactive progress bar advances realistically from start to
finish (including the slow codegen and linking phases).
-}
module Coal.Compiler.Pass.Counts (
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
