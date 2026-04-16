-- +
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.TypePhase.ReportTypeErrors

Report type errors that occurred during constraint generation and solving.
Collect errors from both the constraint generation phase and the solver phase,
then report them to the user and abort compilation if any errors were found.
-}
module Coal.Compiler.Pass.TypePhase.ReportTypeErrors (
  passReportTypeErrors,
) where

import Coal.AST.HasMetadata (getMetadata)
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError), MonadIO)

{- | Type error pass.

Gather constraint generation errors and solver rule violations from the
compiler state, report them as type errors, and throw a TypeError exception
if any errors were found.
-}
passReportTypeErrors :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passReportTypeErrors = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl Module{..} = do
  constraintsGenErrors <- compilerGetConstraintsGenErrorsC
  forM_ constraintsGenErrors $
    \err ->
      tellErrors [ConstraintsError err (errorLocation err)]

  solverRuleViolations <- compilerGetSolverRuleViolationsC
  forM_ solverRuleViolations $
    \err ->
      tellErrors [SolverError err (errorLocation err)]

  unless (null constraintsGenErrors && null solverRuleViolations) (throwError TypeError)
  return Module{..}
 where
  errorLocation err = ErrorLocation (principalPath modulePath) (getMetadata err)
