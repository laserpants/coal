{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.ReportTypeErrors

Report type errors that occurred during constraint generation and solving.
Collect errors from the constraint generation phase, the solver phase, and
kind-constraint generation, then report them to the user and abort compilation
if any errors were found.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.ReportTypeErrors (
  passReportTypeErrors,
) where

import Coal.Compiler.HasMetadata (getMetadata)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.TypeSystem.Kind.Constraint (KindConstraintError (..))
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)

{- | Type error pass.

Gather constraint generation errors, solver rule violations, and
kind-constraint-generation errors from the compiler state, report them as type
errors, and throw a TypeError exception if any errors were found.
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

  -- Kind errors (e.g. a type constructor or trait that is not in scope) are
  -- reported against the definition that produced them; 'KindError' itself
  -- carries no source metadata, so the location travels alongside it. Without
  -- this the errors collected by 'generateKindConstraints' are silently dropped.
  kindErrors <- compilerGetKindConstraintsGenErrorsC
  forM_ kindErrors $
    \(KindConstraintError loc err) ->
      tellErrors [KindError err (ErrorLocation (principalPath modulePath) loc)]

  unless (null constraintsGenErrors && null solverRuleViolations && null kindErrors) (throwError TypeError)
  return Module{..}
 where
  errorLocation err = ErrorLocation (principalPath modulePath) (getMetadata err)
