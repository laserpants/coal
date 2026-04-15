{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.ReportTypeErrors (
  passReportTypeErrors,
) where

import Coal.AST.HasMetadata (getMetadata)
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError), MonadIO)

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
