{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.ReportTypeErrors (
  passReportTypeErrors,
) where

import Coal.AST.HasMetadata (getMetadata)
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError), MonadIO)
import Control.Monad.State (gets)
import Data.List (nub)

passReportTypeErrors :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passReportTypeErrors = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl Module{..} = do
  constraintsGenErrors <- nub <$> gets compilerConstraintsGenErrors
  forM_ constraintsGenErrors $
    \err ->
      tellErrors [ConstraintsError err (errorLocation err)]

  solverRuleViolations <- nub <$> gets compilerSolverRuleViolations
  forM_ solverRuleViolations $
    \err ->
      tellErrors [SolverError err (errorLocation err)]

  unless (null constraintsGenErrors && null solverRuleViolations) (throwError TypeError)
  pure Module{..}
 where
  errorLocation err = ErrorLocation (principalPath modulePath) (getMetadata err)
