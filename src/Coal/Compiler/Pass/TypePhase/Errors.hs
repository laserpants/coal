module Coal.Compiler.Pass.TypePhase.Errors (passTypePhaseErrors) where

import Coal.AST.HasMetadata (getMetadata)
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.ProtoState
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module.Path
import Coal.ProtoLanguage.ProtoModule
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError), MonadIO)
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import Data.List (nub)

passTypePhaseErrors :: (MonadIO m) => Pass Metadata m (ProtoModule Metadata Kind IndexedType) (ProtoModule Metadata Kind IndexedType)
passTypePhaseErrors = Pass{runPass = pass}

pass :: (Monad m) => ProtoModule Metadata Kind IndexedType -> CompilerT Metadata m (ProtoModule Metadata Kind IndexedType)
pass m@(ProtoModule path _ _) = do
  constraintsGenErrors <- gets protoOcompilerConstraintsGenErrors
  let errs1 = nub constraintsGenErrors
  forM_ errs1 $
    \err ->
      tellErrors [ConstraintsError err (ErrorLocation (principalPath path) (getMetadata err))]

  solverRuleViolations <- gets protoOcompilerSolverRuleViolations
  let errs2 = nub solverRuleViolations
  forM_ errs2 $
    \err ->
      tellErrors [SolverError err (ErrorLocation (principalPath path) (getMetadata err))]

  unless (null errs1 && null errs2) (throwError TypeError)
  pure m
