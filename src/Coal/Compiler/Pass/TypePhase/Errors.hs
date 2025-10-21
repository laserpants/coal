{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TypePhase.Errors (passTypePhaseErrors) where

import Coal.Ast.Metadata (Metadata (..), getMetadata)
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (gets)
import Data.List (nub)

passTypePhaseErrors :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passTypePhaseErrors =
  Pass
    { passName = "TypePhaseErrors"
    , runPass = pass
    }

pass :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass m@(Module path _ _) = do
  constraintsGenErrors <- gets compilerConstraintsGenErrors
  let errs1 = nub constraintsGenErrors
  forM_ errs1 $
    \err ->
      tellErrors [ConstraintsError err (ErrorLocation (principalPath path) (getMetadata err))]
  solverRuleViolations <- gets compilerSolverRuleViolations
  let errs2 = nub solverRuleViolations
  forM_ errs2 $
    \err ->
      tellErrors [SolverError err (ErrorLocation (principalPath path) (getMetadata err))]
  unless (null errs1 && null errs2) (throwError TypeError)
  pure m
