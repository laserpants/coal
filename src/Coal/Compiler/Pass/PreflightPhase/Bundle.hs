{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PreflightPhase.Bundle (passBundle) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Module.Builders
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.State

passBundle :: (Monad m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passBundle =
  Pass
    { passName = "Bundle"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT Metadata m [Module Metadata Kind ()]
pass modules = do
  forM_ modules $
    \m@(Module p _ _) -> do
      b <- build m
      insertModuleC (principalPath p) b
  pure modules
