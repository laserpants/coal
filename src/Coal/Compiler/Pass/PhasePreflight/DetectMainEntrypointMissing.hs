{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.DetectMainEntrypointMissing

Detect missing main entry point in Main module.

This pass validates that the Main module contains a "main" function, which
serves as the program entry point. Coal programs must define a main function
in their Main module to specify where program execution begins.

For example, a valid Main module must include:

@
module Main {

  fun main() =
    ...
}
@

If the Main module exists but lacks a main function, this pass reports an
error during the preflight phase, preventing the build from proceeding.
-}
module Coal.Compiler.Pass.PhasePreflight.DetectMainEntrypointMissing (
  passDetectMainEntrypointMissing,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

{- | Main entry point detection pass.

Validate that the Main module contains a "main" function to serve as the
program entry point.
-}
passDetectMainEntrypointMissing :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectMainEntrypointMissing = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  detectMainEntrypointMissing m
  return m

class RuleContext e where
  detectMainEntrypointMissing :: (Monad m) => e -> CompilerT Metadata m ()

instance (RuleContext e) => RuleContext [e] where
  detectMainEntrypointMissing = traverse_ detectMainEntrypointMissing

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectMainEntrypointMissing = traverse_ detectMainEntrypointMissing

instance RuleContext (Module Metadata () ()) where
  detectMainEntrypointMissing =
    \case
      Module (Path ["Main"]) _ o -> do
        unless ("main" `elem` concatMap functionDefinitions o) $
          throwError MissingMainEntryPoint
      _ ->
        pure ()

functionDefinitions :: Definition Metadata () () -> [Name]
functionDefinitions =
  \case
    DFunction _ name _ ->
      [name]
    _ ->
      []
