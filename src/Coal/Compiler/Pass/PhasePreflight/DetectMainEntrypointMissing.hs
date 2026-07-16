{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
module Coal.Main {

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
import Coal.Compiler.Config (configEntryPoint)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (..))
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Extras (Name)

{- | Main entry point detection pass.

Validate that the Main module contains a "main" function to serve as the
program entry point.
-}
passDetectMainEntrypointMissing :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectMainEntrypointMissing = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  CompilerState{compilerConfig} <- get
  let (requiredMod, requiredFunc) = case configEntryPoint compilerConfig of
        Nothing -> (Path ["Main"], "main")
        Just (modName, funcName) -> (Path [modName], funcName)
  case m of
    Module path _ defs
      | path == requiredMod ->
          unless (requiredFunc `elem` concatMap functionDefinitions defs) $
            throwError MissingMainEntryPoint
    _ -> pure ()
  return m

functionDefinitions :: Definition Metadata () () -> [Name]
functionDefinitions =
  \case
    DFunction _ name _ ->
      [name]
    _ ->
      []
