{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DetectMainEntrypointMissing (
  passDetectMainEntrypointMissing,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

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
