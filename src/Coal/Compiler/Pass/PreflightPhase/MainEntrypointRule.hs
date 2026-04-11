{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.MainEntrypointRule (passMainEntrypointRule) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

passMainEntrypointRule :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passMainEntrypointRule = mapPass $ Pass{runPass = traverse impl}

impl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
impl mm = do
  setCurrentPathC (protoOmodulePath mm)
  detectMainEntrypoint mm
  return mm

class RuleContext e where
  detectMainEntrypoint :: (Monad m) => e -> CompilerT Metadata m ()

instance (RuleContext e) => RuleContext [e] where
  detectMainEntrypoint = traverse_ detectMainEntrypoint

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectMainEntrypoint = traverse_ detectMainEntrypoint

instance RuleContext (Module Metadata () ()) where
  detectMainEntrypoint =
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
