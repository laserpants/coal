{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.MainEntrypointRule (passMainEntrypointRule) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language (Kind (..))
import Coal.Language.Module
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

passMainEntrypointRule :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passMainEntrypointRule = mapPass $ Pass{runPass = withCurrentModuleC_ detectMainEntrypoint}

class RuleContext e where
  detectMainEntrypoint :: (Monad m) => e -> CompilerT Metadata m ()

instance (RuleContext e) => RuleContext [e] where
  detectMainEntrypoint = traverse_ detectMainEntrypoint

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectMainEntrypoint = traverse_ detectMainEntrypoint

instance RuleContext (Module Metadata Kind t) where
  detectMainEntrypoint =
    \case
      Module (Path ["Main"]) _ o -> do
        unless ("main" `elem` concatMap functionDefinitions o) $
          throwError MissingMainEntryPoint
      _ ->
        pure ()

functionDefinitions :: Definition Metadata Kind t -> [Name]
functionDefinitions =
  \case
    d@DFunction{} ->
      [definitionName d]
    _ ->
      []
