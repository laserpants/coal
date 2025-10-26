{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.MainEntrypointRule (
  passMainEntrypointRule,
) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language (Kind (..))
import Coal.Language.Module
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Name, traverse_)

passMainEntrypointRule :: (Monad m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passMainEntrypointRule =
  mapPass $
    Pass
      { passName = "MainEntrypointRule"
      , runPass = pass
      }

pass :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
pass m@(Module p _ _) = do
  setCompilerModuleC p
  detectMainEntrypoint m
  pure m

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
        unless ("main" `elem` concatMap functionDefs o) $
          throwError MissingMainEntryPoint
      _ ->
        pure ()

functionDefs :: Definition Metadata Kind t -> [Name]
functionDefs =
  \case
    d@DFunction{} ->
      [definitionName d]
    _ ->
      []
