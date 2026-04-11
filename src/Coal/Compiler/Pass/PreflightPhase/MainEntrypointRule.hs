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
import Coal.Language.Module.Path
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

passMainEntrypointRule :: (MonadIO m) => Pass Metadata m [BuildEnvelope (ProtoModule Metadata () ())] [BuildEnvelope (ProtoModule Metadata () ())]
passMainEntrypointRule = mapPass $ Pass{runPass = traverse impl}

impl :: (MonadIO m) => ProtoModule Metadata () () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata () ())
impl mm = do
  lift $ setCurrentPathC (protoOmodulePath mm)
  detectMainEntrypoint mm
  return mm

class RuleContext e where
  detectMainEntrypoint :: (Monad m) => e -> CompilerT Metadata (ProtoCompilerT m Metadata) ()

instance (RuleContext e) => RuleContext [e] where
  detectMainEntrypoint = traverse_ detectMainEntrypoint

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectMainEntrypoint = traverse_ detectMainEntrypoint

instance RuleContext (ProtoModule Metadata () ()) where
  detectMainEntrypoint =
    \case
      ProtoModule (Path ["Main"]) _ o -> do
        unless ("main" `elem` concatMap functionDefinitions o) $
          throwError MissingMainEntryPoint
      _ ->
        pure ()

functionDefinitions :: ProtoDefinition Metadata () () -> [Name]
functionDefinitions =
  \case
    ProtoDFunction _ name _ ->
      [name]
    _ ->
      []
