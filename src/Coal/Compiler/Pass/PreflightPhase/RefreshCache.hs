{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.RefreshCache (
  passRefreshCache,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (parseSourceFile)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Extras (Name)
import Text.Megaparsec (runParser)

passRefreshCache :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passRefreshCache = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl = traverse refreshCache

refreshCache :: (MonadIO m) => BuildEnvelope (Module Metadata () ()) -> CompilerT Metadata m (BuildEnvelope (Module Metadata () ()))
refreshCache =
  \case
    BSource src ->
      pure (BSource src)
    BCached Build{..} -> do
      CompilerState{..} <- get
      if any (\dep -> principalPath dep `elem` compilerToBeRecompiled) buildDependencies
        then do
          res <- compileFromSource (principalPath buildPath)
          case res of
            Left e -> do
              tellErrors [e]
              throwError PreflightFailure
            Right r ->
              pure r
        else pure (BCached Build{..})

compileFromSource :: (MonadIO m) => Name -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
compileFromSource name = do
  src <- getSourceC name
  toBeRecompiled name
  case runParser parseSourceFile "" src of
    Left{} ->
      error "Implementation error"
    Right module_ -> do
      pure $ Right (BSource module_)
