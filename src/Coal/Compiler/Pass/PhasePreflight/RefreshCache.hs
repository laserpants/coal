{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.RefreshCache

Refresh cached build artifacts when their dependencies have been modified.

This pass examines cached builds and determines whether they need to be
recompiled based on dependency changes. If any dependencies of a cached
build have been touched (modified), the module is recompiled from source.
Otherwise, the cached build is reused for incremental compilation.

This enables fast incremental builds by avoiding unnecessary recompilation
of modules whose dependencies haven't changed.
-}
module Coal.Compiler.Pass.PhasePreflight.RefreshCache (
  passRefreshCache,
)
where

import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
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

{- | Cache refresh pass.

Check cached builds and recompile from source if any dependencies have been
modified. This enables incremental compilation by avoiding unnecessary work
on modules whose dependencies are unchanged.
-}
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
      let touched dep = principalPath dep `elem` compilerTouched
       in if any touched buildDependencies
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
  setTouchedC name
  case runParser parseSourceFile "" src of
    Left parserError -> do
      -- Parser failed on previously cached source - this indicates source corruption or parser bug
      tellErrors [ParserError (show name) parserError]
      throwError PreflightFailure
    Right module_ -> do
      pure $ Right (BSource module_)
