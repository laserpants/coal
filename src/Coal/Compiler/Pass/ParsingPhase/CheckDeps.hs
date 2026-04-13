{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (parseModule)
import Coal.Parser.Core (spaces)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Extras (Name)
import Text.Megaparsec (eof, runParser)

passCheckDeps :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passCheckDeps = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl = traverse check

check :: (MonadIO m) => BuildEnvelope (Module Metadata () ()) -> CompilerT Metadata m (BuildEnvelope (Module Metadata () ()))
check =
  \case
    BSource src -> do
      pure (BSource src)
    BCached Build{..} -> do
      CompilerState{..} <- get
      if any (\dep -> principalPath dep `elem` compilerToBeRecompiled) buildDependencies
        then do
          res <- buildFromSource (principalPath buildPath)
          case res of
            Left e -> do
              tellErrors [e]
              throwError ParserFailure
            Right r ->
              pure r
        else pure (BCached Build{..})

buildFromSource :: (MonadIO m) => Name -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
buildFromSource name = do
  src <- getSourceC name
  toBeRecompiled name
  case runParser (spaces *> parseModule <* eof) "" src of
    Left{} ->
      error "Implementation error"
    Right module_ -> do
      pure $ Right (BSource module_)
