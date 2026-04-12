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
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (parseModule)
import Coal.Parser.Core (spaces)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Data.Text (Text)
import Extras (Name)
import Text.Megaparsec (eof, runParser)

passCheckDeps :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passCheckDeps = Pass{runPass = pass}

pass :: (MonadIO m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
pass = traverse check

check :: (MonadIO m) => BuildEnvelope (Module Metadata () ()) -> CompilerT Metadata m (BuildEnvelope (Module Metadata () ()))
check =
  \case
    BSource src -> do
      pure (BSource src)
    BCached Build{..} -> do
      CompilerState{..} <- get
      if any (\dep -> principalPath dep `elem` compilerToBeRecompiled) buildDependencies
        then do
          let name = principalPath buildPath
          src <- getSourceC name
          res <- fromSource name src
          case res of
            Left e -> do
              tellErrors [e]
              throwError ParserFailure
            Right r ->
              pure r
        else pure (BCached Build{..})

fromSource :: (MonadIO m) => Name -> Text -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
fromSource name src = do
  toBeRecompiled name
  case runParser (spaces *> parseModule <* eof) "" src of
    Left{} ->
      error "Implementation error"
    Right module_ -> do
      pure $ Right (BSource module_)
