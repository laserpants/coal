{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build (ModuleBuild (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (parseModule)
import Coal.Parser.Core (spaces)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Data.Text (Text)
import Extras (Name)
import Text.Megaparsec (eof, runParser)

passCheckDeps :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
passCheckDeps = Pass{runPass = pass}

pass :: (MonadIO m) => [BuildUnit (Module Metadata Kind ())] -> CompilerT Metadata m [BuildUnit (Module Metadata Kind ())]
pass = traverse check

fromSource :: (MonadIO m) => Name -> Text -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildUnit (Module Metadata Kind ())))
fromSource name src = do
  insertFreshModule name
  case runParser (spaces *> parseModule <* eof) "" src of
    Left{} ->
      error "Implementation error"
    Right module_ -> do
      pure $ Right (BSource module_)

check :: (MonadIO m) => BuildUnit (Module Metadata Kind ()) -> CompilerT Metadata m (BuildUnit (Module Metadata Kind ()))
check =
  \case
    BSource src ->
      pure (BSource src)
    BCached ModuleBuild{..} -> do
      CompilerState{..} <- get
      if any (\dep -> principalPath dep `elem` compilerFreshModules) moduleDependencies
        then do
          let name = principalPath moduleBuildPath
          src <- getVerbatimSourceC name
          res <- fromSource name src
          case res of
            Left e -> do
              tellErrors [e]
              throwError ParserFailure
            Right r ->
              pure r
        else pure (BCached ModuleBuild{..})
