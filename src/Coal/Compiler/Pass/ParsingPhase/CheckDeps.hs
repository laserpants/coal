{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (parseModule)
import Coal.Parser.Core (spaces)
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Extras (Name)
import Text.Megaparsec (eof, runParser)

passCheckDeps :: (MonadIO m) => Pass Metadata m [BuildUnit (ProtoModule Metadata () ())] [BuildUnit (ProtoModule Metadata () ())]
passCheckDeps = Pass{runPass = pass}

pass :: (MonadIO m) => [BuildUnit (ProtoModule Metadata () ())] -> CompilerT Metadata (ProtoCompilerT m Metadata) [BuildUnit (ProtoModule Metadata () ())]
pass = traverse check

check :: (MonadIO m) => BuildUnit (ProtoModule Metadata () ()) -> CompilerT Metadata (ProtoCompilerT m Metadata) (BuildUnit (ProtoModule Metadata () ()))
check =
  \case
    BSource src -> do
      pure (BSource src)
    BCached ProtoBuild{..} -> do
      ProtoCompilerState{..} <- lift $ get
      if any (\dep -> principalPath dep `elem` protoOcompilerToBeRecompiled) protoObuildDependencies
        then do
          let name = principalPath protoObuildPath
          src <- lift $ getSourceC name
          res <- fromSource name src
          case res of
            Left e -> do
              tellErrors [e]
              throwError ParserFailure
            Right r ->
              pure r
        else pure (BCached ProtoBuild{..})

fromSource :: (MonadIO m) => Name -> Text -> CompilerT Metadata (ProtoCompilerT m Metadata) (Either (CompilerError Metadata) (BuildUnit (ProtoModule Metadata () ())))
fromSource name src = do
  lift $ toBeRecompiled name
  case runParser (spaces *> parseModule <* eof) "" src of
    Left{} ->
      error "Implementation error"
    Right module_ -> do
      pure $ Right (BSource module_)
