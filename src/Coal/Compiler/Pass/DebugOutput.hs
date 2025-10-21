{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts) where

import Coal.Compiler
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Prettyprinter (Pretty (..))

generateDebugArtifacts :: (MonadIO m, Pretty t, Show t) => Text -> Pass a m (Module a k t) (Module a k t)
generateDebugArtifacts ll =
  Pass
    { passName = "debug<" <> ll <> ">"
    , runPass = pass ll
    }

pass :: (MonadIO m, Pretty t, Show t) => Text -> Module a k t -> CompilerT a m (Module a k t)
pass label m = do
  liftIO $ writeDotFiles label m
  pure m
